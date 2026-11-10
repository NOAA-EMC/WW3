#!/usr/bin/env python3
"""./dev — entry point for the WW3 (WAVEWATCH III) dev environment.

Usage: ./dev <group> <command> [args...]
"""
from __future__ import annotations

import getopt
import os
import shutil
import subprocess
import sys
from pathlib import Path

# Keep IMAGE_NAME in sync with `image:` in compose.yaml.
IMAGE_NAME = "sim-ww3-dev:local"
SERVICE_NAME = "dev"

REPO_ROOT = Path(__file__).resolve().parent.parent
# Directory ./dev was started from. main() chdirs to the repo root, so file
# arguments are resolved against this first.
INVOKED_FROM = Path.cwd()

SWITCH_DIR = Path("model/bin")
BUILD_ROOT = Path("build")
BUILD_TYPES = ("Release", "Debug", "RelWithDebInfo", "MinSizeRel")
REGTESTS_DIR = Path("regtests")
# `optstr` of regtests/bin/run_cmake_test; keep in sync.
RUN_CMAKE_TEST_OPTSTR = "b:C:dfg:hi:m:n:No:Op:q:r:s:t:STw:"

USAGE = """\
Usage: ./dev <group> <command> [args...]

Docker (host side):
  docker build [args...]          Build the dev image (docker compose build).
                                  Extra args go to `docker compose build`
                                  (e.g. --no-cache). The first build compiles
                                  the Spack environment and takes over an hour.
  docker shell                    Start an interactive bash inside the dev container.
  docker run -- <cmd...>          Run an arbitrary command inside the dev container.
  docker clean                    Remove the dev image (asks first).

Build / regression tests (run inside the container):
  build [--type Release|Debug|RelWithDebInfo|MinSizeRel] <switch> [-- <cmake args...>]
                                  Configure and build WW3 with a switch file.
                                  <switch> is a name in model/bin (NCEP_st4 for
                                  model/bin/switch_NCEP_st4) or the path of a
                                  switch file inside the repo. The build dir is
                                  build/<name> for Release (the default) and
                                  build/<name>-<type> otherwise; executables are
                                  in its bin/. No argument lists the switches.
  regtest [run_cmake_test options] <case>
                                  Run a regression test: cd regtests &&
                                  ./bin/run_cmake_test <options> ../model <case>.
                                  `regtest -h` prints the run_cmake_test
                                  options; no argument lists the test cases.

Misc:
  help                            Show this help.

Environment overrides:
  HOST_UID / HOST_GID             Override host UID/GID passed to the build.
"""


def die(msg: str, code: int = 1, *, with_usage: bool = False) -> None:
    print(f"error: {msg}", file=sys.stderr)
    if with_usage:
        print(USAGE, end="", file=sys.stderr)
    sys.exit(code)


def run(cmd: list[str]) -> None:
    rc = subprocess.run(cmd).returncode
    if rc != 0:
        sys.exit(rc)


def require_compose() -> None:
    if shutil.which("docker") is None:
        die("docker is not installed or not on PATH", 127)
    rc = subprocess.run(
        ["docker", "compose", "version"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    ).returncode
    if rc != 0:
        die("'docker compose' plugin is required (v2)", 127)


def require_image() -> None:
    # `docker compose run` builds a missing image on the fly, which for this
    # image means an unannounced build of over an hour. Ask for it explicitly.
    rc = subprocess.run(
        ["docker", "image", "inspect", IMAGE_NAME],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    ).returncode
    if rc != 0:
        die(f"image {IMAGE_NAME} not found; run ./dev docker build first")


def run_in_container(args: list[str]) -> None:
    require_compose()
    require_image()
    # Allocate a pseudo-TTY only when stdin is interactive. In CI / piped
    # contexts `docker compose run` would otherwise abort with "the input
    # device is not a TTY"; -T disables TTY allocation there.
    cmd = ["docker", "compose", "run", "--rm"]
    if not sys.stdin.isatty():
        cmd.append("-T")
    run([*cmd, SERVICE_NAME, *args])


# ---------------------------------------------------------------------------
# docker subcommands

def cmd_docker_build(args: list[str]) -> None:
    require_compose()
    run(["docker", "compose", "build", *args])


def cmd_docker_shell(args: list[str]) -> None:
    if args:
        die("./dev docker shell takes no arguments", 2)
    run_in_container(["bash"])


def cmd_docker_run(args: list[str]) -> None:
    if args and args[0] == "--":
        args = args[1:]
    if not args:
        die("usage: ./dev docker run -- <command...>", 2)
    run_in_container(args)


def cmd_docker_clean(args: list[str]) -> None:
    if args:
        die("./dev docker clean takes no arguments", 2)
    require_compose()
    print(f"This will remove image '{IMAGE_NAME}'.")
    print("The BuildKit build cache is kept (`docker builder prune` removes it).")
    try:
        ans = input("Continue? [y/N] ")
    except EOFError:
        ans = ""
    if ans.strip().lower() not in {"y", "yes"}:
        print("aborted.")
        sys.exit(1)
    subprocess.run(["docker", "compose", "down", "--remove-orphans"])
    subprocess.run(
        ["docker", "image", "rm", "-f", IMAGE_NAME],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    print("done.")


DOCKER_SUBS = {
    "build": cmd_docker_build,
    "shell": cmd_docker_shell,
    "run": cmd_docker_run,
    "clean": cmd_docker_clean,
}


def dispatch_docker(args: list[str]) -> None:
    if not args:
        print(USAGE, end="", file=sys.stderr)
        sys.exit(1)
    sub, rest = args[0], args[1:]
    if sub in {"-h", "--help", "help"}:
        print(USAGE, end="")
        return
    fn = DOCKER_SUBS.get(sub)
    if fn is None:
        die(f"unknown docker subcommand: {sub}", 1, with_usage=True)
    fn(rest)


# ---------------------------------------------------------------------------
# build (CMake build of one switch configuration)

# CMAKE_OPTIONS (set in the image) keeps the RPATHs of `cmake --install`ed
# programs: the container has no LD_LIBRARY_PATH. Unquoted on purpose: it is a
# word list, as in regtests/bin/run_cmake_test.
BUILD_SH = r"""
set -e
BUILD_DIR="$1"; SWITCH_FILE="$2"; BUILD_TYPE="$3"; shift 3
cmake -S . -B "${BUILD_DIR}" ${CMAKE_OPTIONS:-} -DSWITCH="${PWD}/${SWITCH_FILE}" \
    -DCMAKE_BUILD_TYPE="${BUILD_TYPE}" "$@"
cmake --build "${BUILD_DIR}" -j "$(nproc)"
echo "executables: ${BUILD_DIR}/bin/"
"""


def list_switches() -> list[str]:
    return sorted(
        p.name[len("switch_"):]
        for p in SWITCH_DIR.glob("switch_*")
        if p.is_file()
    )


def print_switches(stream) -> None:
    print(f"switches in {SWITCH_DIR.as_posix()}/ (switch_<name>):", file=stream)
    for name in list_switches():
        print(f"  {name}", file=stream)


def resolve_switch(arg: str) -> tuple[str, Path]:
    # Returns (name, repo-relative path). A name in model/bin wins; otherwise
    # the argument is a file path, relative to where ./dev was started or to
    # the repo root. Only the repo is mounted, so the file must be inside it.
    bare = arg[len("switch_"):] if arg.startswith("switch_") else arg
    named = SWITCH_DIR / f"switch_{bare}"
    if "/" not in arg and named.is_file():
        return bare, named
    for base in (INVOKED_FROM, REPO_ROOT):
        candidate = (base / arg).resolve()
        if not candidate.is_file():
            continue
        try:
            rel = candidate.relative_to(REPO_ROOT)
        except ValueError:
            die(f"switch file must be inside the repository: {candidate}")
        name = candidate.name
        if name.startswith("switch_"):
            name = name[len("switch_"):]
        return name, rel
    print(f"error: unknown switch: {arg!r}", file=sys.stderr)
    print_switches(sys.stderr)
    sys.exit(1)


def cmd_build(args: list[str]) -> None:
    build_type = "Release"
    switch = None
    cmake_args: list[str] = []
    it = iter(args)
    for a in it:
        if a == "--":
            cmake_args.extend(it)
            break
        if a == "--type":
            try:
                build_type = next(it)
            except StopIteration:
                die("--type requires an argument")
        elif a.startswith("--type="):
            build_type = a.split("=", 1)[1]
        elif a.startswith("-"):
            die(f"unknown build option: {a} (CMake options go after --)")
        elif switch is None:
            switch = a
        else:
            die(f"unexpected argument: {a} (CMake options go after --)")
    if build_type not in BUILD_TYPES:
        die(f"--type must be one of {', '.join(BUILD_TYPES)}, got: {build_type!r}")
    if switch is None:
        print(
            "usage: ./dev build [--type <type>] <switch> [-- <cmake args...>]",
            file=sys.stderr,
        )
        print_switches(sys.stderr)
        sys.exit(1)

    name, path = resolve_switch(switch)
    dirname = name if build_type == "Release" else f"{name}-{build_type.lower()}"
    build_dir = BUILD_ROOT / dirname
    run_in_container(
        [
            "bash",
            "-c",
            BUILD_SH,
            "--",
            build_dir.as_posix(),
            path.as_posix(),
            build_type,
            *cmake_args,
        ]
    )


# ---------------------------------------------------------------------------
# regtest (regtests/bin/run_cmake_test wrapper)

REGTEST_SH = 'cd regtests && exec ./bin/run_cmake_test "$@"'


def list_cases() -> list[str]:
    # Test cases are the regtests/ subdirectories with an input*/ directory
    # (this leaves out regtests/bin and regtests/unittests).
    if not REGTESTS_DIR.is_dir():
        return []
    return sorted(
        p.name
        for p in REGTESTS_DIR.iterdir()
        if p.is_dir() and any(c.is_dir() for c in p.glob("input*"))
    )


def print_cases(stream) -> None:
    print("test cases (regtests/<case>; see regtests/<case>/info):", file=stream)
    for case in list_cases():
        print(f"  {case}", file=stream)


def regtest_usage_exit() -> None:
    print("usage: ./dev regtest [run_cmake_test options] <case>", file=sys.stderr)
    print("       ./dev regtest -h   (run_cmake_test options)", file=sys.stderr)
    print_cases(sys.stderr)
    sys.exit(1)


def cmd_regtest(args: list[str]) -> None:
    if not args:
        regtest_usage_exit()
    # run_cmake_test splits its arguments with `getopt $optstr $*`: unquoted,
    # and GNU getopt moves positional arguments after the options. Parse the
    # same way so `ww3_tp2.1 -n 4` is understood as <case> plus `-n 4`.
    for a in args:
        if not a or any(c.isspace() for c in a):
            die(f"run_cmake_test cannot take empty or whitespace arguments: {a!r}")
    try:
        opts, positional = getopt.gnu_getopt(args, RUN_CMAKE_TEST_OPTSTR)
    except getopt.GetoptError as exc:
        die(f"{exc} (./dev regtest -h lists the run_cmake_test options)")
    if any(opt == "-h" for opt, _ in opts):
        run_in_container(["bash", "-c", REGTEST_SH, "--", "-h"])
        return

    if len(positional) == 1:
        model_dir, case = "../model", positional[0]
    elif len(positional) == 2:
        model_dir, case = positional
    else:
        regtest_usage_exit()
    case = case.rstrip("/")
    if case not in list_cases():
        print(f"error: unknown test case: {case!r}", file=sys.stderr)
        print_cases(sys.stderr)
        sys.exit(1)

    takes_value = {
        c for i, c in enumerate(RUN_CMAKE_TEST_OPTSTR)
        if c != ":" and RUN_CMAKE_TEST_OPTSTR[i + 1:i + 2] == ":"
    }
    forwarded: list[str] = []
    for opt, value in opts:
        forwarded.append(opt)
        if opt[1:] in takes_value:
            forwarded.append(value)
    run_in_container(
        ["bash", "-c", REGTEST_SH, "--", *forwarded, model_dir, case]
    )


# ---------------------------------------------------------------------------
# main

GROUPS = {
    "docker": dispatch_docker,
    "build": cmd_build,
    "regtest": cmd_regtest,
}


def main(argv: list[str]) -> None:
    os.chdir(REPO_ROOT)
    os.environ.setdefault("HOST_UID", str(os.getuid()))
    os.environ.setdefault("HOST_GID", str(os.getgid()))

    if not argv:
        print(USAGE, end="", file=sys.stderr)
        sys.exit(1)
    group, rest = argv[0], argv[1:]
    if group in {"help", "-h", "--help"}:
        print(USAGE, end="")
        return
    fn = GROUPS.get(group)
    if fn is None:
        die(f"unknown group: {group}", 1, with_usage=True)
    fn(rest)


if __name__ == "__main__":
    try:
        main(sys.argv[1:])
    except KeyboardInterrupt:
        sys.exit(130)
