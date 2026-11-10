# syntax=docker/dockerfile:1.7
# WAVEWATCH III (WW3) development environment. Usage: README.md (`./dev`).
#
# Dependencies mirror the CI (.github/workflows/gnu.yml): the Spack environment
# of model/ci/spack_gnu.yaml plus mpich 3.4.2, and OASIS3-MCT built from
# regtests/ww3_tp2.14/input/oasis3-mct.
#
# Layer order keeps the slow Spack layers (~30 min on 32 cores) cached: a tool
# added to step 6 or a different HOST_UID/GID only rebuilds the layers after
# it. RUN stays on /bin/sh: in bash, UID is a read-only shell variable and
# step 7 would break.
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Asia/Tokyo \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# ---------------------------------------------------------------------------
# 1. Compilers (Ubuntu's GCC 13) and Spack prerequisites. cmake is registered
#    as a Spack external, as in the CI.
# ---------------------------------------------------------------------------
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential gfortran \
        cmake pkg-config \
        python3 git curl ca-certificates \
        patch file unzip bzip2 xz-utils zstd gzip tar

# ---------------------------------------------------------------------------
# 2. Spack: same fork and tag as the CI
# ---------------------------------------------------------------------------
ENV SPACK_ROOT=/opt/spack \
    PATH=/opt/spack/bin:${PATH}
RUN git clone -c feature.manyFiles=true --depth=2 --branch=spack-stack-1.9.3 \
        https://github.com/JCSDA/spack.git "${SPACK_ROOT}"

# ---------------------------------------------------------------------------
# 3. Spack environment ww3-gnu: the CI setup job's commands, run through
#    `spack -e` instead of shell activation (same effect on the env's config).
#    The install is split in three layers so that a late failure does not
#    discard the earlier ones; source tarballs stay in a BuildKit cache mount.
# ---------------------------------------------------------------------------
COPY model/ci/spack_gnu.yaml /opt/ww3-ci/spack_gnu.yaml
RUN spack env create ww3-gnu /opt/ww3-ci/spack_gnu.yaml && \
    spack -e ww3-gnu compiler find && \
    spack -e ww3-gnu external find cmake && \
    spack -e ww3-gnu add mpich@3.4.2 && \
    spack -e ww3-gnu concretize
RUN --mount=type=cache,target=/opt/spack/var/spack/cache \
    spack -e ww3-gnu install --dirty --fail-fast -j "$(nproc)" \
        mpich netcdf-c netcdf-fortran
RUN --mount=type=cache,target=/opt/spack/var/spack/cache \
    spack -e ww3-gnu install --dirty --fail-fast -j "$(nproc)" \
        parallelio metis parmetis scotch jasper g2 bacio w3emc
# Remaining roots (esmf) and the view. `clean -b` drops the bootstrap store
# under /root, which the non-root user cannot read anyway.
RUN --mount=type=cache,target=/opt/spack/var/spack/cache \
    spack -e ww3-gnu install --dirty --fail-fast -j "$(nproc)" && \
    spack clean -smpb

# ---------------------------------------------------------------------------
# 4. Runtime environment, loaded by docker/entrypoint.sh (and by interactive
#    bash via /etc/bash.bashrc, for `docker exec`):
#    - the Spack activation (view PATH, CMAKE_PREFIX_PATH, ESMFMKFILE, MPICC...)
#      frozen at build time, so containers never run Spack;
#    - the variables the CI exports before building WW3.
#    The activation's LD_LIBRARY_PATH is dropped: the view also holds Spack's
#    OpenSSL, zlib-ng, ncurses, readline... that system tools (git, wget)
#    must not load. Spack-built programs and the MPI wrappers set RPATHs, and
#    installed WW3 programs keep theirs through CMAKE_OPTIONS, which
#    run_cmake_test (before its `make install`) and `./dev build` pass to cmake.
# ---------------------------------------------------------------------------
RUN set -e; \
    spack env activate --sh ww3-gnu > /opt/ww3-env.sh; \
    sed -i '/^export LD_LIBRARY_PATH=/d' /opt/ww3-env.sh; \
    if grep -q LD_LIBRARY_PATH /opt/ww3-env.sh; then \
        echo 'LD_LIBRARY_PATH left in /opt/ww3-env.sh' >&2; exit 1; \
    fi; \
    { echo 'export CC=mpicc FC=mpif90'; \
      echo 'export OASISDIR=/opt/oasis3-mct'; \
      echo 'export CMAKE_OPTIONS=-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON'; \
      echo 'export WW3_ENV_LOADED=1'; } >> /opt/ww3-env.sh; \
    bash -n /opt/ww3-env.sh; \
    grep -q ESMFMKFILE /opt/ww3-env.sh; \
    grep -q '\.spack-env/view/bin' /opt/ww3-env.sh; \
    echo '[ -n "$WW3_ENV_LOADED" ] || . /opt/ww3-env.sh' >> /etc/bash.bashrc

# ---------------------------------------------------------------------------
# 5. OASIS3-MCT -> /opt/oasis3-mct (= OASISDIR), as in the CI's build-oasis
#    step. Besides its own sources it only reads WW3's FindNetCDF.cmake through
#    $WWATCH3_DIR/../cmake (so $WWATCH3_DIR must exist). The cleanup line is the
#    one regtests/ww3_tp2.14/input_*/prep_env.sh runs before the same build.
# ---------------------------------------------------------------------------
COPY cmake/FindNetCDF.cmake /opt/ww3-oasis-build/cmake/FindNetCDF.cmake
COPY regtests/ww3_tp2.14/input/oasis3-mct/ /opt/ww3-oasis-build/oasis3-mct/
RUN set -e; \
    . /opt/ww3-env.sh; \
    export WWATCH3_DIR=/opt/ww3-oasis-build/model; \
    export OASIS_INPUT_PATH=/opt/ww3-oasis-build/oasis3-mct; \
    export OASIS_WORK_PATH=/opt/oasis3-mct; \
    mkdir -p "${WWATCH3_DIR}"; \
    cd "${OASIS_INPUT_PATH}/util/make_dir"; \
    rm -rf Makefile CMakeCache.txt CMakeFiles cmake_install.cmake cmplr src tmp; \
    cmake .; \
    make; \
    # TopMakefileOasis3 sends compiler output to COMP.log / COMP.err and can
    # exit 0 on failure, so check the products FindOASIS.cmake looks for.
    if [ ! -f "${OASIS_WORK_PATH}/lib/libpsmile.MPI1.a" ] || \
       [ ! -f "${OASIS_WORK_PATH}/build/lib/psmile.MPI1/mod_oasis.mod" ]; then \
        cat "${OASIS_WORK_PATH}/COMP.err" >&2 || true; \
        exit 1; \
    fi; \
    rm -rf /opt/ww3-oasis-build

# ---------------------------------------------------------------------------
# 6. Tools around builds and regtests: gdb (run_cmake_test -d),
#    bc (run_cmake_test -T), wget (model/bin/ww3_from_ftp.sh),
#    doxygen (-DENABLE_DOCS=ON), ...
# ---------------------------------------------------------------------------
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update && \
    apt-get install -y --no-install-recommends \
        gdb valgrind doxygen bc wget sudo tzdata procps less

COPY --chmod=755 docker/entrypoint.sh /usr/local/bin/ww3-entrypoint

# ---------------------------------------------------------------------------
# 7. Non-root developer user matching host UID/GID. The ARGs are declared here
#    so that a different UID/GID only rebuilds this layer.
# ---------------------------------------------------------------------------
ARG UID=1000
ARG GID=1000
ARG USERNAME=dev
RUN set -eux; \
    # Ubuntu 24.04 ships an "ubuntu" user at UID 1000; remove if it collides.
    if id -u ubuntu >/dev/null 2>&1; then userdel -r ubuntu 2>/dev/null || true; fi; \
    if getent group "${GID}" >/dev/null; then \
        existing_group="$(getent group "${GID}" | cut -d: -f1)"; \
        if [ "${existing_group}" != "${USERNAME}" ]; then groupmod -n "${USERNAME}" "${existing_group}"; fi; \
    else \
        groupadd --gid "${GID}" "${USERNAME}"; \
    fi; \
    useradd --uid "${UID}" --gid "${GID}" --shell /bin/bash --create-home "${USERNAME}"; \
    usermod -aG sudo "${USERNAME}"; \
    echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/"${USERNAME}"; \
    chmod 0440 /etc/sudoers.d/"${USERNAME}"; \
    install -d -o "${USERNAME}" -g "${USERNAME}" /workspace

USER dev
WORKDIR /workspace

# PID 1 init comes from compose.yaml (`init: true`); the entrypoint only loads
# /opt/ww3-env.sh and execs the command.
ENTRYPOINT ["/usr/local/bin/ww3-entrypoint"]
CMD ["bash"]
