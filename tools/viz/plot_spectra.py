#!/usr/bin/env python3
"""Directional spectra of ww3_ounp output points at selected times.

For each station: polar panels of E(f, theta) at the selected times (north up,
clockwise; the angle is the direction waves travel TO, the radius is
frequency) and, below, the frequency spectrum E(f) = sum_theta E dtheta for the
same times on one axis. The polar panels share one colour scale.

Input : the ww3_ounp spectra NetCDF in <work_dir> (POINT%TYPE = 1; the *.nc
        that has efth; --file to choose).
Output: <work_dir>/figs/spectra_<station>.png (one file per station)

Usage:
  ./dev tools plot_spectra runs/<case>/work
  ./dev tools plot_spectra runs/<case>/work --station C50 --hours 0 6 12 24
"""
from __future__ import annotations

import argparse
import math
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import ww3nc  # noqa: E402
from ww3nc import plt  # noqa: E402

import netCDF4  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib.colors import LogNorm, Normalize  # noqa: E402


def frequency_edges(ds: netCDF4.Dataset, freq: np.ndarray) -> np.ndarray:
    if "frequency1" in ds.variables and "frequency2" in ds.variables:
        f1 = np.asarray(ds.variables["frequency1"][:], dtype=float)
        f2 = np.asarray(ds.variables["frequency2"][:], dtype=float)
        return np.concatenate([f1[:1], f2])
    mid = np.sqrt(freq[:-1] * freq[1:])
    return np.concatenate([[freq[0] ** 2 / mid[0]], mid, [freq[-1] ** 2 / mid[-1]]])


def plot_station(ds, times, its, ist: int, name: str, fmax: float, log_decades: float,
                 out: pathlib.Path) -> None:
    n = len(its)
    fig = plt.figure(figsize=(3.6 * n, 7.2), constrained_layout=True)
    grid = fig.add_gridspec(2, n, height_ratios=[1.15, 1.0])
    spectra = [ww3nc.spectrum(ds, it, ist) for it in its]
    freq, dirs, _ = spectra[0]
    fedges = frequency_edges(ds, freq)
    dth = 360.0 / len(dirs)
    tedges = np.deg2rad(np.concatenate([dirs - dth / 2, [dirs[-1] + dth / 2]]))
    keep = freq <= fmax
    if not keep.any():
        raise SystemExit(f"error: --fmax {fmax:g} Hz is below the lowest frequency {freq[0]:.4g} Hz")
    vmax = max(float(e[keep].max()) for _, _, e in spectra) or 1.0
    if log_decades > 0:
        norm = LogNorm(vmin=vmax * 10.0 ** -log_decades, vmax=vmax)
        label = f"E(f, theta) [m2 s rad-1], log scale ({log_decades:g} decades)"
    else:
        norm = Normalize(vmin=0.0, vmax=vmax)
        label = "E(f, theta) [m2 s rad-1]"

    mesh = None
    for k, (it, (_, _, efth)) in enumerate(zip(its, spectra)):
        ax = fig.add_subplot(grid[0, k], projection="polar")
        ax.set_theta_zero_location("N")
        ax.set_theta_direction(-1)
        T, R = np.meshgrid(tedges, fedges)
        data = np.ma.masked_less_equal(efth, norm.vmin) if log_decades > 0 else efth
        mesh = ax.pcolormesh(T, R, data, cmap="viridis", norm=norm, shading="flat")
        ax.set_facecolor(plt.cm.viridis(0.0))  # masked (below the scale) shows as the lowest colour
        ax.set_rlim(0.0, fmax)
        ax.set_rlabel_position(200)
        ax.tick_params(labelsize=7)
        ax.set_title(ww3nc.time_label(times, it), fontsize=9)
    fig.colorbar(mesh, ax=fig.axes, location="right", shrink=0.5, label=label)

    ax1 = fig.add_subplot(grid[1, :])
    colors = plt.cm.viridis(np.linspace(0.0, 0.9, n))
    df = np.diff(fedges)
    for color, it, (f, _, efth) in zip(colors, its, spectra):
        ef = efth.sum(axis=1) * np.deg2rad(dth)  # m2 s (= m2 Hz-1)
        hs = 4.0 * math.sqrt(max(float((ef * df).sum()), 0.0))
        ax1.plot(f, ef, color=color, marker=".", label=f"{ww3nc.time_label(times, it)}  Hs~{hs:.2f} m")
    ax1.set_xlim(0.0, fmax)
    ax1.set_xlabel("frequency [Hz]")
    ax1.set_ylabel("E(f) [m2 s]")
    ax1.grid(alpha=0.3)
    ax1.legend(fontsize=8)
    ax1.set_title("frequency spectrum (Hs from the bands, without the tail)", fontsize=9)

    fig.suptitle(f"station {name}: directional spectrum, angle = direction waves travel TO "
                 "(north up, clockwise), radius = frequency [Hz]", fontsize=10)
    fig.savefig(out, dpi=ww3nc.DPI)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("work_dir", type=pathlib.Path, help="run directory (runs/<case>/work)")
    ap.add_argument("--file", help="NetCDF file (path or glob in work_dir); default: the *.nc that has efth")
    ap.add_argument("--station", action="append",
                    help="station name (station_name) or 1-based index; repeatable (default: all)")
    ap.add_argument("--hours", type=float, nargs="+",
                    help="times to show, hours after the first record (default: 4 evenly spaced)")
    ap.add_argument("--fmax", type=float, default=0.5, help="largest frequency shown [Hz] (default 0.5)")
    ap.add_argument("--log-decades", type=float, default=0.0,
                    help="log colour scale spanning N decades below the maximum, e.g. 3 to see a "
                         "weak wind sea next to swell (default 0 = linear)")
    args = ap.parse_args()

    path = ww3nc.find_nc(args.work_dir, args.file, needs=("efth", "station_name"))
    out_dir = ww3nc.figs_dir(args.work_dir)
    with netCDF4.Dataset(path) as ds:
        times = ww3nc.read_times(ds)
        its = ww3nc.select_times(times, args.hours)
        names = ww3nc.station_names(ds)
        if args.station:
            picked = []
            for s in args.station:
                if s in names:
                    picked.append(names.index(s))
                elif s.isdigit() and 1 <= int(s) <= len(names):
                    picked.append(int(s) - 1)
                else:
                    raise SystemExit(f"error: unknown station {s!r} (stations: {', '.join(names)})")
        else:
            picked = list(range(len(names)))
        for ist in picked:
            out = out_dir / f"spectra_{names[ist]}.png"
            plot_station(ds, times, its, ist, names[ist], args.fmax, args.log_decades, out)
            print(f"wrote {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
