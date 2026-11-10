#!/usr/bin/env python3
"""GIF animation of a ww3_ounf field over all (or selected) time records.

Each frame is the same map as plot_fields.py (field, wave-direction arrows,
input boundary points, wind in the title) with a fixed colour scale.

Input : the ww3_ounf NetCDF in <work_dir> (the *.nc that has the variable;
        --file to choose).
Output: <work_dir>/figs/<var>.gif

Usage:
  ./dev tools animate_fields runs/<case>/work
  ./dev tools animate_fields runs/<case>/work --var hs --fps 6
"""
from __future__ import annotations

import argparse
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import ww3nc  # noqa: E402
from ww3nc import plt  # noqa: E402

import netCDF4  # noqa: E402
from matplotlib.animation import FuncAnimation, PillowWriter  # noqa: E402


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("work_dir", type=pathlib.Path, help="run directory (runs/<case>/work)")
    ap.add_argument("--file", help="NetCDF file (path or glob in work_dir); default: the *.nc that has --var")
    ap.add_argument("--var", default="hs", help="variable to animate (default hs)")
    ap.add_argument("--hours", type=float, nargs="+",
                    help="frames, hours after the first record (default: every record)")
    ap.add_argument("--fps", type=float, default=4, help="frames per second (default 4)")
    ap.add_argument("--arrow-step", type=int, default=8,
                    help="draw a direction arrow every N grid points; 0 = none (default 8)")
    ap.add_argument("--vmin", type=float, help="colour scale minimum (default: data minimum)")
    ap.add_argument("--vmax", type=float, help="colour scale maximum (default: data maximum)")
    ap.add_argument("--cmap", default="viridis", help="matplotlib colormap (default viridis)")
    args = ap.parse_args()

    path = ww3nc.find_nc(args.work_dir, args.file, needs=(args.var,))
    out = ww3nc.figs_dir(args.work_dir) / f"{args.var}.gif"
    with netCDF4.Dataset(path) as ds:
        for note in ww3nc.missing_overlays(ds, args.arrow_step):
            print(f"note: {note}")
        times = ww3nc.read_times(ds)
        its = list(range(len(times))) if args.hours is None else ww3nc.select_times(times, args.hours)
        if args.vmin is None or args.vmax is None:
            lo, hi = float("inf"), float("-inf")
            for it in its:  # one record at a time: the frames are not kept in memory
                f = ww3nc.read_field(ds, args.var, it)
                lo, hi = min(lo, float(f.min())), max(hi, float(f.max()))
        vmin = args.vmin if args.vmin is not None else lo
        vmax = args.vmax if args.vmax is not None else hi
        if vmax <= vmin:
            vmax = vmin + 1e-6

        fig, ax = plt.subplots(figsize=(5.6, 5.0), constrained_layout=True)
        mesh = ww3nc.draw_field_map(ax, ds, args.var, its[0], times, vmin=vmin, vmax=vmax,
                                    cmap=args.cmap, arrow_step=args.arrow_step)
        fig.colorbar(mesh, ax=ax, shrink=0.85, label=ww3nc.label_of(ds, args.var))

        def frame(k: int):
            ax.clear()
            ww3nc.draw_field_map(ax, ds, args.var, its[k], times, vmin=vmin, vmax=vmax,
                                 cmap=args.cmap, arrow_step=args.arrow_step)
            return []

        anim = FuncAnimation(fig, frame, frames=len(its), blit=False)
        anim.save(out, writer=PillowWriter(fps=args.fps), dpi=100)
    print(f"wrote {out} ({len(its)} frames, range {vmin:.4g}-{vmax:.4g})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
