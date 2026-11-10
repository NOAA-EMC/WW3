#!/usr/bin/env python3
"""Maps of a ww3_ounf field at selected times, one panel per time.

Each panel shows the field (default Hs), arrows for the direction the waves
travel (from `dir`, if present), the input boundary points (MAPSTA = 2) and
the mean 10 m wind in the title. All panels share one colour scale.

Input : the ww3_ounf NetCDF in <work_dir> (the *.nc that has the variable;
        --file to choose).
Output: <work_dir>/figs/<var>_maps.png

Usage:
  ./dev tools plot_fields runs/<case>/work
  ./dev tools plot_fields runs/<case>/work --var t02 --hours 0 6 12 24
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("work_dir", type=pathlib.Path, help="run directory (runs/<case>/work)")
    ap.add_argument("--file", help="NetCDF file (path or glob in work_dir); default: the *.nc that has --var")
    ap.add_argument("--var", default="hs", help="variable to map (default hs)")
    ap.add_argument("--hours", type=float, nargs="+",
                    help="times to show, hours after the first record (default: 4 evenly spaced)")
    ap.add_argument("--arrow-step", type=int, default=8,
                    help="draw a direction arrow every N grid points; 0 = none (default 8)")
    ap.add_argument("--vmin", type=float, help="colour scale minimum (default: data minimum)")
    ap.add_argument("--vmax", type=float, help="colour scale maximum (default: data maximum)")
    ap.add_argument("--cmap", default="viridis", help="matplotlib colormap (default viridis)")
    args = ap.parse_args()

    path = ww3nc.find_nc(args.work_dir, args.file, needs=(args.var,))
    with netCDF4.Dataset(path) as ds:
        for note in ww3nc.missing_overlays(ds, args.arrow_step):
            print(f"note: {note}")
        times = ww3nc.read_times(ds)
        its = ww3nc.select_times(times, args.hours)
        fields = [ww3nc.read_field(ds, args.var, it) for it in its]
        vmin = args.vmin if args.vmin is not None else float(min(f.min() for f in fields))
        vmax = args.vmax if args.vmax is not None else float(max(f.max() for f in fields))
        if vmax <= vmin:
            vmax = vmin + 1e-6

        ncols = min(4, len(its))
        nrows = math.ceil(len(its) / ncols)
        fig, axes = plt.subplots(nrows, ncols, figsize=(4.2 * ncols, 4.2 * nrows + 0.6),
                                 squeeze=False, constrained_layout=True)
        mesh = None
        for ax, it in zip(axes.flat, its):
            mesh = ww3nc.draw_field_map(ax, ds, args.var, it, times, vmin=vmin, vmax=vmax,
                                        cmap=args.cmap, arrow_step=args.arrow_step)
        for ax in list(axes.flat)[len(its):]:
            ax.set_visible(False)
        fig.colorbar(mesh, ax=axes, shrink=0.85, label=ww3nc.label_of(ds, args.var))
        fig.suptitle(f"{args.var} ({path.name})  arrows: direction waves travel",
                     fontsize=10)

    out = ww3nc.figs_dir(args.work_dir) / f"{args.var}_maps.png"
    fig.savefig(out, dpi=ww3nc.DPI)
    print(f"wrote {out} (times: {', '.join(str(i) for i in its)}; range {vmin:.4g}-{vmax:.4g})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
