#!/usr/bin/env python3
"""Write a ww3_strt.inp (ITYPE 4) with a JONSWAP spectrum and cos^n spreading.

ww3_strt's own JONSWAP option (ITYPE 2) uses the Hasselmann et al. (1980)
spreading and cannot set Hs directly, so this tool builds the spectrum itself
and passes it as a user-defined spectrum (ITYPE 4). The same spectrum is put
at every sea point (and, without nest.ww3, is kept on the boundary points).

The spectrum is F(f, theta) = E(f) D(theta) in m2 Hz-1 rad-1 on the model's
discrete grid (f_k = freq1 * xfr**(k-1), theta_j = (j-1+thoff) * 2pi/nth,
Cartesian "going to"). E(f) is scaled so that Hs computed the way WW3 outputs
it (band widths DSII, plus the f**-5 tail 0.25 f_NK E(f_NK)) equals --hs-m.
The spectral grid options must match SPECTRUM_NML in ww3_grid.nml.

Output: the file given by --out (and a summary on stdout).

Usage:
  ./dev tools jonswap_strt --hs-m 0.5 --tp-s 6 --from-deg 270 --cos-power 2 \\
      --xfr 1.1 --freq1 0.05 --nk 32 --nth 36 --thoff 0 \\
      --out runs/<case>/input/ww3_strt.inp
"""
from __future__ import annotations

import argparse
import math
import pathlib
import shlex
import sys

GRAV = 9.81  # m s-2; the JONSWAP constant g^2 (2 pi)^-4 cancels in the Hs scaling


def frequencies(freq1: float, xfr: float, nk: int) -> tuple[list[float], list[float]]:
    """Band centres and widths [Hz], as SIG/DSII in model/src/w3gridmd.F90."""
    f = [freq1 * xfr**k for k in range(nk)]
    df = [0.5 * fk * (xfr - 1.0 / xfr) for fk in f]
    df[0] = 0.5 * f[0] * (xfr - 1.0)
    df[-1] = 0.5 * f[-1] * (xfr - 1.0) / xfr
    return f, df


def jonswap(f: float, fp: float, gamma: float, sigma_a: float, sigma_b: float) -> float:
    """JONSWAP E(f) with alpha = 1 [m2 Hz-1 per unit alpha]."""
    sigma = sigma_a if f <= fp else sigma_b
    r = math.exp(-((f - fp) ** 2) / (2.0 * sigma**2 * fp**2))
    return GRAV**2 * (2.0 * math.pi) ** -4 * f**-5 * math.exp(-1.25 * (fp / f) ** 4) * gamma**r


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--hs-m", type=float, required=True, help="significant wave height [m]")
    ap.add_argument("--tp-s", type=float, required=True, help="peak period [s]")
    ap.add_argument("--gamma", type=float, default=3.3, help="peak enhancement factor (default 3.3)")
    ap.add_argument("--sigma-a", type=float, default=0.07, help="peak width below fp (default 0.07)")
    ap.add_argument("--sigma-b", type=float, default=0.09, help="peak width above fp (default 0.09)")
    ap.add_argument("--from-deg", type=float, required=True,
                    help="mean direction the waves come FROM, nautical (deg, clockwise from north)")
    ap.add_argument("--cos-power", type=int, default=2, help="n in cos^n spreading (default 2)")
    ap.add_argument("--xfr", type=float, required=True, help="SPECTRUM%%XFR")
    ap.add_argument("--freq1", type=float, required=True, help="SPECTRUM%%FREQ1 [Hz]")
    ap.add_argument("--nk", type=int, required=True, help="SPECTRUM%%NK")
    ap.add_argument("--nth", type=int, required=True, help="SPECTRUM%%NTH")
    ap.add_argument("--thoff", type=float, default=0.0, help="SPECTRUM%%THOFF (default 0)")
    ap.add_argument("--out", type=pathlib.Path, required=True, help="ww3_strt.inp to write")
    args = ap.parse_args()

    if args.hs_m <= 0 or args.tp_s <= 0:
        ap.error("--hs-m and --tp-s must be positive")
    if args.cos_power < 1:
        ap.error("--cos-power must be >= 1")
    thoff = max(-0.5, min(0.5, args.thoff))  # ww3_grid clips THOFF the same way

    f, df = frequencies(args.freq1, args.xfr, args.nk)
    fp = 1.0 / args.tp_s
    if not f[0] < fp < f[-1]:
        ap.error(f"fp = {fp:.4f} Hz is outside the model frequencies {f[0]:.4f}-{f[-1]:.4f} Hz")

    # Directions: Cartesian, the direction waves travel to (w3gridmd.F90 TH).
    dth = 2.0 * math.pi / args.nth
    th = [(j + thoff) * dth for j in range(args.nth)]
    th0 = math.radians((270.0 - args.from_deg) % 360.0)  # nautical "from" -> Cartesian "to"
    d = [max(math.cos(t - th0), 0.0) ** args.cos_power for t in th]
    norm = sum(d) * dth
    d = [v / norm for v in d]  # sum(D) * dth = 1 [rad-1]

    # Frequency spectrum scaled to Hs as WW3 outputs it (with the tail term).
    e = [jonswap(fk, fp, args.gamma, args.sigma_a, args.sigma_b) for fk in f]
    m0 = sum(ek * dk for ek, dk in zip(e, df)) + 0.25 * f[-1] * e[-1]
    scale = (args.hs_m / 4.0) ** 2 / m0
    e = [ek * scale for ek in e]

    # Checks printed for the user: Hs, peak band, mean direction and spread
    # (spread as WW3's SPR: sqrt(2 (1 - |mean unit vector|)) in degrees).
    m0 = sum(ek * dk for ek, dk in zip(e, df)) + 0.25 * f[-1] * e[-1]
    a1 = sum(dj * math.cos(t) for dj, t in zip(d, th)) * dth
    b1 = sum(dj * math.sin(t) for dj, t in zip(d, th)) * dth
    spread = math.degrees(math.sqrt(max(0.0, 2.0 * (1.0 - math.hypot(a1, b1)))))
    mean_from = (270.0 - math.degrees(math.atan2(b1, a1))) % 360.0
    kpeak = max(range(args.nk), key=lambda k: e[k])

    command = "./dev tools jonswap_strt " + " ".join(shlex.quote(a) for a in sys.argv[1:])
    lines = [
        "$ WAVEWATCH III initial conditions, ITYPE 4 (user-defined spectrum)",
        "$ Generated by tools/prep/jonswap_strt.py; do not edit by hand. Command:",
        f"$   {command}",
        f"$ JONSWAP Hs = {args.hs_m:g} m, Tp = {args.tp_s:g} s, gamma = {args.gamma:g}, "
        f"cos^{args.cos_power} from {args.from_deg:g} deg (nautical)",
        f"$ Spectral grid: XFR = {args.xfr:g}, FREQ1 = {args.freq1:g} Hz, NK = {args.nk}, "
        f"NTH = {args.nth}, THOFF = {thoff:g}",
        "$ Values: F(f,theta) [m2 Hz-1 rad-1]; NK values (frequency fastest) per direction,",
        "$ directions j = 1..NTH (Cartesian, going to). No comment lines inside the block.",
        "$",
        "   4",
        "   1.0",
    ]
    for j in range(args.nth):
        # Values far below single precision (WW3 reads REALs) are written as 0.
        row = [v if v >= 1e-30 else 0.0 for v in (e[k] * d[j] for k in range(args.nk))]
        for i in range(0, args.nk, 8):
            lines.append(" " + " ".join(f"{v:12.5e}" for v in row[i:i + 8]))
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text("\n".join(lines) + "\n", encoding="ascii")

    print(f"wrote {args.out}")
    print(f"  Hs (with tail)   = {4.0 * math.sqrt(m0):.4f} m")
    print(f"  peak band        = {f[kpeak]:.4f} Hz (Tp = {1.0 / f[kpeak]:.2f} s; requested fp = {fp:.4f} Hz)")
    print(f"  mean direction   = {mean_from:.1f} deg (from, nautical)")
    print(f"  directional spread (SPR) = {spread:.1f} deg")
    return 0


if __name__ == "__main__":
    sys.exit(main())
