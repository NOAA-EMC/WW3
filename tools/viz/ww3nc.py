"""Shared helpers for the WAVEWATCH III NetCDF output of ww3_ounf / ww3_ounp.

Imported by the scripts in tools/viz/ through sys.path (this module is not a
tool by itself). Conventions it handles (see the skill ww3-postprocess,
references/netcdf.md):

- ww3_ounf fields on RECT/CURV grids are (time, y, x) with Cartesian x, y in m,
  or (time, latitude, longitude). `dir`/`dp` are the direction waves come
  FROM, nautical (deg, clockwise from north). Wind is `uwnd`/`vwnd`
  (east/north). Unstructured and SMC output (dimension `node` / `seapoint`) is
  not supported by the map helpers.
- ww3_ounp spectra are efth(time, station, frequency, direction) in
  m2 s rad-1 (dimensions are looked up by name, so POINT%DIMORDER does not
  matter; short output stored as log10 is decoded); `direction` is the
  direction waves travel TO, nautical, and is not monotonic. Cartesian station
  coordinates x/y are in km.
- Time works with any CF calendar (standard, 360_day, ...): records are
  identified by hours after the first record.
- Masked (fill) values come back as numpy masked arrays with NaN underneath.
"""
from __future__ import annotations

import dataclasses
import os
import pathlib
import tempfile
from typing import Sequence

import numpy as np

os.environ.setdefault("MPLCONFIGDIR", os.path.join(tempfile.gettempdir(), "ww3-mpl"))
import matplotlib  # noqa: E402

matplotlib.use("Agg")  # headless container: no display, save files only
import matplotlib.pyplot as plt  # noqa: E402,F401  (re-exported for the tools)
import netCDF4  # noqa: E402

DPI = 130


# ---------------------------------------------------------------------------
# files

def find_nc(work_dir: pathlib.Path, pattern: str | None, needs: Sequence[str]) -> pathlib.Path:
    """Return the NetCDF file to read.

    With `pattern` (a path, or a glob relative to work_dir) the single match is
    used. Without it, the *.nc files in work_dir are searched for one that has
    every variable in `needs` (ww3_ounf / ww3_ounp names depend on PREFIX,
    TIMESPLIT and SAMEFILE, so the content decides).
    """
    if pattern:
        p = pathlib.Path(pattern)
        if p.is_file():
            return p
        matches = sorted(work_dir.glob(pattern))
        if len(matches) != 1:
            raise SystemExit(f"error: --file {pattern!r} matches {len(matches)} files in {work_dir}")
        return matches[0]
    candidates = []
    for path in sorted(work_dir.glob("*.nc")):
        with netCDF4.Dataset(path) as ds:
            if all(v in ds.variables for v in needs):
                candidates.append(path)
    if not candidates:
        raise SystemExit(f"error: no *.nc in {work_dir} has {', '.join(needs)} "
                         "(run ww3_ounf / ww3_ounp first, or give --file)")
    if len(candidates) > 1:
        raise SystemExit("error: several files have " + ", ".join(needs) + ": "
                         + ", ".join(p.name for p in candidates)
                         + " (choose one with --file; TIMESPLIT = 0 in ww3_ounf/ww3_ounp writes one file per run)")
    return candidates[0]


def figs_dir(work_dir: pathlib.Path) -> pathlib.Path:
    out = work_dir / "figs"
    out.mkdir(parents=True, exist_ok=True)
    return out


# ---------------------------------------------------------------------------
# time

_UNIT_HOURS = {"day": 24.0, "hour": 1.0, "minute": 1.0 / 60.0, "second": 1.0 / 3600.0}


@dataclasses.dataclass
class Times:
    labels: list[str]   # "YYYY-MM-DD HH:MM" of each record (in the file's calendar)
    hours: np.ndarray   # hours after the first record

    def __len__(self) -> int:
        return len(self.hours)


def read_times(ds: netCDF4.Dataset) -> Times:
    t = ds.variables["time"]
    raw = np.ma.atleast_1d(np.ma.masked_invalid(np.ma.asarray(t[:], dtype=float)))
    raw = np.ma.masked_greater(raw, 1e30)
    valid = ~np.ma.getmaskarray(raw)
    # Records come first; ww3_ounp with POINT%DIMORDER = F leaves unwritten
    # (fill) records at the end of `time`.
    count = len(raw) if valid.all() else int(np.argmin(valid))
    if count == 0:
        raise SystemExit("error: the file has no time records")
    values = np.asarray(raw[:count])
    calendar = getattr(t, "calendar", "standard")
    dates = netCDF4.num2date(values, t.units, calendar=calendar, only_use_cftime_datetimes=True)
    labels = [d.strftime("%Y-%m-%d %H:%M") for d in np.atleast_1d(dates)]
    unit = t.units.split()[0].lower().rstrip("s")
    if unit not in _UNIT_HOURS:
        raise SystemExit(f"error: unexpected time units {t.units!r}")
    return Times(labels, (values - values[0]) * _UNIT_HOURS[unit])


def select_times(times: Times, hours: Sequence[float] | None, default_count: int = 4) -> list[int]:
    """Indices of the records nearest to `hours` after the first record.

    Without `hours`, `default_count` records evenly spaced from the first to
    the last are taken.
    """
    rel = times.hours
    if hours is None:
        n = min(default_count, len(rel))
        return sorted({int(round(i)) for i in np.linspace(0, len(rel) - 1, n)})
    step = float(np.diff(rel).min()) if len(rel) > 1 else 1.0
    picked = []
    for h in hours:
        i = int(np.argmin(np.abs(rel - h)))
        if abs(rel[i] - h) > 0.5 * max(step, 1e-6):
            raise SystemExit(f"error: no record near +{h:g} h (records span 0 to +{rel[-1]:g} h)")
        picked.append(i)
    return picked


def time_label(times: Times, it: int) -> str:
    return f"{times.labels[it]} (+{times.hours[it]:g} h)"


# ---------------------------------------------------------------------------
# fields (ww3_ounf)

def field_coords(ds: netCDF4.Dataset) -> tuple[np.ndarray, np.ndarray, str, str, str]:
    """2-D X, Y for pcolormesh, the axis labels and the kind of axes.

    kind is "cartesian" (1-D x/y, converted to km), "lonlat" (1-D longitude
    and latitude) or "index" (curvilinear grids with 2-D x/y or 2-D
    longitude/latitude: drawn in grid indices, since plain coordinate axes
    garble them, e.g. near a pole; no map projection is attempted).
    """
    for dim in ("node", "seapoint"):
        if dim in ds.dimensions:
            raise SystemExit(f"error: this is unstructured/SMC output (dimension {dim!r}); "
                             "the map tools only support RECT and CURV grids")
    if "x" in ds.variables and "y" in ds.variables:
        xv, yv = ds.variables["x"], ds.variables["y"]
        if xv.ndim == 1:
            scale = 1e-3 if getattr(xv, "units", "m") == "m" else 1.0
            X, Y = np.meshgrid(np.asarray(xv[:]) * scale, np.asarray(yv[:]) * scale)
            return X, Y, "x [km]", "y [km]", "cartesian"
        coord2d = xv
    elif "longitude" in ds.variables and "latitude" in ds.variables:
        lon, lat = ds.variables["longitude"], ds.variables["latitude"]
        if lon.ndim == 1:
            X, Y = np.meshgrid(np.asarray(lon[:]), np.asarray(lat[:]))
            return X, Y, "longitude [deg]", "latitude [deg]", "lonlat"
        coord2d = lon
    else:
        raise SystemExit("error: no x/y or longitude/latitude coordinates in the file")
    ny, nx = coord2d.shape
    X, Y = np.meshgrid(np.arange(1, nx + 1), np.arange(1, ny + 1))
    return X, Y, "grid index i (curvilinear grid)", "grid index j", "index"


def read_field(ds: netCDF4.Dataset, name: str, it: int) -> np.ma.MaskedArray:
    if name not in ds.variables:
        raise SystemExit(f"error: variable {name!r} is not in the file "
                         f"(request its tag in ww3_shel TYPE%FIELD%LIST and ww3_ounf FIELD%LIST)")
    v = ds.variables[name]
    if v.ndim == 3:            # (time, y, x)
        a = v[it]
    elif v.ndim == 4:          # (time, level, y, x)
        a = v[it, 0]
    else:
        raise SystemExit(f"error: {name!r} has dimensions {v.dimensions}; the map tools need (time, y, x)")
    # NaN under the mask: matplotlib scales the data under masked points too,
    # and the fill value (9.97e36) would overflow there.
    return np.ma.masked_invalid(np.ma.asarray(a, dtype=float).filled(np.nan))


def label_of(ds: netCDF4.Dataset, name: str) -> str:
    v = ds.variables[name]
    long_name = getattr(v, "long_name", name)
    units = getattr(v, "units", "")
    return f"{long_name} [{units}]" if units else long_name


def from_dir_to_vector(from_deg: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Unit vector (east, north) of the direction waves travel, from the
    nautical "coming from" direction of ww3_ounf `dir`/`dp`."""
    rad = np.deg2rad(from_deg)
    return -np.sin(rad), -np.cos(rad)


def wind_text(ds: netCDF4.Dataset, it: int) -> str:
    if "uwnd" not in ds.variables or "vwnd" not in ds.variables:
        return ""
    u = float(read_field(ds, "uwnd", it).mean())
    v = float(read_field(ds, "vwnd", it).mean())
    speed = float(np.hypot(u, v))
    from_deg = (270.0 - np.degrees(np.arctan2(v, u))) % 360.0
    return f"U10 {speed:.1f} m/s from {from_deg:.0f} deg"


def missing_overlays(ds: netCDF4.Dataset, arrow_step: int) -> list[str]:
    """Notes for the user about overlays the file cannot provide."""
    notes = []
    if arrow_step > 0 and "dir" not in ds.variables:
        notes.append("no 'dir' in the file: no direction arrows "
                     "(with FIELD%SAMEFILE = F each variable has its own file)")
    if "uwnd" not in ds.variables:
        notes.append("no 'uwnd'/'vwnd' in the file: no wind in the titles "
                     "(needs WND, and FIELD%VECTOR = T in ww3_ounf)")
    return notes


def draw_field_map(ax, ds: netCDF4.Dataset, var: str, it: int, times: Times, *,
                   vmin: float, vmax: float, cmap: str, arrow_step: int):
    """One map panel: the field, wave-direction arrows (if `dir` exists) and
    the input boundary points (MAPSTA = 2). Returns the pcolormesh.
    On curvilinear grids (drawn in grid indices) no arrows are drawn: the grid
    axes are not east/north there."""
    X, Y, xlabel, ylabel, kind = field_coords(ds)
    field = read_field(ds, var, it)
    mesh = ax.pcolormesh(X, Y, field, cmap=cmap, vmin=vmin, vmax=vmax, shading="nearest")
    if "dir" in ds.variables and arrow_step > 0 and kind != "index":
        d = read_field(ds, "dir", it)
        u, v = from_dir_to_vector(d)
        s = (slice(arrow_step // 2, None, arrow_step),) * 2
        ax.quiver(X[s], Y[s], u[s], v[s], color="white", scale=25, width=0.004,
                  headwidth=4, pivot="middle")
    if "MAPSTA" in ds.variables:
        mapsta = ds.variables["MAPSTA"][:]
        bnd = np.asarray(mapsta) == 2
        if bnd.any():
            ax.plot(X[bnd], Y[bnd], "s", color="tab:red", markersize=1.5,
                    label="input boundary")
    title = time_label(times, it)
    wind = wind_text(ds, it)
    ax.set_title(title + ("\n" + wind if wind else ""), fontsize=9)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    if kind in ("cartesian", "index"):
        ax.set_aspect("equal")
    return mesh


# ---------------------------------------------------------------------------
# point spectra (ww3_ounp, POINT%TYPE = 1)

def station_names(ds: netCDF4.Dataset) -> list[str]:
    raw = ds.variables["station_name"][:]
    names = []
    for row in np.ma.filled(raw, b" "):
        names.append(b"".join(row).decode("ascii", "replace").strip().strip("\x00").strip())
    return names


def spectrum(ds: netCDF4.Dataset, it: int, ist: int):
    """Frequencies [Hz], directions [deg, travelling TO, nautical, ascending]
    and E(f, theta) [m2 s rad-1] (frequency x direction, directions sorted)."""
    v = ds.variables["efth"]
    index = tuple(it if d == "time" else ist if d == "station" else slice(None)
                  for d in v.dimensions)
    rest = [d for d in v.dimensions if d not in ("time", "station")]
    if sorted(rest) != ["direction", "frequency"]:
        raise SystemExit(f"error: efth has dimensions {v.dimensions}; expected time, station, frequency, direction")
    if getattr(v, "units", "").startswith("log10"):
        # SPECTRA%TYPE <= 3 stores NINT(log10(E + 1e-12)/0.0004) as short
        # (ww3_ounp.F90:1872-1874). The file also says valid_min = 0, which
        # would mask every (negative) log value, so decode by hand.
        v.set_auto_maskandscale(False)
        try:
            raw = np.asarray(v[index])
        finally:
            v.set_auto_maskandscale(True)
        logv = raw * float(getattr(v, "scale_factor", 1.0)) + float(getattr(v, "add_offset", 0.0))
        efth = 10.0 ** logv - 1e-12
        fill = getattr(v, "_FillValue", None)
        if fill is not None:
            efth = np.where(raw == fill, 0.0, efth)
    else:
        efth = np.ma.filled(np.ma.asarray(v[index], dtype=float), 0.0)
    if rest == ["direction", "frequency"]:
        efth = efth.T
    efth = np.clip(efth, 0.0, None)
    freq = np.asarray(ds.variables["frequency"][:], dtype=float)
    dirs = np.asarray(ds.variables["direction"][:], dtype=float) % 360.0
    order = np.argsort(dirs)
    return freq, dirs[order], efth[:, order]
