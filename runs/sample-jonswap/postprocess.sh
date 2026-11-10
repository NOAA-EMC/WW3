#!/bin/bash
# Post-processing (visualization) of runs/sample-jonswap: the figures V1-V4 of SPEC.md section 10.2.
# Run after run.sh:  ./dev docker run -- bash runs/sample-jonswap/postprocess.sh
# Figures go to runs/sample-jonswap/$WORK/figs/.
set -e
CASE=sample-jonswap
WORK=${WORK:-work}           # the run directory run.sh wrote (runs/*/work*/ is ignored)
W=runs/$CASE/$WORK
cd /workspace

python3 tools/viz/plot_fields.py "$W" --var hs --hours 0 2 6 24           # V1
python3 tools/viz/animate_fields.py "$W" --var hs                         # V2
python3 tools/viz/plot_spectra.py "$W" --station C50 --hours 0 2 6 24 --log-decades 3   # V3
python3 tools/viz/plot_fields.py "$W" --var t02 --hours 0 2 6 24          # V4
