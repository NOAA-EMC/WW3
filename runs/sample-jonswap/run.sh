#!/bin/bash
# 実行: ./dev docker run -- bash runs/<case>/run.sh
# restart から続ける: ./dev docker run -- env WORK=work2 RESTART=work/restart001.ww3 bash runs/<case>/run.sh
set -e
CASE=sample-jonswap
MPI="mpirun -np 8"            # switch が SHRD なら MPI=""
WORK=${WORK:-work}            # 作業ディレクトリ (runs/*/work*/ は ignore)。毎回作り直す
RESTART=${RESTART:-}          # 続けるときの restart (runs/<case>/ からのパス)。WORK の外に置く
B=/workspace/build/$CASE/bin
cd /workspace/runs/$CASE
case "$RESTART" in "$WORK"/*) echo "RESTART must be outside $WORK" >&2; exit 1 ;; esac
rm -rf "$WORK" && mkdir "$WORK" && cd "$WORK"

# exe <prog> <入力> [launcher ...]: 入力を <prog>.nml|inp にリンクして実行し、<入力の名前>.out を検査する
exe() {
  local p=$1 f=$2 ext=${2##*.} o; shift 2
  o=$(basename "$f" ".$ext").out
  rm -f "$p.nml" "$p.inp" && ln -s "$f" "$p.$ext"
  "$@" "$B/$p" > "$o"
  if grep -q "ERROR" "$o"; then echo "ERROR in $WORK/$o" >&2; exit 1; fi
}
# run <prog> [launcher ...]: input/<prog>.nml (無ければ .inp) があれば実行する
run() {
  local p=$1 f; shift
  for f in "../input/$p.nml" "../input/$p.inp"; do
    if [ -e "$f" ]; then exe "$p" "$f" "$@"; return; fi
  done
}
# runeach <prog>: input/<prog>*.nml (無ければ *.inp) を 1 つずつ実行する
runeach() {
  local p=$1 ext f
  for ext in nml inp; do
    compgen -G "../input/$p*.$ext" > /dev/null || continue
    for f in ../input/$p*.$ext; do exe "$p" "$f"; done
    return
  done
}

run ww3_grid
if [ -n "$RESTART" ]; then cp "../$RESTART" restart.ww3; else run ww3_strt; fi
run ww3_bounc                 # うねり: NetCDF のスペクトル
run ww3_bound                 # うねり: ASCII のスペクトル
runeach ww3_prep              # 強制場: ASCII (種類ごとに ww3_prep_<field>.inp)
runeach ww3_prnc              # 強制場: NetCDF (種類ごとに ww3_prnc_<field>.nml)
run ww3_shel $MPI
runeach ww3_ounf
runeach ww3_ounp
echo "finished: $(ls *.out | tr '\n' ' ')"
