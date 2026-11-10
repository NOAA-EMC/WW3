---
name: ww3-postprocess
description: "WW3 のシミュレーション結果をポスト処理する skill。ww3_ounf・ww3_ounp で NetCDF にした出力を、tools/viz のスクリプトで図にする (場の平面図・アニメーション・方向スペクトル)。仕様書 (runs/<case>/SPEC.md) の 10 節「ポスト処理」の図の表を、ケースの postprocess.sh で実行する。次のようなときに使う: 結果を図にしたい / Hs や周期・波向の分布を見たい / 時間変化をアニメーションにしたい / 地点の方向スペクトルを見たい / postprocess.sh を書きたい / 図が出ない・おかしい / NetCDF の変数名・単位・方向の規約を知りたい / 新しい図の種類を足したい。"
---

# ww3-postprocess

ポスト処理は「NetCDF への変換」と「可視化」の 2 段。何をするかはケースの仕様書 `runs/<case>/SPEC.md` の 10 節で決め
(skill `ww3-sim-spec`)、この skill のツールで実行する。

```
ww3_shel ─ out_grd.ww3 ─ ww3_ounf ─ work/*.nc (格子) ─┐
         └ out_pnt.ww3 ─ ww3_ounp ─ work/*.nc (点)  ─┴─ postprocess.sh (tools/viz) ─ work/figs/
           (ここまで run.sh が実行する)                    (仕様書 10.2 節の図の表どおり)
```

## 図のツール

`./dev tools <name>` でコンテナ内の Python (numpy・matplotlib・netCDF4) が動く。`<work>` は `runs/<case>/work` (repo root からのパス)。
図は `<work>/figs/` に出る。オプションは `./dev tools <name> --help`。

| 図 | コマンド | 出力 |
|---|---|---|
| 場の平面図を数時刻並べる (波の進む向きの矢印・境界点・風つき) | `./dev tools plot_fields <work> [--var hs] [--hours 0 6 12 24] [--arrow-step 8] [--vmin --vmax --cmap]` | `figs/<var>_maps.png` |
| 場のアニメーション (GIF) | `./dev tools animate_fields <work> [--var hs] [--fps 4] [--hours …]` | `figs/<var>.gif` |
| 地点の方向スペクトル (極座標) と周波数スペクトル | `./dev tools plot_spectra <work> [--station C50] [--hours …] [--fmax 0.5] [--log-decades 3]` | `figs/spectra_<station>.png` |

- 入力の NetCDF は、作業ディレクトリの `*.nc` から必要な変数を持つものを自動で選ぶ (複数あれば `--file`)。
- `--hours` は最初の記録からの時間。省くと 4 時刻 (アニメーションは全時刻)。
- 色の範囲は選んだ時刻全体で共通 (時刻の比較ができるように)。
- 座標軸: 直交格子は km、経緯度の正規格子は度で描く。2 次元の座標を持つ曲線格子 (例: regtest `ww3_tp2.5` の北極) は、
  座標のまま描くと崩れるので格子番号 (i, j) の平面で描き、矢印は描かない (地図の投影は未対応)。
  非構造格子と SMC 格子 (次元 `node`・`seapoint`) の場の図は未対応で、エラーで止まる。
- `./dev tools` が `ModuleNotFoundError: No module named 'numpy'` などで止まるときは、イメージが描画用の Python を足す前のもの。
  `./dev docker build` で作り直す (Spack の層はキャッシュが効くので数分)。
- 初期スペクトルを作る前処理のツール `./dev tools jonswap_strt` もある (skill `ww3-run-config` の `references/boundary.md`)。

## postprocess.sh

ケースの仕様書 10.2 節の図の表を、1 行 = 1 図で書く。行末のコメントに図の ID を書き、表と 1 対 1 にする。
コンテナ内で `python3 tools/viz/<tool>.py` を直接呼ぶ。下は雛形 (実際の例は `runs/sample-jonswap/postprocess.sh`)。

```bash
#!/bin/bash
# 実行 (run.sh の後): ./dev docker run -- bash runs/<case>/postprocess.sh
set -e
CASE=<case>
WORK=${WORK:-work}
W=runs/$CASE/$WORK
cd /workspace

python3 tools/viz/plot_fields.py "$W" --var hs --hours 0 6 12 24          # V1
python3 tools/viz/animate_fields.py "$W" --var hs                         # V2
python3 tools/viz/plot_spectra.py "$W" --station C50 --log-decades 3      # V3
```

## 図に必要な出力の要求

図の材料は、実行の前に設定ファイルで要求しておく (書き方は skill `ww3-run-config` の `references/shel-output.md`):

| 図 | 要るもの |
|---|---|
| 場の図 | ww3_shel の `TYPE%FIELD%LIST` と ww3_ounf の `FIELD%LIST` にその変数のタグ。矢印には `DIR`、風の表示には `WND` (`FIELD%VECTOR = T`、既定) |
| 方向スペクトル | ww3_shel の点出力 (`TYPE%POINT%FILE` と `DATE%POINT`)、ww3_ounp の `POINT%TYPE = 1` (`SPECTRA%OUTPUT = 3`) |
| 1 つのファイルにまとめる | ww3_ounf は `FIELD%TIMESPLIT = 0` と `FIELD%SAMEFILE = T`、ww3_ounp は `POINT%TIMESPLIT = 0`。ツールは 1 つのファイルを読むので、期間で分かれると一部しか描けず、変数ごとに分かれると矢印や風が出ない (そのときは注記を表示する) |
| 値の精度 | ww3_ounf は `FIELD%TYPE = 4` (float。既定の short だと fp が 0.001 Hz、dp が 1° 刻みになる)。ww3_ounp は `SPECTRA%TYPE = 4` (float、既定)。short (log10) で書いたスペクトルもツールは戻して描く |
| 終わらせる | ww3_ounf・ww3_ounp とも `TIMESTRIDE` を必ず書く (既定の `'0'` だと終わらないことがある) |

変数名・単位・方向の規約・座標・時刻は [`references/netcdf.md`](references/netcdf.md)。特に間違えやすいのは次の 3 つ:

- ww3_ounf の `dir`・`dp` は**来る方向**、ww3_ounp のスペクトルの `direction` は**向かう方向** (どちらも北から時計回り)。
  ツールは矢印・極座標とも「波の進む向き」で描く。
- 直交格子の座標は、ww3_ounf が m、ww3_ounp が km。ツールはどちらも km で描く。
- `fp` は周波数 (Hz) で、周期ではない。

## 図を確かめる

- t=0 の図が初期条件 (仕様書 5 節) と合う (例: 一様な Hs、波向、方向スペクトルの山の位置)。
- 矢印・スペクトルの向きが、風や境界から期待される向きになっている (来る/向かうの取り違えが無いか)。
- 除外点・陸は白く抜け、境界点 (`MAPSTA = 2`) は赤い点で示される。
- 色の範囲が極端な値に引きずられていないか (必要なら `--vmin`/`--vmax`。スペクトルは `--log-decades`)。
- 確かめた結果は仕様書 11 節に書く。

## 新しい図を足す

1. `tools/viz/plot_<subject>.py` (アニメーションなら `animate_<subject>.py`) を作る。既存のスクリプトにそろえる:
   shebang、docstring に Input・Output・Usage、argparse、`main() -> int`、`sys.path` で `ww3nc` を読み込む、英語のラベル、
   `ww3nc.DPI`、色は viridis。
2. NetCDF の読み込みは `ww3nc` の関数 (`find_nc`・`read_times`・`select_times`・`read_field`・`field_coords`・`spectrum`・
   `figs_dir`) を使い、約束事 (方向・単位・欠損) を各スクリプトで書き直さない。足りない処理は `ww3nc.py` に足す。
3. `runs/sample-jonswap` などの実際の出力で動かし、図を目で確かめる。
4. 使うケースの仕様書 10.2 節と `postprocess.sh` に行を足し、この SKILL.md の「図のツール」の表にも足す。

## 使わない場面

- 何を出力するか・ww3_ounf/ww3_ounp の設定キーを調べる → skill `ww3-run-config`
- どの図を作るかを決める・仕様書を書く → skill `ww3-sim-spec`
- regtest の結果を比べる → LLM.md §5 (`matrix.comp`)

## このスキルの保守

このスキルとツールは使いながら直していく。

- 使っていて**不足** (欲しい図・オプション・変数の説明が無い) や**誤り** (書いたとおりに動かない、図が規約と食い違う) に
  気づいたら、その作業の中で SKILL.md・`references/netcdf.md`・`tools/viz/` を直す。
- 書く前に確かめる。根拠はソースの `file:line` か、実際の出力 (`ncdump -h`) と図にする。確かめていないことは書かないか、
  「未確認」と明記する。
- 直したことは、その作業の報告でユーザーに伝える。他の skill (`ww3-run-config`・`ww3-sim-spec`) や LLM.md と食い違ったら、
  両方を直す。
