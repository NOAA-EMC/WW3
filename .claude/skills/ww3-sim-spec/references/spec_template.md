# シミュレーション仕様書テンプレート

`runs/<case>/SPEC.md` の雛形。下の「---」より後をコピーして埋める。記入例は `runs/sample-jonswap/SPEC.md`。

書き方:

- 仕様書がケースの一次の出典。`input/` の設定ファイル・`run.sh`・`postprocess.sh` は仕様書に合わせる。
  設定を変えるときは仕様書を先に直し、12 節に記録する。
- 値の表は「項目 | 値 | 設定ファイル・キー」の 3 列にする。キーは `ww3_grid.nml` の `RECT%SX` のように、ファイルと変数名で書く。
  設定ファイルの書き方は skill `ww3-run-config` を見る。
- 単位を必ず書く。方向は「来る方向 / 向かう方向」と「北から時計回り / Cartesian」を明記する。
- 使わない項目も行を消さず、「なし」と書く (検討して使わないことが分かるように)。
- 確かめていないことは「未確認」と書く。
- `<...>` は記入する箇所、`例:` で始まる値は書き換える。

---

# <case> シミュレーション仕様書

> ケース: `runs/<case>/`。設定ファイルは `input/`、実行は `run.sh`、ポスト処理は `postprocess.sh`。
> 実行: `./dev build runs/<case>/input/switch_<case>` → `./dev docker run -- bash runs/<case>/run.sh`
> → `./dev docker run -- bash runs/<case>/postprocess.sh`

## 1. 目的

- 調べること: <例: 一様な風のもとで、風上の境界から入るうねりと風浪がどう重なるか>
- 使い道: <例: 可視化のサンプル>
- 成果物: <例: `work/figs/` の図 (10.2 節)、`work/*.nc`>

## 2. 領域と格子

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 格子の種類・座標系 | <例: 正規格子 (RECT)・直交座標 (m)> | `ww3_grid.nml` `GRID%TYPE`・`GRID%COORD` |
| 範囲 | <例: x 0〜100 km、y 0〜100 km> | `RECT%X0`・`RECT%Y0`・`RECT%NX`・`RECT%NY` |
| 解像度 | <例: 1 km (101 × 101 点)> | `RECT%SX`・`RECT%SY` |
| 閉じ方 | <例: なし> | `GRID%CLOS` |
| スペクトル格子 | <例: 0.05〜0.96 Hz (XFR 1.1、NK 32)、36 方向 (10°)、THOFF 0> | `SPECTRUM%XFR`・`FREQ1`・`NK`・`NTH`・`THOFF` |

## 3. 地形

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 水深 | <例: 一様 1000 m> | `DEPTH%FILENAME`・`DEPTH%SF`・`IDLA`・`IDFM` |
| 海と陸の境目 | <例: ZLIM −0.1 m、最小水深 2.5 m> | `GRID%ZLIM`・`GRID%DMIN` |
| 海陸マスク | <例: なし (水深から決める)> | `MASK_NML` |
| 障害物 | <例: なし> | `&MISC FLAGTR`・`OBST_NML` |

- データの出典と作り方: <例: 一様なので 1 行 (`10201*1000.`)>

## 4. 境界条件 (うねり)

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 境界点 | <例: 西端の列 (ix = 1、iy = 1〜101)> | `INBND_POINT` / マスクの値 2 |
| 境界のスペクトル | <例: 初期条件と同じスペクトルで一定 (nest.ww3 を与えない)> | `ww3_bounc.nml`・`spec.list` / なし |
| それ以外の外周 | <例: 除外点 (波は入らず、出ていく)> | |

- スペクトルの出典 (ファイルを使うとき): <...>

## 5. 初期条件

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 種類 | <例: 任意のスペクトル (ITYPE 4)、全海点で同じ> | `ww3_strt.inp` の ITYPE |
| スペクトル | <例: JONSWAP Hs 0.5 m、Tp 6 s、γ 3.3、cos² (270° から来る)> | <例: `./dev tools jonswap_strt …` で作る> |
| restart から続けるか | <例: いいえ> | `run.sh` の `RESTART` |

## 6. 強制場

| 種類 | 値 (出典・期間・補間) | 設定ファイル・キー |
|---|---|---|
| 風 | <例: 一様 5 m/s、270° から (来る方向・北から時計回り)> | `ww3_shel.nml` `INPUT%FORCING%WINDS`・`HOMOG_INPUT` / `ww3_prnc_wind.nml` |
| 流れ | <例: なし> | `INPUT%FORCING%CURRENTS` |
| 水位 | <例: なし> | `INPUT%FORCING%WATER_LEVELS` |
| 海氷 | <例: なし> | `INPUT%FORCING%ICE_CONC` |

## 7. 物理と数値

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| switch | <例: `model/bin/switch_NCEP_st4` のまま (ST4・NL1・BT1・DB1、DIST/MPI)> | `input/switch_<case>` |
| 物理 namelist | <例: ST4 の既定 (TEST471)。変更なし> | `GRID%NML` のファイル |
| 計算する過程 | <例: x・y の伝播、屈折、源項> | `RUN%FLCX`・`FLCY`・`FLCTH`・`FLCK`・`FLSOU` |
| 時間刻み | <例: DTMAX 150 s、DTXY 50 s、DTKTH 75 s、DTMIN 10 s> | `TIMESTEPS%DTMAX`・`DTXY`・`DTKTH`・`DTMIN` |

- CFL の確かめ: <例: DTXY ≤ min(Δx, Δy) / (1.15·g/(4π·FREQ1)) = 1000 / 17.96 = 55.7 s。50 s で満たす>

## 8. 実行

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 期間 | <例: 2020-01-01 00:00 〜 2020-01-02 00:00 (24 時間)> | `ww3_shel.nml` `DOMAIN%START`・`DOMAIN%STOP` |
| ビルド | <例: `build/<case>/`> | `input/switch_<case>` |
| 並列 | <例: MPI 8 プロセス> | `run.sh` の `MPI` |
| 所要時間の目安 | <例: 約 5 分 (32 コアのマシンで 8 プロセス)> | |

## 9. 出力

| 種類 | 値 (変数・地点・間隔) | 設定ファイル・キー |
|---|---|---|
| 格子出力 | <例: HS T02 T0M1 FP DIR SPR DP WND を 1 時間ごと> | `TYPE%FIELD%LIST`・`DATE%FIELD` |
| 点出力 | <例: 3 地点 (下の表) を 1 時間ごと> | `TYPE%POINT%FILE`・`DATE%POINT` |
| restart | <例: なし> | `DATE%RESTART` |

| 地点名 | 位置 | 目的 |
|---|---|---|
| <例: C50> | <例: (50 km, 50 km)> | <例: 領域の中央> |

## 10. ポスト処理

### 10.1 NetCDF への変換

| 出力 | 値 | 設定ファイル・キー |
|---|---|---|
| 格子 (ww3_ounf) | <例: `work/ww3.field.nc`、上の全変数、1 時間ごと 25 時刻、float> | `ww3_ounf.nml` `FIELD%LIST`・`TIMESTRIDE`・`TIMECOUNT`・`TIMESPLIT`・`TYPE` |
| 点のスペクトル (ww3_ounp) | <例: `work/ww3.spec.nc`、全地点、1 時間ごと> | `ww3_ounp_spec.nml` `POINT%TYPE = 1`・`SAMEFILE` |

- 図のツール (skill `ww3-postprocess`) は 1 つのファイルを読むので、`TIMESPLIT = 0`・`SAMEFILE = T` にしておく。
  `TIMESTRIDE` は必ず書く (既定の `'0'` だと終わらないことがある)。

### 10.2 可視化

図は `postprocess.sh` が作る (1 行 = 1 図。行末のコメントに ID を書く)。ツールは skill `ww3-postprocess`。

| ID | 図 | 入力 | 変数 | 時刻・地点 | 出力 | 確かめること |
|---|---|---|---|---|---|---|
| V1 | <例: Hs の平面図 (波向の矢印、境界点)> | <例: `ww3.field.nc`> | <例: hs、dir、uwnd/vwnd> | <例: 0・2・6・24 h> | <例: `work/figs/hs_maps.png`> | <例: 矢印が東向き、t=0 で一様 0.5 m> |

## 11. 検証

受け入れの基準と、確かめた結果を書く。結果には確かめた日と方法 (コマンド・図) を添える。

| 基準 | 結果 |
|---|---|
| <例: `run.sh` が最後まで通る (`.out` に `ERROR` が無い)> | <例: 2026-11-10 確認> |
| <例: t=0 の内部で Hs = 0.50 m、SPR ≈ 31.5°> | <...> |

## 12. 変更履歴

| 日付 | 変更 | 理由 |
|---|---|---|
| <YYYY-MM-DD> | <例: 新規作成> | <...> |
