# sample-jonswap シミュレーション仕様書

> ケース: `runs/sample-jonswap/`。設定ファイルは `input/`、実行は `run.sh`、ポスト処理は `postprocess.sh`。
> 実行: `./dev build runs/sample-jonswap/input/switch_sample-jonswap` → `./dev docker run -- bash runs/sample-jonswap/run.sh`
> → `./dev docker run -- bash runs/sample-jonswap/postprocess.sh`

## 1. 目的

- 調べること: 一様な風 (5 m/s) のもとで、風上の境界から入り続ける JONSWAP のうねりに風浪がどう重なり、
  南北の境界から波が抜けると Hs と波向がどう変わるか。
- 使い道: 仕様書・ポスト処理 (可視化) の skill のサンプル。最小の構成で WW3 の入出力と図の流れを示す。
- 成果物: `work/figs/` の図 (10.2 節)、`work/ww3.field.nc`・`work/ww3.spec.nc`。

## 2. 領域と格子

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 格子の種類・座標系 | 正規格子 (RECT)・直交座標 (m) | `ww3_grid.nml` `GRID%TYPE = 'RECT'`・`GRID%COORD = 'CART'` |
| 範囲 | x 0〜100 km、y 0〜100 km | `RECT%X0 = 0.`・`RECT%Y0 = 0.`・`RECT%NX = 101`・`RECT%NY = 101` |
| 解像度 | 1 km (101 × 101 点) | `RECT%SX = 1000.`・`RECT%SY = 1000.` |
| 閉じ方 | なし | `GRID%CLOS = 'NONE'` |
| スペクトル格子 | 0.05〜0.96 Hz (XFR 1.1、32 周波数)、36 方向 (10°)、THOFF 0 | `SPECTRUM%XFR = 1.1`・`FREQ1 = 0.05`・`NK = 32`・`NTH = 36`・`THOFF = 0.` |

## 3. 地形

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 水深 | 一様 1000 m (Tp 20 s の波まで深海) | `DEPTH%FILENAME = '../input/depth.dat'`・`DEPTH%SF = -1.`・`IDLA = 2`・`IDFM = 1` |
| 海と陸の境目 | ZLIM −0.1 m、最小水深 2.5 m | `GRID%ZLIM = -0.1`・`GRID%DMIN = 2.5` |
| 海陸マスク | なし (全点が海。外周の扱いは 4 節) | `MASK_NML` なし |
| 障害物 | なし | `&MISC FLAGTR` は既定の 0 |

- データの作り方: 一様なので、`input/depth.dat` は並び指定の READ の繰り返し記法で 1 行 `10201*1000.` (101 × 101 個の 1000) と書く。
  ww3_grid がこのまま読めることを確かめた (11 節)。

## 4. 境界条件 (うねり)

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 境界点 | 西端の列 x = 0 km (ix = 1、iy = 1〜101 の 101 点) | `ww3_grid.nml` `INBND_POINT(1) = 1 1 F`・`INBND_POINT(2) = 1 101 T` |
| 境界のスペクトル | 初期条件 (5 節) と同じスペクトルで一定。`nest.ww3` を与えないと、境界点は初期のスペクトルのまま保たれる | なし (`ww3_bounc.nml` を置かない) |
| それ以外の外周 | 南・北・東の外周は除外点 (波は入らず、出ていく) | 境界点のリストに無い外周の海点は除外点になる |

- ww3_shel は `INPUT FILE WITH BOUNDARY CONDITIONS NOT FOUND` の警告を出すが、これは想定どおり。

## 5. 初期条件

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 種類 | 任意のスペクトル (ITYPE 4)、全海点で同じ | `ww3_strt.inp` の ITYPE = 4 |
| スペクトル | JONSWAP: Hs 0.5 m、Tp 6 s、γ 3.3 (σ 0.07 / 0.09)。方向分布 cos²、270° から来る (西から東へ進む)。Hs は WW3 の出力と同じ定義 (尾部込み) で 0.5 m | `./dev tools jonswap_strt --hs-m 0.5 --tp-s 6 --from-deg 270 --cos-power 2 --xfr 1.1 --freq1 0.05 --nk 32 --nth 36 --thoff 0 --out runs/sample-jonswap/input/ww3_strt.inp` |
| restart から続けるか | いいえ | `run.sh` の `RESTART` は空 |

- 周波数格子が離散なので、ピークは 1/6 Hz に最も近い帯 0.1726 Hz (5.79 s) になる。

## 6. 強制場

| 種類 | 値 (出典・期間・補間) | 設定ファイル・キー |
|---|---|---|
| 風 | 一様 5 m/s (10 m 風速)、270° から (来る方向・北から時計回り)。全期間で一定 | `ww3_shel.nml` `INPUT%FORCING%WINDS = 'H'`、`HOMOG_INPUT(1..2)` (開始と終了に同じ値) |
| 流れ | なし | `INPUT%FORCING%CURRENTS` は既定の 'F' |
| 水位 | なし | `INPUT%FORCING%WATER_LEVELS` は既定の 'F' |
| 海氷 | なし | `INPUT%FORCING%ICE_CONC` は既定の 'F' |

## 7. 物理と数値

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| switch | `model/bin/switch_NCEP_st4` のまま (`DIST MPI PR3 UQ ST4 FLX0 LN1 NL1 BT1 DB1 MLIM …`) | `input/switch_sample-jonswap` |
| 物理 namelist | ST4 の既定 (TEST471)。変更なし | `input/namelists.nml` (`GRID%NML`) |
| 計算する過程 | x・y の伝播、屈折、源項 (水深・流れが一様なので波数シフトは不要) | `RUN%FLCX`・`FLCY`・`FLCTH`・`FLSOU = T` |
| 時間刻み | DTMAX 150 s、DTXY 50 s、DTKTH 75 s、DTMIN 10 s | `TIMESTEPS%DTMAX`・`DTXY`・`DTKTH`・`DTMIN` |

- CFL の確かめ: DTXY ≤ min(Δx, Δy) / (1.15·g/(4π·FREQ1)) = 1000 / (1.15 × 9.81 / (4π × 0.05)) = 1000 / 17.96 = 55.7 s。
  50 s で満たす。DTMAX = 3·DTXY、DTKTH = DTMAX/2、出力の 1 時間は DTMAX の 24 倍。

## 8. 実行

| 項目 | 値 | 設定ファイル・キー |
|---|---|---|
| 期間 | 2020-01-01 00:00 〜 2020-01-02 00:00 (24 時間) | `ww3_shel.nml` `DOMAIN%START`・`DOMAIN%STOP` |
| ビルド | `build/sample-jonswap/` | `input/switch_sample-jonswap` |
| 並列 | MPI 8 プロセス | `run.sh` の `MPI="mpirun -np 8"` |
| 所要時間の目安 | 約 5 分 (32 コアのマシンで 8 プロセス、`run.sh` 全体) | |

## 9. 出力

| 種類 | 値 (変数・地点・間隔) | 設定ファイル・キー |
|---|---|---|
| 格子出力 | HS T02 T0M1 FP DIR SPR DP WND を 1 時間ごと | `ww3_shel.nml` `TYPE%FIELD%LIST`・`DATE%FIELD` |
| 点出力 | 3 地点 (下の表) のスペクトルを 1 時間ごと | `TYPE%POINT%FILE = '../input/points.list'`・`DATE%POINT` |
| restart | なし | `DATE%RESTART` なし |

| 地点名 | 位置 | 目的 |
|---|---|---|
| W10 | (10 km, 50 km) | 境界のすぐ風下 |
| C50 | (50 km, 50 km) | 領域の中央 |
| E90 | (90 km, 50 km) | 風下の端の近く |

## 10. ポスト処理

### 10.1 NetCDF への変換

| 出力 | 値 | 設定ファイル・キー |
|---|---|---|
| 格子 (ww3_ounf) | `work/ww3.field.nc`。9 節の全変数、1 時間ごと 25 時刻、float | `ww3_ounf.nml` `FIELD%LIST`・`TIMESTRIDE = '3600'`・`TIMECOUNT = '25'`・`TIMESPLIT = 0`・`TYPE = 4` |
| 点のスペクトル (ww3_ounp) | `work/ww3.spec.nc`。全地点を 1 ファイルに、1 時間ごと 25 時刻 | `ww3_ounp_spec.nml` `POINT%TYPE = 1`・`SAMEFILE = T`・`TIMESPLIT = 0`・`SPECTRA%OUTPUT = 3`・`SPECTRA%TYPE = 4` |

### 10.2 可視化

図は `postprocess.sh` が作る (1 行 = 1 図。行末のコメントに ID)。ツールは skill `ww3-postprocess`。

| ID | 図 | 入力 | 変数 | 時刻・地点 | 出力 | 確かめること |
|---|---|---|---|---|---|---|
| V1 | Hs の平面図 (波の進む向きの矢印、境界点、風) | `ww3.field.nc` | hs、dir、uwnd/vwnd、MAPSTA | 0・2・6・24 h | `work/figs/hs_maps.png` | t=0 で一様 0.5 m・矢印が東向き。南北の端で Hs が下がり、矢印が外向きに傾く。6 h 以降はほぼ定常 |
| V2 | Hs のアニメーション | `ww3.field.nc` | hs、dir、uwnd/vwnd、MAPSTA | 全 25 時刻 | `work/figs/hs.gif` | V1 の変化が連続して見える |
| V3 | 中央の地点の方向スペクトルと周波数スペクトル | `ww3.spec.nc` | efth | C50、0・2・6・24 h (対数の色、3 桁) | `work/figs/spectra_C50.png` | 0.17 Hz・東向きのうねりの山。時間とともに 0.3 Hz 付近に東向きの風浪の山が育つ |
| V4 | 平均周期 T02 の平面図 (波の進む向きの矢印、境界点、風) | `ww3.field.nc` | t02、dir、uwnd/vwnd、MAPSTA | 0・2・6・24 h | `work/figs/t02_maps.png` | t=0 で一様 4.66 s。風浪が加わって短くなり、6 h 以降は風上の境界付近で約 3.5 s、風下で約 2.7 s |

## 11. 検証

| 基準 | 結果 |
|---|---|
| `jonswap_strt` の計算で Hs = 0.5 m、SPR = 31.5° (cos²)、平均波向 270° | 2026-11-10 確認: Hs 0.5000 m、ピーク 0.1726 Hz、270.0°、SPR 31.5° |
| ww3_grid が `depth.dat` (`10201*1000.`) を読み、海点 9902 (うち境界点 101、`MAPSTA` = 1 の点 9801)・除外点 299 になる | 2026-11-10 確認 (`ww3_grid.out` の `Number of sea points : 9902`・`Number of excluded points : 299`、`ww3.field.nc` の `MAPSTA` の値ごとの数) |
| `run.sh` が最後まで通る (`.out` に `ERROR` が無い。境界の警告だけ) | 2026-11-10 確認 (約 4 分 40 秒) |
| t=0 の内部で Hs = 0.50 m、spr ≈ 31.5°、dir = 270°、fp ≈ 1/6 Hz | 2026-11-10 確認: hs 0.500、spr 31.50°、dir 270.0°、fp 0.170 Hz |
| 風浪が重なる: 平均周期 t02 が下がり、Hs が少し上がる | 2026-11-10 確認: 内部の平均で t02 4.66 → 2.83 s、Hs 0.50 → 0.53 m (6 h 以降ほぼ一定)。C50 の周波数スペクトルに 0.33 Hz 付近の山 (V3)、t02 の分布 (V4) |
| 西の境界付近で Hs が 0.5 m 前後を保つ | 2026-11-10 確認: x = 2 km の列の平均で 0.52 m (6・12・24 h) |
| V1〜V4 が作られ、「確かめること」を満たす | 2026-11-10 確認 (図を目で確認) |

- 数値の確かめ方: `ww3.field.nc` をコンテナの python3 (netCDF4) で読み、外周から 5 点を除いた内部の最小・平均・最大と、
  x = 2・50・98 km の列 (同じく南北の端 5 点を除く) の平均を、時刻 0・6・12・24 h について出した。

## 12. 変更履歴

| 日付 | 変更 | 理由 |
|---|---|---|
| 2026-11-10 | 新規作成 | 仕様書・ポスト処理の skill のサンプル |
