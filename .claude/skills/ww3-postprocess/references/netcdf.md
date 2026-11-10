# ww3_ounf・ww3_ounp の NetCDF の約束事

描画やデータの取り出しで間違えやすい点の表。値は `runs/sample-jonswap` の出力 (`ncdump -h`) とソースで確かめた。
`ww3nc.py` (`tools/viz/`) はこの約束事を前提にしている。

## ファイル名

名前は `FILE%PREFIX` (既定 `'ww3.'`)、`TIMESPLIT` (日付の桁: 0・4・6・8・10)、`SAMEFILE` で決まる。描画ツールは名前を決め打ちせず、
作業ディレクトリの `*.nc` から必要な変数を持つファイルを探す (`ww3nc.find_nc`。複数あるときは `--file` で選ぶ)。

| 出力 | 設定 | ファイル名の例 (確認済み) |
|---|---|---|
| ww3_ounf | `TIMESPLIT = 0`、`SAMEFILE = T` | `ww3.field.nc` |
| ww3_ounf | `TIMESPLIT = 8`、`SAMEFILE = T` | `ww3.20080522.nc` |
| ww3_ounf | `TIMESPLIT = 6`、`SAMEFILE = F` | `ww3.200805_hs.nc` (変数ごと) |
| ww3_ounp (`TYPE = 1`) | `TIMESPLIT = 0`、`SAMEFILE = T` | `ww3.spec.nc` |
| ww3_ounp (`TYPE = 1`) | `SAMEFILE = F` | `ww3.Point1_196806_spec.nc` (地点ごと) |
| ww3_ounp (`TYPE = 2`) | `TIMESPLIT = 6` (既定) | `ww3.200805_tab.nc` |

## ww3_ounf (格子の出力)

| 項目 | 内容 |
|---|---|
| 座標 (直交格子) | 1 次元の `x(x)`・`y(y)`、単位 m。値は `X0 + SX·(i−1)`。経度・緯度は無い |
| 座標 (球面格子) | `longitude`・`latitude` (正規格子なら 1 次元、曲線格子なら 2 次元) |
| 配列の並び | 場は `(time, y, x)` (球面なら `(time, latitude, longitude)`) |
| 時刻 | `time`、double、`days since 1990-01-01T00:00:00`、`calendar` は `&MISC CALTYPE` (既定 standard。360_day などでもツールは動く) |
| `MAPSTA(y, x)` | 点の状態。1 = 海、2 = 境界点 (うねりを入れる点)、0 = 陸、8 = 除外点 |
| 型 (`FIELD%TYPE`) | 4 = float (`_FillValue` 9.96921e+36)。2・3 = short に `NINT(値/scale_factor)` で詰める (`model/src/ww3_ounf.F90:2925-2936`)。short だと `fp` は 0.001 Hz、`dp` は 1° 刻みになるので、解析には 4 を使う |
| 欠損 | 陸と除外点、未定義の値は `_FillValue`。netCDF4 は自動でマスクする |

よく使う変数 (タグ → 変数):

| タグ | 変数 | 単位 | 意味・規約 |
|---|---|---|---|
| HS | `hs` | m | 有義波高 (`sea_surface_wave_significant_height`) |
| T02 / T0M1 | `t02` / `t0m1` | s | 平均周期 |
| FP | `fp` | s-1 | ピーク周波数 (周期ではない) |
| DIR | `dir` | degree | 平均波向。**来る方向**、北から時計回り (`sea_surface_wave_from_direction`)。矢印にするときは 180° 足す |
| DP | `dp` | degree | ピーク波向 (来る方向) |
| SPR | `spr` | degree | 方向の広がり。cos² の分布で約 31.5° |
| WND | `uwnd`・`vwnd` | m s-1 | 10 m 風の東向き・北向きの成分 (`FIELD%VECTOR = T`、既定)。F にすると `wspd`・`wdir` (風速と、来る方向) になる (`model/src/ww3_ounf.F90:1241-1244`、`model/src/w3ounfmetamd.F90:2625`、`:2635`)。ツールの風の表示は `uwnd`・`vwnd` を使う |
| PHS・PTP・PDIR | `phs<N>`・`ptp<N>`・`pdir<N>` | m・s・degree | 成分 (partition) ごとの値。N は `FIELD%PARTITION` で、0 が風浪 |

## ww3_ounp (点の出力)

共通:

| 項目 | 内容 |
|---|---|
| 時刻 | `time`、double、`days since 1990-01-01 00:00:00` |
| 地点 | `station` (番号)、`station_name(station, string40)` (char。`points.list` の名前) |
| 位置 (直交格子) | `x(time, station)`・`y(time, station)`、単位 **km** (ww3_ounf の m と違う) |
| 位置 (球面格子) | `longitude`・`latitude` |

`POINT%TYPE = 1` (スペクトル、`_spec.nc`):

| 変数 | 単位 | 意味・規約 |
|---|---|---|
| `efth(time, station, frequency, direction)` | m2 s rad-1 | 方向スペクトル E(f,θ)。`SPECTRA%TYPE = 4` (既定) で float。3 以下だと short に `NINT(log10(E+1e-12)/0.0004)` で詰め、単位は `log10(m2 s rad-1 +1E-12)` になる (`model/src/ww3_ounp.F90:1872-1874`)。このときファイルの `valid_min = 0` は誤りで (log の値は負)、netCDF4 の自動マスクが全値を隠すので、自動の処理を止めて戻す (`ww3nc.spectrum` はそうしている。確認済み) |
| `frequency` | s-1 | 帯の中心。`frequency1`・`frequency2` が帯の端 (差が帯の幅) |
| `direction` | degree | **向かう方向**、北から時計回り (`sea_surface_wave_to_direction`)。単調に並んでいないので、描くときは並べ替える |
| `dpt` | m | 水深 |
| `wnd`・`wnddir` | m s-1・degree | 風速と風向 (`wind_from_direction`、来る方向) |

- 周波数スペクトルは E(f) = Σ_θ E(f,θ)·Δθ (Δθ はラジアン)。Hs を帯から出すと、WW3 が足す尾部 (0.25·f_NK·E(f_NK)) の分だけ
  ww3_ounf の `hs` より小さくなる。
- `POINT%DIMORDER = F` (NetCDF-4 のときだけ使える。3 だと止まる、`model/src/ww3_ounp.F90:694-696`) にすると、
  `efth` の次元が `(station, time, frequency, direction)` になり、`time` の末尾に未記入 (欠損) の記録が残る (確認済み)。
  ツールは次元名で添字を引き、先頭から続く記録だけを使う。
- 境界のスペクトル (ww3_bounc) に渡すときは `POINT%SAMEFILE = F` にする (ww3_bounc は 1 ファイル 1 地点しか読まない。
  skill `ww3-run-config` の `references/boundary.md`)。

`POINT%TYPE = 2` (`_tab.nc`、`PARAM%OUTPUT = 2` のとき): `hs`・`lm` (平均波長)・`tr` (T₋₁,₀)・`th1p`・`sth1p` (ピークの帯の波向と広がり)・
`fp`・`th1m`・`sth1m` (全体の平均波向と広がり)。`th1p`・`th1m` は**来る方向**、北から時計回り (`MOD(630−θ)`、
`model/src/ww3_ounp.F90:1989`、`:2017`)。`PARAM%OUTPUT` の既定は 3 で、中身が違う (U* で無次元化した量)。

## テンプレートの誤り

| 場所 | 書かれている内容 | 実際 |
|---|---|---|
| `model/nml/ww3_ounp.nml:65`、`:158` | `direction` は "convention to, origin East, trigonometric order" | 北から時計回りの向かう方向 (`MOD(450−θ,360)`、`model/src/ww3_ounp.F90:3245`) |
| `model/nml/ww3_ounp.nml:115` | `PARAM%OUTPUT` の既定は 4 | 既定は 3 (`model/src/w3nmlounpmd.F90:624`) |
| `model/nml/ww3_ounf.nml:38` | `TIMEUNIT` の `'I'` は秒 | 受け付けるのは `'D'` か `'S'` だけ (`model/src/ww3_ounf.F90:487-490`) |
