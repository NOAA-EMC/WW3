# ww3_grid: 格子・地形・スペクトル格子・時間刻み

- 読むファイル: `ww3_grid.nml` (無ければ `ww3_grid.inp`)、`GRID%NML` が指す物理 namelist のファイル、地形のデータファイル。
- できるファイル: `mod_def.ww3` (他の全プログラムが最初に読む)。標準出力 (`ww3_grid.out` に保存する) と `ww3_grid.nml.log`。
- 物理 namelist の書き方は [physics.md](physics.md)。

## ww3_grid.nml の group

テンプレートは `model/nml/ww3_grid.nml` (各 group の説明と既定値がコメントにある)。
`SPECTRUM_NML`・`TIMESTEPS_NML`・`GRID_NML` は必須 (無いと止まる。`model/src/w3nmlgridmd.F90:585`、`:799`、`:909`)。

| group | 主な項目 | 注意 |
|---|---|---|
| `SPECTRUM_NML` | `XFR` `FREQ1` `NK` `NTH` `THOFF` | 既定値は 0 なので全部書く。周波数は FREQ1·XFR^(k−1)、方向は 360°/NTH 刻みで、最初の方向は THOFF × 刻み。XFR は 1.00001 以上、THOFF は ±0.5 に丸められる (`model/src/w3gridmd.F90:1262-1268`) |
| `RUN_NML` | `FLDRY` `FLCX` `FLCY` `FLCTH` `FLCK` `FLSOU` | 既定は全部 F で、全部 F だと停止する (`:1367-1370`)。源項 (風による発達など) を使うなら `FLSOU = T` |
| `TIMESTEPS_NML` | `DTMAX` `DTXY` `DTKTH` `DTMIN` | 下の「時間刻み」 |
| `GRID_NML` | `NAME` `NML` `TYPE` `COORD` `CLOS` `ZLIM` `DMIN` | `NML` は物理 namelist のファイル (work 基準のパス)。`TYPE` は `RECT`・`CURV`・`UNST`・`SMCG`。`COORD` は `'SPHE'` か `'CART'` を正確に書く (`:3573-3574`)。`CLOS` は `NONE`・`SMPL` (経度方向に一周。SX = 360/NX に固定される)・`TRPL` (CURV 格子のみ。RECT だと停止) |
| `RECT_NML` | `NX` `NY` `SX` `SY` `SF` `X0` `Y0` `SF0` | SX・SY は SF で、X0・Y0 は SF0 で**割られる** (`:3800-3809`)。分単位で書くなら SF = 60。(X0, Y0) は格子点 (1, 1)。直交座標なら m |
| `CURV_NML` | `NX` `NY`、`XCOORD` / `YCOORD` の `FILENAME` `SF` `OFF` `IDLA` `IDFM` | 座標 = SF × 値 + OFF (`:3890-3891`)。各格子点の経度・緯度を NX×NY のファイルで渡す |
| `UNST_NML` | `FILENAME` (Gmsh の .msh) `SF` | 正の水深なら `SF = -1.` (`model/nml/ww3_grid.nml:305-311`)。開境界は `&UNST UGOBCAUTO` (既定 T) か境界リスト |
| `SMC_NML` | セル・面のファイル | switch `SMC` が必要。regtest `ww3_tp2.10`・`ww3_tp2.16` |
| `DEPTH_NML` | `FILENAME` `SF` `IDLA` `IDFM` `FORMAT` | 下の「地形データ」 |
| `MASK_NML` | `FILENAME` `IDLA` `IDFM` `FORMAT` | 書かなければ水深と境界点リストからマスクを作る |
| `OBST_NML` | `FILENAME` `SF` `IDLA` `IDFM` `FORMAT` | `&MISC FLAGTR` が 0 以外のときだけ読む |
| `INBND_COUNT_NML` / `INBND_POINT_NML` | `N_POINT`、`INBND_POINT(i) = ix iy connect` | うねりを入れる境界点 ([boundary.md](boundary.md))。マスクファイルが無いときだけ使う |
| `EXCL_COUNT_NML` / `EXCL_POINT_NML` / `EXCL_BODY_NML` | 計算から外す点・水域 | マスクファイルが無いときだけ使う |
| `OUTBND_COUNT_NML` / `OUTBND_LINE_NML` | 子格子へ渡す境界線 | この格子を親にしてネストするとき ([boundary.md](boundary.md)) |
| `SLOPE_NML` / `SED_NML` | 海岸の傾斜・底質の地図 | `&REF1 REFMAP`・`&SBT4 SEDMAPD50` を使うときだけ ([physics.md](physics.md)) |

`.inp` で書く場合のレコードの順 (`model/inp/ww3_grid.inp`):

1. 格子の名前
2. スペクトル: `XFR FREQ1 NK NTH THOFF`
3. フラグ: `FLDRY FLCX FLCY FLCTH FLCK FLSOU`
4. 時間刻み: `DTMAX DTXY DTKTH DTMIN`
5. 物理 namelist。`END OF NAMELISTS` (1 桁目から) で終える
6. `'TYPE' FLAGLL 'CLOS'`
7. `NX NY`
8. RECT なら間隔と原点、CURV なら座標ファイルの行
9. 水深の行
10. (SMC のみ) SMC のファイルの行
11. (FLAGTR > 0) 障害物の行
12. マスクの行と、境界点・除外点のリスト
13. (使うとき) 底質の行
14. 出力境界線 (最後は点数 0 の行で閉じる)

`.inp` の行は 80 桁で切れる (`LINE*80`、`model/src/w3gridmd.F90:787`)。
データファイルのパスは、`.inp`・`.nml` のどちらでも 60 文字で切れる (`FNAME*60`・`TNAME*60` に写される。`:787`、`:4006`、`:4554` など)。
`../input/` を含めて数える。`GRID%NML` だけはそのまま開かれる (`:1617`)。

## 地形データ

### 水深

- 格納値 = SF × ファイルの値 (`model/src/w3gridmd.F90:4077-4078`)。WW3 は海面下を負で持つので、正の水深のファイルには負の SF を掛ける
  (regtest `ww3_tp2.4`・`ww3_tp2.5` は `DEPTH%SF = -1.`)。
- 格納値が `ZLIM` 以下の点が海になる (`:4106-4110`)。`DMIN` は計算で使う最小の水深 (1e-3 m 以上に補正。`:4015`)。
- ファイルは NX×NY の値。並べ方は `IDLA` と `IDFM` で指定する (`model/inp/ww3_grid.inp:590-602`):

  | IDLA | 並び | IDFM | 書式 |
  |---|---|---|---|
  | 1 | 下の行 (IY=1) から 1 行ずつ | 1 | 自由書式 |
  | 2 | 1 と同じ並びを 1 回の READ で | 2 | `FORMAT` で指定した固定書式 |
  | 3 | 上の行から 1 行ずつ | 3 | unformatted |
  | 4 | 3 と同じ並びを 1 回の READ で | | |

- `.inp` で装置番号 10 を指定すると、データを制御ファイルの中に続けて書ける (`model/inp/ww3_grid.inp:603-606`。`.nml` では使えない)。
- 非構造格子では Gmsh のメッシュが水深を持つ。SMC 格子ではセルのファイルが水深を持つ。

### 海陸マスクと境界点

- マスクの値: 0 = 陸、1 = 海、2 = 境界点 (うねりを入れる点)、3 = 計算から除外 (`model/inp/ww3_grid.inp:828-833`)。
- マスクファイルを使うと:
  - 閉じていない格子 (`CLOS = 'NONE'`) の外周の海点 (値 1) は除外点 (3) にされる (`model/src/w3gridmd.F90:4844-4855`)。
    外周からうねりを入れたいなら、その点を 2 にしておく。
  - `ZLIM` より浅い点は陸にされる (`:4864`)。
- マスクファイルを使わない (`MASK%FILENAME` を書かない) と、水深から海陸を決め、境界点と除外点のリストを足す (`:4553-4557`)。
  - 境界点: `INBND_POINT(i) = ix iy connect`。connect が T で、前の点と同じ格子線か対角線上にあれば、間の点も境界点になる
    (`model/inp/ww3_grid.inp:840-846`)。
  - リストに無い外周の海点は除外点になる。

### 障害物 (sub-grid obstruction)

- `&MISC FLAGTR` が 0 以外のときに読む (`model/src/w3gridmd.F90:4116-4130`。テンプレートの説明は 1〜4、コードは 6 まで受け付ける)。
  1・3 はセルの境界、2・4 はセルの中心での透過率。3・4 は海氷を連続的に扱う ([physics.md](physics.md) の海氷)。
- ファイルは x 方向と y 方向の 2 つの NX×NY の場 (0〜1) を続けて書く。
- テンプレートの `&MISC CICE0 = 0.25, CICEN = 0.75, FLAGTR = 4 /` (`model/inp/ww3_grid.inp:541`、`model/nml/namelists.nml:582`) を
  写すと、障害物のファイルを要求される。無いなら `FLAGTR = 0` (既定) にする。

## 時間刻み

| 項目 | 意味 |
|---|---|
| `DTMAX` | 全体の時間刻み。強制場の更新と出力はこの刻みで行う |
| `DTXY` | 空間伝播の時間刻み。最低周波数の CFL 条件で決める。流れの影響は無視してよい (モデルが自動で刻みを細かくする) |
| `DTKTH` | 屈折と波数シフトの時間刻み |
| `DTMIN` | 源項の動的な時間刻みの下限 |

- CFL 条件 (`manual/app/tstep.tex`):

  ```
  DTXY ≤ min(Δx, Δy) / (1.15 · g / (4π · FREQ1))
  ```

  g/(4π·FREQ1) は最低周波数の深海の群速度で、1.15 倍は中間水深での最大値。球面格子では、最も極側の海の行の Δx で計算する。
- ww3_grid はこの条件を検査しない。DTMAX を 1 s 以上に、DTMIN を 0〜DTMAX に補正するだけ (`model/src/w3gridmd.F90:1391-1397`)。
- 目安 (`manual/app/tstep.tex`、`model/nml/ww3_grid.nml:48-67`):
  - DTMAX = DTXY の 2〜4 倍 (球面格子で CFL が厳しいのは一部の点だけなので)
  - DTKTH = DTMAX/2 (強い流れがあるなら DTMAX/10)
  - DTMIN = 5〜15 s (全球の格子では 30 s が多い)
  - 4 つを互いの整数倍にする
- 計算例 (この skill の検証に使った北東太平洋 0.35457° 格子。最も北の行は 62.3°N):
  Δx = 0.35457° × 111.2 km × cos 62.3° ≈ 18.3 km、群速度 1.15 × 9.81/(4π × 0.04118) ≈ 21.8 m/s なので DTXY ≤ 840 s。
  DTMAX/DTXY/DTKTH/DTMIN = 1800/600/900/15 s で動かした。
- regtest の例:

  | 格子 | DTMAX / DTXY / DTKTH / DTMIN (s) | 出典 |
  |---|---|---|
  | GFSv16 北半球 10′ | 1800 / 420 / 840 / 30 | `regtests/ww3_ufs1.2/input/ww3_grid_gnh_10m.nml` |
  | UFS 全球 1° | 1800 / 450 / 900 / 30 | `regtests/ww3_ufs1.1/input/ww3_grid_glo_1deg.nml` |
  | アドリア海の曲線格子 | 900 / 450 / 450 / 5 | `regtests/ww3_tp2.15/input/ww3_grid.inp` |
  | Iroise 1.5 km (強い潮流) | 180 / 45 / 45 / 10 | `regtests/ww3_tp2.8/input/ww3_grid.inp` |

## スペクトル格子

| 例 | XFR / FREQ1 / NK / NTH / THOFF | 周波数の範囲 |
|---|---|---|
| テンプレート (`model/nml/ww3_grid.nml:18-21`) | 1.1 / 0.04118 / 32 / 24 / 0 | 0.041〜0.79 Hz |
| GFSv16 (`regtests/ww3_ufs1.2/input/ww3_grid_gnh_10m.nml`) | 1.07 / 0.035 / 50 / 36 / 0.5 | 0.035〜0.96 Hz |

- 最低周波数 FREQ1 は CFL 条件 (上) に効く。下げるほど DTXY を小さくする必要がある。
- NK・NTH を変えると restart・境界スペクトルとの関係が変わる ([boundary.md](boundary.md))。

## ww3_grid.out で確かめること

- スペクトル格子と 4 つの時間刻み (補正後の、実際に使う値)。
- 物理パッケージごとの `(default values)` / `(user def. values)`。namelist が読まれたかはここで分かる (`model/src/w3gridmd.F90:7337-7531`)。
- 警告 (`WARNING`)、海点・陸点・境界点・除外点の数。

## テンプレートの誤り

- `model/nml/ww3_grid.nml` の `OUTBND_LINE` の説明 (`X0`: "x index start point" など) は誤り。実際は座標 (球面なら度、直交なら m) で、
  `XO = XO0 + (IP-1)*DXO` と使われる (`model/src/w3gridmd.F90:5882-5883`)。`.inp` 版の説明 (`model/inp/ww3_grid.inp:867-872`) は正しい。
- `&MISC ... FLAGTR = 4` の例 (上記) は障害物ファイルを要求する。
