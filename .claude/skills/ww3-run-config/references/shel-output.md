# ww3_shel (実行期間・使う強制場・出力) と後処理

## ww3_shel.nml

テンプレート: `model/nml/ww3_shel.nml`。検証で使った例 (風を `wind.ww3` から読み、格子・点・restart を出力する):

```
&DOMAIN_NML
  DOMAIN%START   = '20080522 000000'
  DOMAIN%STOP    = '20080522 120000'
/
&INPUT_NML
  INPUT%FORCING%WINDS = 'T'
/
&OUTPUT_TYPE_NML
  TYPE%FIELD%LIST          = 'WND HS FP DIR T02'
  TYPE%POINT%FILE          = '../input/points.list'
/
&OUTPUT_DATE_NML
  DATE%FIELD          = '20080522 000000' '3600' '20080522 120000'
  DATE%POINT          = '20080522 000000' '3600' '20080522 120000'
  DATE%RESTART        = '20080522 120000' '43200' '20080522 120000'
/
&HOMOG_COUNT_NML
/
&HOMOG_INPUT_NML
/
```

| group | 項目 | 説明 |
|---|---|---|
| `DOMAIN_NML` | `START` `STOP` | 実行期間。restart から続けるなら START を restart の時刻に合わせる ([boundary.md](boundary.md)) |
| `INPUT_NML` | `INPUT%FORCING%WATER_LEVELS` `CURRENTS` `WINDS` `ICE_CONC` `ATM_MOMENTUM` `AIR_DENSITY` `ICE_PARAM1`〜`5` `MUD_*` | `'F'` 使わない (既定)、`'T'` ファイル (`wind.ww3` など) から読む、`'H'` 一様な値 ([forcing.md](forcing.md))、`'C'` 結合モデルから受け取る。1 文字目だけで判定される (`model/src/ww3_shel.F90:359`)。group を空にすれば強制場なし |
| `OUTPUT_TYPE_NML` | `TYPE%FIELD%LIST` | 格子出力 (type 1) の変数。タグを空白区切りで (下の表) |
| | `TYPE%POINT%FILE` | 点出力 (type 2) の点のリスト。1 行に 1 点 `lon lat 'name'` (直交座標なら m) |
| | `TYPE%RESTART%EXTRA` ほか | restart に足す変数など (テンプレートの説明を読む) |
| `OUTPUT_DATE_NML` | `DATE%FIELD` `DATE%POINT` `DATE%TRACK` `DATE%RESTART` `DATE%BOUNDARY` `DATE%PARTITION` | それぞれ `'start' 'stride' 'stop'` (`DATE%FIELD%START` のように分けて書いてもよい)。stride が `'0'` なら出力しない |
| | `DATE%RESTART2` | checkpoint 用の 2 本目の restart (`YYYYMMDD.HHMMSS.restart.ww3`) |
| `OUTPUT_PATH_NML` | `PATH%GRD_OUT` `PNT_OUT` `RST_OUT` | 既定は `'./'`。ディレクトリは先に作っておく。後処理は作業ディレクトリの `out_grd.ww3` などを読むので、変えると後処理で困る (`model/nml/ww3_shel.nml:320-326`)。`RST_OUT` は `restart.ww3` を読む場所にもなる (`model/src/w3iorsmd.F90:476-480`) |
| `HOMOG_COUNT_NML` / `HOMOG_INPUT_NML` | 一様な強制場 | [forcing.md](forcing.md) |

- うねりの境界 (`nest.ww3`) は設定不要。格子に境界点があれば自動で読む ([boundary.md](boundary.md))。
- 出力の時刻を DTMAX の整数倍にすると、計算の刻みが乱されず再現性が保たれる (`manual/app/tstep.tex`)。

### よく使う出力タグ

`TYPE%FIELD%LIST` と ww3_ounf の `FIELD%LIST` で使う。全体は `model/src/w3iogomd.F90` の `CASE('<tag>')` (764 行目付近から)。

| タグ | 内容 | タグ | 内容 |
|---|---|---|---|
| `DPT` | 水深 | `HS` | 有義波高 |
| `CUR` | 流れ | `T02` / `T0M1` | 平均周期 |
| `WND` | 風 | `FP` | ピーク周波数 |
| `ICE` | 氷の密接度 | `DIR` / `DP` | 平均波向 / ピーク波向 |
| `WLV` | 水位 | `SPR` | 方向の広がり |
| `UST` | 摩擦速度 | `PHS` `PTP` `PDIR` | 成分 (partition) ごとの波高・周期・波向 |
| `CHA` | Charnock 係数 | `EF` | 周波数スペクトル (`&OUTS E3D` の設定が要る) |

- 知らないタグは警告を出して無視される (`model/src/w3iogomd.F90:1175-1178`)。
- `EF`・`TH1M`・`STH1M`・`TH2M`・`STH2M`・`USF`・`USP`・`P2L` は、物理 namelist の `&OUTS` で対応する変数 (`E3D`・`TH1MF`・…・`US3D`・`USSP`・`P2SF`)
  を設定しないと、警告 (`not allowed: need to set parameter … in OUTS namelist`) を出して外される (`model/src/w3iogomd.F90:265-296`)。

### `.inp` で書く場合

- テンプレートは `model/inp/ww3_shel.inp`。強制場の行は、標準のビルドでは 9 行 (水位・流れ・風・氷・運動量・空気密度と、同化データの 3 行)。
  各行は `使うか (F/T/C)  一様か (F/T)`。
- IC1〜IC5・IS2・BT8・BT9 のどれかでビルドしたときは、その前に氷のパラメータ 5 行と泥 3 行が要る (`model/src/ww3_shel.F90:655-682`)。

## 実行と確かめ方

- DIST/MPI のビルドは `mpirun -np <N> $B/ww3_shel`、SHRD のビルドは `$B/ww3_shel` (SKILL.md の `run.sh` の `MPI`)。
- 作業ディレクトリにできるもの: `log.ww3`、`out_grd.ww3`、`out_pnt.ww3`、`restartNNN.ww3`、`nest<N>.ww3` (type 5)、
  ST4 のビルドでは応力の表 `ST4TABUHF2.bin` (`model/src/w3src4md.F90:1710`)。
- 正常終了は「`ww3_shel.out` の最後に `End of program` があり、`ERROR` が無い」で判定する。強制場のファイルが無い・合わないと、
  ww3_shel は `*** WAVEWATCH III ERROR IN W3FLDO` を出したうえで `End of program` を出し、終了コード 0 で終わる
  (`model/src/ww3_shel.F90:1732-1736` → `FINALISE`。検証で確認)。このとき `out_grd.ww3` などは作られない。
- `mpirun` の下で止まると、原因の文が残らず `EXTCDE MPI_ABORT, IEXIT= <N>` だけになることがある。同じ作業ディレクトリで
  `mpirun` なしで動かすと文が見える (検証で確認)。
- `log.ww3` の時間ステップ表: 入力欄 `b` (境界)・`w` (風)・`l` (水位)・`c` (流れ)・`t` (運動量)・`r` (空気密度)・`i` (海氷) に、
  読み込んだステップで `F` (最初)・`X` が付き、`L` はファイルの終わり。出力欄は `g` (格子)・`p` (点)・`t` (track)・`r` (restart)・`b` (境界) など
  (`model/src/w3initmd.F90:1624`、`model/src/w3wavemd.F90:1290-1312`)。

## ww3_ounf.nml (格子出力 → NetCDF)

テンプレート: `model/nml/ww3_ounf.nml`。検証で使った例:

```
&FIELD_NML
  FIELD%TIMESTART        =  '20080522 000000'
  FIELD%TIMESTRIDE       =  '3600'        ! 必ず書く (下の注意)
  FIELD%TIMECOUNT        =  '13'
  FIELD%TIMESPLIT        =  8             ! ファイル名の日付の桁: 0 なし、4 年、6 年月、8 年月日、10 時まで
  FIELD%LIST             =  'WND HS FP DIR T02'
  FIELD%PARTITION        =  '0'           ! 0 は風浪、1 以降はうねりの成分
  FIELD%TYPE             =  4             ! 2: short、3: 変数による、4: real
/
&FILE_NML
  FILE%NETCDF        = 4
/
```

- `FIELD_NML` は必須 (無いと止まる。`model/src/w3nmlounfmd.F90:302-308`)。`FILE_NML` の `PREFIX` (既定 `'ww3.'`)、`IX0`・`IXN`・`IY0`・`IYN` で切り出せる。
- **`FIELD%TIMESTRIDE` は必ず書く**。既定の `'0'` のまま `TIMESTART` を最初の記録より前にすると、要求の時刻が進まず ww3_ounf が終わらない
  (検証で確認。`model/src/ww3_ounf.F90:645-657` のループ)。
- `out_grd.ww3` に無い変数は `*** NOT AVAILABLE ***` と出て作られない (`model/src/ww3_ounf.F90:551`)。ww3_shel の `TYPE%FIELD%LIST` に入れておく。
- ファイル名: `FIELD%SAMEFILE = T` (既定) なら `<PREFIX><日付>.nc` に全変数 (例 `ww3.20080522.nc`)、F なら変数ごと (例 `ww3.200805_hs.nc`)。

## ww3_ounp.nml (点出力 → NetCDF)

テンプレート: `model/nml/ww3_ounp.nml`。

```
&POINT_NML
  POINT%TIMESTART        =  '20080522 000000'
  POINT%TIMESTRIDE       =  '3600'
  POINT%TIMECOUNT        =  '13'
  POINT%LIST             =  'all'          ! 'all' か点の番号 '1 2 3'
  POINT%TYPE             =  2              ! 1: スペクトル、2: パラメータ、3: 源項
/
&FILE_NML
  FILE%NETCDF        = 4
/
&SPECTRA_NML
/
&PARAM_NML
  PARAM%OUTPUT       = 2                   ! 2: 平均波浪パラメータ (Hs など)
/
&SOURCE_NML
/
```

- `POINT_NML` は必須 (無いと止まる。`model/src/w3nmlounpmd.F90:315`)。
- **`POINT%TIMESTRIDE` は必ず書く**。既定は `TIMESTART = '19000101 000000'`・`TIMESTRIDE = '0'` (`model/src/w3nmlounpmd.F90:302-303`) で、
  このままだと ww3_ounf と同じく要求の時刻が進まず、ww3_ounp が終わらない (検証で確認)。
- できるファイル: TYPE 1 → `…_spec.nc`、2 → `…_tab.nc`、3 → `…_src.nc`。`POINT%SAMEFILE = F` なら点ごとに
  `ww3.<点の名前>_<日付>_spec.nc` のように分かれる (検証では `ww3.200805_tab.nc`、`ww3.Point1_196806_spec.nc`)。
- TYPE 2 の中身は `PARAM%OUTPUT` で決まり、既定は 3 (U* で無次元化した量。`ust`・`cd`・`alpha` など) (`model/src/w3nmlounpmd.F90:624`、検証で確認)。
  Hs などの平均波浪パラメータが欲しいなら `PARAM%OUTPUT = 2`。
- TYPE 1 のスペクトルの方向は「向かう方向、北から時計回り」(`DIR = MOD(450 - THD, 360)`、`model/src/ww3_ounp.F90:3245`)。
  この形式は ww3_bounc がそのまま読める ([boundary.md](boundary.md))。
- 源項 (TYPE 3) は保存したスペクトルから計算し直すので、実行と同じビルドで動かす。

## ww3_multi (複数格子) との違い

- 格子ごとに拡張子が格子名になる (`mod_def.<grid>`、`restart.<grid>`、強制場は `wind.<入力格子>`)。
- 制御ファイルは `ww3_multi.nml` (か `.inp`)。各格子の強制場を `'no'`・`'native'`・入力格子名で選ぶ。
- 例: regtest `mww3_test_*`、`ww3_ufs1.*`。手順は LLM.md §4.1・§5.1 (`-m <grid_set>`) と regtest の info。

## テンプレートの誤り

| 場所 | 内容 | 実際 |
|---|---|---|
| `model/nml/ww3_shel.nml:64-68` | 例で `WATER_LEVELS = 'C'`・`CURRENTS = 'C'` | `'C'` は結合 (coupling)。単独の実行では `'T'` か `'F'` |
| `model/nml/ww3_shel.nml:267` | 出力の例 `'… CUR LEV'` | `LEV` というタグは無い (警告を出して無視)。水位は `WLV` |
| `model/inp/ww3_shel.inp:7` | 氷と泥の行は IC1〜IC4 のときだけ | IC5・IS2・BT8・BT9 のときも要る |
| `model/inp/ww3_ounf.inp:82` | IX・IY の範囲の行がコメントになっている | 非 SMC の格子では必須。外すとファイルの終わりで止まる (regtest `ww3_tp2.1/input/ww3_ounf.inp` は書いてある) |
| `model/nml/ww3_ounp.nml:115` | `PARAM%OUTPUT` の既定は 4 | 既定は 3 |
