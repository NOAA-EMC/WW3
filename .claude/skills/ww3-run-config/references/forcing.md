# 風・流れ・水位・海氷 (強制場)

強制場は、前処理で WW3 の形式 (`wind.ww3` など) に変換し、ww3_shel が読む。

1. ww3_prnc (NetCDF から) か ww3_prep (ASCII・バイナリから) で `<field>.ww3` を作る。1 回の実行で 1 種類。
2. ww3_shel の `INPUT%FORCING%<field> = 'T'` にした種類だけが読まれる ([shel-output.md](shel-output.md))。
3. 時間の補間は ww3_shel が、switch の `WNT*`・`WNX*` (風)・`CRT*`・`CRX*` (流れ) に従って行う。ww3_prnc は時間の補間をしない。

できるファイルの名前は種類で決まり、入力ファイルの名前には依らない (`model/src/w3fldsmd.F90:296-347`):

| 種類 (prnc の FIELD / prep の ID) | できるファイル |
|---|---|
| `WINDS` / `WND`、`WINDS_AST` / `WNS` (風 + 気温と海面水温の差) | `wind.ww3` |
| `CURRENTS` / `CUR` | `current.ww3` |
| `WATER_LEVELS` / `LEV` | `level.ww3` |
| `ICE_CONC` / `ICE`、`ICE_BERG` / `ISI` | `ice.ww3` |
| `ATM_MOMENTUM` / `TAU` | `momentum.ww3` |
| `AIR_DENSITY` / `RHO` | `density.ww3` |
| `ICE_PARAM1`〜`5` | `ice1.ww3`〜`ice5.ww3` |

同じ種類を 2 回処理すると上書きされる。複数の種類を使うなら `input/ww3_prnc_<field>.nml` を種類ごとに作る。
SKILL.md の `run.sh` (`runeach ww3_prnc`) が `input/ww3_prnc*.nml` を 1 つずつ `ww3_prnc.nml` にリンクして実行する
(regtest の `ww3_prnc.nml` という名前もそのまま拾う)。

## ww3_prnc.nml

テンプレート: `model/nml/ww3_prnc.nml`。検証で使った例:

```
&FORCING_NML
  FORCING%TIMESTART         = '20080522 000000'   ! 取り込む期間 (両端を含む)
  FORCING%TIMESTOP          = '20080522 120000'
  FORCING%FIELD%WINDS       = T                   ! 種類を 1 つだけ T にする
  FORCING%GRID%LATLON       = T                   ! 緯度経度の格子からモデル格子へ双線形補間
/
&FILE_NML
  FILE%FILENAME      = '../input/wind.nc'         ! 作業ディレクトリ基準のパス
  FILE%LONGITUDE     = 'longitude'                ! 次元の名前
  FILE%LATITUDE      = 'latitude'
  FILE%VAR(1)        = 'u10'                      ! 変数の名前を成分の順に (東向き、北向き)
  FILE%VAR(2)        = 'v10'
/
```

- `FORCING_NML` と `FILE_NML` は必須 (無いと止まる。`model/src/w3nmlprncmd.F90:309-316`)。
- `FORCING%GRID%ASIS = T` は、NetCDF がモデルの格子と同じ並びのとき (補間しない)。非構造格子では節点の並びのデータに使う。
- 取り込む期間の前の記録は読み飛ばし、後の記録で終わる (`model/src/ww3_prnc.F90:1860-1865`)。
- `FILE%TIMESHIFT` を 0 以外にすると、記録の時刻を書き換える (`:605-610`。使うときはテンプレートの説明を読む)。

## NetCDF の要件 (ww3_prnc)

| 項目 | 要件 | 出典 (`model/src/ww3_prnc.F90`) |
|---|---|---|
| 時刻 | 変数 `time` (か `MT`)。units は `hours since 2008-05-22 00:00:00` のような形。**次元**の名前も `time` (か `MT`) にする。違う名前だとエラーにならないまま記録の数が決まらない | `:816-818`、`:868-870` |
| calendar | `standard`・`gregorian`・`360_day`。属性が無ければ standard とみなす。モデルの `&MISC CALTYPE` (既定 standard) と一致しないと止まる | `:819-839` |
| 欠損値 | 読む変数すべてに `_FillValue` が必要。無いと止まる。`scale_factor`・`add_offset` は適用する | `:856-862` |
| 座標変数 | 名前で探す: `longitude`・`lon`・`Longitude`・`x`・`X` (緯度も同様) | `:895-902` |
| 並び | `var(time, lat, lon)` | |
| 緯度の向き | モデルが RECT (正規緯度経度) の格子なら、緯度は南から北への昇順。降順だと `LATITUDE VALUES MUST BE REVERSED` で止まる (`ncpdq -h -O -a -lat file.nc` で並べ替える) | `:977-982`、`:2411-2413` |
| 範囲 | 元データの範囲外のモデル格子点は、警告を出して補間の重みを 0 にする (止まらず、その点の強制場は 0 になる)。出たら元データの範囲を広げる | `:1063-1076` |
| ファイル名 | `FILE%FILENAME` は 80 文字で切れる (`../input/` を含めて数える) | `:315`、`:585` |

検証で ww3_prnc を通った最小の例 (CDL。`ncgen -b -o wind.nc wind.cdl` で NetCDF にする):

```
netcdf wind {
dimensions:
	time = UNLIMITED ;
	latitude = 10 ;
	longitude = 18 ;
variables:
	double time(time) ;
		time:units = "hours since 2008-05-22 00:00:00" ;
		time:calendar = "standard" ;
	float latitude(latitude) ;
		latitude:units = "degrees_north" ;
	float longitude(longitude) ;
		longitude:units = "degrees_east" ;
	float u10(time, latitude, longitude) ;
		u10:units = "m s-1" ;
		u10:_FillValue = -9999.f ;
	float v10(time, latitude, longitude) ;
		v10:units = "m s-1" ;
		v10:_FillValue = -9999.f ;
data:
	...
}
```

## ww3_prep.inp (ASCII・バイナリから)

- `.inp` だけ。テンプレートは `model/inp/ww3_prep.inp`。例: regtest `mww3_test_08/i_lowres_shel` (`ww3_prep_wind.inp`・`ww3_prep_ice.inp`)。
- 1 行目に `'WND' 'LL' T T` のように、種類・格子の種類・時刻の有無・ヘッダの有無を書き、続けてデータファイルの書式とファイル名を書く。
  格子の種類は `AI` (モデル格子と同じ並びのまま)・`LL` (直線格子)・`F1` (曲線格子)・`F2` (F1 で 2 つの場を合成)
  (`model/inp/ww3_prep.inp:4-32`)。
- 種類ごとに 1 回実行する点は ww3_prnc と同じ。

## 一様な強制場 (ww3_shel だけで与える)

前処理をせず、全域で一様な値を ww3_shel に直接書く。テスト向け。

```
&INPUT_NML
  INPUT%FORCING%WINDS = 'H'
/
&HOMOG_COUNT_NML
  HOMOG_COUNT%N_WND = 1
/
&HOMOG_INPUT_NML
  HOMOG_INPUT(1)%NAME   = 'WND'
  HOMOG_INPUT(1)%DATE   = '20080522 000000'
  HOMOG_INPUT(1)%VALUE1 = 10.     ! 風速 (m/s)
  HOMOG_INPUT(1)%VALUE2 = 270.    ! 風向: 来る方向、北から時計回り (内部では 270 − θ。model/src/w3fldsmd.F90:2377)
/
```

- `'H'` にしてエントリが 1 つも無いと止まる (`model/src/ww3_shel.F90:1185-1205`)。1 種類あたり 200 エントリまで (`NHMAX`、`:320`)。

## 実行中の確かめ方

- `log.ww3` の時間ステップ表で、入力欄 (`w` 風・`l` 水位・`c` 流れ・`t` 運動量・`r` 空気密度・`i` 海氷) に、
  強制場を読んだステップで `F` (最初)・`X` が付く。
- 期間の途中で強制場の記録が尽きても実行は止まらず、`Past last <field>` と出力される (`model/src/ww3_shel.F90:2510`)。
  取り込む期間 (`FORCING%TIMESTART`/`TIMESTOP`) が ww3_shel の実行期間を覆っているかを先に確かめる。

## テンプレートの誤り

- `model/nml/ww3_prnc.nml:29-31` のコメントにある `FORCING%FIELD%WIND_AST`、`INPUT%FORCING%ATM_MOMENTUM`、`INPUT%FORCING%AIR_DENSITY` は誤り。
  正しくは `FORCING%FIELD%WINDS_AST`、`FORCING%FIELD%ATM_MOMENTUM`、`FORCING%FIELD%AIR_DENSITY` (`model/src/w3nmlprncmd.F90` の型定義)。
