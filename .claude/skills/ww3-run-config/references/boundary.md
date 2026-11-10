# うねり (開境界のスペクトル) と初期条件

計算領域の外から入ってくるうねりは、開境界の点にスペクトルを与えて入れる。流れは次のとおり:

1. ww3_grid で境界点を決める (マスク値 2 の点)
2. ww3_bounc (NetCDF のスペクトル) か ww3_bound (ASCII) で、境界点ごとのスペクトルを `nest.ww3` にまとめる
3. ww3_shel は、境界点があれば `nest.ww3` を自動で読む (ww3_shel.nml に設定は無い)

## 1. 境界点を決める (ww3_grid)

- 構造格子 (RECT・CURV): 次のどちらか ([grid.md](grid.md) の「海陸マスクと境界点」)。
  - マスクファイルで値を 2 にする。
  - マスクファイルを使わず `INBND_POINT(i) = ix iy connect` で並べる (connect が T なら前の点との間も境界点になる)。
- 非構造格子: `&UNST UGOBCAUTO = T` (既定) でメッシュの開境界を自動で使うか、`UGOBCFILE` のリスト。
- 境界点を作っておいて `nest.ww3` を与えないと、ww3_shel は警告を出すだけで止まらず、境界は初期条件のスペクトルのまま
  一定になる (`model/src/w3iobcmd.F90:334-340`、`model/inp/ww3_grid.inp:836-839`)。
- 読まれたかは `log.ww3` の時間ステップ表の `b` 欄で確かめる (`F` = 最初の読み込み、`X` = 読み込み、`L` = ファイルの終わり。
  `model/src/w3wavemd.F90:1290-1312`)。

## 2a. ww3_bounc: NetCDF のスペクトルから (推奨)

テンプレート: `model/nml/ww3_bounc.nml`。例: regtest `ww3_tp1.11` (repo 内のデータで動く。`.cdl` を `ncgen -b` で `.nc` にする)。

```
&BOUND_NML
  BOUND%MODE    = 'WRITE'              ! 'READ' は既存の nest.ww3 を表示するだけ
  BOUND%INTERP  = 2                    ! 1: 最も近いスペクトル、2: 近い 2 つの間の線形補間
  BOUND%VERBOSE = 1
  BOUND%FILE    = '../input/spec.list' ! NetCDF ファイルの一覧
/
```

既定値は `MODE='WRITE'`・`INTERP=2`・`VERBOSE=1`・`FILE='spec.list'` (`model/src/w3nmlbouncmd.F90:238-241`)。

- `spec.list`: 1 行に 1 ファイル。パスは作業ディレクトリ基準 (`../input/xxx_spec.nc` など)。空行も 1 ファイルと数えるので、末尾に空行を残さない。
- **1 ファイル = 1 地点**: ww3_bounc は各ファイルから位置を 1 つだけ読み (`model/src/ww3_bounc.F90:549-562`)、スペクトルも
  最初の地点の分だけ読む (`:602-604`)。複数の地点を 1 ファイルにまとめると、2 地点目以降は黙って捨てられる。
- 境界点ごとに、位置の近いスペクトルを割り当てる (球面格子なら大円距離)。
- NetCDF の要件 (`model/src/ww3_bounc.F90:496-609`):
  - 次元 `time`・`frequency`・`direction`。
  - 変数 `time` (units と calendar)、位置の `latitude`・`longitude` (直交座標の格子なら `y`・`x`)、`frequency` (Hz)、
    `direction` (度。**向かう方向、北から時計回り**。内部の向きへは 2.5π − θ で変換する)。
  - スペクトル `efth` (か `Efth`)。`_FillValue` 属性が無いと止まる。`scale_factor`・`add_offset` は適用する。
    単位は ww3_ounp と同じ `m2 s rad-1` にする (`units` 属性は読まれず、値を 2π で割って取り込む。`:645-660`)。
  - `station_name` 変数があれば `efth(time, station, frequency, direction)`、無ければ `efth(time, frequency, direction, latitude, longitude)`。
  - これは ww3_ounp が書く `_spec.nc` (`POINT%TYPE = 1`) の形式そのもの。ただし ww3_ounp の既定は `POINT%SAMEFILE = T`
    (全地点を 1 ファイルにまとめる。`model/src/w3nmlounpmd.F90:307`) なので、境界に渡すときは `POINT%SAMEFILE = F` にして
    地点ごとのファイルを作り、`spec.list` に全部並べる (regtest `ww3_tp1.11` の ww3_ounp もこの形)。
- 全ファイルで周波数・方向・時刻の数をそろえる。違うと止まる (`model/src/ww3_bounc.F90:539-545`)。
- ファイルのスペクトル格子がモデルと違えば、変換して取り込む (`:628-640`)。

## 2b. ww3_bound: ASCII のスペクトルから

- 入力は ww3_outp が書く ASCII の転送ファイル。`BOUND_NML` は ww3_bounc と同じ形。例: regtest `ww3_tr1/input_bndin` (`bnd*.spc`)。
- ww3_bounc より厳しい: 入力の NK がモデル以下で、XFR と NTH がモデルと一致しないと止まる (`model/src/ww3_bound.F90:519-547`)。

## 2c. 親の WW3 の実行から入れる (ネスト)

- 方法 1 (テンプレートの推奨。`model/nml/ww3_grid.nml:740-741`): 親で子の境界に沿って出力点を置き、ww3_ounp で
  スペクトル (`POINT%TYPE = 1`、`POINT%SAMEFILE = F`) を地点ごとの NetCDF にして、子の ww3_bounc に渡す。
- 方法 2: 親の ww3_grid に出力境界線 (`OUTBND_LINE(i) = x0 y0 dx dy np`。値は座標) を書き、親の ww3_shel で出力 type 5
  (`DATE%BOUNDARY`) を有効にする。親の作業ディレクトリに `nest<N>.ww3` ができる (`model/src/w3iobcmd.F90:347`) ので、
  子の作業ディレクトリに `nest.ww3` としてコピーする。`np` を負にすると新しいファイル (N が増える)。

## 初期条件 (ww3_strt.inp)

`.inp` だけ (`.nml` は無い)。テンプレートは `model/inp/ww3_strt.inp`。1 行目の最初の文字がコメント記号 (`$`)。

| ITYPE | 初期スペクトル | 追加の行 |
|---|---|---|
| 1 | 周波数と空間はガウス分布、方向は cos^n | `fp σf θm n Xm σx Ym σy Hmax` |
| 2 | JONSWAP | `α fp θm γ σA σB Xm σx Ym σy` (regtest `ww3_tp2.5`) |
| 3 | 開始時刻の局所の風から fetch-limited な JONSWAP | 無し。ww3_shel が風から作るので、風の強制場が要る |
| 4 | 任意のスペクトル | 倍率と NK×NTH の値 |
| 5 | 静穏 (calm) | 無し |
| 6 | 3 と同じで、fetch を m で指定 | fetch |

- ITYPE は 1〜6 以外だと止まる (`model/src/ww3_strt.F90:382-387`)。
- 平均方向 θm は**来る方向、北から時計回り** (630 − θ で内部の向きに変換。`model/src/ww3_strt.F90:470`)。
  テンプレートの "oceanographic" という説明は誤り。
- 実際の風で回すなら 3 か 5 が扱いやすい。3 は検証で確認済み (`log.ww3` に `Restart file read; cold start (wind).` と出る)。
- `restart.ww3` が無いと、ww3_shel は警告を出して cold start する (LN0 なら風から、LN1・SEED なら静穏から。
  `model/src/w3iorsmd.F90:596-612`)。

## restart から続ける

- ww3_shel で `DATE%RESTART = 'start' 'stride' 'stop'` を指定すると、作業ディレクトリに `restart001.ww3`、`restart002.ww3` … ができる
  (`model/src/w3iorsmd.F90:486-503`。検証で確認済み)。`DATE%RESTART2` (checkpoint) は `YYYYMMDD.HHMMSS.restart.ww3` という名前になる。
- 続けるときは、`restartNNN.ww3` を新しい作業ディレクトリの `restart.ww3` にコピーし、ww3_strt は実行しない。
  ww3_shel の `DOMAIN%START` は restart の時刻に合わせる。違うと `CONFLICTING TIMES` で止まる (`model/src/w3iorsmd.F90:634-637`)。
- restart は海点の数 (NSEA) とスペクトルの大きさ (NSPEC) しか照合しない (`model/src/w3iorsmd.F90:553-577`。格子名の違いは警告だけ)。
  - 物理係数・時間刻みを変えても使える。
  - 水深・マスクを変えて海点の数が変わると使えない。
  - NK・NTH を変えずに XFR・FREQ1・THOFF を変えると、照合を通って中身を読み違える。
- ww3_uprstr は restart を補正して `restart001.ww3` を書く。使うときは `restart.ww3` に改名する (`model/src/ww3_uprstr.F90:105-116`)。

## テンプレートの誤り

- `model/inp/ww3_strt.inp` の平均方向の説明 "oceanographic" は誤り。来る方向・北から時計回りで読まれる。
- `model/nml/ww3_grid.nml` の `OUTBND_LINE` の説明 ("index") は誤り。座標で書く ([grid.md](grid.md))。
