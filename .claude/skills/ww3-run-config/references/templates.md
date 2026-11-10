# regtest をテンプレートにする

regtest (`regtests/<case>/`) は、設定ファイル一式 (`input/`) と目的の説明 (`info`) を持つので、新しいケースの出発点に向いている。

## 使い方

1. 下の表から近いケースを選び、`regtests/<case>/info` を読む (目的・必要な switch・実行例)。
2. `regtests/<case>/input/` から必要なファイルを `runs/<case>/input/` にコピーする。
   - regtest の制御ファイルもデータを `../input/<file>` で参照しているので、`runs/<case>/{input,work}` の配置ならそのまま通る
     (例: `regtests/ww3_tp2.5/input/ww3_grid.nml`)。
   - `input` 以外の入力ディレクトリ (`input_*`) のファイルは、`../input/` や別の `../input_*/` を参照していることがあるので、
     コピーしたらパスを直す。
   - `.inp` と `.nml` の両方があれば、同じ内容なので `.nml` を使う。
3. switch は `input/switch*` から選び、`switch_<case>` という名前でコピーする (SKILL.md の手順 3)。
4. regtest は物理を切った (ST0 など) 伝播のテストが多い。実際の海を回すなら、switch を `NCEP_st4` などの preset にし、
   `RUN%FLSOU = T` と物理 namelist を足す ([physics.md](physics.md))。

## repo 内のデータだけで動くケース

| ケース | 見本になる設定 | 主なファイル |
|---|---|---|
| `ww3_tp2.5` | 実際の水深を持つ曲線格子 (北極海、361×361)、JONSWAP の初期値、NetCDF の格子出力。CI で実行するケース | `ww3_grid.nml` (CURV)、`lon/lat/depth.361x361.IDLA1.dat`、`ww3_strt.inp` (ITYPE 2)、`ww3_shel.nml`、`ww3_ounf.nml` |
| `ww3_tp2.4` | 実際の水深を持つ正規緯度経度格子 (北東太平洋 0.35457°、225×106)。`-g curv` で曲線格子版 | `ww3_grid.nml` (RECT)、`depth.225x106.IDLA1.dat` |
| `ww3_tp1.11` | NetCDF の境界スペクトル (ww3_bounc)。`.nml` だけ | `ww3_bounc.nml`、`spec.list`、`ww3.Point0_196806_spec.cdl`、`INBND_POINT`。`ncgen -b -o input/ww3.Point0_196806_spec.nc input/ww3.Point0_196806_spec.cdl` で `.nc` を作り、`spec.list` を `../input/ww3.Point0_196806_spec.nc` に直す (検証ではこの形で動かした) |
| `ww3_tr1` (`input_bndin`) | ASCII の境界スペクトル (ww3_bound)。`input` は親として境界を出力する側。回転極の格子 (switch RTD) | `ww3_bound.nml`、`bnd1..3.spc`、`RTD.mask` (境界点が 2) |
| `mww3_test_08` (`i_lowres_shel`) | ASCII の風と海氷 (ww3_prep)、障害物 | `ww3_prep_wind.inp`、`ww3_prep_ice.inp`、`outer.depth`、`outer.obst` |
| `ww3_tp2.9` | マスク・障害物のファイル (RECT と曲線格子) | `rect_2d.{bot,mask,obs}`、`curv_2d.{bot,mask,obs,lon,lat}` |

この skill の検証では、`ww3_tp2.5`・`ww3_tp1.11`・`ww3_tp2.4` (水深) を元にしたケースを、SKILL.md の手順で `runs/` から実行した。
`ww3_tp2.4` の水深には、合成した風の NetCDF を ww3_prnc で与え、NCEP_st4 で風浪が発達することを確かめた。

## データの取得が要るケース

次のケースのデータは repo に無く、`model/bin/ww3_from_ftp.sh` で NOAA の ftp から取得する (LLM.md §5.1)。
`./dev docker run -- model/bin/ww3_from_ftp.sh` で実行できる。

| ケース | 見本になる設定 |
|---|---|
| `ww3_tp2.15` | ETOPO1 の水深の曲線格子 (アドリア海)、障害物、ww3_prnc の風 (`wind.nc`)、ST4。`input_rho` は FLX5 用の運動量と空気密度 |
| `ww3_tp2.8` | ww3_bounc の境界スペクトル + ww3_prnc の流れ (Iroise 1.5 km、強い潮流)。一様な風 |
| `ww3_tp2.17` | 非構造格子 (PDLIB)、ww3_prnc の風・流れ・水位 (`'AI'` = 節点の並びのまま) |
| `ww3_tp2.12` | 全球 30′ の水深・障害物・マスク (`glo_30m.*`)。これらのファイルは repo にあるので、格子の見本には使える |
| `ww3_ufs1.1`〜`1.3` | 運用 (GFSv16 など) の ww3_multi 構成と、チューニング済みの namelist (namelist のファイルは repo にある) |

## run_cmake_test がしていること (手で実行するときの対応)

`regtests/bin/run_cmake_test` を使わずに実行するときは、次を自分で行う。

| run_cmake_test | SKILL.md の `run.sh` |
|---|---|
| プログラムごとに、入力を `<prog>.nml` (か `.inp`) という名前でリンクし、標準出力を `<work>/<入力ファイル名>.out` に書く (`:1160-1195`) | `exe` が同じことをする |
| `ww3_prnc*`・`ww3_prep*`・`ww3_ounf*`・`ww3_ounp*` などに一致するファイルを、順に同じ名前へリンクし直して実行する | `runeach` が同じことをする |
| 入力ディレクトリの `.cdl` を `ncgen -b` で `.nc` にする (`:594`) | しない。`.nc` を作って `input/` に置き、`../input/<file>.nc` で参照する (例: `ncgen -b -o input/x.nc input/x.cdl`) |
| ww3_uprstr の前に `restart001.ww3` を `restart.ww3` に改名する (`:2322`) | ww3_uprstr は扱っていない |
| ww3_shel の失敗を検出しない (`if $runprog … \| tee $ofile` は tee の終了コードを見る。`:1555`) | 終了コード (`set -e`) と `.out` の `ERROR` で止まる |
| 実行のたびに CMake でビルドする | `./dev build` で 1 回ビルドして使い回す |

## info の誤り

- info の実行例は旧来の `./bin/run_test` の形。`-c intel` などは `run_cmake_test` では使えないので外す。
- `regtests/ww3_tp2.13/info`: `-s PR3_MPI` は無い (`switch_PR3_UQ_MPI` を使う)。
- `regtests/ww3_ufs1.1/info` など: `-s` が無いが、`input/switch` も無い (`-s MPI` などを付ける)。
- `regtests/mww3_test_09/info:40`: ケース名が `mww3_test_03` になっている。
- `regtests/ww3_tp2.19`: info が無い (`regtests/bin/matrix.base` の実行例を見る)。
