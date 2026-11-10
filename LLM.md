# LLM Instructions — WW3 repo guide

This file provides guidance to Claude Code (claude.ai/code) and Codex when working with code in this repository.
It is the single source of truth for both: `CLAUDE.md` and `AGENTS.md` are symlinks to this file.

WAVEWATCH III (WW3) v7.14 の fork (`origin` = `oceanic-constellations/sim-WW3`、既定ブランチ `main`。upstream は NOAA-EMC/WW3 の `develop`)。
波作用密度スペクトルの平衡方程式を解く Fortran (F90 + CPP) の波浪モデルで、物理・数値スキーム・並列化・I/O の選択は
**すべてコンパイル時の switch で決まる** (§3)。ここに無い詳細は §8 の一次資料を参照。

---

## 1. ディレクトリの役割

```
model/src/             Fortran ソース。モジュールとプログラムがフラットに並ぶ (命名は §4.2)
  cmake/               switches.json (switch の検証・依存・追加ソース) / src_list.cmake (常時ビルドするソース)
  PDLIB/  SCRIP/       非構造格子の領域分割 (switch PDLIB) / 格子間補間 (SCRIP, SCRIPNC)
model/bin/             switch_<name> (各センターの構成プリセット)、レガシー make ビルド、ww3_from_ftp.sh
model/inp/ model/nml/  各プログラムの入力テンプレート (.inp = 旧書式、.nml = namelist 書式)
model/tools/bash/      .inp → .nml 変換スクリプト
regtests/              回帰テスト: <case>/ (info + input*/) と bin/ (実行・比較ドライバ)
manual/                LaTeX のユーザーマニュアル (支配方程式・数値解法・入力仕様)
docs/                  Doxygen 設定 / smc_docs/ SMC 格子生成ツール
```

---

## 2. ビルド (CMake。1 ビルドディレクトリ = 1 switch 構成)

```bash
mkdir build && cd build                 # build/ と install/ は .gitignore 済み
cmake .. -DSWITCH=NCEP_st4              # = model/bin/switch_NCEP_st4。switch ファイルの絶対パスも可
make -j8                                # → build/bin/ww3_*、build/lib/libww3.a
cmake --build . --target ww3_shel       # 単一ターゲットだけ
cmake .. -DCMAKE_BUILD_TYPE=Debug       # 既定は Release
```

- 必要なもの: CMake ≥ 3.19、Fortran + C コンパイラ (フラグ定義は Intel / GNU / PGI / Cray)。
  switch に `MPI` を含むなら `CC=mpicc FC=mpif90` を export する (CI と同じ)。
- 依存ライブラリは switch で決まる: NetCDF-Fortran (`-DNETCDF=ON` が既定。無い環境では `-DNETCDF=OFF` で
  ww3_ounf / ounp / bounc / trnc / prnc を外す)、ParMETIS か SCOTCH (`PDLIB`)、OASIS3-MCT (`OASIS`、env `OASISDIR`)、
  NCEPLIBS g2 / bacio / w3emc (`NCEP2`)、ESMF (`-DMULTI_ESMF=ON`。実行ファイルは作らず `libww3_multi_esmf` だけ)。
  探索パスは `<Pkg>_ROOT` か `CMAKE_PREFIX_PATH`。
- その他: `-DENDIAN=BIG|LITTLE|NATIVE` (unformatted 出力。既定 BIG)、`-DENABLE_DOCS=ON` + `make enable_docs` (Doxygen)。
- switch ファイルを変えると CMake が再構成され、全ソースを再コンパイルする。構成を並行して持つならビルドディレクトリを分ける。
- CI (`.github/workflows/gnu.yml`, `intel.yml`) は switch `Ifremer1 NCEP_st2 NCEP_st4 ite_pdlib NCEP_st4sbs NCEP_glwu OASACM UKMO`
  と `MULTI_ESMF` をビルドするだけ。実行するのは `regtest_gnu.yml` の `ww3_tp2.5` 1 ケースで、結果の比較はしない。
  依存ライブラリは Spack (`model/ci/spack_*.yaml`) で入れている。
- `model/bin/w3_setup` + `w3_make` はレガシーの make ビルド (`regtests/bin/run_test` が使う)。新規作業は CMake 系 (`run_cmake_test`) を使う。

---

## 3. switch システム (最重要)

- switch ファイルは空白区切りの単語列 (例 `switch_NCEP_st4`: `NCO NOGRB DIST MPI PR3 UQ ST4 STAB0 FLX0 LN1 NL1 … O0 O1 O2 …`)。
  各単語 `X` は `ww3_lib` に `-DW3_X` (PUBLIC) として定義され、コードは `#ifdef W3_X` で分岐する (model/src 全体で約 8,300 か所)。
  **編集した箇所が、ビルドする構成で実際にコンパイルされるか**を必ず確かめる。`#ifdef` を触ったら、関係する構成
  (最低でも SHRD 系と MPI 系) でビルドを通す。
- `model/src/cmake/switches.json` がカテゴリ単位の制約の一次情報源 (`model/README.md` にある `bin/switches.json` は誤記):
  - `num_switches`: `one` (ちょうど 1 つ必須) / `upto1` / `upto2`
  - `requires` / `conflicts`: 違反すると cmake 実行時に FATAL_ERROR
  - `build_files`: その switch で追加コンパイルするソース (例: `ST4` → `w3src4md.F90`)。PDLIB / SCRIP の追加ソースは
    `model/src/CMakeLists.txt` 側で処理する

必須カテゴリ (`one`) の主なもの:

| 対象 | 選択肢 |
|---|---|
| 並列 | `SHRD` か `DIST` + `MPI` |
| GSE 対策 / 伝播スキーム | `PR0`–`PR3` / `PR0`・`PR1`・`UQ`・`UNO` |
| 応力 / 入力・白波 | `FLX0`–`FLX5` / `ST0`–`ST4`・`ST6` (ST と FLX の組み合わせに conflict 制約あり) |
| 非線形 4 波 / 線形入力 | `NL0`–`NL5` / `LN0`・`SEED`・`LN1` |
| 浅海・底面 | `BT0`・`BT1`・`BT4`・`BT8`・`BT9`、`DB0`・`DB1`、`TR0`・`TR1`、`BS0`・`BS1` |
| 海氷 / 反射 | `IC0`–`IC5`、`IS0`–`IS2` / `REF0`・`REF1` |
| 風・流れの時空間補間 | `WNT0`–`2`・`WNX0`–`2`・`CRT0`–`2`・`CRX0`–`2` |
| GRIB | `NOGRB`・`NCEP2` |

任意: `SMC`、`PDLIB` (+ `METIS` か `SCOTCH`、要 `MPI`)、`OMPG`・`OMPH`、`OASIS` 系、`TIDE`、`B4B`、`SETUP`、`UOST`、`IG1`、`RTD`、`NLS`、`MLIM` など。

- **switches.json に無い単語は検証されずに、そのまま `-DW3_*` になる**: 出力系 `O0`–`O16` (`O2a` などを含む)、
  デバッグ用 `S` (`STRACE` によるサブルーチン追跡)、テスト出力 `T`・`T0`–`T9` など。typo しても黙って無効になるだけなので、
  `grep -n "W3_<X>\b" model/src/*.F90` で実在を確認する。旧 `.ftn` 前提の `model/bin/all_switches` と `find_switch` は現状動かない。
- 新しい switch を足すとき:
  1. `switches.json` の該当カテゴリ (無ければ新カテゴリ) に `build_files` / `requires` / `conflicts` 付きで追加する
  2. 新規ソースは、常時ビルドなら `src_list.cmake` の `ftn_src` へ、switch 依存なら `build_files` へ入れる
  3. レガシー make と `sort_switch` も通すなら、`model/bin/build_utils.sh` の対応表も更新する

---

## 4. アーキテクチャ

### 4.1 プログラムはファイルでつながるパイプライン

```
ww3_grid              → mod_def.ww3   格子・スペクトル格子・物理パラメータ (他の全プログラムが最初に読む)
ww3_strt              → restart.ww3   初期条件
ww3_prep / ww3_prnc   → wind.ww3, current.ww3, level.ww3, ice.ww3 などの強制場 (prnc は NetCDF 入力)
ww3_bound / ww3_bounc → nest.ww3      境界スペクトル (bounc は NetCDF 入力)
ww3_shel (単一格子) / ww3_multi (多格子モザイク)
                      → out_grd.ww3 (格子出力)、out_pnt.ww3 (点出力)、restartNNN.ww3、nestN.ww3、track_o.ww3、partition.ww3
後処理: ww3_outf / ww3_ounf (格子)、ww3_outp / ww3_ounp (点)、ww3_trck / ww3_trnc (track)、ww3_grib、
        gx_outf / gx_outp (GrADS)、ww3_systrk (波系追跡)、ww3_gint (格子統合)、ww3_uprstr (restart 更新・同化)、
        ww3_gspl (格子分割)、ww3_prtide (TIDE 時のみ)
```

- 拡張子は単一格子なら `ww3`、`ww3_multi` では格子名 (`mod_def.<grid>`)。
- `model/nml/` にあるプログラム (grid, shel, multi, bound, bounc, prnc, outp, ounf, ounp, trnc, uprstr) は
  `<prog>.nml` があればそちらを読み、無ければ `<prog>.inp` を読む。他は `.inp` だけ。namelist の読み込みは `w3nml*md.F90`。
- 物理パラメータの namelist (`&SIN4`、`&SDS4`、`&MISC` …) は `ww3_grid` が読んで **mod_def に書き込む**
  (`ww3_grid.inp` では `END OF NAMELISTS` までの区間、`ww3_grid.nml` では `GRID%NML` が指すファイル)。値を変えたら ww3_grid からやり直す。

### 4.2 ソースの命名 (フラットな model/src を探すための地図)

- `ww3_*.F90`・`gx_*.F90` = プログラム (実行ファイル一覧は `model/src/CMakeLists.txt` の `programs`)。
  `ww3_grid` の本体は `w3gridmd.F90`。`w3*md.F90` = モジュール、`wm*md.F90` = 多格子ドライバ。
- **物理項は switch 番号 = ファイル番号**: `ST<n>` → `w3src<n>md` (入力・白波)、`NL<n>` → `w3snl<n>md`、`BT<n>` → `w3sbt<n>md`、
  `IC<n>` / `IS<n>` → `w3sic<n>md` / `w3sis<n>md`、`FLX<n>` → `w3flx<n>md`、
  `DB1`・`TR1`・`BS1`・`REF1`・`LN1` → `w3sdb1md`・`w3str1md`・`w3sbs1md`・`w3ref1md`・`w3sln1md`。
- 伝播: `PR<n>` → `w3pro<n>md`、`UQ` → `w3uqckmd`、`UNO` → `w3uno2md`、SMC 格子 → `w3psmcmd`・`w3smcomd`、
  非構造格子 → `w3profsmd` (explicit。PR1–3 で常にビルド) と `w3profsmd_pdlib` + `PDLIB/` (領域分割・implicit)。
- I/O: `w3iogrmd` (mod_def)、`w3iorsmd` (restart)、`w3iogomd` (out_grd)、`w3iopomd` (out_pnt)、`w3iobcmd` (nest)、
  `w3iotrmd` (track)、`w3iosfmd` (partition)、`w3fldsmd` (強制場ファイル)。
- 結合: OASIS = `w3oacpmd` + `w3agcmmd`・`w3ogcmmd`・`w3igcmmd`、ESMF = `wmesmfmd`。

### 4.3 データモジュールと「現在の格子」ポインタ (最大のハマりどころ)

- 状態は 6 つのデータモジュールに分かれる: `w3gdatmd` (格子と物理パラメータ = mod_def の中身)、`w3wdatmd` (波浪状態:
  作用密度スペクトル `VA`、時刻など)、`w3adatmd` (補助・診断量)、`w3idatmd` (入力強制場)、`w3odatmd` (出力設定と
  MPI rank 情報 `IAPROC`・`NAPROC`)、`wmmdatmd` (多格子)。
- 各モジュールは格子ごとの派生型配列 (`GRIDS(:)`、`WDATAS(:)`、`WADATS(:)`、`INPUTS(:)`、`OUTPTS(:)`、`MDATAS(:)`) と、
  その成分を指す同名のモジュール変数 POINTER (`NX`、`NSEA`、`MAPSTA`、`VA` …) を持つ。
- `W3SETG(IMOD, …)`・`W3SETW`・`W3SETA`・`W3SETI`・`W3SETO`・`WMSETM` がポインタを `GRIDS(IMOD)%…` などに張り替える。
  **どの格子のデータに触れているかは、直前の W3SETx 呼び出しで決まる** (ww3_multi では頻繁に切り替わる)。
- 確保の順序: `W3NMOD`・`W3NDAT`・`W3NAUX`・`W3NINP`・`W3NOUT` (格子数) → `W3DIMX`・`W3DIMS`・`W3DIMW`・`W3DIMA`・`W3DIMI` (配列) → `W3SETx`。
- 変数を足すときは **派生型メンバ・同名ポインタ宣言・`W3SETx` の `=>`・必要なら `W3DIMx` での確保** の 4 か所をそろえる。
  ファイルに保存するなら §4.6 も対象になる。

### 4.4 時間積分

- `ww3_shel` → `W3INIT` (w3initmd: mod_def・restart の読み込み、並列分割、出力の初期化) → 入力更新の区切りごとに `W3WAVE`。
- `ww3_multi` → `WMINIT` → `WMWAVE` (wmwavemd) が格子ごとに `W3WAVE` を回す。格子間の関係 (nesting・overlap) は `wmgridmd`、
  格子間の境界・データ交換は `wminiomd` が受け持つ。
- `W3WAVE` (w3wavemd.F90) の 1 グローバルステップ: 入力の時間補間 → (implicit 解法なら) `W3SRCE(srce_imp_pre)` →
  スペクトル空間の伝播 `W3KTPn` (前半) → 空間伝播 (規則・曲線格子 `W3XYPn`、SMC `W3PSMC`、非構造 `W3XYPUG` / `PDLIB_*`) →
  `W3KTPn` (後半) → 源項 `W3SRCE(srce_direct | srce_imp_post)` → 出力 (`W3IOGO`、`W3IOPO`、`W3IORS`、`W3IOBC`、`W3IOTR`、`W3IOSF`)。
- `W3SRCE` (w3srcemd.F90) は海点 1 点ぶんのスペクトルを受け取り、switch に応じて `W3SIN<n>`・`W3SDS<n>`・`W3SNL<n>`・`W3SBT<n>` …
  を呼んで動的時間刻みで積分する。新しい物理項も、ここに `#ifdef` 分岐でつなぐ。

### 4.5 並列化

- スペクトル `VA(ISPEC, JSEA)` の海点は MPI rank に round-robin で分配される (`ISEA` = 全体の海点番号、`JSEA` = rank 内番号、
  `ISEA = IAPROC + (JSEA-1)*NAPROC`)。変換には `w3parall.F90` の `INIT_GET_ISEA`・`INIT_GET_JSEA_ISPROC` を使い、
  式を直書きしない (PDLIB では対応が違う)。
- 源項は rank ローカルの海点ごとに計算する。空間伝播は `W3GATH` で 1 スペクトル成分を全海点ぶん集め、伝播してから
  `W3SCAT` で戻す (成分単位の並列)。
- `PDLIB` (非構造格子) は ParMETIS / SCOTCH による領域分割 + halo 交換 (`PDLIB_exchange*`) という別系統。
- OpenMP は `OMPG` (単独) / `OMPH` (MPI とのハイブリッド)。
- MPI 数・スレッド数・restart をまたいだ bit 一致が重視される (switch `B4B`、matrix の `rstrt_b4b`・`npl_b4b`・`nth_b4b`)。

### 4.6 バイナリファイルの版管理

- `w3iogrmd` の `VERGRD` (mod_def)、`w3iorsmd` の `VERINI` (restart)、`w3iogomd` の `VEROGR` (out_grd)、`w3iopomd` の
  `VEROPT` (out_pnt) の日付文字列がファイルに書かれ、読み込み時に照合される。レイアウトを変えたら日付を更新し、
  PR に *mod_def change* / *restart file change* / *out_grd change* / *out_pnt change* と書く。
- 版番号は `VERSION` と `w3initmd.F90` の `WWVER` をそろえる。

---

## 5. テスト

### 5.1 regtests (主な検証手段)

```bash
cd regtests                     # ここから実行する (ケース名は cwd からの相対パスで解決される)
./bin/run_cmake_test -s PR1 -w work_PR1 ../model ww3_tp2.1                              # 逐次
./bin/run_cmake_test -s PR1_MPI -w work_PR1_MPI -f -p mpirun -n 4 ../model ww3_tp2.5    # MPI
```

- 引数は `../model` (model dir) とケース名。`-s X` → `<case>/input/switch_X` (省略時は `input/switch`)、`-w` 作業 dir
  (既定 `work`)、`-i` 入力 dir、`-g x` → `ww3_grid_x.inp`、`-N` .nml 入力、`-m <grid_set>` ww3_multi で実行、
  `-f` 前後処理を SHRD (非 MPI) でビルド、`-p` / `-n` MPI 実行、`-o native|netcdf|both|grib|all` 後処理の形式。全オプションは `-h`。
- ケースごとの `<case>/info` に目的・必要な switch・実行例が書いてある。系統: `ww3_tp1.*` / `ww3_tp2.*` 伝播 (非構造・SMC・
  結合なども含む)、`ww3_ts*` 源項、`ww3_tic*` 海氷、`ww3_tbt*` 泥底、`ww3_tig*` 長周期波、`ww3_ufs*` UFS 構成、
  `mww3_test_*` ww3_multi など。
- ビルドは `<case>/<work>/build` (env `path_build_root` で変更でき、cmake への追加引数は env `CMAKE_OPTIONS`)、
  実行ファイルは `<work>/exe/`、ビルドログは `<work>/build.log`、各プログラムの標準出力は `<work>/<入力ファイル名>.out`。
  `regtests/*/work*` と `regtests/build*` は .gitignore 済み。
- **合否判定は無い**。変更前と変更後の work dir の出力を比べて判断する。
- 外部データが要るケース (ww3_tp2.8・2.12・2.14・2.15・2.17・2.18・2.19・2.21、ww3_tic1.4、ww3_ufs1.*、mww3_test_09) は、
  先に `model/bin/ww3_from_ftp.sh` を実行する (NOAA の ftp から取得して regtests に展開する)。`-k` を付けると残る
  `ww3_from_ftp.v7.14.2.tar.gz` は .gitignore の対象外なので commit しない。
- 網羅実行 (matrix): `./bin/matrix_cmake_<site> ../model [intel|gnu]` が `matrix` (分割した `matrixNN` と `msuball.sh`) を生成
  → 実行 → `./bin/matrix.comp all <基準の regtests dir> <比較する regtests dir>` で別 checkout と比較する。
  site スクリプトは各 HPC 向けなので、他の環境ではコピーして直す (スクリプト冒頭の注記どおり)。
  全ケースと有効化フラグの一次情報源は `regtests/bin/matrix.base`。
- 新規ケースは `<case>/info` と `input/` を作り、`matrix.base` に追記する (`matrix.comp` はケース dir を glob で拾うので、既存の命名に合わせる)。

### 5.2 unit tests

- `regtests/unittests/` (CTest + `test_io*.F90`) はあるが、トップの `CMakeLists.txt` で `include(CTest)` と
  `add_subdirectory(regtests/unittests)` がコメントアウトされていて、既定では無効。

---

## 6. コード規約

- Fortran 90 free-form の `.F90` + CPP。2 スペースインデント、行末空白なし (#808 で全体を整形済み)。
  既存コードはキーワード大文字が主流なので、周囲に合わせる。
- ほぼ全ファイルが `#include "w3macros.h"` (`CHECK_ALLOC_STATUS` などのマクロ) を取り込む。エラー終了は `EXTCDE` (w3servmd)。
- Doxygen: FILE / MODULE / SUBROUTINE / FUNCTION / PROGRAM の直前に `!> @brief …`、モジュール変数には行末の `!<`
  (テンプレートは `docs/ww3_doxy_tmpl.md`)。旧来の `!/` ヘッダ (版履歴、`1. Purpose` …) も残っているので、既存ファイルではその書式を踏襲する。
- デバッグ用の既存パターンを踏襲する: `#ifdef W3_S` で `STRACE`、`#ifdef W3_T` 系でテスト出力。
- 既定の REAL は単精度 (Intel `-real-size 32`、PGI `-r4`)。
- upstream は警告ゼロ化を進めている: Debug ビルドの警告を増やさない。手続きの参照は `EXTERNAL` ではなく明示的な `INTERFACE` にする (#1521)。

---

## 7. PR

`.github/pull_request_template.md` に従う: answer change (regtest の出力が変わるか) の有無と理由、
*mod_def / out_grd / out_pnt / restart file change* のラベル、matrix.comp の結果 (`matrixCompSummary.txt` など)、
新機能には regtest を追加、squash merge 用の Commit Message 欄。

---

## 8. 一次資料

- `model/README.md` — CMake の詳細 (ライブラリ探索、`EXCLUDE_FIND`、FAQ)
- `model/bin/README.md` — 各センター (NCEP / Ifremer / NRL / UKMO / Univ. of Melbourne) の switch と namelist の設定例
- `manual/` — ユーザーマニュアル (`make` で PDF。latex / bibtex / dvipdf が必要)。式は `eqs/`、数値解法は `num/`、実行は `run/`
- Doxygen HTML: https://noaa-emc.github.io/WW3 ・ Wiki: https://github.com/NOAA-EMC/WW3/wiki (Quick Start、Code Management、matrix.comp の使い方)
