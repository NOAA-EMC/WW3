# 物理と数値スキーム: switch と namelist

## どこで決まるか

| 決めるもの | ファイル | 変えたら |
|---|---|---|
| 物理パッケージの選択 (ST4 か ST6 か、NL1、BT1 …)、伝播スキーム、並列化、強制場の時空間補間、出力 switch | switch (`runs/<case>/input/switch_<case>`) | `./dev build` → ww3_grid から全部 |
| 各パッケージの係数 (`&SIN4 BETAMAX` など) | 物理 namelist (`GRID%NML` のファイル、`.inp` なら本文中) | ww3_grid → ww3_shel → 後処理 |

- ww3_grid は namelist の値を `mod_def.ww3` に書き込む。ww3_shel は実行時に namelist を読まない。
- mod_def に書かれた物理パッケージ名は、読み込む側のビルドの switch と照合され、違うと止まる
  (照合は `model/src/w3iogrmd.F90:687-750`、文は `:383-388`)。ww3_grid から後処理まで同じビルドで動かす。
- switch の文法・カテゴリ・`switches.json` の規則は LLM.md §3。

## switch の選び方

`model/bin/switch_<preset>` を `runs/<case>/input/switch_<case>` にコピーして編集する。実用的な preset:

| preset | 要点 | 向いている用途 |
|---|---|---|
| `NCEP_st4` | `DIST MPI PR3 UQ ST4 FLX0 LN1 NL1 BT1 DB1 MLIM IC0 IS0 REF0 WNT1 WNX1 CRT1 CRX1` (と `NCO NOGRB O0 …`) | 構造格子の標準的な構成。この skill の検証で使った |
| `Ifremer1` | ST4 に `BT4` (SHOWEX 底面摩擦)・`REF1` (海岸反射)・`TIDE`・`RWND`・`WNT2` | 沿岸 |
| `ite_pdlib` | Ifremer1 に近い物理 + `PDLIB SCOTCH` | 非構造格子 (領域分割・陰解法) |
| `UoM_nl1` | `ST6 FLX4 LN1 NL1` | ST6 |
| `NCEP_st2` | `ST2 STAB2 FLX3` | ST2 |
| `SMCMlt` | `SHRD PR2 UNO SMC` + ST4 | SMC 格子 |

- `UKMO_gbl`・`UKMO_uk` (`PR*` と `UQ`/`UNO` が無い) と `swin` (`IS*` が無い) は、そのままでは CMake の検査で失敗する。
- 組み合わせの制約 (`model/src/cmake/switches.json`):
  - ST3・ST4 は FLX1〜FLX4 と組めない (FLX0 か FLX5)。ST6 は FLX0 と組めない (preset は FLX4)。ST2 は FLX2 か FLX3。
  - PDLIB・OASIS は MPI が必要。OMPH は MPI と OMPG が必要。
  - PDLIB は METIS か SCOTCH も必要 (この検査は `model/src/CMakeLists.txt:152-170`)。
- `NCO` は NCEP の運用向けに装置番号の割り当てなどを変える (`model/src/ww3_shel.F90:618-636`)。出力ファイルの名前は変わらない。
- `O0` が無いと、ww3_grid は最終的な namelist の値の一覧を出さない。`(user def. values)` の表示は `O0` が無くても出る。
- 強制場の時空間補間 (`WNT*`・`WNX*`・`CRT*`・`CRX*`) は switch で決まる (ww3_prnc は時間の補間をしない)。

### 風から応力への変換 (FLX)

- ST4 + FLX0: ST4 自身が応力を計算する (標準)。
- ST4 + FLX5: 応力を強制場から与える (`UST = SQRT(TAUA/RHOAIR)`、`model/src/w3flx5md.F90:94`)。
  大気の運動量 (`ATM_MOMENTUM`) の強制場が要る。無いと `TAUA = 0.01` のまま黙って進む。空気密度 (`AIR_DENSITY`) は、
  無ければ既定の値 (`DAIR`) を使う (`model/src/w3wavemd.F90:1270-1280`)。例: regtest `ww3_tp2.15/input_rho` ([forcing.md](forcing.md))。
- `&SIN4 ZWND` は入力する風の高さ (m、既定 10)。10 m 風でないなら変える。

## namelist の渡し方

- `.nml`: `ww3_grid.nml` の `GRID%NML` にファイル名を書く (work 基準のパス。例 `'../input/namelists.nml'`)。
  ファイルが無いと止まる (`STATUS='OLD'`)。末尾に `END OF NAMELISTS` を書く慣習だが、`.nml` 経由では検査されない。
- `.inp`: `ww3_grid.inp` の時間刻みの行の後から、`END OF NAMELISTS` (1 桁目から) の行まで (`model/src/w3gridmd.F90:1615-1629`)。
- 書き方は `&SIN4 BETAMAX = 1.33, ZWND = 10. /`。1 つの group を複数行に分けてよい (例: `regtests/ww3_ufs1.1/input/namelists_glo_1deg.nml`)。
- 読まれ方 (`model/src/w3gridmd.F90:7337-7531`):
  - 行の最初の空白以外の文字が `&` で、続く 4 文字が group 名の行だけを namelist として読む。`$ &SIN4 …` はコメント扱い。
  - 同じ group が 2 つあると、最初のものだけが使われる。
  - コンパイルしていない switch の group は探されないので、書いても黙って無視される (`model/inp/ww3_grid.inp:36-38`)。
  - group の中の変数名を誤ると、読み込みエラーで ww3_grid が止まる。
- `.inp` の行は 80 桁で切れる (`LINE*80`、`model/src/w3gridmd.F90:787`)。長いときは複数行に分ける。
- 読まれたかどうかは `ww3_grid.out` の `(user def. values)` で確かめる (読まれなければ `(default values)`)。

## 主な group と既定値

| group (switch) | 主な変数 (既定値) | 出典 |
|---|---|---|
| `&SIN4` (ST4) | `BETAMAX` 1.43、`ZWND` 10、`ALPHA0` 0.0095、`ZALP` 0.006、`TAUWSHELTER` 0.3、`SWELLF` 0.66、`SINTHP` 2 | `model/src/w3gridmd.F90:1727-1756` |
| `&SDS4` (ST4) | `SDSBCHOICE` 1、`SDSC2` −2.2e-5、`SDSCUM` −0.40344、`SDSC6` 0.3、`SDSBR` 0.9e-3、`FXFM3` 2.5 | `:2149-2198` |
| `&SNL1` (NL1) | `LAMBDA` 0.25、`NLPROP` は ST4 なら 2.5e7、ST6 なら 3.0e7、ST2 なら 1.0e7、それ以外 2.78e7 | `:1884-1893` |
| `&SBT1` (BT1) | `GAMMA` −0.067 (NCEP・UFS の構成は −0.038) | `:2348` |
| `&SDB1` (DB1) | `BJALFA` 1、`BJGAM` 0.73、`BJFLAG` T | `:2387-2406` |
| `&REF1` (REF1) | `REFCOAST` 0 (既定では海岸で反射しない)、`REFRMAX` 1 | `:2876-2891` |
| `&PRO3` (PR3) | `CFLTM` 0.7、`WDTHCG` 1.5、`WDTHTH` (= WDTHCG) | `:2603-2712` |
| `&MISC` (常に読む) | `CICE0` = `CICEN` = 0.5、`FLAGTR` 0、`NOSW` 5 (NCO を含むビルドでは 3)、`PTM` 1、`CALTYPE` 'standard' | `:2749-2801` |

- `&SBT4` (BT4)・`&REF1` で地図を使うと、ww3_grid が `SED_NML`・`SLOPE_NML` の地図を読む ([grid.md](grid.md))。
- TR1 (三波相互作用) には namelist が無い (定数はソースに固定。`model/inp/ww3_grid.inp` の `STR1` の説明は「未実装」)。

### チューニング値の出典

- ST4 の既定値は TEST471 で、ECMWF の風で全球に最良とされる (`manual/eqs/ST4.tex:153-163`)。
  CFSR の風向けの組は表で T471f (`BETAMAX` 1.33)。本文では TEST451f と書かれている。
  T500 は水深による砕波を含み、浅い海向き。
- 名前の付いた組: `regtests/ww3_ts1/input/namelists_ST4_{T475,T500,T700,T702,T707}.nml`。
- 運用の例 (GFSv16 / UFS): `regtests/ww3_ufs1.1/input/namelists_glo_1deg.nml`
  (`&SIN4 BETAMAX = 1.315, TAUWSHELTER = 1.0, SWELLF = 0.798 …`、`&SNL1 NLPROP = 2.502E7`、`&SDS4 …`、
  `&MISC CICE0 = 0.25, CICEN = 0.75, FLAGTR = 4`、`&SBT1 GAMMA = -0.038`、`&PRO3 WDTHCG = 1.5, WDTHTH = 1.5`)。
  `FLAGTR = 4` なので障害物ファイルも要る。

## 海氷

- IC0 (氷の物理なし): 氷の密接度が臨界値を超えると、その点を陸のように扱う。
  - `&MISC CICE0 < CICEN` にすると、その間で連続的に遮る。ただし `FLAGTR` が 3 未満だと `CICE0 = CICEN` に戻される
    (`model/src/w3gridmd.F90:3081-3086`)。
  - 連続的に扱うなら `CICE0 = 0.25, CICEN = 0.75, FLAGTR = 4` (運用の例と同じ)。FLAGTR が 0 以外なので障害物ファイルが要る
    (障害物が無ければ 0 の場でよい)。
- IC1〜IC5 (氷による減衰): 氷のパラメータの強制場 (`ICE_PARAM1`〜`5`、[forcing.md](forcing.md)) を使う。
  どれを使うかは `ww3_grid.out` の `Sice will be calculated using …` に出る (`model/src/w3gridmd.F90:2437-2470`)。
- IS1 (散乱) は氷の密接度と `&SIS1` の係数 (`ISC1`・`ISC2`) だけで計算する (`model/src/w3sis1md.F90:185`、`model/src/w3gridmd.F90:1136`)。
  IS2 はテンプレートの `SIS2` の説明 (`model/inp/ww3_grid.inp:238` から) を読む。
- ww3_shel を `.inp` で書くなら、IC1〜IC5・IS2・BT8・BT9 のビルドでは氷と泥の行が増える ([shel-output.md](shel-output.md))。

## README・テンプレートの誤り

`model/bin/README.md` の namelist の例は、そのまま写すと動かないものがある:

| 場所 | 書かれている内容 | 実際 |
|---|---|---|
| `model/bin/README.md:203`、`:226` | `&PSMC DTIME = …, LATMIN = …` | `/PSMC/` に DTIME・LATMIN は無い (時間刻みは `DTIMS`)。ww3_grid が止まる |
| `model/bin/README.md:146` | `&SDS4 SDSC1 = 1.0, …` | `/SDS4/` に SDSC1 は無い。ww3_grid が止まる |
| `model/bin/README.md:113` | `&UG UGOBCAUTO = T, …` | group 名は `&UNST`。`&UG` は黙って無視される |
| `model/bin/README.md:97` | `&SIN4 BETAMAX = 1.33` | 終わりの `/` が無い |
| `model/inp/ww3_grid.inp:542`、`model/nml/namelists.nml:583` | `&FLX3 CDMAX = …` | FLX3 のビルドでしか読まれない。ST4 (FLX0) では黙って無視される |
