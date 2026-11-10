---
name: ww3-run-config
description: "WW3 のシミュレーションを新しく始めるとき・既存の設定を変えるときに読む、設定ファイルの一覧と書き方。地形 (水深・海陸マスク・境界点・障害物)、うねり (開境界のスペクトル)、初期条件、風・流れ・水位・海氷の強制場、物理パッケージ (switch) と物理係数 (namelist)、時間刻み・スペクトル格子、実行期間と出力 (格子・点・restart)、後処理を、どのファイルのどの項目で設定し、変えたらどのプログラムからやり直すかを扱う。次のようなときに使う: 新しい海域・期間で WW3 を回したい / regtest を元に runs/<case> を作りたい / 水深データ・風の NetCDF・境界スペクトルを差し替えたい / ww3_grid・ww3_strt・ww3_bounc・ww3_prnc・ww3_shel・ww3_ounf・ww3_ounp の .nml や .inp を書く・直す / 物理 namelist (&SIN4 など) や時間刻みを変えたい / 出力する変数や出力点を変えたい / restart から続けたい / テンプレートどおりに書いたのに止まる・結果が出ない。"
---

# ww3-run-config

WW3 の実行は、プログラムを順に動かし、決まった名前のファイルで結果を受け渡すパイプライン (LLM.md §4.1)。
1 つのシミュレーションを 1 つのケース `runs/<case>/` として持つ。設定ファイルは `input/` に置いて git で追跡し、
実行は `work/` (ignore) で行う。

## 読み込みの規則 (全プログラム共通)

- 各プログラムは作業ディレクトリの `<prog>.nml` を探し、あればそれだけを読む。無ければ `<prog>.inp` を読む
  (例: `model/src/w3gridmd.F90:1189-1202`)。
  - `.nml` を読めるもの: ww3_grid・ww3_prnc・ww3_bounc・ww3_bound・ww3_shel・ww3_ounf・ww3_ounp・ww3_outp・ww3_trnc・ww3_uprstr・ww3_multi
  - `.inp` だけのもの: ww3_strt・ww3_prep・ww3_outf
- プログラムは引数を取らない。データファイル (水深・座標・NetCDF の強制場やスペクトル) は、制御ファイルに書いたパスで開く。
  相対パスは作業ディレクトリが基準なので、`work/` からは `../input/<file>` と書く (regtest も同じ書き方)。
- 結果は決まった名前で作業ディレクトリにでき、次のプログラムが同じ名前で読む:
  `mod_def.ww3` (ww3_grid) → `restart.ww3` (ww3_strt)・`nest.ww3` (ww3_bounc / ww3_bound)・
  `wind.ww3` `current.ww3` `level.ww3` `ice.ww3` (ww3_prnc / ww3_prep) → `out_grd.ww3` `out_pnt.ww3`
  `restartNNN.ww3` `log.ww3` (ww3_shel) → NetCDF (ww3_ounf / ww3_ounp)。
- `.nml` を読んだプログラムは、読んだ値を `<prog>.nml.log` に書き出す。
- namelist の group は、次のものが必須 (無いと止まる)。それ以外は書き忘れても既定値のまま黙って進む。
  - ww3_grid: `SPECTRUM_NML`・`TIMESTEPS_NML`・`GRID_NML` (`model/src/w3nmlgridmd.F90:585`、`:799`、`:909`)
  - ww3_prnc: `FORCING_NML`・`FILE_NML`
  - ww3_ounf: `FIELD_NML`
  - ww3_ounp: `POINT_NML`

## 設定ファイルの一覧

| 設定したいもの | ファイル (`runs/<case>/input/`) | 読むプログラム | できるファイル | 詳細 |
|---|---|---|---|---|
| 物理パッケージ・伝播スキーム・並列化 (コンパイル時) | `switch_<case>` | CMake (`./dev build`) | `build/<case>/bin/` | [physics.md](references/physics.md) |
| スペクトル格子・時間刻み・格子の種類と範囲 | `ww3_grid.nml` (か `.inp`) | ww3_grid | `mod_def.ww3` | [grid.md](references/grid.md) |
| 地形: 水深・海陸マスクと境界点・障害物・座標 / メッシュ | `ww3_grid.nml` から参照するデータファイル | ww3_grid | `mod_def.ww3` | [grid.md](references/grid.md) |
| 物理係数 (`&SIN4`・`&SDS4`・`&MISC` …) | `ww3_grid.nml` の `GRID%NML` が指すファイル (`.inp` なら本文中) | ww3_grid | `mod_def.ww3` | [physics.md](references/physics.md) |
| 初期条件 | `ww3_strt.inp` (か、前回の restart) | ww3_strt | `restart.ww3` | [boundary.md](references/boundary.md) |
| うねり (開境界のスペクトル) | `ww3_bounc.nml` + `spec.list` + NetCDF スペクトル (か `ww3_bound` + ASCII) | ww3_bounc / ww3_bound | `nest.ww3` | [boundary.md](references/boundary.md) |
| 風・流れ・水位・海氷 | `ww3_prnc_<field>.nml` + NetCDF (か `ww3_prep_<field>.inp` + ASCII) | ww3_prnc / ww3_prep | `wind.ww3` など | [forcing.md](references/forcing.md) |
| 実行期間・使う強制場・出力 (格子・点・restart) | `ww3_shel.nml` + `points.list` | ww3_shel | `out_grd.ww3`・`out_pnt.ww3`・`restartNNN.ww3` | [shel-output.md](references/shel-output.md) |
| 後処理 (NetCDF への変換) | `ww3_ounf.nml`・`ww3_ounp.nml` (種類ごとに `ww3_ounp_<name>.nml` も可) | ww3_ounf / ww3_ounp | `ww3.*.nc` | [shel-output.md](references/shel-output.md) |

テンプレートにする regtest の選び方と、`run_cmake_test` が裏でしていることは [templates.md](references/templates.md)。

## 新しくシミュレーションを始める

1. **テンプレートを選ぶ**: [templates.md](references/templates.md) から近い regtest を選び、`regtests/<case>/info` を読む。
2. **ケースを作る**:

   ```
   runs/<case>/
     README.md   目的、データの取得元と作り方、実行の手順
     run.sh      実行のスクリプト (下の手順 5)
     input/      git で追跡: switch_<case>、*.nml / *.inp、points.list、spec.list、地形データ
     work/       ignore (runs/*/work*/): 実行と出力。run.sh が毎回作り直す
   ```

   - テンプレートの入力を `input/` にコピーし、データは `../input/<file>` で参照する。
   - `input/` に置く制御ファイルは、ww3_grid・ww3_strt・ww3_bounc・ww3_shel などは 1 つずつ。ww3_prnc・ww3_prep・ww3_ounf・ww3_ounp は
     種類ごとに `<prog>_<name>.nml` (`.inp`) と複数置ける。テンプレートの別の版 (`ww3_grid_36dirs.nml` など) はコピーしない。
   - `*.nc` は .gitignore で除外されるので、NetCDF の強制場やスペクトルは commit されない。取得元と作り方を `README.md` に書く。
3. **switch を決めてビルドする**:

   ```bash
   cp model/bin/switch_NCEP_st4 runs/<case>/input/switch_<case>   # preset かテンプレートの switch を元にする
   ./dev build runs/<case>/input/switch_<case>                     # → build/<case>/bin/
   ```

   - ファイル名を `switch` のままにすると、全ケースが `build/switch` を共有し、ケースを替えるたびに全体が再コンパイルされる。
   - `<case>` は `model/bin` の preset 名 (`NCEP_st4` など) と重ねない (`build/` の中で衝突する)。
4. **入力を書く**: 一覧の順に、各 references の表を見ながら書く。テンプレート (`model/inp`・`model/nml`) のコメントには
   誤りがあるので、各 references の「テンプレートの誤り」を先に見る。
5. **実行する**: 次を `runs/<case>/run.sh` として保存し (`<case>` と `MPI` を書き換える)、`./dev docker run -- bash runs/<case>/run.sh` で
   実行する (検証済みの手順)。対話シェルに貼り付けない (`set -e` で失敗したときにシェルごと終わる)。

   ```bash
   #!/bin/bash
   # 実行: ./dev docker run -- bash runs/<case>/run.sh
   # restart から続ける: ./dev docker run -- env WORK=work2 RESTART=work/restart001.ww3 bash runs/<case>/run.sh
   set -e
   CASE=<case>
   MPI="mpirun -np 8"            # switch が SHRD なら MPI=""
   WORK=${WORK:-work}            # 作業ディレクトリ (runs/*/work*/ は ignore)。毎回作り直す
   RESTART=${RESTART:-}          # 続けるときの restart (runs/<case>/ からのパス)。WORK の外に置く
   B=/workspace/build/$CASE/bin
   cd /workspace/runs/$CASE
   case "$RESTART" in "$WORK"/*) echo "RESTART must be outside $WORK" >&2; exit 1 ;; esac
   rm -rf "$WORK" && mkdir "$WORK" && cd "$WORK"

   # exe <prog> <入力> [launcher ...]: 入力を <prog>.nml|inp にリンクして実行し、<入力の名前>.out を検査する
   exe() {
     local p=$1 f=$2 ext=${2##*.} o; shift 2
     o=$(basename "$f" ".$ext").out
     rm -f "$p.nml" "$p.inp" && ln -s "$f" "$p.$ext"
     "$@" "$B/$p" > "$o"
     if grep -q "ERROR" "$o"; then echo "ERROR in $WORK/$o" >&2; exit 1; fi
   }
   # run <prog> [launcher ...]: input/<prog>.nml (無ければ .inp) があれば実行する
   run() {
     local p=$1 f; shift
     for f in "../input/$p.nml" "../input/$p.inp"; do
       if [ -e "$f" ]; then exe "$p" "$f" "$@"; return; fi
     done
   }
   # runeach <prog>: input/<prog>*.nml (無ければ *.inp) を 1 つずつ実行する
   runeach() {
     local p=$1 ext f
     for ext in nml inp; do
       compgen -G "../input/$p*.$ext" > /dev/null || continue
       for f in ../input/$p*.$ext; do exe "$p" "$f"; done
       return
     done
   }

   run ww3_grid
   if [ -n "$RESTART" ]; then cp "../$RESTART" restart.ww3; else run ww3_strt; fi
   run ww3_bounc                 # うねり: NetCDF のスペクトル
   run ww3_bound                 # うねり: ASCII のスペクトル
   runeach ww3_prep              # 強制場: ASCII (種類ごとに ww3_prep_<field>.inp)
   runeach ww3_prnc              # 強制場: NetCDF (種類ごとに ww3_prnc_<field>.nml)
   run ww3_shel $MPI
   runeach ww3_ounf
   runeach ww3_ounp
   echo "finished: $(ls *.out | tr '\n' ' ')"
   ```

   - MPI で並列に動かすのは ww3_shel だけでよい。switch が DIST/MPI でも、前後処理のプログラムはそのまま 1 プロセスで動く。
     switch が SHRD なら `MPI=""` にする (`mpirun` を付けると同じ計算が N 個走り、同じファイルに書き込む)。
   - 失敗の検出は 2 段にしている。
     - エラーで止まったプログラムは 0 以外の終了コードを返すので、`set -e` で止まる (`EXTCDE` → `EXIT`、`model/src/w3servmd.F90`)。
     - 例外がある: ww3_shel は強制場のファイルが無い・合わないとき、`*** WAVEWATCH III ERROR IN W3FLDO` を出したうえで
       `End of program` を出し、終了コード 0 で終わる (`model/src/ww3_shel.F90:1732-1736` → `FINALISE`。検証で確認)。
       そのため、`.out` に `ERROR` があれば止める。`End of program` の有無だけでは判定できない。
     - `mpirun` の下で止まると、原因の文が残らず `EXTCDE MPI_ABORT, IEXIT= <N>` だけになることがある。そのときは同じ作業ディレクトリで
       ww3_shel を `mpirun` なしで動かすと文が見える (検証で確認)。`grep -n 'EXTCDE ( <N> )' model/src/*.F90` で止まった場所も探せる。
   - 標準エラーに出る `yaksa: ... leaked handle pool objects` (MPICH) と
     `Note: The following floating-point exceptions are signalling` (gfortran) は無害。
6. **確かめる**:
   - `ww3_grid.out`: 物理パッケージごとの `(user def. values)` (namelist が読まれた) / `(default values)`、警告、海点の数。
   - `log.ww3` の時間ステップ表: 入力欄 (`b` 境界・`w` 風・`l` 水位・`c` 流れ・`i` 海氷 …) に `F` (最初の読み込み)・`X` (読み込み)・
     `L` (ファイルの終わり) が付く。強制場や `nest.ww3` が読まれているかはここで分かる。
   - ww3_ounf / ww3_ounp の NetCDF (`ncdump -h`)。

## 設定を変えるとき

| 変えたもの | やり直す範囲 |
|---|---|
| switch (物理パッケージ・伝播・出力 switch) | `./dev build` → ww3_grid から全部。全プログラムを同じビルドで動かす (mod_def に書かれた物理パッケージ名を照合し、違うと `MOD DEF FILE WAS GENERATED WITH A DIFFERENT WW3 VERSION OR USING A DIFFERENT SWITCH FILE` で止まる。`model/src/w3iogrmd.F90:687-750`) |
| 物理係数・時間刻み・RUN フラグ | ww3_grid → ww3_shel → 後処理。restart・強制場・`nest.ww3` はそのまま使える |
| スペクトル格子 (NK・NTH・XFR・FREQ1) | ww3_grid → ww3_strt → ww3_shel → 後処理。`nest.ww3` は読むときに変換されるので作り直さなくてよい (`model/src/w3iobcmd.F90:458`)。ただし ww3_bound を作り直すなら、ASCII の入力の NTH・XFR がモデルと一致している必要がある |
| 水深・ZLIM・DMIN・マスク・障害物 | ww3_grid → ww3_strt (海点が変わるなら) → ww3_bounc / ww3_bound (境界点が変わるなら) → ww3_shel → 後処理 |
| 格子の範囲・解像度・座標 | 全部 (ww3_prnc / ww3_prep も) |
| 風・流れなどの元データ | ww3_prnc / ww3_prep → ww3_shel → 後処理 |
| 境界スペクトルの元データ | ww3_bounc / ww3_bound → ww3_shel → 後処理 |
| 実行期間・出力の指定 | ww3_shel → 後処理 |
| 後処理の指定 | ww3_ounf / ww3_ounp だけ |

`run.sh` は毎回すべてを作り直すので、やり直し漏れは起きない (時間が問題になるときだけ、上の表で範囲を絞る)。
自分で一部だけ実行するときは、読み込み時の照合が緩く、やり直し漏れが黙って通ることに注意する:

- restart は海点数 (NSEA) とスペクトルの大きさ (NSPEC) しか照合しない (`model/src/w3iorsmd.F90:553-577`)。
  NK・NTH を変えずに XFR・FREQ1 を変えると、古い restart が通って中身を読み違える。
- 強制場の `.ww3` は種類と NX・NY しか照合しない (`model/src/w3fldsmd.F90:428-455`)。格子を動かしても NX・NY が同じなら、
  古い強制場が通る。

restart から続けるときは、`run.sh` に `RESTART` を渡す (ww3_strt の代わりに restart を `restart.ww3` にコピーする)。
ww3_shel の `DOMAIN%START` を restart の時刻に合わせる ([boundary.md](references/boundary.md))。

## 共通の約束事

- 時刻は `'yyyymmdd hhmmss'` (UTC)、間隔は秒。入出力の間隔を DTMAX の整数倍にすると結果の再現性が保たれる。
- 方向の規約 (モデルの内部は Cartesian: 東向きが 0°、反時計回り):

  | 入力・出力 | 規約 |
  |---|---|
  | ww3_strt の平均方向、ww3_shel の一様な風・流れ | 来る方向、北から時計回り |
  | NetCDF のスペクトル (ww3_bounc が読む・ww3_ounp が書く) | 向かう方向、北から時計回り |
  | NetCDF の風・流れ (ww3_prnc) | 東向き・北向きの成分 (u, v) |

- 水深: 格納値 = 倍率 × ファイルの値で、海面下が負。正の水深のファイルには `DEPTH%SF = -1.` などの負の倍率を掛ける。
  格納値が ZLIM 以下の点が海になる ([grid.md](references/grid.md))。
- namelist の落とし穴 ([physics.md](references/physics.md)):
  - コンパイルしていない switch の group は黙って無視される。同じ group が 2 つあると最初のものだけが使われる。
  - group の中の変数名を誤ると ww3_grid が止まる。
  - `.inp` の namelist 部は 1 行 80 桁で切れ、`END OF NAMELISTS` は 1 桁目から書く。長い group は複数行に分ける。
- ファイル名の長さ: ww3_grid が読むデータファイルのパスは、`.inp`・`.nml` のどちらでも 60 文字で切れる
  (`FNAME*60`、`model/src/w3gridmd.F90:787`)。ww3_prnc の `FILE%FILENAME` は 80 文字 (`model/src/ww3_prnc.F90:315`)。
  `../input/` を含めた長さで数える。
- `.ww3` ファイルは既定で big endian (`-DENDIAN`)。別のビルドで作った `.ww3` を読むときはそろえる。

## 使わない場面

- Fortran ソースを変える (物理項の追加など) → LLM.md §4・§6
- regtest を実行・比較する → LLM.md §5 (`./dev regtest`)
- 開発イメージ・コンテナの環境 → README.md

## このスキルの保守

このスキルは使いながら直していく。

- 使っていて**不足** (載っていない設定・手順・ファイル) や**誤り** (書いたとおりに動かない、ソースと食い違う) に気づいたら、
  その作業の中で SKILL.md と references を直す。
- 書く前に確かめる。根拠はソース・テンプレートの `file:line` か、実際に動かした結果にする。
  確かめていないことは書かないか、「未確認」と明記する。
- 新しいデータソース (再解析の風・水深データ・外部モデルの境界スペクトルなど) を使ったら、その準備手順と落とし穴を
  references に追記する。
- 直したことは、その作業の報告でユーザーに伝える。LLM.md と食い違ったら両方を直す。
