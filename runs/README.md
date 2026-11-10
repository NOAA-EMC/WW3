# runs/

WW3 のシミュレーションのケースを置く。1 つのケースを 1 つのディレクトリにまとめる。

```
runs/<case>/
  SPEC.md          仕様書 (目的・設定・ポスト処理・検証)。ケースの一次の出典
  run.sh           実行のスクリプト (./dev docker run -- bash runs/<case>/run.sh)
  postprocess.sh   ポスト処理 (図) のスクリプト (./dev docker run -- bash runs/<case>/postprocess.sh)
  input/           git で追跡する: switch_<case>、各プログラムの .nml / .inp、points.list、地形データなど
  work/            git で追跡しない (.gitignore の runs/*/work*/): 実行と出力。run.sh が毎回作り直す。図は work/figs/
```

- 制御ファイルからデータは `../input/<file>` で参照する (プログラムは `work/` で実行する)。
- `*.nc` は .gitignore で除外されるので、NetCDF の強制場や境界スペクトルは commit されない。取得元と作り方を `SPEC.md` に書く。
- switch は `input/switch_<case>` という名前にして `./dev build runs/<case>/input/switch_<case>` でビルドする (実行ファイルは `build/<case>/bin/`)。

記入例は `sample-jonswap/` (100 km 四方、JONSWAP のうねりと一様な風)。

| 知りたいこと | skill |
|---|---|
| 仕様書の書き方 (テンプレート)、進め方、レビュー | `ww3-sim-spec` (`.claude/skills/ww3-sim-spec/`) |
| 設定ファイルの一覧・書き方・実行の手順 | `ww3-run-config` (`.claude/skills/ww3-run-config/`) |
| NetCDF への変換と図 (postprocess.sh) | `ww3-postprocess` (`.claude/skills/ww3-postprocess/`) |
