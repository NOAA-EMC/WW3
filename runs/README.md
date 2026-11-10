# runs/

WW3 のシミュレーションのケースを置く。1 つのケースを 1 つのディレクトリにまとめる。

```
runs/<case>/
  README.md   目的、データの取得元と作り方、実行の手順
  run.sh      実行のスクリプト (./dev docker run -- bash runs/<case>/run.sh)
  input/      git で追跡する: switch_<case>、各プログラムの .nml / .inp、points.list、地形データなど
  work/       git で追跡しない (.gitignore の runs/*/work*/): 実行と出力。run.sh が毎回作り直す
```

- 制御ファイルからデータは `../input/<file>` で参照する (プログラムは `work/` で実行する)。
- `*.nc` は .gitignore で除外されるので、NetCDF の強制場や境界スペクトルは commit されない。取得元と作り方を各ケースの `README.md` に書く。
- switch は `input/switch_<case>` という名前にして `./dev build runs/<case>/input/switch_<case>` でビルドする (実行ファイルは `build/<case>/bin/`)。

設定ファイルの一覧・書き方・実行の手順は skill `ww3-run-config` (`.claude/skills/ww3-run-config/SKILL.md`) を参照。
