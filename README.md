# sim-WW3

[NOAA-EMC/WW3](https://github.com/NOAA-EMC/WW3) (WAVEWATCH III® v7.14) の fork。波作用密度スペクトルの平衡方程式を解く
Fortran の波浪モデルで、物理・数値スキーム・並列化・I/O はビルド時の switch ファイルで選ぶ。
コードの構成・switch・回帰テストの詳しい説明は [`LLM.md`](./LLM.md) を参照。

ビルド・実行・回帰テストは Docker コンテナ内で行う前提。コンテナには CI と同じ依存ライブラリ (Spack 環境と OASIS3-MCT) が入っている。

---

## 前提条件

- Linux / WSL2 (Ubuntu 等)、AVX2 対応の x86_64 CPU (Spack 環境の target が `x86_64_v3` のため)
- Docker Engine 24+ と Docker Compose v2 (`docker compose` サブコマンドが使えること) — インストール手順は下記
- 外部ネットワーク (イメージのビルドで GitHub と各ライブラリの配布元からソースを取得する。外部データが要る回帰テストでは NOAA の ftp からデータを取得する)
- ホスト側 Python 3.8+ (`./dev` は `tools/dev.py` への symlink で、ホストの `python3` で起動する)
- 初回のイメージビルドには時間がかかる。Spack が MPICH・HDF5・NetCDF・ESMF などをソースからビルドするためで、
  32 コアのマシンでもこの部分に約 30 分かかる (全体の時間はダウンロードの速さで変わる)。イメージは約 2 GB

ホストの UID/GID をコンテナ内 `dev` ユーザに合わせて作成するので、bind mount したファイルはホスト側でもユーザ所有のまま編集できる。

---

## Docker のインストール (Ubuntu / WSL2)

Ubuntu の `docker.io` パッケージは Compose v2 プラグインが同梱されない版があるため、
Docker 公式 APT リポジトリから入れる。

```bash
# 0. 古い非公式パッケージが入っていれば削除 (初回のみ / 未導入なら no-op)
for p in docker.io docker-doc docker-compose docker-compose-v2 podman-docker containerd runc; do
    sudo apt remove -y "$p"
done

# 1. 公式 GPG 鍵を登録
sudo apt update
sudo apt install -y ca-certificates curl gnupg
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# 2. APT リポジトリを登録
echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] \
https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}") stable" \
| sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# 3. Engine + CLI + Buildx + Compose v2 を導入
#    ★ リポジトリ登録後の apt update は必須 (省くと docker-ce が見つからない)
sudo apt update
sudo apt install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
```

> `E: Package 'docker-ce' has no installation candidate` が出たら、直前の `sudo apt update` が
> 抜けている。`apt update` の出力に `https://download.docker.com/linux/ubuntu <codename> InRelease`
> の行が現れることを確認してから install し直す。

### 非 root ユーザで docker を使えるようにする (必須)

`./dev` は `sudo` を付けずに `docker compose` を呼ぶ。`docker` グループに自分を追加する。

```bash
sudo usermod -aG docker "$USER"
newgrp docker          # もしくは一度ログアウト/ログイン
```

> `sudo docker` で代用しない。root が作った成果物が bind mount 経由でホスト側に root 所有で
> 残り、`compose.yaml` の `HOST_UID`/`HOST_GID` 整合が壊れる。

### デーモンの起動確認

```bash
# 通常の Linux (systemd)
sudo systemctl enable --now docker

# WSL2 で systemd を使っていない場合
sudo service docker start
```

---

## クイックスタート

```bash
# 1. 開発イメージをビルド (要 Docker。未導入なら上記「Docker のインストール」を先に実施)
./dev docker build

# 2. WW3 をビルド (switch は model/bin/switch_<name>。実行ファイルは build/<name>/bin/)
./dev build NCEP_st4

# 3. 回帰テストを実行 (出力は regtests/<case>/<work>/。合否判定は無いので、変更前後の出力を比べて判断する)
./dev regtest -s PR1 -w work_PR1 ww3_tp1.1                               # 逐次
./dev regtest -s PR1_MPI -w work_PR1_MPI -f -p mpirun -n 4 ww3_tp2.1     # MPI

# 4. 外部データが要るケース (ww3_tp2.8 など。LLM.md §5.1) は、先に NOAA の ftp からデータを取得する
./dev docker run -- model/bin/ww3_from_ftp.sh
```

## `./dev` コマンド一覧

**Docker (ホスト側)**

| コマンド | 動作 |
|---|---|
| `./dev docker build [--no-cache ...]` | `docker compose build` を呼び、`HOST_UID`/`HOST_GID` をビルド引数として渡す。追加引数は `docker compose build` にそのまま透過。 |
| `./dev docker shell` | `dev` サービスで `bash` を起動 (`docker compose run --rm dev bash`)。 |
| `./dev docker run -- <cmd ...>` | コンテナ内で任意コマンドを実行。例: `./dev docker run -- mpif90 --version` |
| `./dev docker clean` | dev イメージを削除 (確認プロンプトあり)。BuildKit のビルドキャッシュは残る。 |

**ビルド・回帰テスト (コンテナ内)**

| コマンド | 動作 |
|---|---|
| `./dev build [--type Release\|Debug\|RelWithDebInfo\|MinSizeRel] <switch> [-- <cmake 引数>]` | CMake で configure + ビルド。`<switch>` は `model/bin` の名前 (`NCEP_st4` = `model/bin/switch_NCEP_st4`) か、repo 内の switch ファイルのパス。build dir は `build/<name>` (Release、既定) か `build/<name>-<type>` で、実行ファイルはその `bin/`。引数なしで switch 一覧。 |
| `./dev regtest [run_cmake_test のオプション] <case>` | `cd regtests && ./bin/run_cmake_test <オプション> ../model <case>` を実行。`-h` で run_cmake_test のオプション一覧、引数なしでケース一覧。 |
| `./dev tools <name> [引数]` | `tools/<group>/<name>.py` (描画の `tools/viz`、前処理の `tools/prep`) をコンテナ内の `python3` で実行。パスは repo root から。`<name> --help` でオプション、引数なしでツール一覧。 |
| `./dev help` | ヘルプ表示。 |

シミュレーションのケースは `runs/<case>/` に置き、仕様書 `SPEC.md`・`run.sh`・`postprocess.sh` で動かす ([`runs/README.md`](./runs/README.md))。
例 (`runs/sample-jonswap`):

```bash
./dev build runs/sample-jonswap/input/switch_sample-jonswap     # WW3 をビルド (build/sample-jonswap/)
./dev docker run -- bash runs/sample-jonswap/run.sh             # 実行 (約 5 分)
./dev docker run -- bash runs/sample-jonswap/postprocess.sh     # 図 (runs/sample-jonswap/work/figs/)
```

`./dev tools` やポスト処理が `ModuleNotFoundError: No module named 'numpy'` などで止まるときは、イメージが描画用の Python
(`/opt/venv`) を足す前のもの。`./dev docker build` で作り直す (Spack の層はキャッシュが効くので数分)。

`HOST_UID` / `HOST_GID` は通常 `id -u` / `id -g` から自動取得されるが、必要なら環境変数で上書き可。
CMake への追加オプションは `--` の後に渡す。例: ESMF 用ライブラリは `./dev build multi_esmf -- -DMULTI_ESMF=ON`、
NetCDF を使わないなら `-- -DNETCDF=OFF`。

---

## 開発コンテナ

ベースは `ubuntu:24.04` (`TZ=Asia/Tokyo` / `LANG=C.UTF-8`)。定義は [`docker/Dockerfile`](./docker/Dockerfile)、
非 root の `dev` ユーザ (ホスト UID/GID に整合) で動く。

- **コンパイラ**: GCC 13 (gcc / g++ / gfortran。Ubuntu 24.04 標準)
- **依存ライブラリ**: CI (`.github/workflows/gnu.yml`) と同じ手順で、JCSDA の Spack (`spack-stack-1.9.3`) を使って
  [`model/ci/spack_gnu.yaml`](./model/ci/spack_gnu.yaml) と `mpich@3.4.2` を Spack 環境 `ww3-gnu` に入れる
  (NetCDF-C/Fortran、HDF5、METIS/ParMETIS、SCOTCH、g2/bacio/w3emc、PIO、ESMF)。Spack は `/opt/spack`
- **OASIS3-MCT**: `regtests/ww3_tp2.14/input/oasis3-mct` を CI と同じ手順でビルドし、`/opt/oasis3-mct` に置く
- **ツール**: CMake (Ubuntu の apt 版を Spack の external として使う)、gdb、valgrind、doxygen
- **Python (ポスト処理)**: `/opt/venv` (Ubuntu の python3 3.12) に numpy・matplotlib・netCDF4。`./dev tools` が使う

これで CI の build matrix にある全構成 (`Ifremer1` `NCEP_st2` `NCEP_st4` `ite_pdlib` `NCEP_st4sbs` `NCEP_glwu` `OASACM` `UKMO`
と `MULTI_ESMF`) をビルドできる。

コンテナ内のコマンドは、[`docker/entrypoint.sh`](./docker/entrypoint.sh) が `/opt/ww3-env.sh` を読み込んでから実行する。
主な環境変数:

| 変数 | 値 | 用途 |
|---|---|---|
| `PATH`、`CMAKE_PREFIX_PATH`、`ESMFMKFILE` など | Spack 環境 `ww3-gnu` の view | `spack env activate ww3-gnu` と同じ。ただし `LD_LIBRARY_PATH` は設定しない (下記) |
| `CC` / `FC` | `mpicc` / `mpif90` | CI と同じく MPI の wrapper でビルドする |
| `OASISDIR` | `/opt/oasis3-mct` | switch に `OASIS` を含む構成 (`OASACM` など) |
| `CMAKE_OPTIONS` | `-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON` | `./dev build` と `run_cmake_test` が cmake に渡す。install した実行ファイルにも Spack のライブラリへの RPATH を残す |

view には Spack 版の OpenSSL・zlib・ncurses なども入っているので、`LD_LIBRARY_PATH` に view を足すと git などのシステムのツールまで
それを読み込む。そこでコンテナでは `LD_LIBRARY_PATH` を設定せず、実行ファイルは RPATH でライブラリを見つける。
コンテナ内で cmake を直接呼んで install するときは `$CMAKE_OPTIONS` を付ける。
`PATH` の先頭は `/opt/venv/bin`、その次が view の `bin` なので、`python3` はポスト処理用の venv、`tar` などは Spack 版が使われる。

[`compose.yaml`](./compose.yaml) (service `dev`) の要点:

- リポジトリ root を `/workspace` に bind mount
- `user` / build args の `UID` `GID` はホストの `HOST_UID` / `HOST_GID` (既定 `id -u` / `id -g`) に整合
- `init: true`、`tty`/`stdin_open` 有効
- `shm_size: 2gb` (MPICH のノード内通信用)、スタックサイズ無制限 (`ulimit -s unlimited` 相当)

`model/ci/spack_gnu.yaml` を変えると、次の `./dev docker build` は Spack 環境から作り直しになり、初回と同じくらい時間がかかる。
ツールの追加やホストの UID の違いでは Spack の層は作り直されない (Dockerfile は Spack の層を先に置いている)。

---

## ドキュメント

- [`LLM.md`](./LLM.md) (= `CLAUDE.md` / `AGENTS.md`) — リポジトリガイドの一次情報源 (ビルド・switch・アーキテクチャ・回帰テスト・コード規約)
- [`model/README.md`](./model/README.md) — CMake ビルドの詳細 (ライブラリ探索・オプション・FAQ)
- [`model/bin/README.md`](./model/bin/README.md) — 各センターの switch と namelist の設定例
- [`manual/`](./manual) — ユーザーマニュアル (LaTeX。支配方程式・数値解法・入力仕様)
- `regtests/<case>/info` — 各回帰テストの目的・必要な switch・実行例
- upstream の [WW3 Wiki](https://github.com/NOAA-EMC/WW3/wiki) と [Doxygen](https://noaa-emc.github.io/WW3)

## ライセンス

[GNU LGPL v3](./LICENSE.md)。WAVEWATCH III® は National Weather Service の商標。
