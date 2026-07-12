# 运行与开发

## 环境

- Haskell: GHC 9.10.3
- Stack resolver: `lts-24.19`
- `stack.yaml` 当前启用 `allow-newer: true`，用于兼容 GHC 9.10.3 下的依赖版本。
- Server 默认监听 `127.0.0.1:9160`。

## 常用命令

一条命令启动本地测试环境：

```bash
make dev-test
```

它会在需要时先执行 server build / client dependency install，然后启动：

- `MUD_DEV_MODE=1 stack exec mud-hs-exe`
- Vite/Svelte client dev server on `http://127.0.0.1:8080`
- 测试入口 `http://127.0.0.1:8080/?test=1&user=tester`

停止时按 `Ctrl-C`，脚本会同时清理 server 和 client 两个子进程。

可用参数：

```bash
TEST_USER=story1 make dev-test
TEST_RESET=0 make dev-test
OPEN_BROWSER=0 make dev-test
SKIP_BUILD=1 make dev-test
```

也可以直接调用脚本：

```bash
scripts/dev-test.sh --user story1 --no-reset --no-open
```

手动命令：

```bash
stack build
stack test
stack exec mud-hs-exe
```

启动 server：

```bash
stack exec mud-hs-exe
```

测试专用 server：

```bash
MUD_DEV_MODE=1 stack exec mud-hs-exe
```

`MUD_DEV_MODE=1` 只用于本地开发。它允许测试入口在登录时重置同名玩家存档，避免每次换新登录名。

打开 client：

```bash
cd client
npm install
npm run dev
```

然后访问 `http://127.0.0.1:8080`。客户端现在是 Vite/Svelte 应用，不能再通过直接打开 `client/index.html` 或普通静态 HTTP server 获得开发体验。

测试专用入口：

```text
http://127.0.0.1:8080/?test=1
```

这个入口会自动用 `tester` 登录，不需要点登录按钮；默认会请求 server 重置 `tester` 的存档并从完整新流程开始。server 必须用 `MUD_DEV_MODE=1` 启动，否则会拒绝重置请求。

可用参数：

- `user=<name>`：指定测试用户名，例如 `?test=1&user=story1`。
- `reset=0`：自动登录但不重置存档，例如 `?test=1&reset=0`。

## 测试

当前测试入口是 [test/Spec.hs](../test/Spec.hs)，覆盖内容包括：

- YAML 世界加载。
- 世界引用校验。
- 移动更新房间内玩家集合。
- 不允许跨房间攻击 NPC。
- 玩家战败不会杀死 NPC。
- 主动招式失败原因、AP/Qi 消耗、战斗快照。
- DoT 效果 tick。
- 威远镖局旧案剧情完整流程：黑衣人显现、玩家独立剧情战斗、回头找老镖师、沿官道护送少年、前厅分离。
- 玩家存档 round trip：剧情状态、位置、房间占位、金钱、背包、已学武功、已准备武功。

本机如果没有 `ghcup`，`stack test` 可能先打印：

```text
ghcup: command not found
Installing 9.10.3 via ghcup failed
Warning: GHC install hook exited with code: 3.
```

只要后续继续用已安装的 GHC 构建并最终显示 `All tests passed` / `Test suite mud-hs-test passed`，测试就是通过的。

## 保存目录

玩家存档写入 [saves/](../saves)，文件名来自 sanitize 后的玩家 id。测试存档写入 `.stack-work/test-saves`。
