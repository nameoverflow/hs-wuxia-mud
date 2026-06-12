# 运行与开发

## 环境

- Haskell: GHC 9.10.3
- Stack resolver: `lts-24.19`
- `stack.yaml` 当前启用 `allow-newer: true`，用于兼容 GHC 9.10.3 下的依赖版本。
- Server 默认监听 `127.0.0.1:9160`。

## 常用命令

```bash
stack build
stack test
stack exec mud-hs-exe
```

启动 server：

```bash
stack exec mud-hs-exe
```

打开 client：

```bash
cd client
python3 -m http.server 8080
```

然后访问 `http://localhost:8080`。直接打开 `client/index.html` 通常也能工作，但用本地 HTTP server 更接近浏览器正常加载资源的路径。

## 测试

当前测试入口是 [test/Spec.hs](../test/Spec.hs)，覆盖内容包括：

- YAML 世界加载。
- 世界引用校验。
- 移动更新房间内玩家集合。
- 不允许跨房间攻击 NPC。
- 玩家战败不会杀死 NPC。
- 技能失败原因、AP/Qi 消耗、战斗快照。
- DoT 效果 tick。
- 冷雨客栈剧情完整流程：剧情选择、杀死纸伞客、掉落、隐藏剧情 NPC、青衣客结尾、秘籍使用后学习武功。
- 玩家存档 round trip：剧情状态、金钱、背包、已学武功、已准备武功。

本机如果没有 `ghcup`，`stack test` 可能先打印：

```text
ghcup: command not found
Installing 9.10.3 via ghcup failed
Warning: GHC install hook exited with code: 3.
```

只要后续继续用已安装的 GHC 构建并最终显示 `All tests passed` / `Test suite mud-hs-test passed`，测试就是通过的。

## 保存目录

玩家存档写入 [saves/](../saves)，文件名来自 sanitize 后的玩家 id。测试存档写入 `.stack-work/test-saves`。
