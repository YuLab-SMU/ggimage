# ggimage 仓库协作说明

## 修改范围与验证

- 本仓库是 R 包。改动 `R/` 中代码时，优先为行为边界添加或更新 `tests/testthat/` 回归测试。
- 修改 roxygen 标签后运行 `roxygen2::roxygenise()`，检查生成的 `NAMESPACE` 与 `man/` 是否符合预期；不要只手改生成文件。
- 安装并测试当前 checkout 时，显式把临时库放在搜索路径最前，避免误加载用户库中的旧版本：

  ```r
  .libPaths(c("/tmp/ggimage-lib", .libPaths()))
  library(ggimage)
  ```

  安装命令：`R CMD INSTALL --library=/tmp/ggimage-lib .`
- 先运行受影响的测试文件；改动涉及公共图层、图像缓存、图例或坐标尺寸时，补跑相关 `testthat` 回归测试。
- 运行 `git diff --check` 检查空白错误。尝试 `R CMD check` 时，注意当前 checkout 的 DESCRIPTION 元数据可能导致 Author/Maintainer 前置检查失败；应区分仓库元数据问题与代码检查结果，不要把前置失败说成代码问题。

## 性能优化

- 性能结论必须来自可复现、同条件的基准。区分微基准（单个转换/函数）与端到端 `ggplotGrob()` 耗时；微基准不能直接宣称整图加速。
- 对图像/栅格化路径，明确测量重复图片与唯一图片、缓存开关、GIF 帧数及图例/主图场景。不要把单张静态图结果外推到动画或用户未提供的实际素材。
- 优先采用局部、生命周期明确的复用（例如单次 panel/key 绘制内），避免无效期不明的进程级缓存。若缓存跨调用，必须说明文件变化、变换参数、动画帧和自定义回调的失效语义。
- `grid` grob 通常承载各自的位置、尺寸和绘制属性。可复用图像像素或 raster 数据，不要在未证明语义等价前合并/共享逐行 grob。
- 对 `geom_image_repel()`，稀疏与密集场景复杂度不同。不要为了速度静默降低 `max.iter`、改变 `force` 或改变配对顺序；这会影响布局结果。先基准目标分布并检查现有自适应 broad-phase/保守回退。
- 图像裁边（如 `image_read2(cut_empty_space = TRUE)`）会改变像素边界/图像尺寸；关闭或缓存不能视为无行为差异。优先由调用方避免重复读取，只有明确缓存键与失效策略后才考虑跨调用缓存。

## R 命名空间

- 对非 base 函数使用显式 `pkg::fun()`，或通过 roxygen `@importFrom pkg fun` 声明依赖并重新生成 `NAMESPACE`。
- `R CMD check` 的 “no visible global function definition” NOTE 应通过限定命名空间或正确的 roxygen 导入修复；不要忽略，也不要只在 `NAMESPACE` 中手工加行。
- `grDevices::as.raster()` 若在多个 R 文件中使用，在一个合适的 roxygen 块声明 `@importFrom grDevices as.raster`，再运行 `roxygen2::roxygenise()` 更新 `NAMESPACE`。`as.raster` 是 S3 泛型，应由 grDevices 导入以供 Magick 等类的方法分派；不要仅因测试或交互环境可见就认为包命名空间 NOTE 已解决。

## 基准脚本

- `bench/` 下的性能脚本应使用本地素材，避免网络和外部服务影响结果；记录运行环境、样本数、重复次数和基准范围。
- 修改基准脚本时，确保它测量的场景与脚本注释一致，并区分“多次独立调用”与“单次批量调用”。
