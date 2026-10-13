## ggimage 0.3.7

+ `geom_bgimage()` accepts `pixel`, `x`, `y` and `scale` to place the image in data coordinates: the chosen pixel of the image is anchored to a data point, and the placement does not depend on the size of the image or of the panel (2026-10-10, Sat, #25)
+ `geom_emoji()` downloads from a live twemoji CDN; the previous host answered with a 35-byte text placeholder and an HTTP 200 status, so every emoji glyph failed to render
+ `geom_worldcup2018()` now takes its 32 flags from the same host as `geom_flag()`; the hard-coded `upload.wikimedia.org` URLs it used before are unreachable from some networks, which made the function draw nothing there
+ `geom_subview()` reads a subview passed as its first argument again, so `geom_subview(legend, x = 10, y = 50)` works as the published examples show; that call used to fail with "x aesthetic mapping should be provided", which never mentioned the subview (#29)
+ rasterise `.eps`, `.ps` and `.pdf` at 300 dpi instead of ImageMagick's 72 dpi default, which is what made an `.eps` look grainy; the extension is matched case-insensitively, and `.pdf` no longer goes through a `pdftools`-backed reader, which failed with "there is no package called 'pdftools'" wherever that optional dependency is not installed (#30)
+ rebuild the vignette as a Quarto document and drop the old `.Rmd`, and document the geoms and helpers it had been missing: `ggbackground()`, `geom_worldcup2018()`, `geom_phylopic()` with `phylopic_uid()` and `download_phylopic()`, the `list.*()` helpers, the interactive geoms, legend keys, `theme_transparent()` and `theme_nothing()`, `image_read2()`, `ggpreview()`, and the image cache; the `geom_icon()` example runs again with the current Ionicons names
+ document that a `geom_subview()` inset need not be a `ggplot`: anything `ggplotify::as.grob()` converts can be used, so the vignette now draws a base graphics plot, a `ggplot` and a `magick` image as sub-views
+ document the aesthetics supported by `geom_image()` and `geom_image_repel()`, and make the `geom_image_repel()` example actually overlap
+ describe the package more fully in `Title` and `Description`, and take both from `DESCRIPTION` when rendering the README so that the two cannot drift apart
+ `geom_bgimage(pixel = c(col, row))` anchors the centre of that pixel to `(x, y)`. It anchored the bottom-right corner of the pixel, so the image landed one pixel away from the point it was given (2026-10-12, Mon, #25)
+ `geom_bgimage()` places its image as an annotation, so a background no longer contributes an observation to the plot: it cannot widen the axis ranges, and it no longer breaks a plot whose axis is discrete (2026-10-12, Mon, #25)
+ `ggbackground()` takes the row that carries its background from the data the plot already has. The fixed point it used before was an observation of its own, which widened the axis ranges and failed on a discrete axis (2026-10-12, Mon)
+ `geom_image()` draws the rows it can when a row's image is missing, unreadable, or an empty path. Such a row warns and draws nothing where it used to fail the whole plot, and two of them in one panel failed with "subscript out of bounds"
+ `geom_image()` draws the rows of an image list-column as row-wise images, and caches such images by content rather than by object identity
+ `geom_image_repel()` measures each repulsion box from that row's own image when the images come from a list-column. It used to receive the one-element list that wraps the object, warned "Unexpected img type: list" for every row, and silently repelled nothing
+ `geom_image()` and `geom_subview()` key the image cache on the content of a `magick` object rather than on its memory address, which every object shares: an image of the same size as one already cached was drawn as that cached image
+ the `"image"` legend key shows the image the layer was given, or the images a guide is given with `override.aes`, instead of the package logo in every key
+ `geom_phylopic()` accepts a local file or an absolute URL, which is what `download_phylopic()` returns and what its documentation describes
+ a multi-page `.pdf` is drawn as its first page, where the whole stack of pages used to be handed to the caller
+ `image_read2()` rasterises `.pdf`, `.eps` and `.ps` at 300 dpi like the image geoms do, and the extension of a URL is read after its query string
+ a URL that could not be checked because the optional `httr` is not installed is no longer counted as a missing image. The check is skipped with one warning and magick reads the URL, so it reports the real reason when an image fails
+ `clear_image_cache()` is exported, as its own documentation instructs
+ `stats` is declared in `Imports`
+ the vignette describes `ggbackground()`, `geom_bgimage()`, `ggimage.keytype` and `asp` as they behave
+ the vignette builds without a network connection. `R CMD check` renders it again, and it used to fail wherever the service queries were refused: the examples that ask PhyloPic or GitHub for a name are now shown rather than run
+ the vignette shows frame-data-driven animation with `gganimate`: preprocess a GIF into still files with `magick`, build one row per (glyph, frame), and draw it with `geom_image()` plus `transition_manual()`, while stating that a static plot still draws one image per row and that the per-frame delays, loop count and disposal of the source file are not preserved
+ `image_read2()` trims the margin of every frame of a multi-page document and returns them all. It used to crop the first page only and to keep every page when that page filled the canvas, so the number of frames depended on the margins
+ `image_read2()` no longer adds its 300 dpi density when an argument was passed to `...` unnamed, which would match the `density` formal of `image_read()` twice
+ the `"image"` legend key runs the layer's `image_fun`, so the key shows what the panel shows
+ `ape`, `ComplexHeatmap`, `ggtree`, `gridGraphics`, `lattice`, `patchwork`, `pheatmap`, `rsvg` and `UpSetR` are gone from `Suggests`. Only the vignette sub-view examples used them, and those now draw with what the package itself imports, leaving `httr`, `quarto` and `testthat` as the only suggested packages
+ `purrr` and `rlang::caller_env` are no longer imported by name: neither was ever called by this package. `purrr` stays in `Imports` because `ggiraph::layer_interactive()`, which the namespace carries, calls `purrr::` itself

## ggimage 0.3.6

+ add `geom_image_repel()` for deterministic, offline image overlap avoidance, with configurable iteration, force, padding, and movement direction
+ support `width` and `height` aesthetics in `geom_image()`, including aspect-ratio-preserving one-sided sizing
+ improve image legend keys with point, rectangle, image, and blank key modes, including colour and alpha handling
+ clarify image coordinate semantics and add regression coverage for `hjust`, nudging, `coord_fixed()`, and full-panel images (2026-09-25, Fri)
+ isolate `geom_bgimage()` to one background grob instead of repeating it for every plot row
+ bound and accelerate `geom_image_repel()` with deterministic cleanup limits, adaptive broad-phase candidate filtering, and sparse/dense regression benchmarks
+ deduplicate panel-local image preparation while preserving row-specific dimensions, positions, and interactive attributes
+ add configurable image-cache capacity, TTL, byte limits, LRU/FIFO eviction, and hit/miss/eviction diagnostics via `get_image_cache_stats()` and related helpers
+ cache successful URL checks with optional TTL/capacity controls, and make Phylopic downloads lock-safe with temporary files and atomic installation
+ optimize repeated subview conversion, legend logo loading, and `image_read2()` whitespace cropping while preserving alpha channels
+ add offline developer benchmarks covering repel scaling, image preparation, cache policy, image utilities, and subview rendering

## ggimage 0.3.5

+ use `magick::image_fx()` to contral transparency of images (2026-01-07, Thu, #61)
+ in interactive version, if `ipar` parameters not exist, use the static geom (2025-09-01, Mon, #60)
+ cache image to speedup (2025-08-31, Sun, #59)

## ggimage 0.3.4

+ remove `geom_twitchemote()` (2025-08-24, Sun, #37)
+ add interactive version and compatible with ggplot2 v=4.0.0 (2025-08-24, Sun, #58)
  - geom_image_interactive
  - geom_phylopic_interactive
  - GeomInteractiveImage
+ fixed `list.flag()`  (2025-08-24, Sun, #50)
+ update source url of `geom_flag()` (2025-08-24, Sun, #55)

## ggimage 0.3.3

+ now the image will not be distorted when both width and height are specified. (2023-06-19, Mon, #47)

## ggimage 0.3.2

+ support new api of phylopic database and fix the issue of gray colorspace (2023-04-02, Sun, #45, #46)
+ introduce 'filename' parameter in `ggpreview()` (2022-12-02, Fri)
+ `ggpreview()` works with `last_plot()` 
+ `ggpreview()` to preview a plot if it was saved to a file (2022-12-01, Thu)

## ggimage 0.3.1

+ update flag URL (2022-04-25, Mon, #39)

## ggimage 0.3.0

+ `download_phylopic` function (2021-09-29, Wed)

## ggimage 0.2.9

+ import `ggfun` (2021-08-20, Fri)

## ggimage 0.2.8

+ import `as_tibble` and `tibble` as `as_data_frame` and `data_frame` were deprecated (2020-04-01, Wed)
+ fixing aspect ratio of images (2020-01-09, Thu)

## ggimage 0.2.7

+ handle with online image not available (2019-12-31, Tue)
+ vignette move to <https://guangchuangyu.github.io/pkgdocs/ggimage.html>

## ggimage 0.2.6

+ change default image width to 256 for phylopic and it can be set to other values by e.g. `options(phylopic_width=512)` (2019-12-05, Thu)
  - <https://github.com/YuLab-SMU/treedata-book/issues/1>
  
## ggimage 0.2.5

+ `phylopic_uid` now return a data.frame (2019-11-29, Fri)
+ force flag to be uppercase (2019-1-04, Mon)
  - <https://github.com/GuangchuangYu/ggimage/issues/20>

## ggimage 0.2.4

+ bug fixed of checking phylopic ID (2019-10-17, Thu)
  - <https://groups.google.com/d/msg/bioc-ggtree/XfRV7BqNE4g/zCYH5aT5CwAJ>
+ fixed image aspect ratio (2019-10-02, Wed)
  - <https://stackoverflow.com/questions/58165226/is-it-possible-to-plot-images-in-a-ggplot2-plot-that-dont-get-distorted-when-y?noredirect=1#comment102713437_58165226>
  
## ggimage 0.2.3

+ update ionicons url (2019-09-23, Mon)

## ggimage 0.2.2

+ `geom_bgimage` (2019-08-05, Mon)

## ggimage 0.2.1

+ `phylopic` now check the id by `phylopic_valid_id` (2019-01-16, Wed)
+ `phylopic_valid_id` check whether a silhouette image is exist
  - if not exist, then use `supertaxa`
  - if supertaxa is also not exist, use `subtaxa`
  - if subtaxa is also not exist, use `other` related taxa
  - otherwise, return NA
+ `phylopic_uid` supports querying phylopic uid from scientific name
+ three optional image color legend key (2018-12-11, Tue)
  - <https://github.com/GuangchuangYu/ggimage/issues/18>

## ggimage 0.2.0

+ `draw_key_image` that draws key of image colors using `rectGrob` (2018-12-10, Mon)
+ support `aes(color)` 
+ compatible with ggplot2 v3.1.0

## ggimage 0.1.8

+ support `pdf` file (2018-12-03, Mon)
+ support `svg` file
  - <https://github.com/GuangchuangYu/ggimage/issues/11>
+ `geom_twitchemote` layer

## ggimage 0.1.7

+ `geom_worldcup2018` layer (2018-06-19, Tue)

## ggimage 0.1.6

+ update url of `icon`and `list.icon` (2018-05-23, Wed)
+ mv `get_aes_var` to `rvcheck`

## ggimage 0.1.5

+ compatible with ggplot2-dev, v=2.2.1.9000 (2018-05-02, Wed)
+ `image_read2` internally use `magick::image_read` with additional feature to
  remove marginal empty space (2018-04-25, Wed)
+ re-export `ggplotify::as.grob` and `ggplotify::as.ggplot` (2018-04-24, Tue)

## ggimage 0.1.4

+ `ggbackground` for setting background image of ggplot (2018-04-18, Wed)

## ggimage 0.1.3

+ `geom_image` now accepts `asp` parameter to adjust aspect ratio of images
  (2018-03-22, Thu)

## ggimage 0.1.2

+ mv code in `toGrob` for base plot to `base2grob` pkg

## ggimage 0.1.1

+ add angle parameter <2017-12-27, Wed>

CHANGES IN VERSION 0.1.0
------------------------
 o update vignette <2017-12-04, Mon>
 o geom_subview now supports aes mapping <2017-11-21, Tue>

CHANGES IN VERSION 0.0.7
------------------------
 o geom_subview compatible with meme object <2017-10-24, Tue>
   + https://github.com/GuangchuangYu/meme

CHANGES IN VERSION 0.0.6
------------------------
 o add image_fun parameter; user can use magick function to process image <2017-10-17, Tue>
 o using magick to read image instead of EBImage <2017-10-17, Tue>

CHANGES IN VERSION 0.0.5
------------------------
 o geom_icon <2017-09-05, Wed>

CHANGES IN VERSION 0.0.4
------------------------
 o geom_emoji <2017-03-23, Thu>
 o geom_flag <2017-03-22, Wed>
 o support size = Inf <2017-03-22, Wed>
 o geom_phylopic layer <2017-03-22, Wed>
 o color parameter for geom_image and related layers <2017-03-22, Wed>
 o geom_pokemon <2017-03-22, Wed>

CHANGES IN VERSION 0.0.3
------------------------
 o geom_subview <2017-03-21, Tue>
 o theme_transparent from ggtree <2017-03-21, Tue>

CHANGES IN VERSION 0.0.2
------------------------
 o remove parsing `image` parameter via `...`, and set image="https://www.r-project.org/logo/Rlogo.png" by default <2017-02-22, Wed>

CHANGES IN VERSION 0.0.1
------------------------
 o geom_image layer <2017-02-21, Tue>
