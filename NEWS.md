## ggimage 0.3.4.002

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
