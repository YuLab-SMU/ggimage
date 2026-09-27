## geom_ggtree_image <- function() {

## }


##' geom layer for visualizing image files
##'
##'
##' @title geom_image
##' @param mapping aes mapping
##' @param data data
##' @param stat stat
##' @param position position
##' @param inherit.aes logical, whether inherit aes from ggplot()
##' @param na.rm logical, whether remove NA values
##' @param by one of 'width' or 'height'
##' @param nudge_x horizontal adjustment to nudge image
##' @param nudge_y vertical adjustment to nudge image
##' @param use_cache logical, whether to use image caching for better performance (default: TRUE)
##' @param width,height Image width and height in native panel units, matching the
##'   plotting units used by the `size`/`by` behavior. They can be mapped per
##'   row via `aes(width = ..., height = ...)` or set for the whole layer. They
##'   override `size` (and therefore also `size = Inf` and `by`):
##'   - only `width` is provided, the height is derived from the image ratio;
##'   - only `height` is provided, the width is derived from the image ratio;
##'   - both are provided, the image is drawn in exactly that box, which may
##'     distort it when the ratio of the box differs from the image ratio.
##'
##'   `coord_fixed()` controls the physical aspect of the panel; it does not
##'   reinterpret these values. Values that are `NA`, not finite or not
##'   positive are ignored, and the `size`/`by` behavior is used for the
##'   affected image.
##' @param ... additional parameters
##' @return geom layer
##' @importFrom ggplot2 layer
##' @export
##' @examples
##' \dontrun{
##' library("ggplot2")
##' library("ggimage")
##' set.seed(2017-02-21)
##' d <- data.frame(x = rnorm(10),
##'                 y = rnorm(10),
##'                 image = sample(c("https://www.r-project.org/logo/Rlogo.png",
##'                                 "https://jeroenooms.github.io/images/frink.png"),
##'                               size=10, replace = TRUE)
##'                )
##' # With caching enabled (default)
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image))
##'
##' # With caching disabled
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image), use_cache=FALSE)
##'
##' # Per-row image size, via the `width` and `height` aesthetics
##' ggplot(d, aes(x, y)) +
##'     geom_image(aes(image=image, width=abs(x)/10, height=abs(y)/10))
##'
##' # Only one of them: the other dimension keeps the ratio of the image
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image, width=abs(x)/10))
##' }
##' @author Guangchuang Yu
geom_image <- function(mapping=NULL, data=NULL, stat="identity",
                       position="identity", inherit.aes=TRUE,
                       na.rm=FALSE, by="width", nudge_x = 0, nudge_y = 0, use_cache=TRUE,
                       width=NULL, height=NULL, ...) {

    by <- match.arg(by, c("width", "height"))

    params <- list(
        na.rm = na.rm,
        by = by,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        use_cache = use_cache,
        ...
    )
    if (!is.null(width)) params$width <- width
    if (!is.null(height)) params$height <- height

    layer(
        data=data,
        mapping=mapping,
        geom=GeomImage,
        stat=stat,
        position=position,
        show.legend=NA,
        inherit.aes=inherit.aes,
        params = params,
        check.aes = FALSE
    )
}


##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 aes
##' @importFrom grid gTree
##' @importFrom grid gList
recycle_image_dimension <- function(value, n) {
    if (is.null(value) || length(value) == 0L) {
        return(rep(NA_real_, n))
    }
    value <- rep(as.numeric(value), length.out = n)
    value[!is.finite(value) | value <= 0] <- NA_real_
    value
}

resolve_image_dimension <- function(data_value, fallback, n) {
    value <- recycle_image_dimension(data_value, n)
    if (!is.null(fallback) && length(fallback) > 0L) {
        fallback <- recycle_image_dimension(fallback, n)
        value[is.na(value)] <- fallback[is.na(value)]
    }
    value
}

GeomImage <- ggproto("GeomImage", Geom,
                     setup_data = function(data, params) {
                         if (is.null(data$subset))
                             return(data)
                         data[which(data$subset),]
                     },

                     default_aes = aes(image=system.file("extdata/Rlogo.png", package="ggimage"),
                                       size=0.05, width = NA_real_, height = NA_real_, colour = NULL, angle = 0, alpha=1),

                     draw_panel = function(data, panel_params, coord, by, na.rm=FALSE,
                                           .fun = NULL, image_fun = NULL,
                                           hjust=0.5, nudge_x = 0, nudge_y = 0, asp=1,
                                           use_cache=TRUE, width = NULL, height = NULL) {
                         data <- GeomImage$make_image_data(data, panel_params, coord, .fun, nudge_x, nudge_y)

                         GeomImage$draw_grobs(data, panel_params, coord, by,
                                              image_fun, hjust, asp, use_cache,
                                              width, height)
                     },
                     ## draws the images of an already transformed `data`; split
                     ## out of `draw_panel()` so that layers which move the data
                     ## around (e.g. `GeomImageRepel`) can reuse it.
                     draw_grobs = function(data, panel_params, coord, by, image_fun = NULL,
                                           hjust=0.5, asp=1, use_cache=TRUE,
                                           width = NULL, height = NULL) {
                         adjs <- GeomImage$build_adjust(data, panel_params, by)
                         widths <- resolve_image_dimension(
                             if ("width" %in% names(data)) data$width else NULL,
                             width,
                             nrow(data)
                         )
                         heights <- resolve_image_dimension(
                             if ("height" %in% names(data)) data$height else NULL,
                             height,
                             nrow(data)
                         )
                         image_fun_key <- if (use_cache) {
                             image_fun_cache_key(image_fun)
                         } else {
                             NULL
                         }

                         grobs <- lapply(seq_len(nrow(data)), function(i){
                              imageGrob(x = data$x[i],
                                        y = data$y[i],
                                        size = data$size[i],
                                        img = data$image[i],
                                        colour = data$colour[i],
                                        opacity = data$alpha[i],
                                        angle = data$angle[i],
                                        adj = adjs[i],
                                        image_fun = image_fun,
                                        image_fun_key = image_fun_key,
                                        hjust = hjust,
                                        by = by,
                                        asp = asp,
                                        use_cache = use_cache,
                                        width = widths[i],
                                        height = heights[i]
                              )
                             })
                         ggname("geom_image", gTree(children = do.call(gList, grobs)))
                     },
                     make_image_data = function(data, panel_params, coord, .fun, nudge_x = 0, nudge_y = 0,...){
                         data$x <- data$x + nudge_x
                         data$y <- data$y + nudge_y
                         data <- coord$transform(data, panel_params)

                         if (!is.null(.fun) && is.function(.fun)) {
                             data$image <- .fun(data$image)
                         }
                         if (is.null(data$image)){
                             return(NULL)
                         }else{
                             return(data)
                         }
                     },
                     build_adjust = function(data, panel_params, by){
                         if (by=='height' && "y.range" %in% names(panel_params)) {
                             adjs <- data$size / diff(panel_params$y.range)
                         } else if (by == 'width' && "x.range" %in% names(panel_params)){
                             adjs <- data$size / diff(panel_params$x.range)
                         } else if ("r.range" %in% names(panel_params)) {
                             adjs <- data$size / diff(panel_params$r.range)
                         } else {
                             adjs <- data$size
                         }
                         adjs[is.infinite(adjs)] <- 1
                         return(adjs)
                     },
                     non_missing_aes = c("size", "image"),
                     required_aes = c("x", "y"),
                     ## `draw_key_image()` respects the `ggimage.keytype` option,
                     ## use `options(ggimage.keytype = "blank")` to restore the
                     ## key-less behaviour of previous versions.
                     draw_key = draw_key_image
                     )

#### caching mechanism for images ####
# Image values continue to live in yulab.utils' process-global cache.  The
# metadata below is package-local and is deliberately kept separate so older
# cache entries (and users of yulab.utils) remain readable.
.IMAGE_CACHE_ITEM <- ".ggimage_cache_image"
.IMAGE_TRANSFORM_CACHE_ITEM <- ".ggimage_cache_image_transform"
.IMAGE_CACHE_META <- new.env(parent = emptyenv())
.IMAGE_CACHE_STATS <- new.env(parent = emptyenv())

.image_cache_stats_default <- function() {
  list(hits = 0L, misses = 0L, evictions = 0L, expirations = 0L)
}

.image_cache_stats_get <- function(item) {
  stats <- get0(item, envir = .IMAGE_CACHE_STATS, ifnotfound = NULL)
  if (is.null(stats)) stats <- .image_cache_stats_default()
  stats
}

.image_cache_stats_add <- function(item, field, amount = 1L) {
  stats <- .image_cache_stats_get(item)
  stats[[field]] <- as.integer(stats[[field]] + amount)
  assign(item, stats, envir = .IMAGE_CACHE_STATS)
  invisible(NULL)
}

.image_cache_stats_reset <- function() {
  rm(list = ls(envir = .IMAGE_CACHE_STATS, all.names = TRUE),
     envir = .IMAGE_CACHE_STATS)
  invisible(NULL)
}

# Cache policy options (all limits are per cache unless a cache-specific option
# is supplied).  The defaults are intentionally unlimited, matching the
# historical behaviour.  A clock option is provided as a small test seam and
# must return a POSIXct or numeric value in seconds.
#
#   ggimage.image_cache = list(capacity = Inf, ttl = Inf, bytes = Inf,
#                              eviction = "lru")
#   ggimage.image_cache_capacity / _ttl / _bytes / _eviction
#   ggimage.image_cache_base_capacity / _transform_capacity
#   ggimage.image_cache_base_ttl / _transform_ttl
#   ggimage.image_cache_base_bytes / _transform_bytes
#   ggimage.image_cache_clock
#
# Byte limits are estimates based on object.size(), with a conservative
# width * height * 4 estimate for magick images. They do not measure native
# ImageMagick allocations and should be treated as approximate guardrails.
.image_cache_policy <- function() {
  nested <- getOption("ggimage.image_cache", NULL)
  if (!is.list(nested)) nested <- list()
  nested_base <- nested$base
  if (!is.list(nested_base)) nested_base <- list()
  nested_transform <- nested$transform
  if (!is.list(nested_transform)) nested_transform <- nested$transformed
  if (!is.list(nested_transform)) nested_transform <- list()

  pick <- function(option, ...) {
    value <- getOption(option, NULL)
    if (!is.null(value)) return(value)
    keys <- c(...)
    for (key in keys) {
      if (!is.null(nested[[key]])) return(nested[[key]])
    }
    NULL
  }
  pick_specific <- function(option, specific, global, default) {
    value <- getOption(option, NULL)
    if (!is.null(value)) return(value)
    if (!is.null(specific)) return(specific)
    if (!is.null(global)) return(global)
    default
  }
  normalize_capacity <- function(value) {
    value <- suppressWarnings(as.numeric(value)[1L])
    if (length(value) != 1L || is.na(value) || is.nan(value) || value < 0)
      return(Inf)
    if (is.infinite(value)) Inf else floor(value)
  }
  normalize_ttl <- function(value) {
    value <- suppressWarnings(as.numeric(value)[1L])
    if (length(value) != 1L || is.na(value) || is.nan(value) || value < 0)
      return(Inf)
    value
  }
  normalize_bytes <- function(value) {
    value <- suppressWarnings(as.numeric(value)[1L])
    if (length(value) != 1L || is.na(value) || is.nan(value) || value < 0)
      return(Inf)
    if (is.infinite(value)) Inf else floor(value)
  }
  global_capacity <- pick("ggimage.image_cache_capacity", "capacity")
  if (is.null(global_capacity))
    global_capacity <- getOption("ggimage.image_cache_max_items", NULL)
  if (is.null(global_capacity))
    global_capacity <- getOption("ggimage.cache.max_items", NULL)
  if (is.null(global_capacity)) global_capacity <- nested$max_items
  global_ttl <- pick("ggimage.image_cache_ttl", "ttl")
  if (is.null(global_ttl)) global_ttl <- getOption("ggimage.cache.ttl", NULL)
  if (is.null(global_ttl)) global_ttl <- nested$ttl
  global_eviction <- pick("ggimage.image_cache_eviction", "eviction")
  global_bytes <- pick("ggimage.image_cache_bytes", "bytes", "byte_capacity")
  if (is.null(global_bytes)) global_bytes <- getOption("ggimage.image_cache_byte_capacity", NULL)
  if (is.null(global_bytes)) global_bytes <- getOption("ggimage.image_cache_max_bytes", NULL)
  base_capacity <- getOption("ggimage.image_cache_base_capacity", NULL)
  if (is.null(base_capacity)) base_capacity <- nested_base$capacity
  transform_capacity <- getOption("ggimage.image_cache_transform_capacity", NULL)
  if (is.null(transform_capacity)) transform_capacity <- nested_transform$capacity
  base_ttl <- getOption("ggimage.image_cache_base_ttl", NULL)
  if (is.null(base_ttl)) base_ttl <- nested_base$ttl
  transform_ttl <- getOption("ggimage.image_cache_transform_ttl", NULL)
  if (is.null(transform_ttl)) transform_ttl <- nested_transform$ttl
  base_bytes <- getOption("ggimage.image_cache_base_bytes", NULL)
  if (is.null(base_bytes)) base_bytes <- getOption("ggimage.image_cache_base_byte_capacity", NULL)
  if (is.null(base_bytes)) base_bytes <- nested_base$bytes %||% nested_base$byte_capacity
  transform_bytes <- getOption("ggimage.image_cache_transform_bytes", NULL)
  if (is.null(transform_bytes)) transform_bytes <- getOption("ggimage.image_cache_transform_byte_capacity", NULL)
  if (is.null(transform_bytes)) transform_bytes <- nested_transform$bytes %||% nested_transform$byte_capacity
  eviction <- global_eviction %||% "lru"
  if (length(eviction) != 1L || is.na(eviction) ||
      !eviction %in% c("lru", "fifo", "none")) eviction <- "lru"
  clock <- getOption("ggimage.image_cache_clock", nested$clock %||% Sys.time)
  if (!is.function(clock)) clock <- Sys.time
  list(
    base_capacity = normalize_capacity(pick_specific(
      "ggimage.image_cache_base_capacity", base_capacity,
      global_capacity, Inf)),
    transform_capacity = normalize_capacity(pick_specific(
      "ggimage.image_cache_transform_capacity", transform_capacity,
      global_capacity, Inf)),
    base_ttl = normalize_ttl(pick_specific(
      "ggimage.image_cache_base_ttl", base_ttl, global_ttl, Inf)),
    transform_ttl = normalize_ttl(pick_specific(
      "ggimage.image_cache_transform_ttl", transform_ttl, global_ttl, Inf)),
     base_bytes = normalize_bytes(pick_specific(
       "ggimage.image_cache_base_bytes", base_bytes, global_bytes, Inf)),
     transform_bytes = normalize_bytes(pick_specific(
       "ggimage.image_cache_transform_bytes", transform_bytes, global_bytes, Inf)),
     eviction = as.character(eviction),
    clock = clock
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

##' Get or set the global image cache policy.
##'
##' Cache limits may also be supplied with `options()`: use
##' `ggimage.image_cache_capacity`, `ggimage.image_cache_ttl`,
##' `ggimage.image_cache_bytes`, and `ggimage.image_cache_eviction` for both
##' caches, or the corresponding `ggimage.image_cache_base_*` and
##' `ggimage.image_cache_transform_*` options independently. Byte limits are
##' approximate object-size guardrails rather than process-memory measurements.
##' The default capacity, TTL, and byte cap are infinite, preserving the
##' historical unbounded cache. `eviction` is one of `"lru"`, `"fifo"`, or
##' `"none"`; the last value disables capacity and byte eviction. `clock` is
##' intended for deterministic tests and should return seconds or POSIXct.
##'
##' @param capacity,ttl Optional shared capacity (number of entries) and TTL
##'   (seconds). `NULL` leaves the current option unchanged.
##' @param bytes Optional shared estimated byte cap for each cache. `NULL` leaves
##'   the current option unchanged. This is an estimate, not a process-memory
##'   measurement; native image buffers may be larger than the estimate.
##' @param eviction Optional eviction policy: `"lru"`, `"fifo"`, or `"none"`.
##' @param base_capacity,transform_capacity Cache-specific capacities.
##' @param base_ttl,transform_ttl Cache-specific TTLs in seconds.
##' @param base_bytes,transform_bytes Cache-specific estimated byte caps.
##' @param clock Optional function used as the cache clock.
##' @return `get_image_cache_policy()` returns a policy list. The setter returns
##'   the previous policy invisibly.
##' @export
get_image_cache_policy <- function() {
  policy <- .image_cache_policy()
  policy$clock <- NULL
  policy
}

##' @rdname get_image_cache_policy
##' @export
set_image_cache_policy <- function(capacity = NULL, ttl = NULL, eviction = NULL,
                                   base_capacity = NULL,
                                   transform_capacity = NULL,
                                   base_ttl = NULL, transform_ttl = NULL,
                                   bytes = NULL, base_bytes = NULL,
                                   transform_bytes = NULL, clock = NULL) {
  old <- get_image_cache_policy()
  values <- list(
    ggimage.image_cache_capacity = capacity,
    ggimage.image_cache_ttl = ttl,
    ggimage.image_cache_eviction = eviction,
    ggimage.image_cache_base_capacity = base_capacity,
    ggimage.image_cache_transform_capacity = transform_capacity,
    ggimage.image_cache_base_ttl = base_ttl,
    ggimage.image_cache_transform_ttl = transform_ttl,
    ggimage.image_cache_bytes = bytes,
    ggimage.image_cache_base_bytes = base_bytes,
    ggimage.image_cache_transform_bytes = transform_bytes,
    ggimage.image_cache_clock = clock
  )
  values <- values[!vapply(values, is.null, logical(1))]
  if (length(values)) do.call(options, values)
  invisible(old)
}

# Helper function to check if image is invalid
is_invalid <- function(img) {
  is.null(img) || length(img) == 0 || (is.character(img) && (is.na(img) || img == ""))
}

# generate stable key: path -> standardized character; object -> digest
#' @importFrom digest digest
image_cache_key <- function(img) {
  if (is.character(img)) {
    kp <- tryCatch(normalizePath(img, winslash = "/", mustWork = FALSE),
                   error = function(e) img)
    return(kp)
  }
  digest(img)
}

# generate transform key based on base_key and transform parameters
image_fun_cache_key <- function(image_fun) {
  if (is.null(image_fun)) "" else digest(image_fun)
}

image_transform_key <- function(base_key, angle, colour, opacity, image_fun = NULL,
                                image_fun_key = NULL) {
  if (is.null(image_fun_key)) image_fun_key <- image_fun_cache_key(image_fun)
  paste(base_key,
        if (is.null(angle) || is.na(angle)) 0 else angle,
        if (is.null(colour) || is.na(colour)) "" else as.character(colour),
        if (is.null(opacity) || is.na(opacity)) "" else as.character(opacity),
        image_fun_key, sep = "|")
}

.image_cache_estimate_bytes <- function(value) {
  estimate <- suppressWarnings(as.numeric(object.size(value)))
  if (length(estimate) != 1L || !is.finite(estimate) || estimate < 0)
    estimate <- 0
  if (methods::is(value, "magick-image")) {
    info <- tryCatch(magick::image_info(value), error = function(e) NULL)
    if (!is.null(info) && nrow(info)) {
      pixels <- suppressWarnings(sum(as.numeric(info$width) *
                                     as.numeric(info$height) * 4))
      if (is.finite(pixels)) estimate <- max(estimate, pixels)
    }
  }
  floor(estimate)
}

.image_cache_now <- function(policy) {
  value <- tryCatch(policy$clock(), error = function(e) Sys.time())
  value <- suppressWarnings(as.numeric(value[[1L]]))
  if (!length(value) || !is.finite(value)) as.numeric(Sys.time()) else value
}

.image_cache_meta_get <- function(item) {
  if (exists(item, envir = .IMAGE_CACHE_META, inherits = FALSE))
    get(item, envir = .IMAGE_CACHE_META, inherits = FALSE)
  else list()
}

.image_cache_meta_set <- function(item, metadata) {
  assign(item, metadata, envir = .IMAGE_CACHE_META)
}

.image_cache_next_sequence <- function() {
  sequence <- get0(".sequence", envir = .IMAGE_CACHE_META, ifnotfound = 0L)
  sequence <- sequence + 1L
  assign(".sequence", sequence, envir = .IMAGE_CACHE_META)
  sequence
}

.image_cache_remove_keys <- function(item, keys) {
  if (!length(keys)) return(invisible(NULL))
  values <- tryCatch(get_cache_item(item), error = function(e) NULL)
  if (is.null(values)) return(invisible(NULL))
  keep <- setdiff(names(values), keys)
  tryCatch(rm_cache_item(item), error = function(e) NULL)
  if (length(keep)) {
    tryCatch(update_cache_item(item, values[keep]), error = function(e) NULL)
  }
  invisible(NULL)
}

.image_cache_prune <- function(item, policy, now) {
  values <- tryCatch(get_cache_item(item), error = function(e) NULL)
  if (is.null(values) || !length(values)) return(values)
  # Let yulab.utils remove entries written with its native TTL wrapper.
  for (key in names(values)) tryCatch(get_cache_element(item, key), error = function(e) NULL)
  values <- tryCatch(get_cache_item(item), error = function(e) NULL)
  metadata <- .image_cache_meta_get(item)
  expired <- names(values)[vapply(names(values), function(key) {
    entry <- metadata[[key]]
    !is.null(entry) && is.finite(entry$expires) && now >= entry$expires
  }, logical(1))]
  if (length(expired)) {
    .image_cache_remove_keys(item, expired)
    .image_cache_stats_add(item, "expirations", length(expired))
    metadata[expired] <- NULL
    .image_cache_meta_set(item, metadata)
    values <- tryCatch(get_cache_item(item), error = function(e) NULL)
  }
  if (length(metadata)) {
    metadata <- metadata[intersect(names(metadata), names(values))]
    .image_cache_meta_set(item, metadata)
  }
  values
}

.image_cache_touch <- function(item, key, ttl, now, reset = FALSE, bytes = NULL) {
  metadata <- .image_cache_meta_get(item)
  entry <- metadata[[key]]
  if (is.null(entry) || reset) {
    expires <- if (is.finite(ttl)) now + ttl else Inf
    entry <- list(created = now, last_access = now, expires = expires,
                  sequence = .image_cache_next_sequence())
  } else {
    entry$last_access <- now
  }
  if (!is.null(bytes)) entry$bytes <- .image_cache_estimate_bytes(bytes)
  metadata[[key]] <- entry
  .image_cache_meta_set(item, metadata)
}

.image_cache_enforce_capacity <- function(item, capacity, eviction, now, ttl,
                                          bytes = Inf) {
  values <- .image_cache_prune(item, .image_cache_policy(), now)
  if (is.null(values) || !length(values) || eviction == "none")
    return(invisible(NULL))
  metadata <- .image_cache_meta_get(item)
  for (key in names(values)) {
    if (is.null(metadata[[key]]))
      .image_cache_touch(item, key, ttl, now, bytes = values[[key]])
    else if (is.null(metadata[[key]]$bytes)) {
      metadata[[key]]$bytes <- .image_cache_estimate_bytes(values[[key]])
    }
  }
  metadata <- .image_cache_meta_get(item)
  total_bytes <- sum(vapply(metadata[names(values)], function(x) {
    value <- x$bytes
    if (is.null(value) || !is.finite(value) || value < 0) 0 else value
  }, numeric(1)))
  over_capacity <- is.finite(capacity) && length(values) > capacity
  over_bytes <- is.finite(bytes) && total_bytes > bytes
  if (!over_capacity && !over_bytes) return(invisible(NULL))
  metric <- if (eviction == "fifo") {
    vapply(metadata[names(values)], function(x) x$sequence, numeric(1))
  } else {
    vapply(metadata[names(values)], function(x) x$last_access, numeric(1))
  }
  ordered <- names(values)[order(metric, names(values))]
  remove <- character()
  remaining_bytes <- total_bytes
  for (key in ordered) {
    if ((!is.finite(capacity) || length(values) - length(remove) <= capacity) &&
        (!is.finite(bytes) || remaining_bytes <= bytes)) break
    remove <- c(remove, key)
    remaining_bytes <- remaining_bytes - (metadata[[key]]$bytes %||% 0)
  }
  if (!length(remove)) return(invisible(NULL))
  .image_cache_remove_keys(item, remove)
  metadata[remove] <- NULL
  .image_cache_meta_set(item, metadata)
  .image_cache_stats_add(item, "evictions", length(remove))
  invisible(NULL)
}

# store/read cache entries while preserving the original helper signatures
cache_get_image <- function(key, use_cache = TRUE) {
  if (!use_cache) return(NULL)
  policy <- .image_cache_policy()
  now <- .image_cache_now(policy)
  .image_cache_prune(.IMAGE_CACHE_ITEM, policy, now)
  value <- tryCatch(get_cache_element(.IMAGE_CACHE_ITEM, key), error = function(e) NULL)
  if (!is.null(value)) {
    .image_cache_stats_add(.IMAGE_CACHE_ITEM, "hits")
    .image_cache_touch(.IMAGE_CACHE_ITEM, key, policy$base_ttl, now)
  } else {
    .image_cache_stats_add(.IMAGE_CACHE_ITEM, "misses")
  }
  value
}

cache_set_image <- function(key, value, use_cache = TRUE) {
  if (!use_cache) return(invisible(value))
  policy <- .image_cache_policy()
  now <- .image_cache_now(policy)
  tryCatch(update_cache_item(.IMAGE_CACHE_ITEM, stats::setNames(list(value), key)),
           error = function(e) return(invisible(NULL)))
    .image_cache_touch(.IMAGE_CACHE_ITEM, key, policy$base_ttl, now,
                     reset = TRUE, bytes = value)
  .image_cache_enforce_capacity(.IMAGE_CACHE_ITEM, policy$base_capacity,
                                policy$eviction, now, policy$base_ttl,
                                 policy$base_bytes)
  invisible(value)
}

cache_get_transformed <- function(tkey, use_cache = TRUE) {
  if (!use_cache) return(NULL)
  policy <- .image_cache_policy()
  now <- .image_cache_now(policy)
  .image_cache_prune(.IMAGE_TRANSFORM_CACHE_ITEM, policy, now)
  value <- tryCatch(get_cache_element(.IMAGE_TRANSFORM_CACHE_ITEM, tkey), error = function(e) NULL)
  if (!is.null(value)) {
    .image_cache_stats_add(.IMAGE_TRANSFORM_CACHE_ITEM, "hits")
    .image_cache_touch(.IMAGE_TRANSFORM_CACHE_ITEM, tkey,
                       policy$transform_ttl, now)
  } else {
    .image_cache_stats_add(.IMAGE_TRANSFORM_CACHE_ITEM, "misses")
  }
  value
}

cache_set_transformed <- function(tkey, value, use_cache = TRUE) {
  if (!use_cache) return(invisible(value))
  policy <- .image_cache_policy()
  now <- .image_cache_now(policy)
  tryCatch(update_cache_item(.IMAGE_TRANSFORM_CACHE_ITEM,
                             stats::setNames(list(value), tkey)),
           error = function(e) return(invisible(NULL)))
  .image_cache_touch(.IMAGE_TRANSFORM_CACHE_ITEM, tkey, policy$transform_ttl,
                     now, reset = TRUE, bytes = value)
  .image_cache_enforce_capacity(.IMAGE_TRANSFORM_CACHE_ITEM,
                                policy$transform_capacity, policy$eviction,
                                now, policy$transform_ttl,
                                 policy$transform_bytes)
  invisible(value)
}

# clean all image cache (and policy metadata)
clear_image_cache <- function() {
  tryCatch(rm_cache_item(.IMAGE_CACHE_ITEM), error = function(e) invisible(NULL))
  tryCatch(rm_cache_item(.IMAGE_TRANSFORM_CACHE_ITEM), error = function(e) invisible(NULL))
  for (item in c(.IMAGE_CACHE_ITEM, .IMAGE_TRANSFORM_CACHE_ITEM))
    if (exists(item, envir = .IMAGE_CACHE_META, inherits = FALSE))
      rm(list = item, envir = .IMAGE_CACHE_META)
  .image_cache_stats_reset()
  invisible(NULL)
}

.image_cache_stats_snapshot <- function(item, policy, now) {
  .image_cache_prune(item, policy, now)
  values <- tryCatch(get_cache_item(item), error = function(e) NULL)
  metadata <- .image_cache_meta_get(item)
  bytes <- if (length(values)) sum(vapply(names(values), function(key) {
    value <- metadata[[key]]$bytes
    if (is.null(value) || !is.finite(value) || value < 0)
      .image_cache_estimate_bytes(values[[key]]) else value
  }, numeric(1))) else 0
  stats <- unlist(.image_cache_stats_get(item), use.names = TRUE)
  c(stats, entries = length(values), bytes = bytes)
}

##' Return image cache hit, miss, eviction, expiry, and size diagnostics.
##'
##' The `bytes` and `entries` values are estimates/current counts. Byte estimates
##' use `object.size()` (and a pixel-based lower bound for magick images), so
##' native ImageMagick allocations are not accounted for exactly.
##'
##' @return A list with `base` and `transform` named diagnostic vectors.
##' @export
get_image_cache_stats <- function() {
  policy <- .image_cache_policy()
  now <- .image_cache_now(policy)
  list(
    base = .image_cache_stats_snapshot(.IMAGE_CACHE_ITEM, policy, now),
    transform = .image_cache_stats_snapshot(.IMAGE_TRANSFORM_CACHE_ITEM,
                                             policy, now)
  )
}

##' @rdname get_image_cache_stats
##' @export
get_image_cache_diagnostics <- get_image_cache_stats

##' Reset image cache hit/miss and eviction/expiry counters.
##'
##' Cache entries are retained; use `clear_image_cache()` to clear entries and
##' reset these counters together.
##' @export
reset_image_cache_stats <- function() {
  .image_cache_stats_reset()
  invisible(NULL)
}

# Stat cache (pruning expired entries first keeps the count meaningful)
get_image_cache_size <- function() {
  policy <- .image_cache_policy()
  .image_cache_prune(.IMAGE_CACHE_ITEM, policy, .image_cache_now(policy))
  ci <- tryCatch(get_cache_item(.IMAGE_CACHE_ITEM), error = function(e) NULL)
  length(ci)
}

get_image_transform_cache_size <- function() {
  policy <- .image_cache_policy()
  .image_cache_prune(.IMAGE_TRANSFORM_CACHE_ITEM, policy, .image_cache_now(policy))
  ci <- tryCatch(get_cache_item(.IMAGE_TRANSFORM_CACHE_ITEM), error = function(e) NULL)
  length(ci)
}

# Apply opacity independently of colour so alpha-only mappings work.
apply_image_opacity <- function(img, opacity) {
  if (is.null(opacity) || is.na(opacity) || opacity == 1) {
    return(img)
  }

  # Make the alpha channel explicit before image_fx. Without a matte channel,
  # ImageMagick leaves opaque images unchanged when alpha is modified.
  img <- magick::image_background(img, color = "none")
  magick::image_fx(img,
                   expression = paste0("u.a * ", opacity),
                   channel = "alpha")
}

prepare_image <- function(img, colour, opacity, angle, image_fun, use_cache = TRUE,
                          image_fun_key = NULL) {
  tryCatch({
    if (is_invalid(img)) {
      warning("Invalid image path or object provided: ", paste(img, collapse=","))
      return(NULL)
    }

    # Unified generation of base images key
    img_key <- image_cache_key(img)

    # First check the base images cache
    cached_img <- cache_get_image(img_key, use_cache)

    # if don't hit the cache, read the raw image file.
    if (is.null(cached_img)) {
      if (!methods::is(img, "magick-image")) {
        if (is.character(img)) {
          cached_img <- switch(tools::file_ext(img),
                               "svg" = magick::image_read_svg(img),
                               "pdf" = magick::image_read_pdf(img),
                               magick::image_read(img))
        } else {
          warning("Unexpected img type: ", class(img))
          return(NULL)
        }
      } else {
        cached_img <- img
      }
      if (!is.null(cached_img)) {
        cache_set_image(img_key, cached_img, use_cache)
      }
    }

    if (is.null(cached_img)) {
      warning("Failed to load image")
      return(NULL)
    }

    # —— secondary cache（optional，cache for angle/colour/opacity image） —— #
    tkey <- NULL
    transformed <- NULL
    if (use_cache) {
      tkey <- image_transform_key(img_key, angle, colour, opacity, image_fun,
                                  image_fun_key)
      transformed <- cache_get_transformed(tkey, use_cache)
    }

    if (is.null(transformed)) {
      # apply the available user function
      if (!is.null(image_fun) && is.function(image_fun)) {
        transformed <- image_fun(cached_img)
      } else {
        transformed <- cached_img
      }

      # rotate
      if (!is.null(angle) && !is.na(angle) && angle != 0) {
        transformed <- magick::image_rotate(transformed, angle)
      }
      # Colorize independently from opacity; alpha is applied below for both
      # colour and colour = NULL cases.
      if (!is.null(colour) && !is.na(colour)) {
        transformed <- magick::image_colorize(transformed, opacity = 100, color = colour)
      }
      transformed <- apply_image_opacity(transformed, opacity)

      cache_set_transformed(tkey, transformed, use_cache)
    }

    transformed
  }, error = function(e) {
    warning("Error in prepare_image: ", e$message)
    NULL
  })
}

#### caching mechanism for images end ####

##' @importFrom magick image_read
##' @importFrom magick image_read_svg
##' @importFrom magick image_read_pdf
##' @importFrom magick image_transparent
##' @importFrom magick image_rotate
##' @importFrom grid rasterGrob
##' @importFrom grid viewport
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @importFrom methods is
##' @importFrom tools file_ext
##' @importFrom yulab.utils get_cache_element update_cache_item rm_cache_item get_cache_item
imageGrob <- function(x, y, size, img, colour, opacity, angle, adj, image_fun, hjust, by,
                     asp=1, default.units='native', use_cache=TRUE,
                     width = NA_real_, height = NA_real_, image_fun_key = NULL) {
  if (is.na(img)) {
    return(zeroGrob())
  }

  # Use prepare_image for unified caching and transformation
  cached_img <- prepare_image(img, colour, opacity, angle, image_fun, use_cache,
                              image_fun_key)

  if (is.null(cached_img)) {
    return(zeroGrob())
  }

  explicit_width <- length(width) == 1L && is.finite(width) && width > 0
  explicit_height <- length(height) == 1L && is.finite(height) && height > 0

  ## A fully explicit box does not depend on the source image aspect ratio.
  asp <- if (explicit_width && explicit_height) {
    NULL
  } else {
    getAR2(cached_img) / asp
  }

  if (explicit_width || explicit_height) {
    if (explicit_width && explicit_height) {
      grob_width <- width
      grob_height <- height
    } else if (explicit_width) {
      grob_width <- width
      grob_height <- width / asp
    } else {
      grob_width <- height * asp
      grob_height <- height
    }
    width <- grob_width
    height <- grob_height
  } else if (size == Inf) {
    x <- 0.5; y <- 0.5; width <- 1; height <- 1
  } else if (by == "width") {
    width <- size * adj; height <- size / asp
  } else {
    width <- size * asp * adj; height <- size
  }

  explicit_dimensions <- explicit_width || explicit_height
  if (hjust == 0 || hjust == "left") {
    x <- x + width/2
  } else if (hjust == 1 || hjust == "right") {
    x <- x - width/2
  }

  grob <- rasterGrob(
    x = x, y = y, image = cached_img, default.units = default.units,
    height = height, width = if (explicit_dimensions || size == Inf) width else NULL
  )
  grob
}

# ##' @importFrom grid makeContent
# ##' @importFrom grid convertHeight
# ##' @importFrom grid convertWidth
# ##' @importFrom grid unit
# ##' @method makeContent fixasp_raster
# ##' @export
# makeContent.fixasp_raster <- function(x) {
#     ## reference https://stackoverflow.com/questions/58165226/is-it-possible-to-plot-images-in-a-ggplot2-plot-that-dont-get-distorted-when-y?noredirect=1#comment102713437_58165226
#     ## and https://github.com/GuangchuangYu/ggimage/issues/19#issuecomment-572523516
#     ## Convert from relative units to absolute units
#     children <- x$children
#     for (i in seq_along(children)) {
#         y <- children[[i]]
#         h <- convertHeight(y$height, "cm", valueOnly = TRUE)
#         w <- convertWidth(y$width, "cm", valueOnly = TRUE)
#         ## Decide how the units should be equal
#         ## y$width <- y$height <- unit(sqrt(h*w), "cm")
#
#         y$width <- unit(w, "cm")
#         y$height <- unit(h, "cm")
#         x$children[[i]] <- y
#     }
#     x
# }

##' @importFrom magick image_info
getAR2 <- function(magick_image) {
    info <- image_info(magick_image)
    info$width/info$height
}


compute_just <- getFromNamespace("compute_just", "ggplot2")


## @importFrom EBImage readImage
## @importFrom EBImage channel
## imageGrob2 <- function(x, y, size, img, by, colour, alpha) {
##     if (!is(img, "Image")) {
##         img <- readImage(img)
##         asp <- getAR(img)
##     }

##     unit <- "native"
##     if (any(size == Inf)) {
##         x <- 0.5
##         y <- 0.5
##         width <- 1
##         height <- 1
##         unit <- "npc"
##     } else if (by == "width") {
##         width <- size
##         height <- size/asp
##     } else {
##         width <- size * asp
##         height <- size
##     }

##     if (!is.null(colour)) {
##         color <- col2rgb(colour) / 255

##         img <- channel(img, 'rgb')
##         img[,,1] <- colour[1]
##         img[,,2] <- colour[2]
##         img[,,3] <- colour[3]
##     }

##     if (dim(img)[3] >= 4) {
##         img[,,4] <- img[,,4]*alpha
##     }

##     rasterGrob(x = x,
##                y = y,
##                image = img,
##                default.units = unit,
##                height = height,
##                width = width,
##                interpolate = FALSE)
## }


## getAR <- function(img) {
##     dims <- dim(img)[1:2]
##     dims[1]/dims[2]
## }


##################################################
##                                              ##
## another solution, but the speed is too slow  ##
##                                              ##
##################################################

## draw_key_image <- function(data, params, size) {
##     imageGrob(0.5, 0.5, image=data$image, size=data$size)
## }

## ##' @importFrom ggplot2 ggproto
## ##' @importFrom ggplot2 Geom
## ##' @importFrom ggplot2 aes
## ##' @importFrom ggplot2 draw_key_blank
## GeomImage <- ggproto("GeomImage", Geom,
##                      non_missing_aes = c("size", "image"),
##                      required_aes = c("x", "y"),
##                      default_aes = aes(size=0.05, image="https://www.r-project.org/logo/Rlogo.png"),
##                      draw_panel = function(data, panel_scales, coord, by, na.rm=FALSE) {
##                          data$image <- as.character(data$image)
##                          data <- coord$transform(data, panel_scales)
##                          imageGrob(data$x, data$y, data$image, data$size, by)
##                      },
##                      draw_key = draw_key_image
##                      )


## ##' @importFrom grid grob
## imageGrob <- function(x, y, image, size=0.05, by="width") {
##     grob(x=x, y=y, image=image, size=size, by=by, cl="image")
## }

## ##' @importFrom grid drawDetails
## ##' @importFrom grid grid.raster
## ##' @importFrom EBImage readImage
## ##' @method drawDetails image
## ##' @export
## drawDetails.image <- function(x, recording=FALSE) {
##     image_object <- lapply(x$image, readImage)
##     names(image_object) <- x$image
##     for (i in seq_along(x$image)) {
##         img <- image_object[[x$image[i]]]
##         size <- x$size[i]
##         by <- x$by
##         asp <- getAR(img)
##         if (is.na(size)) {
##             width <- NULL
##             height <- NULL
##         } else if (by == "width") {
##             width <- size
##             height <- size/asp
##         } else {
##             width <- size * asp
##             height <- size
##         }

##         grid.raster(x$x[i], x$y[i],
##                     width = width,
##                     height = height,
##                     image = img,
##                     interpolate=FALSE)
##     }
## }

## ##' @importFrom ggplot2 discrete_scale
## ##' @importFrom scales identity_pal
## ##' @importFrom ggplot2 ScaleDiscreteIdentity
## ##' @export
## scale_image <- function(..., guide = "legend") {
##   sc <- discrete_scale("image", "identity", identity_pal(), ..., guide = guide,
##                        super = ScaleDiscreteIdentity)

##   sc
## }
