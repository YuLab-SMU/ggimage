##' geom layer for visualizing image files with basic overlap avoidance
##'
##' `geom_image_repel()` draws images like [geom_image()], but moves images
##' that overlap each other apart before drawing them. The displacement is
##' computed offline (no network, no rendering, no `ggrepel` dependency) from
##' the bounding boxes of the images, so the result is deterministic: the same
##' plot always produces the same layout.
##'
##' @details
##' The algorithm works on the *centres* of the images, after they have been
##' nudged (`nudge_x`/`nudge_y`) and transformed by the coordinate system
##' (this is the space in which `width`/`height` are expressed, i.e. fractions
##' of the panel):
##' \enumerate{
##'   \item each image gets an axis aligned box of size
##'         `width + box.padding` by `height + box.padding`;
##'   \item for every pair of boxes that overlap on **both** axes, the two
##'         images are pushed apart along the allowed `direction`, by
##'         `force * overlap / 2` each, away from each other;
##'   \item step 2 is repeated at most `max.iter` times, stopping early once no
##'         pair overlaps. The residual overlap after `max.iter` iterations is
##'         about `(1 - force)^max.iter` of the initial one, so the default
##'         (`force = 0.1`, `max.iter = 100`) leaves essentially no overlap.
##' }
##' which keeps the result reproducible. For large inputs, a mutable uniform-grid
##' broad phase visits only boxes sharing one of the query box's cells; sparse
##' layouts therefore avoid scanning every pair. The grid costs `O(n + k)` per
##' sweep for `k` candidate checks (with `O(n^2)` worst-case dense behaviour),
##' while small inputs retain the full pair scan. The cleanup is hard-capped at
##' 256 sweeps so dense inputs cannot make cleanup grow with `n` beyond that
##' fixed number of sweeps.
##'
##' Images are pushed away from each other only, they are **not** kept inside
##' the panel, and images are never re-ordered or removed. Images with
##' `size = Inf` (e.g. the background image added by [geom_bgimage()]) and
##' images that cannot be loaded are ignored by the repulsion: they keep their
##' original position and do not push the other images around.
##'
##' The width of an image that is sized through `size`/`by` (instead of an
##' explicit `width`) is derived by `grid` from its height and from the
##' physical aspect of the panel. `geom_image_repel()` uses the aspect
##' reported by the coordinate system (`coord_fixed()`, exact) and assumes a
##' square panel otherwise, which over-estimates the width of a panel that is
##' wider than it is high. Pass explicit `width`/`height` when the exact box
##' matters.
##'
##' @title geom_image_repel
##' @param mapping aes mapping
##' @param data data
##' @param stat stat
##' @param position position
##' @param inherit.aes logical, whether inherit aes from ggplot()
##' @param na.rm logical, whether remove NA values
##' @param by one of 'width' or 'height'
##' @param nudge_x horizontal adjustment to nudge image
##' @param nudge_y vertical adjustment to nudge image
##' @param use_cache logical, whether to use image caching for better performance
##' @param width,height Image width and height in native panel units, see
##'   [geom_image()]. Only one of them keeps the aspect ratio of the image.
##' @param max.iter non-negative whole number of displacement iterations.
##'   `max.iter = 0` keeps the original coordinates (and `force = 0` does the
##'   same).
##' @param force non-negative number, the fraction of the current overlap that
##'   is resolved in one iteration (each image of an overlapping pair moves by
##'   `force * overlap / 2`). Larger values converge faster but produce bigger
##'   jumps.
##' @param box.padding non-negative number, extra space added around every
##'   image box, in the same units as `width`/`height`. Repelled images are
##'   kept at least `box.padding` apart.
##' @param direction one of 'both', 'x' or 'y', the axis (or axes) along which
##'   images are allowed to move. Overlap is always detected on both axes;
##'   `direction` only restricts the displacement, so `direction = "x"` slides
##'   overlapping images horizontally until they no longer overlap.
##' @param ... additional parameters
##' @return geom layer
##' @importFrom ggplot2 layer
##' @export
##' @examples
##' library("ggplot2")
##' library("ggimage")
##' d <- data.frame(x = c(0.5, 0.52, 0.51),
##'                 y = c(0.5, 0.51, 0.49),
##'                 image = system.file("extdata/Rlogo.png", package = "ggimage"))
##' ## overlapping images are pushed apart
##' ggplot(d, aes(x, y, image = image)) +
##'     geom_image_repel(width = 0.1, box.padding = 0.01)
##' ## max.iter = 0 keeps the original coordinates
##' ggplot(d, aes(x, y, image = image)) +
##'     geom_image_repel(width = 0.1, max.iter = 0)
##' ## only move them apart horizontally
##' ggplot(d, aes(x, y, image = image)) +
##'     geom_image_repel(width = 0.1, direction = "x")
##' @author Guangchuang Yu
geom_image_repel <- function(mapping=NULL, data=NULL, stat="identity",
                             position="identity", inherit.aes=TRUE,
                             na.rm=FALSE, by="width", nudge_x = 0, nudge_y = 0,
                             use_cache=TRUE, width=NULL, height=NULL,
                             max.iter=100, force=0.1,
                             box.padding=0, direction="both", ...) {

    by <- match.arg(by, c("width", "height"))
    max.iter <- check_repel_count(max.iter, "max.iter")
    force <- check_repel_number(force, "force")
    box.padding <- check_repel_number(box.padding, "box.padding")
    direction <- check_repel_direction(direction)

    params <- list(
        na.rm = na.rm,
        by = by,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        use_cache = use_cache,
        max.iter = max.iter,
        force = force,
        box.padding = box.padding,
        direction = direction,
        ...
    )
    if (!is.null(width)) params$width <- width
    if (!is.null(height)) params$height <- height

    layer(
        data=data,
        mapping=mapping,
        geom=GeomImageRepel,
        stat=stat,
        position=position,
        show.legend=NA,
        inherit.aes=inherit.aes,
        params = params,
        check.aes = FALSE
    )
}


GeomImageRepel <- ggproto("GeomImageRepel", GeomImage,
                          draw_panel = function(data, panel_params, coord, by,
                                                na.rm=FALSE, .fun = NULL,
                                                image_fun = NULL,
                                                hjust=0.5, nudge_x = 0, nudge_y = 0,
                                                asp=1, use_cache=TRUE,
                                                width = NULL, height = NULL,
                                                max.iter=100, force=0.1,
                                                box.padding=0, direction="both") {
                              max.iter <- check_repel_count(max.iter, "max.iter")
                              force <- check_repel_number(force, "force")
                              box.padding <- check_repel_number(box.padding, "box.padding")
                              direction <- check_repel_direction(direction)

                              data <- GeomImage$make_image_data(
                                  data, panel_params, coord, .fun, nudge_x, nudge_y
                              )
                              if (is.null(data) || nrow(data) == 0L) {
                                  return(zeroGrob())
                              }

                              data <- repel_image_data(
                                  data, panel_params, coord, by = by, asp = asp,
                                  width = width, height = height,
                                  image_fun = image_fun, use_cache = use_cache,
                                  max.iter = max.iter, force = force,
                                  box.padding = box.padding, direction = direction
                              )

                              GeomImage$draw_grobs(data, panel_params, coord, by,
                                                   image_fun, hjust, asp,
                                                   use_cache, width, height)
                          })


check_repel_number <- function(value, name, minimum = 0) {
    if (!(is.numeric(value) && length(value) == 1L &&
          !is.na(value) && is.finite(value) && value >= minimum)) {
        stop("`", name, "` must be a single finite number >= ", minimum,
             ", not ", paste(format(value), collapse = ", "), ".",
             call. = FALSE)
    }
    as.numeric(value)
}

check_repel_count <- function(value, name) {
    value <- check_repel_number(value, name)
    if (value != floor(value)) {
        stop("`", name, "` must be a whole number, not ", format(value), ".",
             call. = FALSE)
    }
    as.integer(value)
}

check_repel_direction <- function(direction) {
    supported <- c("both", "x", "y")
    if (!(is.character(direction) && length(direction) == 1L &&
          !is.na(direction) && direction %in% supported)) {
        stop("`direction` must be one of \"both\", \"x\" or \"y\", not ",
             paste(format(direction), collapse = ", "), ".", call. = FALSE)
    }
    direction
}


## physical aspect (height / width) of the panel; the width of an image drawn
## with an `NULL` width (i.e. sized through `size`) is derived by grid from the
## height in physical units, so it has to be converted back with this ratio.
## `coord_fixed()` (and only it) knows the ratio, a square panel is assumed
## otherwise, which over-estimates the width of a wide panel.
panel_aspect_ratio <- function(coord, panel_params) {
    ar <- tryCatch(coord$aspect(panel_params), error = function(e) NULL)
    if (is.null(ar) || length(ar) != 1L || !is.finite(ar) || ar <= 0) {
        return(1)
    }
    as.numeric(ar)
}

## bounding boxes (native panel units, `box.padding` included) of the images of
## an already transformed `data`, following the rules of `imageGrob()`.
## `keep` is FALSE for images that do not take part in the repulsion, i.e.
## images without a size (`size = Inf`) and images that cannot be loaded.
image_repel_boxes <- function(data, panel_params, coord, by = "width", asp = 1,
                              width = NULL, height = NULL, image_fun = NULL,
                              use_cache = TRUE, box.padding = 0) {
    n <- nrow(data)
    explicit_width <- resolve_image_dimension(
        if ("width" %in% names(data)) data$width else NULL, width, n
    )
    explicit_height <- resolve_image_dimension(
        if ("height" %in% names(data)) data$height else NULL, height, n
    )

    boxes <- data.frame(
        width = rep(NA_real_, n),
        height = rep(NA_real_, n),
        keep = rep(FALSE, n)
    )
    if (n == 0L) {
        return(boxes)
    }

    panel_ar <- panel_aspect_ratio(coord, panel_params)
    for (i in seq_len(n)) {
        img <- data$image[i]
        size <- data$size[i]
        ## `imageGrob()` draws no image (or a full panel background) for these
        if (is.na(img) || length(size) != 1L || is.na(size) ||
            !is.finite(size)) {
            next
        }

        w <- explicit_width[i]
        h <- explicit_height[i]
        if (!is.na(w) && !is.na(h)) {
            boxes$width[i] <- w + box.padding
            boxes$height[i] <- h + box.padding
            boxes$keep[i] <- TRUE
            next
        }

        ## the aspect ratio of the image is needed, both to complete an
        ## explicit dimension and to turn `size` into a box
        prepared <- prepare_image(img, colour = NULL, opacity = 1,
                                  angle = data$angle[i], image_fun = image_fun,
                                  use_cache = use_cache)
        if (is.null(prepared)) next
        ar <- getAR2(prepared)
        if (!is.finite(ar) || ar <= 0) next

        ## `imageGrob()` uses `ar / asp` as the aspect of the image
        ratio <- ar / asp
        if (!is.na(w)) {
            h <- w / ratio
        } else if (!is.na(h)) {
            w <- h * ratio
        } else {
            ## `size` gives the height of the image, the width is derived by
            ## grid from the height and the physical aspect of the panel
            h <- if (by == "width") size / ratio else size
            w <- h * ar * panel_ar
        }
        boxes$width[i] <- w + box.padding
        boxes$height[i] <- h + box.padding
        boxes$keep[i] <- TRUE
    }

    boxes
}


## A mutable uniform grid used as a conservative broad phase. The grid is
## rebuilt for each solver sweep and updated after every movement. Updating it
## while scanning j in row order is important: a movement made by an earlier
## pair can create a later overlap, just as it can in the historical full scan.
repel_grid_state <- function(x, y, width, height) {
    n <- length(x)
    positive_size <- c(width[is.finite(width) & width > 0],
                       height[is.finite(height) & height > 0])
    cell_size <- if (length(positive_size)) median(positive_size) else 1
    cell_size <- max(cell_size, .Machine$double.eps)

    ## Very large boxes can span an excessive number of cells. In that case a
    ## full scan is slower but bounded and preserves the exact solver path.
    span_x <- width / cell_size + 1
    span_y <- height / cell_size + 1
    coverage <- span_x * span_y
    if (any(!is.finite(coverage)) || any(span_x > 64 | span_y > 64) ||
        sum(coverage) > max(4096, n * 64)) {
        return(NULL)
    }

    cells <- new.env(hash = TRUE, parent = emptyenv())
    state <- new.env(parent = emptyenv())
    state$members <- vector("list", n)
    state$x <- x
    state$y <- y

    cell_range <- function(lo, hi) {
        seq.int(floor(lo / cell_size), floor(hi / cell_size))
    }
    box_keys <- function(i) {
        x_cells <- cell_range(state$x[i] - width[i] / 2,
                              state$x[i] + width[i] / 2)
        y_cells <- cell_range(state$y[i] - height[i] / 2,
                              state$y[i] + height[i] / 2)
        as.vector(outer(x_cells, y_cells,
                        FUN = function(a, b) paste(a, b, sep = ":")))
    }
    insert <- function(i) {
        keys <- box_keys(i)
        state$members[[i]] <- keys
        for (key in keys) {
            ids <- if (exists(key, cells, inherits = FALSE)) cells[[key]] else integer()
            cells[[key]] <- c(ids, i)
        }
    }
    remove <- function(i) {
        for (key in state$members[[i]]) {
            ids <- cells[[key]]
            keep <- ids != i
            if (any(keep)) cells[[key]] <- ids[keep]
            else rm(list = key, envir = cells)
        }
        state$members[[i]] <- character()
    }
    update <- function(i, new_x, new_y) {
        remove(i)
        state$x[i] <- new_x
        state$y[i] <- new_y
        insert(i)
    }
    query <- function(i) {
        keys <- box_keys(i)
        ids <- unlist(lapply(keys, function(key) {
            if (exists(key, cells, inherits = FALSE)) cells[[key]] else integer()
        }), use.names = FALSE)
        if (!length(ids)) return(integer())
        sort(unique(ids))
    }

    for (i in seq_len(n)) insert(i)
    list(query = query, update = update)
}

## One sequential solver sweep. The full nested loops remain the reference path
## for small inputs; the grid path only replaces checks that cannot overlap on
## either axis and otherwise visits pairs in exactly the same row order.
repel_boxes_sweep <- function(x, y, width, height, move_x, move_y, force,
                             broad.phase, clearance = 0) {
    n <- length(x)
    overlapping <- FALSE
    max_overlap <- 0
    use_grid <- isTRUE(broad.phase) && n > 64L &&
        all(is.finite(x)) && all(is.finite(y)) &&
        all(is.finite(width)) && all(is.finite(height))
    grid <- if (use_grid) repel_grid_state(x, y, width, height) else NULL

    for (i in seq_len(n - 1L)) {
        if (is.null(grid)) {
            candidates <- (i + 1L):n
        } else {
            candidates <- grid$query(i)
            candidates <- candidates[candidates > i]
        }
        if (!length(candidates)) next

        ## Query again after every candidate: movement of i or j updates the
        ## grid and can make a later pair enter the candidate set.
        next_j <- i + 1L
        repeat {
            if (!is.null(grid)) {
                candidates <- grid$query(i)
                candidates <- candidates[candidates >= next_j]
                if (!length(candidates)) break
                j <- candidates[1L]
            } else {
                j <- next_j
                if (j > n) break
            }
            next_j <- j + 1L

            ox <- (width[i] + width[j]) / 2 - abs(x[j] - x[i])
            oy <- (height[i] + height[j]) / 2 - abs(y[j] - y[i])
            if (!(ox > 0 && oy > 0)) next
            overlapping <- TRUE
            max_overlap <- max(max_overlap, min(ox, oy))
            sx <- if (x[j] - x[i] == 0) 1 else sign(x[j] - x[i])
            sy <- if (y[j] - y[i] == 0) 1 else sign(y[j] - y[i])

            if (move_x) {
                shift <- force * ox / 2 + clearance
                x[i] <- x[i] - shift * sx
                x[j] <- x[j] + shift * sx
            }
            if (move_y) {
                shift <- force * oy / 2 + clearance
                y[i] <- y[i] - shift * sy
                y[j] <- y[j] + shift * sy
            }
            if (!is.null(grid)) {
                grid$update(i, x[i], y[i])
                grid$update(j, x[j], y[j])
            }
        }
    }
    list(x = x, y = y, overlapping = overlapping, max_overlap = max_overlap)
}


## Deterministic pairwise solver kept isolated for direct geometry testing.
repel_boxes <- function(x, y, width, height, max.iter = 100L, force = 0.1,
                        direction = "both", broad.phase = TRUE) {
    n <- length(x)
    if (n < 2L || max.iter <= 0L || force <= 0) {
        return(list(x = x, y = y))
    }

    move_x <- direction %in% c("both", "x")
    move_y <- direction %in% c("both", "y")
    overlapping <- FALSE

    for (iter in seq_len(max.iter)) {
        sweep <- repel_boxes_sweep(
            x, y, width, height, move_x, move_y, force, broad.phase
        )
        x <- sweep$x
        y <- sweep$y
        overlapping <- sweep$overlapping
        if (!overlapping) break
    }

    ## Finish long runs with a small positive clearance. Short runs preserve
    ## the force-dependent partial update used to tune the layout. The cleanup
    ## is deliberately capped: a dense cluster can otherwise spend O(n^3)
    ## time in the old n-scaled sweep even after the force iterations have
    ## already converged. A fixed cap keeps the worst case deterministic while
    ## retaining the existing pair order (and therefore the small-layout
    ## behaviour).
    if (max.iter >= 10L && overlapping) {
        clearance <- 1e-4
        overlap_tol <- 1e-12
        cleanup_limit <- 256L
        for (cleanup in seq_len(cleanup_limit)) {
            sweep <- repel_boxes_sweep(
                x, y, width, height, move_x, move_y, 1, broad.phase,
                clearance = clearance
            )
            x <- sweep$x
            y <- sweep$y
            if (!sweep$overlapping || sweep$max_overlap <= overlap_tol) break
        }
    }

    list(x = x, y = y)
}

repel_image_data <- function(data, panel_params, coord, by = "width", asp = 1,
                             width = NULL, height = NULL, image_fun = NULL,
                             use_cache = TRUE, max.iter = 100L, force = 0.1,
                             box.padding = 0, direction = "both") {
    if (is.null(data) || nrow(data) == 0L) {
        return(data)
    }

    boxes <- image_repel_boxes(data, panel_params, coord, by = by, asp = asp,
                               width = width, height = height,
                               image_fun = image_fun, use_cache = use_cache,
                               box.padding = box.padding)
    idx <- which(boxes$keep)

    ## Keep the drawn boxes observable and consistent with the boxes used by
    ## the solver. For size/by rows this materializes the aspect-derived width
    ## that grid would otherwise leave as a NULL width unit.
    if (!"width" %in% names(data)) data$width <- NA_real_
    if (!"height" %in% names(data)) data$height <- NA_real_
    data$width[idx] <- boxes$width[idx] - box.padding
    data$height[idx] <- boxes$height[idx] - box.padding

    if (length(idx) < 2L) {
        return(data)
    }

    moved <- repel_boxes(x = data$x[idx], y = data$y[idx],
                         width = boxes$width[idx], height = boxes$height[idx],
                         max.iter = max.iter, force = force,
                         direction = direction)
    data$x[idx] <- moved$x
    data$y[idx] <- moved$y
    data
}
