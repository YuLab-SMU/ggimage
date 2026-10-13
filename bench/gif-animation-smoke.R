#!/usr/bin/env Rscript

# Integration smoke test: frame-data-driven GIF animation with gganimate.
#
# Validates the sequence documented in the accompanying proposal/vignette:
#   deterministic source GIF -> per-frame PNGs -> one-row-per-(glyph, frame)
#   data frame -> static layer + geom_image() + transition_manual() -> animate()
#
# This script is a validation harness only. It deliberately makes no changes to
# the package: bench/ is listed in .Rbuildignore, so nothing here reaches
# R CMD check.
#
# Environment notes (this machine):
#   * ~/.Rprofile puts a stale ggimage 0.3.6 first on .libPaths(). We therefore
#     never call library(ggimage); we load the source tree with pkgload::load_all()
#     so the code under test is unambiguously the checkout.
#   * The renderer is gganimate::magick_renderer(), the choice the proposal
#     documents. That holds whether or not the optional gifski/av renderers
#     happen to be installed: the script never relies on the default renderer.
#   * Rendering is headless; no X server is required.
#
# Offline: no network access, no downloaded fixtures. Every input is generated
# in tempdir() at run time and removed on exit.
#
# Exit status: 0 only when every check passes; 1 as soon as any check fails.

suppressWarnings(suppressMessages({
    library(ggplot2)
    library(magick)
    library(gganimate)
}))

pkg_root <- "/home/wang/data/source/visualization/ggimage"

## ---------------------------------------------------------------------------
## Check bookkeeping: one PASS/FAIL line per check, non-zero exit on failure.
## ---------------------------------------------------------------------------

.results <- new.env(parent = emptyenv())
.results$passed <- 0L
.results$failed <- 0L
.results$lines <- character(0)

check <- function(label, ok, detail = NULL) {
    ok <- isTRUE(ok)
    if (ok) {
        .results$passed <- .results$passed + 1L
    } else {
        .results$failed <- .results$failed + 1L
    }
    status <- if (ok) "PASS" else "FAIL"
    line <- sprintf("%s: %s", status, label)
    if (!is.null(detail) && nzchar(detail)) {
        line <- sprintf("%s [%s]", line, detail)
    }
    .results$lines <- c(.results$lines, line)
    cat(line, "\n", sep = "")
    invisible(ok)
}

## Run an expression, reporting a FAIL (rather than aborting the script) when it
## errors. Real workflow failures are recorded verbatim for the final report.
## The expression's own value is discarded so it cannot leak into the log, and
## the result is returned invisibly so top-level calls print nothing.
guard <- function(label, expr) {
    value <- tryCatch(
        force(expr),
        error = function(e) {
            check(label, FALSE, paste0("error: ", conditionMessage(e)))
            NULL
        }
    )
    invisible(value)
}

## ---------------------------------------------------------------------------
## Pixel helpers.
##
## magick::image_data(x, channels = "rgb") returns an array laid out as
## [channel, x, y] for a single frame. Subsetting with drop = FALSE and
## reshaping yields an ordinary [x, y] matrix, which is what the probes below
## assert against. Probes are intentionally coarse: they test for *saturated*
## colour in a region rather than exact hex values, so they tolerate
## antialiasing and renderer differences.
## ---------------------------------------------------------------------------

channel_matrix <- function(img, channel) {
    d <- magick::image_data(img, channels = "rgb")
    slice <- d[channel, , , drop = FALSE]
    matrix(as.integer(slice), nrow = dim(slice)[2], ncol = dim(slice)[3])
}

## Saturation = max(r,g,b) - min(r,g,b). Glyph pixels come from the solid-colour
## source frames and are strongly saturated; the white background and the
## grey/dark band are not. This distinguishes "a glyph is here" from
## "background is here" without hard-coding any particular colour.
sat_matrix <- function(img) {
    r <- channel_matrix(img, 1L)
    g <- channel_matrix(img, 2L)
    b <- channel_matrix(img, 3L)
    (pmax(r, g, b) - pmin(r, g, b)) > 100L
}

## Mean colour of the saturated pixels inside a pixel-space window, plus how
## many there were. Returns NULL when the window holds no saturated pixel.
region_color <- function(img, x0, x1, y0, y1) {
    r <- channel_matrix(img, 1L)
    g <- channel_matrix(img, 2L)
    b <- channel_matrix(img, 3L)
    rr <- r[x0:x1, y0:y1, drop = FALSE]
    gg <- g[x0:x1, y0:y1, drop = FALSE]
    bb <- b[x0:x1, y0:y1, drop = FALSE]
    sat <- (pmax(rr, gg, bb) - pmin(rr, gg, bb)) > 100L
    n <- sum(sat)
    if (n == 0L) {
        return(NULL)
    }
    list(n = n,
         r = mean(rr[sat]),
         g = mean(gg[sat]),
         b = mean(bb[sat]))
}

## Classify a mean colour into a coarse hue name, so the checks can assert
## "this frame shows its own source colour" without pinning exact channel
## values (the renderer may rescale, e.g. green arrives as (0,128,0)).
hue_name <- function(col) {
    if (is.null(col)) {
        return(NA_character_)
    }
    r <- col$r
    g <- col$g
    b <- col$b
    if (r > 150 && g > 150 && b < 120) return("yellow")
    if (r < 120 && g > 90 && b < 120) return("green")
    if (r < 120 && g < 120 && b > 150) return("blue")
    if (r > 150 && g < 120 && b < 120) return("red")
    if (r > 200 && g > 200 && b > 200) return("white")
    if (abs(r - g) < 40 && abs(g - b) < 40) return("grey")
    "other"
}

## ---------------------------------------------------------------------------
## Setup: honest library path, then the workflow.
## ---------------------------------------------------------------------------

cat("# ggimage frame-data-driven GIF animation smoke test\n")
cat("# package root: ", pkg_root, "\n", sep = "")
cat("# R: ", R.version.string, "\n", sep = "")

if (!requireNamespace("pkgload", quietly = TRUE)) {
    cat("FAIL: setup: pkgload is not installed\n")
    quit(status = 1L)
}

## Load the checkout, never the stale 0.3.6 that ~/.Rprofile puts first.
load_error <- NULL
tryCatch(
    suppressWarnings(suppressMessages(
        pkgload::load_all(pkg_root, quiet = TRUE)
    )),
    error = function(e) load_error <<- conditionMessage(e)
)

if (!is.null(load_error)) {
    cat("FAIL: setup: pkgload::load_all() failed [", load_error, "]\n", sep = "")
    cat("\n=== SUMMARY ===\n")
    cat(sprintf("checks failed: 1 (load error)\n"))
    quit(status = 1L)
}

loaded_path <- find.package("ggimage")
loaded_version <- as.character(utils::packageVersion("ggimage"))
cat("# loaded ggimage ", loaded_version, " from ", loaded_path, "\n", sep = "")

## The stale copy must not be what we are testing.
stale <- grepl("/home/wang/Rlib/", loaded_path, fixed = TRUE)
check("setup: ggimage loaded from the checkout, not the stale library",
      !stale && identical(normalizePath(loaded_path), normalizePath(pkg_root, mustWork = FALSE)),
      paste0("path=", loaded_path, " version=", loaded_version))

## The proposal requires these; assert them explicitly so a missing optional
## dependency is reported as a clear FAIL rather than a confusing render error.
for (pkg in c("magick", "gganimate", "transformr", "tweenr")) {
    ok <- requireNamespace(pkg, quietly = TRUE)
    check(paste0("setup: dependency available: ", pkg), ok)
}
check("setup: magick_renderer() is available (the proposal's documented renderer)",
      requireNamespace("magick", quietly = TRUE) &&
          exists("magick_renderer", envir = asNamespace("gganimate"), inherits = FALSE),
      "gganimate::magick_renderer() needs magick")

## ---------------------------------------------------------------------------
## Step 1: deterministic 4-frame source GIF.
## ---------------------------------------------------------------------------

work_dir <- file.path(tempdir(), "ggimage-gif-smoke")
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

src_colors <- c("red", "green", "blue", "yellow")
src_hues <- c("red", "green", "blue", "yellow")
gif_path <- file.path(work_dir, "source.gif")

build_gif <- function() {
    frames <- lapply(src_colors, function(cl) magick::image_blank(120, 120, color = cl))
    magick::image_write(magick::image_join(frames), gif_path, format = "gif")
    gif_path
}
invisible(guard("step1: build the 4-frame source GIF", build_gif()))

check("step1: source GIF exists and is non-empty",
      file.exists(gif_path) && file.info(gif_path)$size > 0,
      paste0("bytes=", if (file.exists(gif_path)) file.info(gif_path)$size else NA))

src <- magick::image_read(gif_path)
check("step1: source GIF reads back as 4 frames",
      length(src) == 4L, paste0("nframes=", length(src)))

## ---------------------------------------------------------------------------
## Step 2: materialize the still frames as real PNG files.
##
## NOTE: magick::image_write() infers the output format from the *image object*,
## not from the filename extension. Writing a GIF frame to "frame1.png" without
## format = "png" therefore produces GIF bytes inside a .png file. We pass
## format = "png" explicitly and verify the PNG magic number below.
## ---------------------------------------------------------------------------

frame_pngs <- file.path(work_dir, sprintf("frame%d.png", 1:4))

write_pngs <- function() {
    for (i in 1:4) {
        magick::image_write(src[i], frame_pngs[i], format = "png")
    }
    frame_pngs
}
invisible(guard("step2: materialize per-frame PNG files", write_pngs()))

png_magic_ok <- vapply(frame_pngs, function(p) {
    if (!file.exists(p)) {
        return(FALSE)
    }
    identical(readBin(p, "raw", n = 4L), as.raw(c(0x89, 0x50, 0x4E, 0x47)))
}, logical(1))

check("step2: four frame PNGs exist and carry real PNG magic bytes",
      length(frame_pngs) == 4L && all(file.exists(frame_pngs)) && all(png_magic_ok),
      paste0("png=", paste(basename(frame_pngs[file.exists(frame_pngs)]), collapse = ",")))

## ---------------------------------------------------------------------------
## Step 3: the frame table -- one row per (glyph, frame) = 8 rows.
##
## Glyph A sits at a fixed position and carries the frame-varying image, so it
## is the witness that transition_manual() maps state level f to output frame f.
## Glyph B holds a constant image and varies x across frames, so its position is
## the witness that per-(glyph, frame) rows behave as intended.
## ---------------------------------------------------------------------------

glyph_x <- c(rep(2.0, 4), 1.2, 2.6, 4.0, 5.4)
glyph_y <- c(rep(0.80, 4), rep(0.52, 4))

frame_tbl <- data.frame(
    glyph = rep(c("A", "B"), each = 4),
    x = glyph_x,
    y = glyph_y,
    frame = rep(1:4, 2),
    image = c(frame_pngs, frame_pngs),
    stringsAsFactors = FALSE
)

check("step3: frame table has 8 rows (2 glyphs x 4 frames)",
      nrow(frame_tbl) == 8L &&
          identical(sort(unique(frame_tbl$frame)), 1:4) &&
          identical(sort(unique(frame_tbl$glyph)), c("A", "B")),
      paste0("rows=", nrow(frame_tbl)))

check("step3: image column holds ordinary character paths",
      is.character(frame_tbl$image) && all(file.exists(frame_tbl$image)))

## ---------------------------------------------------------------------------
## Step 4: build the plot.
##
## The static layer's data frame deliberately has NO frame column, so it must be
## repeated across every rendered frame (checked in (c) below).
## ---------------------------------------------------------------------------

static_layer <- geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.35),
    fill = "grey15",
    show.legend = FALSE
)

build_plot <- function() {
    ggplot() +
        static_layer +
        ggimage::geom_image(
            data = frame_tbl,
            aes(x = x, y = y, image = image),
            size = 0.18,
            inherit.aes = FALSE
        ) +
        xlim(0.5, 6) +
        ylim(0, 1) +
        theme_void() +
        gganimate::transition_manual(frame)
}

p <- guard("step4: build the ggplot object", build_plot())
check("step4: plot builds and carries a gganim transition",
      inherits(p, "gg") && inherits(p, "gganim"))

## ---------------------------------------------------------------------------
## Step 5: render with the public magick renderer.
##
## Output path (check g): gganimate exposes a documented save helper for exactly
## this case. `gganimate::animate()` itself has no path argument
## (formals(animate) is `plot, ...`), and `gifski_renderer(file = )` is the only
## renderer with a `file` argument -- but tying the check to one renderer would
## make it break when the optional gifski happens to be installed or absent.
## Instead, animate() with magick_renderer() returns a `magick-image` object, and
## gganimate registers a public S3 method `save_animation.magick-image()`
## (reachable through the exported `anim_save(filename, animation)`) that calls
## magick::image_write(animation, file). That is the documented, public route to
## an explicit non-default output path, and it is what we use and verify.
## ---------------------------------------------------------------------------

out_gif <- file.path(work_dir, "rendered.gif")
n_frames <- 4L
fps <- 4L
render_w <- 400L
render_h <- 400L

render <- function() {
    gganimate::animate(
        p,
        nframes = n_frames,
        fps = fps,
        renderer = gganimate::magick_renderer(),
        width = render_w,
        height = render_h
    )
}

anim <- guard("step5: animate() renders with magick_renderer()", render())

if (is.null(anim)) {
    cat("\n# animate() failed; skipping dependent checks\n")
    check("step5: animate() returned an animation object", FALSE)
} else {
    check("step5: animate() returned an object with 4 frames",
          length(anim) == 4L, paste0("nframes=", length(anim)))
}

## ---------------------------------------------------------------------------
## Check (g): write to an explicit non-default output path via public API.
## ---------------------------------------------------------------------------

if (!is.null(anim)) {
    save_ok <- TRUE
    save_err <- NULL
    tryCatch(
        gganimate::anim_save(out_gif, anim),
        error = function(e) {
            save_ok <<- FALSE
            save_err <<- conditionMessage(e)
        }
    )

    check("g) anim_save() writes the animation to an explicit non-default path",
          save_ok && file.exists(out_gif) && file.info(out_gif)$size > 0,
          if (save_ok) {
              paste0("path=", basename(out_gif), " bytes=", file.info(out_gif)$size)
          } else {
              paste0("error: ", save_err)
          })

    check("g) the explicitly written file is a readable multi-frame GIF",
          save_ok && file.exists(out_gif) && length(magick::image_read(out_gif)) == 4L,
          if (save_ok && file.exists(out_gif)) {
              paste0("nframes=", length(magick::image_read(out_gif)))
          } else {
              "not written"
          })
}

## ---------------------------------------------------------------------------
## Remaining checks all inspect the rendered animation.
## ---------------------------------------------------------------------------

if (is.null(anim)) {
    check("a) rendered animation has >= 3 frames", FALSE, "animate() failed")
    check("b) frame mapping", FALSE, "animate() failed")
    check("c) static layer in every frame", FALSE, "animate() failed")
    check("d) glyph B position varies", FALSE, "animate() failed")
    check("f) preprocessed frame PNGs survive the render", FALSE, "animate() failed")
} else {

    ## --- (a) at least 3 frames, and at least two differ in pixel data --------
    n_rendered <- length(anim)
    distinct_frames <- 0L
    if (n_rendered >= 2L) {
        for (i in 2:n_rendered) {
            d1 <- magick::image_data(anim[i - 1L], channels = "rgb")
            d2 <- magick::image_data(anim[i], channels = "rgb")
            if (!identical(as.integer(d1), as.integer(d2))) {
                distinct_frames <- distinct_frames + 1L
            }
        }
    }
    check("a) animation has >= 3 frames",
          n_rendered >= 3L, paste0("nframes=", n_rendered))
    check("a) consecutive frames differ in pixel data",
          distinct_frames >= 2L,
          paste0("differing adjacent pairs=", distinct_frames))

    ## --- (b) frame mapping: frame f shows its own source colour in A --------
    ## Glyph A is fixed at the top-left; the probe window sits inside it and
    ## clear of glyph B's band (which lives lower down, and to the right).
    a_x0 <- 90L; a_x1 <- 130L; a_y0 <- 60L; a_y1 <- 110L

    observed_hues <- character(n_rendered)
    for (f in seq_len(n_rendered)) {
        col <- region_color(anim[f], a_x0, a_x1, a_y0, a_y1)
        observed_hues[f] <- if (is.null(col)) "none" else hue_name(col)
    }

    mapping_ok <- n_rendered == 4L &&
        identical(observed_hues, src_hues)

    check("b) each rendered frame shows its own source colour in glyph A's region",
          mapping_ok,
          paste0("expected=", paste(src_hues, collapse = ">"),
                 " observed=", paste(observed_hues, collapse = ">")))

    ## --- (c) static layer present in EVERY frame ---------------------------
    ## The band occupies the bottom strip of the canvas. The static layer has no
    ## frame column, so it must survive into all four output frames.
    band_results <- logical(n_rendered)
    for (f in seq_len(n_rendered)) {
        r <- channel_matrix(anim[f], 1L)
        g <- channel_matrix(anim[f], 2L)
        b <- channel_matrix(anim[f], 3L)
        ## Bottom strip in relative terms: the lower ~30% of the canvas.
        y0 <- max(1L, floor(nrow(r) * 0.70))
        y1 <- nrow(r)
        rr <- r[, y0:y1, drop = FALSE]
        gg <- g[, y0:y1, drop = FALSE]
        bb <- b[, y0:y1, drop = FALSE]
        dark_nongrey <- sum(rr < 80L & gg < 80L & bb < 80L)
        band_results[f] <- dark_nongrey > 0L
    }
    check("c) static layer present (dark band) in every rendered frame",
          all(band_results),
          paste0("frames_with_band=", sum(band_results), "/", n_rendered))

    ## --- (d) glyph B moves left -> right -----------------------------------
    ## Probe two windows in glyph B's y band (below glyph A, which stops at
    ## y ~ 115). Early frame: colour on the LEFT, none on the RIGHT. Late frame:
    ## the reverse. Colours are compared against the *source* colour that glyph B
    ## carries, to prove the moving glyph is B and not a stray artefact.
    b_y0 <- 150L; b_y1 <- 215L
    left_x0 <- 85L;  left_x1 <- 125L
    right_x0 <- 330L; right_x1 <- 378L

    early <- region_color(anim[1], left_x0, left_x1, b_y0, b_y1)
    early_right <- region_color(anim[1], right_x0, right_x1, b_y0, b_y1)
    late <- region_color(anim[4], left_x0, left_x1, b_y0, b_y1)
    late_right <- region_color(anim[4], right_x0, right_x1, b_y0, b_y1)

    early_hue <- hue_name(early)
    late_hue <- hue_name(late_right)

    moved_ok <- !is.null(early) && is.null(early_right) &&
        !is.null(late_right) && is.null(late) &&
        identical(early_hue, src_hues[1]) &&
        identical(late_hue, src_hues[4])

    check("d) glyph B: early frame coloured on the LEFT, absent on the RIGHT",
          !is.null(early) && is.null(early_right),
          paste0("left=", early_hue, " right=", if (is.null(early_right)) "none" else hue_name(early_right)))
    check("d) glyph B: late frame coloured on the RIGHT, absent on the LEFT",
          !is.null(late_right) && is.null(late),
          paste0("left=", if (is.null(late)) "none" else late_hue, " right=", late_hue))
    check("d) glyph B's colour matches the expected source colour in both probes",
          moved_ok,
          paste0("early_left=", early_hue, " late_right=", late_hue))

    ## --- (e) static baseline: a plain plot does not autoplay ---------------
    ## An ordinary static ggplot + geom_image() with the GIF as the image input
    ## must draw exactly one raster, and that raster must be the GIF's first
    ## frame. This proves the animation is opt-in, not a property of geom_image.
    static_grob <- tryCatch({
        gs <- ggplot() +
            ggimage::geom_image(
                data = data.frame(x = 1, y = 1, image = gif_path),
                aes(x = x, y = y, image = image),
                size = 0.4,
                inherit.aes = FALSE
            ) +
            xlim(0, 2) + ylim(0, 2) + theme_void()
        gb <- ggplot2::ggplot_build(gs)
        gb
    }, error = function(e) {
        check("e) static ggplot + geom_image() builds", FALSE,
              paste0("error: ", conditionMessage(e)))
        NULL
    })

    if (!is.null(static_grob)) {
        check("e) static ggplot + geom_image() builds", TRUE)

        ## Draw it to a raster device and inspect the result.
        static_png <- file.path(work_dir, "static.png")
        draw_ok <- tryCatch({
            grDevices::png(static_png, width = 300, height = 300)
            print(static_grob$plot)
            grDevices::dev.off()
            TRUE
        }, error = function(e) {
            while (grDevices::dev.cur() > 1L) grDevices::dev.off()
            FALSE
        })

        check("e) static plot draws to a raster device", draw_ok && file.exists(static_png))

        if (draw_ok && file.exists(static_png)) {
            static_img <- magick::image_read(static_png)
            check("e) static plot is a single (non-animated) raster",
                  length(static_img) == 1L,
                  paste0("nframes=", length(static_img)))

            ## The drawn dot must be the GIF's FIRST frame (red), not a later
            ## one -- i.e. a plain plot shows frame 1 and does not autoplay.
            d <- magick::image_data(static_img, channels = "rgb")
            r <- matrix(as.integer(d[1, , , drop = FALSE]),
                        nrow = dim(d)[2], ncol = dim(d)[3])
            g <- matrix(as.integer(d[2, , , drop = FALSE]),
                        nrow = dim(d)[2], ncol = dim(d)[3])
            b <- matrix(as.integer(d[3, , , drop = FALSE]),
                        nrow = dim(d)[2], ncol = dim(d)[3])
            sat <- (pmax(r, g, b) - pmin(r, g, b)) > 100L
            n_sat <- sum(sat)
            first_hue <- if (n_sat > 0L) {
                hue_name(list(n = n_sat,
                              r = mean(r[sat]), g = mean(g[sat]), b = mean(b[sat])))
            } else {
                "none"
            }
            check("e) the single drawn raster is the GIF's first frame (red)",
                  n_sat > 0L && identical(first_hue, src_hues[1]),
                  paste0("hue=", first_hue, " pixels=", n_sat))
        }
    }

    ## --- (f) preprocessed PNGs survive the render --------------------------
    check("f) all preprocessed frame PNGs still exist after the render",
          length(frame_pngs) == 4L && all(file.exists(frame_pngs)),
          paste0("present=", sum(file.exists(frame_pngs)), "/4"))
}

## ---------------------------------------------------------------------------
## Summary.
## ---------------------------------------------------------------------------

cat("\n=== SUMMARY ===\n")
cat(sprintf("checks passed: %d\n", .results$passed))
cat(sprintf("checks failed: %d\n", .results$failed))
cat(sprintf("total:         %d\n", .results$passed + .results$failed))

if (.results$failed > 0L) {
    cat("\nFailed checks:\n")
    for (ln in .results$lines[startsWith(.results$lines, "FAIL")]) {
        cat("  ", ln, "\n", sep = "")
    }
    cat("\nRESULT: FAIL\n")
    quit(status = 1L)
} else {
    cat("\nRESULT: PASS\n")
    quit(status = 0L)
}
