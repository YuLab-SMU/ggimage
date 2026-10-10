## Issue #30: an `.eps` came out grainy because the vector formats were read at
## ImageMagick's 72 dpi default.  A `.pdf` was no better off: it went through
## `magick::image_read_pdf()`, a wrapper over pdftools, which is only a magick
## *Suggests*, so the format failed outright wherever pdftools is absent.
write_vector <- function(ext, width = 2, height = 1) {
    path <- tempfile(fileext = paste0(".", ext))
    if (identical(ext, "pdf")) {
        grDevices::pdf(path, width = width, height = height)
    } else {
        grDevices::postscript(path, width = width, height = height, pointsize = 12,
                              horizontal = FALSE, onefile = FALSE)
    }
    ## a small page cannot hold the default margins
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    grDevices::dev.off()
    path
}

## A format can be unreadable on a machine that has no delegate for it, so skip
## in that case only.  Testing the plain reader first keeps a real failure
## inside the dispatcher reportable instead of silently skipped.
read_or_skip <- function(path) {
    skip_if(is.null(tryCatch(magick::image_read(path), error = function(e) NULL)),
            "no rasteriser for this format on this machine")
    ggimage:::.read_image_file(path)
}

test_that("vector images are read at a resolution that keeps them sharp", {
    skip_if_not_installed("magick")

    img <- read_or_skip(write_vector("eps"))
    info <- magick::image_info(img)

    ## two inches at 300 dpi; at the old 72 dpi default this was 144 px
    expect_equal(info$width, 600, tolerance = 0.05)
    expect_equal(as.numeric(sub("x.*$", "", info$density)), 300)
})

test_that("the extension is matched case-insensitively", {
    skip_if_not_installed("magick")

    eps <- write_vector("eps")
    lower <- read_or_skip(eps)

    upper <- tempfile(fileext = ".EPS")
    file.copy(eps, upper, overwrite = TRUE)
    upper_info <- magick::image_info(ggimage:::.read_image_file(upper))

    ## `.EPS` used to miss the vector branches and fall back to 72 dpi
    expect_equal(as.numeric(sub("x.*$", "", upper_info$density)), 300)
    expect_equal(upper_info$width, magick::image_info(lower)$width)
})

test_that("a PDF is read at 300 dpi without pdftools", {
    skip_if_not_installed("magick")

    pdf <- write_vector("pdf")

    ## Skip only where the machine has no PDF rasteriser at all -- not merely
    ## because the dispatcher failed, which is the bug this test guards.
    reference <- tryCatch(magick::image_read(pdf, density = 300), error = function(e) NULL)
    skip_if(is.null(reference), "no PDF rasteriser on this machine")

    ## Record the failure mode of the reader the dispatcher used before: it
    ## needs pdftools, which magick only suggests.
    old <- tryCatch(magick::image_read_pdf(pdf), error = function(e) e)
    if (inherits(old, "error")) {
        expect_match(conditionMessage(old), "pdftools")
    }

    ## Catch the dispatcher's error by hand.  testthat turns an uncaught
    ## missing-package error into a *skip*, which would hide exactly the
    ## regression under test, so report it as a failure instead.
    dispatched <- tryCatch(ggimage:::.read_image_file(pdf), error = function(e) e)
    if (inherits(dispatched, "error")) {
        fail(paste("the dispatcher could not read a PDF:", conditionMessage(dispatched)))
    } else {
        info <- magick::image_info(dispatched)
        expect_equal(info$width, 600, tolerance = 0.05)
        expect_equal(as.numeric(sub("x.*$", "", info$density)), 300)
    }
})

test_that("raster images are read unchanged", {
    skip_if_not_installed("magick")

    png <- system.file("extdata", "Rlogo.png", package = "ggimage")
    expect_equal(
        magick::image_info(ggimage:::.read_image_file(png))$width,
        magick::image_info(magick::image_read(png))$width
    )
})
