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

test_that("an SVG is read on a machine without rsvg", {
    skip_if_not_installed("magick")

    svg <- tempfile(fileext = ".svg")
    writeLines(c('<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
                 '<rect width="10" height="10" fill="#00ff00"/></svg>'), svg)

    ## skip only where ImageMagick itself has no SVG delegate at all.  The
    ## plain read warns about the coarse renderer on builds without the
    ## librsvg delegate -- that is the point of this test, not a failure.
    reference <- tryCatch(suppressWarnings(magick::image_read(svg)),
                          error = function(e) NULL)
    skip_if(is.null(reference), "no SVG rasteriser on this machine")

    ## the condition `R CMD check --as-cran` creates: rsvg is hidden from the
    ## checked package because it is not a declared dependency.  Before the
    ## fix the dispatcher called magick::image_read_svg() unconditionally,
    ## which errored there and the image drew as nothing.  (image_read() warns
    ## about the coarser renderer on builds without the librsvg delegate; the
    ## warning is for the user, who can install rsvg, not a test failure.)
    ##
    ## Which renderer ran is observed through the fallback binding, not
    ## through output metadata: ImageMagick builds disagree about the density
    ## they tag an SVG render with (300 where the internal rasteriser honours
    ## the request, 118 on Debian's build via the librsvg delegate), so a
    ## density assertion would fail on some platforms against correct code.
    fell_back <- FALSE
    real_fallback <- ggimage:::.read_svg_without_rsvg
    local_mocked_bindings(
        .has_rsvg = function() FALSE,
        .read_svg_without_rsvg = function(img) {
            fell_back <<- TRUE
            suppressWarnings(real_fallback(img))
        },
        .package = "ggimage"
    )
    dispatched <- tryCatch(ggimage:::.read_image_file(svg), error = function(e) e)
    if (inherits(dispatched, "error")) {
        fail(paste("the dispatcher could not read an SVG without rsvg:",
                   conditionMessage(dispatched)))
    } else {
        expect_true(fell_back, label = "the rsvg-less fallback renderer ran")
        expect_true(methods::is(dispatched, "magick-image"))
    }
})

test_that("an SVG still prefers rsvg where it is installed", {
    skip_if_not_installed("magick")
    skip_if(!ggimage:::.has_rsvg(), "rsvg not installed on this machine")

    svg <- tempfile(fileext = ".svg")
    writeLines(c('<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
                 '<rect width="10" height="10" fill="#00ff00"/></svg>'), svg)

    ## the fallback must not silently replace the sharp renderer
    via_rsvg <- magick::image_info(magick::image_read_svg(svg))
    dispatched <- magick::image_info(ggimage:::.read_image_file(svg))
    expect_equal(dispatched$width, via_rsvg$width)
})

test_that("raster images are read unchanged", {
    skip_if_not_installed("magick")

    png <- system.file("extdata", "Rlogo.png", package = "ggimage")
    expect_equal(
        magick::image_info(ggimage:::.read_image_file(png))$width,
        magick::image_info(magick::image_read(png))$width
    )
})

test_that("a query-like suffix names the file, it is not the extension", {
    skip_if_not_installed("magick")

    dir <- tempfile()
    dir.create(dir)
    red <- file.path(dir, "red.png")
    magick::image_write(magick::image_blank(4, 4, color = "red"), red)

    ## A query string is the real-world case, but '?' is not a legal character
    ## in a Windows file name, so that suffix can only be created where the
    ## file system accepts it.  '#' goes through the same strip (`[?#].*$`)
    ## and is legal everywhere, so both spellings are exercised where they
    ## can be, and at least one of them everywhere.
    suffixes <- if (.Platform$OS.type == "windows") {
        "#token=1"
    } else {
        c("?token=1", "#token=1")
    }

    for (k in seq_along(suffixes)) {
        blue <- file.path(dir, sprintf("blue%d.png%s", k, suffixes[k]))
        magick::image_write(magick::image_blank(4, 4, color = "blue"), blue)

        ## dropping the suffix before *opening* the file would read a
        ## different (non-existent) path here
        data <- magick::image_data(ggimage:::.read_image_file(blue), channels = "RGB")
        expect_identical(as.character(data[, 1, 1]), c("00", "00", "ff"))
    }

    ## the suffix is ignored where the format is decided, so the 300 dpi branch
    ## still applies to a name that carries one
    eps <- write_vector("eps")
    query <- file.path(dir, paste0("logo.eps", suffixes[1]))
    file.copy(eps, query, overwrite = TRUE)
    info <- magick::image_info(read_or_skip(query))
    expect_equal(as.numeric(sub("x.*$", "", info$density)), 300)
})
