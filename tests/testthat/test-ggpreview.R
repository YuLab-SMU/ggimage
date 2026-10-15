## The no-filename branch writes a PNG into the session temp directory and used
## to leave it there for the rest of the session.  These tests count the PNGs
## in tempdir() around the call, which needs no device other than the null one.
ggpreview_png_count <- function() {
    length(list.files(tempdir(), pattern = "[.]png$"))
}

ggpreview_test_plot <- function() {
    ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
}

ggpreview_draw <- function(...) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    ggimage::ggpreview(...)
}

test_that("ggpreview() removes the tempfile it created itself", {
    before <- ggpreview_png_count()

    ggpreview_draw(plot = ggpreview_test_plot(), width = 2, height = 2)

    expect_identical(ggpreview_png_count(), before)
})

test_that("ggpreview() keeps a file the caller named", {
    before <- ggpreview_png_count()
    keep <- tempfile(fileext = ".png")
    on.exit(unlink(keep, force = TRUE), add = TRUE)

    ggpreview_draw(filename = keep, plot = ggpreview_test_plot(),
                   width = 2, height = 2)

    expect_true(file.exists(keep))
    ## the named file is not a leak into the temp directory's PNGs
    expect_identical(ggpreview_png_count(), before + 1L)
    unlink(keep, force = TRUE)

    ## and it survives the call's own cleanup
    ggpreview_draw(filename = keep, plot = ggpreview_test_plot(),
                   width = 2, height = 2)
    expect_true(file.exists(keep))
})

test_that("ggpreview() cleans up on whatever device draws it", {
    ## the null device leaves no file behind at all, so any PNG is the leak
    before <- ggpreview_png_count()
    ggpreview_draw(plot = ggpreview_test_plot(), width = 2, height = 2)
    expect_identical(ggpreview_png_count(), before)

    ## `device = "pdf"` routes ggsave through a different device than the
    ## default and must clean up just the same.  Where the build has no
    ## delegate for reading that file back, ggpreview() warns and previews the
    ## plot itself -- the cleanup is what this assertion is about, so the
    ## warning is expected rather than a failure.
    before <- ggpreview_png_count()
    suppressWarnings(ggpreview_draw(plot = ggpreview_test_plot(), width = 2,
                                    height = 2, device = "pdf"))
    expect_identical(ggpreview_png_count(), before)

    ## a non-ggplot object goes through `ggplotify::as.grob()` and must clean
    ## up the same way
    before <- ggpreview_png_count()
    ggpreview_draw(plot = grid::grob(), width = 2, height = 2)
    expect_identical(ggpreview_png_count(), before)
})

test_that("ggpreview() only unlinks its own file", {
    ## a caller-supplied file in the same directory must not be touched
    dir <- tempfile("ggpreview-")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    other <- file.path(dir, "keep.png")
    writeLines("keep", other)

    ggpreview_draw(filename = file.path(dir, "out.png"),
                   plot = ggpreview_test_plot(), width = 2, height = 2)

    expect_identical(readLines(other), "keep")
    expect_true(file.exists(file.path(dir, "out.png")))
})

test_that("ggpreview() previews the grob when the file cannot be read back", {
    ## On a build whose ImageMagick has no Ghostscript delegate, reading a PDF
    ## answers with an image of *no frames* rather than an error, and the
    ## failure only surfaces later, inside as.grob(): "Image must have at least
    ## 1 frame to write a bitmap".  The preview has a usable grob already, so
    ## warn and draw that instead.
    empty <- suppressWarnings(magick::image_blank(4, 4, color = "red")[0])
    local_mocked_bindings(image_read = function(...) empty, .package = "ggimage")

    before <- ggpreview_png_count()
    expect_warning(ggpreview_draw(plot = ggpreview_test_plot(),
                                  width = 2, height = 2),
                   "could not re-read")
    ## nothing was leaked while doing it
    expect_identical(ggpreview_png_count(), before)
})

test_that("ggpreview() reports a file that was not written at all", {
    ## ggsave() failing leaves nothing to read back, and the warning must say
    ## so rather than abort the call from inside magick
    local_mocked_bindings(image_read = function(...) {
        stop("no delegate for this format", call. = FALSE)
    }, .package = "ggimage")

    expect_warning(ggpreview_draw(plot = ggpreview_test_plot(),
                                  width = 2, height = 2),
                   "no delegate")
})
