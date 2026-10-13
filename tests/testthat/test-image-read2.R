test_that("image_read2 crops margins without changing RGB or alpha pixels", {
    bitmap <- array(as.raw(255L), dim = c(4L, 8L, 10L))
    bitmap[1L, 3:6, 4:8] <- as.raw(10L)
    bitmap[2L, 3:6, 4:8] <- as.raw(20L)
    bitmap[3L, 3:6, 4:8] <- as.raw(30L)
    bitmap[4L, 3:6, 4:8] <- as.raw(128L)

    source <- magick::image_read(bitmap)
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path), add = TRUE)
    magick::image_write(source, path)

    original <- magick::image_read(path)
    cropped <- image_read2(path)
    expected <- original[[1]][, 3:6, 4:8, drop = FALSE]

    expect_equal(as.vector(cropped[[1]]), as.vector(expected))
    expect_equal(dim(cropped[[1]]), c(4L, 4L, 5L))
    expect_true(magick::image_info(cropped)$matte)
    expect_identical(cropped[[1]][4L, 2L, 2L], as.raw(128L))
})

test_that("image_read2 leaves an image without white margins unchanged", {
    bitmap <- array(as.raw(0L), dim = c(4L, 3L, 2L))
    bitmap[1L, , ] <- as.raw(10L)
    bitmap[2L, , ] <- as.raw(20L)
    bitmap[3L, , ] <- as.raw(30L)
    bitmap[4L, , ] <- as.raw(64L)

    source <- magick::image_read(bitmap)
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path), add = TRUE)
    magick::image_write(source, path)

    original <- magick::image_read(path)
    cropped <- image_read2(path)
    expect_equal(cropped[[1]], original[[1]])
    expect_equal(magick::image_info(cropped)$width,
                 magick::image_info(original)$width)
    expect_equal(magick::image_info(cropped)$height,
                 magick::image_info(original)$height)
})

test_that("a multi-frame image keeps every frame, each trimmed by its own margin", {
    first <- array(as.raw(255L), dim = c(3L, 8L, 8L))
    first[1L, 3:5, 3:5] <- as.raw(200L)
    second <- array(as.raw(255L), dim = c(3L, 8L, 8L))
    second[2L, 4:6, 2:7] <- as.raw(10L)

    two <- c(magick::image_read(first), magick::image_read(second))
    path <- tempfile(fileext = ".gif")
    on.exit(unlink(path), add = TRUE)
    magick::image_write(two, path, format = "gif")

    trimmed <- image_read2(path)
    expect_length(trimmed, 2L)
    expect_equal(dim(trimmed[[1L]]), c(3L, 3L, 3L))
    expect_equal(dim(trimmed[[2L]]), c(3L, 3L, 6L))
})

test_that("a positional density argument reaches image_read instead of clashing", {
    path <- tempfile(fileext = ".pdf")
    on.exit(unlink(path), add = TRUE)
    grDevices::pdf(path, width = 2, height = 2)
    graphics::plot.new()
    graphics::box()
    grDevices::dev.off()

    reference <- tryCatch(
        magick::image_read(path, density = 300),
        error = function(e) NULL
    )
    skip_if(is.null(reference), "no PDF rasteriser on this machine")

    ## `300` is unnamed, so it binds to the density formal of image_read(); the
    ## reader must not add a named density on top of it
    info <- magick::image_info(image_read2(path, 300, cut_empty_space = FALSE))
    expect_equal(as.numeric(sub("x.*$", "", info$density)), 300)
})

## A 1xN or Nx1 image used to abort `rowSums()` with "'x' must be an array of
## at least two dimensions": slicing one channel out of a bitmap whose width or
## height is 1 collapses to a plain vector.  The trim mask is now rebuilt with
## an explicit (row, column) margin, so degenerate shapes survive.

## Write a magick image of the given size to a temp PNG and return the path.
thin_image_path <- function(width, height, dark = NULL) {
    bitmap <- array(as.raw(255L), dim = c(3L, width, height))
    for (cell in dark) {
        bitmap[, cell[[1L]], cell[[2L]]] <- as.raw(0L)
    }
    path <- tempfile(fileext = ".png")
    magick::image_write(magick::image_read(bitmap), path)
    path
}

test_that("image_read2 handles 1x1, 1x20, and 20x1 all-white images", {
    for (dims in list(c(1L, 1L), c(1L, 20L), c(20L, 1L))) {
        width <- dims[[1L]]
        height <- dims[[2L]]
        path <- thin_image_path(width, height)
        on.exit(unlink(path), add = TRUE)

        image <- image_read2(path)

        ## An all-white input has no margin to trim, so it is returned
        ## unchanged: same size, same pixels.
        info <- magick::image_info(image)
        expect_equal(info$width, width)
        expect_equal(info$height, height)
        ## a greyscale PNG carries one channel; what matters here is that the
        ## (row, column) margin survives instead of collapsing to a vector
        expect_equal(dim(image[[1L]])[2:3], c(width, height))
        expect_true(all(as.integer(image[[1L]]) == 255L))
    }
})

test_that("image_read2 trims a 20x1 image whose interior pixel is dark", {
    ## One dark pixel in the middle of a single-row image: the trim must keep
    ## that column and drop the white columns on either side, without aborting.
    path <- thin_image_path(20L, 1L, dark = list(c(7L, 1L)))
    on.exit(unlink(path), add = TRUE)

    image <- image_read2(path)
    info <- magick::image_info(image)
    expect_equal(info$width, 1L)
    expect_equal(info$height, 1L)
    ## the surviving pixel is the dark one the trim was meant to keep
    expect_true(all(as.integer(image[[1L]]) == 0L))
})

test_that("image_read2 trims a 1x20 image whose interior pixel is dark", {
    path <- thin_image_path(1L, 20L, dark = list(c(1L, 5L)))
    on.exit(unlink(path), add = TRUE)

    image <- image_read2(path)
    info <- magick::image_info(image)
    expect_equal(info$width, 1L)
    expect_equal(info$height, 1L)
    expect_true(all(as.integer(image[[1L]]) == 0L))
})

test_that("image_read2 leaves a fully dark thin image alone", {
    ## Every pixel dark means every row and column is kept, so no cropping
    ## happens and the margin-based shortcut must not misfire.
    path <- thin_image_path(20L, 1L, dark = lapply(seq_len(20L), function(i) c(i, 1L)))
    on.exit(unlink(path), add = TRUE)

    image <- image_read2(path)
    info <- magick::image_info(image)
    expect_equal(info$width, 20L)
    expect_equal(info$height, 1L)
})

test_that("image_read2 trims thin images reproducibly", {
    ## Re-reading the file must give the same result, and writing the trimmed
    ## result back out and reading it again must be a no-op (the margin is
    ## already gone), which also proves the degenerate shape survives a second
    ## pass through the reader.
    path <- thin_image_path(1L, 20L, dark = list(c(1L, 8L)))
    on.exit(unlink(path), add = TRUE)

    once <- image_read2(path)
    again <- image_read2(path)
    expect_equal(as.integer(again[[1L]]), as.integer(once[[1L]]))

    settled <- tempfile(fileext = ".png")
    on.exit(unlink(settled), add = TRUE)
    magick::image_write(once, settled)
    twice <- image_read2(settled)
    expect_equal(magick::image_info(twice)$width, magick::image_info(once)$width)
    expect_equal(magick::image_info(twice)$height, magick::image_info(once)$height)
    expect_equal(as.integer(twice[[1L]]), as.integer(once[[1L]]))
})
