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
