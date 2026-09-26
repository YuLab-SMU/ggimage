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
