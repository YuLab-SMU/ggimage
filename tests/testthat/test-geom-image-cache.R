test_that("alpha is applied when colour is NULL", {
  image <- magick::image_blank(width = 1, height = 1, color = "red")

  transformed <- ggimage:::prepare_image(
    image,
    colour = NULL,
    opacity = 0.4,
    angle = 0,
    image_fun = NULL,
    use_cache = FALSE
  )

  alpha <- as.integer(magick::image_data(transformed, channels = "rgba")[4, 1, 1])
  expect_equal(alpha, round(0.4 * 255), tolerance = 1)
})

test_that("image_fun participates in the transform cache key", {
  ggimage:::clear_image_cache()
  withr::defer(ggimage:::clear_image_cache())

  image <- magick::image_blank(width = 1, height = 1, color = "white")
  red_fun <- function(x) {
    magick::image_colorize(x, opacity = 100, color = "red")
  }
  blue_fun <- function(x) {
    magick::image_colorize(x, opacity = 100, color = "blue")
  }

  red <- ggimage:::prepare_image(
    image,
    colour = NULL,
    opacity = 1,
    angle = 0,
    image_fun = red_fun,
    use_cache = TRUE
  )
  blue <- ggimage:::prepare_image(
    image,
    colour = NULL,
    opacity = 1,
    angle = 0,
    image_fun = blue_fun,
    use_cache = TRUE
  )

  red_pixel <- as.integer(magick::image_data(red, channels = "rgba")[1:3, 1, 1])
  blue_pixel <- as.integer(magick::image_data(blue, channels = "rgba")[1:3, 1, 1])
  expect_equal(red_pixel, c(255L, 0L, 0L))
  expect_equal(blue_pixel, c(0L, 0L, 255L))
  expect_equal(ggimage:::get_image_transform_cache_size(), 2L)
})


test_that("repeated local images reuse the image caches", {
  ggimage:::clear_image_cache()
  withr::defer(ggimage:::clear_image_cache())

  image <- system.file("extdata/Rlogo.png", package = "ggimage")
  args <- list(
    x = 0.5, y = 0.5, size = 0.05, img = image,
    colour = NULL, opacity = 1, angle = 0, adj = 1,
    image_fun = NULL, hjust = 0.5, by = "width", asp = 1,
    use_cache = TRUE, width = NA_real_, height = NA_real_
  )
  grobs <- replicate(3L, do.call(ggimage:::imageGrob, args), simplify = FALSE)

  expect_length(grobs, 3L)
  expect_equal(ggimage:::get_image_cache_size(), 1L)
  expect_equal(ggimage:::get_image_transform_cache_size(), 1L)
})
