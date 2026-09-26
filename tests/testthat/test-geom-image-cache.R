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


test_that("cache policy bounds base and transformed caches with LRU eviction", {
  ggimage:::clear_image_cache()
  withr::defer(ggimage:::clear_image_cache())
  now <- 0
  withr::local_options(list(
    ggimage.image_cache_base_capacity = 2,
    ggimage.image_cache_transform_capacity = 1,
    ggimage.image_cache_ttl = Inf,
    ggimage.image_cache_eviction = "lru",
    ggimage.image_cache_clock = function() now
  ))

  ggimage:::cache_set_image("a", "a")
  now <- 1
  ggimage:::cache_set_image("b", "b")
  now <- 2
  expect_equal(ggimage:::cache_get_image("a"), "a")
  now <- 3
  ggimage:::cache_set_image("c", "c")
  expect_equal(ggimage:::get_image_cache_size(), 2L)
  expect_null(ggimage:::cache_get_image("b"))
  expect_equal(ggimage:::cache_get_image("a"), "a")
  expect_equal(ggimage:::cache_get_image("c"), "c")

  ggimage:::cache_set_transformed("one", 1)
  ggimage:::cache_set_transformed("two", 2)
  expect_equal(ggimage:::get_image_transform_cache_size(), 1L)
  expect_equal(ggimage:::cache_get_transformed("two"), 2)
})

test_that("cache policy TTL uses a controllable clock", {
  ggimage:::clear_image_cache()
  withr::defer(ggimage:::clear_image_cache())
  now <- 10
  withr::local_options(list(
    ggimage.image_cache_capacity = Inf,
    ggimage.image_cache_ttl = 5,
    ggimage.image_cache_clock = function() now
  ))

  ggimage:::cache_set_image("ttl", "value")
  expect_equal(ggimage:::cache_get_image("ttl"), "value")
  now <- 15
  expect_null(ggimage:::cache_get_image("ttl"))
  expect_equal(ggimage:::get_image_cache_size(), 0L)

  ggimage:::cache_set_transformed("ttl", "value")
  now <- 20
  expect_null(ggimage:::cache_get_transformed("ttl"))
  expect_equal(ggimage:::get_image_transform_cache_size(), 0L)
})

test_that("use_cache FALSE and clear_image_cache bypass and clear policy state", {
  ggimage:::clear_image_cache()
  withr::defer(ggimage:::clear_image_cache())
  withr::local_options(list(
    ggimage.image_cache_capacity = 1,
    ggimage.image_cache_ttl = Inf
  ))

  expect_equal(ggimage:::cache_set_image("uncached", "value", use_cache = FALSE),
               invisible("value"))
  expect_null(ggimage:::cache_get_image("uncached"))
  ggimage:::cache_set_image("cached", "value")
  expect_equal(ggimage:::get_image_cache_size(), 1L)
  ggimage:::clear_image_cache()
  expect_equal(ggimage:::get_image_cache_size(), 0L)
  expect_equal(ggimage:::get_image_transform_cache_size(), 0L)
})

test_that("cache policy getter and setter expose compatible options", {
  old_options <- options(
    ggimage.image_cache_capacity = NULL,
    ggimage.image_cache_ttl = NULL,
    ggimage.image_cache_eviction = NULL
  )
  withr::defer(options(old_options))
  old <- ggimage::set_image_cache_policy(capacity = 3, ttl = 4, eviction = "fifo")
  policy <- ggimage::get_image_cache_policy()
  expect_equal(policy$base_capacity, 3)
  expect_equal(policy$transform_capacity, 3)
  expect_equal(policy$base_ttl, 4)
  expect_equal(policy$transform_ttl, 4)
  expect_equal(policy$eviction, "fifo")
  expect_type(old, "list")
})
