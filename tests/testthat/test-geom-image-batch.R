local_batch_image <- system.file("extdata/Rlogo.png", package = "ggimage")

local_batch_data <- function(image = local_batch_image, n = 1L) {
  data.frame(
    x = seq(0.2, 0.2 + 0.2 * (n - 1L), length.out = n),
    y = seq(0.3, 0.3 + 0.1 * (n - 1L), length.out = n),
    size = rep(0.05, n),
    image = rep(image, length.out = n),
    colour = rep(NA_character_, n),
    alpha = rep(1, n),
    angle = rep(0, n),
    width = seq(0.1, 0.1 + 0.1 * (n - 1L), length.out = n),
    height = seq(0.2, 0.2 + 0.1 * (n - 1L), length.out = n),
    stringsAsFactors = FALSE
  )
}

test_that("draw_grobs deduplicates repeated transforms within one panel", {
  data <- local_batch_data(n = 4L)
  data$colour <- c("red", "red", "blue", "blue")
  data$alpha <- c(0.5, 0.5, 0.5, 0.5)
  data$angle <- c(0, 0, 0, 15)

  calls <- 0L
  prepare <- ggimage:::prepare_image
  testthat::local_mocked_bindings(
    prepare_image = function(...) {
      calls <<- calls + 1L
      prepare(...)
    },
    .package = "ggimage"
  )

  grob <- ggimage:::GeomImage$draw_grobs(
    data, list(x.range = c(0, 1)), NULL, by = "width", use_cache = FALSE
  )

  ## Rows 1/2 share a transform, while rows 3/4 differ by angle as well as
  ## colour. Explicit dimensions must not participate in the preparation key.
  expect_equal(calls, 3L)
  expect_length(grob$children, 4L)
  expect_equal(unname(vapply(grob$children, function(x) as.numeric(x$width), numeric(1))),
               unname(data$width))
  expect_equal(unname(vapply(grob$children, function(x) as.numeric(x$height), numeric(1))),
               unname(data$height))
  expect_equal(unname(vapply(grob$children, function(x) as.numeric(x$x), numeric(1))),
               unname(data$x))
  expect_equal(unname(vapply(grob$children, function(x) as.numeric(x$y), numeric(1))),
               unname(data$y))
})

test_that("unique transforms still prepare once per row when uncached", {
  ggimage:::clear_image_cache()
  withr::defer(ggimage:::clear_image_cache())
  data <- local_batch_data(n = 3L)
  data$colour <- c("red", "green", "blue")
  data$angle <- c(0, 10, 20)

  calls <- 0L
  prepare <- ggimage:::prepare_image
  testthat::local_mocked_bindings(
    prepare_image = function(...) {
      calls <<- calls + 1L
      prepare(...)
    },
    .package = "ggimage"
  )

  ggimage:::GeomImage$draw_grobs(
    data, list(x.range = c(0, 1)), NULL, by = "width", use_cache = FALSE
  )
  expect_equal(calls, 3L)
  expect_equal(ggimage:::get_image_cache_size(), 0L)
  expect_equal(ggimage:::get_image_transform_cache_size(), 0L)
})
