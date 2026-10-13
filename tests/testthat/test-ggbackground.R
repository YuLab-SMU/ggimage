test_that("ggbackground preserves annotation_custom coordinates", {
  background <- tempfile(fileext = ".png")
  magick::image_write(
    magick::image_blank(width = 20, height = 20, color = "blue"),
    path = background
  )

  logo <- grid::rasterGrob(
    matrix("#ff0000", nrow = 20, ncol = 20),
    interpolate = FALSE
  )
  plot <- ggplot2::ggplot(
    data.frame(x = 0:1, y = c(-1, 1)),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    ggplot2::theme_void()

  plot <- ggimage::ggbackground(plot, background) +
    ggplot2::annotation_custom(
      logo,
      xmin = 0,
      xmax = 0.3,
      ymin = 0.3,
      ymax = -1
    ) +
    ggplot2::coord_cartesian(clip = "off")

  output <- tempfile(fileext = ".png")
  ggplot2::ggsave(output, plot, width = 2, height = 2, dpi = 100)
  pixels <- magick::image_data(magick::image_read(output), channels = "RGB")
  red <- pixels[1, , ] == "ff" & pixels[2, , ] == "00" & pixels[3, , ] == "00"
  expect_gt(sum(red), 0)
})


test_that("background layer does not inherit plot aesthetics", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3, group = letters[1:3]),
    ggplot2::aes(x, y, colour = group, alpha = group)
  ) + ggplot2::geom_point()

  result <- ggimage::ggbackground(plot, tempfile(fileext = ".png"))
  background_layer <- result$layers[[1]]

  expect_false(background_layer$inherit.aes)
  expect_equal(nrow(background_layer$data), 1L)
})


ggbackground_ranges <- function(plot) {
  panel <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]
  list(x = panel$x.range, y = panel$y.range)
}

ggbackground_rasters <- function(x) {
  out <- if (inherits(x, "rastergrob")) list(x) else list()
  children <- list(x$children, x$grobs)
  for (items in children) {
    if (!is.null(items)) {
      for (child in items) out <- c(out, ggbackground_rasters(child))
    }
  }
  out
}

test_that("ggbackground does not train the panel scales", {
  background <- tempfile(fileext = ".png")
  magick::image_write(
    magick::image_blank(width = 8, height = 8, color = "grey"),
    path = background
  )

  plot <- ggplot2::ggplot(
    data.frame(x = seq(2, 20, by = 2), y = seq_len(10)),
    ggplot2::aes(x, y)
  ) + ggplot2::geom_point()

  ## the background needs a row to carry the layer, and that row must not be
  ## a data point of its own: a fixed 0.5 shifted the lower limit of x
  expect_equal(ggbackground_ranges(ggimage::ggbackground(plot, background)),
               ggbackground_ranges(plot))
})

test_that("ggbackground works on discrete axes", {
  background <- tempfile(fileext = ".png")
  magick::image_write(
    magick::image_blank(width = 8, height = 8, color = "grey"),
    path = background
  )

  data <- data.frame(g = letters[1:3], y = c("p", "q", "r"),
                     stringsAsFactors = TRUE)
  plot <- ggplot2::ggplot(data, ggplot2::aes(g, y)) + ggplot2::geom_point()
  with_background <- ggimage::ggbackground(plot, background)

  ## a numeric anchor cannot be placed on a discrete scale at all
  expect_silent(ggplot2::ggplot_build(with_background))
  expect_equal(ggbackground_ranges(with_background), ggbackground_ranges(plot))
  expect_length(ggbackground_rasters(ggplot2::ggplotGrob(with_background)), 1L)
})

ggbackground_points <- function(plot) {
  ## every row that is actually drawn, the background layer included
  do.call(rbind, lapply(ggplot2::ggplot_build(plot)$data,
                        function(layer) layer[, c("x", "y")]))
}

test_that("ggbackground reads layer-level aesthetics", {
  background <- tempfile(fileext = ".png")
  magick::image_write(
    magick::image_blank(width = 8, height = 8, color = "grey"),
    path = background
  )

  ## the common `ggplot() + geom_point(data = d, aes(x, y))` idiom maps x and y
  ## on the layer, so the plot-level mapping is empty and the anchor used to
  ## fall back to a hard-coded (0.5, 0.5) row
  data <- data.frame(x = c(100, 200), y = c(1, 2))
  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x, y))
  with_background <- ggimage::ggbackground(plot, background)

  expect_equal(ggbackground_ranges(with_background), ggbackground_ranges(plot))
  expect_false(any(ggbackground_points(with_background)$x == 0.5))
  expect_length(ggbackground_rasters(ggplot2::ggplotGrob(with_background)), 1L)
})

test_that("ggbackground reads layer-level aesthetics on discrete axes", {
  background <- tempfile(fileext = ".png")
  magick::image_write(
    magick::image_blank(width = 8, height = 8, color = "grey"),
    path = background
  )

  data <- data.frame(g = letters[1:3], y = c("p", "q", "r"),
                     stringsAsFactors = TRUE)
  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(g, y))
  with_background <- ggimage::ggbackground(plot, background)

  ## the numeric fallback aborted with "Discrete value supplied to a
  ## continuous scale"
  expect_silent(ggplot2::ggplot_build(with_background))
  expect_equal(ggbackground_ranges(with_background), ggbackground_ranges(plot))
  expect_length(ggbackground_rasters(ggplot2::ggplotGrob(with_background)), 1L)
})
