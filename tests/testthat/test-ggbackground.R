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
