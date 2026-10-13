test_that("theme_transparent() makes every surface transparent", {
    th <- ggimage::theme_transparent()
    expect_equal(th$panel.background$fill, "transparent")
    expect_equal(th$plot.background$fill, "transparent")
    expect_equal(th$legend.key$fill, "transparent")
    expect_equal(th$legend.background$fill, "transparent")
    expect_true(is.na(th$panel.background$colour))
})

test_that("theme_transparent() clears the facet strip, not just the panel", {
    ## the strips inherit the grey panel background from the base theme, so
    ## without this element a facetted plot kept one opaque band
    th <- ggimage::theme_transparent()
    expect_equal(th$strip.background$fill, "transparent")
    expect_true(is.na(th$strip.background$colour))
})

test_that("theme_transparent() can still be overridden by later elements", {
    ## ggplot2 merges themes with +; the transparent surfaces are the theme's
    ## defaults, not a locked-in look.  (Passing the same element through ... is
    ## a duplicate argument and theme() rejects it, by design.)
    th <- ggimage::theme_transparent() +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "cyan"))
    expect_equal(th$panel.background$fill, "cyan")
    ## an element the caller did not touch keeps its transparent value
    expect_equal((ggimage::theme_transparent() +
        ggplot2::theme(plot.title = ggplot2::element_text(colour = "red"))
    )$strip.background$fill, "transparent")
})


test_that("theme_nothing() keeps the plot to its panel", {
    th <- ggimage::theme_nothing()
    expect_equal(th$plot.margin, grid::unit(c(0, 0, -0.2, -0.2), "lines"))
})
