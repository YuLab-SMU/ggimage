find_grobs <- function(x, cls) {
    out <- list()
    if (inherits(x, cls)) {
        out[[length(out) + 1L]] <- x
    }
    for (children in list(x$children, x$grobs)) {
        if (!is.null(children)) {
            for (child in children) {
                out <- c(out, find_grobs(child, cls))
            }
        }
    }
    out
}

keydata <- function(...) {
    data.frame(..., size = 0.05, stringsAsFactors = FALSE)
}

## `scales::alpha()` is not consistent for length 1 vs longer input
## ("#FF0000FF" vs "#FF0000"), so keys are compared as rgba values.
rgba <- function(x) as.integer(grDevices::col2rgb(x, alpha = TRUE))

## the most frequent colour of a raster grob (the background of the R logo
## is transparent, the logo itself is flat coloured by `color_image()`)
dominant_colour <- function(grob) {
    raster <- grob$raster
    names(sort(table(as.vector(raster)), decreasing = TRUE))[1]
}

test_that("GeomImage draws a key instead of a blank one", {
    key <- ggimage:::GeomImage$draw_key(keydata(colour = "red", alpha = 1), list(), 20)

    expect_false(inherits(key, "zeroGrob"))
    expect_true(inherits(key, "points"))
})

test_that("default key is a point filled with the colour aesthetic", {
    withr::with_options(list(ggimage.keytype = NULL), {
        key <- draw_key_image(keydata(colour = "#F8766D", alpha = 1), list(), 20)
    })

    expect_true(inherits(key, "points"))
    expect_equal(rgba(key$gp$col), rgba("#F8766D"))
    expect_equal(rgba(key$gp$fill), rgba("#F8766D"))
})

test_that("key falls back to black when colour is not mapped or NA", {
    ## only `alpha` is mapped, `colour` is not part of the key data
    expect_no_warning(
        key <- draw_key_image(keydata(alpha = 0.5), list(), 20)
    )
    expect_equal(rgba(key$gp$col), c(0L, 0L, 0L, 128L))

    key <- draw_key_image(keydata(colour = NA_character_, alpha = 1), list(), 20)
    expect_equal(rgba(key$gp$col), c(0L, 0L, 0L, 255L))
})

test_that("alpha is reflected in the key", {
    opaque <- draw_key_image(keydata(colour = "red", alpha = 1), list(), 20)
    semi <- draw_key_image(keydata(colour = "red", alpha = 0.5), list(), 20)

    expect_equal(rgba(opaque$gp$col), c(255L, 0L, 0L, 255L))
    expect_equal(rgba(semi$gp$col), c(255L, 0L, 0L, 128L))

    ## missing alpha is treated as opaque
    expect_equal(
        rgba(draw_key_image(keydata(colour = "red", alpha = NA_real_), list(), 20)$gp$col),
        c(255L, 0L, 0L, 255L)
    )
})

test_that("ggimage.keytype selects the key grob", {
    data <- keydata(colour = "#00BFC4", alpha = 1)

    withr::with_options(list(ggimage.keytype = "rect"), {
        key <- draw_key_image(data, list(), 20)
    })
    expect_true(inherits(key, "rect"))
    expect_equal(rgba(key$gp$fill), rgba("#00BFC4"))
    expect_true(is.na(key$gp$col))

    withr::with_options(list(ggimage.keytype = "image"), {
        key <- draw_key_image(data, list(), 20)
        semi <- draw_key_image(keydata(colour = "#00BFC4", alpha = 0.5), list(), 20)
        ## no colour to colorize with, the image is displayed as is
        uncoloured <- draw_key_image(keydata(alpha = 1), list(), 20)
    })
    expect_true(inherits(key, "gTree"))
    expect_length(key$children, 1L)
    expect_true(inherits(key$children[[1]], "rastergrob"))
    ## the image is colorized with the colour aesthetic
    expect_equal(rgba(dominant_colour(key$children[[1]]))[1:3], rgba("#00BFC4")[1:3])
    ## ... and made translucent with the alpha aesthetic
    expect_true(rgba(dominant_colour(semi$children[[1]]))[4] < 255L)
    expect_true(inherits(uncoloured$children[[1]], "rastergrob"))

    withr::with_options(list(ggimage.keytype = "blank"), {
        expect_true(inherits(draw_key_image(data, list(), 20), "zeroGrob"))
    })
})

test_that("unsupported ggimage.keytype falls back to point", {
    withr::with_options(list(ggimage.keytype = "oops"), {
        expect_warning(
            key <- draw_key_image(keydata(colour = "red", alpha = 1), list(), 20),
            "Unsupported"
        )
    })

    expect_true(inherits(key, "points"))
    expect_equal(rgba(key$gp$col), rgba("red"))
})

test_that("key data of several groups draws one grob per group", {
    data <- data.frame(
        colour = c("#F8766D", "#00BFC4"),
        alpha = c(1, 0.5),
        size = 0.05,
        stringsAsFactors = FALSE
    )
    key <- draw_key_image(data, list(), 20)

    expect_true(inherits(key, "gTree"))
    expect_length(key$children, 2L)
    expect_equal(rgba(key$children[[1]]$gp$col), rgba("#F8766D"))
    expect_equal(rgba(key$children[[2]]$gp$col), c(0L, 191L, 196L, 128L))
})

test_that("geom_image draws legend keys of mapped colour", {
    d <- data.frame(
        x = 1:3,
        y = 1:3,
        g = c("a", "b", "a"),
        image = system.file("extdata/Rlogo.png", package = "ggimage"),
        stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
        geom_image(ggplot2::aes(image = image)) +
        ggplot2::scale_colour_manual(values = c(a = "red", b = "blue"))

    gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
    guide <- gt$grobs[[which(gt$layout$name == "guide-box-right")]]
    keys <- find_grobs(guide, "points")

    expect_length(keys, 2L)
    cols <- vapply(keys, function(k) rgba(k$gp$col), integer(4))
    expect_equal(cols[, order(cols[1, ]), drop = FALSE],
                 cbind(rgba("blue"), rgba("red")))
})
