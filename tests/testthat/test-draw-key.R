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
## is transparent, the key is flat coloured by the layer's colour)
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

test_that("image legend reuses the bundled logo within a session", {
    cache <- ggimage:::.ggimage_key_image_cache
    if (exists("image", envir = cache, inherits = FALSE)) {
        rm("image", envir = cache)
    }
    on.exit(if (exists("image", envir = cache, inherits = FALSE)) {
        rm("image", envir = cache)
    }, add = TRUE)

    reads <- 0L
    load <- function() {
        reads <<- reads + 1L
        magick::image_read(system.file("extdata/Rlogo.png", package = "ggimage"))
    }
    testthat::local_mocked_bindings(load_key_image = load, .package = "ggimage")

    withr::local_options(ggimage.keytype = "image")
    draw_key_image(keydata(colour = "red", alpha = 1), list(), 20)
    draw_key_image(keydata(colour = "blue", alpha = 0.5), list(), 20)

    expect_equal(reads, 1L)
    expect_true(exists("image", envir = cache, inherits = FALSE))
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


test_that("an image in the key data is drawn instead of the bundled logo", {
    red <- tempfile(fileext = ".png")
    blue <- tempfile(fileext = ".png")
    magick::image_write(magick::image_blank(6, 6, color = "red"), red)
    magick::image_write(magick::image_blank(6, 6, color = "blue"), blue)
    withr::local_options(ggimage.keytype = "image")

    data <- keydata(colour = NA_character_, alpha = 1)
    data$image <- red
    key <- draw_key_image(data, list(), 20)
    expect_equal(as.character(key$children[[1]]$raster[1, 1]), "#ff0000ff")

    ## one image per group, also when the images are objects rather than paths
    two <- draw_key_image(
        data.frame(colour = c(NA_character_, NA_character_), alpha = c(1, 1),
                   size = 0.05,
                   image = I(list(magick::image_blank(6, 6, color = "red"),
                                  magick::image_blank(6, 6, color = "blue"))),
                   stringsAsFactors = FALSE),
        list(), 20
    )
    expect_equal(as.character(two$children[[1]]$raster[1, 1]), "#ff0000ff")
    expect_equal(as.character(two$children[[2]]$raster[1, 1]), "#0000ffff")
})

test_that("a key image that cannot be read costs its own key only", {
    missing_file <- file.path(tempdir(), "ggimage-no-such-key-file.png")
    two <- data.frame(colour = c(NA_character_, NA_character_), alpha = c(1, 1),
                      size = 0.05, stringsAsFactors = FALSE)
    two$image <- c(missing_file, system.file("extdata/Rlogo.png", package = "ggimage"))
    withr::local_options(ggimage.keytype = "image")

    ## ImageMagick words this differently per platform and build:
    ## "unable to open image ..." on the Unix builds, "UnableToOpenBlob ..."
    ## through Windows' Rterm.exe -- the file name is in both.
    expect_warning(key <- draw_key_image(two, list(), 20),
                   "unable to open image|UnableToOpenBlob|ggimage-no-such-key-file")
    expect_length(key$children, 2L)
    expect_true(inherits(key$children[[1]], "zeroGrob"))
    expect_true(inherits(key$children[[2]], "rastergrob"))
})

test_that("the legend shows one image per key when the keys carry images", {
    red <- tempfile(fileext = ".png")
    blue <- tempfile(fileext = ".png")
    magick::image_write(magick::image_blank(6, 6, color = "red"), red)
    magick::image_write(magick::image_blank(6, 6, color = "blue"), blue)

    d <- data.frame(x = 1:4, y = 1:4, g = c("a", "a", "b", "b"),
                    stringsAsFactors = FALSE)
    plot <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
        ggimage::geom_image(image = red, size = 0.08) +
        ggplot2::guides(colour = ggplot2::guide_legend(
            override.aes = list(image = c(red, blue))
        )) +
        ggplot2::theme_void()

    withr::local_options(ggimage.keytype = "image")
    keys <- find_grobs(ggplot2::ggplotGrob(plot), "rastergrob")
    dims <- vapply(keys, function(grob) nrow(grob$raster), integer(1))
    ## the keys show the images handed to the guide; only the bundled logo,
    ## which the layer does not use here, would be taller than these 6x6 files
    expect_false(any(dims > 6L))
    expect_gte(sum(dims == 6L), 3L)
})

test_that("the image key applies the layer's image_fun", {
    white <- magick::image_blank(6, 6, color = "white")
    tint_red <- function(x) magick::image_colorize(x, 100, "red")

    data <- keydata(image = I(list(white)), colour = NA_character_, alpha = NA_real_)
    withr::local_options(ggimage.keytype = "image")
    plain <- draw_key_image(data, list(), 20)$children[[1]]
    tinted <- draw_key_image(data, list(image_fun = tint_red), 20)$children[[1]]

    expect_equal(as.character(plain$raster[1, 1]), "#ffffffff")
    expect_equal(as.character(tinted$raster[1, 1]), "#ff0000ff")
})

test_that("the transform key separates missing colour from a real one", {
    key <- function(colour, opacity = 1, angle = 0) {
        ggimage:::image_transform_key("base", angle, colour, opacity, NULL)
    }

    ## "" and NA were mapped to the same empty component as each other and as
    ## a real colour's absence; a real colour may never share their key
    expect_identical(key(NA_character_), key(""))
    expect_identical(key(NA_character_), key(NULL))
    expect_identical(key(""), key(NULL))
    expect_false(identical(key(""), key("red")))
    expect_false(identical(key(NA_character_), key("red")))
    expect_false(identical(key("blue"), key("red")))
    expect_false(identical(key(""), key("v:")))

    ## "no colour" keeps one key, so an untinted image is not cached twice
    expect_length(unique(c(key(NA_character_), key(""), key(NULL))), 1L)
    ## and an NA alpha stays distinct from an opaque one
    expect_false(identical(key("red", NA_real_), key("red", 1)))
    ## the pre-existing "NA angle means no rotation" aliasing is unchanged
    expect_identical(key("red", angle = NA_real_), key("red", angle = 0))
})

test_that("the transform key routes each tint to its own cached value", {
    ## the key is what the image caches are indexed by, so a collision means
    ## a tinted image is drawn where an untinted one belongs
    memo <- new.env(hash = TRUE, parent = emptyenv())
    store <- function(colour, value) {
        assign(ggimage:::image_transform_key("base", 0, colour, 1, NULL),
               value, envir = memo)
    }
    read <- function(colour) {
        get(ggimage:::image_transform_key("base", 0, colour, 1, NULL),
            envir = memo)
    }

    store("red", "tinted")
    store("", "plain")

    expect_identical(read(""), "plain")
    expect_identical(read(NA_character_), "plain")
    expect_identical(read("red"), "tinted")
    expect_false(identical(read(""), read("red")))
})
