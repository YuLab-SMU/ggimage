test_that("missing names preserve batch rows as NA", {
    result <- phylopic_uid(c("", NA_character_))

    expect_identical(result$name, c("", NA_character_))
    expect_true(all(is.na(result$uid)))
})

test_that("phylopic keeps valid IDs and missing IDs aligned", {
    uid <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    result <- ggimage:::phylopic(c(uid, NA_character_, ""))

    expect_identical(result, c(
        paste0("https://images.phylopic.org/images/", uid, "/vector.svg"),
        NA_character_,
        NA_character_
    ))
})

test_that("missing response fields are treated as no match", {
    expect_identical(ggimage:::.phylopic_build(list()), NULL)
    expect_identical(
        ggimage:::.phylopic_extract_uids(list(`_embedded` = list(items = list()))),
        character()
    )

    local_mocked_bindings(
        .phylopic_from_json = function(url) list(build = 558L),
        .package = "ggimage"
    )
    expect_true(is.na(ggimage:::phylopic_uid_item("missing")))
})

test_that("duplicate names resolve once and stay aligned", {
    uid <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    calls <- character()
    ggimage:::.phylopic_uid_cache_clear()
    local_mocked_bindings(
        .phylopic_from_json = function(url) {
            calls <<- c(calls, url)
            if (!grepl("embed_items=true", url, fixed = TRUE)) {
                list(build = 558L)
            } else {
                list(`_embedded` = list(items = list(
                    `_links` = list(vectorFile = list(href = paste0(
                        "https://images.phylopic.org/images/", uid,
                        "/vector.svg"
                    )))
                )))
            }
        },
        .package = "ggimage"
    )

    input <- c(first = "Canis lupus", second = "canis lupus",
               missing = NA_character_, third = "Canis lupus")
    result <- ggimage:::phylopic(input)

    expect_identical(names(result), names(input))
    expect_identical(unname(result), c(
        paste0("https://images.phylopic.org/images/", uid, "/vector.svg"),
        paste0("https://images.phylopic.org/images/", uid, "/vector.svg"),
        NA_character_,
        paste0("https://images.phylopic.org/images/", uid, "/vector.svg")
    ))
    expect_length(calls, 2L)
    ggimage:::.phylopic_uid_cache_clear()
})

test_that("successful UID lookups are memoized per seed", {
    uid <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    calls <- character()
    ggimage:::.phylopic_uid_cache_clear()
    local_mocked_bindings(
        .phylopic_from_json = function(url) {
            calls <<- c(calls, url)
            if (!grepl("embed_items=true", url, fixed = TRUE)) {
                list(build = 558L)
            } else {
                list(`_embedded` = list(items = list(
                    `_links` = list(vectorFile = list(href = paste0(
                        "https://images.phylopic.org/images/", uid,
                        "/vector.svg"
                    )))
                )))
            }
        },
        .package = "ggimage"
    )

    first <- ggimage::phylopic_uid("Canis lupus")
    second <- ggimage::phylopic_uid("canis lupus")

    expect_identical(first$uid, second$uid)
    expect_length(calls, 2L)
    ggimage:::.phylopic_uid_cache_clear()
})

test_that("download_phylopic skips non-empty existing duplicate targets", {
    tmp <- tempfile("ggimage-phylopic-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    id <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    dir.create(file.path(tmp, id))
    source <- file.path(tmp, "source.svg")
    writeLines("source", source)
    local_mocked_bindings(
        phylopic = function(id) {
            url <- rep(paste0("file://", source), length(id))
            names(url) <- names(id)
            url
        },
        .package = "ggimage"
    )

    result <- ggimage::download_phylopic(c(id, id), destdir = tmp)

    target <- paste0(tmp, "/", id, "source.svg")
    expect_identical(unname(result), c(target, target))
    expect_identical(readLines(target), "source")
})

test_that("download_phylopic installs downloads atomically", {
    tmp <- tempfile("ggimage-phylopic-atomic-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    id <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    target <- paste0(tmp, "/", id, "vector.svg")
    calls <- 0L
    local_mocked_bindings(
        phylopic = function(id) {
            url <- rep("mock://vector.svg", length(id))
            names(url) <- names(id)
            url
        },
        .phylopic_download = function(url, destfile, ...) {
            calls <<- calls + 1L
            writeLines("complete", destfile)
            0L
        },
        .package = "ggimage"
    )

    result <- ggimage::download_phylopic(id, destdir = tmp)
    expect_identical(unname(result), target)
    expect_true(ggimage:::.phylopic_regular_file(target))
    expect_identical(readLines(target), "complete")
    expect_identical(calls, 1L)

    ## A second call reuses the complete regular file and does not invoke the
    ## downloader again.
    ggimage::download_phylopic(id, destdir = tmp)
    expect_identical(calls, 1L)
})

test_that("embedded vector links are extracted without network access", {
    uid <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    response <- list(
        `_embedded` = list(
            items = list(
                `_links` = list(
                    vectorFile = list(
                        href = paste0(
                            "https://images.phylopic.org/images/",
                            uid,
                            "/vector.svg"
                        )
                    )
                )
            )
        )
    )

    expect_identical(ggimage:::.phylopic_extract_uids(response), uid)
    expect_identical(
        ggimage:::.phylopic_extract_uids(list(`_embedded` = list())),
        character()
    )
})

test_that("a local file or an absolute URL is the silhouette itself", {
    svg <- tempfile(fileext = ".svg")
    writeLines(c('<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
                 '<rect width="10" height="10" fill="#00ff00"/></svg>'), svg)
    uid <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"

    ## the paths download_phylopic() returns must be usable without PhyloPic,
    ## so they may not be looked up as a name or prefixed as a uid
    expect_identical(
        ggimage:::phylopic(c(svg, uid, "https://example.org/a.svg")),
        c(svg,
          paste0("https://images.phylopic.org/images/", uid, "/vector.svg"),
          "https://example.org/a.svg")
    )

    found <- function(x) {
        out <- if (inherits(x, "rastergrob")) list(x) else list()
        for (children in list(x$children, x$grobs)) {
            if (!is.null(children)) {
                for (child in children) out <- c(out, found(child))
            }
        }
        out
    }
    plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
        ggimage::geom_phylopic(ggplot2::aes(image = svg), size = 0.4)
    expect_length(found(ggplot2::ggplotGrob(plot)), 1L)
})
