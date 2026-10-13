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

    ## a `file://` URL carries no uid, so the name falls back to a digest of
    ## the input; it is stable across calls and carries no separator
    target <- result[[1L]]
    expect_identical(unname(result), c(target, target))
    expect_identical(readLines(target), "source")
})

test_that("download_phylopic installs downloads atomically", {
    tmp <- tempfile("ggimage-phylopic-atomic-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    id <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    target <- paste0(tmp, "/", id, ".svg")
    calls <- 0L
    local_mocked_bindings(
        phylopic = function(id) {
            url <- rep(paste0("https://images.phylopic.org/images/", id,
                              "/vector.svg"), length(id))
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

test_that("download_phylopic keys destfile on the resolved uid", {
    tmp <- tempfile("ggimage-phylopic-uid-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    uid_a <- "6b4e4b00-5f13-4967-b5aa-842f84052e7c"
    uid_b <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

    ## the resolver maps a name and its uid onto the same image, while two
    ## near-identical inputs ("x" / "xvector") map onto different ones
    local_mocked_bindings(
        phylopic = function(id) {
            id <- as.character(id)
            u <- ifelse(id %in% c(uid_b, "xvector"), uid_b, uid_a)
            url <- paste0("https://images.phylopic.org/images/", u,
                          "/vector.svg")
            names(url) <- names(id)
            url
        },
        .phylopic_download = function(url, destfile, ...) {
            writeLines("svg", destfile)
            0L
        },
        .package = "ggimage"
    )

    by_uid <- ggimage::download_phylopic(uid_a, destdir = tmp)
    by_name <- ggimage::download_phylopic("Canis lupus", destdir = tmp)
    ## the name input and the uid input land on ONE file
    expect_identical(unname(by_uid), unname(by_name))
    expect_identical(basename(by_uid), paste0(uid_a, ".svg"))

    ## one file per distinct uid, and no path separator in any name
    result <- ggimage::download_phylopic(c("x", "xvector", "a/b c"),
                                         destdir = tmp)
    expect_identical(basename(unname(result)),
                     c(paste0(uid_a, ".svg"), paste0(uid_b, ".svg"),
                       paste0(uid_a, ".svg")))
    expect_false(any(grepl("[/\\\\ ]", basename(unname(result)))))
    expect_identical(sort(list.files(tmp)),
                     sort(c(paste0(uid_a, ".svg"), paste0(uid_b, ".svg"))))
})

test_that("download_phylopic falls back to a digest for an unparsable URL", {
    ## a URL whose shape carries no uid keeps a stable, separator-free name
    expect_identical(
        ggimage:::.phylopic_dest_name("mock://vector.svg", "Canis lupus"),
        paste0("phylopic-", digest::digest("Canis lupus"), ".svg")
    )

    two <- ggimage:::.phylopic_dest_name(
        c("mock://vector.svg", "mock://other.svg"), c("a", "b")
    )
    expect_identical(two, vapply(c("a", "b"), function(x) {
        paste0("phylopic-", digest::digest(x), ".svg")
    }, character(1), USE.NAMES = FALSE))
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

test_that("autocomplete_name() keeps the names that did match", {
    ## one unknown name must not discard the whole batch, and the informative
    ## message stays as a warning, as phylopic_uid() already does
    local_mocked_bindings(
        .autocomplete_name_search = function(name, ...) {
            if (grepl("zzzz", name)) {
                warning("No matching names found for ", name, ". ", call. = FALSE)
                return(data.frame(name = as.character(name),
                                  match_name = NA_character_,
                                  stringsAsFactors = FALSE))
            }
            data.frame(name = name,
                       match_name = c(paste0(name, " a"), paste0(name, " b")),
                       stringsAsFactors = FALSE)
        },
        .package = "ggimage"
    )

    expect_warning(
        result <- ggimage::autocomplete_name(c("Canis", "zzzznotataxon")),
        "No matching names found for zzzznotataxon"
    )
    expect_s3_class(result, "data.frame")
    expect_identical(result$name, c("Canis", "Canis", "zzzznotataxon"))
    ## the bad name contributes an NA-ish row instead of aborting the call
    expect_identical(result$match_name, c("Canis a", "Canis b", NA_character_))
})

test_that("autocomplete_name() binds results with differing columns", {
    ## `do.call(rbind, x)` refused frames with different columns, which turned
    ## one partial response into a failure of the whole call
    local_mocked_bindings(
        .autocomplete_name_search = function(name, ...) {
            if (identical(name, "a")) {
                return(data.frame(name = "a", match_name = "A",
                                  stringsAsFactors = FALSE))
            }
            data.frame(name = "b", match_name = "B", extra = "E",
                       stringsAsFactors = FALSE)
        },
        .package = "ggimage"
    )

    result <- ggimage::autocomplete_name(c("a", "b"))
    expect_identical(result$name, c("a", "b"))
    expect_identical(result$match_name, c("A", "B"))
    expect_identical(result$extra, c(NA_character_, "E"))
})
