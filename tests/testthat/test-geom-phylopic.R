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
