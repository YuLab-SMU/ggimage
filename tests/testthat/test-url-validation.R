test_that("check_url validates duplicate URLs only once", {
    checked <- character()
    exists <- function(url, ...) {
        checked <<- c(checked, url)
        TRUE
    }
    urls <- c(
        "https://example.invalid/one.png",
        "https://example.invalid/one.png",
        "https://example.invalid/two.png"
    )

    expect_identical(
        ggimage:::check_url(urls, url_exists = exists),
        urls
    )
    expect_identical(
        checked,
        c("https://example.invalid/one.png", "https://example.invalid/two.png")
    )
})

test_that("check_url maps one failed duplicate URL to NA", {
    checked <- character()
    exists <- function(url, ...) {
        checked <<- c(checked, url)
        !identical(url, "https://example.invalid/missing.png")
    }
    urls <- c(
        "https://example.invalid/missing.png",
        "https://example.invalid/missing.png",
        "https://example.invalid/ok.png"
    )

    result <- ggimage:::check_url(urls, url_exists = exists)
    expect_true(all(is.na(result[1:2])))
    expect_identical(result[[3]], urls[[3]])
    expect_length(checked, 2L)
})

test_that("url.exists catches request failures without retrying errors", {
    calls <- 0L
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        stop("simulated timeout")
    }

    expect_false(
        ggimage:::url.exists(
            "https://example.invalid/timeout.png",
            timeout = 0.01,
            max_retries = 5L,
            backoff = 0,
            request = request
        )
    )
    expect_identical(calls, 1L)
})

test_that("url.exists bounds retries for 502 responses", {
    calls <- 0L
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        502L
    }

    expect_false(
        ggimage:::url.exists(
            "https://example.invalid/bad-gateway.png",
            max_retries = 2L,
            backoff = 0,
            request = request
        )
    )
    ## One initial request plus the configured two retries.
    expect_identical(calls, 3L)
})

test_that("url.exists returns success after a bounded 502 retry sequence", {
    calls <- 0L
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        if (calls < 3L) 502L else 200L
    }

    expect_true(
        ggimage:::url.exists(
            "https://example.invalid/retry.png",
            max_retries = 2L,
            backoff = 0,
            request = request
        )
    )
    expect_identical(calls, 3L)
})

test_that("url.exists caches successful default requests across calls", {
    calls <- 0L
    now <- 100
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        200L
    }
    withr::local_options(
        ggimage.url_cache = list(ttl = 10, capacity = 2,
                                 clock = function() now)
    )
    local_mocked_bindings(.url_request = request, .package = "ggimage")
    ggimage:::.url_cache_clear()
    on.exit(ggimage:::.url_cache_clear(), add = TRUE)

    expect_true(ggimage:::url.exists("https://example.invalid/cached.png"))
    expect_true(ggimage:::url.exists("https://example.invalid/cached.png"))
    expect_identical(calls, 1L)

    now <- 111
    expect_true(ggimage:::url.exists("https://example.invalid/cached.png"))
    expect_identical(calls, 2L)
})

test_that("url validation cache evicts by bounded capacity", {
    calls <- character()
    request <- function(url, timeout, ...) {
        calls <<- c(calls, url)
        200L
    }
    withr::local_options(
        ggimage.url_cache = list(ttl = 100, capacity = 2,
                                 clock = function() 100)
    )
    local_mocked_bindings(.url_request = request, .package = "ggimage")
    ggimage:::.url_cache_clear()
    on.exit(ggimage:::.url_cache_clear(), add = TRUE)

    a <- "https://example.invalid/a.png"
    b <- "https://example.invalid/b.png"
    c <- "https://example.invalid/c.png"
    expect_true(ggimage:::url.exists(a))
    expect_true(ggimage:::url.exists(b))
    expect_true(ggimage:::url.exists(c))
    expect_true(ggimage:::url.exists(a))
    expect_identical(calls, c(a, b, c, a))
})

test_that("failed URL validation is not sticky in the cache", {
    calls <- 0L
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        if (calls == 1L) 500L else 200L
    }
    withr::local_options(
        ggimage.url_cache = list(ttl = 100, capacity = 2,
                                 clock = function() 100)
    )
    local_mocked_bindings(.url_request = request, .package = "ggimage")
    ggimage:::.url_cache_clear()
    on.exit(ggimage:::.url_cache_clear(), add = TRUE)

    target <- "https://example.invalid/transient.png"
    expect_false(ggimage:::url.exists(target))
    expect_true(ggimage:::url.exists(target))
    expect_identical(calls, 2L)
})

test_that("worldcup2018() flags come from the host flag() uses", {
    ## Regression: the 32 World Cup flags used to be hard-coded to
    ## upload.wikimedia.org, which is unreachable from some networks, so
    ## geom_worldcup2018() silently drew nothing there.  They must come from the
    ## same source as flag(), and building them must not cost a request per flag.
    local_mocked_bindings(
        url.exists = function(...) stop("worldcup2018() must not validate URLs"),
        .package = "ggimage"
    )

    codes <- ggimage:::list.worldcup2018()
    expect_length(codes, 32L)
    ## the mapping below is only meaningful while England is in the list
    expect_true("EN" %in% codes)

    urls <- ggimage:::worldcup2018(codes)
    expected <- codes
    expected[expected == "EN"] <- "GB-ENG"   # England has no ISO 3166-1 code

    expect_length(urls, 32L)
    expect_identical(names(urls), codes)
    expect_identical(
        unname(urls),
        paste0(
            "https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/",
            expected, ".png"
        )
    )
    expect_false(any(grepl("wikimedia", urls, fixed = TRUE)))
})

test_that("worldcup2018() maps unknown and missing codes to NA", {
    expect_true(is.na(ggimage:::worldcup2018("XX")))
    expect_true(is.na(ggimage:::worldcup2018(NA_character_)))
    expect_identical(
        unname(ggimage:::worldcup2018(c("AR", "XX"))),
        c(
            "https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/AR.png",
            NA_character_
        )
    )
})

test_that("remote wrappers share deduplicating URL validation", {
    checked <- character()
    exists <- function(url, ...) {
        checked <<- c(checked, url)
        TRUE
    }
    local_mocked_bindings(url.exists = exists, .package = "ggimage")

    expect_identical(
        ggimage:::flag(c("us", "us")),
        c(
            "https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/US.png",
            "https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/US.png"
        )
    )
    expect_identical(
        ggimage:::icon(c("add", "add")),
        c("https://ionicons.com/ionicons/svg/add.svg", "https://ionicons.com/ionicons/svg/add.svg")
    )
    expect_identical(
        ggimage:::emoji(c("1f600", "1f600")),
        c("https://raw.githubusercontent.com/jdecked/twemoji/main/assets/72x72/1f600.png", "https://raw.githubusercontent.com/jdecked/twemoji/main/assets/72x72/1f600.png")
    )
    expect_identical(
        ggimage:::pokemon(c("pikachu", "pikachu")),
        c(
            "https://raw.githubusercontent.com/Templarian/slack-emoji-pokemon/master/emojis/pikachu.png",
            "https://raw.githubusercontent.com/Templarian/slack-emoji-pokemon/master/emojis/pikachu.png"
        )
    )

    expect_identical(
        checked,
        c(
            "https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/US.png",
            "https://ionicons.com/ionicons/svg/add.svg",
            "https://raw.githubusercontent.com/jdecked/twemoji/main/assets/72x72/1f600.png",
            "https://raw.githubusercontent.com/Templarian/slack-emoji-pokemon/master/emojis/pikachu.png"
        )
    )
})
