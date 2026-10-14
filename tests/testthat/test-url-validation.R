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

test_that("a zero ttl disables the URL validation cache entirely", {
    ## `.url_cache_get()`/`.url_cache_set()` guard on
    ## `policy$ttl <= 0 || policy$capacity <= 0` and must then never read or
    ## write an entry.  Deleting that guard would let an entry with a zero
    ## expiry be written and still be treated as a hit, so every call here has
    ## to reach the request function.
    calls <- 0L
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        200L
    }
    withr::local_options(
        ggimage.url_cache = list(ttl = 0, capacity = 2,
                                 clock = function() 100)
    )
    local_mocked_bindings(.url_request = request, .package = "ggimage")
    ggimage:::.url_cache_clear()
    on.exit(ggimage:::.url_cache_clear(), add = TRUE)

    target <- "https://example.invalid/no-ttl.png"
    for (i in 1:3) {
        expect_true(ggimage:::url.exists(target))
    }
    expect_identical(calls, 3L)
    ## Nothing may be left behind for a later call to find.
    expect_false(exists(target, envir = ggimage:::.url_validation_cache,
                        inherits = FALSE))
})

test_that("a zero capacity disables the URL validation cache entirely", {
    calls <- 0L
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        200L
    }
    withr::local_options(
        ggimage.url_cache = list(ttl = 100, capacity = 0,
                                 clock = function() 100)
    )
    local_mocked_bindings(.url_request = request, .package = "ggimage")
    ggimage:::.url_cache_clear()
    on.exit(ggimage:::.url_cache_clear(), add = TRUE)

    target <- "https://example.invalid/no-capacity.png"
    for (i in 1:3) {
        expect_true(ggimage:::url.exists(target))
    }
    expect_identical(calls, 3L)
    expect_false(exists(target, envir = ggimage:::.url_validation_cache,
                        inherits = FALSE))
})

test_that("a zero ttl does not break a positive-capacity cache for other URLs", {
    ## The guard has to be a real short-circuit rather than a broken policy:
    ## switching the option back on must restore caching.
    calls <- 0L
    request <- function(url, timeout, ...) {
        calls <<- calls + 1L
        200L
    }
    withr::local_options(
        ggimage.url_cache = list(ttl = 0, capacity = 2, clock = function() 100)
    )
    local_mocked_bindings(.url_request = request, .package = "ggimage")
    ggimage:::.url_cache_clear()
    on.exit(ggimage:::.url_cache_clear(), add = TRUE)

    target <- "https://example.invalid/toggled.png"
    expect_true(ggimage:::url.exists(target))
    expect_true(ggimage:::url.exists(target))
    expect_identical(calls, 2L)

    withr::local_options(
        ggimage.url_cache = list(ttl = 10, capacity = 2, clock = function() 100)
    )
    expect_true(ggimage:::url.exists(target))
    expect_true(ggimage:::url.exists(target))
    expect_identical(calls, 3L)
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
    ## an unknown code is dropped with a warning that names it; a missing (NA)
    ## code is a legitimate empty row and stays silent
    expect_warning(expect_true(is.na(ggimage:::worldcup2018("XX"))),
                   "outside the 32 qualifiers: XX")
    expect_no_warning(expect_true(is.na(ggimage:::worldcup2018(NA_character_))))
    expect_identical(
        unname(suppressWarnings(ggimage:::worldcup2018(c("AR", "XX")))),
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
        c("https://ionic.io/ionicons/ionicons/svg/add.svg", "https://ionic.io/ionicons/ionicons/svg/add.svg")
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
            "https://ionic.io/ionicons/ionicons/svg/add.svg",
            "https://raw.githubusercontent.com/jdecked/twemoji/main/assets/72x72/1f600.png",
            "https://raw.githubusercontent.com/Templarian/slack-emoji-pokemon/master/emojis/pikachu.png"
        )
    )
})

test_that("flag_url() returns NA for codes that cannot name a flag file", {
    ## Regression: an NA or empty code used to be upper-cased into the literal
    ## "NA", and the region-flags repository really contains NA.png (Namibia),
    ## so a missing code silently drew the Namibian flag.  Anything that cannot
    ## be a country or subdivision code must yield NA instead -- which the
    ## drawing layer skips -- rather than a URL built from garbage.
    bad <- c(NA_character_, "", " ", "us/ca", "../x")
    expect_true(all(is.na(ggimage:::flag_url(bad))))
    expect_length(ggimage:::flag_url(bad), length(bad))

    ## The specific failure mode this guards against: a missing code must not
    ## resolve to Namibia.
    expect_false(any(grepl("/NA\\.png$", na.omit(ggimage:::flag_url(bad)))))

    ## Other malformed shapes: over-long codes, digits-only, traversal, spaces,
    ## punctuation, and multi-slash paths.
    also_bad <- c("USA", "1", "2A", ".", "..", "us/ca/gb", "u s", "us-",
                  "US-", "-US", "us.ca", "\u00e9\u00e9", "null")
    expect_true(all(is.na(ggimage:::flag_url(also_bad))))
})

test_that("flag_url() still builds real country and subdivision codes", {
    base <- "https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png"
    expect_identical(ggimage:::flag_url("us"), paste0(base, "/US.png"))
    expect_identical(ggimage:::flag_url("GB-ENG"), paste0(base, "/GB-ENG.png"))
    ## case-insensitive, as before the fix
    expect_identical(ggimage:::flag_url("gb-eng"), paste0(base, "/GB-ENG.png"))
    ## "NA" is a real ISO code (Namibia) and must keep working
    expect_identical(ggimage:::flag_url("NA"), paste0(base, "/NA.png"))

    mixed <- ggimage:::flag_url(c("us", NA_character_, "GB-ENG", ""))
    expect_identical(
        mixed,
        c(paste0(base, "/US.png"), NA_character_, paste0(base, "/GB-ENG.png"),
          NA_character_)
    )
})

test_that("flag() drops invalid codes without spending a request on them", {
    checked <- character()
    exists <- function(url, ...) {
        checked <<- c(checked, url)
        TRUE
    }
    local_mocked_bindings(url.exists = exists, .package = "ggimage")

    base <- "https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png"
    result <- ggimage:::flag(c("us", NA_character_, "", "../x"))
    expect_identical(result, c(paste0(base, "/US.png"), NA_character_,
                               NA_character_, NA_character_))
    ## only the one buildable URL reaches validation
    expect_identical(checked, paste0(base, "/US.png"))
})
