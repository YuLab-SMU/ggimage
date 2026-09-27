.url_bound_number <- function(value, default, lower, upper) {
    value <- suppressWarnings(as.numeric(value)[1L])
    if (is.na(value) || !is.finite(value)) {
        value <- default
    }
    min(max(value, lower), upper)
}

.url_bound_integer <- function(value, default, lower, upper) {
    value <- suppressWarnings(as.integer(value)[1L])
    if (is.na(value)) {
        value <- default
    }
    min(max(value, lower), upper)
}

.url_validation_cache <- new.env(parent = emptyenv())
.url_validation_cache_meta <- new.env(parent = emptyenv())
assign("sequence", 0L, envir = .url_validation_cache_meta)

.url_cache_policy <- function() {
    nested <- getOption("ggimage.url_cache", NULL)
    if (!is.list(nested)) nested <- list()

    pick <- function(option, name, default) {
        value <- getOption(option, NULL)
        if (is.null(value)) value <- nested[[name]]
        if (is.null(value)) value <- default
        value
    }

    ttl <- suppressWarnings(as.numeric(pick(
        "ggimage.url_cache_ttl", "ttl", 0
    ))[1L])
    capacity <- suppressWarnings(as.numeric(pick(
        "ggimage.url_cache_capacity", "capacity", 0
    ))[1L])
    if (length(ttl) != 1L || is.na(ttl) || !is.finite(ttl) || ttl < 0) {
        ttl <- 0
    }
    if (length(capacity) != 1L || is.na(capacity) || !is.finite(capacity) ||
        capacity < 0) {
        capacity <- 0
    }
    ## Keep accidental option values from turning this into an unbounded cache.
    ttl <- min(ttl, 3600)
    capacity <- min(floor(capacity), 10000)
    clock <- pick("ggimage.url_cache_clock", "clock", Sys.time)
    if (!is.function(clock)) clock <- Sys.time
    list(ttl = ttl, capacity = capacity, clock = clock)
}

.url_cache_now <- function(policy) {
    value <- tryCatch(policy$clock(), error = function(e) Sys.time())
    value <- suppressWarnings(as.numeric(value)[1L])
    if (length(value) != 1L || is.na(value) || !is.finite(value)) {
        value <- as.numeric(Sys.time())
    }
    value
}

.url_cache_clear <- function() {
    rm(list = ls(envir = .url_validation_cache, all.names = TRUE),
       envir = .url_validation_cache)
    assign("sequence", 0L, envir = .url_validation_cache_meta)
    invisible(NULL)
}

.url_cache_next_sequence <- function() {
    value <- get("sequence", envir = .url_validation_cache_meta,
                 inherits = FALSE) + 1L
    assign("sequence", value, envir = .url_validation_cache_meta)
    value
}

.url_cache_get <- function(url, policy, now = .url_cache_now(policy)) {
    if (policy$ttl <= 0 || policy$capacity <= 0 ||
        !exists(url, envir = .url_validation_cache, inherits = FALSE)) {
        return(FALSE)
    }
    item <- get(url, envir = .url_validation_cache, inherits = FALSE)
    if (!is.list(item) || is.null(item$expires) || item$expires <= now) {
        rm(list = url, envir = .url_validation_cache)
        return(FALSE)
    }
    item$used <- now
    item$sequence <- .url_cache_next_sequence()
    assign(url, item, envir = .url_validation_cache)
    TRUE
}

.url_cache_set <- function(url, policy, now = .url_cache_now(policy)) {
    if (policy$ttl <= 0 || policy$capacity <= 0) return(invisible(NULL))
    assign(url, list(expires = now + policy$ttl,
                     used = now, sequence = .url_cache_next_sequence()),
           envir = .url_validation_cache)
    keys <- ls(envir = .url_validation_cache, all.names = TRUE)
    if (length(keys) > policy$capacity) {
        entries <- mget(keys, envir = .url_validation_cache, inherits = FALSE)
        sequence <- vapply(entries, function(x) {
            value <- suppressWarnings(as.numeric(x$sequence)[1L])
            if (length(value) != 1L || is.na(value)) -Inf else value
        }, numeric(1))
        ## URL cache eviction is LRU; sequence resolves equal-clock ties.
        remove <- keys[order(sequence)[seq_len(length(keys) - policy$capacity)]]
        rm(list = remove, envir = .url_validation_cache)
    }
    invisible(NULL)
}

.url_request <- function(url, timeout) {
    ## httr remains a Suggests dependency.  URL validation is therefore
    ## deliberately optional: callers get FALSE when it is unavailable.
    if (!requireNamespace("httr", quietly = TRUE)) {
        return(NA_integer_)
    }

    response <- httr::HEAD(url, httr::timeout(timeout))
    httr::status_code(response)
}

url.exists <- function(url,
                       timeout = getOption("ggimage.url_timeout", 5),
                       max_retries = getOption("ggimage.url_max_retries", 3L),
                       backoff = getOption("ggimage.url_backoff", 0.1),
                       max_backoff = getOption("ggimage.url_max_backoff", 1),
                       request = .url_request) {
    if (length(url) != 1L || is.na(url) || !nzchar(as.character(url))) {
        return(FALSE)
    }

    ## Only the default request path participates in the process-local cache.
    ## In particular, check_url(url, url_exists = mock) remains a pure injection
    ## seam for tests and callers; mocked request functions are never cached by
    ## accident.  A caller that replaces .url_request explicitly can still test
    ## this cache without making a network request.
    use_cache <- missing(request)
    policy <- if (use_cache) .url_cache_policy() else NULL
    now <- if (use_cache && policy$ttl > 0 && policy$capacity > 0) {
        .url_cache_now(policy)
    } else {
        NA_real_
    }
    if (use_cache && .url_cache_get(as.character(url), policy, now)) {
        return(TRUE)
    }

    timeout <- .url_bound_number(timeout, default = 5, lower = 0.001, upper = 60)
    max_retries <- .url_bound_integer(max_retries, default = 3L, lower = 0L, upper = 5L)
    backoff <- .url_bound_number(backoff, default = 0.1, lower = 0, upper = 1)
    max_backoff <- .url_bound_number(max_backoff, default = 1, lower = 0, upper = 5)

    attempt <- 0L
    repeat {
        status <- tryCatch(
            as.integer(request(as.character(url), timeout = timeout))[1L],
            error = function(e) NA_integer_
        )

        if (identical(status, 200L)) {
            if (use_cache) .url_cache_set(as.character(url), policy, now)
            return(TRUE)
        }
        if (!identical(status, 502L) || attempt >= max_retries) {
            ## Failures, including timeouts, are deliberately not inserted into
            ## the cache: a transient outage must not become a sticky result.
            return(FALSE)
        }

        attempt <- attempt + 1L
        delay <- min(backoff * (2 ^ (attempt - 1L)), max_backoff)
        if (delay > 0) {
            Sys.sleep(delay)
        }
    }
}

check_url <- function(url, url_exists = url.exists, ...) {
    if (length(url) == 0L) {
        return(url)
    }

    ## Validate unique non-empty URLs only, then map the results back to the
    ## original vector so duplicate inputs keep their original positions.
    valid_input <- !is.na(url) & nzchar(as.character(url))
    if (any(valid_input)) {
        unique_urls <- unique(as.character(url[valid_input]))
        valid <- vapply(unique_urls, function(x) {
            ## A failed/mocked request must not abort a plotting layer.
            isTRUE(tryCatch(url_exists(x, ...), error = function(e) FALSE))
        }, logical(1))
        invalid <- valid_input
        invalid[valid_input] <- !valid[match(as.character(url[valid_input]), unique_urls)]
        url[invalid] <- NA_character_
    }

    ## Preserve the existing check_url contract for missing/empty values.
    url[!valid_input] <- NA_character_
    url
}

get_github_files <- function(owner, repo, path, branch = "main") {
    url <- paste0(
    "https://api.github.com/repos/",
    owner, "/", repo,
    "/contents/", path,
    "?ref=", branch
  )
  
  res <- jsonlite::fromJSON(url)
  
  return(res$name)
}

zeroGrob <- function() .zeroGrob

.zeroGrob <- grid::grob(cl = "zeroGrob", name = "NULL")

widthDetails.zeroGrob <- function(x) unit(0, "cm")
heightDetails.zeroGrob <- function(x) unit(0, "cm")
grobWidth.zeroGrob <- function(x) unit(0, "cm")
grobHeight.zeroGrob <- function(x) unit(0, "cm")
drawDetails.zeroGrob <- function(x, recording) {}
