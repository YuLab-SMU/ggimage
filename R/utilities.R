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
            return(TRUE)
        }
        if (!identical(status, 502L) || attempt >= max_retries) {
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
