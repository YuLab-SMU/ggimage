## get_github_files() reads GitHub's contents API, which serves at most 1000
## entries per directory and reports that silently.  These tests inject the
## response through the `request` seam, so they need no network.

github_files_response <- function(n, prefix = "file-") {
    data.frame(name = paste0(prefix, seq_len(n)), stringsAsFactors = FALSE)
}

test_that("get_github_files() warns when the listing hits the 1000-entry cap", {
    result <- NULL
    expect_warning(
        result <- ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) github_files_response(1000L)
        ),
        "1000-entry cap"
    )

    ## the truncated list is still returned, and the warning says what is wrong
    expect_length(result, 1000L)
})

test_that("the warning names the repository and the path", {
    expect_warning(
        ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) github_files_response(1000L)
        ),
        "ionic-team/ionicons/src/svg"
    )
})

test_that("get_github_files() is silent below the cap", {
    expect_no_warning(
        result <- ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) github_files_response(999L)
        )
    )
    expect_length(result, 999L)

    expect_no_warning(
        ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) github_files_response(0L)
        )
    )
})

test_that("a failed listing names the URL instead of a raw parse error", {
    error <- tryCatch(
        ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) stop("rate limit exceeded")
        ),
        error = function(e) conditionMessage(e)
    )

    expect_match(error, "https://api.github.com/repos/ionic-team/ionicons/contents/src/svg")
    expect_match(error, "rate limit exceeded", fixed = TRUE)
    ## the message is not wrapped as a call, so it reads as a statement
    expect_no_match(error, "Error in")
})

test_that("get_github_files() builds the request URL from its arguments", {
    seen <- NULL
    ggimage:::get_github_files(
        "owner", "repo", "some/path", branch = "dev",
        request = function(url) {
            seen <<- url
            github_files_response(1L)
        }
    )

    expect_identical(
        seen,
        "https://api.github.com/repos/owner/repo/contents/some/path?ref=dev"
    )
})
