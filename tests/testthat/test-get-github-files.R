## get_github_files() lists a directory through GitHub's trees API, which
## reports giving up in its `truncated` flag.  These tests inject the response
## through the `request` seam, so they need no network.

github_files_response <- function(n, prefix = "file-", truncated = FALSE) {
    list(truncated = truncated,
         tree = data.frame(path = paste0(prefix, seq_len(n), ".svg"),
                           stringsAsFactors = FALSE))
}

test_that("get_github_files() warns when GitHub truncates the listing", {
    result <- NULL
    expect_warning(
        result <- ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) github_files_response(1000L, truncated = TRUE)
        ),
        "truncated"
    )

    ## the truncated list is still returned, and the warning says what is wrong
    expect_length(result, 1000L)
    ## entries come back as basenames, which is what the callers' sub() expects
    expect_equal(result[1L], "file-1.svg")
})

test_that("the warning names the repository and the path", {
    expect_warning(
        ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) github_files_response(10L, truncated = TRUE)
        ),
        "ionic-team/ionicons/src/svg"
    )
})

test_that("get_github_files() is silent when nothing is truncated", {
    expect_no_warning(
        result <- ggimage:::get_github_files(
            "ionic-team", "ionicons", "src/svg",
            request = function(url) github_files_response(1357L)
        )
    )
    ## the whole directory comes back, not a 1000-entry slice of it
    expect_length(result, 1357L)

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

    expect_match(error, "https://api.github.com/repos/ionic-team/ionicons/git/trees/main:src/svg")
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
        "https://api.github.com/repos/owner/repo/git/trees/dev:some/path"
    )
})
