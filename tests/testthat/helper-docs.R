## Helpers for asserting that the *documentation* matches the code.
##
## The suite runs from tests/testthat/ under R CMD check and from the package
## root under devtools::test(), so locate man/*.Rd through a candidate list
## rather than a single hardcoded path.

pkg_rd_file <- function(topic) {
    file <- paste0(topic, ".Rd")
    candidates <- c(
        file.path("..", "..", "man", file),
        file.path("..", "man", file),
        file.path("man", file)
    )
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0L) return(NULL)
    hit[[1L]]
}

count_braces <- function(line, brace) {
    hits <- gregexpr(brace, line, fixed = TRUE)[[1L]]
    if (identical(as.integer(hits), -1L)) 0L else length(hits)
}

## the body of one \section{...}{...}, brace-balanced
rd_section_lines <- function(rd, section) {
    lines <- readLines(rd, warn = FALSE)
    start <- grep(paste0("^\\\\section\\{", section, "\\}\\{"), lines)
    if (length(start) != 1L) return(NULL)
    depth <- 0L
    body <- character()
    for (i in start:length(lines)) {
        depth <- depth + count_braces(lines[[i]], "{") -
            count_braces(lines[[i]], "}")
        if (i > start) body <- c(body, lines[[i]])
        if (i > start && depth <= 0L) break
    }
    body
}

## the aesthetics listed in a `@section Aesthetics:` block, split into the
## bold (required) and the plain (optional) ones
rd_aesthetics <- function(rd) {
    body <- rd_section_lines(rd, "Aesthetics")
    if (is.null(body)) return(NULL)
    text <- paste(body, collapse = "\n")
    ## `\item ` with the trailing space, so the `\itemize{` line is not an item
    items <- regmatches(text, gregexpr("\\\\item [^\n]*", text))[[1L]]
    if (length(items) == 0L) return(NULL)
    bold <- grepl("\\\\strong", items)
    list(
        required = sub(".*\\\\code\\{([^}]+)\\}\\}.*", "\\1", items[bold]),
        optional = sub(".*\\\\code\\{([^}]+)\\}.*", "\\1", items[!bold])
    )
}
