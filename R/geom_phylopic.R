##' geom layer for using phylopic image
##'
##'
##' @title geom_phylopic
##' @inheritParams geom_pokemon
##' @param image_fun a function to process magick-image objects, default is
##' \code{function(x)magick::image_background(x, color = "none", flatten = FALSE)}.
##' @return ggplot2 layer
##' @export
##' @author Guangchuang Yu
geom_phylopic <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", 
                       image_fun = function(x)magick::image_background(x, color = "none", flatten = FALSE), ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, image_fun = image_fun, ..., .fun = phylopic)
}

##' download phylopic images
##'
##'
##' This function allows users to download phylopic images using phylopic id
##' @title download_phylopic
##' @param id phylopic id
##' @param destdir directory where the downloaded images are to be saved.
##' @param ... additional parameters passed to download.file
##' @param reuse whether to reuse existing non-empty regular files
##' @param lock_timeout maximum seconds to wait for a concurrent download lock
##' @param lock_poll seconds between lock acquisition attempts
##' @return a character string (or vector) with downloaded file path
##' @importFrom utils download.file
##' @export
##' @author Guangchuang Yu
download_phylopic <- function(id, destdir = ".", ..., reuse = TRUE,
                              lock_timeout = getOption(
                                  "ggimage.phylopic_lock_timeout", 30),
                              lock_poll = getOption(
                                  "ggimage.phylopic_lock_poll", 0.05)) {
    url <- phylopic(id)
    n <- basename(url)
    destfile <- rep(NA_character_, length(url))
    names(destfile) <- names(url)

    valid <- !is.na(url) & nzchar(url)
    ## Key the destination on the RESOLVED image rather than on the input
    ## string: a name and its uid then share one file instead of downloading
    ## the same image twice, and no input can put a separator or a space into
    ## the file name.  `.phylopic_dest_name()` falls back to a digest of the
    ## input when the URL does not carry a usable uid.
    destfile[valid] <- file.path(destdir,
                                 .phylopic_dest_name(url[valid], id[valid]))
    pending <- which(valid)
    if (!length(pending)) return(invisible(destfile))

    reuse <- isTRUE(reuse)
    lock_timeout <- suppressWarnings(as.numeric(lock_timeout)[1L])
    if (length(lock_timeout) != 1L || is.na(lock_timeout) ||
        !is.finite(lock_timeout) || lock_timeout < 0) lock_timeout <- 30
    lock_timeout <- min(lock_timeout, 300)
    lock_poll <- suppressWarnings(as.numeric(lock_poll)[1L])
    if (length(lock_poll) != 1L || is.na(lock_poll) ||
        !is.finite(lock_poll) || lock_poll <= 0) lock_poll <- 0.05
    lock_poll <- min(lock_poll, 1)

    ## Duplicate input IDs can point to the same destination.  Download each
    ## destination at most once, without introducing parallel network calls.
    pending <- pending[!duplicated(destfile[pending])]
    for (i in pending) {
        target <- destfile[[i]]
        parent <- dirname(target)
        if (!dir.exists(parent)) {
            stop("Phylopic download directory does not exist: ", parent,
                 call. = FALSE)
        }
        lockdir <- paste0(target, ".lock")
        acquired <- .phylopic_lock_acquire(lockdir, lock_timeout, lock_poll)
        if (!acquired) {
            if (reuse && .phylopic_regular_file(target)) next
            stop("timed out waiting for Phylopic download lock: ", target,
                 call. = FALSE)
        }

        tryCatch({
            ## Check again after acquiring the lock.  A peer may have completed
            ## the file while this call was waiting.
            if (reuse && .phylopic_regular_file(target)) next
            if (file.exists(target) && isTRUE(file.info(target)$isdir)) {
                stop("Phylopic download target is a directory: ", target,
                     call. = FALSE)
            }

            temp <- tempfile(
                pattern = paste0(".", basename(target), "-"),
                tmpdir = parent
            )
            on.exit(unlink(temp, force = TRUE), add = TRUE)
            .phylopic_download(url[[i]], temp, ...)
            if (!.phylopic_regular_file(temp)) {
                stop("Phylopic download did not produce a non-empty file: ",
                     target, call. = FALSE)
            }
            ## temp and target share a directory, so rename is atomic on the
            ## supported filesystems.  No caller can observe a partial target.
            if (!file.rename(temp, target)) {
                stop("could not atomically install Phylopic download: ", target,
                     call. = FALSE)
            }
        }, finally = {
            unlink(lockdir, recursive = TRUE, force = TRUE)
        })
    }
    invisible(destfile)
}

.phylopic_regular_file <- function(path) {
    if (length(path) != 1L || is.na(path) || !file.exists(path)) {
        return(FALSE)
    }
    info <- file.info(path)
    isTRUE(!is.na(info$size) && info$size > 0 &&
           !is.na(info$isdir) && !info$isdir)
}

## One file name per distinct downloaded image.  PhyloPic vector URLs look
## like ".../images/<uid>/vector.svg", so the uid is the parent directory of
## the file name; the extension of the URL is kept.  Anything that does not
## yield a usable uid -- a mocked URL, a local file, an unexpected shape --
## falls back to a digest of the input, which is still stable across calls and
## still free of separators.
.phylopic_dest_name <- function(url, input) {
    url <- as.character(url)
    input <- as.character(input)
    ext <- sub("^.*\\.([^./]+)(?:\\?.*)?$", "\\1", basename(url))
    ## `sub()` returns the input unchanged when it does not match, which would
    ## silently reuse the whole URL as an extension.
    no_ext <- !grepl("\\.", basename(url))
    ext[no_ext] <- "svg"
    ext[is.na(ext) | !nzchar(ext)] <- "svg"

    uid <- basename(dirname(url))
    ## A real uid is a UUID; anything else is not the image identity.
    ok <- !is.na(uid) & grepl("^[A-Za-z0-9_-]+$", uid) &
        uid != "." & uid != ".." & uid != "/"

    out <- character(length(url))
    out[ok] <- paste0(uid[ok], ".", ext[ok])
    if (any(!ok)) {
        for (i in which(!ok)) {
            out[[i]] <- paste0("phylopic-", digest::digest(input[[i]]),
                               ".", ext[[i]])
        }
    }
    out
}

.phylopic_lock_acquire <- function(lockdir, timeout, poll) {
    if (.phylopic_lock_create(lockdir)) {
        return(TRUE)
    }
    ## An abandoned lock -- its owner died, or a leftover from an older build --
    ## is never going to release itself, so claim it for this process before
    ## waiting on anyone.
    if (.phylopic_lock_stale(lockdir) && .phylopic_lock_takeover(lockdir)) {
        return(TRUE)
    }
    started <- as.numeric(Sys.time())
    repeat {
        if (.phylopic_lock_create(lockdir)) {
            return(TRUE)
        }
        if (as.numeric(Sys.time()) - started >= timeout) {
            ## the owner may have died while this call waited
            if (.phylopic_lock_stale(lockdir) &&
                .phylopic_lock_takeover(lockdir)) {
                return(TRUE)
            }
            return(FALSE)
        }
        Sys.sleep(poll)
    }
}

## The lock is a directory stamped with its owner's PID.  The stamp is written
## as the lock is created, so a lock without a usable stamp can only be a
## leftover of an interrupted process.
.phylopic_lock_create <- function(lockdir) {
    if (!dir.create(lockdir, showWarnings = FALSE, recursive = FALSE)) {
        return(FALSE)
    }
    tryCatch(writeLines(as.character(Sys.getpid()), file.path(lockdir, "pid")),
             error = function(e) NULL)
    TRUE
}

## Remove a lock that looks abandoned, then re-create it stamped with this
## process' PID.  Racing takers resolve by who creates the directory first.
.phylopic_lock_takeover <- function(lockdir) {
    unlink(lockdir, recursive = TRUE, force = TRUE)
    .phylopic_lock_create(lockdir)
}

## The lock holds its owner's PID.  It is stale when that PID is gone, or the
## stamp is missing or unreadable -- e.g. a lock left by a build that predates
## the stamp, or was interrupted before it -- or when the stamp carries this
## very process' PID, or when the lock is older than a generous ceiling, in
## case a process with that PID lives on unrelated to this download.  On
## Windows, where liveness cannot be probed without killing the process, a
## foreign lock is judged by age alone.
.phylopic_lock_stale <- function(lockdir, max_age = 3600) {
    stamp <- tryCatch(readLines(file.path(lockdir, "pid"), warn = FALSE),
                      error = function(e) NULL, warning = function(w) NULL)
    if (length(stamp) != 1L) return(TRUE)
    pid <- suppressWarnings(as.integer(stamp))
    if (is.na(pid) || pid <= 0L) return(TRUE)
    if (pid == Sys.getpid()) return(TRUE)
    if (.Platform$OS.type != "windows" && !.phylopic_pid_alive(pid)) {
        return(TRUE)
    }
    born <- tryCatch(file.info(file.path(lockdir, "pid"))$mtime,
                     error = function(e) NULL)
    if (is.null(born) || is.na(born)) return(FALSE)
    as.numeric(Sys.time()) - as.numeric(born) > max_age
}

## Is the process running?  `ps` answers by its exit status, and neither form
## touches the process.  (tools::pskill cannot be used: it is silent about a
## missing process -- there is nothing to signal -- so it answers "alive" to
## everything, and on Windows it ignores the signal and kills the process.)
## Where no safe probe exists, report "alive" and let the age ceiling collect
## the lock instead.
.phylopic_pid_alive <- function(pid) {
    if (.Platform$OS.type == "windows") {
        return(TRUE)
    }
    status <- suppressWarnings(
        system2("ps", c("-p", pid, "-o", "pid="), stdout = FALSE, stderr = FALSE))
    if (!is.numeric(status) || length(status) != 1L || is.na(status)) {
        return(TRUE)
    }
    status == 0
}

.phylopic_download <- function(url, destfile, ...) {
    utils::download.file(url, destfile, ...)
}

phylopic <- function(id) {
    ## http://www.phylopic.org/assets/images/submissions/7fb9bea8-e758-4986-afb2-95a2c3bf983d.512.png
    #width <- getOption("phylopic_width")
    #if (is.null(width))
    #    width <- 256

    #basepath <- getOption('phylopic_dir')
    #if (is.null(basepath)) {
    #    basepath <- "http://phylopic.org/assets/images/submissions"
    #}         

    #url <- paste0(basepath, "/", id, ".", width, ".png")

    #if (is.null(basepath))
    #    url <- check_url(url)
    #return(url)
    basepath <- getOption('phylopic_dir')
    if (is.null(basepath)){
        basepath <- "https://images.phylopic.org/images/"
    }
    ## the option names a directory, so it composes with the id the same way no
    ## matter whether the caller wrote the trailing slash; without it, "dir" and
    ## an id joined to paste0 produce "dir<id>", which is not a URL anyone serves
    if (!grepl("/$", basepath)) {
        basepath <- paste0(basepath, "/")
    }

    id <- .autocomplete_uid(x = id)

    ## Keep unresolved IDs as NA rather than constructing an unusable URL.
    ## GeomImage already treats NA images as zero-width grobs.
    url <- rep(NA_character_, length(id))
    names(url) <- names(id)
    reference <- .phylopic_is_reference(id)
    valid <- !is.na(id) & nzchar(id)
    url[valid & !reference] <- paste0(basepath, id[valid & !reference],
                                      "/vector.svg")
    url[reference] <- id[reference]

    return(url)

}

# Universally unique identifier (uuid) of phylopic database
# is a 128-bit number. It has 32 alphanumeric characters in the
# form of 8-4-4-4-12.

## Normalize only the lookup key.  The first spelling seen is still passed to
## the resolver, so output values and names remain aligned with the input.
.phylopic_lookup_key <- function(x) {
    x <- as.character(x)
    key <- x
    valid <- !is.na(x)
    key[valid] <- tolower(trimws(x[valid]))
    key
}

## Successful name lookups are stable for a session and safe to reuse.  Missing
## results are intentionally not cached: a transient API failure should be
## retried on a later call.  The cache is private to the package namespace and
## includes the seed, so changing seed preserves phylopic_uid semantics.
.phylopic_uid_cache <- new.env(parent = emptyenv())

.phylopic_uid_cache_key <- function(name, seed) {
    paste0(.phylopic_lookup_key(name), "\r",
           paste(as.character(seed), collapse = ","))
}

.phylopic_uid_cache_get <- function(name, seed) {
    key <- .phylopic_uid_cache_key(name, seed)
    if (exists(key, envir = .phylopic_uid_cache, inherits = FALSE)) {
        get(key, envir = .phylopic_uid_cache, inherits = FALSE)
    } else {
        NULL
    }
}

.phylopic_uid_cache_set <- function(name, seed, uid) {
    if (length(uid) == 1L && !is.na(uid) && nzchar(uid)) {
        assign(.phylopic_uid_cache_key(name, seed), uid,
               envir = .phylopic_uid_cache)
    }
    invisible(uid)
}

## Primarily useful for tests and for callers that deliberately want to force
## fresh API lookups during a long-running session.
.phylopic_uid_cache_clear <- function() {
    rm(list = ls(envir = .phylopic_uid_cache, all.names = TRUE),
       envir = .phylopic_uid_cache)
    invisible(NULL)
}

## A value that already points at an image -- a local file such as the ones
## download_phylopic() returns, or an absolute URL -- needs no lookup and is
## drawn as it stands.
.phylopic_is_reference <- function(x) {
    !is.na(x) & nzchar(x) & (grepl("^[a-zA-Z][-+.a-zA-Z0-9]*://", x) |
                             file.exists(x))
}

.autocomplete_uid <- function(x){
    input_names <- names(x)
    x <- as.character(x)
    result <- rep(NA_character_, length(x))
    names(result) <- input_names
    if (length(x) == 0L) {
        return(result)
    }

    key <- .phylopic_lookup_key(x)
    missing <- is.na(x) | !nzchar(trimws(x))
    reference <- .phylopic_is_reference(x)
    result[reference] <- x[reference]
    uuid <- vapply(seq_along(x), function(index) {
        i <- x[[index]]
        if (missing[[index]] || reference[[index]]) {
            FALSE
        } else {
            x1 <- strsplit(i, split = '-', fixed = TRUE)[[1]]
            length(x1) == 5L &&
                identical(as.integer(nchar(x1)), c(8L, 4L, 4L, 4L, 12L))
        }
    }, logical(1))
    result[uuid] <- x[uuid]

    lookup <- which(!missing & !uuid & !reference)
    if (length(lookup) == 0L) {
        return(result)
    }

    ## Resolve only the first occurrence of each normalized name, then map the
    ## result back to every input row.  This keeps order, names, and per-row NA
    ## behavior unchanged while avoiding repeated API requests.
    unique_keys <- unique(key[lookup])
    for (lookup_key in unique_keys) {
        indices <- lookup[key[lookup] == lookup_key]
        first <- indices[[1L]]
        uid <- tryCatch(phylopic_uid(name = x[[first]])$uid,
                        error = function(e) NA_character_)
        if (length(uid) == 0L || is.na(uid[[1]]) ||
            !nzchar(as.character(uid[[1]]))) {
            uid <- NA_character_
        } else {
            uid <- as.character(uid[[1]])
        }
        result[indices] <- uid
    }
    result
}


##' obtaion suggestions for full names based on partial text name
##'
##'
##' @title autocomplete_name
##' @param name partial text name
##' @param ... additional parameters
##' @return scientific name
##' @export
autocomplete_name <- function(name, ...){
    x <- lapply(name, .autocomplete_name_search)
    x <- .autocomplete_name_bind(x)
    return(x)
}

## A name that matches nothing contributes a zero-row (or all-NA) result rather
## than aborting the batch, so the names that did match are still returned.
.autocomplete_name_search <- function(name, ...){
    x <- gsub("[^a-zA-Z]+", "%20", tolower(name))
    url <- paste0('https://api.phylopic.org/autocomplete?query=', x)
    res <- suppressWarnings(tryCatch(jsonlite::fromJSON(url),
                    error = function(e) return(NULL)))
    if (is.null(res) || length(res$matches)==0){
         warning(paste0("No matching names found for ", name, ". \n",
                    "Ensure provided name is a valid taxonomic name or ",
                    "try a species/genus resolution name."
                    ), call. = FALSE)
         x <- data.frame(name = as.character(name), match_name = NA_character_,
                         stringsAsFactors = FALSE)
    }else{
        x <- res$matches
        x <- data.frame(name = name, match_name = x, stringsAsFactors = FALSE)
    }
    return(x)
}

## `do.call(rbind, x)` refuses frames with different columns, which turned one
## partial response into a failure of the whole call.  Union the columns and
## fill the missing ones with NA instead; an empty list yields no rows.
.autocomplete_name_bind <- function(x) {
    x <- x[!vapply(x, is.null, logical(1))]
    if (length(x) == 0L) {
        return(data.frame(name = character(), match_name = character(),
                          stringsAsFactors = FALSE))
    }
    columns <- unique(unlist(lapply(x, names), use.names = FALSE))
    rows <- lapply(x, function(frame) {
        frame <- as.data.frame(frame, stringsAsFactors = FALSE)
        missing <- setdiff(columns, names(frame))
        for (column in missing) frame[[column]] <- NA
        frame[columns]
    })
    do.call(rbind, rows)
}

# phylopic_valid_id <- function(id) {
#     res <- vapply(id, phylopic_valid_id_item, character(1))
#     i <- which(res == "")
#     res[i] <- NA
#     return(res)
# }
# 
# phylopic_valid_id_item <- function(id) {
#     url <- paste0("http://phylopic.org/api/a/name/", id,
#                   "/images?subtaxa=true&supertaxa=true&options=pngFiles+canoicalName+json")
#     res <- tryCatch(jsonlite::fromJSON(url)$result,
#                     error = function(e) return(NULL))
#     if (is.null(res)) return("")
# 
#     if (length(res$same) > 0) {
#         taxa <- res$same
#     } else if (length(res$supertaxa) > 0) {
#         taxa <- res$supertaxa
#     } else if (length(res$subtaxa) > 0){
#         taxa <- res$subtaxa
#     } else if (length(res$other) > 0) {
#         taxa <- res$other
#     } else {
#         return("")
#     }
# 
#     uid <- taxa$uid[1]
#     return(uid)
# }

##' query phylopic to get uid from scientific name
##'
##' 
##' @title phylopic_uid
##' @param name scientific name
##' @param seed The random seed to use to generate the same uid,
##' because a name might have many uid, the function will extract one
##' of them randomly, default is 123.
##' @return phylopic uid
##' @export
##' @author Guangchuang Yu
phylopic_uid <- function(name, seed=123) {
    ## Missing names and incomplete API responses are returned as NA.  This
    ## preserves the input length and lets callers process a batch without
    ## losing the names that did resolve.
    name_input <- name
    name <- as.character(name)
    uid <- rep(NA_character_, length(name))
    if (length(name) == 0L) {
        return(data.frame(name = name_input, uid = uid,
                          stringsAsFactors = FALSE))
    }

    key <- .phylopic_lookup_key(name)
    missing <- is.na(name) | !nzchar(trimws(name))
    lookup <- which(!missing)
    for (lookup_key in unique(key[lookup])) {
        indices <- lookup[key[lookup] == lookup_key]
        first <- indices[[1L]]
        cached <- .phylopic_uid_cache_get(name[[first]], seed)
        if (!is.null(cached)) {
            value <- cached
        } else {
            value <- tryCatch(phylopic_uid_item(name[[first]], seed = seed),
                              error = function(e) NA_character_)
            if (length(value) != 1L || is.na(value) || !nzchar(value)) {
                value <- NA_character_
            } else {
                value <- as.character(value)
                .phylopic_uid_cache_set(name[[first]], seed, value)
            }
        }
        uid[indices] <- value
    }
    data.frame(name = name_input, uid = uid, stringsAsFactors = FALSE)
}

## Keep JSON parsing behind a small helper so malformed responses have one
## well-defined outcome and the parsing policy can be tested without a network.
.phylopic_from_json <- function(url) {
    suppressWarnings(tryCatch(jsonlite::fromJSON(url),
                               error = function(e) NULL))
}

.phylopic_build <- function(res) {
    if (!is.list(res) || is.null(res$build) || length(res$build) == 0L) {
        return(NULL)
    }

    build <- res$build[[1L]]
    if (length(build) != 1L || is.na(build) || !nzchar(as.character(build))) {
        return(NULL)
    }
    as.character(build)
}

.phylopic_vector_hrefs <- function(x) {
    if (is.null(x)) {
        return(character())
    }
    if (is.data.frame(x)) {
        x <- as.list(x)
    }
    if (!is.list(x)) {
        return(character())
    }

    ## The default jsonlite simplification returns data frames, while a
    ## mocked/unsimplified response can be a list of item objects.  Walking
    ## both shapes keeps missing optional fields harmless.
    if (!is.null(x$href) && is.character(x$href)) {
        return(unname(x$href))
    }
    if (!is.null(x$vectorFile)) {
        return(.phylopic_vector_hrefs(x$vectorFile))
    }
    if (!is.null(x$`_links`)) {
        return(.phylopic_vector_hrefs(x$`_links`))
    }
    unlist(lapply(x, .phylopic_vector_hrefs), use.names = FALSE)
}

.phylopic_extract_uids <- function(res) {
    if (!is.list(res) || is.null(res$`_embedded`) ||
        !is.list(res$`_embedded`) || is.null(res$`_embedded`$items)) {
        return(character())
    }

    href <- .phylopic_vector_hrefs(res$`_embedded`$items)
    href <- href[!is.na(href) & nzchar(href)]
    if (length(href) == 0L) {
        return(character())
    }

    pattern <- "^.*\\/images\\/([^\\/]+)\\/vector\\.svg(?:\\?.*)?$"
    is_vector <- grepl(pattern, href)
    matches <- regmatches(href[is_vector], regexec(pattern, href[is_vector]))
    uid <- vapply(matches, function(x) x[[2L]], character(1))
    uid[!is.na(uid) & nzchar(uid)]
}

##' @importFrom withr with_seed
phylopic_uid_item <- function(name, seed = 123, ...) {
    if (length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
        return(NA_character_)
    }

    baseurl <- 'https://api.phylopic.org/images?'
    nm <- gsub("[^a-zA-Z]+", "%20", tolower(name))

    url1 <- paste0(baseurl, "filter_name=", nm)
    res1 <- .phylopic_from_json(url1)
    build <- .phylopic_build(res1)
    if (is.null(build)) {
        return(NA_character_)
    }

    url2 <- paste0(baseurl, "embed_items=true&page=0&filter_name=", nm,
                   "&build=", build)
    res2 <- .phylopic_from_json(url2)
    uids <- .phylopic_extract_uids(res2)
    if (length(uids) == 0L) {
        return(NA_character_)
    }

    withr::with_seed(seed, sample(uids, 1))
}



