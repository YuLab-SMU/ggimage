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
##' @return a character string (or vector) with downloaded file path
##' @importFrom utils download.file
##' @export
##' @author Guangchuang Yu
download_phylopic <- function(id, destdir = ".", ...) {
    url <- phylopic(id)
    n <- basename(url)
    destfile <- rep(NA_character_, length(url))
    names(destfile) <- names(url)

    valid <- !is.na(url) & nzchar(url)
    destfile[valid] <- paste0(destdir, '/', id[valid], n[valid])
    for (i in which(valid)) {
        utils::download.file(url[i], destfile[i], ...)
    }
    invisible(destfile)
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

    id <- .autocomplete_uid(x = id)

    ## Keep unresolved IDs as NA rather than constructing an unusable URL.
    ## GeomImage already treats NA images as zero-width grobs.
    url <- rep(NA_character_, length(id))
    names(url) <- names(id)
    valid <- !is.na(id) & nzchar(id)
    url[valid] <- paste0(basepath, id[valid], "/vector.svg")

    return(url)

}

# Universally unique identifier (uuid) of phylopic database 
# is a 128-bit number. It has 32 alphanumeric characters in the
# form of 8-4-4-4-12.
.autocomplete_uid <- function(x){
    x <- as.character(x)
    input_names <- names(x)
    result <- vapply(seq_along(x), function(index) {
        i <- x[[index]]
        if (is.na(i) || !nzchar(trimws(i))) {
            return(NA_character_)
        }

        x1 <- strsplit(i, split='-', fixed = TRUE)[[1]]
        is_uid <- length(x1) == 5 &&
            identical(as.integer(nchar(x1)), c(8L, 4L, 4L, 4L, 12L))
        if (is_uid) {
            return(i)
        }

        ## A name that cannot be resolved is represented by NA.  In
        ## particular, do not let one missing name abort a batch lookup.
        uid <- tryCatch(phylopic_uid(name = i)$uid,
                        error = function(e) NA_character_)
        if (length(uid) == 0L || is.na(uid[[1]]) ||
            !nzchar(as.character(uid[[1]]))) {
            NA_character_
        } else {
            as.character(uid[[1]])
        }
    }, character(1))
    names(result) <- input_names
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
    x <- do.call('rbind', x)
    return(x)
}

.autocomplete_name_search <- function(name, ...){
    x <- gsub("[^a-zA-Z]+", "%20", tolower(name))
    url <- paste0('https://api.phylopic.org/autocomplete?query=', x)
    res <- suppressWarnings(tryCatch(jsonlite::fromJSON(url),
                    error = function(e) return(NULL)))
    if (is.null(res) || length(res$matches)==0){
         stop(paste0("No matching names found for ", name, ". \n",
                    "Ensure provided name is a valid taxonomic name or ",
                    "try a species/genus resolution name."
                    )
         )
    }else{
        x <- res$matches
    }
    x <- data.frame(name=name, match_name=x)
    return(x)
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
    uid <- vapply(seq_along(name), function(index) {
        x <- name[[index]]
        value <- tryCatch(phylopic_uid_item(x, seed = seed),
                          error = function(e) NA_character_)
        if (length(value) != 1L || is.na(value) || !nzchar(value)) {
            NA_character_
        } else {
            as.character(value)
        }
    }, character(1))
    return(data.frame(name = name_input, uid = uid, stringsAsFactors = FALSE))
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



