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
    destfile <- paste0(destdir, '/', id, n)

    for (i in seq_along(url)) {
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
    
    url <- paste0(basepath, id, "/vector.svg")
    if (is.null(basepath)){
        url <- check_url(url)
    }

    return(url)
    
}

# Universally unique identifier (uuid) of phylopic database 
# is a 128-bit number. It has 32 alphanumeric characters in the
# form of 8-4-4-4-12.
.autocomplete_uid <- function(x){
    x <- unlist(
        lapply(x, function(i){
        x1 <- unlist(strsplit(i, split='-'))
        flag1 <- length(x1) == 5
        if (!flag1){
            i <- phylopic_uid(name = i)$uid
        }else{
            flag2 <- all(nchar(x1) == c(8, 4, 4, 4, 12))
            if (!flag2){
                i <- phylopic_uid(name = i)$uid
            }
        }
        return(i)
      }
    )
    )
    return(x)
    
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
    ## if (length(name) == 1) {
    ##     res <- phylopic_uid_item(name)
    ## } else {
    ##     res <- lapply(name, phylopic_uid_item)
        
    ##     names(res) <- name
    ##     return(res)
    ## }

    res <- data.frame(name = name,
                      uid = vapply(name, phylopic_uid_item, seed=seed, character(1)))
    return(res)
}

##' @importFrom withr with_seed
phylopic_uid_item <- function(name, seed = 123, ...) {
    #x <- gsub("[^a-zA-Z]+", "+", name)
    #url <- paste0("http://phylopic.org/api/a/name/search?text=",
    #              x, "&options=scientific+json")
    #res <- jsonlite::fromJSON(url)$result[[1]]
    
    baseurl <- 'https://api.phylopic.org/images?'

    nm <- gsub("[^a-zA-Z]+", "%20", tolower(name))

    url1 <- paste0(baseurl, "filter_name=", nm)
    
    res <- suppressWarnings(tryCatch(jsonlite::fromJSON(url1), error = function(e)return(NULL)))

    url2 <- paste0(baseurl, "embed_items=true&page=0&filter_name=", nm, "&build=", res$build)


    res <- suppressWarnings(tryCatch(jsonlite::fromJSON(url2),
                    error = function(e) return(NULL)))
    if ("errors" %in% names(res) || is.null(res)){
        stop(paste0("Image resource of Phylopic database is not available for ", name, ". \n",
                    "Ensure provided name is a valid taxonomic name or ",
                    "try a species/genus resolution name. \n",
                    'Or using the autocomplete_name function to get the suggestions for the full name.'
                    )
        )
    }else{
        href <- res$`_embedded`$items$`_links`$vectorFile$href
        uids <- sub("/vector.svg", "", sub('+.*images/', "", href))
    }
    
    return(withr::with_seed(seed, sample(uids, 1)))
    #for (id in res$uid) {
    #    uid <- phylopic_valid_id(id)
    #    if (!is.na(uid))
    #        break
    #}
    #return(uid)

    ## uid <- phylopic_valid_id(res$uid)
    ## i <- which(!is.na(uid))
    ## res <- res[i, , drop = FALSE]
    ## res$uid <- uid[i]
    ## return(res)
}



