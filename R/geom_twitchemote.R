# ##' geom layer for using Twitch emotes
# ##'
# ##'
# ##' @title geom_twitchemotes
# ##' @inheritParams geom_pokemon
# ##' @return ggplot2 layer
# ##' @export
# ##' @author Brendan Rocks
# geom_twitchemote <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
#                              na.rm=FALSE, by="width", ...) {
#     geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = twitchemote)
# }

# twitchemote <- function(twitchemotes) {
#     emote_lookup <- list.twitchemote()
#     if (is.null(emote_lookup)) return(NULL)

#     emote_ids <- emote_lookup[tolower(twitchemotes)]

#     ## Return a URL for the emote itself
#     url <- paste0("https://static-cdn.jtvnw.net/emoticons/v1/", emote_ids, "/1.0")

#     check_url(url)
# }

# ## https://github.com/GuangchuangYu/ggimage/pull/12#issuecomment-569788906
# ##' @importFrom jsonlite fromJSON
# list.twitchemote <- function() {
#     ## Obtain a list mapping emote text to emote ids
#     emote_list <- tryCatch(jsonlite::fromJSON("https://api.twitchemotes.com/api/v4/channels/0"),
#                            error = function(e) NULL)
#     if (is.null(emote_list)) {
#         warning(paste("--> The api version is gone.\n",
#                       "--> Please check https://twitchemotes.com/apidocs\n"))
#         return(NULL)
#     }

#     emotes <- emote_list$emotes
#     id <- emotes$id
#     names(id) <- tolower(emotes$code)
#     return(id)
# }
