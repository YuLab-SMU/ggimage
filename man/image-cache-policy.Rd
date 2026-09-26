% Generated manually for the public image-cache policy API.
\name{image-cache-policy}
\alias{get_image_cache_policy}
\alias{set_image_cache_policy}
\title{Configure the global image cache policy}
\usage{
get_image_cache_policy()

set_image_cache_policy(
  capacity = NULL,
  ttl = NULL,
  eviction = NULL,
  base_capacity = NULL,
  transform_capacity = NULL,
  base_ttl = NULL,
  transform_ttl = NULL,
  clock = NULL
)
}
\arguments{
  \item{capacity}{Optional shared maximum number of entries in each cache.}
  \item{ttl}{Optional shared time-to-live in seconds.}
  \item{eviction}{Eviction strategy: \code{"lru"}, \code{"fifo"}, or \code{"none"}.}
  \item{base_capacity, transform_capacity}{Maximum entries for the base-image or transformed-image cache.}
  \item{base_ttl, transform_ttl}{Time-to-live in seconds for the base-image or transformed-image cache.}
  \item{clock}{Optional function returning the current time in seconds or a \code{POSIXct} value; useful for deterministic tests.}
}
\value{
\code{get_image_cache_policy()} returns the effective policy as a list.\code{set_image_cache_policy()} returns the previous policy invisibly.
}
\description{
The image and transformed-image caches are process-global. These helpers configure optional capacity and time-to-live limits while preserving the historical unlimited defaults. Cache entries are evicted least-recently-used by default when a finite capacity is configured.
}
\details{
The same settings can be supplied through options named \code{ggimage.image_cache_capacity}, \code{ggimage.image_cache_ttl}, \code{ggimage.image_cache_eviction}, and their \code{base} or \code{transform} variants. The default capacity and time-to-live are \code{Inf}.
}
\examples{
old <- set_image_cache_policy(capacity = 256, ttl = 3600)
get_image_cache_policy()
set_image_cache_policy(
  capacity = old$base_capacity,
  ttl = old$base_ttl,
  eviction = old$eviction
)
}
\keyword{internal}
