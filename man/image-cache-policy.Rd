% Generated manually for the public image-cache policy API.
\name{image-cache-policy}
\alias{get_image_cache_policy}
\alias{set_image_cache_policy}
\alias{get_image_cache_stats}
\alias{get_image_cache_diagnostics}
\alias{reset_image_cache_stats}
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
  bytes = NULL,
  base_bytes = NULL,
  transform_bytes = NULL,
  clock = NULL
)

get_image_cache_stats()
get_image_cache_diagnostics()
reset_image_cache_stats()
}
\arguments{
  \item{capacity,ttl}{Optional shared maximum number of entries in each cache.}
  \item{bytes}{Optional shared estimated byte cap for each cache. Estimates are based on R object size (with a pixel lower bound for magick images), not exact native memory.}
  \item{eviction}{Eviction strategy: \code{"lru"}, \code{"fifo"}, or \code{"none"}.}
  \item{base_capacity, transform_capacity}{Maximum entries for the base-image or transformed-image cache.}
  \item{base_ttl, transform_ttl}{Time-to-live in seconds for the base-image or transformed-image cache.}
  \item{base_bytes, transform_bytes}{Estimated byte caps for the base-image or transformed-image cache.}
  \item{clock}{Optional function returning the current time in seconds or a \code{POSIXct} value; useful for deterministic tests.}
}
\value{
\code{get_image_cache_policy()} returns the effective policy as a list.\code{set_image_cache_policy()} returns the previous policy invisibly. \code{get_image_cache_stats()} and \code{get_image_cache_diagnostics()} return a list containing named \code{base} and \code{transform} vectors with hit, miss, eviction, expiry, entry-count, and estimated-byte counters. \code{reset_image_cache_stats()} resets counters while retaining entries.
}
\description{
The image and transformed-image caches are process-global. These helpers configure optional capacity and time-to-live limits while preserving the historical unlimited defaults. Cache entries are evicted least-recently-used by default when a finite capacity is configured.
}
\details{
The same settings can be supplied through options named \code{ggimage.image_cache_capacity}, \code{ggimage.image_cache_ttl}, \code{ggimage.image_cache_bytes}, \code{ggimage.image_cache_eviction}, and their \code{base} or \code{transform} variants. The default capacity, time-to-live, and byte cap are \code{Inf}. Byte caps are approximate and do not include all native ImageMagick allocations. \code{clear_image_cache()} clears entries and resets diagnostics; \code{use_cache = FALSE} bypasses both cache entries and diagnostics.
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
