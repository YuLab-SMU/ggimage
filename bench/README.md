# Developer benchmarks

These scripts provide a small, offline-first performance baseline for the image
geoms. They are intentionally kept outside the package runtime and are excluded
from source packages by `.Rbuildignore`. No benchmark dependency is added to
`DESCRIPTION`.

## Requirements

Install the current checkout first (or load it with your usual development
workflow), then run the scripts from the repository root:

```sh
R CMD INSTALL .
Rscript bench/bench-geom-image.R
Rscript bench/bench-geom-image-repel.R
```

Both scripts require `ggplot2` and the locally installed `ggimage`. The
[`bench`](https://cran.r-project.org/package=bench) package is optional. When it
is unavailable, the scripts use base R's `system.time()` and report
`backend = "base::system.time"`; install `bench` when you need more stable
iteration statistics:

```sh
R -e 'install.packages("bench")'
```

The default is three timed iterations per case. Set
`GGIMAGE_BENCH_ITERATIONS` to a positive integer to change this (for example,
`GGIMAGE_BENCH_ITERATIONS=10 Rscript bench/bench-geom-image.R`). For a quick
smoke run, `GGIMAGE_BENCH_SIZES` accepts a comma-separated subset while keeping
the default coverage unchanged (for example,
`GGIMAGE_BENCH_SIZES=1,10 GGIMAGE_BENCH_ITERATIONS=1 Rscript bench/bench-geom-image.R`).
Redirect the plain-text table to a file if you want to retain a run; the scripts
do not write artifacts by themselves.

## What is measured

`bench-geom-image.R` builds a `ggplotGrob()` for `geom_image()` at `n = 1, 10,
100, 1000`. It compares repeated use of one local image with unique local file
paths, `use_cache = TRUE` and `FALSE`, and size-based dimensions with explicit
`width`/`height`. “Unique” means unique temporary local paths; the files are
copies of the bundled R logo, so no network access is involved.

`bench-geom-image-repel.R` builds a `ggplotGrob()` for
`geom_image_repel()` at `n = 10, 50, 100, 250`, using deterministic dense and
sparse layouts and `max.iter = 1, 10, 100`. The image is local and caching is
left enabled so the result focuses on layout cost after image preparation.

## Interpreting results

Compare medians within the same machine and R session. Growth with `n` is the
most useful first signal: the repel layer intentionally has pairwise work, so
dense cases and larger `max.iter` values should scale more steeply than sparse
cases. For `geom_image()`, the repeated/local-cache case is the warm-cache
reference; unique paths or `use_cache = FALSE` include more image preparation.
Explicit dimensions avoid the size-to-image-aspect calculation and are useful
when isolating that overhead, but they do not make a visual equivalence claim
for all source images.

These are tracking benchmarks, not a release gate. Repeat a suspicious result,
keep the same device/backend, and record the R, ggplot2, ggimage, and operating
system versions alongside any regression report. URL and Phylopic behavior is
not exercised: keeping the default run offline prevents network latency from
being mistaken for geometry performance. Network/request-count experiments
should be added as a separate mocked benchmark rather than enabled here.
