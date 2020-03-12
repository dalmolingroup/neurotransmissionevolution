#' @export
linMap <- function(x, from, to) approxfun(range(x), c(from, to))(x)

#' @export
darken <- function(color, intensity=0.5) rgb(t(col2rgb(color)*(1-intensity)/255))

#' @export
download_if_missing <- function(url, filename = basename(url)) {
  filename <- here::here("data-raw/download", filename)
  if (!file.exists(filename)) {
    download.file(url, filename)
  }
}

#' @export
custom_knit <- function(input, encoding, fig.path = "figs") {
  custom_options <- rmarkdown::knitr_options(
    opts_knit  = list(base.dir = here::here("manuscripts")),
    opts_chunk = list(fig.path = "figs/", dev = "pdf")
  )

  custom_format <- rmarkdown::output_format(
    knitr    = custom_options,
    pandoc   = rmarkdown::pandoc_options(to = "gfm", ext = ".md"),
    df_print = "kable"
  )

  rmarkdown::render(
    input,
    encoding       = encoding,
    output_dir     = here::here("manuscripts"),
    output_format  = custom_format
  )
}

custom_knit_old <- function(input, encoding) {
  knitr::opts_knit$set(base.dir = here::here("manuscripts"));
  knitr::opts_chunk$set(fig.path = "figs/");
  knitr::opts_chunk$set(dev = "pdf");
  knitr::knit(input, here::here("manuscripts", paste0(tools::file_path_sans_ext(basename(input)), ".md")))
}

custom_ezknit <- function(input, encoding) {
  ezknitr::ezknit(input, out_dir = here::here("manuscripts"), fig_dir = "figs", keep_html = F)
}
