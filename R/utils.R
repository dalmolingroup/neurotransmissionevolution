#' @export
linMap <- function(x, from, to) approxfun(range(x), c(from, to))(x)

#' @export
darken <- function(color, intensity=0.5) rgb(t(col2rgb(color)*(1-intensity)/255))

#' @export
download_if_missing <- function(url, filename) {
  if(missing(filename)) {
    filename <- basename(url)
  }
  filename <- here::here("data-raw/download", filename)
  if (!file.exists(filename)) {
    download.file(url, filename)
  }
}

#' @export
custom_knit <- function(input, encoding) {
  knitr::opts_knit$set(base.dir = here::here("manuscripts"));
  knitr::opts_chunk$set(fig.path = "figs/");
  knitr::knit(input, here::here("manuscripts", paste0(tools::file_path_sans_ext(basename(input)), ".md")))
}

custom_knit_old <- function(input, encoding) {
  ezknitr::ezknit(input, out_dir = here::here("manuscripts"), fig_dir = "figs", keep_html = F)
}

custom_knit2 <- function(input, encoding, base.dir = here::here("manuscripts"), fig.path = "figs") {
  knitr::opts_knit$set(base.dir = base.dir)
  knitr::opts_chunk$set(fig.path = fig.path)

  rmarkdown::render(
    input,
    encoding       = encoding,
    output_dir     = base.dir,
    output_format  = "md_document",
    output_options = list(
      variant  = "markdown_github",
      df_print = "kable",
      dev      = "pdf"
    )
  )
}
