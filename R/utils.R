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
  ezknitr::ezknit(input, out_dir = here::here("manuscripts"), fig_dir = "figs", keep_html = F)
}

custom_knit2 <- function(input, encoding) {
  knitr::opts_chunk$set(fig.path = "figsAAA")
  rmarkdown::render(
    input,
    encoding       = encoding,
    output_dir     = here::here("manuscripts"),
    output_format  = "md_document",
    output_options = list(
      variant  = "markdown_github",
      df_print = "kable",
      dev      = "pdf"
    )
    # fig_dir = "figs",
    # keep_html = F
  )
}


custom_knit3 <- function(input, encoding) {
  knitr::opts_knit$set(base.dir = "../manuscripts");
  knitr::opts_chunk$set(fig.path = "figsAAA");
  knitr::opts_knit
}
