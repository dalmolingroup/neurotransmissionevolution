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
