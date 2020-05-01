#' @export
linMap <- function(x, from, to) approxfun(range(x), c(from, to))(x)

#' @export
darken <- function(color, by = 0.5) {
  d <- rgb(t(col2rgb(color)*(1-by)/255))
  setNames(d, names(color))
}

#' @export
combine_scores <- function(string_interactions, sources = c("n","f","p","a","e","d","t")){
  sources <- paste0(sources, "score")
  cs <- 1 - string_interactions[, sources, drop = FALSE]
  cs <- apply(X = cs, MARGIN = 1, FUN = function(x) 1 - prod(x))
  return(cs)
}

#' @export
download_if_missing <- function(url, filename = basename(url)) {
  filename <- here::here("data-raw/download", filename)
  if (!file.exists(filename)) {
    download.file(url, filename)
  }
}

#' @export
print_dataframe_specification <- function(df, location = "", source = "", caption = NULL, label = NULL, extra_styling = NULL) {

    df <- cbind(`#` = 1:nrow(df), df) %>% dplyr::rename(
      `Col. name` = col_name,
      `Col. type` = col_type,
      `Used?`     = used,
      Example     = ex,
      Description = desc
    )

    ncol_df <- ncol(df)

    desc_size <- df[[ncol_df]] %>% nchar %>% max

    knitr::opts_knit$set(kable.force.latex = TRUE)

    kdf <- knitr::kable(df, caption = caption, label = label, booktabs = TRUE, longtable = FALSE)
    kdf <- kableExtra::kable_styling(kdf, position = "left", latex_options = c("striped", "HOLD_position", extra_styling))

    if(desc_size > 35){
      kdf <- kableExtra::column_spec(kdf, ncol_df, width = "18em")
    }

    kdf <- kableExtra::add_header_above(kdf, setNames(ncol_df, label), bold = TRUE, background = "#EEEEEE", monospace = T, font_size = 12)

    kdf <- kableExtra::footnote(kdf, source, general_title = "Source: ", title_format = "bold", footnote_as_chunk = T)
    kdf <- kableExtra::footnote(kdf, location, general_title = "Location: ", title_format = "bold", footnote_as_chunk = T)

    print(kdf)
}

#' @export
custom_knit <- function(input, encoding, fig.path = "figs/") {
  current_directory <- basename(dirname(input))
  current_file <- tools::file_path_sans_ext(basename(input))

  custom_options <- rmarkdown::knitr_options(
    opts_knit = list(
      base.dir          = here::here("manuscripts"),
      self.contained    = FALSE,
      kable.force.latex = TRUE
    ),
    opts_chunk = list(
      dev        = "pdf",
      fig.path   = paste0(fig.path, current_directory, ".", current_file, ".", collapse = ""),
      cache.path = paste0("cache/", current_file, "/latex/")
    )
    # knit_hooks = list(
    #   size = function(before, options, envir) {
    #     if (before) return(paste0("\n \\", options$size, "\n\n"))
    #     else return("\n\n \\tiny \n")
    #   }
    # )
  )

  pandoc_to <- "latex"
  pandoc_ext <- ".tex"

  custom_format <- rmarkdown::output_format(
    knitr    = custom_options,
    # pandoc   = rmarkdown::pandoc_options(to = "gfm", ext = ".md"),
    # pandoc   = rmarkdown::pandoc_options(to = "html", ext = ".html"),
    pandoc   = rmarkdown::pandoc_options(to = pandoc_to, ext = pandoc_ext),
    df_print = "kable"
  )

  rmarkdown::render(
    input,
    encoding       = encoding,
    output_file    = paste0(current_directory, ".", current_file, pandoc_ext, collapse = ""),
    output_dir     = here::here("manuscripts"),
    output_format  = custom_format,
    output_options = list(always_allow_html = "yes")
  )
}
