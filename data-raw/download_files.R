library(purrr)
library(tibble)

download_if_missing <- function(filename, url) {
  
}

files <- tribble(
  ~filename,           ~url,
  "species.v11.0.txt", "https://stringdb-static.org/download/species.v11.0.txt",
  "x",                 "y",
  "x",                 "y",
  "x",                 "y",
  "x",                 "y",
  "x",                 "y",
  "x",                 "y",
  "x",                 "y",
)