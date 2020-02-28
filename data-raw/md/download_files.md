``` r
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

files
```

| filename          | url                                                                                                                                     |
|:------------------|:----------------------------------------------------------------------------------------------------------------------------------------|
| species.v11.0.txt | <a href="https://stringdb-static.org/download/species.v11.0.txt" class="uri">https://stringdb-static.org/download/species.v11.0.txt</a> |
| x                 | y                                                                                                                                       |
| x                 | y                                                                                                                                       |
| x                 | y                                                                                                                                       |
| x                 | y                                                                                                                                       |
| x                 | y                                                                                                                                       |
| x                 | y                                                                                                                                       |
| x                 | y                                                                                                                                       |
