library(readr)
library(dplyr)
library(igraph)
library(tidylog)
library(magrittr)

if (!file.exists("data-raw/taxonomy")) {
  tmp <- tempfile()
  download.file("https://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump.tar.gz", tmp)
  untar(tmp, exdir = "data-raw/taxonomy")
  unlink(tmp)
}

if (!file.exists("data-raw/species.v11.0.txt")) {
  download.file("https://stringdb-static.org/download/species.v11.0.txt", "data-raw/species.v11.0.txt")
}

string_species <- read_tsv(
  "data-raw/species.v11.0.txt",
  skip = 1,
  col_names = c("taxid", "string_name"),
  col_types = cols_only("c", "-", "c")
)

ncbi_merged_ids <- read_delim(
  "data-raw/taxonomy/merged.dmp",
  delim = "|",
  trim_ws = TRUE,
  col_names = c("taxid","new_taxid"),
  col_types = cols_only("c", "c")
)

string_species %<>% left_join(ncbi_merged_ids) %>% mutate(new_taxid = coalesce(new_taxid, taxid))

string_updated_taxids <- string_species[["new_taxid"]]

ncbi_edgelist <- read_delim(
  "data-raw/taxonomy/nodes.dmp",
  skip = 1,
  delim = "|",
  trim_ws = TRUE,
  col_names = c("n1","n2"),
  col_types = cols(col_integer(), col_integer(), .default = col_skip())
)

ncbi_taxon_names <- read_delim(
  "data-raw/taxonomy/names.dmp",
  delim = "|",
  trim_ws = TRUE,
  col_names = c("name","ncbi_name","type"),
  col_types = cols_only("c", "c", "-", "c")
)

ncbi_taxon_names %<>% subset(type == "scientific name", c("name","ncbi_name"))

eukaryota_taxon_id <- subset(ncbi_taxon_names, ncbi_name == "Eukaryota", "name", drop = TRUE)

g <- graph_from_data_frame(ncbi_edgelist[,2:1], directed = TRUE, vertices = ncbi_taxon_names)

rm(ncbi_edgelist, ncbi_merged_ids)

eukaryote_root <- V(g)[eukaryota_taxon_id]
eukaryote_leaves <- V(g)[string_updated_taxids]

# not_found <- subset(string_species, !new_taxid %in% ncbi_taxon_names$name)

eukaryote_paths <- shortest_paths(g, from = eukaryote_root, to = eukaryote_leaves, mode = "out")$vpath

eukaryote_vertices <- eukaryote_paths %>% unlist %>% unique

eukaryote_tree <- induced_subgraph(g, eukaryote_vertices, impl = "create_from_scratch")

# ncbi_tree <- ape::as.phylo(get.edgelist(eukaryote_tree)) %>% ape::ladderize()
ncbi_tree <- treeio::as.phylo(eukaryote_tree) %>% ape::ladderize()

# plot(phylo_tree, type="cladogram")

string_eukaryotes <- string_species %>% filter(new_taxid %in% ncbi_tree$tip.label) %>% inner_join(ncbi_taxon_names, by = c("new_taxid" = "name"))

write.table(string_eukaryotes[,"ncbi_name"],"inst/extdata/string_eukaryotes_names.txt", col.names = F, row.names = F, quote = F)

usethis::use_data(ncbi_tree, overwrite = TRUE)
usethis::use_data(string_eukaryotes, overwrite = TRUE)
