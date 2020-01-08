library(ape)
library(tidyverse)
library(magrittr)
library(igraph)

# loading species names and taxon ids
data(string_eukaryotes)

# loading newick tree obtained from timetree
timetree_newick <- read.tree("data-raw/timetree_eukaryotes.nwk")

# plot(timetree_newick %>% ladderize, type = "cladogram", use.edge.length = F)

# replacing timetree's underscores with spaces
timetree_newick[["tip.label"]] %<>% str_replace_all("_", " ")

# which timetree species' names exactly match with ncbi's
taxid_indexes <- timetree_newick[["tip.label"]] %>% match(string_eukaryotes[["ncbi_name"]])

# find out which timetree species names didn't exactly match ncbi's
unmatched_names <- timetree_newick[["tip.label"]] %>% magrittr::extract(taxid_indexes %>% is.na)
print(unmatched_names)

# manually creating lookup table to be joined
ncbi_to_timetree <- tribble(
  ~timetree_name,              ~ncbi_name,
  "Cercospora fijiensis",      "Pseudocercospora fijiensis",
  "Arthroderma benhamiae",     "Trichophyton benhamiae",
  "Macropus eugenii",          "Notamacropus eugenii",
  "Ostreococcus lucimarinus",  "Ostreococcus sp. 'lucimarinus'",
  "Oryza nivara",              "Oryza sativa f. spontanea"
)

# joining info
species_dictionary <- string_eukaryotes %>% left_join(ncbi_to_timetree)

# coalescing NAs to ncbi_name
species_dictionary %<>% mutate(timetree_name = coalesce(timetree_name, ncbi_name))
species_dictionary %<>% mutate(timetree_name = ifelse(timetree_name %in% timetree_newick[["tip.label"]], timetree_name, NA))

selected_genera <- species_dictionary[["timetree_name"]] %>%
  strsplit(" ") %>%
  sapply("[", 1) %>%
  unique

# ensuring a cleaner newick file with only necessary data
timetree_newick[["node.label"]] <- NULL
timetree_newick[["edge.length"]] <- NULL

# loading newick tree obtained from timetree
tree_85k <- read.tree("data-raw/Eukaryota_species.nwk")

# replacing timetree's underscores with spaces
tree_85k[["tip.label"]] %<>% str_replace_all("_", " ")

# storing genus
tree_85k[["tip.genus"]] <- sapply(strsplit(tree_85k[["tip.label"]]," "), "[", 1)

# keeping only selected genera
tree_genus <- tree_85k %$% keep.tip(., tip.label[tip.genus %in% selected_genera])

# hack
# tree_genus$node.label[1] <- " "

# converting phylo to igraph
graph_genus <- as.igraph.phylo(tree_genus, directed = TRUE)
