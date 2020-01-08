library(ape)
library(phytools)
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

# annotting genera
species_dictionary %<>% mutate(genus_search = coalesce(timetree_name, ncbi_name) %>% strsplit(" ") %>% sapply("[", 1))

# unique genera
selected_genera <- species_dictionary[["genus_search"]] %>% unique

# ensuring a cleaner newick file with only necessary data
timetree_newick[["node.label"]] <- NULL
timetree_newick[["edge.length"]] <- NULL

# loading newick tree obtained from timetree
tree_85k <- read.tree("data-raw/Eukaryota_species.nwk")

# replacing timetree's underscores with spaces
tree_85k[["tip.label"]] %<>% str_replace_all("_", " ")

# storing genus
tree_85k[["tip.genus"]] <- sapply(strsplit(tree_85k[["tip.label"]]," "), "[", 1)
tree_85k_genera <- tree_85k[["tip.genus"]] %>% unique

# keeping only selected genera
tree_genus <- tree_85k %$% keep.tip(., tip.label[tip.genus %in% selected_genera])
tree_genus[["tip.genus"]] <- sapply(strsplit(tree_genus[["tip.label"]]," "), "[", 1)

# hack
# tree_genus$node.label[1] <- " "

# converting phylo to igraph
# graph_genus <- as.igraph.phylo(tree_genus, directed = TRUE)

# para cada especie nao encontrada
unfound_species <- species_dictionary %>% filter(is.na(timetree_name) & genus_search %in% tree_85k_genera)

for(i in 1:nrow(unfound_species)){
  # buscar todas as especies do mesmo genero na arvore
  tip_genus <- tree_genus[["tip.label"]] %>% strsplit("[_ ]") %>% sapply("[", 1)
  sisters <- tree_genus[["tip.label"]][tip_genus == unfound_species[[i, "genus_search"]]]
  # obter ancestral comum dessas especies
  where <- getMRCA(tree_genus, sisters %>% c(.[1]))
  # adicionar um nó para essa especie
  tree_genus %<>% bind.tip(tip.label = unfound_species[[i, "ncbi_name"]], where = where)
}

# for some reason bind.tip adds underscores to species names
tree_genus[["tip.label"]] %<>% str_replace_all("_", " ")

# filtrar arvore para especies encontradas + supostas
found_and_supposed_species <- species_dictionary$timetree_name %>% na.omit %>% c(unfound_species[["ncbi_name"]])
test <- keep.tip(tree_genus, found_and_supposed_species)

plot(test %>% rotatePhyloTree("Homo sapiens"), type = "cladogram", use.edge.length = F)

pdf("C:/R/test.pdf", width=20, height=75)
  plot(test %>% rotatePhyloTree("Homo sapiens"), type = "cladogram", use.edge.length = F)
dev.off()
