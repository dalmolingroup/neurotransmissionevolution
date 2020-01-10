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

# keeping track of found species
found_species <- species_dictionary %>% filter(!is.na(timetree_name) | genus_search %in% tree_85k_genera)
found_species %<>% mutate(forced_name = coalesce(timetree_name, ncbi_name))

# filtrar arvore para especies encontradas + supostas
tree_genus %<>% keep.tip(found_species[["forced_name"]])

# converting to ncbi taxids
tree_genus[["tip.label"]] <- found_species[["new_taxid"]][match(tree_genus[["tip.label"]], found_species[["forced_name"]])]

# ensuring a cleaner newick file with only necessary data
tree_genus[["node.label"]] <- NULL
tree_genus[["edge.length"]] <- NULL

plot(tree_genus %>% rotatePhyloTree("9606"), type = "cladogram", use.edge.length = F)

pdf("C:/R/416_test.pdf", width=20, height=75)
  plot(tree_genus %>% rotatePhyloTree("9606"), type = "cladogram", use.edge.length = F)
dev.off()

####################
## unfound genera
####################
data(ncbi_tree)

# converting ncbi phylo to igraph
graph_ncbi <- as.igraph.phylo(ncbi_tree, directed = TRUE)
plot(as.undirected(graph_ncbi), layout = layout_as_tree(graph_ncbi), vertex.label = NA, vertex.size=1)

# converting phylo to igraph
graph_genus <- as.igraph.phylo(tree_genus, directed = TRUE)
plot(as.undirected(graph_genus), layout = layout_as_tree(graph_genus), vertex.label = NA, vertex.size=1)

# para cada especie cujo genero nao está no timetree
unfound_genera <- species_dictionary %>% filter(is.na(timetree_name) & !genus_search %in% tree_85k_genera)

# procurar as duas espécies mais próximas, na árvore do ncbi, que estejam na arvore do timetree
tip_nodes <- V(graph_ncbi)[degree(graph_ncbi, mode = "out") == 0]

# undirected distances between all species nodes
tip_distances <- graph_ncbi %>% distances(v = tip_nodes, to = tip_nodes, mode = "all") %>% as_tibble(rownames = "from") %>% pivot_longer(-from, names_to = "to", values_to = "distance")

# removing self references
tip_distances %<>% filter(distance > 0)

# we only want to search for species of unfound genera
tip_distances %<>% inner_join(unfound_genera %>% select(new_taxid), by = c("from" = "new_taxid"))

# we only want to find species already present in the genus_tree
tip_distances %<>% inner_join(found_species %>% select(new_taxid), by = c("to" = "new_taxid")) %>% group_by(from) %>% top_n(-2, distance) %>% top_n(2, to)

# out distance matrix between all nodes in tree, needed to find MRCAs
out_distances <- graph_genus %>% distances(mode = "out")

# finding the mrca for each species of unfound genera
closest_relative <- tip_distances %>% group_by(from) %>% summarise(closest_relative = {
  # which rows have no infinite distances? the last one represents the MRCA
  mrca_row_index <- max(which(rowSums(is.infinite(out_distances[, to])) == 0))
  rownames(out_distances)[mrca_row_index]
})

# adding unfound genera species
graph_genus %<>% add_vertices(nrow(closest_relative), color = "red", attr = list(name = closest_relative[["from"]]))

# edges_to_add[1] -> edges_to_add[2], edges_to_add[2] -> edges_to_add[3]...
edges_to_add <- V(graph_genus)[closest_relative %>% select(closest_relative, from) %>% t %>% as.vector]$name

# connecting species leafs to the supposed MRCA
graph_genus %<>% add_edges(V(graph_genus)[edges_to_add])

plot(as.undirected(graph_genus), layout = layout_as_tree(graph_genus), vertex.label = NA, vertex.size=2)

# V(graph_genus)$a <- str_replace_all(V(graph_genus)$name, "/^[a-z0-9]+$/", "_")

# finally converting to phylo format
phylo_graph_genus <- treeio::as.phylo(graph_genus)

# adding tip.alias
phylo_graph_genus[["tip.alias"]] <- species_dictionary[["string_name"]][match(phylo_graph_genus[["tip.label"]], species_dictionary[["new_taxid"]])]

# converting to string ids
phylo_graph_genus[["tip.label"]] <- species_dictionary[["taxid"]][match(phylo_graph_genus[["tip.label"]], species_dictionary[["new_taxid"]])]

# ensuring a cleaner newick file with only necessary data
phylo_graph_genus[["node.label"]] <- NULL
phylo_graph_genus[["edge.length"]] <- NULL

# phylo_graph_genus %<>% rotatePhyloTree("9606")

usethis::use_data(phylo_graph_genus, overwrite = TRUE)

write.tree(phylo_graph_genus, "inst/extdata/476_tree.nwk")


################
################

phylo_graph_genus[["tip.label"]] <- phylo_graph_genus[["tip.alias"]]

pdf("C:/R/476_test.pdf", width=20, height=75)
  phylo_graph_genus %>% rotatePhyloTree("Homo sapiens") %>% plot(type = "cladogram")
dev.off()
