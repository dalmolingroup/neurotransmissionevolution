######################
######################
library(ape)
library(phytools)
library(tidyverse)
library(magrittr)
library(igraph)

setwd("data-raw")

if (!file.exists("download/Eukaryota_species.nwk")) {
  download.file(
    "http://www.timetree.org/ajax/direct_download?direct-download-format=newick&direct-download-id=23070&direct-download-rank=species",
    "download/Eukaryota_species.nwk"
  )
}

##################################################
##     UNFOUND SPECIES WITH MATCHING GENERA     ##
##################################################
## this first part fills in missing species     ##
## by searching for it's genera in the 85k tree ##
##################################################

# loading species names and taxon ids
data(string_eukaryotes)

# loading newick tree obtained from timetree
timetree_newick <- read.tree("download/timetree_335_eukaryotes.nwk")

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

# annotating genera
species_dictionary %<>% mutate(genus_search = coalesce(timetree_name, ncbi_name) %>% strsplit(" ") %>% sapply("[", 1))

# unique genera
selected_genera <- species_dictionary[["genus_search"]] %>% unique

# the following genera names are unreliable and should not be searched for
duplicated_genera <- scan("duplicated_genera.txt", what = "character")

# these are unreliable selected_genera:
unreliable_genera <- intersect(selected_genera, duplicated_genera)

# removing unreliable genera from selected_genera:
# selected_genera %<>% setdiff(unreliable_genera)

# loading newick tree obtained from timetree
tree_85k <- read.tree("download/Eukaryota_species.nwk")

# ensuring a cleaner newick file with only necessary data
# this is actually really important
tree_85k[["node.label"]] <- NULL
tree_85k[["edge.length"]] <- NULL

# replacing timetree's underscores with spaces
tree_85k[["tip.label"]] %<>% str_replace_all("_", " ")

# storing genus
tree_85k[["tip.genus"]] <- sapply(strsplit(tree_85k[["tip.label"]]," "), "[", 1)
tree_85k_genera <- tree_85k[["tip.genus"]] %>% unique

# subtracting unreliable genera
tree_85k_genera %<>% setdiff(unreliable_genera)

# keeping only selected genera, including unreliable ones
tree_genus <- tree_85k %$% keep.tip(., tip.label[tip.genus %in% selected_genera])
tree_genus[["tip.genus"]] <- sapply(strsplit(tree_genus[["tip.label"]]," "), "[", 1)

# unfound species which genera are present in the 85k tree
unfound_species <- species_dictionary %>% filter(is.na(timetree_name) & genus_search %in% tree_85k_genera)

# for each unfound species which genus is present in the 85k tree,
for(i in 1:nrow(unfound_species)){
  # we search for all species of this genus ("sister species") in the 85k tree
  # this part is tricky because bind.tip rebuilds the tree from scratch
  # so we need to keep removing underscores. there are better ways to do this.
  tip_genus <- tree_genus[["tip.label"]] %>% strsplit("[_ ]") %>% sapply("[", 1)
  sister_species <- tree_genus[["tip.label"]][tip_genus == unfound_species[[i, "genus_search"]]]
  # we obtain the sister_species' most recent common ancestor (MRCA)
  # c(.[1]) is a hack because the MRCA function only works with at least 2 nodes
  where <- getMRCA(tree_genus, sister_species %>% c(.[1]))
  # and then add a leaf node linked to this MRCA
  tree_genus %<>% bind.tip(tip.label = unfound_species[[i, "ncbi_name"]], where = where)
}

# for some reason bind.tip adds underscores to species names
tree_genus[["tip.label"]] %<>% str_replace_all("_", " ")

# keeping track of found species
found_species <- species_dictionary %>% filter(!is.na(timetree_name) | genus_search %in% tree_85k_genera)
# forced_name means it either was found in timetree or we forced it by looking at genera names
found_species %<>% mutate(forced_name = coalesce(timetree_name, ncbi_name))

# so we keep only found species in this tree we are building (timetree + forced by genera)
tree_genus %<>% keep.tip(found_species[["forced_name"]])

# converting to ncbi taxids
tree_genus[["tip.label"]] <- found_species[["new_taxid"]][match(tree_genus[["tip.label"]], found_species[["forced_name"]])]

# plotting to file
# pdf("406_test.pdf", width=20, height=75)
plot(tree_genus, type = "cladogram", use.edge.length = F)
# dev.off()

##################################################
##                UNFOUND GENERA                ##
##################################################
## the following part fills in missing species  ##
## by searching for its closest relatives in    ##
## in the NCBI tree                             ##
##################################################
data(ncbi_tree)

# converting ncbi phylo to igraph
graph_ncbi <- as.igraph.phylo(ncbi_tree, directed = TRUE)
plot(as.undirected(graph_ncbi), layout = layout_as_tree(graph_ncbi), vertex.label = NA, vertex.size=1)

# converting phylo to igraph
graph_genus <- as.igraph.phylo(tree_genus, directed = TRUE)
plot(as.undirected(graph_genus), layout = layout_as_tree(graph_genus), vertex.label = NA, vertex.size=1)

# for each species which genus is not in timetree
# we'll look for its two closest species (in the NCBI tree) which are present in the tree_genus we just built
unfound_genera <- species_dictionary %>% filter(is.na(timetree_name) & !genus_search %in% tree_85k_genera)

# this is the igraph equivalent of "phylo_tree$tip.label"
tip_nodes <- V(graph_ncbi)[degree(graph_ncbi, mode = "out") == 0]

# undirected distances between all species nodes
tip_distances <- graph_ncbi %>%
  distances(v = tip_nodes, to = tip_nodes, mode = "all") %>%
  as_tibble(rownames = "from") %>%
  pivot_longer(-from, names_to = "to", values_to = "distance")

# removing self references (zero distances)
tip_distances %<>% filter(distance > 0)

# we only want to search for species of unfound genera
tip_distances %<>% inner_join(unfound_genera %>% select(from = new_taxid))

# we only want to find species already present in the genus_tree
tip_distances %<>% inner_join(found_species %>% select(to = new_taxid))

# we only want the two closest relatives
tip_distances %<>%
  group_by(from) %>%
  top_n(-2, distance) %>% # top 2 smallest distances
  top_n(2, to) # more than 2 species have the same smallest distance, so we get the first ones

# out distance matrix between all nodes in tree, needed to find MRCAs
out_distances <- graph_genus %>% distances(mode = "out")

# for each species of unfound genera,
# we find the MRCA for its two closest relatives
unfound_genera_mrca <- tip_distances %>% group_by(from) %>% summarise(mrca = {
  # which rows have no infinite distances? the last one represents the MRCA
  mrca_row_index <- max(which(rowSums(is.infinite(out_distances[, to])) == 0))
  rownames(out_distances)[mrca_row_index]
})

# adding unfound genera species nodes
graph_genus %<>% add_vertices(nrow(unfound_genera_mrca), color = "red", attr = list(name = unfound_genera_mrca[["from"]]))

# defining unfound genera species edges
# edges_to_add[1] -> edges_to_add[2], edges_to_add[2] -> edges_to_add[3]...
edges_to_add <- V(graph_genus)[unfound_genera_mrca %>% select(mrca, from) %>% t %>% as.vector]$name

# connecting species leafs to the supposed MRCA
graph_genus %<>% add_edges(V(graph_genus)[edges_to_add])

# plotting
plot(as.undirected(graph_genus), layout = layout_as_tree(graph_genus), vertex.label = NA, vertex.size=2)

# finally converting to phylo format
phylo_graph_genus <- treeio::as.phylo(graph_genus)

# adding tip.alias (this is not exported with write.tree)
phylo_graph_genus[["tip.alias"]] <- species_dictionary[["string_name"]][match(phylo_graph_genus[["tip.label"]], species_dictionary[["new_taxid"]])]

# converting back to string ids
phylo_graph_genus[["tip.label"]] <- species_dictionary[["taxid"]][match(phylo_graph_genus[["tip.label"]], species_dictionary[["new_taxid"]])]

# ensuring a cleaner newick file with only necessary data
phylo_graph_genus[["node.label"]] <- NULL
phylo_graph_genus[["edge.length"]] <- NULL

# phylo_graph_genus %<>% rotatePhyloTree("9606")

# usethis::use_data(phylo_graph_genus, overwrite = TRUE)

write.tree(phylo_graph_genus, "../inst/extdata/hybrid_tree.nwk")

# moving ctenophora before porifera
mnemiopsis_taxid <- species_dictionary %>% filter(ncbi_name == "Mnemiopsis leidyi") %>% pull(taxid)
amphimedon_taxid <- species_dictionary %>% filter(ncbi_name == "Amphimedon queenslandica") %>% pull(taxid)

# reordering tip.labels
from_to <- c(
  "400682" = "27923",  # amphimedon to mnemiopsis
  "10228"  = "400682", # trichoplax to amphimedon
  "27923"  = "10228"   # mnemiopsis to trichoplax
)

modified_phylo <- phylo_graph_genus

modified_phylo[["tip.label"]] %<>% recode(!!!from_to)

# modified_phylo <- phylo_graph_genus %>%
#   drop.tip(mnemiopsis_taxid) #%>%
  # bind.tree(read.tree(text = paste0("(",mnemiopsis_taxid,");")), where = getParent(.,which(.$tip.label==amphimedon_taxid)), position = 1)

# read.tree(text="(a,(b,(c,d)));") %>%
#   bind.tree(read.tree(text="(x0);"), where = getParent(.,which(.$tip.label=="c")), position = 1) %>%
#   plot(type="cladogram")

write.tree(modified_phylo, "../inst/extdata/hybrid_tree_modified.nwk")


################
################

# phylo_graph_genus[["tip.label"]] <- phylo_graph_genus[["tip.alias"]]

pdf("hybrid_tree.pdf", width=20, height=75)
  phylo_graph_genus %>% rotatePhyloTree("9606") %>% plot(type = "cladogram")
dev.off()

# modified_phylo[["tip.label"]] <- modified_phylo[["tip.alias"]]

pdf("hybrid_tree_modified.pdf", width=20, height=75)
  modified_phylo %>% rotatePhyloTree("9606") %>% plot(type = "cladogram")
dev.off()
