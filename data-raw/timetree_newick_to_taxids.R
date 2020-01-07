library(ape)
library(dplyr)
library(stringr)
library(magrittr)

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
unmatched_names <- timetree_newick[["tip.label"]] %>% extract(which(taxid_indexes %>% is.na)) %T>% print

# manually writing a dictionary to fix unmatched names
unmatched_names_fix <- c(
  "Cercospora fijiensis"     = "Pseudocercospora fijiensis",
  "Arthroderma benhamiae"    = "Trichophyton benhamiae",
  "Macropus eugenii"         = "Notamacropus eugenii",
  "Ostreococcus lucimarinus" = "Ostreococcus sp. 'lucimarinus'",
  "Oryza nivara"             = "Oryza sativa f. spontanea"
)

# "for each tip.label, if there exists an unmatched_names_fix value with this index, use this value.
# else just leave it as is."
timetree_newick[["tip.label"]] %<>% coalesce(unmatched_names_fix[.], .) %>% unname

# rematching after manual fix (which timetree species' names exactly match with ncbi's)
taxid_indexes <- timetree_newick[["tip.label"]] %>% match(string_eukaryotes[["ncbi_name"]])

# "for each tip.label, if there exists a matched string_eukaryotes$taxid, use this value.
# else just leave it as is."
timetree_newick[["taxid"]] <- coalesce(string_eukaryotes[taxid_indexes, "taxid", T], timetree_newick[["tip.label"]])

# the following assignments ensure a cleaner newick file with only necessary data
timetree_newick[["node.label"]] <- NULL
timetree_newick[["edge.length"]] <- NULL

# exporting phylo object
usethis::use_data(timetree_newick, overwrite = TRUE)

# parseable data goes into the inst/extdata folder
timetree_newick[["tip.label"]] <- timetree_newick[["taxid"]]
write.tree(writable_newick, "inst/extdata/timetree_newick.nwk")
