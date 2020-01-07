library(ape)
library(dplyr)
library(stringr)
library(magrittr)

# loading species names and taxon ids
data(string_eukaryotes_fix)

string_eukaryotes_fix %<>% mutate(searched_name = coalesce(resolved_name, ncbi_name))

# loading newick tree obtained from timetree
timetree_newick_2nd <- read.tree("data-raw/resolved_names.nwk")

# plot(timetree_newick %>% ladderize, type = "cladogram", use.edge.length = F)

# replacing timetree's underscores with spaces
timetree_newick_2nd[["tip.label"]] %<>% str_replace_all("_", " ")

# which timetree species' names exactly match with the resolved names
taxid_indexes <- timetree_newick_2nd[["tip.label"]] %>% match(string_eukaryotes_fix[["searched_name"]])

# find out which timetree species names didn't exactly match ncbi's
unmatched_names <- timetree_newick_2nd[["tip.label"]] %>% extract(which(taxid_indexes %>% is.na)) %T>% print

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
timetree_newick_2nd[["tip.label"]] %<>% coalesce(unmatched_names_fix[.], .) %>% unname

# rematching after manual fix (which timetree species' names exactly match with searched ones)
taxid_indexes <- timetree_newick_2nd[["tip.label"]] %>% match(string_eukaryotes_fix[["searched_name"]])

# "for each tip.label, if there exists a matched string_eukaryotes$taxid, use this value.
# else just leave it as is."
timetree_newick_2nd[["taxid"]] <- coalesce(string_eukaryotes_fix[taxid_indexes, "taxid", T], timetree_newick_2nd[["tip.label"]])

# the following assignments ensure a cleaner newick file with only necessary data
timetree_newick_2nd[["node.label"]] <- NULL
timetree_newick_2nd[["edge.length"]] <- NULL

# exporting phylo object
usethis::use_data(timetree_newick_2nd, overwrite = TRUE)

# parseable data goes into the inst/extdata folder
timetree_newick_2nd[["tip.label"]] <- paste(
  timetree_newick_2nd[["taxid"]],
  timetree_newick_2nd[["tip.label"]]
)

string_eukaryotes_fix %<>% mutate(newick_name = paste(taxid, ncbi_name) %>% str_replace_all(" ","_"))

write.tree(timetree_newick_2nd, "inst/extdata/timetree_380_eukaryotes.nwk")
