library(neurotransmission)
library(ape)
library(magrittr)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(tibble)
library(tidylog)

################
################

data("timetree_newick", "ncbi_tree", package="neurotransmission")

ncbi_tree %>% ladderize %>% plot(type="cladogram")

pdf("~/Downloads/timetree_335_eukaryotes.pdf", width=20, height=50)
  timetree_newick %>% rotatePhyloTree("Homo sapiens") %>% plot(type="cladogram", use.edge.length = F)
dev.off()

################
################

timetree_85k <- read.tree("data-raw/Eukaryota_species.nwk")$tip.label %>%
  str_replace_all("_"," ") %>%
  as.tibble %>%
  separate(value, c("genus","species"), extra = "merge", remove = F)

# write.table(timetree_85k, "inst/extdata/timetree_85k.txt", row.names = F, col.names = F, quote = F)

unresolved_141 <- read.table(
  "inst/extdata/timetree_unresolved_names.txt",
  stringsAsFactors = F,
  col.names = c("unresolved_genus","unresolved_species")
)

unresolved_141$unresolved_name <- paste(
  unresolved_141$unresolved_genus,
  unresolved_141$unresolved_species
)

unresolved_141$genus_in_85k <- unresolved_141$unresolved_genus %in% timetree_85k_genus

unresolved_141 %<>% left_join(timetree_85k, by = c("unresolved_genus" = "genus"))

unresolved_141 <- unresolved_141[!duplicated(unresolved_141$unresolved_name),]

unresolved_141 %<>% select(unresolved_name, value) %>% rename(resolved_name = value)

data("string_eukaryotes")

string_eukaryotes_fix <- left_join(string_eukaryotes, unresolved_141, by = c("ncbi_name" = "unresolved_name"))

usethis::use_data(string_eukaryotes_fix, overwrite = TRUE)

to_search <- string_eukaryotes_fix %$% coalesce(resolved_name, ncbi_name) %>% unique

write.table(to_search, "inst/extdata/resolved_names.txt", row.names = F, col.names = F, quote = F)
