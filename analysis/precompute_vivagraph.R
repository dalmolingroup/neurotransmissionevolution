subgraphs <- decompose.graph(g)
lcc_index <- which.max(sapply(subgraphs, vcount))
lcc <- subgraphs[[lcc_index]]

unconnected_nodes <- lapply(subgraphs[-lcc_index], as_data_frame, what = "vertices") %>% bind_rows
unconnected_edges <- lapply(subgraphs[-lcc_index], as_data_frame) %>% bind_rows
unconnected_graph <- graph_from_data_frame(unconnected_edges, directed = F, vertices = unconnected_nodes)
unconnected_graph_layout <- layout_on_grid(unconnected_graph)



lcc_v <- as_data_frame(lcc, "vertices") %>%
  select_if(is.numeric) %>%
  apply(2, linMap, 0.001, 1) %>%
  dist %>%
  as.matrix

lcc_v <- 1/lcc_v^2

lcc_v[lcc_v == Inf] <- max(lcc_v[lcc_v!=max(lcc_v)] )
lcc_v[lcc_v <= 1] <- 0

row.names(lcc_v) <- V(lcc)$name
colnames(lcc_v) <- V(lcc)$name

dist_graph <- graph_from_adjacency_matrix(lcc_v, mode = "undirected", weighted = TRUE, diag = FALSE)

plot(dist_graph, layout = layout_with_fr, vertex.label = NA, vertex.size = 1)
