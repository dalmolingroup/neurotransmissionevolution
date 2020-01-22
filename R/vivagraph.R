#' @export
#' @import igraph
#' @import dplyr
vivagraph <- function(graph,
                      layout = NULL,
                      precompute_niter = 1000,
                      precompute_multiplier = 75,
                      pinned_cols = 2,
                      pinned_rows = 0,
                      repin = FALSE,
                      lcc_offset = 200,
                      pinned_offset = -100){

  subgraphs <- decompose.graph(graph)
  lcc_index <- which.max(sapply(subgraphs, vcount))

  ##############
  # lcc
  ##############
  g_v <- as_data_frame(graph, "vertices") %>%
    select_if(is.numeric) %>%
    apply(2, linMap, 0.001, 1) %>%
    dist %>%
    as.matrix

  g_v <- 1/g_v^2

  g_v[g_v == Inf] <- max(g_v[g_v!=max(g_v)] )
  # g_v[g_v <= 1] <- 0

  row.names(g_v) <- V(graph)$name
  colnames(g_v) <- V(graph)$name

  dist_graph <- graph_from_adjacency_matrix(g_v, mode = "undirected", weighted = TRUE, diag = FALSE)

  dist_layout <- layout_with_fr(dist_graph, niter = precompute_niter) * precompute_multiplier

  V(graph)$x <- dist_layout[,1] + lcc_offset
  V(graph)$y <- dist_layout[,2]

  ##############
  # unconnected
  ##############
  unconnected_nodes <- lapply(subgraphs[-lcc_index], as_data_frame, what = "vertices") %>% bind_rows
  unconnected_edges <- lapply(subgraphs[-lcc_index], as_data_frame) %>% bind_rows

  unconnected <- graph_from_data_frame(unconnected_edges, directed = F, vertices = unconnected_nodes)
  unconnected_layout <- (layout_on_grid(unconnected, width = pinned_cols, height = pinned_rows) * 20) + pinned_offset

  V(graph)[V(unconnected)$name]$x <- unconnected_layout[,1]
  V(graph)[V(unconnected)$name]$y <- unconnected_layout[,2]
  V(graph)[V(unconnected)$name]$pinned <- 1

  if(is.matrix(layout)){
    V(graph)$x <- layout[,1]
    V(graph)$y <- layout[,2]
  }

  if(repin == TRUE) {
    V(graph)[V(unconnected)$name]$x <- unconnected_layout[,1]
    V(graph)[V(unconnected)$name]$y <- unconnected_layout[,2]
    V(graph)[V(unconnected)$name]$pinned <- 1
  }

  graph_json <- jsonlite::toJSON(list(
     nodes = igraph::as_data_frame(graph, "vertices")
    ,links = igraph::as_data_frame(graph, "edges"))
  )

  server <- function(input, output, session) {
    session$sendCustomMessage(type = "dataTransferredFromServer", graph_json)

    shiny::observeEvent(input$coordinates, {
      if(!is.null(input$coordinates)) shiny::stopApp(input$coordinates)
    })
  }

  shiny::addResourcePath("www", system.file("www/vivagraph", package="neurotransmission"))

  layout <- shiny::runGadget(shiny::shinyApp(ui = shiny::htmlTemplate(system.file("www/vivagraph/index.html", package="neurotransmission")), server))

  matrix(layout, ncol=2,byrow=T)
}

#' @export
rotate_layout <- function(layout, angle){
  #matriz de rotacao
  angle <- angle*(pi/180)
  rotm <- matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)),ncol=2)
  #rotacao
  layout <- t(rotm %*% (t(layout)))
}
