d3force <- function(graph, layout = NULL){
  if(is.matrix(layout)){
    V(graph)$x <- layout[,1]
    V(graph)$y <- layout[,2]
  }

  graph_json <- jsonlite::toJSON(list(
    nodes = igraph::as_data_frame(graph, "vertices") %>% rename(id = name)
    ,links = igraph::as_data_frame(graph, "edges") %>% rename(source = from, target = to))
  )

  # print(graph_json)

  server <- function(input, output, session) {
    session$sendCustomMessage(type = "dataTransferredFromServer", graph_json)

    shiny::observeEvent(input$coordinates, {
      if(!is.null(input$coordinates)) shiny::stopApp(input$coordinates)
    })
  }

  # shiny::addResourcePath('vivagraph.min.js', 'inst/www/vivagraph.min.js')
  # shiny::addResourcePath('multiselect.js', 'inst/www/multiselect.js')
  # shiny::addResourcePath('index.js', 'inst/www/index.js')

  shiny::addResourcePath("www", system.file("www", package="neurotransmission"))

  layout <- shiny::runGadget(shiny::shinyApp(ui = shiny::htmlTemplate(system.file("www/index.html", package="neurotransmission")), server))

  matrix(layout, ncol = 3, byrow = T)
}