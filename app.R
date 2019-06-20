library(esquisse)
library(gapminder)

fixed_height <- '
  .container-fluid {
    position:absolute;
    top:5px;
    bottom:5px;
  }
  .row, .well, div[class^="col-"] {
    height:100%;
  }
'

ui <- function(request) {
  
  docs_panel <- sidebarPanel(
    h1("Menu"),
    selectInput("model", "Model", choices = c("A" = "a", "B" = "b")),
    bookmarkButton(),
    width = 3
  )
  
  plot_panel <- mainPanel(
    esquisserUI('esquisse', header = FALSE, choose_data = TRUE),
    width = 9
  )
  
  fluidPage(
    tags$head(tags$style(fixed_height)),
    sidebarLayout(docs_panel, plot_panel)
  )
}

server <- function(input, output, session) {
  
  esquisseData <- reactiveValues(
    name = 'Gapminder',
    data = gapminder
  )

  callModule(esquisserServer, 'esquisse', esquisseData)
}

# run the app                                                                                                           
shinyApp(ui, server, enableBookmarking = 'server')
