#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- function(req) {
  fluidPage(
   # Application title
   titlePanel("Test Selectize Bookmarking"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput("continents",
                     "Select Continents:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     options = NULL),
         bookmarkButton()),
      
      mainPanel(h3(textOutput("caption")))
   )
)}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   updateSelectizeInput(session, 'continents', choices = gapminder$continent, server = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = 'server')

