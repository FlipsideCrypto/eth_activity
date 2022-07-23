library(shiny)
source("global.R")
# Define UI 
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  wellPanel(class = "main-app",
     div(class = 'header',
         h2(class = 'line', "Ethereum EOA Activity"),
         h4(class = 'line', "How active are you?"),
         fluidRow(class = 'eoa-bar',
           column(11,
                  textInput(inputId = "address", label = "",
                            placeholder = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045",
                            width = '100%')
                  ),
           column(1, class = 'eoa-btn',
                  actionButton("submit", label = "Search")
                  )
          ),
         conditionalPanel("input.submit > 0",
                          div(class = 'eoa-tbl',
                          dataTableOutput("compare")
                          )
                          )
     ),
     div(class = "chart",
         hr(),
         plotlyOutput("main_plot")
         )
      )
  
))
