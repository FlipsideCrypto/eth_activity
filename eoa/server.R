library(shiny)
source("global.R")
# Define server logic 
shinyServer(function(input, output) {

  results <- eventReactive(input$submit, {
    
    if(nchar(input$address) != 42 | !grepl("^0x", input$address)){
      stop("Double check address is a valid ETH address (not ENS)")
    } else {
      get_eoa_activity(eoa_address = input$address, 
                     api_key = readLines("api_key.txt"),
                     ttl = 0)
    }
        
  })
  
  output$compare <- DT::renderDataTable({
   tbl_eoa(eoa_daily_history, eoa_activity = results())
  })
  
  output$main_plot <- renderPlotly({
    plot_eoa(eoadh = eoa_daily_history)
  })

})
