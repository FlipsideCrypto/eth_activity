library(shiny)
source("global.R")
# Define server logic 
shinyServer(function(input, output, session) {

  results <- eventReactive(input$submit, {
    if(nchar(input$address) != 42 | !grepl("^0x", input$address)){
      stop("Double check address is a valid ETH address (not ENS)")
    } else {
      get_eoa_activity(eoa_address = input$address, 
                     api_key = readLines("api_key.txt"),
                     ttl = 0)
    }
  })
  
  eoa_stats <- reactive({
    tbl_eoa(eoa_daily_history, eoa_activity = results())
  })

 output$compare <- renderUI({
   tagList(
     lapply(names(eoa_stats()), 
            FUN = function(x){card_eoa(eoa_stats()[[x]], x)}),
     div(class = 'stats-box',
     a(class='twitter socials', href='https://twitter.com','Share:')
     )
   )
 })
  
 plot_ <- reactive({
   
   if(input$submit == 0){
     plot_eoa(eoadh = eoa_daily_history)
     
   } else { 
     a <- list(
       x = cut(as.numeric(results()[["Days Active"]]),
               breaks = c(0, 1,10,100,1000, Inf),
               labels = c("1","2-10","11-100","101-1000","1001+")),
       y = 10,
       text = "    You're here!", # alignment is so annoying
       xref = "x",
       yref = "y",
       showarrow = FALSE,
       arrowhead = 1,
       xanchor = 'left')
     
     plot_eoa(eoadh = eoa_daily_history, label = a)
   }
 })
 
  output$main_plot <- renderPlotly({
   plot_()
  })
  
})
