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
            FUN = function(x){card_eoa(eoa_stats()[[x]], x)})
   )
 })
  
 plot_ <- reactive({
   
   if(input$submit == 0){
     plot_eoa(eoadh = eoa_daily_history)
     
   } else { 
     
     x = cut(as.numeric(eoa_stats()[["Days Active"]]),
               breaks = c(0, 1,10,100,1000, Inf),
               labels = c("1","2-10","11-100","101-1000","1001+"))
     
     plot_eoa(eoadh = eoa_daily_history, x)
   }
 })
 
  output$main_plot <- renderPlotly({
   plot_()
  })
  
 heat_ <- eventReactive(input$submit, {
   get_tx_by_day(input$address, api_key = readLines("api_key.txt"))
 })  
 
 output$heatmap <- renderUI({
   tagList(
     renderCalheatmapR(plot_tx(heat_())))
 })
  
})
