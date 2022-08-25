library(shiny)
source("global.R")
# Define server logic 
shinyServer(function(input, output, session) {
  
  results <- reactiveValues()
  
  onclick(id = "go-button", expr = {
    
    if(nchar(input$address) != 42 | !grepl("^0x", input$address)){
      stop("Double check address is a valid ETH address (not ENS)")
      
    } else {
      x <- get_tx_by_day(eoa_address = input$address,
                         api_key = readLines("api_key.txt"),
                         ttl = 0)
      x$date <- as.Date(x$DAY_)
      results$table <- x
    }
    
  })
  
  
  
  eoa_stats <- reactive({
    eoa_activity <- list(
      "txn" = sum(results$table$NUM_TX),
      "days" = length(unique(results$table$date)),
      "fees" = paste0(round(sum(results$table$FEES_PAID),2), ' Ξ')
    )
  })
  
  output$ntxn <- renderText(ifelse(eoa_stats()$txn > 0, eoa_stats()$txn, ""))
  output$days <- renderText(ifelse(eoa_stats()$days > 0, eoa_stats()$days, ""))
  output$fees <- renderText(ifelse(eoa_stats()$fees != '0 Ξ', eoa_stats()$fees, ""))
  
  
  output$median_days <- renderText(ifelse(eoa_stats()$txn > 0, "<1 day", ""))
  output$percentile <- renderText({
    round(
      100*(eoa_daily_history[eoa_daily_history$UNIQUE_DAYS == eoa_stats()$days, "eoa_cumprop"]), 
      2)
  })
  
  # output$title <- renderUI({
  #   
  #   da <- eoa_stats()$days
  #   
  #   percent <- { 
  #     round(
  #       100*(eoa_daily_history[eoa_daily_history$UNIQUE_DAYS == da, "eoa_cumprop"]), 
  #       2)
  #   }
  #   
  #   tagList(
  #     div(class = 'chart-title', span(
  #       paste0("You've shown up more than ", percent, "% of ETH users")
  #     )))
  #   
  # })
  
  # output$compare <- renderUI({
  #   tagList(
  #     lapply(names(eoa_stats()), 
  #            FUN = function(x){card_eoa(eoa_stats()[[x]], x)})
  #   )
  # })
  
  # plot_ <- reactive({
  #   
  #   if(input$submit == 0){
  #     plot_eoa(eoadh = eoa_daily_history)
  #     
  #   } else { 
  #     
  #     x = cut(as.numeric(eoa_stats()[["Days Active"]]),
  #             breaks = c(0, 1,10,100,1000, Inf),
  #             labels = c("1","2-10","11-100","101-1000","1001+"))
  #     
  #     plot_eoa(eoadh = eoa_daily_history, x)
  #   }
  # })
  
  output$main_plot <- renderPlotly({
    x = cut(as.numeric(eoa_stats()$txn),
                         breaks = c(0, 1,10,100,1000, Inf),
                         labels = c("1","2-10","11-100","101-1000","1001+"))
    plot_eoa(eoadh = eoa_daily_history, user_bar = x)
  })
  
  output$heatmap <- renderPlotly(plot_tx(results$table))
  
  # output$heatmap <- renderUI({
  #   renderPlotly(plot_tx(results()))
  # })
  
})
