library(shiny)
source("global.R")
# Define server logic 
shinyServer(function(input, output, session) {
  
  results <- reactiveValues()
  
  onclick(id = "go-button", expr = {
    
    if(nchar(input$address) != 42 | !grepl("^0x", input$address)){
      #showNotification("Addresses must be 42 chars long and start with 0x. Like this: 0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")
      showModal(modalDialog(
        title = "on no! error!",
        HTML("Addresses must be 42 chars long and start with 0x<br>Like this: 0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")
      ))
      warning("Double check address is a valid ETH address (not ENS)")
      
    } else {
      x <- get_tx_by_day(eoa_address = input$address,
                         api_key = api_key,
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
  
  
  output$favorite_days <- renderText({
    ifelse(eoa_stats()$txn > 0,
           names(which.max(table(rep(weekdays(results$table$date), results$table$NUM_TX)))),
           "")
    })
  
  output$percentile <- renderText({
    if(is.null(results$table)){
      ""
    } else {
    paste0(round(
      100*(eoa_daily_history[eoa_daily_history$UNIQUE_DAYS == eoa_stats()$days, "eoa_cumprop"]), 
      2),"%")
    }
  })
 
  output$main_plot <- renderPlotly({
   
    days = as.numeric(eoa_stats()$days) 
    
    x = cut(days,
            breaks = c(0,1,3,10,50,100,300,1000,Inf),
            labels = c("1","</= 3","4-10","11-50","51-100","101-300","301-1000","1001 +"))
    
    if(is.null(results$table)){
      NULL
    } else {
      if(days == 0){
        plot_eoa(eoadh = eoa_daily_history, user_bar = NULL)
      } else {
        plot_eoa(eoadh = eoa_daily_history, user_bar = x)
      }
    }
    
  })
  
  output$heatmap <- renderPlotly(plot_tx(results$table))
  
})
