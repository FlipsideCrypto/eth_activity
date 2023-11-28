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
      
      tryCatch(expr = {
        
        withProgress(message = "Querying...", detail = "", expr = {
          
          incProgress(amount = 0.1,
                      detail = "Inputs read...")
          
          incProgress(amount = 0.4,
                      detail = "Calculating...")
          
          x <- get_tx_by_day(eoa_address = input$address,
                             api_key = api_key)
          results$table <- x
          
        })
        
      }, error = function(e){
        showModal(modalDialog(
          title = "on no! error!",
          HTML("This address has 0 tx. <br> Reminder: Contracts cannot initiate transactions!")))
        Sys.sleep(2)
        session$reload()
      })
      
      results$table <- x
    }
  })
  
  
  observe({
    rr <<- results$table
    
  })
  
  eoa_stats <- reactive({
    eoa_activity <- list(
      "txn" = sum(results$table$num_tx),
      "days" = length(unique(results$table$date)),
      "fees" = paste0(round(sum(results$table$fees_paid),2), ' Ξ')
    )
  })
  
  output$ntxn <- renderText(ifelse(eoa_stats()$txn > 0, eoa_stats()$txn, ""))
  output$days <- renderText(ifelse(eoa_stats()$days > 0, eoa_stats()$days, ""))
  output$fees <- renderText(ifelse(eoa_stats()$fees != '0 Ξ', eoa_stats()$fees, ""))
  
  
  output$favorite_days <- renderText({
    ifelse(eoa_stats()$txn > 0,
           names(which.max(table(rep(weekdays(results$table$date), results$table$num_tx)))),
           "")
    })
  
  output$percentile <- renderText({
    if(is.null(results$table)){
      ""
    } else {
      total_eoa <- sum(eoa_daily_history$EOA_FREQ)
      user_rank <- total_eoa - eoa_daily_history[
        eoa_daily_history$UNIQUE_DAYS == eoa_stats()$days, 
        "eoa_cumulative"]
      
      rnk <- cut(user_rank,
                 breaks = c(0,1e3,1e4,1e5,250000,1e6,5e6,1e7,Inf),
                 labels = c("Top 1000!","Top 10K","Top 100,000","Top 250,000",
                            "Top 1 Million","Top 5 Million","Top 10 Million","Not Ranked"))
      paste0(rnk)
      
    }
  })
 
  output$main_plot <- renderPlotly({
   
    days = as.numeric(eoa_stats()$days) 
    
    x = cut(days,
            breaks = c(0,1,10,50,100,250,1000,Inf),
            labels = c("1","2-10","11-50","51-100","101-250","251-1000","1001+"))
    
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
