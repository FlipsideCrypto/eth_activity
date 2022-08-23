
library(shinyjs)
library(ggplot2)
library(dplyr)
library(plotly)
library(jsonlite)
library(httr)
library(shroomDK)
# gitignored - get your own ShroomDK key from Flipside Crypto!
api_key <- readLines("api_key.txt")

# EOA Daily History (query updates DAILY) ----

eoa_daily_history <- fromJSON(
  paste0("https://node-api.flipsidecrypto.com/api/v2/queries/",
         "49534ded-30f3-450a-8d31-558cb159966e/data/latest")
)


eoa_daily_history$eoa_bucket <- cut(eoa_daily_history$UNIQUE_DAYS,
                                    breaks = c(0, 1,10,100,1000, Inf),
                                    labels = c("1","2-10","11-100","101-1000","1001+"))

eoa_daily_history <- eoa_daily_history %>% 
  mutate(eoa_proportion = EOA_FREQ/sum(EOA_FREQ),
         eoa_cumulative = cumsum(EOA_FREQ),
         eoa_cumprop = cumsum(eoa_proportion))

# FUNCTIONS ----

get_tx_by_day <- function(eoa_address, api_key = api_key, ttl = 0){
 
  withProgress(message = "Querying...", detail = "", expr = {
    
    query <- {
      "SELECT FROM_ADDRESS as eoa, 
       date_trunc('DAY', block_timestamp) as day_,
  count(*) as num_tx,
  sum(TX_FEE) as fees_paid
  FROM ethereum.core.fact_transactions
  WHERE FROM_ADDRESS = lower('_EOA_ADDRESS_')
  GROUP BY day_, eoa
"
    }
    
    query <- gsub(pattern = "_EOA_ADDRESS_", replacement = tolower(eoa_address),
                  x = query, fixed = TRUE)
    
    incProgress(amount = 0.1,
               detail = "Query Created")
    
    query_token <- create_query_token(query = query, 
                                      api_key = api_key, 
                                      ttl = ttl,
                                      cache = FALSE)
    incProgress(amount = 0.2,
                detail = "Calculating...")
    
    res <- get_query_from_token(query_token$token, 
                                api_key = api_key)
    
    if(length(res$results) == 0){
      stop("Double check that address is typed correctly, no transactions found.")
    }
    
    incProgress(amount = 0.2, 
                detail = "Cleaning Result...")
    
    df <- clean_query(res)
    
    incProgress(0.5, 
                 detail = "Done!")
    
    return(df)
  })
  
}

plot_eoa <- function(eoadh = eoa_daily_history, user_bar = NULL){

  eoadh <- eoadh %>% group_by(eoa_bucket) %>% 
    summarise(sum_eoa = sum(EOA_FREQ))
  
  if(is.null(user_bar)){
    
    eoa_plotly <- eoadh %>% plot_ly(x = ~eoa_bucket,
                                    y = ~sum_eoa/1e6,
                                    color = I("#1C6DB8"),
                                    type = "bar",
                                    hoverinfo = 'text',
                                    hovertext = ~paste0(
                                      "Days Active: ", eoa_bucket,
                                      "\nAddresses: ", 
                                      scales::label_comma(accuracy = 1)(sum_eoa)))
    
  } else { 
    
    eoadh$lab <- c("Others")
    eoadh$lab[eoadh$eoa_bucket == user_bar] <- "You"
    eoadh$lab <- as.factor(eoadh$lab)
    
    eoa_plotly <- eoadh %>% plot_ly(x = ~eoa_bucket,
                                    y = ~sum_eoa/1e6,
                                    type = 'bar',
                                    color = ~lab,
                                    colors = c("#1C6DB8","#d99d45"),
                                    hoverinfo = 'text',
                                    hovertext = ~paste0(
                                      "Days Active: ", eoa_bucket,
                                      "\nAddresses: ", 
                                      scales::label_comma(accuracy = 1)(sum_eoa)))
  }
  
  
  eoa_plotly <- eoa_plotly %>% 
    layout(title = "ETH Accounts by their Historic Days Active",
           font = list(
             family = "Inter",
             color = 'white'),
           yaxis = list(title = "# Addresses (Millions)", 
                        showgrid = FALSE,
                        color = "#FFF"), 
           xaxis = list(title = "Days Active",
                        showticklabels = TRUE,
                        color = "#FFF"),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent",
           legend = list(font = list(color = '#FFFFFF')),
           hovermode = 'x') %>%
    plotly::config(scrollZoom = FALSE,
           displayModeBar = FALSE, 
           displaylogo = FALSE)
  
  return(eoa_plotly)
  
}

plot_tx <- function(eoa_tx){
    
    maxdate <- Sys.Date()
    mindate <- Sys.Date() - 391
    
    d <- data.frame(
      dates_in_year = seq.Date(mindate, maxdate, by = 'day')
    )
    
    d$month <- format(d$dates_in_year, '%b')
    d$day <- weekdays(d$dates_in_year)
    
    # start on Monday
    d <- d[which(d$day == "Monday")[1]:length(d$day), ]
    d$week <- ceiling(nrow(d)/7) # fill last week number *
    # infill previous weeks (1,1,1,1,1,1,1,2,2,2,2,2,2,2,.... N,N,N,N,N,N,N,*)
    fillweek = floor(nrow(d)/7)*7
    d$week[1:fillweek] <- unlist(lapply(1:(nrow(d)/7), replicate, n = 7))
    
    
    data <- merge(d, eoa_tx, by.x = "dates_in_year", by.y = "date", all.x = TRUE)
    data <- data[, c("dates_in_year","week","month", "day", "NUM_TX")]
    data$NUM_TX[is.na(data$NUM_TX)] <- 0
    
    monthlabel = data %>% 
      group_by(month) %>% 
      summarise(w1 = first(week)) %>% 
      dplyr::arrange(w1)
    
    p <- plot_ly(data = data)
    p <- add_heatmap(p = p, x = ~week,
                     y = ~day, 
                     z = ~NUM_TX*5, # scale up for better coloring 
                     text = paste0(
                       data$day,", ",
                       data$dates_in_year,
                       "\nTransactions:",
                       data$NUM_TX
                     ), 
                     colors = 'Blues',
                     zauto = FALSE, 
                     zmax = 50, 
                     zmin = 0,
                     hoverinfo = 'text',
                     xgap = 3,
                     ygap = 3,
                     showscale = FALSE)
    
    p %>% layout(title = "Year in Review",
                 font = list(
                   family = "Inter",
                   color = 'white'),
                 plot_bgcolor = "transparent", 
                 paper_bgcolor = "transparent",
                 yaxis=list(
                   showline = FALSE,
                   color = "#FFF",
                   showgrid = FALSE,
                   zeroline = FALSE,
                   tickmode="array",
                   ticktext=data$day[1:7],
                   tickvals=c(0,1,2,3,4,5,6),
                   title="",
                   autorange = 'reversed'
                 ),
                 xaxis= list(
                   showline = FALSE,
                   showgrid = FALSE,
                   zeroline = FALSE,
                   color = "#FFF",
                   ticktext = c(monthlabel$month,monthlabel$month[1]),
                   tickvals = c(monthlabel$w1, max(monthlabel$w1)+4),
                   title = ""
                 )) %>%
      plotly::config(scrollZoom = FALSE,
                     displayModeBar = FALSE, 
                     displaylogo = FALSE)
}

card_eoa <- function(card_value, card_label){ 
    # Creates a card using html
HTML(
  paste0(
  '<div class="card">
    <div class="card-body">
      <p class="card-value">',
  card_value,
  '</p><p class="card-label">',
  card_label,
  '</p></div></div>'
  )
)
  
  }
  


