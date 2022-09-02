library(shinyjs)
library(ggplot2)
library(dplyr)
library(plotly)
library(jsonlite)
library(httr)
library(shroomDK)
# gitignored - get your own ShroomDK key from Flipside Crypto!
api_key <- readLines("api_key.txt")


#423E75 # dark purple
#CECBF5 # light purple
#0D0C1C # black background

# EOA Daily History (query updates DAILY) ----

eoa_daily_history <- fromJSON(
  paste0("https://node-api.flipsidecrypto.com/api/v2/queries/",
         "49534ded-30f3-450a-8d31-558cb159966e/data/latest")
         #"e90160dc-765a-45f7-8a28-d2258c4127e9/data/latest") -- last 391 day version
)

eoa_daily_history$eoa_bucket <- cut(eoa_daily_history$UNIQUE_DAYS,
                                    breaks = c(0,1,3,10,50,100,300,1000,Inf),
                                    labels = c("1","</= 3","4-10","11-50","51-100","101-300","301-1000","1001 +"))

# for last 391 day version
#breaks = c(0, 1,3,10,50,100,300, Inf),
#labels = c("1","</= 3","4-10","11-50","51-100","101-300","301 +")

eoa_daily_history <- eoa_daily_history %>% 
  subset(UNIQUE_DAYS > 1) %>%
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

plot_eoa <- function(eoadh = eoa_daily_history, 
                     user_bar = NULL, 
                     title = "") {
  
  eoadh <- eoadh %>% group_by(eoa_bucket) %>% 
    summarise(sum_eoa = sum(EOA_FREQ))
    
    eoadh$lab <- c("Others")
    eoadh$lab[eoadh$eoa_bucket == user_bar] <- "You"
    eoadh$lab <- as.factor(eoadh$lab)
    
    eoa_plotly <- eoadh %>% 
      plot_ly(x = ~eoa_bucket,
              y = ~sum_eoa/1e6,
              type = 'bar',
              color = ~lab,
              colors = rev(c("#696286","#9288BA")),
              hoverinfo = 'text',
              hovertext = ~paste0(
                "Days Active: ", eoa_bucket,
                "\nAddresses: ", 
                scales::label_comma(accuracy = 1)(sum_eoa)))
    
    if(is.null(user_bar)){
    urhere <- NULL  
    } else { 
    urhere <- list(
      x = user_bar,
      y = eoadh$sum_eoa[eoadh$lab == "You"]/1000000 + 10,
      text = "You\n↓",
      xref = "x",
      yref = "y",
      showarrow = FALSE
    )
      }
  
  eoa_plotly <- 
    eoa_plotly %>% 
    layout(annotations = urhere,
           title = title,
           font = list(
             family = "Roboto Mono",
             color = '#423E75'),
           showlegend = FALSE,
           margin = list(l=35, r=0, b=35, t=0, autoexpand = FALSE),
           yaxis = list(title = "# Addresses (Millions)", 
                        showgrid = FALSE,
                        color = "#423E75"), 
           xaxis = list(title = "Days Active",
                        showticklabels = TRUE,
                        color = "#423E75"),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent",
           hovermode = 'x') %>%
    plotly::config(scrollZoom = FALSE,
                   displayModeBar = FALSE, 
                   displaylogo = FALSE)
  
  return(eoa_plotly)
  
}

plot_tx <- function(eoa_tx) {
  
  maxdate <- Sys.Date()
  mindate <- Sys.Date() - 391
  
  d <- data.frame(
    date = seq.Date(mindate, maxdate, by = 'day')
  )
  
  d$month <- format(d$date, '%b')
  d$day <- weekdays(d$date)
  
  # start on Monday
  d <- d[which(d$day == "Monday")[1]:length(d$day), ]
  d$week <- ceiling(nrow(d)/7) # fill last week number *
  # infill previous weeks (1,1,1,1,1,1,1,2,2,2,2,2,2,2,.... N,N,N,N,N,N,N,*)
  fillweek = floor(nrow(d)/7)*7
  d$week[1:fillweek] <- unlist(lapply(1:(nrow(d)/7), replicate, n = 7))
  
  
  data <- merge(d, eoa_tx, by = "date", all.x = TRUE)
  data <- data[, c("date","week","month", "day", "NUM_TX")]
  data$NUM_TX[is.na(data$NUM_TX)] <- 0
  
  monthlabel = data %>% 
    group_by(month) %>% 
    summarise(w1 = first(week)) %>% 
    dplyr::arrange(w1)
  
  data$day <- toupper(substr(data$day, 1, 3))
  data$day <- ordered(data$day, 
                      levels = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"),
                      labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
  
    colfunc <- colorRampPalette(c("#C8B1F2", "#4F4A59"))
  
  data[data$NUM_TX == 0,]$NUM_TX <- 0.01
  
  hline <- function(y = 0, color = "grey") {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(color = color, width = 0.5)
    )
  }
  
  plot_ly(data,
          x = ~week, 
          y = ~day,
          marker = list(size = ~log(NUM_TX + 1,base = 1.15), 
                        color = "#423E75",
                        line = list(width = 0, color = "#423E75")
          ),
          text = paste0(
            data$day,", ",
            data$date,
            "\nTransactions:",
            round(data$NUM_TX)
          ),
          hoverinfo = 'text', 
          type = 'scatter', mode = "markers") %>%
    layout(
      shapes = list(hline(-0.5), hline(0.5), hline(1.5), hline(2.5), hline(3.5), hline(4.5), hline(5.5), hline(6.5)),
      font = list(
        family = "Roboto Mono",
        color = "#423E75"),
      plot_bgcolor = "transparent", 
      paper_bgcolor = "transparent",
      margin = list(l=20, r=0, b=20, t=0, autoexpand = FALSE),
      yaxis=list(
        showline = FALSE,
        ticklen = 0,
        tickwidth = 0,
        color = "#423E75",
        showgrid = FALSE,
        zeroline = FALSE,
        tickmode="array",
        #tickvals=c(0,1,2,3,4,5,6),
        title="",
        autorange = 'reversed',
        tickangle = 270
      ),
      xaxis= list(
        showline = FALSE,
        showgrid = FALSE,
        zeroline = FALSE,
        color = "#423E75",
        ticklen = 0,
        tickwidth = 0,
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
