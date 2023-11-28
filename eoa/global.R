library(shinyjs)
library(ggplot2)
library(dplyr)
library(plotly)
library(jsonlite)
library(httr)
library(shroomDK)

# gitignored - get your own ShroomDK key from Flipside Crypto!
# for local deployments read your own api_key
# for RConnect deployments rely on manual setting of environment variable

api_key <- ifelse(file.exists('api_key.txt'),
                  readLines("api_key.txt"),
                  Sys.getenv('api_key'))  

#423E75 # dark purple
#CECBF5 # light purple
#0D0C1C # black background

# EOA Daily History (query updates DAILY) ----

eoa_daily_history <- fromJSON(
  paste0("https://node-api.flipsidecrypto.com/api/v2/queries/",
         "49534ded-30f3-450a-8d31-558cb159966e/data/latest")
)

eoa_daily_history$eoa_bucket <- cut(eoa_daily_history$UNIQUE_DAYS,
                                    breaks = c(0,1,10,50,100,250,1000,Inf),
                                    labels = c("1","2-10","11-50","51-100","101-250","251-1000","1001+"))


eoa_daily_history <- eoa_daily_history %>%
  mutate(eoa_proportion = EOA_FREQ/sum(EOA_FREQ),
         eoa_cumulative = cumsum(EOA_FREQ),
         eoa_cumprop = cumsum(eoa_proportion))

# FUNCTIONS ----

get_tx_by_day <- function(eoa_address, api_key = api_key, ttl = 0){
  
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
    
    df <- shroomDK::auto_paginate_query(query = query, api_key = api_key)
    colnames(df) <- tolower(colnames(df))
    df$date <- as.Date(df$day_)
    
    return(df)

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
  mindate <- Sys.Date() - 375
  
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
  data <- data[, c("date","week","month", "day", "num_tx")]
  data$num_tx[is.na(data$num_tx)] <- 0
  
  data$day <- toupper(substr(data$day, 1, 3))
  data$day <- ordered(data$day, 
                      levels = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"),
                      labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
  
  colfunc <- colorRampPalette(c("#C8B1F2", "#4F4A59"))
  
  s <- scale(data$num_tx)
  m <- min(s)
  
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
  
  # purposefully grabbed > 1 year to chop to a good Monday and current month.
  data <- data[-c(1:7), ]
  
  monthlabel = data %>% 
    group_by(month) %>% 
    summarise(w1 = first(week)) %>% 
    dplyr::arrange(w1)
  
  monthlabel$w1[1] <- 1 
  
  initial_months <- months(data$date)[
    c(which(!(months(data$date)[-nrow(data)] == months(data$date)[-1])),
      nrow(data) )]
  
  # not all months are 4 weeks, so expect some squishing but still readable
  ticktext = substr(initial_months, 1, 3)
  tickvals = c(monthlabel$w1, max(monthlabel$w1)+4)
  
  plot_ly(data,
          x = ~week, 
          y = ~day,
          marker = list(size = ~4*(scale(data$num_tx) - m), 
                        color = "#423E75",
                        line = list(width = 0, color = "#423E75")
          ),
          text = paste0(
            data$day,", ",
            data$date,
            "\nTransactions:",
            data$num_tx),
          hoverinfo = 'text', 
          type = 'scatter', mode = "markers") %>%
    layout(
      shapes = list(hline(-0.5), hline(0.5), hline(1.5), 
                    hline(2.5), hline(3.5), 
                    hline(4.5), hline(5.5), hline(6.5)),
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
        tickwidth = 1,
        tickangle = 0,
        ticktext = ticktext,
        tickvals = tickvals,
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
