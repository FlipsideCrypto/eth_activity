library(shinyjs)
library(jsonlite)
library(httr)
library(shroomDK)
library(ggplot2)
library(plotly)
library(dplyr)

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

get_eoa_activity <- function(eoa_address, api_key = api_key, ttl = 0){ 
  
  withProgress(message = "Querying...", detail = "", expr = {
    
  query <- {
  "SELECT FROM_ADDRESS as eoa, 
  count(DISTINCT(date_trunc('DAY', block_timestamp))) as days_active,
  count(*) as num_tx
  FROM ethereum.core.fact_transactions
  WHERE FROM_ADDRESS = lower('_EOA_ADDRESS_')
  GROUP BY eoa
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
  
  incProgress(amount = 0.2, 
              detail = "Cleaning Result...")
  
  df <- clean_query(res)
  
  incProgress(0.5, 
              detail = "Done!")
  return(df)
    })
          }

plot_eoa <- function(eoadh = eoa_daily_history, label = NULL){

  eoadh <- eoadh %>% group_by(eoa_bucket) %>% 
    summarise(sum_eoa = sum(EOA_FREQ))

  
  eoa_plotly <- plot_ly(data = eoadh, 
                        type = 'bar',
                        color = I("#1C6DB8"),
                        x = ~eoa_bucket,
                        y = ~sum_eoa/1e6,
                        hoverinfo = 'text',
                        hovertext = ~paste0(
                          "Days Active: ", eoa_bucket,
                          "\nEOAs: ", 
                          scales::label_comma(accuracy = 1)(sum_eoa))
  ) %>% 
    layout(title = "ETH Accounts by their Historic Days Active",
           font = list(
               family = "Inter",
               color = 'white'),
           yaxis = list(title = "# EOAs (Millions)", 
                        showgrid = FALSE,
                        color = "#FFF"), 
           xaxis = list(title = "Days Active",
                        showticklabels = TRUE,
                        color = "#FFF"),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent",
           legend = list(font = list(color = '#FFFFFF'))) %>%
    # variety of useful config options to be aware of
    # https://plotly.com/r/configuration-options/
    config(scrollZoom = FALSE,
           # displayModeBar = TRUE, # default is hover
           toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                       filename= 'template_image',
                                       height= 500,
                                       width= 700,
                                       scale= 1),
           displaylogo = FALSE
    ) 
  
  if(!is.null(label)){
   eoa_plotly <-eoa_plotly %>% layout(annotations = label)
  }
  
  return(eoa_plotly)
  
}

tbl_eoa <- function(eoadh = eoa_daily_history, eoa_activity){
  
  list(
    "TX Count" = eoa_activity$NUM_TX,
    "Days Active" = eoa_activity$DAYS_ACTIVE,
    "Days Active %tile" = {
      paste0(
        100 * 
          round(
            eoadh[eoadh$UNIQUE_DAYS == eoa_activity$DAYS_ACTIVE, "eoa_cumprop"],
            4),
        "%"
      )
    }
  )
 
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
  


