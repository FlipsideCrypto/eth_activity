library(shinyjs)
library(jsonlite)
library(httr)
library(shroomDK)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(reactable)

# gitignored - get your own ShroomDK key from Flipside Crypto!
api_key <- readLines("api_key.txt")

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

plot_eoa <- function(eoadh = eoa_daily_history, range = 1:200){

  eoa_plotly <- plot_ly(data = eoadh[range, ], type = 'bar',
                        x = ~UNIQUE_DAYS,
                        y = ~EOA_FREQ/1e6,
                        text = ~paste0(
                          "Days Active: ", UNIQUE_DAYS,
                          "\nEOAs: ", 
                          scales::label_comma(accuracy = 1)(EOA_FREQ))
  ) %>% 
    layout(title = "",
           yaxis = list(title = "# EOAs (Millions)", 
                        showgrid = FALSE,
                        color = "#FFF"), 
           xaxis = list(title = "Days Active",
                        showticklabels = TRUE,
                        color = "#FFF",
                        gridcolor = "#202933"),
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
  
  return(eoa_plotly)
  
}

tbl_eoa <- function(eoadh = eoa_daily_history, eoa_activity){
  
  x = data.frame(
    "Days Active" = eoa_activity$DAYS_ACTIVE,
    "Tx Count" = eoa_activity$NUM_TX,
    "Percentile" = {
      paste0(
        100 * 
          round(
            eoadh[eoadh$UNIQUE_DAYS == eoa_activity$DAYS_ACTIVE, "eoa_cumprop"],
            4),
        "%"
      )
    },row.names = NULL, check.names = FALSE
  )
  
  
  return(x)

}

# EOA Daily History (query updates DAILY) ----

eoa_daily_history <- fromJSON(
  paste0("https://node-api.flipsidecrypto.com/api/v2/queries/",
         "49534ded-30f3-450a-8d31-558cb159966e/data/latest")
)

eoa_daily_history <- eoa_daily_history %>% 
  mutate(eoa_proportion = EOA_FREQ/sum(EOA_FREQ),
         eoa_cumulative = cumsum(EOA_FREQ),
         eoa_cumprop = cumsum(eoa_proportion))

