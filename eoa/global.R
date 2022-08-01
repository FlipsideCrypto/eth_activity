library(jsonlite)
library(httr)
library(shroomDK)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

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

plot_eoa <- function(eoadh = eoa_daily_history, range = 1:21){
  
  eoa_plot <- ggplot(eoadh[range, ], 
                     aes(x = UNIQUE_DAYS, y = EOA_FREQ/1e6,
                         text = paste0(
                           "Days Active: ", UNIQUE_DAYS,
                           "\nEOAs: ", 
                           scales::label_comma(accuracy = 1)(EOA_FREQ))
                     )) + 
    geom_bar(stat = 'identity') + 
    theme_classic() + 
    xlab("Days Active") + ylab("# EOAs (Millions)") 
  
  eoa_plotly <- ggplotly(eoa_plot, tooltip = "text") %>% 
    layout(xaxis = list(range = c(0, 20)),
           title = list(
             text = "Vast Majority of Ethereum Addresses Rarely Active",
             y = 0.95)
    )
  
  return(eoa_plotly)
  
}

tbl_eoa <- function(eoadh = eoa_daily_history, eoa_activity){
  
  x = data.frame(
    "Days Active" = eoa_activity$DAYS_ACTIVE,
    "Number of Tx" = eoa_activity$NUM_TX,
    "Active Days Percentile" = {
      paste0(
        100 * 
          round(
            eoadh[eoadh$UNIQUE_DAYS == eoa_activity$DAYS_ACTIVE, "eoa_cumprop"],
            4),
        "%"
      )
    },row.names = NULL, check.names = FALSE
  )
  
  
  datatable(x, 
            options = list(paging = FALSE,    ## paginate the output
                           searching = FALSE,
                           autoWidth = TRUE, ## use smart column width handling
                           server = FALSE,   ## use client-side processing
                           dom = 't',
                           columnDefs = list(list(targets = '_all', 
                                                  className = 'dt-center'))
            ),
            extensions = 'Buttons',
            filter = 'none',
            rownames = FALSE
  )

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

