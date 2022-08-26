library(shiny)
source("global.R")

# Define UI -------

shinyUI(fluidPage(
  title = 'Ethereum Activity',
  useShinyjs(),
  
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Roboto+Mono'),
    tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Inter'),
    tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Architects+Daughter')
  ),
  tags$head(tags$script(src = 'rudderstack.js')),
  tags$style(type='text/css',
             '.shiny-output-error { visibility: hidden; }',
             '.shiny-output-error:before { visibility: hidden; }'
  ),
  
  withTags({
    header(class='top-banner',
           section(
             a(class='fs-logo', href='https://www.flipsidecrypto.com', 
               'Powered by Flipside Crypto', onclick = 'rudderstack.track("ntr-click-flipside-icon")'),
             section(class='socials',
                     a(class='twitter', href='https://twitter.com/flipsidecrypto',
                       'Twitter', onclick = 'rudderstack.track("ntr-click-twitter-icon")'),
                     a(class='linkedin', href='https://www.linkedin.com/company/flipside-crypto',
                       'LinkedIn', onclick = 'rudderstack.track("ntr-click-linkedin-icon")'),
                     a(class='discord', href='https://flipsidecrypto.com/discord',
                       'Discord', onclick = 'rudderstack.track("ntr-click-discord-icon")'),
                     a(href='https://app.flipsidecrypto.com/auth/signup/', # redirects to xyz signup
                       'Sign Up', onclick = 'rudderstack.track("ntr-click-signup-icon")')
             )
           )
    )
  }),
  
  div(class = "timecard-holder", 
      fluidRow(class='solid',
               div(class='title', 'Ethereum Activity'),
               div(class = "subtitle", 'Your Onchain Timecard'),
      ),
      br(),
      fluidRow(class = "address-row",
               column(9, class = 'light-left',
                      div(style = "height: 60px", # sorry for the inline styling, it's just easier this way :crying:
                          "Address",
                          textInput(inputId = "address", 
                                    label = NULL,
                                    placeholder = "enter a full address (non-ens)", 
                                    width = '400px'))
               ),
               column(3, class = 'light-right', id = "go-button",
                      div(style = "height: 60px; width: 100%", 
                          "Fill Out",
                          div(id = "click", HTML("<b>PUNCH</b>")))
               ) # close column 3
      ), # close row
      
      fluidRow(
        column(4, class = 'light-left',
               div(style = "height: 60px; width: 100%",
                   "# Txn.", div(class = "show-result", textOutput("ntxn")))),
        column(4, class = 'light-left',
               div(style = "height: 60px; width: 100%",
                   "Days Active", div(class = "show-result", textOutput("days")))),
        column(4, class = 'light-left',
               div(style = "height: 60px; width: 100%",
                   "Fees Ξ", div(class = "show-result", textOutput("fees"))))
      ),
      
      fluidRow(
        column(12,class = "light-left",
               div(style = "width: 100%",
                   div("Daily Punchcard"), br(),
                   plotlyOutput('heatmap', width = "100%", height = "300px")
               ), br(),
        )
      ),
      fluidRow(class='solid',
               div(class = "subtitle", 'You vs. Everyone Else'),
      ),
      
      fluidRow(
        column(6, class = 'light-left',
               div(style = "height: 60px; width: 100%",
                   "Favorite Day", div(class = "show-result", textOutput("favorite_days")))),
        column(6, class = 'light-left',
               div(style = "height: 60px; width: 100%",
                   "More Active than", div(class = "show-result", textOutput("percentile"))))
      ),
      
      fluidRow(
        column(12,class = "light-left",
               div(style = "width: 100%",
                   div("Active Days"),
                   plotlyOutput('main_plot', width = "100%", height = "300px")
               ), 
               br()       
        )
      ),
      
      fluidRow(class='solid',
               div(class = "subtitle", 'About this Timecard'),
      ),
      fluidRow(
        column(6, class = 'light-left foot',
               div(style = "height: 60px; width: 100%",
                   HTML(
                     paste0(
                       "Built w/ ❤️ by the team at Flipside Crypto. Powered by ",
                       "<u><a href = 'https://sdk.flipsidecrypto.xyz/shroomdk'>ShroomDK</a></u>"
                     ))
                   )),
        column(6, class = 'light-right foot',
               div(style = "height: 60px; width: 100%",
                   p("Part 1 of our mission to clock you onchain. What do you want to know about yourself?"),
                   ))
      ),
      fluidRow(
        column(12, class = 'light-left foot',
               div(style = "height: 60px; width: 100%, text-align: center",
                   HTML(
                     paste0("Join us in ",
                            "<u><a href = 'https://flipsidecrypto.com/discord'>Discord</a></u>",
                            br(),"Code is open source on ",
                            "<u><a href = 'https://github.com/FlipsideCrypto/eth_activity'>Github</a></u>")
                   )             
               ))
      )
      
  ) # close timecard-holder div
  
) # end FluidPage
) # end shinyUI


