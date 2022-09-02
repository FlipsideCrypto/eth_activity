library(shiny)
library(shinyscreenshot)
source("global.R")

# Define UI -------
shinyUI(fluidPage(
  title = 'Eth Timecard',
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
               'Flipside Crypto', onclick = 'rudderstack.track("ntr-click-flipside-icon")'),
             section(class='socials',
                     a(class='twitter', href='https://twitter.com/flipsidecrypto',
                       'Twitter', onclick = 'rudderstack.track("ntr-click-twitter-icon")'),
                     a(class='discord', href='https://flipsidecrypto.com/discord',
                       'Discord', onclick = 'rudderstack.track("ntr-click-discord-icon")'),
                     a(href='https://app.flipsidecrypto.com/auth/signup/', # redirects to xyz signup
                       'Join Us', onclick = 'rudderstack.track("ntr-click-signup-icon")')
             )
           )
    )
  }),
  
  div(class = "timecard-holder", id = 'tch', 
      fluidRow(class='solid',
               column(1, ""),
               column(10, div(class='title', 'Ethereum Activity'),
                      div(class = "subtitle", 'Your Onchain Timecard')),
               column(1, screenshotButton(label = NULL, selector = '#smallshot', filename = 'mytimecard', scale = 3))
      ),
      br(),
      fluidRow(class = "address-row",
               column(9, class = 'light', style = "border-width: 1px 1px 1px 0;",
                      div(style = "height: 60px", # sorry for the inline styling, it's just easier this way :crying:
                          "Address",
                          textInput(inputId = "address", 
                                    label = NULL,
                                    placeholder = "enter a full address (non-ens)", 
                                    width = '400px'))
               ),
               column(3, class = 'light', id = "go-button", 
                      style = "border-width: 1px 0 1px 0;",
                      div(style = "height: 49px; width: 100%", 
                          div(id = "click", "FILL OUT"))
               ) # close column 3
      ), # close row
      div(id = "smallshot", style = "background-color: #CECBF5;",
          fluidRow(
            column(4, class = 'light',
                   style = "border-width: 0 1px 1px 0;",
                   div(style = "height: 60px; width: 100%",
                       "# Txn.", div(class = "show-result", textOutput("ntxn")))),
            column(4, class = 'light',
                   style = "border-width: 0 1px 1px 0;",
                   div(style = "height: 60px; width: 100%",
                       "Days Active", div(class = "show-result", textOutput("days")))),
            column(4, class = 'light',
                   style = "border-width: 0 0 1px 0;",
                   div(style = "height: 60px; width: 100%",
                       "Fees Ξ", div(class = "show-result", textOutput("fees"))))
          ),
          
          fluidRow(
            column(12,class = "light",
                   style = "border-width: 0 0 1px 0;",
                   div(style = "width: 100%",
                       div("Daily Punchcard"), br(),
                       plotlyOutput('heatmap', width = "100%", height = "300px")
                   ), br(),
            )
          )),
      fluidRow(class='solid',
               div(class = "subtitle", 'You vs. Everyone Else'),
      ),
      
      fluidRow(
        column(6, class = 'light',  style = "border-width: 0 1px 1px 0;",
               div(style = "height: 60px;",
                   "Favorite Day", div(class = "show-result", textOutput("favorite_days")))),
        column(6, class = 'light',
               style = "border-width: 0 0 1px 0;",
               div(style = "height: 60px; width: 100%",
                   "More Active Than", div(class = "show-result", textOutput("percentile"))))
      ),
      
      fluidRow(
        column(12, class = "light",
               style = "border-width: 0;",
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
        column(6, class = 'light foot', style = "border-width: 0 1px 1px 0;",
               div(style = "height: 60px; width: 100%",
                   HTML(
                     paste0(
                       "Built w/ ❤️ by the team at Flipside Crypto. Powered by ",
                       "<a href = 'https://sdk.flipsidecrypto.xyz/shroomdk'>ShroomDK</a>"
                     ))
               )),
        column(6, class = 'light foot', style = "border-width: 0 0 1px 0;",
               div(style = "height: 60px; width: 100%",
                   p("Part 1 of our mission to clock you onchain. What do you want to know about yourself?"),
               ))
      ),
      fluidRow(
        column(12, class = 'light foot', style = "border-width: 0 0 1px 0;",
               div(style = "height: 60px; width: 100%, text-align: center",
                   HTML(
                     paste0("Join us in ",
                            "<a href = 'https://flipsidecrypto.com/discord'>Discord</a>",
                            br(),"Get the code on ",
                            "<a href = 'https://github.com/FlipsideCrypto/eth_activity'>Github</a>")
                   )             
               ))
      ),
      fluidRow(
        div(class = 'about',
            screenshotButton(label = "Print Your Card", selector = '#tch', filename = 'mytimecard')
        )
      )
      
  ) # close timecard-holder div
  
) # end FluidPage
) # end shinyUI


