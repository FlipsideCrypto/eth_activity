library(shiny)
source("global.R")

# Define UI -------

shinyUI(fluidPage(
    title = 'ETH Activity',
    useShinyjs(),
    
    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
      tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Roboto+Mono'),
      tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Inter')
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
    
    # APP LABEL HERE -----------------------------------  
    
    withTags({
      section(class='hero',
              h1(
                class='header', 
                'ETH Activity', 
              ),
              p('Search your address to get stats on your activity'),
      )
    }),
    
    # APP START HERE -----------------------------------  
   
    div(class = 'chart-block',
        fluidRow(
                 column(10,
                        textInput(inputId = "address", label = "",
                                  placeholder = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045",
                                  width = '90%')),
                 column(2, class = 'eoa-btn',
                        actionButton("submit", label = "Search", width = '90%'))
        )
    ),
    
    div(
      class = 'chart-container',
      div(
        class = 'chart-block',
        div(class = 'chart-title', span('Your ETH Activity')),
        div(class = 'chart',
            fluidRow(
              column(3,
                     conditionalPanel("input.submit > 0",
                                      div(class = 'eoa-tbl',
                                          uiOutput('compare'))
                     )),
              column(9, 
                     div(class = 'heat',
                         conditionalPanel("input.submit > 0",
                                          uiOutput('heatmap'))
                     )
              )
            ),
            plotlyOutput("main_plot")
        )
      )
    ),
    div(class = "about",
        h3("About"),
        br(),
        p("Built w/ ❤️ by the team and community at Flipside Crypto."),
        p("Have a feature request, or want to build using Flipside's free data?"),
        HTML(
          paste0("Join us in ","<u><a href = 'https://flipside.com/discord'>Discord</a><u>")
        )
    )
    
) # end FluidPage
) # end shinyUI


