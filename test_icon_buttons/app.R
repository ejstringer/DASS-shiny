#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#https://fontawesome.com/icons/house?s=solid&f=classic  - icons

iconx <- c('ambulance', 'car','bolt', 'ghost', 'mug-hot',
           'umbrella', 'gift', 'video','headphones', 'camera')
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           uiOutput('cost'),
           plotOutput("distPlot"),
           lapply(1:5, function(x) actionButton(paste0('key_', x),
                                                '',# letters[x],
                                                icon = icon(iconx[x])))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$cost <-renderText({
   
    as.character(icon("ghost"))
  })
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        plot(1:10,1:10)
        text(4,5, icon(iconx[1]))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


ui <- shinyUI(
  fluidPage(
    actionButton("Press","press", icon = icon("ghost")),
    uiOutput("cost_compare")
  )
)

server <- function(input, output, session) {
  output$cost_compare <- renderText({
    if(input$Press%%2==0){
      condition <- T
    } else{
      condition <-F
    }
    ifelse(condition,
           as.character(icon("angle-up")), as.character(icon("angle-down")))
    
  }
  
  )
  
}

shinyApp(ui, server)
