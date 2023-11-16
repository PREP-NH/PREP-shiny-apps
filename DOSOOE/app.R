#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd('../../')
print(getwd())
source('./helpers.R')



library(shiny)

# locshash <- hash()
# locshash[["Lamprey River"]] <- '32125'
# locshash[["Upper Piscataqua River"]] <- '79039,28225'
# locshash[["Great Bay"]] <- '79069,15318'
# locshash[["Squamscott River"]] <- '79076'
# locshash[["Salmon Falls"]] <- '22506'
# locshash[["Oyster River"]] <- '79081'
# locshash[["Hampton River"]] <- '28202,79043'
# locshash[["Portsmouth Harbor"]] <- '28194'

load('./Shiny/DOPrep.RData')
# head(locshash[["Great Bay"]])
locs <- c('Great Bay', 'Lamprey River', 'Upper Piscataqua River', 
          'Squamscott River', 'Salmon Falls', 'Oyster River', 
          'Hampton River', 'Portsmouth Harbor')

ui <- fluidPage(
  selectInput(inputId="dd", label="pick a location",
              choices=locs),
  plotOutput(outputId = "myHisto")
)


server <- function(input, output) {

  DO_data <- reactive({
    
    do_df<- locshash[[input$dd]]
    DO_df_sm<- DO_under5mglSummer(do_df)
  })
  
  output$myHisto <- renderPlot({
    fig<- DO_under5mgFig(DO_data(), input$dd)
    fig
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
