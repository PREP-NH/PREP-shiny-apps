library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(ggplot2)
library(lubridate)
library(scales)
# Function to get data from the API
getprepdbdata <- function(resultid, start_date, end_date){
  # Format the start_date and end_date
  start_date_formatted <- format(as.Date(start_date), "%Y-%m-%dT%H:%M:%S")
  end_date_formatted <- format(as.Date(end_date), "%Y-%m-%dT%H:%M:%S")

  # Make the GET request with the start and end date

  dbhtml <- GET(url = paste0('http://data.prepestuaries.org:3001/timeseriesresultvalues?',
                                   'and=(valuedatetime.gt.%22', start_date_formatted,
                                 '%22,valuedatetime.lt.%22', end_date_formatted,
                                 '%22,resultid.in.(', resultid, '))'
                               ))
  db_text <- content(dbhtml, "text", encoding = "UTF-8")
  dbjson <- fromJSON(db_text, flatten = TRUE)
  db <- as.data.frame(dbjson)

  return(db)
}
data <- read.csv("wiswalldamtimeseries.csv", encoding = "UTF-8")

# Create a text representation for the dropdown from the 'variable' and 'units' columns
data$dropdown_label <- paste(data$variable, data$units, sep=" - ")

# UI setup
ui <- fluidPage(
  titlePanel("Wiswall Dam sensor data plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("result_id", "Select a Result", choices=setNames(data$resultid,data$dropdown_label)),
      dateInput("startdate", "Start date", value = floor_date(Sys.Date(), "month")),
      dateInput("enddate", "End date", value = Sys.Date()),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      plotlyOutput("timeseriesPlot")
    )
  )
)
# Server setup
server <- function(input, output, session) {

  # Define a reactive value that updates both data and label when the update button is clicked
  reactiveDataAndLabel <- eventReactive(input$update, {
    # Ensure the result_id is selected
    req(input$result_id)

    # Fetch the data using the getprepdbdata function
    fetchedData <- getprepdbdata(input$result_id, input$startdate, input$enddate)
    fetchedData$valuedatetime <- as.POSIXct(fetchedData$valuedatetime, format="%Y-%m-%dT%H:%M:%S")
    # Fetch the label for the plot
    label <- data$dropdown_label[which(data$resultid == input$result_id)]

    # Return a list containing both the data and label
    list(data = fetchedData, label = label)
  })

  # Render the plot output in response to the reactiveDataAndLabel() being triggered
  output$timeseriesPlot <- renderPlotly({
    # Ensure that the data list has been fetched following an update button press
    dataList <- req(reactiveDataAndLabel())

    # Extract the data and label from the list
    df <- dataList$data
    label <- dataList$label

    # Ensure we have a valid label
    req(label) # Make sure label is not NULL or empty

    # Assuming the data frame 'df' has a 'date' and 'value' column for plotting
    p<- ggplot(df, aes(x = valuedatetime, y = datavalue, text = paste("Value:", datavalue, " Datetime:", valuedatetime))) +
      geom_point() +
      labs(title = label, x = "Date", y = label) +
      theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_datetime(labels = date_format("%m/%d/%Y"))

    ggplotly(p, tooltip = "text")
  })

}

# Run the app
shinyApp(ui, server)