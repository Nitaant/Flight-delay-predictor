library(shiny)
library(randomForest)

# Load the saved random forest model
model <- readRDS("model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Flight Delay Prediction"),
  sidebarLayout(
    sidebarPanel(
      textInput("dep_delay", "Departure Delay (minutes):"),
      selectInput("carrier", "Carrier:",
                  choices = unique(as.character(data$carrier))),
      selectInput("origin", "Origin:",
                  choices = unique(as.character(data$origin))),
      selectInput("dest", "Destination:",
                  choices = unique(as.character(data$dest))),
      br(),
      actionButton("submit", "Submit"),
      actionButton("reset", "Reset")
    ),
    mainPanel(
      h3("Predicted Arrival Delay:"),
      verbatimTextOutput("prediction"),
      verbatimTextOutput("accuracy"),
      verbatimTextOutput("precision"),
      verbatimTextOutput("recall"),
      tags$p("From the Given outputs we can observe that the outputs only differ when the departure time is changed.The reasons for the departure delay can be based on weather, air-traffic which can be useful in getting even more accurate results.Arrival delay can also be caused due to similar reasons.According to the data, collected it can be observed that the origin and destination do not affect much"),
      
    )
  )
)

# Define server
server <- function(input, output) {
  # Define reactive for user inputs
  flight_data <- reactive({
    data.frame(
      dep_delay = as.numeric(input$dep_delay),
      carrier = input$carrier,
      origin = input$origin,
      dest = input$dest
    )
  })
  
  # Define reactive for predicted arrival delay
  prediction <- eventReactive(input$submit, {
    predict(model, flight_data())
  })
  
  # Output predicted arrival delay
  output$prediction <- renderPrint({
    if (!is.na(prediction())) {
      if (prediction() > 0) {
        paste0("Yes, the flight will be delayed by ", round(prediction(), 2), " minutes.")
      } else if (prediction() < 0) {
        paste0("No, the flight will reach early by ", round(abs(prediction()), 2), " minutes.")
      } else {
        "The flight is on time."
      }
    }
  })
  
  output
  


 
  # Reset input values
  observeEvent(input$reset, {
    updateTextInput(session, "dep_delay", value = "")
    updateSelectInput(session, "carrier", selected = "")
    updateSelectInput(session, "origin", selected = "")
    updateSelectInput(session, "dest", selected = "")
  })
}


# Run the app
shinyApp(ui = ui, server = server)