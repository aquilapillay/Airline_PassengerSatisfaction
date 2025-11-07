# Load necessary packages
library(shiny)
library(dplyr)
library(ggplot2)

# Load your airline dataset
airline_data <- read.csv("Airline Passenger Satisfaction.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Arrival Delay vs Departure Delay Comparison by Airline"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("airline", "Select Airline:", choices = c("", unique(airline_data$Airline.Name))),
      actionButton("update_button", "Update Plot")
    ),
    mainPanel(
      plotOutput("delay_comparison")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$delay_comparison <- renderPlot({
    req(input$update_button)
    
    filtered_data <- airline_data
    if (input$airline != "") {
      filtered_data <- filtered_data %>% filter(Airline.Name == input$airline)
    }
    
    # Plot arrival delay vs departure delay comparison for the selected airline
    ggplot(filtered_data, aes(x = Departure.Delay.in.Minutes, y = Arrival.Delay.in.Minutes)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "purple") + # Add trend line
      labs(x = "Departure Delay (Minutes)", y = "Arrival Delay (Minutes)",
           title = paste("Arrival Delay vs Departure Delay Comparison for", input$airline))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

