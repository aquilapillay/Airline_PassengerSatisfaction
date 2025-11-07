library(shiny)
library(dplyr)
library(ggplot2)

airline_data <- read.csv("airline_dataset.csv")  

ui <- fluidPage(
  titlePanel("Flight Selection and Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("airline", "Select Airline:", choices = c("", unique(airline_data$Airline.Name))),
      checkboxGroupInput("satisfaction_variables", "Select Satisfaction Variables:", 
                         choices = c("Seat Comfort", "Inflight Entertainment", "Cleanliness", "Food and Drink", "Ease of Online Booking")),
      sliderInput("age_range", "Select Age Range:", 
                  min = 0, max = 100, value = c(0, 100)),
      actionButton("filter_button", "Apply Filters")
    ),
    mainPanel(
      plotOutput("visualization")
    )
  )
)
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$filter_button)
    
    filtered <- airline_data
    
    if (input$airline != "") {
      filtered <- filtered %>% filter(Airline.Name == input$airline)
    }
    
    filtered <- filtered %>% filter(Age >= input$age_range[1], Age <= input$age_range[2])
    
    return(filtered)
  })
  
  output$visualization <- renderPlot({
    filtered <- filtered_data()
    selected_variables <- input$satisfaction_variables
    
    if ("Seat Comfort" %in% selected_variables) {
      p <- ggplot(filtered, aes(x = Seat.comfort)) +
        geom_histogram(fill = "skyblue", color = "black") +
        labs(x = "Seat Comfort", y = "Frequency", 
             title = "Distribution of Seat Comfort Ratings")
    }
    
    if ("Inflight Entertainment" %in% selected_variables) {
      p <- ggplot(filtered, aes(x = Inflight.entertainment)) +
        geom_histogram(fill = "lightgreen", color = "black") +
        labs(x = "Inflight Entertainment", y = "Frequency", 
             title = "Distribution of Inflight Entertainment Ratings")
    }
    
    if ("Cleanliness" %in% selected_variables) {
      p <- ggplot(filtered, aes(x = Cleanliness)) +
        geom_histogram(fill = "lightcoral", color = "black") +
        labs(x = "Cleanliness", y = "Frequency", 
             title = "Distribution of Cleanliness Ratings")
    }
    
    if ("Food and Drink" %in% selected_variables) {
      p <- ggplot(filtered, aes(x = Food.and.drink)) +
        geom_histogram(fill = "orange", color = "black") +
        labs(x = "Food and Drink", y = "Frequency", 
             title = "Distribution of Food and Drink Ratings")
    }
    
    if ("Ease of Online Booking" %in% selected_variables) {
      p <- ggplot(filtered, aes(x = Ease.of.Online.booking)) +
        geom_histogram(fill = "lightgray", color = "black") +
        labs(x = "Ease of Online Booking", y = "Frequency", 
             title = "Distribution of Ease of Online Booking Ratings")
    }
    
    return(p)
  })
}

shinyApp(ui = ui, server = server)

