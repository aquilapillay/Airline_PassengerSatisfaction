library(shiny)
library(dplyr)
library(ggplot2)

airline_data <- read.csv("airline_dataset.csv")  # Replace "airline_dataset.csv" with your dataset file path

ui <- fluidPage(
  titlePanel("Rating Distribution by Gender"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:", choices = c("", unique(airline_data$Gender))),
      actionButton("update_button", "Update Plot")
    ),
    mainPanel(
      plotOutput("rating_distribution")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$rating_distribution <- renderPlot({
    req(input$update_button)
    
    filtered_data <- airline_data
    if (input$gender != "") {
      filtered_data <- filtered_data %>% filter(Gender == input$gender)
    }
    
    bins <- seq(0, 5, by = 0.5)
    

    rating_counts <- cut(filtered_data$Overall.Satisfaction.Rating, bins, right = FALSE)
    
 
    ggplot() +
      geom_bar(aes(x = rating_counts), fill = "red", color = "black") +
      labs(x = "Overall Satisfaction Rating", y = "Count", 
           title = "Rating Distribution by Gender")
  })
}
shinyApp(ui = ui, server = server)
