library(shiny)
library(shinyalert)
library(tidyverse)

# Load data
student_risk <- readRDS("student_risk.rds")

numeric_vars <- names(student_risk)[sapply(student_risk, is.numeric)]

categorical_vars <- names(student_risk)[
  sapply(student_risk, function(x)
    is.factor(x) | is.character(x))
]

# UI
ui <- fluidPage(
  
 titlePanel("Student Performance and Risks"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Select Variables"),
      
      selectInput(
        "catvar1",
        "Categorical Variable",
        choices = categorical_vars,
        selected = categorical_vars[1]
      ),
      
      selectInput(
        "catvar2",
        "Group By",
        choices = categorical_vars,
        selected = categorical_vars[2]
      ),
      
      hr(),
      
      h3("Filter Data"),
      
      radioButtons(
        "family_income_level",
        "Family Income Level",
        choices = c("All", "Low", "Middle", "High"),
        selected = "All"
      ),
      
      radioButtons(
        "dropout",
        "Dropout Status",
        choices = c("All", "Yes", "No"),
        selected = "All"
      ),
      
      radioButtons(
        "first_generation",
        "First Generation Student",
        choices = c("All", "Yes", "No"),
        selected = "All"
      ),
      
      radioButtons(
        "scholarship_holder",
        "Scholarship Holder",
        choices = c("All", "Yes", "No"),
        selected = "All"
      ),
      
      radioButtons(
        "gender",
        "Gender",
        choices = c("All", "Female", "Male", "Other"),
        selected = "All"
      ),
      
      radioButtons(
        "nationality",
        "Nationality",
        choices = c("All", levels(student_risk$nationality)),
        selected = "All"
      ),
      
      radioButtons(
        "course_type",
        "Course Type",
        choices = c("All", levels(student_risk$course_type)),
        selected = "All"
      )
      
    ),
    
    mainPanel(
      
      plotOutput("catplot", height = "600px")
      
    )
    
  )
  
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    data <- student_risk
    
    if (input$family_income_level != "All") {
      data <- data |> 
        filter(family_income_level == input$family_income_level)
    }
    
    if (input$dropout != "All") {
      data <- data |> 
        filter(dropout == input$dropout)
    }
    
    if (input$first_generation != "All") {
      data <- data |> 
        filter(first_generation == input$first_generation)
    }
    
    if (input$scholarship_holder != "All") {
      data <- data |> 
        filter(scholarship_holder == input$scholarship_holder)
    }
    
    if (input$gender != "All") {
      data <- data |> 
        filter(gender == input$gender)
    }
    
    if (input$nationality != "All") {
      data <- data |> 
        filter(nationality == input$nationality)
    }
    
    if (input$course_type != "All") {
      data <- data |> 
        filter(course_type == input$course_type)
    }
    
    data
    
  })
  
  output$catplot <- renderPlot({
    
    req(nrow(filtered_data()) > 0)
    
    ggplot(
      filtered_data(),
      aes(
        x = .data[[input$catvar1]],
        fill = .data[[input$catvar2]]
      )
    ) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = input$catvar1,
        fill = input$catvar2,
        y = "Percent",
        title = paste(
          input$catvar2,
          "within",
          input$catvar1
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
}

# Run App
shinyApp(ui = ui, server = server)