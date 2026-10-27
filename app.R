library(shiny)
library(shinyalert)
library(tidyverse)

student_risk <- readRDS("student_risk.rds")

numeric_vars <- names(student_risk)[sapply(student_risk, is.numeric)]
numeric_vars <- setdiff(numeric_vars, "person_id")

categorical_vars <- names(student_risk)[
  sapply(student_risk, \(x) is.factor(x) | is.character(x))
]
categorical_vars <- setdiff(categorical_vars, "person_id")

# Define UI for application
ui <- fluidPage(
  
  h2("Student Performance and Risks"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h2("Student Performance and Risk Variables"),
      
      hr(),
      
      h3("Categorical Variables"),
      
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
      
      h3("Numeric Variables"),
      
      selectInput(
        "numvar1",
        "Numeric Variable 1",
        choices = numeric_vars,
        selected = numeric_vars[1]
      ),
      
      uiOutput("slider1"),
      
      br(),
      
      selectInput(
        "numvar2",
        "Numeric Variable 2",
        choices = numeric_vars,
        selected = numeric_vars[2]
      ),
      
      uiOutput("slider2"),
      
      hr(),
      
      h3("Categorical Filters"),
      
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
        "Scholarship Awarded",
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
        "Type of Course",
        choices = c("All", levels(student_risk$course_type)),
        selected = "All"
      ),
      
      hr(),
      
      actionButton(
        "update_data",
        "Update Data",
        class = "btn-primary"
      )
      
    ),
    
    mainPanel(
      
      plotOutput("catplot", height = "600px")
      
    )
    
  )
  
)

# Define server
server <- function(input, output, session) {
  
  output$slider1 <- renderUI({
    
    sliderInput(
      "slider_num1",
      paste("Filter", input$numvar1),
      min = min(student_risk[[input$numvar1]], na.rm = TRUE),
      max = max(student_risk[[input$numvar1]], na.rm = TRUE),
      value = c(
        min(student_risk[[input$numvar1]], na.rm = TRUE),
        max(student_risk[[input$numvar1]], na.rm = TRUE)
      )
    )
    
  })
  
  output$slider2 <- renderUI({
    
    sliderInput(
      "slider_num2",
      paste("Filter", input$numvar2),
      min = min(student_risk[[input$numvar2]], na.rm = TRUE),
      max = max(student_risk[[input$numvar2]], na.rm = TRUE),
      value = c(
        min(student_risk[[input$numvar2]], na.rm = TRUE),
        max(student_risk[[input$numvar2]], na.rm = TRUE)
      )
    )
    
  })
  
  filtered_data <- eventReactive(input$update_data, {
    
    data <- student_risk
    
    if(input$family_income_level != "All"){
      data <- data |>
        filter(family_income_level == input$family_income_level)
    }
    
    if(input$dropout != "All"){
      data <- data |>
        filter(dropout == input$dropout)
    }
    
    if(input$first_generation != "All"){
      data <- data |>
        filter(first_generation == input$first_generation)
    }
    
    if(input$scholarship_holder != "All"){
      data <- data |>
        filter(scholarship_holder == input$scholarship_holder)
    }
    
    if(input$gender != "All"){
      data <- data |>
        filter(gender == input$gender)
    }
    
    if(input$nationality != "All"){
      data <- data |>
        filter(nationality == input$nationality)
    }
    
    if(input$course_type != "All"){
      data <- data |>
        filter(course_type == input$course_type)
    }
    
    if(!is.null(input$slider_num1)){
      data <- data |>
        filter(
          .data[[input$numvar1]] >= input$slider_num1[1],
          .data[[input$numvar1]] <= input$slider_num1[2]
        )
    }
    
    if(!is.null(input$slider_num2)){
      data <- data |>
        filter(
          .data[[input$numvar2]] >= input$slider_num2[1],
          .data[[input$numvar2]] <= input$slider_num2[2]
        )
    }
    
    data
    
  }, ignoreInit = FALSE)
  
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
      theme_minimal()
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)