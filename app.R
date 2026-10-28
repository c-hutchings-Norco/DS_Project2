library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(gt)


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
      
      tabsetPanel(
        
        tabPanel(
          "About",
          
          h2("Student Academic Performance and Dropout Risk"),
          
          p("This application allows users to explore factors associated with student academic success and dropout risk."),
          
          p("The sidebar allows filtering the data based on demographic and academic characteristics."),
          
          p("The Data Download tab displays the filtered dataset and allows downloading the subsetted data."),
          
          p("The Data Exploration tab provides graphical and numerical summaries."),
          
          tags$a(
            href = "https://www.kaggle.com/datasets/mohankrishnathalla/student-academic-performance-and-dropout-risk",
            "Dataset Source",
            target = "_blank"
          ),
          
          br(),
          br(),
          
          img(
            src = "students.jpg",
            width = "500px"
          )
        ),
        
        tabPanel(
          "Data Download",
          
          DTOutput("datatable"),
          
          br(),
          
          downloadButton(
            "download_data",
            "Download Filtered Data"
          )
        ),
        
        tabPanel(
          "Data Exploration",
          
          sidebarLayout(
            
            sidebarPanel(
              
              radioButtons(
                "summary_type",
                "Select Analysis",
                choices = c(
                  "One-Way Table",
                  "Two-Way Table",
                  "Bar Chart",
                  "Numeric Summary"
                )
              ),
              
              uiOutput("analysis_ui")
              
            ),
            
            mainPanel(
              
              tableOutput("summary_table"),
              
              withSpinner(plotOutput("explore_plot"))
              
            )
          )
        )
      )
    )
      
    )
    
)

# Define server
server <- function(input, output, session) {
  output$datatable <- renderDT({
    
    filtered_data()
    
  })
  output$analysis_ui <- renderUI({
    
    if(input$summary_type == "One-Way Table"){
      
      selectInput(
        "oneway_var",
        "Variable",
        choices = categorical_vars
      )
      
    }
    
    else if(input$summary_type == "Two-Way Table"){
      
      tagList(
        
        selectInput(
          "row_var",
          "Row Variable",
          choices = categorical_vars
        ),
        
        selectInput(
          "col_var",
          "Column Variable",
          choices = categorical_vars
        )
        
      )
      
    }
    
    else if(input$summary_type == "Bar Chart"){
      
      tagList(
        
        selectInput(
          "bar_x",
          "X Variable",
          choices = categorical_vars
        ),
        
        selectInput(
          "bar_fill",
          "Fill Variable",
          choices = c("None", categorical_vars)
        )
      )
    }
    
    else {
      
      tagList(
        
        selectInput(
          "num_summary_var",
          "Numeric Variable",
          choices = numeric_vars
        ),
        
        selectInput(
          "group_var",
          "Group By",
          choices = categorical_vars
        )
        
      )
      
    }
    
  })
  
  output$download_data <- downloadHandler(
    
    filename = function(){
      
      paste0(
        "student_risk_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file){
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  output$summary_table <- renderTable({
    
    req(filtered_data())
    
    if(input$summary_type == "One-Way Table"){
      
      table(
        filtered_data()[[input$oneway_var]]
      )}
      if(input$summary_type == "Two-Way Table"){
        
        table(
          filtered_data()[[input$row_var]],
          filtered_data()[[input$col_var]]
        )}
    if(input$summary_type == "Numeric Summary"){
      
      filtered_data() %>%
        group_by(
          .data[[input$group_var]]
        ) %>%
        summarize(
          Mean = mean(
            .data[[input$num_summary_var]],
            na.rm = TRUE
          ),
          SD = sd(
            .data[[input$num_summary_var]],
            na.rm = TRUE
          ),
          Median = median(
            .data[[input$num_summary_var]],
            na.rm = TRUE
          ),
          Min = min(
            .data[[input$num_summary_var]],
            na.rm = TRUE
          ),
          Max = max(
            .data[[input$num_summary_var]],
            na.rm = TRUE
          )
        )
      
    }
    
  })
  
  output$explore_plot <- renderPlot({
    
    req(filtered_data())
    
    if(input$summary_type == "Bar Chart"){
      
      if(input$bar_fill == "None"){
        
        ggplot(
          filtered_data(),
          aes(x = .data[[input$bar_x]])
        ) +
          geom_bar(fill = "steelblue")
        
      } else {
        
        ggplot(
          filtered_data(),
          aes(
            x = .data[[input$bar_x]],
            fill = .data[[input$bar_fill]]
          )
        ) +
          geom_bar(
            position = "dodge"
          )
        
      }
      
    }
    
  })
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