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
  
  h2("Student Performance and Dropout Risks"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Select Variables to Filter Data"),
      
      hr(),
      
      h4("Numerical Filters"),
      
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
      
      h4("Categorical Filters"),
      
      selectizeInput(
        "family_income_level",
        "Family Income Level",
        choices = levels(student_risk$family_income_level),
        selected = NULL,
        multiple = TRUE
      ),
      
      selectizeInput(
        "gender",
        "Gender",
        choices = levels(student_risk$gender),
        selected = NULL,
        multiple = TRUE
      ),
      
      selectizeInput(
        "nationality",
        "Nationality",
        choices = levels(student_risk$nationality),
        selected = NULL,
        multiple = TRUE
      ),
      
      selectizeInput(
        "course_type",
        "Type of Course",
        choices = levels(student_risk$course_type),
        selected = NULL,
        multiple = TRUE
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
          
          img(
            src = "students.jpeg",
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
                  "Three-Way Table",
                  "Bar Chart",
                  "Boxplot",
                  "Histogram",
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
    
    if(input$summary_type == "One-Way Table") {
      
      selectInput(
        "oneway_var",
        "Variable",
        choices = categorical_vars
      )
      
    } else if(input$summary_type == "Two-Way Table") {
      
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
      
    } else if(input$summary_type == "Three-Way Table") {
      
      tagList(
        
        selectInput(
          "table_var1",
          "Variable 1",
          choices = categorical_vars
        ),
        
        selectInput(
          "table_var2",
          "Variable 2",
          choices = categorical_vars
        ),
        
        selectInput(
          "table_var3",
          "Variable 3",
          choices = categorical_vars
        )
        
      )
      
    } else if(input$summary_type == "Bar Chart") {
      
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
      
    } else if(input$summary_type == "Boxplot") {
      
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
      
    } else if(input$summary_type == "Histogram") {
      
      selectInput(
        "num_summary_var",
        "Numeric Variable",
        choices = numeric_vars
      )
      
    } else if(input$summary_type == "Numeric Summary") {
      
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
    if(input$summary_type == "One-Way Table") {
      req(input$oneway_var)
      table(
        filtered_data()[[input$oneway_var]]
      )
      
    } 
    else if(input$summary_type == "Two-Way Table") {
      
      req(input$row_var, input$col_var)
      
      table(
        filtered_data()[[input$row_var]],
        filtered_data()[[input$col_var]]
      )
      
    } 
    else if(input$summary_type == "Three-Way Table") {
      
      req(
        input$table_var1,
        input$table_var2,
        input$table_var3
      )
      
      prop.table(
        table(
          filtered_data()[[input$table_var1]],
          filtered_data()[[input$table_var2]],
          filtered_data()[[input$table_var3]]
        )
      )
      
    }
    else if(input$summary_type == "Numeric Summary") {
      
      req(input$group_var, input$num_summary_var)
      
      filtered_data() %>%
        group_by(.data[[input$group_var]]) %>%
        summarize(
          Mean = mean(.data[[input$num_summary_var]], na.rm = TRUE),
          SD = sd(.data[[input$num_summary_var]], na.rm = TRUE),
          Median = median(.data[[input$num_summary_var]], na.rm = TRUE),
          Min = min(.data[[input$num_summary_var]], na.rm = TRUE),
          Max = max(.data[[input$num_summary_var]], na.rm = TRUE),
          .groups = "drop"
        )
      
    }
  })
  
  output$explore_plot <- renderPlot({
      
      validate(
        need(
          nrow(filtered_data()) > 0,
          "No observations match selected filters."
        )
      )
      
    if(input$summary_type == "Bar Chart") {
      
      req(input$bar_x, input$bar_fill)
      
      if(input$bar_fill == "None") {
          
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
            geom_bar(position = "dodge")
          
        }
        
    } else if(input$summary_type == "Boxplot") {
      
      req(input$group_var, input$num_summary_var)
      
      ggplot(
          filtered_data(),
          aes(
            x = .data[[input$group_var]],
            y = .data[[input$num_summary_var]]
          )
        ) +
          geom_boxplot()
        
    } else if(input$summary_type == "Histogram") {
      
      req(input$num_summary_var)
      
      ggplot(
          filtered_data(),
          aes(
            x = .data[[input$num_summary_var]]
          )
        ) +
          geom_histogram(
            bins = 30,
            fill = "steelblue"
          )
        
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
    
    data <- data |>
      filter(
        family_income_level %in% input$family_income_level
      )
    
    data <- data |>
      filter(
        gender %in% input$gender
      )
    
    data <- data |>
      filter(
        nationality %in% input$nationality
      )
    
    data <- data |>
      filter(
        course_type %in% input$course_type
      )
    
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
  

  
}

# Run the application
shinyApp(ui = ui, server = server)