library(shiny)
library(shinyalert)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyalert(),
  
  h2("Student Performance and Risks"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Subset Student Behaviors and Risks:"),
      selectizeInput(
        "categorical1", 
        "x Variable",
        choices=numeric_vars,
        selected=numeric_vars[1]
      ),
      selectizeInput(
        "corr_y",
        "y Variable",
        choices=numeric_vars,
        selected=numeric_vars[2]
      ),
      br(),
      h2("Choose a subset of the data:"),
      radioButtons(
        "family_income_level",
        "Family Income Level",
        choiceNames = c("All", "Low", "Middle", "High"),
        choiceValues = c("All", "Low", "Middle", "High"),
        selected="Middle"
      ),
      radioButtons(
        "Dropout",
        "Dropout Status",
        choiceNames = c("All", "Yes","No"),
        choiceValues = c("All", "Yes", "No"),
        selected = "no"
      ),
      radioButtons(
        "first_generation",
        "First Generation Student",
        choiceNames = c("All", "Yes", "No"),
        choiceValues = c("All", "Yes", "No"),
        selected = "No"
      ),
      
      radioButtons(
        "scholarship_holder",
        "Scholarship Awarded",
        choiceNames = c("All", "Yes", "No"),
        choiceValues = c("All", "Yes", "No"),
        selected = "No"
      ),
      
      radioButtons(
        "gender",
        "Gender",
        choiceNames = c("All", "Female", "Male", "Other"),
        choiceValues = c("All", "Female", "Male", "Other"),
        selected = "All"
      ),
      
      radioButtons(
        "nationality",
        "Nationality",
        choiceNames = c("All", "Australia", "Brazil", "Canada", "China", "Egypt", 
                        "Germany", "India", "Indonesia", "Mexico", "Nigeria", 
                        "Pakistan", "Philippines", "South Korea", "UK", "USA"),
        choiceValues = c("All", "Australia", "Brazil", "Canada", "China", "Egypt", 
                         "Germany", "India", "Indonesia", "Mexico", "Nigeria", 
                         "Pakistan", "Philippines", "South Korea", "UK", "USA"),
        selected = "All"
      ),
      
      radioButtons(
        "course_type",
        "Type of Course",
        choiceNames = c("All", "Arts & Humanities", "Business", 
                        "Health Sciences", "Law & Social Sciences","STEM"),
        choiceValues = c("All", "Arts & Humanities", "Business", 
                         "Health Sciences", "Law & Social Sciences","STEM"),
        selected = "All"
      ),
      
      h2("Select a Sample Size"),
      sliderInput(
        "corr_n",
        "Sample Size",
        min = 20,
        max = 500,
        value = 20,
        step = 1
      ),
      actionButton("corr_sample","Get a Sample!")
    ),
    mainPanel(
      plotOutput("corr_plot"),
      conditionalPanel("input.corr_sample", #only show if a sample has been taken
                       h2("Guess the correlation!"),
                       column(6, 
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1, 
                                           max = 1
                              )
                       ),
                       column(6, 
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

my_sample <- readRDS("student_risk.rds")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$corr_x, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices,
                           selected = corr_y)
    }
  })
  #now, update the 'x' selections available
  observeEvent(input$corr_y, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y){
      choices <- choices[-which(choices == corr_y)]
      updateSelectizeInput(session,
                           "corr_x",
                           choices = choices,
                           selected = corr_x)
    }
  })
  
  
  
  
  sample_corr <- reactiveValues(
    corr_data=NULL,
    corr_truth = NULL
  )
  
  
  ##############################################################
  #Uncomment the next large block of code to go in an
  #observeEvent() to look for the action button (corr_sample)
  #Note you can highlight and bulk comment/uncomment (ctrl+shift+c or similar on mac)
  observeEvent(input$corr_sample, {
    if(input$hhl_corr == "all"){
      hhl_sub <- HHLvals
    } else if(input$hhl_corr == "english"){
      hhl_sub <- HHLvals["1"]
    } else if(input$hhl_corr == "spanish"){
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    
    if(input$fs_corr == "all"){
      fs_sub <- FSvals
    } else if(input$fs_corr == "yes"){
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }
    
    if(input$schl_corr == "all"){
      schl_sub <- SCHLvals
    } else if(input$schl_corr == "no_hs"){
      schl_sub <- SCHLvals[c("0", "01", "02", "03", "04",
                             "05", "06", "07", "08", "09",
                             "10", "11", "12", "13", "14", "15")]
    } else if(input$schl_corr == "hs"){
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    
    corr_vars <- c(input$corr_x, input$corr_y)
    
    subsetted_data <- my_sample |>
      filter(#cat vars first
        HHLfac %in% hhl_sub,
        FSfac %in% fs_sub,
        SCHLfac %in% schl_sub
      ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}
    
    index <- sample(1:nrow(subsetted_data),
                    size = input$corr_n,
                    replace = TRUE,
                    prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
    sample_corr$corr_data <- subsetted_data[index,]
    
    sample_corr$corr_truth <- cor(
      sample_corr$corr_data |> 
        select(all_of(corr_vars))
    )[1,2]
  })
  
  output$corr_plot <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
      geom_point()
  })
  
  
  #This code does the correlation guessing game! Nothign to change here
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ", 
                        round(sample_corr$corr_truth, 4), 
                        "."),
                 type = "success"
      )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!",
                   "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!",
                   "Try guessing a higher value.")
      }
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)