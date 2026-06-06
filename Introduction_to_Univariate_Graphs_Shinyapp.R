# This project is to introduce students who have little to no knowledge of statistics 
# some basic graphs and how to code them in R.

# For each graph, we will introduce how to data will be like and students can play around with the code.

# Some aims:
  # Increase students' critical thinking (asking questions about what they are expecting)
  # Introduce the specifics (x-axis and y-axis) of some graphs
  # Differentiate different graph expectations for qualitatitive and quantitative
  # Understand when to use each graph
  # Increase interpretability of students

# Later:
  # Compare it with some numerical analysis (measures of mean, median, mode)
  # Boxplot comparison
  # Which graph would be a better representation









# Preparation stage

## Libraries
library(shiny)
library(DT)
library(dplyr)


## Datasets

### Due to learning purposes, will exclude rows with null values
datasets <- list(
  mtcars = na.omit(mtcars),
  iris = na.omit(iris),
  airquality = na.omit(airquality)
)


# Define UI
ui <- fluidPage(

  titlePanel("Introduction to Univariate Statistical Graphs"),
  
  sidebarLayout(
    sidebarPanel(
      "Univariate plots are plots where we will only look into one variable at a time. The x-axis will be the variable chosen and the y-axis will be either of the following:",
      tags$ul(
        tags$li("Frequency distribution"),
        tags$li("Cumulative Frequency distribution"),
        tags$li("Relative Frequency distribution"),
        tags$li("Cumulative Relative Frequency distribution")
      ),
      br(),
      "Depending on the variable chosen (quantiative or qualitative) and graph, you can only choose a certain y-axis. We will go through a few steps:",
      tags$ol(
        tags$li("Choose a dataset."),
        tags$li("Choose one variable to plot on the x-axis."),
        tags$li("Choose a suitable y-axis."),
        tags$li("Choose a suitable graph.")
      ),
      br(),
      "Before we start, keep these things in mind:",
      tags$ul(
        tags$li("What do you hope to know from the graph?"),
        tags$li("Is the variable chosen a qualitative or quantitative variable?"),
        tags$li("What would the graph be like in the x-axis and y-axis?")
      ),
      hr(),
      
      selectInput(
        inputId = "dataset",
        label = "Choose a dataset:",
        choices = names(datasets)
      ),
      
      selectInput(
        inputId = "xaxis",
        label = "Choose a variable to plot on the x-axis:",
        choices = NULL
      ),
      
      selectInput(
        inputId = "graph",
        label = "Choose a suitable graph:",
        choices = list(
          "Quantitative/ Numerical" = c("Histogram", "Ogive", "Boxplot", "Dot plot", "Kernel Density Chart"),
          "Qualitative/ Categorical" = c("Bar Charts", "Pie Chart")
        )
      ),
      
      selectInput(
        inputId = "yaxis",
        label = "Choose a suitable y-axis:",
        choices = c("Frequency distribution", 
                    "Cumulative Frequency distribution (Quantitative Only)",
                    "Relative Frequency distribution",
                    "Cumulative Relative Frequency distribution (Quantitative Only)")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Dataset Details",
          uiOutput("dataset_info"), # dataset information
          
          h3("Dataset Preview"),
          dataTableOutput("preview_dataset") # dataset preview
        ),
        
        tabPanel(
          "Plot",
          h3("Plot"),
          plotOutput("preview_plot") # plot preview
          
        )
      )
      
      
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # selected dataset
  selected_data <- reactive({
    datasets[[input$dataset]]
  })
  
  # selected variable to plot (or x-axis)
  selected_variable <- reactive({
    selected_data()[[input$xaxis]]
  })
  
  # Output: Preview of dataset
  output$preview_dataset <- renderDT({
    datatable(selected_data())
  })
  
  # Output: dataset information
  output$dataset_info <- renderUI({
    if (input$dataset == "mtcars"){
      tagList(
        h3("Motor Trend Car Road Tests"),
        tags$ul(
          tags$li("mpg: Miles/(US) gallon"),
          tags$li("cyl: Number of cylinders"),
          tags$li("disp: Displacement (cu.in.)"),
          tags$li("hp: Gross horsepower"),
          tags$li("drat: Rear axle ratio"),
          tags$li("wt: Weight (1000 lbs)"),
          tags$li("qsec: 1/4 mile time"),
          tags$li("vs: Engine (0 = V-shaped, 1 = straight)"),
          tags$li("am: Transmission (0 = automatic, 1 = manual)"),
          tags$li("gear: Number of forward gears"),
          tags$li("carb: Number of carburetors")
          )
      )
    } 
    else if (input$dataset == "iris") {
      tagList(
        h3("Edgar Anderson's Iris Data"),
        p("This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.")
        )
    } 
    else if (input$dataset == "airquality") {
      tagList(
        h3("New York Air Quality Measurements"),
        p("Daily air quality measurements in New York, May to September 1973."),
        tags$ul(
          tags$li("Ozone:	Mean ozone in parts per billion from 1300 to 1500 hours at Roosevelt Island (ppb)"),
          tags$li("Solar.R: Solar radiation in Langleys in the frequency band 4000–7700 Angstroms from 0800 to 1200 hours at Central Park (lang)"),
          tags$li("Wind: Average wind speed in miles per hour at 0700 and 1000 hours at LaGuardia Airport (mph)"),
          tags$li("Temp: Maximum daily temperature in degrees Fahrenheit at LaGuardia Airport"),
          tags$li("Month:	Month (1-12)"),
          tags$li("Day:	Day of month (1-31)")
        )
      )
    }
  })
  
  # Update: x-axis in UI
  observe({
    updateSelectInput(
      session,
      "xaxis",
      choices = names(selected_data())
    )
  })
  
  # Output: Plot of dataset
  output$preview_plot <- renderPlot({
    
    # Stop execution until the user selects "Histogram"
    req(input$graph == "Histogram")
    req(input$xaxis)
    
    x <- selected_variable()
    
    hist(x, 
         col = "skyblue", 
         border = "white", 
         main = paste("Histogram of", input$xaxis),
         xlab = input$xaxis)
  })
  
}

shinyApp(ui, server)