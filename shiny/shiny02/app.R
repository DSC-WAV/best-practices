# Nicholas Horton
# nhorton@amherst.edu   Sun Oct 13 06:29:48 CDT 2019
# with thanks to the RStudio dynamic UI help page
library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(macleish)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel("Macleish weather app"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h3("Plotting"),      # Third level header: Plotting
      
      # Select variable for y-axis 
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Temperature (C)" = "temperature", 
                              "Wind Speed (meters per second)" = "wind_speed", 
                              "Relative humidity (mm)" = "rel_humidity", 
                              "Atmospheric pressure (millibars)" = "pressure", 
                              "Rainfall (mm)" = "rainfall"), 
                  selected = "temperature"),
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.1),
      # Header
      h3("Subsetting and sampling"),
      
      checkboxInput(inputId = "smoother", "Include smoother (blue line)?", 
                    value = FALSE),
      conditionalPanel(
        condition = "input.smoother == true", # Javascript!
        selectInput("smoothMethod", label = "Which method?",
                    choices = list("auto", "lm", "loess"),
                    selected = "auto")
      ),
      dateRangeInput("daterange", "Date range:",
                     start = "2015-01-01",
                     end   = "2015-12-31",
                     min = "2015-01-01", 
                     max = "2015-12-31"),
      br()
    ),
    
    # Output:
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "scatterplot")),
                  tabPanel(title = "Data", 
                           br(),
                           DT::dataTableOutput(outputId = "weatherdata"))
      )
      
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  orcharddata <- reactive({
    orchard_subset <- filter(orchard_2015, 
                             when > input$daterange[1],
                             when < input$daterange[2])
    return(orchard_subset)
  })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    g <- ggplot(data = orcharddata(), aes_string(x = "when", y = input$y)) +
      geom_point(alpha = input$alpha) +
      labs(x = "Date and time",
           y = toTitleCase(str_replace_all(input$y, "_", " ")))
    if (input$smoother) {
      g <- g + stat_smooth(method = input$smoothMethod, se = FALSE, size = 2)
    }
    g
  })
  
 
  # Print data table if checked
  output$weatherdata <- DT::renderDataTable(
      DT::datatable(data = orcharddata(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
  )
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
