# Shiny app to explore events and buttons
# Nicholas Horton (nhorton@amherst.edu). May 19, 2022
library(tidyverse)
library(shiny)
library(shinybusy)
library(mdsr)
library(RSQLite)

mydb <- dbConnect(RSQLite::SQLite(), ":memory:")
init_data <- tibble(
  name = c("Nick", "Maura", "Julia", "Ben"), 
  series = c("The West Wing", "Sports Night", "The Crown", "The Simpsons"),
  network = c("NBC", "ABC", "Netflix", "Fox")
)
dbWriteTable(mydb, "tvseries", init_data, overwrite = TRUE)



ui <- fluidPage(
  titlePanel("TV series database"),
  # some things take time: this lets users know
  add_busy_spinner(spin = "fading-circle"),
  fluidRow(
    # display dynamic list of networks to select
    column(4, uiOutput("networkcontrols")),
    column(4, 
           textInput(
             inputId = "download",
             label = "Download widget goes here:", 
             value = ""
           )
    )
  ),
  fluidRow(
    column(4, 
           textInput(
             inputId = "nameinput",
             label = "Name to add:", 
             value = ""
           )
    ),
    column(4, 
           textInput(
             inputId = "seriesinput",
             label = "Series and network inputs goes here:", 
             value = ""
           )
    ),
    column(4, 
           textInput(
             inputId = "actionbutton",
             label = "Add the button to add the inputs here:", 
             value = ""
           )
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)

server <- function(input, output) {

  db_contents <- reactive({  
    data <- dbGetQuery(mydb, "SELECT * FROM tvseries")
    return(data)
  })
  
  db_subset <- reactive({  
    data <- db_contents()
    req(input$network)  # wait until there's a selection
    if (input$network != "ALL") {
      data <- filter(data, network == input$network)
    }
    return(data)
  })
  
  output$networkcontrols <- renderUI({
    availablelevels <-
      unique(sort(as.character(db_contents()$network)))
    selectInput(
      inputId = "network",
      label = "Network selector:",
      choices = c("ALL", availablelevels)
    )
  })
  
  output$table <- DT::renderDataTable(DT::datatable(db_subset()))

}

shinyApp(ui = ui, server = server)
