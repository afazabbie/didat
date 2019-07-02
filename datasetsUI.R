library(shiny)
source("datasetsServer.R")

datasetsUI <- function(id) {
  ns <- NS(id)
  #Get the varaibles
  fields <- getDatasets()
  names(fields) <- toupper(fields)
  
  tagList(sidebarLayout(
    sidebarPanel(width = 3,
      h4("Upload new dataset"),
      fileInput(
        inputId = ns('file1'),
        'Choose CSV File',
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv'
        )
      ),
      tags$hr(),
      checkboxInput(inputId = ns('header'), 'Header', TRUE),
      radioButtons(
        inputId = ns('sep'),
        label = 'Separator',
        choices = c(
          Comma = ',',
          Semicolon = ';',
          Tab = '\t'
        ),
        selected = ','
      ),
      radioButtons(
        inputId = ns('quote'),
        label = 'Quote',
        choices = c(
          None = '',
          'Double Quote' = '"',
          'Single Quote' = "'"
        ),
        selected = '"'
      )
    ),
    mainPanel(width = 9,
              tabsetPanel("EDA checklist",
                tabPanel(
                  "View Data",
                  verbatimTextOutput(outputId = ns('res')),
                  selectInput(
                    inputId = ns("data_set"), 
                    label = "Select Dataset:",
                    choices = fields
                  ),
                  #textOutput(outputId = ns('contents'))
                  dataTableOutput(outputId = ns('contents'))
                ),
                tabPanel(
                  "Know your data",
                  h4("Data class"),
                  verbatimTextOutput(outputId = ns("class")),
                  h4("Number of rows and columns"),
                  verbatimTextOutput(outputId = ns("dim")),
                  h4("Variable names"),
                  verbatimTextOutput(outputId = ns("name")),
                  h4("Metadata"),
                  verbatimTextOutput(outputId = ns("str")),
                  h4("Summary Statistics"),
                  verbatimTextOutput(outputId = ns("sum"))
                ),
                tabPanel("Top & Down",
                 h4("First 5 rows "),
                 tableOutput(outputId = ns("top")),
                 h4("Last 5 rows"),
                 tableOutput(outputId = ns("bottom"))
                ),
                tabPanel(
                  "EDA Questions"
                )
                )
              )
  ))
}