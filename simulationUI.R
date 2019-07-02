library(shiny)
#Simulation module UI function
simulationUI <- function(id) {
  ns <- NS(id)
  
  tagList(sidebarLayout(
    sidebarPanel(width = 3,
        h3("Parameters"),
        radioButtons(inputId = ns("distType"),
                    label = "Distribution Type",
                    choices =c("Discrete"="disc",
                               "Continuous"="conti"),
                    selected = "disc"
        ),
        uiOutput(ns("disttControls")),
        uiOutput(outputId = ns("originalParamsControls")),
        sliderInput(inputId = ns("numObs"),
                    label = "Number of Observations",
                    max = 1000,min=0,value = 50,step =10 )
      
    ),
    mainPanel(width = 9,
      h3("Simulations"),
      tabsetPanel(
        tabPanel("Linear Congrential",
                 h4("Generating uniform random numbers"),
                 fluidRow(
                   column(3,
                        sliderInput(inputId = ns("initVal"),
                          label = "Initial Value",
                          min = 0,
                          max = 100,
                          value = 57
                        )
                    ),
                   column(3,
                          sliderInput(inputId = ns("multiplierVal"),
                                      label = "Multiplier",
                                      min = 0,
                                      max = 100,
                                      value = 33
                          )      
                    ),
                   column(3,
                          sliderInput(inputId = ns("modVal"),
                                      label = "Modulus",
                                      min = 0,
                                      max = 100,
                                      value = 64
                          )      
                    ),
                   column(3,
                          sliderInput(inputId = ns("shiftVal"),
                                      label = "Shift",
                                      min = 0,
                                      max = 100,
                                      value = 12
                          )        
                  )
                 ),
                 fluidRow(
                   column(6,h4("Histogram"),
                          plotOutput(outputId = ns("randCongregHist"))),
                   column(6,h4("Run Sequence"),
                          plotOutput(outputId = ns("randCongregRunSeq")))
                 ),
                 fluidRow(
                   column(6,h4("Chi-Square Test"),
                          verbatimTextOutput(outputId = ns("randCongregChisq"))),
                   column(6,h4("Kolmogorov Smirnov Test"),
                          verbatimTextOutput(outputId = ns("randCongregKolmog")))
                 ),
                 fluidRow(
                   verbatimTextOutput(outputId = ns("randCongreg"))
                 )
                 
        ),
        tabPanel("Univariate",
                 h3("Univariate Analysis of Simulated data"),
                 fluidRow(
                   uiOutput(outputId = ns("assumedParamsControls"))
                 ),
                 fluidRow(
                   column(6,h4("P-P Plot"),plotOutput(outputId = ns("univarPP"))),
                   column(6,h4("Kolmogorov-Smirnov Goodness of Fit Tests"),
                          h5("Calculated Value"),
                          verbatimTextOutput(outputId = ns("chisq")),
                          h5("KS test"),
                          verbatimTextOutput(outputId = ns("kolmog"))
                    )
                  )
        ),
        tabPanel("Bivariate",
                 h3("Bivariate Analysis of Simulated data")),
        tabPanel("Multivariate",
                 h3("Multivariate Analysis of Simulated data")),
        tabPanel("Applications",
                 h3("Queueing system"))
      )
      
    )
  ))
}
