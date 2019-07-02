library(shiny)
#Univariate module UI function
univariateUI <- function(id) {
  ns <- NS(id)
  
  tagList(tabsetPanel(
    tabPanel(
      "Categorical",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput(outputId =  ns("ctCatVarControls")),
          uiOutput(outputId =  ns("ctNumControls"))
        ),
        mainPanel(width = 9,
          fluidRow(
            column(width = 8,
                   h4("Bar plot"),
                   plotOutput(outputId = ns("ctBarPlot"))
            ),
            column(width = 4,
                   h4("Frequency Distribution table"),
                   verbatimTextOutput(outputId = ns("freqtab"))       
            )
          )
        )
      )
      
    ),
    tabPanel(
      "Measures",
      sidebarLayout(
        sidebarPanel(width = 3,
                     uiOutput(outputId =  ns("csVarControls")),
                     uiOutput(outputId =  ns("csNumControls"))),
        mainPanel(
          width = 9,
          fluidRow(
            column(
              6,
              h4("Summary Statistics"),
              verbatimTextOutput(outputId = ns("summ"))
            ),
            column(6, h4("Mode"),
                   verbatimTextOutput(outputId = ns("mode")))
          ),
          fluidRow(
            column(
              4,
              h4("Standard Deviation"),
              verbatimTextOutput(outputId = ns("sd"))
            ),
            column(4,
                   h4("Mid Range"),
                   verbatimTextOutput(outputId = ns("midR"))),
            column(
              4,
              h4("Inter Quantile Range"),
              verbatimTextOutput(outputId = ns("iqr"))
            )
          ),
          fluidRow(
            column(
              4,
              h4("Skeweness"),
              verbatimTextOutput(outputId = ns("skew"))
            ),
            column(4,
                   h4("Kurtosis"),
                   verbatimTextOutput(outputId = ns("kurt")))
          )
        )
      )
    ),
    tabPanel("Box & Violin Plot",
             sidebarLayout(
               sidebarPanel(width = 3,
                            uiOutput(outputId =  ns("bvVarControls")),
                            uiOutput(outputId =  ns("bvNumControls"))),
               mainPanel(width = 9,
                         fluidRow(
                           column(6,
                                  fluidRow(
                                    h4("Quantiles"),
                                    verbatimTextOutput(outputId = ns("quan")),
                                    h4("Box plot"),
                                    plotOutput(outputId = ns("boxPlot"))
                                  )),
                           column(6,
                                  h3("Violin plot"),
                                  plotOutput(outputId = ns("violin")))
                         ))
             )),
    tabPanel(
      "Fixed Distribution",
      
      sidebarLayout(
        sidebarPanel(width = 3,
                     uiOutput(outputId =  ns("fdVarControls")),
                     uiOutput(outputId =  ns("fdNumControls"))),
        mainPanel(width = 9,
                  fluidRow(
                    column(6,
                           h3("Histogram"),
                           plotOutput(outputId = ns("histogram"))),
                    column(6,
                           h3("Q-Q Plot"),
                           plotOutput(outputId = ns("qqplot")))
                  ))
      )
    ),
    tabPanel(
      "Lag & Run Sequence Plots",
      
      sidebarLayout(
        sidebarPanel(width = 3,
                     uiOutput(outputId =  ns("lrsVarControls")),
                     uiOutput(outputId =  ns("lrsNumControls"))),
        mainPanel(width = 9,
                  fluidRow(
                    column(
                      6,
                      h3("Lag plot for Randomness test"),
                      plotOutput(outputId = ns("randomness"))
                    ),
                    column(
                      6,
                      h3("Run sequence plot for Fixed Location and Variation test"),
                      plotOutput(outputId = ns("fixedlocvar"))
                    )
                  ))
      )
    ),
    tabPanel(
      "Fitting & Goodness Test",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput(outputId =  ns("fgVarControls")),
          uiOutput(outputId =  ns("fgNumControls")),
          selectInput(
            inputId =  ns("distVar"),
            label = "Distribution type:",
            choices =  c(
              "Normal" = "norm",
              "Cauchy" = "cauchy",
              "Logistic" = "logis",
              "Exponential" = "exp",
              "Weibull" = "weib",
              "Uniform" = "unif"
            )
          )
        ),
        mainPanel(
          width = 9,
          fluidRow(
            h3("Fitting Distributions"),
            column(
              6,
              h4("Method of Parameter Estimation"),
              h5("Method of Moments"),
              verbatimTextOutput(outputId = ns("fitting")),
              h5("Method of Maximum Likelihood"),
              verbatimTextOutput(outputId = ns("maxlik"))
            ),
            column(6, h4("Curve Fitting"),
                   plotOutput(outputId = ns("curveFitting")))
          ),
          
          fluidRow(
            h3("Goodness of Fit Test"),
            column(
              6,
              h4("Kolmogorov-Smirnov Test"),
              verbatimTextOutput(outputId = ns("kstest"))
            ),
            column(6,
                   h4("Chi Square test"),
                   verbatimTextOutput(outputId = ns("chisq")))
          )
        )
      )
    )
  ))
}
