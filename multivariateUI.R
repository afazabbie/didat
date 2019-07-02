library(shiny)
multivariateUI <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    ##################    Regression    #####################
    tabPanel("Regression",
             sidebarLayout(
               sidebarPanel(width = 2,
                 uiOutput(outputId = ns("regDepVarControls")),
                 uiOutput(outputId = ns("regIndVarControls")),
                 checkboxInput(inputId = ns("regType"),
                             label = "With interaction?",
                             value = FALSE
                             ),
                 uiOutput(outputId = ns("regNumControls"))
                 
               ),
               mainPanel(width = 10,
                 fluidRow(
                   column(width = 6,
                          h4("Multiple Linear Regression"),
                          verbatimTextOutput(outputId = ns("multreg"))   
                    ),
                   column(width = 6,
                        h4("Check model assumptions"),
                        plotOutput(outputId = ns("resplot"))
                  )
                 ),
                 fluidRow(
                  column(width = 6,
                         h4("Confidence interval"),
                         verbatimTextOutput(outputId = ns("confint"))     
                  ),
                  column(width = 6,
                         h4("F Test"),
                         uiOutput(outputId = ns("model2VarControls")),
                         verbatimTextOutput(outputId = ns("ftest"))   
                  )
                 )
               )
             )),
    ###################     LDA   #######################
    tabPanel("LDA",
             sidebarLayout(
               sidebarPanel(width = 3,
                  uiOutput(outputId = ns("ldaCatVarControls")),
                 uiOutput(outputId = ns("ldaNumControls"))
                ),
               mainPanel(width = 9,
                 tabsetPanel(
                 tabPanel(
                   "Scatter plot",
                   #input UI
                   fluidRow(column(width = 6,
                                   uiOutput(
                                     outputId = ns("scattContVar1Controls")
                                   )),
                            column(width = 6,
                                   uiOutput(
                                     outputId = ns("scattContVar2Controls")
                                   ))),
                   #output UI
                   fluidRow(h4("Scatter Plot"),
                            plotOutput(outputId = ns("scatmat")))
                 ),
                 tabPanel(
                   "LDA fit",
                   #output UI
                   h4("LDA fit"),
                   verbatimTextOutput(ns("test")),
                   verbatimTextOutput(outputId = ns("fit"))
                 ),
                 tabPanel("LDA Hist",
                          #output UI
                          h4("LDA fit"),
                          fluidRow(
                            column(6,
                                   h3("LDA histogram"),
                                   plotOutput(outputId = ns("ldahist1"))),
                            column(6,
                                   h3("LDA histogram"),
                                   plotOutput(outputId = ns("ldahist2")))
                          )),
                 tabPanel("LDA Summary",
                          #output UI
                          
                          fluidRow(
                            h3("LDA predictions and Summmary"),
                            column(6,
                                   h4("LDA plot"),
                                   plotOutput(outputId = ns("ldaplot"))),
                            column(
                              6,
                              h4("LDA Predictions"),
                              verbatimTextOutput(outputId = ns("premat")),
                              verbatimTextOutput(outputId = ns("pridt"))
                            )
                          ))
               ))
             )),
    
    ###########         PCA     #####################################
    tabPanel("PCA",
             sidebarLayout(
               sidebarPanel(width = 3,
                  uiOutput(outputId = ns("pcaCatVarControls")),
                  uiOutput(outputId = ns("pcaNumControls"))
                ),
               mainPanel(width = 9,
                 fluidRow(
                   fluidRow(h4("PCA plot"),
                            column(width = 4,
                                   uiOutput(outputId = ns("comp1VarControls")),
                                   uiOutput(outputId = ns("comp2VarControls"))
                            ),
                            column(width = 8,
                                   plotOutput(outputId = ns("pcaplot"))
                            )),
                   h4("PCA summary"),
                   verbatimTextOutput(outputId = ns("pcaload")),
                   verbatimTextOutput(outputId = ns("pcasum"))
                 ),
                 fluidRow(h4("PCA Variance plots"),
                          plotOutput(outputId = ns("pcaVarplot")))
               )
             )),
    
    tabPanel("Logistic",
             sidebarLayout(
               sidebarPanel(width = 3,
                 
               uiOutput(outputId = ns(
                 "logisCatVarControls"
               )),
               uiOutput(outputId = ns(
                 "logisVarsControls"
               )),
               uiOutput(outputId = ns(
                 "logisNumControls"
               ))),
               mainPanel(width = 9,
                  fluidRow(h4('Summary'),verbatimTextOutput(outputId = ns("logisSumm"))),
                  fluidRow(h4('Confidence Intervals'),verbatimTextOutput(outputId = ns("logisConfid"))),
                  fluidRow(h4('Odds Ratios'),verbatimTextOutput(outputId = ns("logisRatios")))
               )
             )),
    tabPanel("Cluster",
             sidebarLayout(
               sidebarPanel(width = 3,
                            uiOutput(outputId = ns(
                              "clusterNumControls"
                            )),
                            uiOutput(outputId = ns(
                              "clusterCatVarControls"
                            ))),
               mainPanel(width = 9)
             ))
  ))
}
