library(shiny)
bivariateUI <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel(
      "Numeric-Numeric",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput(outputId = ns("nnNumControls")),
          uiOutput(outputId = ns("nnContVar1Controls")),
          uiOutput(outputId = ns("nnContVar2Controls"))
        ),
        mainPanel(width = 10,
                  tabsetPanel(
                    tabPanel("Scatter plot",
                             fluidRow(
                               column(7,
                                      h4("Scatter plot"),
                                      plotOutput(outputId = ns("scatter"))),
                               column(
                                 5,
                                 h4("Correlation coefficient"),
                                 verbatimTextOutput(outputId = ns("cor"))
                               )
                             )),
                    tabPanel("Linear Regression",
                             fluidRow(
                               column(
                                 6,
                                 h4("Linear Regression"),
                                 verbatimTextOutput(outputId = ns("regress"))
                               ),
                               column(6,
                                      h4("Model Assumptions"),
                                      plotOutput(outputId = ns("resplot")))
                             ))
                  ))
      )
    ),
    tabPanel(
      "Categorical-Numeric",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput(outputId = ns("cnNumControls")),
          uiOutput(outputId = ns("cnCatVarControls")),
          uiOutput(outputId = ns("cnContVarControls"))
        ),
        mainPanel(
          width = 10,
          tabsetPanel(
            tabPanel("Boxplot",
                     fluidRow(
                       column(7, h4("Boxplot"),
                              plotOutput(outputId = ns("boxplot"))),
                       column(5,
                              h4("Unpaired t test"),
                              h4("Paired t test"))
                     )),
            tabPanel("Density Plot",
                     plotOutput(outputId = ns("densityplot"))),
            tabPanel(
              "ANOVA",
              h4("Anova (Parametric)"),
              verbatimTextOutput(outputId = ns("anova")),
              h4("Anova (Parametric) Tukey Test"),
              verbatimTextOutput(outputId = ns("tukey1")),
              h4("Krustal Non-Parametric"),
              verbatimTextOutput(outputId = ns("krustal")),
              h4("Krustal (Non-Parametric) Tukey Test"),
              verbatimTextOutput(outputId = ns("tukey2"))
            ),
            tabPanel(
              "Equality of Variance",
              h4("Levene's Test"),
              verbatimTextOutput(outputId = ns("levene"))
            )
          )
        )
      )
    ),
    tabPanel(
      "Categorical-Categorical",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput(outputId = ns("ccNumControls")),
          uiOutput(outputId = ns("ccCatVar1Controls")),
          uiOutput(outputId = ns("ccCatVar2Controls"))
        ),
        mainPanel(
          width = 10,
          tabsetPanel(
            tabPanel("Cross Tabulation",
                     fluidRow(
                       h4("Cross Tabulation"),
                       verbatimTextOutput(outputId = ns("crosstab"))
                     )),
            tabPanel("Bar Plot",
                     fluidRow(
                       h4("Bar Plot"),
                       plotOutput(outputId = ns("barplot"))
                     )),
            tabPanel(
              "Chi Square Test",
              fluidRow(
                h5("Chi Square Test"),
                verbatimTextOutput(outputId = ns("chisq")),
                h5("Expected"),
                verbatimTextOutput(outputId = ns("chisqexpt")),
                h5("Residuals"),
                verbatimTextOutput(outputId = ns("chisqresi")),
                h5("Standardized Residuals"),
                verbatimTextOutput(outputId = ns("chisqstdres"))
              )
            ),
            tabPanel("Fishers Exact Test",
                     fluidRow(
                       h4("Fishers Exact Test"),
                       verbatimTextOutput(outputId = ns("fisher"))
                     ))
          )
        )
      )
    )
  ))
}