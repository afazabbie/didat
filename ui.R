library(shiny)
library(shinythemes)

source("univariateUI.R")
source("bivariateUI.R")
source("multivariateUI.R")
source("dashboardUI.R")
source("datasetsUI.R")
source("simulationUI.R")

ui <- fluidPage(
  h1("Interactive and Dynamic Exploratory Data Analysis Tool (IDEDAT)"),
  theme = shinytheme("cerulean"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  navbarPage(
    "IDEDAT",
    #Dashboard (Default Home screen)
    # tabPanel("Dashboard",
    #          dashboardUI("dashboard")),
    #Datasets management
    tabPanel("Datasets",
             datasetsUI("dataSets")),
    
    #Univariate Analysis
    tabPanel("Univariate",
             univariateUI("univariate")),
    
    #Bivariate Analysis
    tabPanel("Bivariate",
             bivariateUI("bivariate")),
    
    #Multivariate Analysis
    tabPanel("Multivariate",
             multivariateUI("multivariate")),
    #Simulation
    tabPanel("Simulations",
             simulationUI("simulations"))
  )#,
  #shinythemes::themeSelector()
)
