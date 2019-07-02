library(shiny)
library(dplyr)
library(ggplot2)
library(gmodels)
library(ggfortify)
library(RSQLite)
library(MASS)
library(maxLik)
#Load Modules
source("dashboardServer.R")
source("datasetsServer.R")
source("univariateServer.R")
source("bivariateServer.R")
source("multivariateServer.R")
source("simulationServer.R")
#Entry point
server <- function(input,output,session){
   callModule(datasetsServer,"dataSets")
   #Get Default Active dataset
   dataSet <- callModule(datasetSetup,"dataSets")
   #callModule(dashboardServer,"dashboard",dataSet)
   callModule(univariateServer,"univariate",dataSet)
   callModule(bivariateServer,"bivariate",dataSet)
   callModule(multivariateServer,"multivariate",dataSet)
   callModule(simulationServer,"simulations")
   #callModule(datasetsServer,"dataSets",dataSet)
}