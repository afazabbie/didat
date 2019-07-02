library(shiny)
library(dplyr)
library(ggplot2)
library(gmodels)
library(ggfortify)
library(RSQLite)
library(MASS)
library(maxLik)
source("datasetsServer.R")
#Simulation module server function
simulationServer <- function(input, output, session) {
  ns <- session$ns
 output$disttControls <- renderUI({
   distType <- input$distType
   if(distType == "disc"){
     choices <- c("Uniform"="drunif",
                  "Poisson"="rpois")
   }else{
     
     choices <- c("Normal"="rnorm",
                  "Uniform"="crunif",
                  "Exponential"="rexp",
                  "Weibull"="rweibull")
   }
     selectInput(inputId = ns("distt"),
                 label = "Distribution",
                 choices = choices
     )
   
 })
 
 
 #Original Parameters
 output$originalParamsControls <- renderUI({
   distt <- input$distt
   if(distt == "rnorm") {
     tagList(
       sliderInput(inputId = ns("rnormMean"),label = "Mean",min = -100,max = 100,value = 0),
       sliderInput(inputId = ns("rnormStdev"),label = "Standard Deviation",min = 0,max = 100,value = 1)
     )
   }else if(distt == "rpois" ||distt == "rexp"){
     sliderInput(inputId = ns("rpoisParam"), label = "Lambda", min = 1, 
                 max = 100,value = 1)
   }else if(distt == "drunif" || distt == "crunif"){
     sliderInput(inputId = ns("runifParam"), label = "Minimum and Maximum", min = -100, 
                 max = 100, value = c(10, 50))
   }else if(distt == "rweibull"){
     tagList(
       sliderInput(inputId = ns("rweibScale"), 
                            label = "Scale", 
                            min = 1, 
                            max = 100, value = 0),
       sliderInput(inputId = ns("rweibShape"), 
                            label = "Shape", 
                            min = 10, 
                            max = 100,value = 10)
     )
   }
 })
 
 
 #Assumed parameters
 output$assumedParamsControls <- renderUI({
   distt <- input$distt
   if(distt == "rnorm") {
     tagList(
       column(6,sliderInput(inputId = ns("rnormMeanAss"),label = "Assumed Mean",min = -100,max = 100,value = 0)),
       column(6,sliderInput(inputId = ns("rnormStdevAss"),label = "Assumed Standard Deviation",min = 0,max = 100,value = 1))
     )
   }else if(distt == "rpois" ||distt == "rexp"){
     sliderInput(inputId = ns("rpoisParamAss"), label = "Assumed Lambda", min = 1, 
                 max = 100,value = 1)
   }else if(distt == "drunif" || distt == "crunif"){
     sliderInput(inputId = ns("runifParamAss"), label = "Assumed Minimum and Maximum", min = -100, 
                 max = 100, value = c(10, 50))
   }else if(distt == "rweibull"){
     tagList(
       column(6,sliderInput(inputId = ns("rweibScaleAss"), 
                            label = "Assumed Scale", 
                            min = 1, 
                            max = 100, value = 0)),
       column(6,sliderInput(inputId = ns("rweibShapeAss"), 
                            label = "Assumed Shape", 
                            min = 10, 
                            max = 100,value = 10))
     )
   }
 })
 
 
 
 ############     Linear Congruentional      ################
 randNums <- reactive({
   x <- numeric() #intialise vector
   x[1] <- input$initVal #intial value
   m <- input$modVal #modulus
   a <- input$multiplierVal #multiplier
   b <- input$shiftVal #shift
   k <- input$numObs #length of random variables
   # for loop to capture random numbers
   for(i in 2:k){
     x[i] <- ((a * x[i-1]) + b) %% m
   }
   #x #print random numbers
   u <- (x+0.5)/m #create uniform random numbers
   return(u)
 })
 
 #Print random numbers
 output$randCongreg <- renderPrint(randNums())
 #Histogram
 output$randCongregHist <- renderPlot({
   qplot(randNums(),geom="histogram")
})
 output$randCongregRunSeq <- renderPlot({
   k <- input$numObs #length of random variables
   #d<-data.frame(randNums()[1:k-1],randNums()[2:k])
   plot(randNums()[1:k-1],randNums()[2:k])
   #ggplot(d,aes(randNums()[1:k-1],randNums()[2:k]))+geom_point()#plot of ui-1 againt ui
  })
 output$randCongregChisq <- renderPrint({
   k <- input$numObs #length of random variables
   chisq.test(randNums(),runif(k))#chisquare test
  })
 output$randCongregKolmog <- renderPrint({
   k <- input$numObs #length of random variables
   ks.test(randNums(),runif(k))#kolmogrov sminov test
   
  })
 
 
 ###############         Univariate Simmulation       #############
 univData <- reactive({
   distt <- input$distt
   numObs <- input$numObs
   if(distt == "rpois"){
     rpoisParam <- input$rpoisParam
     rpoisParamAss <- input$rpoisParamAss
     original <- data.frame(table(sort(rpois(numObs,rpoisParam))))
     emperical <- cumsum(original[,2])/numObs
     theoretical <- ppois(as.numeric(levels(original[,1])),rpoisParamAss)
     return(data.frame(emperical,theoretical))
   }else if(distt == "drunif"){
     runifParam <- input$runifParam
     runifParamAss <- input$runifParamAss
     min <- min(runifParam)
     max <- max(runifParam)
     minAss <- min(runifParamAss)
     maxAss <- max(runifParamAss)
     original <- data.frame(table(sort(round((max - min) * runif(numObs)) + min)))
     emperical <- cumsum(original[,2])/numObs
     theoretical <- punif(as.numeric(levels(original[,1])),min = minAss,max = maxAss)
     return(data.frame(emperical,theoretical))
   }else if(distt == "crunif"){
     runifParam <- input$runifParam
     runifParamAss <- input$runifParamAss
     min <- min(runifParam)
     max <- max(runifParam)
     minAss <- min(runifParamAss)
     maxAss <- max(runifParamAss)
     x <- ((max - min) * runif(numObs)) + min
     original <- data.frame(table(sort(x)))
     emperical <- cumsum(original[,2])/numObs
     theoretical <- punif(as.numeric(levels(original[,1])),min=minAss,max=maxAss)
     return(data.frame(emperical,theoretical))
   }else if(distt == "rnorm"){
     mean <- input$rnormMean
     stdev <- input$rnormStdev
     meanAss <- input$rnormMeanAss
     stdevAss <- input$rnormStdevAss
     x <- rnorm(numObs,mean,stdev)
     original <- data.frame(table(sort(x)))
     emperical <- cumsum(original[,2])/numObs
     theoretical <- pnorm(as.numeric(levels(original[,1])),meanAss,stdevAss)
     return(data.frame(x,emperical,theoretical))
   }else if(distt == "rexp"){
     rexpParam <- input$rpoisParam
     rexpParamAss <- input$rpoisParamAss
     x <- (-rexpParam) * log(runif(numObs))
     original <- data.frame(table(sort(x)))
     emperical <- cumsum(original[,2])/numObs
     theoretical <- pexp(as.numeric(levels(original[,1])),rexpParamAss)
     return(data.frame(x,emperical,theoretical))
   }else if(distt == "rweibull"){
     rweibScale <- input$rweibScale
     rweibShape <- input$rweibShape
     rweibScaleAss <- input$rweibScaleAss
     rweibShapeAss <- input$rweibShapeAss
     x <- rweibull(numObs,rweibShape,rweibScale)
     original <- data.frame(table(sort(x)))
     emperical <- cumsum(original[,2])/numObs
     theoretical <-pweibull(as.numeric(levels(original[,1])),rweibShapeAss,rweibScaleAss) 
     return(data.frame(x,emperical,theoretical))
   }
 })
 
 ## Get Distribution Parameters
 disttParams <- reactive({
   distt <- input$distt
   if(distt == "rpois"){
     df <- fitdistr(univData()$x,"poisson")
     return(as.list(df$estimate))
   }else if(distt == "drunif" || distt == "crunif"){
     min <- min(runifParam)
     max <- max(runifParam)
     mu <- (min + max)/2
     stdde <- sqrt(((max - min)^2)/12)
     return(as.list(mu,stdde))
   }else if(distt == "rnorm"){
     df <- fitdistr(univData()$x,"normal")
     return(as.list(df$estimate))
   }else if(distt == "rexp"){
     df <- fitdistr(univData()$x,"exponential")
     return(as.list(df$estimate))
   }else if(distt == "rweibull"){
     df <- fitdistr(univData()$x,"weibull")
     return(as.list(df$estimate))
   }
 })
 
 
 
 ########         Display Outputs          ###########
 output$univarPP <- renderPlot({
   qplot(univData()$emperical,univData()$theoretical,geom = "point") +
     geom_abline(dparams = disttParams(),col="red",size=1)
 })
 
 output$chisq <- renderPrint({
  #max(abs((univData()$emperical - univData()$theoretical)))
   #disttParams()[1]
 })
 
 output$kolmog <- renderPrint({
   ks.test(univData()$emperical,univData()$theoretical)
 })
 
 
 ##########             Bivariate Simulation      #############
 
 
 
 
 
 
}

