library(shiny)
library(dplyr)
library(ggplot2)
library(gmodels)
library(ggfortify)
library(RSQLite)
library(MASS)
library(maxLik)
source("datasetsServer.R")

bivariateServer <- function(input, output, session, dataSet) {
  ns <- session$ns
  
  mydata <- reactive(return(loadDataset(dataSet$data_set)))
  #Define colors
  varColors <- c("blue","green","red","yellow","black","pink","orange")
  #Number of Observations
  output$nnNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("nnNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  
  output$cnNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("cnNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  output$ccNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("ccNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  
  #nnMaxObs <- reactive(as.numeric(as.numeric(input$nnNum)))
  #cnMaxObs <- reactive(as.numeric(as.numeric(input$cnNum)))
  #ccMaxObs <- reactive(as.numeric(as.numeric(input$ccNum)))
  
  #Numeric-Numeric Variables
  output$nnContVar1Controls <- renderUI({
    myData <- head(mydata(),as.numeric(input$nnNum))
    contVars <- getContVars(myData)
    selectInput(
      inputId = ns("nnContVar1"),
      label = "Select first numeric variable",
      choices = contVars
    )
  })
  
  output$nnContVar2Controls <- renderUI({
    myData <- head(mydata(),as.numeric(input$nnNum))
    var1 <- input$nnContVar1
    var2 <- getContVars(myData)
    contVars <- subset(var2, var2 != var1)
    selectInput(
      inputId = ns("nnContVar2"),
      label = "Select second numeric variable",
      choices = contVars
    )
  })
  
  #Categorical-Numeric
  output$cnCatVarControls <- renderUI({
    myData <- head(mydata(),as.numeric(input$cnNum))
    catVars <- getCatVars(myData)
    selectInput(
      inputId = ns("cnCatVar"),
      label = "Select a categoric variable",
      choices = catVars
    )
  })
  
  output$cnContVarControls <- renderUI({
    myData <- head(mydata(),as.numeric(input$cnNum))
    contVars <- getContVars(myData)
    selectInput(
      inputId = ns("cnContVar"),
      label = "Select a numeric variable",
      choices = contVars
    )
  })
  
  #Categoric- Categoric
  output$ccCatVar1Controls <- renderUI({
    myData <- head(mydata(),as.numeric(input$ccNum))
    catVars <- getCatVars(myData)
    selectInput(
      inputId = ns("ccCatVar1"),
      label = "Select first categoric variable",
      choices = catVars
    )
  })
  output$ccCatVar2Controls <- renderUI({
    myData <- head(mydata(),as.numeric(input$ccNum))
    var1 <- input$ccCatVar1
    var2 <- getCatVars(myData)
    catVars <- subset(var2, var2 != var1)
    selectInput(
      inputId = ns("ccCatVar2"),
      label = "Select second categoric variable",
      choices = catVars
    )
  })
  
  
  #####   Display Information ######################
  
  ## Numeric - Numeric
  
  #Scatter Plot
  output$scatter <- renderPlot({
    myData <- head(mydata(),as.numeric(input$nnNum))
    nnContVar1 <- input$nnContVar1
    nnContVar2 <- input$nnContVar2
    mdl <- lm(myData[, nnContVar2] ~ myData[, nnContVar1])
    ggplot(myData,aes(myData[, nnContVar1],myData[, nnContVar2])) +
      geom_point(colour = "blue", size = 3)+
      geom_abline(intercept = mdl$coefficients[1],slope = mdl$coefficients[2],color = "red",size=2) + 
      labs(x = nnContVar2,y= nnContVar1, title = paste("Scatter plot of",nnContVar1,"vs",nnContVar2))
   
  })
  
  # Simple Linear Regression
  output$regress <- renderPrint({
    myData <- head(mydata(),as.numeric(input$nnNum))
    nnContVar1 <- input$nnContVar1
    nnContVar2 <- input$nnContVar2
    mdl <- lm(myData[, nnContVar2] ~ myData[, nnContVar1])
    summary(mdl)
  })
  
  # Correlation coefficients
  output$cor <-renderPrint({
    myData <- head(mydata(),as.numeric(input$nnNum))
    nnContVar1 <- input$nnContVar1
    nnContVar2 <- input$nnContVar2
    cor(myData[, nnContVar2],myData[, nnContVar1])
  })
  
  # Check model assumptions
  output$resplot<- renderPlot({
    myData <- head(mydata(),as.numeric(input$nnNum))
    nnContVar1 <- input$nnContVar1
    nnContVar2 <- input$nnContVar2
    mdl<-lm(myData[, nnContVar2] ~ myData[, nnContVar1])
    autoplot(mdl, which = 1:6, label.size = 2,colour = "red")
  }) 
   
  # Levene test for equality of variance
  output$levene <- renderPrint({
    myData <- head(mydata(),as.numeric(input$nnNum))
    nnContVar1 <- input$nnContVar1
    nnContVar2 <- input$nnContVar2
    #levene.test(myData[, nnContVar2] ~ myData[, nnContVar1])
  })
  
  
  ###    Numeric - Categorical ###
  
  #Box plot
  output$boxplot <-
    renderPlot({
      myData <- head(mydata(),as.numeric(input$cnNum))
      cnCatVar <- input$cnCatVar
      cnContVar <- input$cnContVar
      ggplot(myData,aes(myData[, cnCatVar],myData[, cnContVar])) +
        geom_boxplot(fill = "Steel Blue") +
        labs(x = cnCatVar,y=cnContVar,title=paste("Boxplot of ",cnContVar,"by",cnCatVar))
    })
  
  # Density plot
  output$densityplot <-
    renderPlot({
      myData <- head(mydata(),as.numeric(input$cnNum))
      cnCatVar <- input$cnCatVar
      cnContVar <- input$cnContVar
      ggplot(myData,aes(x=myData[, cnContVar],fill=myData[, cnCatVar])) +
        geom_density(col=NA,alpha=0.5) + 
        labs(x = cnCatVar,y=cnContVar,title=paste("Density Plot of ",cnContVar,"by",cnCatVar))
      
    })
  
  
  #Anova (Parametric)
  output$anova <- renderPrint({
    myData <- head(mydata(),as.numeric(input$cnNum))
    cnCatVar <- input$cnCatVar
    cnContVar <- input$cnContVar
    anv <- aov(myData[, cnContVar] ~ myData[, cnCatVar])
    summary(anv)
  })
  
  #Anova (Parametric) Tukey Test
  output$tukey1 <- renderPrint({
    myData <- head(mydata(),as.numeric(input$cnNum))
    cnCatVar <- input$cnCatVar
    cnContVar <- input$cnContVar
    anv <- aov(myData[, cnContVar] ~ myData[, cnCatVar])
    TukeyHSD(anv)
  })
  
  #kruskal (Non-parametric)
  output$krustal <- renderPrint({
    myData <- head(mydata(),as.numeric(input$cnNum))
    cnCatVar <- input$cnCatVar
    cnContVar <- input$cnContVar
    kruskal.test(myData[, cnCatVar] ~ myData[, cnContVar],data = myData)
  })
  #kruskal (Non-parametric) Test
  output$tukey2 <- renderPrint({
    # myData <- head(mydata(),as.numeric(input$cnNum))
    # cnCatVar <- input$cnCatVar
    # cnContVar <- input$cnContVar
    # kru <- kruskal.test(myData[, cnCatVar] ~ myData[, cnContVar],data = myData)
    # TukeyHSD(kru)
  })
  
  
  ###  Categorical - Categorical
  
  #Cross Tabulation
  output$crosstab <- renderPrint({
    myData <- head(mydata(),as.numeric(input$ccNum))
    ccCatVar1 <- input$ccCatVar1
    ccCatVar2 <- input$ccCatVar2
    CrossTable(myData[, ccCatVar1] , myData[, ccCatVar2],dnn = c(ccCatVar1,ccCatVar2),format = "SPSS")
  })
  
  # Barplot
  output$barplot <- renderPlot({
    myData <- head(mydata(),as.numeric(input$ccNum))
    ccCatVar1 <- input$ccCatVar1
    ccCatVar2 <- input$ccCatVar2
    t <- table(myData[, ccCatVar1] , myData[, ccCatVar2])
    barplot(t, beside = TRUE,legend = TRUE )
  })
  
  # chisquare test
  output$chisq <- renderPrint({
    myData <- head(mydata(),as.numeric(input$ccNum))
    ccCatVar1 <- input$ccCatVar1
    ccCatVar2 <- input$ccCatVar2
    t <- table(myData[, ccCatVar1] , myData[, ccCatVar2])
    chisq.test(t,correct = TRUE)
  })
  # chisquare expected values
  output$chisqexpt <- renderPrint({
    myData <- head(mydata(),as.numeric(input$ccNum))
    ccCatVar1 <- input$ccCatVar1
    ccCatVar2 <- input$ccCatVar2
    t <- table(myData[, ccCatVar1] , myData[, ccCatVar2])
    chi<-chisq.test(t,correct = TRUE)
    chi$expected
  })
  # chisquare residuals values
  output$chisqresi <- renderPrint({
    myData <- head(mydata(),as.numeric(input$ccNum))
    ccCatVar1 <- input$ccCatVar1
    ccCatVar2 <- input$ccCatVar2
    t <- table(myData[, ccCatVar1] , myData[, ccCatVar2])
    chi<-chisq.test(t,correct = TRUE)
    chi$residuals
  })
  
  # chisquare residual standard error values
  output$chisqstdres <- renderPrint({
    myData <- head(mydata(),as.numeric(input$ccNum))
    ccCatVar1 <- input$ccCatVar1
    ccCatVar2 <- input$ccCatVar2
    t <- table(myData[, ccCatVar1] , myData[, ccCatVar2])
    chi<-chisq.test(t,correct = TRUE)
    chi$stdres
  })
  
  #Fisher's Exact Test
  output$fisher <- renderPrint({
    myData <- head(mydata(),as.numeric(input$ccNum))
    ccCatVar1 <- input$ccCatVar1
    ccCatVar2 <- input$ccCatVar2
    t <- table(myData[, ccCatVar1] , myData[, ccCatVar2])
    fisher.test(t)
  })
  
  
}