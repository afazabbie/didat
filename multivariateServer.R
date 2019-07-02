library(shiny)
library(dplyr)
library(ggplot2)
library(gmodels)
library(ggfortify)
library(RSQLite)
library(MASS)
library(maxLik)
source("datasetsServer.R")
multivariateServer <- function(input, output, session,dataSet) {
  ns <- session$ns
  
  #############     Get the Dataset     ################
  myData <- reactive({
    return(loadDataset(dataSet$data_set))
  })
  
  
  
  ############  Regression inputs ###################
  output$regNumControls <- renderUI({
    numRecords <- nrow(myData())
    sliderInput(
      inputId = ns("regNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  
  output$regDepVarControls <- renderUI({
    contVars <- getContVars(myData())
    selectInput(
      inputId = ns("regDepVar"),
      label = "Select a dependent variable",
      choices = contVars
    )
  })
  
  output$regIndVarControls <- renderUI({
    vars <- colnames(myData())
    regDepVar <- input$regDepVar
    var2Exp <- subset(vars, vars != regDepVar)
    selectInput(
      inputId = ns("regIndVars"),
      label = "Select Independent variables",
      choices = var2Exp,
      multiple = TRUE,
      selected = var2Exp
    )
  })
  
  output$model2VarControls <- renderUI({
    model2Vars <- input$regIndVars
    selectInput(
      inputId = ns("model2Var"),
      label = "Select a variable to exclude in model 2",
      choices = model2Vars
    )
  })

  
  
  ##########        PCA inputs    #####################
  output$pcaNumControls <- renderUI({
    numRecords <- nrow(myData())
    sliderInput(
      inputId = ns("pcaNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  
  output$pcaCatVarControls <- renderUI({
    catVars <- getCatVars(myData())
    selectInput(
      inputId = ns("pcaCatVar"),
      label = "Select a categorical variable",
      choices = catVars
    )
  })

  
  ##############         LDA inputs     ############
  
  output$ldaCatVarControls <- renderUI({
    catVars <- getCatVars(myData())
    selectInput(
      inputId = ns("ldaCatVar"),
      label = "Select a categorical variable",
      choices = catVars
    )
  })
  
  output$ldaNumControls <- renderUI({
    numRecords <- nrow(myData())
    sliderInput(
      inputId = ns("ldaNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
 
  
  output$scattContVar1Controls <- renderUI({
    contVars <- getContVars(myData())
    selectInput(
      inputId = ns("scattContVar1"),
      label = "Select first numeric variable",
      choices = contVars
    )
  })
  output$scattContVar2Controls <- renderUI({
    scatContVar1 <- input$scattContVar1
    contVars <- getContVars(myData())
    contVars <- subset(contVars,contVars != scatContVar1)
    selectInput(
      inputId = ns("scattContVar2"),
      label = "Select second numeric variable",
      choices = contVars
    )
  })
  
  #############Logistic regression inputs################
  output$logisNumControls <- renderUI({
    numRecords <- nrow(myData())
    sliderInput(
      inputId = ns("logisNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  output$logisCatVarControls <- renderUI({
    catVars <- getCatVars2Levels(myData())
    selectInput(
      inputId = ns("logisCatVar"),
      label = "Select a binary outcome",
      choices = catVars
    )
  })
  
  output$logisVarsControls <- renderUI({
    logisContVar <- input$logisCatVar
    vars <- colnames(myData())
    logisVars <- subset(vars,vars != logisContVar)
    selectInput(
      inputId = ns("logisVars"),
      label = "Select independent variables",
      choices = logisVars,
      multiple = TRUE,
      selected = logisVars
    )
  })

  
  ###############         Display Information       #################
  

  #Multiple Linear Regression
  mul <- reactive({
    depVar <- input$regDepVar
    indVars <- input$regIndVars
    regType <- input$regType
    mydata <- head(myData(), as.numeric(input$regNum))
    if (regType) {
      fm <- as.formula(paste(depVar," ~ ", paste(indVars, collapse = "*")))
    } else{
      fm <- as.formula(paste(depVar," ~ ", paste(indVars, collapse = "+")))
    }
    
    mymodel<-lm(formula = fm,data = mydata)
    
    return(mymodel)
  })
  
  #Multiple Regression
  output$multreg <- renderPrint(summary(mul()))
  # Confidence interval
  output$confint <- renderPrint(confint(mul())) 
  # Coefficients
  output$coeff<- renderPrint(coef(mul())) 
  # Check model assumptions
  output$resplot<- renderPlot(
    autoplot(mul(), which = 1:6, label.size = 2,colour = "red")
  ) 
  # F-test
  output$ftest <- renderPrint({
    mdl1 <- mul()
    depVar <- input$regDepVar
    indVars <- input$regIndVars
    regType <- input$regType
    model2Var <- input$model2Var
    model2Vars <- subset(indVars, indVars != model2Var)
    mydata <- head(myData(), as.numeric(input$regNum))
    if (regType) {
      fm <- as.formula(paste(depVar," ~ ", paste(model2Vars, collapse = "*")))
    } else{
      fm <- as.formula(paste(depVar," ~ ", paste(model2Vars, collapse = "+")))
    }
    
    mdl2 <- lm(formula = fm,data = mydata)
    anova(mdl1,mdl2)
  })
  
  
  #################           LDA Output    #################################
  #Scatter plot matrix
  output$scatmat <- renderPlot({
    mydata <- head(myData(), as.numeric(input$ldaNum))
    scattCatVar <- input$ldaCatVar
    scattContVar1 <- input$scattContVar1
    scattContVar2 <- input$scattContVar2
    varColors <- c("blue","green","red","yellow","black","pink","orange")
    ggplot(mydata,aes(mydata[,scattContVar1],mydata[,scattContVar2])) + 
      geom_point(aes(col=mydata[,scattCatVar],cex=0.7)) + 
      scale_color_manual(values = varColors,name = scattCatVar) +
      theme(legend.title = element_text(scattCatVar)) + 
      labs(x = scattContVar1,y= scattContVar2, title = paste("Scater plot of ",scattContVar1,"vs",scattContVar2,"by",scattCatVar))
  })
  
  # Fitting LDA
  output$fit <- renderPrint({
    fitCatVar <- input$ldaCatVar
    mydata <- head(myData(), as.numeric(input$ldaNum))
    fitContVar <- getContVars(mydata)
    lda(mydata[,fitContVar],grouping = mydata[,fitCatVar])
  })

  # LDA histograms
  output$ldahist1 <- renderPlot({
  histCatVar <- input$ldaCatVar
  mydata <- head(myData(), as.numeric(input$ldaNum))
  histContVar <- getContVars(mydata)
  f <- lda(mydata[,histContVar], grouping = mydata[,histCatVar])
  f_values<- predict(f)
  #if(length(unique(mydata[,histCatVar])) <=3 ){
  #  ldahist(data = f_values$x[,1],g = mydata[,histCatVar])
  #}else{
    par(mar=c(1,1,1,1))
    ldahist(data = f_values$x[,1],g = mydata[,histCatVar])
  #}
})
  output$ldahist2 <- renderPlot({
    histCatVar <- input$ldaCatVar
    mydata <- head(myData(), as.numeric(input$ldaNum))
    histContVars <- getContVars(mydata)
    f<-lda(mydata[,histContVars],grouping = mydata[,histCatVar])
    f_values<- predict(f)
    #if(length(names(f_values$x)) >= 2){
      par(mar=c(1,1,1,1))
      ldahist(data = f_values$x[,2],g = mydata[,histCatVar])
    #}
  })

  #LDA plot
  output$ldaplot <- renderPlot({
    mydata <- head(myData(), as.numeric(input$ldaNum))
    sumCatVar <- input$ldaCatVar
    sumContVar <- getContVars(mydata)
    f<-lda(mydata[,sumContVar], grouping = mydata[,sumCatVar])
    f_values<- predict(f)
    x <- data.frame(f_values$x)
    if(length(colnames(x)) >= 2){
      varn <- colnames(x)
      varColors <- c("blue","green","red","yellow","black","pink","orange")
      ggplot(x,aes(x[,1],x[,2])) + 
        geom_point(aes(col=mydata[,sumCatVar],cex=0.7)) + 
        scale_color_manual(values = varColors,name = sumCatVar) +
        theme(legend.title = element_text(sumCatVar)) + 
        labs(x = varn[1],y= varn[2], title = paste("Scater plot of ",varn[1],"vs",varn[2],"by",sumCatVar))
    }
    # plot(f_values$x[,1],f_values$x[,2],g = iris$Species,pch=21,bg= c("red","green","blue")[iris$Species])
    # legend(0.5,1.5,fill = c("red","green","blue"), legend = c("setosa","versicolor","viginica"))
  })

  #LDA predictions and Summmary
  output$pridt <- renderPrint({
    mydata <- head(myData(), as.numeric(input$ldaNum))
    sumCatVar <- input$ldaCatVar
    sumContVar <- getContVars(mydata)
    f<-lda(mydata[,sumContVar],grouping =mydata[,sumCatVar])
    summary(f)
  })
  output$premat <- renderPrint({
    mydata <- head(myData(), as.numeric(input$ldaNum))
    sumCatVar <- input$ldaCatVar
    sumContVars <- getContVars(mydata)
    f<-lda(mydata[,sumContVars],grouping = mydata[,sumCatVar])
    predt <- predict(f,mydata[,sumContVars])$class
    table(predt,mydata[,sumCatVar])
  })
  
  
  
  ########################      PCA         #####################
  pc <- reactive({
    mydata <- head(myData(), as.numeric(input$pcaNum))
    contVars <- getContVars(mydata)
    t<-princomp(mydata[,contVars],scores = T)
    return(t)
  })
  
  #PCA inputs
  output$comp1VarControls <- renderUI({
    pcs <- pc()$scores
    comps <- colnames(pcs)
    selectInput(inputId = ns("comp1Var"),
      label = "First component",
      choices = comps
    )
  })
  output$comp2VarControls <- renderUI({
    pcs <- pc()$scores
    comps <- colnames(pcs)
    comp1Var <- input$comp1Var
    comps2 <- subset(comps, comps != comp1Var)
    selectInput(inputId = ns("comp2Var"),
                label = "Second component",
                choices = comps2
    )
  })
  
  #PCA eigen values/loadings
  output$pcaload <- renderPrint({
        pc()$loadings
    })
  #PCA summary
  output$pcasum <- renderPrint({
    summary(pc())
  })
  
  # PCA plots
  output$pcaplot<- renderPlot({
    catVar <- input$pcaCatVar
    mydata <- head(myData(), as.numeric(input$pcaNum))
    pcs <- pc()$scores
    comp1Var <- input$comp1Var
    comp2Var <- input$comp2Var
    p1 <- round(100 * pc()$sdev[comp1Var] ^ 2 / sum(pc()$sdev ^ 2))
    p2 <- round(100 * pc()$sdev[comp2Var] ^ 2 / sum(pc()$sdev ^ 2))
    c2 <- paste(comp2Var,"(", as.character(p1),"%)")
    varColors <- c("blue","green","red","yellow","black","pink","orange")
    ggplot(pcs,aes(pcs[,comp1Var],pcs[,comp2Var])) + 
      geom_point(aes(col=mydata[,catVar],cex=0.7)) + 
      scale_color_manual(values = varColors,name = catVar) +
      labs(x = paste(comp1Var,"(", as.character(p1),"%)"),
           y= paste(comp2Var,"(", as.character(p2),"%)"), 
           title = paste("Scater plot of ",comp1Var,"vs",comp2Var,"by",catVar))
  }) 
  
  #PCA Variance plots
  output$pcaVarplot<- renderPlot({
    plot(pc(), main = "PCA Variance Plot")
  })
  
  ##########      Logistic Regression   ###########
  logisMdl <- reactive({
    logisCatVar <- input$logisCatVar
    logisVars <- input$logisVars
    #mydata <- head(myData(),input$logisNum)
    logisfm <- as.formula(paste(logisCatVar," ~ ", paste(logisVars, collapse = "+")))
    mydata <- myData()
    mydata[,logisCatVar] <- as.factor(mydata[,logisCatVar])
    return(glm(formula = logisfm, data = mydata, family = "binomial"))
  })
  # Logistic Summary
  output$logisSumm <- renderPrint(summary(logisMdl()))
  # Logistic Confidence Intervals
  output$logisConfid <- renderPrint(confint(logisMdl()))
  # Logistic Odds ratio
  output$logisRatios <- renderPrint(exp(coef(logisMdl())))
  
}





# Function to plot the princple components
PCAplot <- function(mydata, catVar, i, j) {
  #
  contVars <- getContVars(mydata)
  len <- length(unique(mydata[, catVar]))
  len
  ipc <- princomp(mydata[contVars], scores = TRUE)
  
  # Pricipal component estimates
  pPC <- ipc$scores
  # Rename columns to include the % of variance
  colnames(pPC) <-
    paste(colnames(pPC),
          
          "(", as.character(round(100 * ipc$sdev ^ 2 / sum(ipc$sdev ^ 2))),
          "%)")
  # Plot i against j Principal component
  plot(
    pPC[, c(i, j)],
    main = paste("PC", i, " and PC", j),
    pch = 21,
    col = mydata[, catVar]
  )
}


