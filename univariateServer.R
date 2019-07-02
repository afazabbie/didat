library(shiny)
library(dplyr)
library(ggplot2)
library(gmodels)
library(ggfortify)
library(RSQLite)
library(MASS)
library(maxLik)
source("datasetsServer.R")
#Univariate module server function
univariateServer <- function(input, output, session,dataSet) {
  ns <- session$ns
  
  # Get the Dataset
  mydata <- reactive({
    return (loadDataset(dataSet$data_set))
  })
  
  # Number of Observations
  output$ctNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("ctNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  
  # Number of Observations
  output$csNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("csNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  
  # Number of Observations
  output$bvNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("bvNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  # Number of Observations
  output$fdNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("fdNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  
  # Number of Observations
  output$lrsNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("lrsNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  # Number of Observations
  output$fgNumControls <- renderUI({
    numRecords <- nrow(mydata())
    sliderInput(
      inputId = ns("fgNum"),
      label = "Number of Observations",
      min = 0,
      max = numRecords,
      value = numRecords,
      step = 10
    )
  })
  # Variable
  output$ctCatVarControls <- renderUI({
    catVars <- getCatVars(mydata())
    selectInput(
      inputId = ns("ctCatVar"),
      label = "Select a categorical variable",
      choices = catVars
    )
  })
  # Variable
  output$csVarControls <- renderUI({
    contVars <- getContVars(mydata())
    selectInput(
      inputId = ns("csVar"),
      label = "Select variable",
      choices = contVars
    )
  })
  
  # Variable
  output$bvVarControls <- renderUI({
    contVars <- getContVars(mydata())
    selectInput(
      inputId = ns("bvVar"),
      label = "Select variable",
      choices = contVars
    )
  })
  
  # Variable
  output$fdVarControls <- renderUI({
    contVars <- getContVars(mydata())
    selectInput(
      inputId = ns("fdVar"),
      label = "Select variable",
      choices = contVars
    )
  })
  
  # Variable
  output$lrsVarControls <- renderUI({
    contVars <- getContVars(mydata())
    selectInput(
      inputId = ns("lrsVar"),
      label = "Select variable",
      choices = contVars
    )
  })
  # Variable
  output$fgVarControls <- renderUI({
    contVars <- getContVars(mydata())
    selectInput(
      inputId = ns("fgVar"),
      label = "Select variable",
      choices = contVars
    )
  })
  
  ########### Categorical       ##################
  # Bar Plot
  output$ctBarPlot <- renderPlot({
    ctCatVar <- input$ctCatVar
    d <- head(mydata(),as.numeric(input$ctNum))
    barplot(table(d[,ctCatVar]),col="red")
  })
  
  #Frequency table
  output$freqtab <- renderPrint({
    ctCatVar <- input$ctCatVar
    d <- head(mydata(),as.numeric(input$ctNum))
    data.frame(table(d[,ctCatVar]))
  })
  
  #Display Information
 
  
  
  
  #########  Centrality and Spread    ###############
  # Get the Dataset
  csmyData <- reactive({
    csMaxObs <- as.numeric(input$csNum)
    d <- loadDataset(dataSet$data_set)
    return (head(d,csMaxObs))
  })
  #Summary data
  output$summ <- renderPrint({
    csVar <- input$csVar
    summary(csmyData()[, csVar])
    
  })
  # Standard deviation
  output$sd <- renderPrint({
    csVar <- input$csVar
    sd(csmyData()[, csVar])
    
  })
  #Mode
  output$mode <- renderPrint({
    csVar <- input$csVar
    t<-data.frame(table(csmyData()[, csVar]))
    t[which(t[,2] == max(t[,2])),1]
  })
  # Mid-Range
  output$midR <- renderPrint({
    csVar <- input$csVar
    (max(csmyData()[, csVar])+min(csmyData()[, csVar]))/2
  })
  # Inter-quartile Range
  output$iqr <- renderPrint({
    csVar <- input$csVar
    IQR(csmyData()[, csVar])
  })
  
  # Skeweness
  output$skew <- renderPrint({
    csVar <- input$csVar
    skewness(csmyData()[, csVar])
  })
  
  # Kurtosis
  output$kurt <- renderPrint({
    csVar <- input$csVar
    kurtosis(csmyData()[, csVar])
  })
  
  ######    Boxplot and Violin Plot                ####
  
  # Get the Dataset
  bvmyData <- reactive({
    bvMaxObs <- as.numeric(input$bvNum)
    d <- loadDataset(dataSet$data_set)
    return (head(d,bvMaxObs))
  })
  
  #Quantile
  output$quan <- renderPrint({
    bvVar <- input$bvVar
    quantile(bvmyData()[, bvVar])
  })
  #Violin Plot
  output$violin <- renderPlot({
    bvVar <- input$bvVar
    qplot(1,bvmyData()[, bvVar], geom="violin",fill="violet",main = bvVar,ylab = bvVar)
  })
  #Box Plot
  output$boxPlot <- renderPlot({
    bvVar <- input$bvVar
    qplot(1,bvmyData()[, bvVar], geom="boxplot",fill="violet",main = bvVar,ylab = bvVar)
    
  })
  
  
  ### Fixed Distributions #######
  
  # Get the Dataset
  fdmyData <- reactive({
    fdMaxObs <- as.numeric(input$fdNum)
    d <- loadDataset(dataSet$data_set)
    return (head(d,fdMaxObs))
  })
  #Histogram
  output$histogram <- renderPlot({
    fdVar <- input$fdVar
    qplot(fdmyData()[, fdVar],main = paste("Histogram of ", fdVar),xlab = fdVar,geom = "histogram",fill="red")
    #hist(fdmyData()[, fdVar],main = paste("Histogram of ", fdVar),xlab = fdVar)
  })
  
  #Q-Q Plot
  output$qqplot <- renderPlot({
    fdVar <- input$fdVar
    ggplot(fdmyData(), aes(sample = fdmyData()[, fdVar])) + 
      stat_qq() + 
      geom_abline(intercept = mean(fdmyData()[, fdVar]), slope = sd(fdmyData()[, fdVar]),col="red",size=1) 
  })
  
  
  ######## Lag and Run Sequence Plots  ##########
  
  # Get the Dataset
  lrsmyData <- reactive({
    lrsMaxObs <- as.numeric(input$lrsNum)
    d <- loadDataset(dataSet$data_set)
    return (head(d,lrsMaxObs))
  })
  
  # Randomness Plot
  output$randomness <- renderPlot({
      lrsVar <- input$lrsVar
      d <- lrsmyData()[, lrsVar]
      len <- length(d)
      d2<-data.frame(d[1:len - 1],d[2:len])
      ggplot(d2,aes( d[1:len - 1],d[2:len])) +
        geom_point(pch = "x",size=3,col="red") +
        labs(title = paste("Lag plot of", lrsVar),
             x = paste(lrsVar, "[", 1, ":", len - 1, "]"),
             y = paste(lrsVar, "[", 2, ":", len, "]"))
    
  })
  
  #Fixed Location and Variation Plot
  output$fixedlocvar <- renderPlot({
      lrsVar <- input$lrsVar
      len <- length(lrsmyData()[, lrsVar])
      i <- seq(1, len, 1)
      d <- data.frame(i,lrsmyData()[, lrsVar])
      ggplot(d,aes(i,lrsmyData()[, lrsVar])) +
        geom_line(col="red") +
        labs(title = paste("Run sequence plot of", lrsVar),x = "Lag",y = lrsVar)
  })
  
  
  ##### Fitting and Goodness of Fit Test  ##########
  
  # Get the Dataset
  fgmyData <- reactive({
    fgMaxObs <- as.numeric(input$fgNum)
    d <- loadDataset(dataSet$data_set)
    return (head(d,fgMaxObs))
  })
  
  # Fitted curve using moments
  output$fitting <- renderPrint({
    fgVar <- input$fgVar
    distVar <- input$distVar
    
    if (distVar == "norm") {
      fitdistr(fgmyData()[, fgVar], "normal")
    } else if (distVar == "cauchy") {
      fitdistr(fgmyData()[, fgVar], "cauchy")
    }else if (distVar == "exp") {
      fitdistr(fgmyData()[, fgVar], "exponential")
    }else if (distVar == "logis") {
      fitdistr(fgmyData()[, fgVar], "logistic")
    }else if (distVar == "weib") {
      fitdistr(fgmyData()[, fgVar], "weibull")
    }else if(distVar == "unif"){
      max = max(fgmyData()[, fgVar])
      min = min(fgmyData()[, fgVar])
      mean <- (min + max)/2 
      stdev <- sqrt(((max - min)^2)/12)
      return(data.frame(mean,stdev))
    }
  })
  
  # Maximum likelihood
  output$maxlik <- renderPrint({
    fgVar <- input$fgVar
    distVar <- input$distVar
    mleMaxLik(fgmyData()[, fgVar],distVar)
  })
  
  
  # Fitted curve using maximum likelihood
  output$curveFitting <- renderPlot({
    fgVar <- input$fgVar
    distVar <- input$distVar

    if (distVar == "norm") {
      fd <- fitdistr(fgmyData()[,fgVar],"normal")
      mu<-fd$estimate[1]
      sigma <-fd$estimate[2]
      fitCurve(fgmyData()[, fgVar], dnorm,xbar = mu, std = sigma, xlab = fgVar)
    } else if (distVar == "cauchy") {
      fd <- fitdistr(fgmyData()[,fgVar],"cauchy")
      mu<-fd$estimate[1]
      sigma <-fd$estimate[2]
      fitCurve(fgmyData()[, fgVar], dcauchy,xbar = mu, std = sigma, xlab = fgVar)
    } else if (distVar == "weib") {
      fd <- fitdistr(fgmyData()[,fgVar],"weibull")
      mu<-fd$estimate[1]
      fitCurve(fgmyData()[, fgVar], dweibull,xbar = mu,std = mu, xlab = fgVar)
    } else if (distVar == "logis") {
      fd <- fitdistr(fgmyData()[,fgVar],"logistic")
      mu<-fd$estimate[1]
      sigma <-fd$estimate[2]
      fitCurve(fgmyData()[, fgVar], dlogis,xbar = mu,std = sigma ,xlab = fgVar)
    } else if (distVar == "exp") {
      fd <- fitdistr(fgmyData()[,fgVar],"exponential")
      rate<- fd$estimate[1]
      fitCurve(fgmyData()[, fgVar], dexp,xbar = rate,std = rate^2 ,xlab = fgVar)
    }else if(distVar == "unif"){
      max = max(fgmyData()[, fgVar])
      min = min(fgmyData()[, fgVar])
      mu <- (min + max)/2 
      sigma <- sqrt(((max - min)^2)/12)
      fitCurve(fgmyData()[, fgVar], dunif,xbar = mu,std = sigma ,xlab = fgVar)
    }
  })
  
  #Display goodness of fit test
  output$kstest <- renderPrint({
    fgVar <- input$fgVar
    distVar <- input$distVar
    ksTest(fgmyData()[,fgVar],distVar)
  })
  output$chisq <- renderPrint({
    fgVar <- input$fgVar
    distVar <- input$distVar
    chisqTest(fgmyData()[,fgVar],distVar)
  })
}

#Function to fit curves
fitCurve <- function(mydata,ddistr,xbar,std,xlab="Label"){
  hist(mydata,probability = TRUE,
       main = paste("Histogram of",xlab),
       xlab = xlab,
       xlim = c((min(mydata)-1),(max(mydata)+1)),
       ylim = NULL)
  d <- function(mydata){
    return(ddistr(mydata,xbar,std))
  }
  curve(d,add = TRUE)
}

#Testing fitCurve
#fitCurve(iris$Sepal.Length,dnorm,xlab = "SepalLength")


mleMaxLik <- function(mydata,distt){
  loglik <- function(param) {
    mu <- param[1]
    sigma <- param[2]
    if (distt == "norm") {
      return(sum(dnorm(mydata, mean=mu, sd=sigma, log=TRUE)))
    } else if (distt == "cauchy") {
      return(sum(dcauchy(mydata, location = mu, scale = sigma, log=TRUE)))
    }else if (distt == "logis") {
      return(sum(dlogis(mydata, location = mu, scale = sigma, log=TRUE)))
    }else if (distt == "weib") {
      return(sum(dweibull(mydata, shape = mu, scale = sigma, log=TRUE)))
    }else if (distt == "exp") {
      return(sum(dexp(mydata, rate = mu, log=TRUE)))
    }else if(distt == "unif"){
      return(sum(dunif(mydata, min = mu, max = sigma, log=TRUE)))
    }
  }
  if(distt == "exp"){
    return(summary(maxLik(logLik = loglik,start = 1)))
  }
  return(summary(maxLik(logLik = loglik,start = c(1,3))))
}



ksTest  <- function(mydata,distt) {
  ds <- sort(mydata)
  if (distt == "norm") {
    fd <- fitdistr(mydata,"normal")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    ks.test(mydata,pnorm(ds,mean = mu,sd=sigma))
  } else if (distt == "cauchy") {
    fd <- fitdistr(mydata,"cauchy")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    ks.test(mydata,pcauchy(ds,location = mu,scale = sigma))
  }else if (distt == "unif") {
    fd <- fitdistr(mydata,"uniform")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    ks.test(mydata,punif(ds,min = mu,max = sigma))
  }else if (distt == "lnorm") {
    fd <- fitdistr(mydata,"lognormal")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    ks.test(mydata,plnorm(ds,meanlog = mu,sdlog = sigma))
  }else if (distt == "exp") {
    fd <- fitdistr(mydata,"exponential")
    mu<-fd$estimate[1]
    ks.test(mydata,pexp(ds,rate = mu))
  }
}

chisqTest  <- function(mydata,distt,df = 10,alpha = 0.05) {
  c <- (max(mydata) - min(mydata)) / df #class width
  s<-seq(min(mydata), max(mydata) + c, by = c)
  cate <- cut(mydata,breaks = s,right = FALSE) #categorise y
  clas <- data.frame(table(cate))
  p <- c()
  if (distt == "norm") {
    fd <- fitdistr(mydata,"normal")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    p[1] <- pnorm(s[2],mean = mu,sd = sigma)
    p[df+1] <- 1 - pnorm(s[df+1],mean = mu,sd = sigma)
    for(i in 2:df)
    {
      p[i] <- pnorm(s[i+1],mean = mu,sd = sigma)-pnorm(s[i],mean = mu,sd = sigma)
    }
  } else if (distt == "cauchy") {
    fd <- fitdistr(mydata,"cauchy")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    p[1] <- pcauchy(s[2],location = mu,scale = sigma)
    p[df+1] <- 1 - pcauchy(s[df+1],location = mu,scale = sigma)
    for(i in 2:df)
    {
      p[i] <- pcauchy(s[i+1],location = mu,scale = sigma)-pcauchy(s[i],location = mu,scale = sigma)
    }
  }else if (distt == "unif") {
    fd <- fitdistr(mydata,"uniform")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    p[1] <- punif(s[2],min = mu,max = sigma)
    p[df+1] <- 1 - punif(s[df+1],min = mu,max = sigma)
    for(i in 2:df)
    {
      p[i] <- punif(s[i+1],min = mu,max = sigma) - punif(s[i],min = mu,max = sigma)
    }
  }else if (distt == "lnorm") {
    fd <- fitdistr(mydata,"lognormal")
    mu<-fd$estimate[1]
    sigma <-fd$estimate[2]
    p[1] <- plnorm(s[2],meanlog = mu,sdlog = sigma)
    p[df+1] <- 1 - plnorm(s[df+1],meanlog = mu,sdlog = sigma)
    for(i in 2:df)
    {
      p[i] <- plnorm(s[i+1],meanlog = mu,sdlog = sigma)-plnorm(s[i],meanlog = mu,sdlog = sigma)
    }
  }else if (distt == "exp") {
    fd <- fitdistr(mydata,"exponential")
    mu<-fd$estimate[1]
    p[1] <- pexp(s[2],rate = mu)
    p[df+1] <- 1 - pexp(s[df+1],rate = mu)
    for(i in 2:df)
    {
      p[i] <- pexp(s[i+1],rate = mu)-pexp(s[i],rate = mu)
    }
  }
  t<-data.frame(clas,p,n*p)
  #xsq<-sum(((t$Freq-t$n...p)^2)/t$n...p);xsq
  chisq.test(t$Freq,p=t$p)
}

