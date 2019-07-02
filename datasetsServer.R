library(shiny)
library(RSQLite)
datasetsServer <- function(input, output, session) {
  ns <- session$ns
  #Get file input
  output$res <- renderText({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return (NULL)
    }
    
    mydata <- as.data.frame(read.csv(
      inFile$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    ))
    
    datasetName <- sub('\\..*', '', inFile$name)
    
    if(datasetExist(datasetName)){
      return(paste("Dataset with name ",datasetName,"exists"))
    }
    
    if(createNewDataset(datasetName, mydata)){
      return("Dataset upload completed successfully")
    }else{
      return("Dataset upload failed")
    }
    #return (datasetName)
  })
  
  
  mydataset <- reactive({
    d <- loadDataset(input$data_set)
    return(d)
  })
  #Display Information
  output$contents <- renderDataTable(mydataset(), options = list(pageLength = 10))

  # Know your data 
  output$dim<-renderPrint(dim(mydataset()))
  output$sum<-renderPrint(summary(mydataset()))
  output$class<-renderPrint(class(mydataset()))
  output$name<-renderPrint(names(mydataset()))
  output$str<-renderPrint(str(mydataset())) 
  output$top<-renderTable(head(mydataset(),5))
  output$bottom<-renderTable(tail(mydataset(),5))
}

#Function to create a new dataset
createNewDataset <- function(datasetName, datasetData) {
  #Connect to the database
  db <- dbConnect(SQLite(), "datasets/datasets.sqlite")
  #Create New dataset into the database
  res <- FALSE
  if (dbWriteTable(
    db,
    name = datasetName,
    value = datasetData,
    row.names = FALSE,
    append = FALSE
  )) {
    res <- TRUE
  }
  #Add this dataset to the list of datasets
  #Close the connection
  dbDisconnect(db)
  return(res)
}

#Function to get all the datasets in the system
getDatasets <- function() {
  #Connect to the database
  db <- dbConnect(SQLite(), "datasets/datasets.sqlite")
  #Fetch the datasets
  res <- dbListTables(db)
  #Close the connection
  dbDisconnect(db)
  #Return the datasets
  return(res)
}


#function to load a dataset specified by datasetName
#maxNum is the maximum number of records to fetch from the dataset
loadDataset <- function(datasetName) {
  #Connect to the database
  db <- dbConnect(SQLite(), "datasets/datasets.sqlite")
  #Fetch the dataset
  res <- dbReadTable(db,datasetName)
  #close the connection
  dbDisconnect(db)
  #Return the dataset
  return(res)
}

# Function to check if a dataset exists
datasetExist <- function(datasetName){
  db <- dbConnect(SQLite(), "datasets/datasets.sqlite")
  #Fetch result
  res <- dbListTables(db)
  #close the connection
  dbDisconnect(db)
  return(is.element(datasetName,res))
}

getCatVars <- function(dataSet){
  catVars <- NULL
  fields <- colnames(dataSet)
  for(field in fields){
    if(!is.numeric(dataSet[1,field])){
      catVars <- c(catVars,field)
    }
  }
  return(catVars)
}



getContVars <- function(dataSet){
  contVars <- NULL
  fields <- colnames(dataSet)
  for(field in fields){
    if(is.numeric(dataSet[1,field])){
      contVars <- c(contVars,field)
    }
  }
  return(contVars)
}

getCatVars2Levels <- function(dataSet){
  vars <- c()
  catVars <- getCatVars(dataSet)
  i <- 1
  for(var in catVars){
    if(length(unique(dataSet[,var])) == 2){
      vars[i] <- var
      i <- i + 1
    }
  }
  return(vars)
}

getCatVars2Levels(head(iris,150))
# dataSet <- loadDataset("carsdata")
# getCatVars(dataSet)
# getContVars(dataSet)

