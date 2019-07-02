#install.packages('rsconnect')

library(rsconnect)
deployApp("F:")

# rsconnect::setAccountInfo(name='afakims',
#                           token='BBB49EC166F3F7610934BD1062EF38F1',
#                           secret='naUE2NCUix16OrDiUCDymcDleSrGxiBubXsvrSH2')
# 
# rsconnect::setAccountInfo(name='afakims', 
#                           token='BBB49EC166F3F7610934BD1062EF38F1', 
#                           secret='naUE2NCUix16OrDiUCDymcDleSrGxiBubXsvrSH2')

#rsconnect::deployApp('F:\afakims')

#git config --global user.email "oyoakim@yahoo.com"
#git config --global user.name "Owor Yoakim"

#------------------------------------------------------#
# PCA of the Iris Data                                                     #
#------------------------------------------------------#
# Function to plot the princple component of iris data
PCAplot<-function(mydata,catVar,i,j){
  #
  contVars <- getContVars(mydata)
  len <- length(unique(mydata[,catVar]));len
  ipc <- princomp(mydata[contVars], scores = TRUE)
  
  # Pricipal component estimates
  pPC <- ipc$scores
  # Rename columns to include the % of variance
  colnames(pPC) <-
    paste(colnames(pPC),
          
          "(",as.character(round(100 * ipc$sdev ^ 2 / sum(ipc$sdev ^ 2))),
          "%)")
  # Plot i against j Principal component
  plot(
    pPC[, c(i, j)],
    main = paste("PC",i," and PC",j),
    pch = 21,
    bg = c(1:len)[unclass(mydata[,catVar])]
  )
}

# Compute principal components

# summary(ipc)
# ipc['scores']
# 
# # Pricipal component estimates
# irisPC <- ipc$scores
# irisPC
# # Partion the plot window to display 6 graphs 
# op <- par(mfrow = c(3, 2))
# # Plot first against second Principal component
# PCAplot(irisPC,1,2)
# # Plot first against third Principal component
# PCAplot(irisPC,1,3)
# # Plot first against fourth Principal component
# PCAplot(irisPC,1,4)
# # Plot second against third Principal component
# PCAplot(irisPC,2,3)
# Plot second against fourth Principal component
#PCAplot(irisPC,2,4)
# Plot third against fourth Principal component
PCAplot(iris,"Species",3,2)
