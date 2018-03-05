library(shiny)
library(tseries)
library(timeDate)
library(timeSeries)
library(fBasics)
library(zoo)
library(fUnitRoots)
library(plyr)
#Import and Head Dataset
myd=read.table("touristDataTransClean.csv",header=T, sep=',') 
head(myd)

shinyServer(function(input, output) {
  
  output$data=renderTable(head(myd,20))
  
  #distribution plots by month/year
  output$distPlot=renderPlot({
    if (input$unit=='Month') {
      TS=ddply(myd, .(Year,Month), numcolwise(sum))
    } else {
      TS=ddply(myd, .(Year), numcolwise(sum))
    }
    par(mfrow=c(1,2))
    # draw the histogram with the specified number of bins
    hist(TS$Count/10000,
         breaks=seq(0,max(TS$Count/10000),l=input$bins+1),
         xlab = 'Tourists Count(10K)',
         main = 'Histogram of Brazil Tourists Count by Month',
         freq=F)
    xfit=seq(0,800,length=1000)
    yfit=dnorm(xfit,mean=mean(TS$Count/10000), sd=sd(TS$Count/10000))
    lines(xfit, yfit, col="red") 
    # qq-plot
    qqnorm(TS$Count)
    qqline(TS$Count, col = 2)
    })
  # Time series plot 
  
})
