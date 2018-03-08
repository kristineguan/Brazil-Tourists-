library(shiny)
library(forecast)
library(plyr)
#Import Dataset
myd=read.table("touristDataTransClean.csv",header=T, sep=',') 

shinyServer(function(input, output) {
  output$data=renderTable(myd[1:10,])
  
  myd_sel=reactive({myd[myd$Year>=input$time[1] & myd$Year<=input$time[2],]})
  output$summary=renderPrint({
    (summary(myd_sel()))
    })
  #--------------------------------
  #distribution plots by month/year
  output$distPlot=renderPlot({
    if (input$unit=='Month') {
      TS=ddply(myd_sel(), .(Year,Month), numcolwise(sum))
    } else {
      TS=ddply(myd_sel(), .(Year), numcolwise(sum))
    }
    
    par(mfrow=c(4,2))
    # Time series plot
    if (input$unit=='Month') {
      ts_raw = ts((TS$Count/10000), start=c(min(TS$Year),1), freq=12)
    } else {
      ts_raw = ts((TS$Count/10000), start=c(min(TS$Year),1), freq=1)
    }
    plot(ts_raw, 
         main="Time plot of Tourists Count",
         ylab='Tourists Count(10K)')
    
    # Time series plot in log10 unit
    if (input$unit=='Month') {
      ts_log = ts((log10(TS$Count)), start=c(min(TS$Year),1), freq=12)
    } else {
      ts_log = ts((log10(TS$Count)), start=c(min(TS$Year),1), freq=1)
    }
    plot(ts_log, 
         main="Time plot of Tourists Count (Log10)",
         ylab='Tourists Count (Log10 unit)')
    
    # histogram with the specified number of bins
    hist(TS$Count/10000,
         breaks=seq(min(TS$Count/10000),max(TS$Count/10000),l=input$bins+1),
         xlab = 'Tourists Count(10K)',
         ylab = 'Probability',
         main = 'Histogram of Brazil Tourists Count',
         freq=F)
    xfit=seq(0,800,length=1000)
    yfit=dnorm(xfit,mean=mean(TS$Count/10000), sd=sd(TS$Count/10000))
    lines(xfit, yfit, col="red") 
    # histogram in log10 unit
    hist(log10(TS$Count),
         breaks=seq(min(log10(TS$Count)),max(log10(TS$Count)),l=input$bins+1),
         xlab = 'Tourists Count (Log10)',
         ylab = 'Probability',
         main = 'Histogram of Brazil Tourists Count (Log10)',
         freq=F)
    xfit=seq(0,10,length=1000)
    yfit=dnorm(xfit,mean=mean(log10(TS$Count)), sd=sd(log10(TS$Count)))
    lines(xfit, yfit, col="red")
    
    # qq-plot
    qqnorm(TS$Count/10000)
    qqline(TS$Count/10000, col = 2)
    
    qqnorm(log10(TS$Count),main = 'Normal Q-Q Plot (Log10)')
    qqline(log10(TS$Count), col = 2)
    
    # seasonal boxplot
    if (input$unit=='Month') {
      data=TS
    boxplot(log10(TS$Count)~TS$Month,data=TS, 
            main="Box-plot for Trourists Count by Month (Log10)", 
            xlab="Month", 
            ylab="Tourists Count by Month (Log10)"
            )}
  })
  #-------------------
  #prediction
  output$predPlot=renderPlot({
    if (input$unit=='Month') {
      TS=ddply(myd_sel(), .(Year,Month), numcolwise(sum))
      Log_10_Count=log10(TS$Count)
      m=arima(Log_10_Count, order=c(2,1,4),seasonal=list(order=c(0,1,1),period=12), method="ML")
    } else {
      TS=ddply(myd_sel(), .(Year), numcolwise(sum))
      Log_10_Count=log10(TS$Count)
      m=auto.arima(Log_10_Count,ic="bic",trace=F,stationary=F,seasonal=F)
    }
    #Log_10_Count=log10(TS$Count)
    #m1=auto.arima(Log_10_Count,ic="bic",trace=F,stationary=F,seasonal=TRUE)
    #m2=arima(Log_10_Count, order=c(2,1,4),seasonal=list(order=c(0,1,1),period=12), method="ML")
    
    if (input$unit=='Month'){h_=12}
    else{h_=5}
    
    f1=forecast.Arima(m,h=h_)
    f1$mean=10**(f1$mean)
    f1$upper=10**(f1$upper)
    f1$lower=10**(f1$lower)
    f1$x=10**(f1$x)
    f1$fitted = 10**(f1$fitted)
    if (input$unit=='Month'){
      plot(f1,main="1-step Ahead Forecast")
    }
    else{
      plot(f1,main="5-step Ahead Forecast")
    }
    
    #lines(fitted(f1),col="red")
  })

    })
