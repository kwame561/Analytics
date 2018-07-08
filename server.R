#------------------------------------------------------------------------
# Load required libraries
#------------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(ggplot2)
library(ggthemes)
library(GGally)
library(scales)
library(corrplot)
library(quantmod)
library(dplyr)
library(reshape2)
library(lubridate)
library(tidyquant)
library(cowplot)
library(zoo)
library(plyr)

#library(rsconnect) # activate line to load library
#rsconnect::deployApp('C:/Users/Court/Documents/Projects/Stock_App') # activate line to deploy update

#------------------------------------------------------------------------
# Setup asset prices and relevant market data
#------------------------------------------------------------------------

StartDate = as.character(Sys.Date()-365*4)
EndDate = as.character(Sys.Date())

shinyServer(function(input, output, session) {

  prices <- eventReactive(input$go, {
    symbols <- c(input$s1,input$s2,input$s3)
  
  # create a list object that has all the adjusted prices
  prices_list = lapply(symbols, function(pri) {
    Ad(getSymbols(pri, from=input$StartDate, auto.assign=FALSE))
  })
  
  # convert the list object into a workable xts object with all the adjusted prices
  prices_xts = do.call(merge, prices_list)
  
  # rename columns using the 'symbols' list
  colnames(prices_xts) = symbols
  
  # create a data frame
  prices_df = data.frame(date=index(prices_xts),coredata(prices_xts))
  
  # then convert dataframe into a molten data frame, which is easy to use with ggplot2
  prices_mdf = melt(prices_df, id.vars = 'date')
  
  })  
    
  selected_data = eventReactive(input$go, {
    
    prices <- prices()
    
    req(input$date)
    validate(need(!is.na(input$StartDate) & !is.na(input$EndDate),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$StartDate < input$EndDate, 
                  "Error: Start date should be earlier than end date."))
    prices %>%
      filter(
        date > as.POSIXct(input$StartDate) & date < as.POSIXct(input$EndDate)
      )
  })

  output$line <- renderPlot({
    
# plot prices using ggplot
plt_prices <-  ggplot(prices(),aes(x=date,y=value))+
  geom_line(aes(group=variable,colour=variable))+
  geom_smooth(linetype = 3)+
  facet_wrap(~variable, scales = 'free_y')
plt_prices <- plt_prices + xlab("Time") + ylab("Prices") + ggtitle("Price Trend") + scale_y_continuous(labels = dollar)+
  scale_color_discrete(name='Asset')

# plot graph
plt_prices

})

#------------------------------------------------------------------------
# Setup asset return data
#------------------------------------------------------------------------
  
  returns <- eventReactive(input$go, {
    symbols <- c(input$s1,input$s2,input$s3)
    
    # create a list object that has all the adjusted prices
    returns_list = lapply(symbols, function(sym) {
      dailyReturn(na.omit(getSymbols(sym, from=input$StartDate,
                                    auto.assign=FALSE)),
                 type = 'log')
    })
    
    # convert the list object into a workable xts object with all the adjusted prices
    returns_xts = do.call(merge, returns_list)
    
    # rename columns using the 'symbols' list
    colnames(returns_xts) = symbols
    
    gret <- 1+returns_xts #calculating gross returns
    gret = gret[complete.cases(gret),] # removes all NA's which may reduce the time window
    n <- nrow(gret)
    fv_gret <- cumprod(gret)
    fv_gret_df = data.frame(date=index(fv_gret),coredata(fv_gret))
    fv_gret_mdf = melt(fv_gret_df,id.vars = 'date')
    
  })    
  
  selected_data = eventReactive(input$go, {
    
    returns <- returns()
    
    req(input$date)
    validate(need(!is.na(input$StartDate) & !is.na(input$EndDate),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$StartDate < input$EndDate, 
                  "Error: Start date should be earlier than end date."))
    returns %>%
      filter(
        date > as.POSIXct(input$StartDate) & date < as.POSIXct(input$EndDate)
      )
  })
    
output$fv <- renderPlot({
  
  
  fv <- ggplot(returns(),aes(x=date,y=round(value,2),colour=variable))+
    geom_line()+
    scale_y_continuous(labels = dollar)+
    labs(
      title = 'FV of $1 invested',
      x = 'Date',
      y = 'Growth of $1 Invested'
    )+
    scale_color_discrete(name='Asset')
  
  fv
})  

#
#
#
corr <- eventReactive(input$go, {
  symbols <- c(input$s1,input$s2,input$s3)
  
  # create a list object that has all the adjusted prices
  returns_list = lapply(symbols, function(sym) {
    dailyReturn(na.omit(getSymbols(sym, from=input$StartDate,
                                   auto.assign=FALSE)),
                type = 'log')
  })
  
  # convert the list object into a workable xts object with all the adjusted prices
  returns_xts = do.call(merge, returns_list)
  
  # rename columns using the 'symbols' list
  colnames(returns_xts) = symbols
  
  returns_xts <- returns_xts[complete.cases(returns_xts),]
  
})

selected_data = eventReactive(input$go, {
  
  corr <- corr()
  
  req(input$date)
  validate(need(!is.na(input$StartDate) & !is.na(input$EndDate),
                "Error: Please provide both a start and an end date."))
  validate(need(input$StartDate < input$Endate, 
                "Error: Start date should be earlier than end date."))
  corr %>%
    filter(
      date > as.POSIXct(input$StartDate) & date < as.POSIXct(input$EndDate)
    )
})

output$corr <- renderPlot({
  my_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + 
      geom_point() + 
      geom_smooth(method=loess, fill="red", color="red", ...) +
      geom_smooth(method=lm, fill="blue", color="blue", ...)
    p
  }
  
  ggpairs(corr(), columns = 2:4,
                  lower = list(continuous = my_fn))
  

})
})

#VIX <- as.xts(na.omit(getSymbols("^VIX",from=StartDate,auto.assign=FALSE)))
#SP500_Price <- as.xts(na.omit(getSymbols("^GSPC",from=StartDate,auto.assign=FALSE)))
#SP500 <- as.xts(dailyReturn(na.omit(getSymbols("^GSPC",from=StartDate,auto.assign=FALSE))),type='log')