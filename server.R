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
library(quantmod)
library(highcharter)
library(dplyr)
library(reshape2)
library(lubridate)
library(cowplot)
library(zoo)
library(plyr)
library(tidytext)
library(purrr)
library(tm.plugin.webmining)
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
    Ad(getSymbols(pri, from=StartDate, auto.assign=FALSE))
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
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], 
                  "Error: Start date should be earlier than end date."))
    prices %>%
      filter(
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2])
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
      dailyReturn(na.omit(getSymbols(sym, from=StartDate,
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
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], 
                  "Error: Start date should be earlier than end date."))
    returns %>%
      filter(
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2])
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


})

#VIX <- as.xts(na.omit(getSymbols("^VIX",from=StartDate,auto.assign=FALSE)))
#SP500_Price <- as.xts(na.omit(getSymbols("^GSPC",from=StartDate,auto.assign=FALSE)))
#SP500 <- as.xts(dailyReturn(na.omit(getSymbols("^GSPC",from=StartDate,auto.assign=FALSE))),type='log')
#r1 <- as.xts(dailyReturn(na.omit(getSymbols(s1,from=StartDate,auto.assign=FALSE))),type='log')
#r2 <- as.xts(dailyReturn(na.omit(getSymbols(s2,from=StartDate,auto.assign=FALSE))),type='log')
#r3 <- as.xts(dailyReturn(na.omit(getSymbols(s3,from=StartDate,auto.assign=FALSE))),type='log')