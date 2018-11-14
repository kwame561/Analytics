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
library(cowplot)
library(zoo)
library(plyr)
library(DT)
library(kableExtra)
library(readxl)
library(tidytext)
library(tidyr)
#library(purrr)
#library(tm)
#library(tm.plugin.webmining)
library(plotly)

#library(rsconnect) # activate line to load library
#rsconnect::deployApp('C:/Users/Court/Documents/Projects/Stock_App') # activate line to deploy update

#------------------------------------------------------------------------
# Setup asset prices and relevant market data
#------------------------------------------------------------------------

StartDate = as.character(Sys.Date()-365*4)
EndDate = as.character(Sys.Date())

# pull file that has tickers
tickers <- read_excel("ticker_symbols.xlsx")

shinyServer(function(input, output, session) {
  
  prices <- eventReactive(input$go, {
    symbols <- c(input$s1,input$s2,input$s3)
  
  # create a list object that has all the adjusted prices
  prices_list = lapply(symbols, function(pri) {
    Ad(getSymbols(pri, from=input$date[1], auto.assign=FALSE))
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

quote <- eventReactive(input$go, {
    quote <- c(input$s1,input$s2,input$s3)
    p <- getQuote(quote, src = 'yahoo')
    p
})

output$quote <- renderDT({
 
 datatable(quote())

})


  selected_prices = reactive({
    
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

  output$line <- renderPlotly({
    
# plot prices using ggplot
plt_prices <-  ggplot(selected_prices(),aes(x=date,y=value,
                                            text = paste('Symbol:',variable,
                                                         '<br>Date:',date,
                                                         '<br>Price:',scales::dollar(value))))+
  geom_line(aes(group=variable,colour=variable))+
  geom_smooth(linetype = 3)+
  facet_wrap(~variable, scales = 'free_y')
plt_prices <- plt_prices + xlab("") + ylab("Prices") + ggtitle("Price Trend") + 
  scale_y_continuous(labels = dollar) +
  theme(legend.title = element_blank())
  #scale_color_discrete(name='Asset')

# plot graph
ggplotly(plt_prices, tooltip = c('text')) %>%
  config(displayModeBar = FALSE) %>% layout(showlegend = FALSE)

})

#------------------------------------------------------------------------
# Setup asset return data
#------------------------------------------------------------------------
  
  returns <- eventReactive(input$go, {
    symbols <- c(input$s1,input$s2,input$s3)
    
    # create a list object that has all the adjusted prices
    returns_list = lapply(symbols, function(sym) {
      dailyReturn(na.omit(getSymbols(sym, from=input$date[1],
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
  
  selected_returns = reactive({
    
    returns <- returns()
    
    req(input$date)
    validate(need(!is.na(input$date2[1]) & !is.na(input$date2[2]),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$date2[1] < input$date2[2], 
                  "Error: Start date should be earlier than end date."))
    data.frame(date=index(returns),coredata(returns)) %>%
      filter(
        date > as.POSIXct(input$date2[1]) & date < as.POSIXct(input$date2[2])
      )
  })
    
output$fv <- renderPlot({
  
  returns() %>%
    ggplot(aes(
      x = date,
      y = round(value, 2),
      colour = variable
    )) +
    geom_line() +
    scale_y_continuous(labels = dollar) +
    labs(title = 'FV of $1 invested',
         x = 'Date',
         y = 'Growth of $1 Invested') +
    scale_color_discrete(name = 'Asset')
  
})  

# generate histogram plot of returns with V@R line
#-------------------------------------------------
symbols <- eventReactive(input$go,{
  symbols <- c(input$s1, input$s2, input$s3)
  
})

dens <- eventReactive(input$go, {
  symbols <- c(input$s1,input$s2,input$s3)
  
  # create a list object that has all the adjusted prices
  returns_list = lapply(symbols, function(sym) {
    dailyReturn(na.omit(getSymbols(sym, from=input$date[1],
                                   auto.assign=FALSE)),
                type = 'log')
  })
  
  # convert the list object into a workable xts object with all the adjusted prices
  returns_xts = do.call(merge, returns_list)
  
  # rename columns using the 'symbols' list
  colnames(returns_xts) = symbols

  # removes all NA's which may reduce the time window  
  returns_xts <- returns_xts[complete.cases(returns_xts),] 
  return_df <- data.frame(date=index(returns_xts),coredata(returns_xts))
  return_mdf = melt(return_df,id.vars = 'date')
  
})

output$choices <- renderUI({
  choices <- c(input$s1, input$s2, input$s3)
  selectInput('var','V@R View',choices)
})


dens_data = reactive({
  dens() %>%
    filter(
      variable == input$var
    )
})

output$dens <- renderPlot({
  # create group mean
  gd = dens_data() %>%
    group_by(variable) %>%
    summarise(value = mean(value))
  
  
  # create VaR via the quantile method
  q = dens_data() %>%
    group_by(variable) %>%
    summarise(value = quantile(value, 1 - (input$conf_lvl) / 100))
  
  # prepare density plots
  dens = ggplot(dens_data(), aes(x = value)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(alpha = .2, fill = "#FF6666") +
    xlab("Profit & Loss Distribution") + ylab("Density/Probability") +
    ggtitle(
      paste("Density Plot of Historical Returns -", input$var),
      subtitle = paste(
        "One-day value at risk of", percent(q$value),"given at the",
        percent((input$conf_lvl) / 100),
        "confidence level"
      )
    ) +
    scale_x_continuous(labels = percent)
  
  # plot density graph
  dens +
    geom_vline(data = q,
               aes(xintercept = value),
               linetype = "dashed") +
    geom_text(data = q,
              aes(
                x = value,
                y = 4,
                label = paste("V@R ", percent(value))
              ),
              colour = "blue") +
    theme_hc()
})


#-------------------------------------------------------
# Return Correlation
#-------------------------------------------------------
corr <- eventReactive(input$go, {
  symbols <- c(input$s1,input$s2,input$s3)
  
  # create a list object that has all the adjusted prices
  returns_list = lapply(symbols, function(sym) {
    dailyReturn(na.omit(getSymbols(sym, from=input$date[1],
                                   auto.assign=FALSE)),
                type = 'log')
  })
  
  # convert the list object into a workable xts object with all the adjusted prices
  returns_xts = do.call(merge, returns_list)
  
  # rename columns using the 'symbols' list
  colnames(returns_xts) = symbols
  
  returns_xts <- returns_xts[complete.cases(returns_xts),]
  
})

selected_corr = reactive({
  
  corr <- corr()
  
  req(input$date)
  validate(need(!is.na(input$date2[1]) & !is.na(input$date2[2]),
                "Error: Please provide both a start and an end date."))
  validate(need(input$date2[1] < input$date2[2], 
                "Error: Start date should be earlier than end date."))
  corr %>%
    filter(
      date > as.POSIXct(input$date2[1]) & date < as.POSIXct(input$date2[2])
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

sharpe <- eventReactive(input$go, {
  
  # create a list objects that has all the daily returns
   r1 <- dailyReturn(na.omit(getSymbols(input$s1, from=input$date[1],
                                   auto.assign=FALSE)),
                type = 'log')
   r2 <- dailyReturn(na.omit(getSymbols(input$s2, from=input$date[1],
                                        auto.assign=FALSE)),
                     type = 'log')
   r3 <- dailyReturn(na.omit(getSymbols(input$s3, from=input$date[1],
                                        auto.assign=FALSE)),
                     type = 'log')
  
  # convert the list object into a workable xts object with all the adjusted prices
  #sharpe_xts = do.call(merge, sharpe_list)
  
  # rename columns using the 'symbols' list
  colnames(r1) = input$s1
  colnames(r2) = input$s2
  colnames(r3) = input$s3
  
  # Average daily returns
  ra1 <- mean(r1, na.rm = TRUE)
  ra2 <- mean(r2, na.rm = TRUE)
  ra3 <- mean(r3, na.rm = TRUE)
  
  # Annualized daily returns
  ar1 <- (1+ra1)^365-1
  ar2 <- (1+ra2)^365-1
  ar3 <- (1+ra3)^365-1
  
  # Stdev
  stdev1 <- sd(r1, na.rm = TRUE)
  stdev2 <- sd(r2, na.rm = TRUE)
  stdev3 <- sd(r3, na.rm = TRUE)
  
  # sharpe ratio
  sharpe1 <- ra1/stdev1*sqrt(252)
  sharpe2 <- ra1/stdev2*sqrt(252)
  sharpe3 <- ra1/stdev3*sqrt(252)
  
  # putting it all together
  Ticker <- c(input$s1,input$s2,input$s3)
  Avg_Daily_Return <- percent(c(ra1,ra2,ra3))
  Daily_Volatility <- percent(c(stdev1,stdev2,stdev3))
  Annualized_Return <- percent(c(ar1,ar2,ar3))
  Sharpe_Ratio <- c(sharpe1,sharpe2,sharpe3)
  
  data.frame(
    Ticker,Avg_Daily_Return,Daily_Volatility
    ,Annualized_Return,Sharpe_Ratio
  )
  
})  

output$sharpe <- renderTable({
  
  sharpe()
  
})

#------------------------------------
# Construct Portflio
#------------------------------------

output$portf_sec1 <- renderUI({
  choices <- c(input$s1, input$s2, input$s3)
  selectInput(
    inputId = 'b1',
    label = 'Choice',
    choices = choices,
    selected = choices[1]
  )
})

output$portf_sec2 <- renderUI({
  choices <- c(input$s1, input$s2, input$s3)
  selectInput(
    inputId = 'b2',
    label = 'Choice',
    choices = choices,
    selected = choices[2]
  )
})

output$portf_sec3 <- renderUI({
  choices <- c(input$s1, input$s2, input$s3)
  selectInput(
    inputId = 'b3',
    label = 'Choice',
    choices = choices,
    selected = choices[3]
  )
})

port <- eventReactive(input$build, {
  symbols <- c(input$b1, input$b2, input$b3)
  index <- c(input$index)
  
  # create a list object that has all the adjusted prices
  r_list = lapply(symbols, function(sym) {
    dailyReturn(na.omit(getSymbols(
      sym, from = input$date3[1],
      auto.assign = FALSE
    )),
    type = 'log')
  })
  
  # convert the list object into a workable xts object with all the adjusted prices
  r_xts = do.call(merge, r_list)
  
  # rename columns using the 'symbols' list
  colnames(r_xts) = symbols
  
  weights <- c(input$w1, input$w2, input$w3)
  
  wr1 <- r_xts[, 1] * input$w1
  wr2 <- r_xts[, 2] * input$w2
  wr3 <- r_xts[, 3] * input$w3
  
  
  rp <- wr1 + wr2 + wr3
  colnames(rp) <- 'Portfolio'
  
  index_r <-
    dailyReturn(na.omit(getSymbols(
      index, from = input$date3[1],
      auto.assign = FALSE
    )),
    type = 'log')
  
  colnames(index_r) <- 'Benchmark'
  
  perform <- cbind(rp, index_r)
  
})

output$portf <- renderPlotly({
  validate(
    need(sum(input$w1,input$w2,input$w3) == 1, "Sum of weights must equal 1")
  )
  
  port <- port()
  
  gret <- 1 + port #calculating gross returns
  gret = gret[complete.cases(gret), ] # removes all NA's which may reduce the time window
  n <- nrow(gret)
  fv_gret <- cumprod(gret) * input$invest
  fv_gret_df = data.frame(date = index(fv_gret), coredata(fv_gret))
  
  withProgress(message = 'Configuring portfolio', value = 0, {
    # Number of times we'll go through the loop
    n <- length(fv_gret_df)
    
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", i, 'of',n))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }
  })
  
  ggplotly(
  fv_gret_df %>%
    melt(id.vars = 'date') %>%
    ggplot(aes(x = date, y = value,
               text = paste('Security:',variable,
                            '<br>Date:',date,
                            '<br>Amount:',scales::dollar(value)))) +
    geom_line(aes(group = variable, colour = variable)) +
    scale_y_continuous(labels = dollar) +
    labs(
      title = paste('Historical performance of',scales::dollar(input$invest),'invested'),
      x = 'Date',
      y = paste('Gowth of ',scales::dollar(input$invest),'/USD invested')
    ) +
    theme(legend.title = element_blank(), legend.position = 'top'),
  tooltip = c('text')
  ) %>% config(displayModeBar = FALSE)
  
})

output$port_sim <- renderPlotly({
  port <- port()
  
  gret <- 1 + port #calculating gross returns
  gret = gret[complete.cases(gret), ] # removes all NA's which may reduce the time window
  n <- nrow(gret)
  fv_gret <- cumprod(gret) * input$invest
  fv_gret_df = data.frame(date = index(fv_gret), coredata(fv_gret))
  
  #-----------
  ## Setup Simulation
  #-----------
  
  obs <- input$sim_days
  theta <- rnorm(obs,0,1)   # normally distributed random terms, mean = 0, stdv = 1
  theta_bm <- rnorm(obs,0,1)   # normally distributed random terms, mean = 0, stdv = 1
  mu <- mean(port[,'Portfolio'])              # expected daily return of portfolio
  mu_bm <- mean(port[,'Benchmark'])              # expected daily return of benchmark
  sd <- sd(port[,'Portfolio'])             # expected annual standard deviation of portfolio
  sd_bm <- sd(port[,'Benchmark'])             # Expected annual standard deviation of benchmark
  s <- input$invest              # starting value/price
  s_bm <- input$invest              # starting value/price
  Portfolio <- c(s)         # Price vector
  Benchmark <- c(s_bm)         # Price vector
  a <- 2                # index used below
  a_bm <- 2
  time <- seq(max(fv_gret_df$date), by = 'day', length.out = obs+1)            # Time. Days to put on the x axis
  
  for(i in theta) {
    S = s*(1 + mu/obs + sd/sqrt(obs)*i)
    Portfolio[a] <- S                        
    s = S                                 
    a = a + 1
  }
  
  for(i in theta_bm) {
    S_bm = s_bm*(1 + mu_bm/obs + sd_bm/sqrt(obs)*i)
    Benchmark[a_bm] <- S_bm                        
    s_bm = S_bm                                 
    a_bm = a_bm + 1
  }
  
  df <- data.frame(time,Portfolio,Benchmark)
  dfm <- reshape2::melt(df, id.vars = 'time', variable.name = 'Security')
  
  withProgress(message = 'Generate Plots', value = 0, {
    # Number of times we'll go through the loop
    n <- length(dfm)
    
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", i, 'of',n))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }
  })
  
  ggplotly(
  ggplot(dfm, aes(x = time, y = value,
                  text = paste('Security:',Security,
                               '<br>Amount:',scales::dollar(value),
                               '<br>Date:',time))) + 
    geom_line(aes(group = Security, colour = Security)) + scale_y_continuous(labels = scales::dollar) +
    labs(
      title = paste('Investment over the next',input$sim_days,'days'),
      x = 'Time in days',
      y = ''
    ),
  tooltip = c('text')
  ) %>% config(displayModeBar = FALSE) %>% layout(showlegend = FALSE)

})

output$sim_hist <- renderPlot({
  # Portfolio simulation
  port <- port()
  
  gret <- 1 + port #calculating gross returns
  gret = gret[complete.cases(gret), ] # removes all NA's which may reduce the time window
  n <- nrow(gret)
  fv_gret <- cumprod(gret) * input$invest
  fv_gret_df = data.frame(date = index(fv_gret), coredata(fv_gret))
  
  #-----------
  ## Setup Simulation
  #-----------
  port_sim <- function(x) {
    obs <- x
    theta <-
      rnorm(obs, 0, 1)   # normally distributed random terms, mean = 0, stdv = 1
    mu <-
      mean(port[, 'Portfolio'])              # expected daily return of portfolio
    sd <-
      sd(port[, 'Portfolio'])             # expected annual standard deviation of portfolio
    s <- input$invest              # starting value/price
    Portfolio <- c(s)         # Price vector
    a <- 2                # index used below
    
    for (i in theta) {
      S = s * (1 + mu / obs + sd / sqrt(obs) * i)
      Portfolio[a] <- S
      s = S
      a = a + 1
    }
    return(Portfolio)
  }
    
  sim1 <- replicate(input$sim_cnt, port_sim(input$sim_days), simplify = "array")
  
  withProgress(message = 'Simulating Portfolio', value = 0, {
    # Number of times we'll go through the loop
    n <- nrow(sim1)
    
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", i, 'of',n))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }
  })
  
  p1 <- qplot(sim1[input$sim_days,], geom = 'histogram', fill=I("#FF9999"), 
        col=I("#000099")) + 
    scale_x_continuous(labels = scales::dollar) + 
    labs(title = paste('The expected value by day',input$sim_days,'is',scales::dollar(mean(sim1[input$sim_days,]))), x = '', y = 'Frequency')
  
  # Benchmark simulation
  bench_sim <- function(x) {
  obs <- input$sim_days
  theta_bm <- rnorm(obs,0,1)   # normally distributed random terms, mean = 0, stdv = 1
  mu_bm <- mean(port[,'Benchmark'])              # expected daily return of benchmark
  sd_bm <- sd(port[,'Benchmark'])             # Expected annual standard deviation of benchmark
  s_bm <- input$invest              # starting value/price
  Benchmark <- c(s_bm)         # Price vector
  a_bm <- 2
  
  for(i in theta_bm) {
    S_bm = s_bm*(1 + mu_bm/obs + sd_bm/sqrt(obs)*i)
    Benchmark[a_bm] <- S_bm                        
    s_bm = S_bm                                 
    a_bm = a_bm + 1
  }
  return(Benchmark)
  }
  
  sim2 <- replicate(input$sim_cnt, bench_sim(input$sim_days), simplify = "array")
  
  withProgress(message = 'Simulating Benchmark', value = 0, {
    # Number of times we'll go through the loop
    n <- nrow(sim2)
    
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", i, 'of',n))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }
  })
  
  p2 <- qplot(sim2[input$sim_days,], geom = 'histogram', fill=I("turquoise3"), 
              col=I("#FF9999")) + 
    scale_x_continuous(labels = scales::dollar) + 
    labs(title = paste('The expected value by day',input$sim_days,'is',scales::dollar(mean(sim2[input$sim_days,]))), x = '', y = 'Frequency')
  
  plot_grid(p1, p2, labels = c('Portfolio', 'Benchmark'))
  
})


# Portfolio summary table
output$portf_sum <- renderTable({
  # calulate average daily returns
  port <- port()
  pra <- mean(port[,1],na.rm = TRUE)
  ira <- mean(port[,2],na.rm = TRUE)
  
  # annualized return
  pra_an <- (1+pra)^365-1
  ira_an <- (1+ira)^365-1
  
  # daily volatility
  pra_v <- sd(port[,1],na.rm = TRUE)
  ira_v <- sd(port[,2],na.rm = TRUE)
  
  # sharpe ratio
  pra_sharpe <- pra/pra_v*sqrt(252)
  ira_sharpe <- ira/ira_v*sqrt(252)
  
  # information ratio
  ir_p <- if(pra-ira>0) (pra-ira)/pra_v else 0
  ir_b <- if(ira-ira>0) (ira-ira)/ira_v else 0
  
  # putting it all together
  Securities <- c('Portfolio','Benchmark')
  Annual_Return <- percent(c(pra_an,ira_an))
  Sharpe <- c(pra_sharpe,ira_sharpe)
  IR <- c(ir_p,ir_b)
  
  dt <- data.frame(
    Securities
    ,Annual_Return,Sharpe,IR
  )
  
  dt
})

output$gbm <- renderUI({
    p(paste('The Geometric Brownian motion model in the simulation above uses as 
constants the actual expected return and expected standard deviations, 
representing the percent drift and percent volatility, respectively, for 
the Portfolio and Benchmark. The distributions below shows the possible 
outcomes at the end of the simulation period of',input$sim_days,'days. Remeber this endeavor is to illustrate 
my comfort level with the R statiscal programming language
      prepared solely for informational purposes.'))
})

#------------------------------------------------------------------------
# Market Sentiment Analysis
#------------------------------------------------------------------------

# sentiment <- eventReactive(input$go,{
#   symbol <- c(input$s1, input$s2, input$s3)
#   
#   company <- tickers %>%
#     filter(Symbol %in% symbol)
#   
#   
#   # create function to download the articles based on the symbols
#   download_articles <- function(symbol) {
#     WebCorpus(YahooNewsSource(symbol))
#   }
#   
#   # create dataframe containing articles per symbol
#   articles <- data_frame(company = company$Name,
#                          symbol = symbol) %>%
#     mutate(corpus = map(symbol, download_articles))
#   
#   
#   # turn each WebCorpus object into a data.frame using the tidy() function
#   tokens <- articles %>%
#     unnest(map(corpus, tidy)) %>%
#     unnest_tokens(word, text) %>%
#     select(company, datetimestamp, word, id, heading)
#   
#   # remove words that are not useful for an analysis,
#   # typically extremely common words such as “the”, “of”, “to”, and so forth
#   data(stop_words)
#   
#   tokens <- tokens %>%
#     anti_join(stop_words)
#   
#   
#   sentiment_count <- tokens %>%
#     inner_join(get_sentiments("loughran"), by = "word") %>%
#     count(sentiment, company) %>%
#     spread(sentiment, n, fill = 0)
#   
# })
# 
# output$sent <- renderPlot({
#   sentiment() %>%
#     mutate(score = (positive - negative) / (positive + negative)) %>%
#     mutate(company = reorder(company, score)) %>%
#     ggplot(aes(company, score, fill = score > 0)) +
#     geom_col(show.legend = FALSE) +
#     scale_y_continuous(labels = scales::percent) +
#     coord_flip() +
#     labs(title = 'Market sentiment based on the most 20 recent news articles',
#          x = "Company/Security",
#          y = "Positivity score among 20 recent news articles")
# })

})
#VIX <- as.xts(na.omit(getSymbols("^VIX",from=StartDate,auto.assign=FALSE)))
#SP500_Price <- as.xts(na.omit(getSymbols("^GSPC",from=StartDate,auto.assign=FALSE)))
#SP500 <- as.xts(dailyReturn(na.omit(getSymbols("^GSPC",from=StartDate,auto.assign=FALSE))),type='log')