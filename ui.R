library(shinycssloaders)
library(shinythemes)
library(dplyr)
library(DT)
library(readxl)
library(plotly)

StartDate = as.character(Sys.Date()-365*4)
EndDate = as.character(Sys.Date())

tickers <- read_excel("ticker_symbols.xlsx")

#------------------------------------------------------------------------
# Define UI layout and navigation bars
#------------------------------------------------------------------------

navbarPage(
  theme = shinytheme('cerulean'),
  #theme = 'my_style.css', # <- working to build out a customized CSS style
  title = 'Stock Analytics',
  tabPanel(
    'Select Stocks',
    icon = icon('search'),
    sidebarLayout(
      sidebarPanel(
        # include directions...
        helpText(
          'Type in ticker symbols in the boxes
          below then press the Pull Data button:'
        ),
        selectInput(
          inputId = 's1',
          label = strong("Input Stock_1 Symbol:"),
          choices = tickers$Symbol,
          selected = "PNC"
        ),
        selectInput(
          inputId = 's2',
          label = strong("Input Stock_2 Symbol:"),
          choices = tickers$Symbol,
          selected = "HBAN"
        ),
        selectInput(
          inputId = 's3',
          label = strong("Input Stock_3 Symbol:"),
          choices = tickers$Symbol,
          selected = "BAC"
        ),
        # Select date range to be plotted
        dateRangeInput(
          "date",
          strong("Select Date Range:"),
          start = StartDate,
          end = EndDate,
          min = StartDate,
          max = EndDate
        ),
        # Include clarifying text ----
        helpText(
          'After entering stock symbols above please
          click the button below to run the
          the application'
        ),
        actionButton("go", "Pull Data"),
        width = 3
        ),
      # generate plot on the page
      mainPanel(
        p('Price trend of the selected securities over time.'),
        plotlyOutput(outputId = "line") %>% withSpinner(type = 4),
        tags$a(href = "https://finance.yahoo.com/trending-tickers",
               "Source: Yahoo Finance",
               target = "_blank"),
        fluidRow(column(
          width = 12,
          p('Recent daily stock quote and volume statistics:'),
          DTOutput(outputId = 'quote') %>% withSpinner(type = 4)#,
          #plotOutput(outputId = 'sent') %>% withSpinner(type = 4)
        )),
        width = 9
      )
        )
),
tabPanel(
  'Analytics',
  icon = icon('bar-chart-o'),
  sidebarLayout(
    sidebarPanel(
      helpText('The graphs depict varying views of the
               secuirites'),
      # Select date range to be plotted
      dateRangeInput(
        "date2",
        strong("Select Date Range:"),
        start = StartDate,
        end = EndDate,
        min = StartDate,
        max = EndDate
      ),
      uiOutput('choices'),
      sliderInput(
        inputId = 'conf_lvl',
        label = strong('Select Confidence Level:'),
        min = 1,
        max = 100,
        value = 99
      ),
      width = 3
      ),
    mainPanel(fluidRow(
      column(
        width = 6,
        p('Performance of a $1 invested in each security over
          time.'),
        plotOutput(outputId = 'fv') %>% withSpinner(type = 5)
        ),
      column(
        width = 6,
        p('Return distribution with V@R'),
        plotOutput(outputId = 'dens') %>% withSpinner(type = 5)
      )
    ),
    fluidRow(
      column(
        width = 6,
        p('Correlation plot of the returns
          for the chosen securities:'),
        plotOutput(outputId = 'corr') %>% withSpinner(type = 5)
        ),
      column(
        width = 6,
        p('Summary statistics:'),
        tableOutput(outputId = 'sharpe') %>% withSpinner(type = 5)
      )
    ),
    
    width = 9)
    )
  
),
tabPanel(
  title = 'Portfolio Simulation',
  icon = icon('blackboard', lib = 'glyphicon'),
  sidebarLayout(sidebarPanel(
    fluidRow(
      column(
        width = 6,
        uiOutput('portf_sec1')),
        column(
          width = 4,
          numericInput(
            "w1",
            "Portf. wgt.",
            0.33,
            min = 0.01,
            max = 1,
            step = 0.01
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput('portf_sec2')),
          column(
            width = 4,
            numericInput(
              "w2",
              "Portf. wgt.",
              0.33,
              min = 0.01,
              max = 1,
              step = 0.01
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput('portf_sec3')),
            column(
              width = 4,
              numericInput(
                "w3",
                "Portf. wgt.",
                0.34,
                min = 0.01,
                max = 1,
                step = 0.01
              )
            )
          ),
          fluidRow(column(
            width = 6,
            textInput("index", label = strong('Benchmark'),
                      "^GSPC")),
            column(
              width = 4,
              numericInput(
                  inputId = 'invest', label = 'Investment',
                  value = 1000
              )
            )
          ),
    fluidRow(column(
      width = 6,
      helpText('Input forecast length in days')
      ),
      column(
        width = 4,
        numericInput(
          inputId = 'sim_days', label = 'Input Days',
          value = 20, min = 1, max = 90, step = 1
        )
      )
    ),
          # Select date range to be plotted
          dateRangeInput(
            "date3",
            strong("Select Date Range:"),
            start = StartDate,
            end = EndDate,
            min = StartDate,
            max = EndDate
          ),
          actionButton("build", "Run Sim"),
          width = 3
        ),
        mainPanel(fluidRow(column(
          width = 6,
          p(
            'The plot below does not consider periodic
            rebalancing of the portfolio.'
          ),
          plotlyOutput(outputId = 'portf')
        ),
        column(
          width = 6,
          p('Simulation of investment via the GBM stochastic process'),
          plotlyOutput(outputId = 'port_sim')
        )
          ),
          p('Summary statistics:'),
          tableOutput(outputId = 'portf_sum'),
          p(
            '*IR = Information Ratio -> calculation of the excess
            return of the portfolio over the benchmark return which is
            divided by the volaitily of the portfolio returns.'
          ),
        h4('Geomteric Brownian Motion'),uiOutput('gbm'),tags$a(
          href="https://www.researchgate.net/publication/290441536_Stochastic_Processes_for_the_Risk_Management",
          "More on the variation of the GBM used in this app can be found here.", target = "_blank"
        ),
          width = 9
          )
      )),
# tabPanel(
#   title = 'Market Sentiment',
#   icon = icon('eye-open', lib = 'glyphicon'),
#   sidebarLayout(sidebarPanel(width = 3),
#                 mainPanel(
#   width = 9)
# )),
    tabPanel(
      'About',
      icon = icon('book'),
      h4('Stock Analytics Application'),
      p(
        'This application is mainly to outline my skill with the
        R Statiscal Programming language
        and is prepared solely for informational purposes, and is not an
        offer to buy or sell or a solicitation of an offer to buy or sell
        any security, product, service or investment. The opinions expressed in this application
        do not constitute investment advice and an independent advice
        should be sought where appropriate.'
      ),
      p(
        'For more information about this app please contact me at:
        courtneywilliams561@gmail.com'
      )
      )
)