ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

libs <- c('shiny', 'readr', 'dplyr', 'plotly', 'forcats')
ipak(libs)

hensel1995 <- read_csv('hensel1995.csv')

# Define UI for application that draws a histogram
shinyUI(navbarPage("ICEWS Explorer",
  
  tabPanel("Setup",
    fluidPage(
      column(6,
        # Step 1
        h4('1) Load raw data. Click here:'),
        actionButton("loadData", "Load Raw Data"),
        helpText(textOutput("loadStatus")),
        h6('Source Sector Counts:'),
        tableOutput("sectorCounts"),
        # Step 2
        # TODO - shade out buttons that have already been chosen and shouldn't be modified
        h4("2) Select sectors and time aggregation"),
        checkboxGroupInput("sectors",
                           "Sectors:",
                           choices=c()),
        radioButtons("agglevel",
                     "Level of Aggregation:",
                     c("Year", "Month")),
        sliderInput("nfloor",
                    "Event Count Floor:",
                    1, 1000, 10, step = 10),
        
        # Step 3
        h4('3) Select Event Coding and Dimensionality:'),
        radioButtons("eCode",
                     "Event Coding",
                     c("CAMEO", "Quad Score")),
        selectInput("ndim",
                    "Number of Latent Dimensions",
                    seq(1, 10, 1), selected = 2),
        
        # Step 4
        h4('4) Build count data. Click here:'),
        actionButton("buildData", "Build Data"),
        helpText(textOutput("buildStatus")),
        
        # Step 5
        h4('5) Build Visualization'),
        actionButton("buildViz", "Build Viz"),
        helpText(textOutput("vizStatus")),
        
        # (Optional)
        h4("(Optional) Download Count Data and Scaling"),
        downloadButton("downloadData", "Download Data")
      )
    )
  ),
                   
  tabPanel("Visualization",
    # Inset Tab Style?
  
    fluidRow(
      column(2,
         wellPanel(
           selectInput("source",
                       "Source Country:",
                       ""),
           selectInput("target", 
                       "Target Country:",
                       ""),
           sliderInput("topN",
                       "Most Active:",
                       0, 1000, 100, step = 50),
           checkboxInput("dirdy",
                         "Directed Dyad?",
                         value = F),
           selectInput("biplotDim1",
                       "Biplot Dimension 1",
                       ""),
           selectInput("biplotDim2",
                       "Biplot Dimension 2",
                       ""),
           selectInput("tsdim",
                       "Time Series Dimension:",
                       ""),
           selectInput("hensel",
                       "Territorial Disputes:",
                       c("", sort(unique(hensel1995$dys)))),
           checkboxInput("reset",
                         "Reset?",
                         value = F)
         )
       ),
    
    column(5,
           h4(textOutput("dyadB")),
           plotlyOutput("biplot"),
           hr(),
           h4(textOutput("dyadTS")),
           plotlyOutput("ts1"),
           hr(),
           h4(textOutput("cameoHistTitle")),
           plotlyOutput('cameoHist')
           ),
    
    column(5,
           h4("Most Active Dyads"),
           plotlyOutput("tn")
          )
      
    )
  )
))
