# Setup -------------------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

libs <- c('shiny', 'dplyr', 'readr', 'tidyr', 'lubridate', 'plotly', 'zoo', 'ca', 'repmis')
ipak(libs)

event.counts <- function(events, agg.date, source, target, code) {
  counts <- events %>%
    group_by_(agg.date, source, target, code) %>%
    summarise(n = n()) %>%
    ungroup()  # this seems trivial but screws up a lot of stuff if you don't do it
  output <- spread_(counts, code, 'n')
  output[is.na(output)] <- 0
  return(output)
}

### Load Data ###
hensel1995 <- read_csv('hensel1995.csv')

cameoNames <- read_csv('CAMEO Codes.csv')
cameoNames$cameoCode <- as.factor(cameoNames$cameoCode)

quad <- c(1,2,3,4)
quadN <- c('Verbal Cooperation', 'Material Cooperation', 'Verbal Conflict', 'Material Conflict')
quadNames <- data.frame(as.factor(quad), quadN)
colnames(quadNames) <- c('quadCode', 'quadN')

shinyServer(function(input, output, session) {
  
  master <- reactiveValues(events0 = NULL, eCounts = NULL)
  
  # Data Build Status
  events0 <- observeEvent(input$loadData, {
    withProgress(message = 'Loading raw event data...please wait', value=0, {
      # TODO events.csv to large for GitHub, maybe host elsewhere and query
      e <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")
      e <- as_tibble(e)
      updateCheckboxGroupInput(session, "sectors", choices = sort(unique(e$sourceSec)) )
      output$loadStatus <- renderText('Event data loaded. Ready to build count data.')
      incProgress(amount=.5, message='Building sector counts...')
      eCounts <- e %>% group_by(sourceSec) %>%
        summarise(n=prettyNum(n(), big.mark=","))
      output$sectorCounts <- renderTable(t(eCounts), colnames=FALSE)
      incProgress(amount=.5)
    })
    master$events0 <- e
  })
  
  # event count aggregation
  counts <- observeEvent(input$buildData, {
    withProgress(message = 'Building count data...please wait', value = 0, {
      
      # filter by sector
      sectors <- input$sectors
      eventsSub <- filter(master$events0, sourceSec %in% sectors & tarSec %in% sectors)
      
      # Change date based on aggregation level
      if(input$agglevel == 'Month') {
        eventsSub$date <- eventsSub$ym
      }
      else {
        eventsSub$date <- eventsSub$y
        eventsSub$date <- ymd(sprintf("%d-01-01",eventsSub$date))
      }

      # event coding
      eCode <- 'CAMEO'
      nCodes <- length(unique(eventsSub$CAMEO))
      codes <- unique(eventsSub$CAMEO)
      if (input$eCode == "Quad Score") {
        eCode <- 'quad'
        nCodes <- length(unique(eventsSub$quad))
        codes <- unique(eventsSub$quad)
      }
      
      incProgress(amount = .33, message = 'aggregating data...')
      
      # build count data
      eCounts <- event.counts(eventsSub, 'date', 'sourceName', 'tarName', eCode)
      # end <- nCodes - 1
      eCounts$n <- rowSums(eCounts[,4:ncol(eCounts)])
      # filter <nfloor event dyads
      eCounts <- filter(eCounts, n >= input$nfloor)

      incProgress(amount = .33, message = 'scaling data...')
      
      # scale data
      countData <- eCounts[,colnames(eCounts) %in% codes]
      countData <- countData[,colSums(countData) > 0]
      eCountsCA <- ca(countData, nd=input$ndim)
      caBeta <- as_tibble(eCountsCA$rowcoord)
      betas <- c()
      for (i in 1:input$ndim) {
        betas <- c(betas, paste('beta', i, sep=""))
      }
      colnames(caBeta) <- betas
      eCounts <- bind_cols(eCounts, caBeta)

      master$eCounts <- eCounts
      incProgress(amount = .34, message = 'complete!')
      output$buildStatus <- renderText("Count data built.")
      
    })
    
  })
  
  observeEvent(input$buildViz, {
    # update visualization panel
    # TODO (make sure all settings are ready to go when user clicks over)
    ndim <- as.integer(input$ndim)
    ec <- master$eCounts
    updateSelectInput(session, "source", choices = sort(unique(ec$sourceName)))
    updateSelectInput(session, "tsdim", choices = seq(1, ndim), selected=1)
    updateSelectInput(session, "biplotDim1", choices = seq(1, ndim), selected = 1)
    updateSelectInput(session, "biplotDim2", choices = seq(1, ndim), selected = 2)
    output$vizStatus <- renderText("Visualization built. Click 'Visualization' tab above to view." )
  })
  
  # data downloader
  output$downloadData <- downloadHandler(
    filename = "icews_count_data.csv",
    content = function(file) {
      write_csv(master$eCounts, file)
    }
  )
  
  # selector
  selector <- eventReactive(input$source, {
    if (!is.null(master$eCounts)) {
      events <- master$eCounts
      print('test1')
      events <- filter(events, sourceName == input$source)
      print('test2')
      targets <- setdiff(unique(events$tarName), input$source)
    }
    else {
      targets = ""
    }
    targets
  })
  
  observeEvent(input$source, {
    if (input$hensel == "") {
      updateSelectInput(session, "target", choices=sort(selector()), selected=sort(selector())[1])
    }
  })
  
  observeEvent(input$reset, {
    if (input$reset == T) {
      events <- master$eCounts
      updateSelectInput(session, "hensel", selected = "")
      updateCheckboxInput(session, "reset", value = F)
      updateSelectInput(session, "source", choices=sort(unique(events$sourceName)), selected = sort(unique(events$sourceName))[1])
    }
  })
  
  observeEvent(input$hensel, {
    if (input$hensel != "") {
      row <- hensel1995[hensel1995$dys == input$hensel, ]
      s <- row$chalname
      target <- row$tgtname
      updateSelectInput(session, "source", choices=s, selected=s)
      updateSelectInput(session, "target", choices=target, selected=target)
    }
  })
  
  # Data
  filteredData <- eventReactive({
    input$source
    input$target
    input$dirdy
    input$tsdim
    }, 
    {
    
    print('test3')
    st <- filter(master$eCounts, sourceName == input$source & tarName == input$target)
    # target-source events
    ts <- filter(master$eCounts, sourceName == input$target & tarName == input$source)

    output <- st
    if (input$dirdy == F) {
      output <- rbind(st, ts)
    }
    
    output
    
  })
  
  vis_dat <- reactive({
    
    output <- filteredData()
    
    betaD <- paste('beta', input$tsdim, sep="")
    output$betaD <- output[[betaD]]
    
    # filter only dimensions to visualize (labeled beta1 and beta2)
    betas <- c("", "")
    betas[1] <- paste("beta", input$biplotDim1, sep="")
    betas[2] <- paste("beta", input$biplotDim2, sep="")
    
    output <- select(output, one_of('date', 'n', 'sourceName', 'tarName', betas, 'betaD'))
    colnames(output) <- c('date', 'n', 'sourceName', 'tarName', 'beta1', 'beta2', 'betaD')
    
    print(output)
    print('test4')
    
    output
  })
  
  # Biplot
  observeEvent({
    input$target
    input$dirdy
    input$biplotDim1
    input$biplotDim2
  },
  {
    if (input$target != "") {
      
      output$dyadB <- renderText(paste(input$source, "-", input$target, " Biplot", sep = ""))
      output$dyadTS <- renderText(paste(input$source, "-", input$target, " Time Series", sep=""))
      
      output$biplot <- renderPlotly({
        plot_ly(data=vis_dat(), type='scatter', 
                x=~beta1, y=~beta2, 
                size=~n, color=~sourceName, mode='markers',
                hoverinfo='text',
                text=~paste('Date: ', date,
                            '<br> N Events: ', n)) %>%
          layout(xaxis = list(title=paste('Dimension', input$biplotDim1, sep = " ")), 
                 yaxis = list(title=paste('Dimension', input$biplotDim2, sep = " ")),
                 legend = list(x = 0.8, y = 0.9))
        })
      
      output$ts1 <- renderPlotly({
        plot_ly(data=vis_dat(), type='scatter',
                x=~date, y=~betaD,
                color=~sourceName, mode='lines+markers',
                hoverinfo='text',
                text=~paste('Date: ', date,
                            '<br> N Events: ', n)) %>%
          layout(xaxis = list(title="Date"), 
                 yaxis = list(title=paste('Dimension', input$tsdim, sep= " ")),
                 showlegend=FALSE)
        })
      }
    })
  
  
  ### Most Active Dyads ###
  topN <- reactive({
    events <- master$eCounts
    topN <- events[order(-events$n)[1:input$topN], ]

    # filter only dimensions to visualize (labeled beta1 and beta2)
    betas <- c("", "")
    betas[1] <- paste("beta", input$biplotDim1, sep="")
    betas[2] <- paste("beta", input$biplotDim2, sep="")
    
    topN <- select(topN, one_of('date', 'n', 'sourceName', 'tarName', betas))
    colnames(topN) <- c('date', 'n', 'sourceName', 'tarName', 'beta1', 'beta2')

    topN
  })
  
  output$tn <- renderPlotly({
    
    if (input$target != "") {
      
      plot_ly(data=topN(), type='scatter',
              x=~beta1, y=~beta2, size=~n, mode='markers', 
              marker=list(color='orange'),
              hoverinfo='text', 
              text=~paste(
                'Source: ', sourceName,
                '<br> Target: ', tarName,
                '<br> Date: ', date,
                '<br> N Events: ', n)) %>%
        # add data from left panel (in red)
        add_markers(data=vis_dat(), type='scatter', 
                    x=~beta1, y=~beta2, 
                    size=~n,
                    marker=list(color='red'), hoverinfo='text',
                    text=~paste(
                      'Source: ', sourceName,
                      '<br> Target: ', tarName,
                      '<br> Date: ', date,
                      '<br> N Events: ', n)) %>%
        layout(xaxis = list(title=paste('Dimension', input$biplotDim1, sep = " ")), 
               yaxis = list(title=paste('Dimension', input$biplotDim2, sep = " ")),
               showlegend=FALSE)
      
    }
    
  })
  
  ### CAMEO Counts ###
  counts <- reactive({
    dyad <- filteredData()
    output$cameoHistTitle <- renderText(paste(input$source, "-", input$target, " Event Counts (Pooled)", sep = ""))
    if (input$eCode == 'CAMEO') {
      codes <- as.character(unique(cameoNames$cameoCode))
      counts <- select(dyad, one_of(codes))
      top10 <- sort(colSums(counts), decreasing=TRUE)[1:10]
      top10 <- data.frame(names(top10), top10)
      colnames(top10) <- c('cameoCode', 'Counts')
      top10 <- left_join(top10, cameoNames, by='cameoCode')
      rownames(top10) <- NULL
      C <- top10
    }
    
    if (input$eCode == 'Quad Score') {
      codes <- quadNames$quadCode
      counts <- select(dyad, one_of(codes))
      top4 <- sort(colSums(counts), decreasing=TRUE)
      top4 <- data.frame(names(top4), top4)
      colnames(top4) <- c('quadN', 'Counts')
      top4 <- left_join(top4, quadNames, by='quadCode')
      rownames(top4) <- NULL
      C <- top4
    }
    C
  })

  output$cameoHist <- renderPlotly({
    
    # TODO - reorder event output so that it makes more sense
    
    if (input$target != "") {
      
      p <- plotly_empty()
      
      if (input$eCode == 'CAMEO') {
        p <- plot_ly(counts(), x=~cameoCode, y=~Counts,
                hoverinfo='text',
                text=~paste(eventName,
                            '<br> Goldstein Score: ', goldstein)) %>%
          layout(xaxis = list(title='CAMEO Code'))
      }
      
      if (input$eCode == 'Quad Score') {
          p <- plot_ly(counts(), x=~quadCode, y=~Counts,
                  hoverinfo='text',
                  text=~paste(quadN)) %>%
            layout(xaxis = list(title='Quad Code'))
      }
      
    }
    
    p
    
  })
  
})
