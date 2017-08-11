# Setup -------------------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

libs <- c('shiny', 'dplyr', 'readr', 'tidyr', 'lubridate', 'plotly', 'zoo', 'ca', 'repmis',
          'countrycode')
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
disputesMall <- read_csv('disputesMall.csv')
disputesYall <- read_csv('disputesYall.csv')

cameoNames <- read_csv('CAMEO Codes.csv')
cameoNames$cameoCode <- as.factor(cameoNames$cameoCode)

quad <- c(1,2,3,4)
quadN <- c('Verbal Cooperation', 'Material Cooperation', 'Verbal Conflict', 'Material Conflict')
quadNames <- data.frame(as.factor(quad), quadN)
colnames(quadNames) <- c('quadCode', 'quadN')

shinyServer(function(input, output, session) {
  
  master <- reactiveValues(events0 = NULL, eCounts = NULL, colLoadings = NULL)
  
  # Data Build Status
  events0 <- observeEvent(input$loadData, {
    withProgress(message = 'Loading raw event data...please wait', value=0, {
      # TODO events.csv to large for GitHub, maybe host elsewhere and query
      e <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")
      e <- as_tibble(e)
      # add continent variable
      ### TODO move this to preprocessing and fix non matches
      e$sourceContinent <- countrycode(e$sourceNum, 'cown', 'continent')
      e$tarContinent <- countrycode(e$tarNum, 'cown', 'continent')
      e$sourceContinent <- ifelse(is.na(e$sourceContinent), '---', e$sourceContinent)
      e$tarContinent <- ifelse(is.na(e$tarContinent), '---', e$tarContinent)
      # manual continent assignment for Hong Kong, Montenegro, Kosovo, Palestine
      e$sourceContinent <- ifelse(e$sourceNum %in% c(0, 997), 'Asia', e$sourceContinent)
      e$tarContinent <- ifelse(e$tarNum %in% c(0, 997), 'Asia', e$tarContinent)
      e$sourceContinent <- ifelse(e$sourceNum %in% c(345, 347), 'Europe', e$sourceContinent)
      e$tarContinent <- ifelse(e$tarNum %in% c(345, 347), 'Europe', e$tarContinent)
      ###
      updateCheckboxGroupInput(session, "continents", choices = sort(unique(e$sourceContinent)), selected = unique(e$sourceContinent))
      updateCheckboxGroupInput(session, "sectors", choices = sort(unique(e$sourceSec)))
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
      continents <- input$continents
      eventsSub <- filter(master$events0, sourceSec %in% sectors & tarSec %in% sectors & 
                            sourceContinent %in% continents & tarContinent %in% continents)
      
      # Change date based on aggregation level
      if(input$agglevel == 'Month') {
        eventsSub$date <- eventsSub$ym
      }
      else {
        eventsSub$date <- eventsSub$y
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
      eCounts <- event.counts(eventsSub, 'date', 'sourceNum', 'tarNum', eCode)
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
      
      # save column loadings to master
      caGamma <- as_tibble(eCountsCA$colcoord)
      
      caCol <- as_tibble(data.frame(colnames(countData), caGamma))
      
      if (input$eCode == 'CAMEO') {
        colnames(caCol)[1] <- 'cameoCode'
        caCol <- left_join(caCol, cameoNames, by='cameoCode')
      }
      if (input$eCode == 'Quad Score') {
        colnames(caCol)[1] <- 'quadCode'
        caCol <- left_join(caCol, quadNames, by='quadCode')
      }
      master$colLoadings <- caCol
      
      ### join cow codes/cow numbers/binary dispute indicator ###
      varsSource <- c('sourceName', 'sourceNum', 'sourceContinent')
      varsTar <- c('tarName', 'tarNum', 'tarContinent')
      eventsCowSource <- master$events0 %>% select(one_of(varsSource))
      eventsCowTar <- master$events0 %>% select(one_of(varsTar))
      
      ecs <- unique(eventsCowSource)
      ect <- unique(eventsCowTar)
      
      eCounts <- left_join(eCounts, ecs, by = 'sourceNum')
      eCounts <- left_join(eCounts, ect, by = 'tarNum')

      # merge hensel indicators
      if(input$agglevel == 'Month') {
        eCounts <- left_join(eCounts, disputesMall, by = c('sourceNum', 'tarNum', 'date'))
      }
      if(input$agglevel == 'Year') {
        eCounts <- left_join(eCounts, disputesYall, by = c('sourceNum', 'tarNum', 'date'))
      }
      eCounts$dispute <- ifelse(is.na(eCounts$dispute), 0, eCounts$dispute)
      
      ### save and output data ###
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
    updateSelectInput(session, "tsdate", choices = sort(unique(ec$date)), selected = sort(unique(ec$date))[1])
    updateSelectInput(session, "biplotDim1", choices = seq(1, ndim), selected = 1)
    updateSelectInput(session, "biplotDim2", choices = seq(1, ndim), selected = 2)
    output$vizStatus <- renderText("Visualization built. Click 'Visualization' tab above to view." )
  })
  
  # reset button
  observeEvent(input$resetData, {
      js$reset()
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
      events <- filter(events, sourceName == input$source)
      targets <- setdiff(unique(events$tarName), input$source)
    }
    else {
      targets = ""
    }
    targets
  })
  
  observeEvent(input$target, {
    if (!is.null(master$eCounts)) {
      # update options for ts viz
      dyad <- filteredData()
      updateSelectInput(session, "tsdate", choices = sort(unique(dyad$date)), selected = sort(unique(dyad$date))[1])
    }
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
    dyad <- filter(dyad, date == input$tsdate)
    output$cameoHistTitle <- renderText(paste(input$source, "-", input$target, " Event Counts (", input$tsdate, ")", sep = ""))
    
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
      codes <- as.character(quadNames$quadCode)
      counts <- select(dyad, one_of(codes))
      top4 <- sort(colSums(counts), decreasing=TRUE)
      top4 <- data.frame(names(top4), top4)
      colnames(top4) <- c('quadCode', 'Counts')
      top4 <- left_join(top4, quadNames, by='quadCode')
      print(top4)
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
  
  ### column loading tables ###
  output$clD1data <- renderTable({
    clD1data <- data.frame()
    if (!is.null(master$colLoadings)) {
      dim1 <- as.integer(input$biplotDim1)
      output$clD1head <- renderText(paste('Dimension ', dim1, sep=""))
      var <- colnames(master$colLoadings)[dim1+1]
      print(var)
      print(master$colLoadings)
      if (input$eCode == 'CAMEO') {
        bottom <- head(arrange_(master$colLoadings, .dots=var), 5)
        top <- tail(arrange_(master$colLoadings, .dots=var), 5)
        clD1data <- select(bind_rows(bottom, top), one_of('cameoCode', var, 'eventName'))
      }
      if (input$eCode == 'Quad Score') {
        clD1data <- arrange_(master$colLoadings, .dots=var)
        clD1data <- select(clD1data, one_of('quadCode', var, 'quadN'))
      }
    }
    clD1data
  })
  
  output$clD2data <- renderTable({
    clD2data <- data.frame()
    if (!is.null(master$colLoadings)) {
      dim2 <- as.integer(input$biplotDim2)
      output$clD2head <- renderText(paste('Dimension ', dim2, sep=""))
      var <- colnames(master$colLoadings)[dim2+1]
      if (input$eCode == 'CAMEO') {
        bottom <- head(arrange_(master$colLoadings, .dots=var), 5)
        top <- tail(arrange_(master$colLoadings, .dots=var), 5)
        clD2data <- select(bind_rows(bottom, top), one_of('cameoCode', var, 'eventName'))
      }
      if (input$eCode == 'Quad Score') {
        clD2data <- arrange_(master$colLoadings, .dots=var)
        clD2data <- select(clD2data, one_of('quadCode', var, 'quadN'))
      }
    }
    clD2data
  })
  
})
