library(repmis)
library(knitr)
library(ca)

event.counts <- function(events, agg.date, source, target, code) {
  counts <- events %>%
    group_by_(agg.date, source, target, code) %>%
    summarise(n = n()) %>%
    ungroup()  # this seems trivial but screws up a lot of stuff if you don't do it
  output <- spread_(counts, code, 'n')
  output[is.na(output)] <- 0
  return(output)
}

quad <- c(1,2,3,4)
quadN <- c('Verbal Cooperation', 'Material Cooperation', 'Verbal Conflict', 'Material Conflict')
quadNames <- data.frame(as.factor(quad), quadN)
colnames(quadNames) <- c('quadCode', 'quadN')

e <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")

sectors <- c('GOV')
eventsSub <- filter(e, sourceSec %in% sectors & tarSec %in% sectors)

codes <- unique(eventsSub$quad)

Dim1 <- data.frame(quadN)
Dim2 <- data.frame(quadN)

for (i in 1:10) {
  eCounts <- event.counts(eventsSub, 'ym', 'sourceName', 'tarName', 'quad')
  eCounts$n <- rowSums(eCounts[,4:ncol(eCounts)])
  # filter <nfloor event dyads
  eCounts <- filter(eCounts, n >= i)
  # scale data
  countData <- eCounts[,colnames(eCounts) %in% codes]
  eCountsCA <- ca(countData, nd=2)
  
  caGamma <- as_tibble(eCountsCA$colcoord)
  
  caCol <- as_tibble(data.frame(colnames(countData), caGamma))
  
  colnames(caCol)[1] <- 'quadCode'
  caCol <- left_join(caCol, quadNames, by='quadCode')
  
  collabel <- paste0('floor', i)
  dim1 <- caCol %>% pull(Dim1)
  dim2 <- caCol %>% pull(Dim2)
  print(dim1)
  
  Dim1[ , collabel] <- dim1
  Dim2[ , collabel] <- dim2
}

kable(Dim1)
kable(Dim2)
