event.counts <- function(events, agg.date, source, target, code) {
  counts <- events %>%
    group_by_(agg.date, source, target, code) %>%
    summarise(n = n()) %>%
    ungroup()  # this seems trivial but screws up a lot of stuff if you don't do it
  output <- spread_(counts, code, 'n')
  output[is.na(output)] <- 0
  return(output)
}


# Filter Pure Events ------------------------------------------------------

library(dplyr)
library(readr)

events <- read_csv('events.csv')
saveRDS(events, file = 'events.rds')
eventsRDS <- readRDS('events.rds')
head(eventsRDS)

countries <- unique(events$sourceName)
countries

cats <- unique(events$sourceSec)
cats

catsSub <- c('GOV', 'REB', 'OPP')

eventsSub <- filter(events, sourceSec %in% catsSub & tarSec %in% catsSub)



add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title") 
{
  add_axis(vis, "x", title = x_lab) %>% 
    add_axis("x", orient = "top", ticks = 0, title = title,
             properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0)
             ), ...)
}


vmccY <- read_csv('vmccY.csv')
vmccYM <- read_csv('vmccYM.csv')
vmccYGG <- read_csv('vmccYGG.csv')
vmccYMGG <- read_csv('vmccYMGG.csv')

hensel1995 <- read_csv('hensel1995.csv')


event.counts <- function(events, agg.date, source, target, code) {
  counts <- events %>%
    group_by_(agg.date, source, target, code) %>%
    summarise(n = n()) %>%
    ungroup()
  output <- spread_(counts, code, 'n')
  output[is.na(output)] <- 0
  return(output)
}

betaSelector <- function(df, dim1, dim2) {
  betas <- c("", "")
  betas[1] <- paste("beta", dim1, sep="")
  betas[2] <- paste("beta", dim2, sep="")
  outputDF <- bind_cols(df[,betas[1]], df[,betas[2]])
  print(outputDF)
  return(outputDF)
}

## output debug ##

events <- read_csv('~/Dropbox/Public/events.csv')
events <- events %>% mutate(date=events$y)
events$date <- ymd(sprintf("%d-01-01",events$date))

sectors <- c('GOV')
eventsSub <- filter(events, sourceSec %in% sectors & tarSec %in% sectors)
head(eventsSub)
#filter 0 event events?

eCounts <- event.counts(eventsSub, 'date', 'sourceName', 'tarName', 'CAMEO')
head(eCounts)

eCounts$n <- rowSums(eCounts[,4:ncol(eCounts)])
# filter 0 event dyads
eCounts <- filter(eCounts, n > 10)
# colnames(eCounts)[colnames(eCounts) %in% c('y', 'ym')] <- 'date'
# filter 0 event events

codes <- unique(events$CAMEO)
countData <- eCounts[,colnames(eCounts) %in% codes]
countData <- countData[,colSums(countData) > 0]

head(countData)
eCountsCA <- ca(countData, 2)
caBeta <- as_tibble(eCountsCA$rowcoord)
betas <- c()
for (i in 1:2) {
  betas <- c(betas, paste('beta', i, sep=""))
}
colnames(caBeta) <- betas

caBeta

eCounts2 <- bind_cols(eCounts, caBeta)

class(eCounts2)
betaD <- 'beta1'
as.vector(select(eCounts2, betaD))
add_column(eCounts2, betaD = list(select(eCounts2, betaD)))

eCounts2$betaD <- eCounts2[[betaD]]

eCounts2$date <- as.Date(eCounts2$date)
head(eCounts2)


head(eCounts2)
colnames(eCounts)[which(colnames(eCounts) == betaD)] <- betaD
eCounts

outliers <- filter(eCounts, beta1 > 2)


topN <- eCounts[order(-eCounts$n)[1:100], ]
topN$date <- topN[,1]
head(topN)

# filter only dimensions to visualize (labeled beta1 and beta2)
betas <- c("", "")
betas[1] <- paste("beta", input$biplotDim1, sep="")
betas[2] <- paste("beta", input$biplotDim2, sep="")

topN <- select(topN, c('date', 'n', 'sourceName', 'tarName', betas))
colnames(topN) <- c('date', 'n', 'sourceName', 'tarName', 'beta1', 'beta2')
print(topN)

topN

plot(eCountsCA$rowcoord[1:100,1], eCountsCA$rowcoord[1:100,2])
head(eCountsCA$rowcoord)
# ok so this is an outlier problem


AfgAus <- filter(eCounts2, sourceName %in% c('Afghanistan', 'Australia') & tarName %in% c('Afghanistan', 'Australia'))


### histogram w/ CAMEO counts ###

cameoNames <- read_csv('CAMEO Codes.csv')
cameoNames$cameoCode <- as.factor(cameoNames$cameoCode)

codes <- as.character(unique(events$CAMEO))
counts <- select(AfgAus, one_of(codes))

top10 <- sort(colSums(counts), decreasing = TRUE)[1:10]
names(top10)
top10 <- data.frame(names(top10), top10)
colnames(top10) <- c('cameoCode', 'Counts')
top10 <- left_join(top10, cameoNames, by='cameoCode')

rownames(top10) <- NULL

plot_ly(top10, x=~cameoCode, y=~Counts, 
        hoverinfo='text',
        text=~paste(eventName,
                    '<br> Goldstein Score: ', goldstein)) %>%
  layout(xaxis = list(title='CAMEO Code'))

# plotly debug
plot_ly(data=AfgAus, type='scatter',
        x=~date, y=~betaD,
        color=~sourceName, mode='lines+markers',
        hoverinfo='text',
        text=~paste('Date: ', date,
                    '<br> N Events: ', n)) %>%
  layout(xaxis = list(title="Date"), 
         yaxis = list(title=paste('Dimension', 1, sep= " ")),
         showlegend=FALSE)

sort(eCountsCA$rowcoord[,1], decreasing = T)


### histogram with quad counts ###

e <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")

sectors <- c('GOV')
eventsSub <- filter(e, sourceSec %in% sectors & tarSec %in% sectors)
eventsSub$date <- eventsSub$y

eCounts <- event.counts(eventsSub, 'date', 'sourceName', 'tarName', 'quad')

eCounts$n <- rowSums(eCounts[,4:ncol(eCounts)])
eCounts <- filter(eCounts, n > 10)

codes <- unique(e$quad)
countData <- eCounts[,colnames(eCounts) %in% codes]
countData <- countData[,colSums(countData) > 0]

eCountsCA <- ca(countData, 2)
caBeta <- as_tibble(eCountsCA$rowcoord)
betas <- c()
for (i in 1:2) {
  betas <- c(betas, paste('beta', i, sep=""))
}
colnames(caBeta) <- betas

eCounts2 <- bind_cols(eCounts, caBeta)

quad <- c(1,2,3,4)
quadN <- c('Verbal Cooperation', 'Material Cooperation', 'Verbal Conflict', 'Material Conflict')
quadNames <- data.frame(as.factor(quad), quadN)
colnames(quadNames) <- c('quadCode', 'quadN')

dyad <- filter(eCounts2, sourceName %in% c('Afghanistan', 'Australia') & tarName %in% c('Afghanistan', 'Australia'))

codes <- as.character(quadNames$quadCode)
counts <- select(dyad, one_of(codes))
top4 <- sort(colSums(counts), decreasing=TRUE)
top4 <- data.frame(names(top4), top4)
colnames(top4) <- c('quadCode', 'Counts')
top4 <- left_join(top4, quadNames, by='quadCode')
rownames(top4) <- NULL
C <- top4

p <- plot_ly(C, x=~quadCode, y=~Counts,
             hoverinfo='text',
             text=~paste(quadN)) %>%
  layout(xaxis = list(title='Quad Code'))
p

### get file from dropbox ###
library(RCurl)
library(repmis)

mykey <- 'koctuiks16uyxal'
filename <- 'events.csv'

events2 <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")

link <- getURL('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1')
events4 <- read_csv(link)


library(data.table)
events3 <- fread('~/Dropbox/Public/events.csv')  

events3 <- as_tibble(events3)



### column loadings ###

e <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")

sectors <- c('GOV')
eventsSub <- filter(e, sourceSec %in% sectors & tarSec %in% sectors)
eventsSub$date <- eventsSub$y

eCounts <- event.counts(eventsSub, 'date', 'sourceName', 'tarName', 'CAMEO')

eCounts$n <- rowSums(eCounts[,4:ncol(eCounts)])
eCounts <- filter(eCounts, n > 10)

codes <- unique(e$CAMEO)
countData <- eCounts[,colnames(eCounts) %in% codes]
countData <- countData[,colSums(countData) > 0]

eCountsCA <- ca(countData, 2)
caGamma <- as_tibble(eCountsCA$colcoord)

caCol <- as_tibble(data.frame(colnames(countData), caGamma))
colnames(caCol) <- c('cameoCode', 'gamma1', 'gamma2')

cameoNames <- read_csv('CAMEO Codes.csv')
cameoNames$cameoCode <- as.factor(cameoNames$cameoCode)

caCol <- left_join(caCol, cameoNames, by='cameoCode')
caCol[order(caCol$gamma1), ]

var <- caCol[,1+1]
caCol[ order(caCol[ , 2]), ]
dim1 <- 1
var <- colnames(caCol)[dim1+1]

caCol[,var]

arrange_(caCol, .dots=var)[1:5]


### add continent indicator to event data ###
library(countrycode)
e <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")
e <- as_tibble(e)

e$sourceContinent <- countrycode(e$sourceNum, 'cown', 'continent')
e$tarContinent <- countrycode(e$tarNum, 'cown', 'continent')
ifelse(is.na(e$sourceContinent), '---', e$sourceContinent)
ifelse(is.na(e$tarContinent), '---', e$tarContinent)




### truplication problem

# ym quad GOV-GOV

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
e$tarContinent <- ifelse(e$sourceNum %in% c(345, 347), 'Europe', e$tarContinent)
###
# ~7,000,000 observations

eventsSub <- filter(e, sourceSec %in% c('GOV') & tarSec %in% c('GOV'))
eventsSub$date <- eventsSub$ym
# ~ 1,600,000 observations

eCode <- 'quad'
nCodes <- length(unique(eventsSub$quad))
codes <- unique(eventsSub$quad)

# build count data
eCounts <- event.counts(eventsSub, 'date', 'sourceNum', 'tarNum', eCode)
# end <- nCodes - 1
eCounts$n <- rowSums(eCounts[,4:ncol(eCounts)])
# filter <nfloor event dyads (greater than 10)
eCounts <- filter(eCounts, n >= 10)
# 35,625 observations
eCountsdyym <- select(eCounts, one_of('sourceNum', 'tarNum', 'date'))

# same number of observations, no duplication here
unique(eCountsdyym)


countData <- eCounts[,colnames(eCounts) %in% codes]
countData <- countData[,colSums(countData) > 0]
eCountsCA <- ca(countData, nd=2)
caBeta <- as_tibble(eCountsCA$rowcoord)
betas <- c()
for (i in 1:2) {
  betas <- c(betas, paste('beta', i, sep=""))
}
colnames(caBeta) <- betas
eCounts <- bind_cols(eCounts, caBeta)
head(eCounts)
# still same number of observations

## join cow codes/cow numbers/binary dispute indicator ###
varsSource <- c('sourceName', 'sourceNum', 'sourceContinent')
varsTar <- c('tarName', 'tarNum', 'tarContinent')
eventsCowSource <- e %>% select(one_of(varsSource))
eventsCowTar <- e %>% select(one_of(varsTar))

ecs <- unique(eventsCowSource)
ect <- unique(eventsCowTar)
ecs$sourceName[ecs$sourceNum == 345] <- 'Yugoslavia'
ect$tarName[ect$tarNum == 345] <- 'Yugoslavia'

ecs %>% arrange(sourceName) %>% print(n=200)

ecs$sourceName[duplicated(ecs$sourceName)]

eCounts <- left_join(eCounts, ecs, by = 'sourceNum')
eCounts <- left_join(eCounts, ect, by = 'tarNum')

# do by number rather than name, problem with the conversion

eCounts <- left_join(eCounts, disputesMall, by = c('sourceNum', 'tarNum', 'date'))
sort(unique(eCounts$sourceName))

unique(disputesMall)
disputesYall
unique(disputesYall)

e <- read_csv('/Users/brendancooley/Dropbox/2016-2017/Lee Prather/Paper 1 - Enforcement Effects/ICEWS analysis/territorial dispute data/events.csv')

