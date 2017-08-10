library(readr)
library(dplyr)

# v2 includes post-2001 claims
hensel <- read_csv('icow_ext2017_v2.csv')

name_conv <- read_csv('name_conv.csv', col_names = F)

colnames(name_conv) <- c('chalname', 'newname')
hensel <- left_join(hensel, name_conv, by = 'chalname')
colnames(hensel)[colnames(hensel) == 'newname'] <- 'chalname2'

colnames(name_conv) <- c('tgtname', 'newname')
hensel <- left_join(hensel, name_conv, by = 'tgtname')
colnames(hensel)[colnames(hensel) == 'newname'] <- 'tgtname2'

hensel$chalname <- ifelse(is.na(hensel$chalname2), hensel$chalname, hensel$chalname2)
hensel$tgtname <- ifelse(is.na(hensel$tgtname2), hensel$tgtname, hensel$tgtname2)
# Ivory Coast manual conversion
hensel$tgtname[hensel$tgtname == "Côte D'Ivoire"] <- "Cote d'Ivoire"

hensel$startyear <- as.integer(substring(hensel$begclaim, 1, 4))
hensel$endyear <- as.integer(substring(hensel$endclaim, 1, 4))
hensel$dyad <- paste(hensel$chalname, "-", hensel$tgtname, sep="") 
hensel$years <- paste("(", hensel$startyear, "-", hensel$endyear, ")", sep="")
hensel$dys <- paste(hensel$dyad, hensel$years, sep = " ")
hensel1995 <- filter(hensel, endyear >= 1995)

write_csv(hensel1995, 'hensel1995.csv')




### build binary indicators for dyad-year disputes ###
hensel1995$sourceNum <- hensel1995$chal
hensel1995$tarNum <- hensel1995$tgt
hensel1995$startYM <- as.yearmon(paste0(as.character(hensel1995$begclaim), '01'), format='%Y%m%d')
hensel1995$endYM <- as.yearmon(paste0(as.character(hensel1995$endclaim), '01'), format='%Y%m%d')
hensel1995$startY <- hensel1995$startyear
hensel1995$endY <- hensel1995$endyear

varsY <- c('sourceNum', 'tarNum', 'startY', 'endY')

# construct dyad-year data
disputesY <- select(hensel1995, one_of(varsY))
disputesY$endY <- ifelse(is.na(disputesY$endY), 2017, disputesY$endY)
disputesY$id <- seq(1, nrow(disputesY))
allY <- disputesY %>%
  rowwise() %>%
  do(data.frame(id=.$id, date=seq(.$startY,.$endY,by=1)))
disputesYall <- left_join(allY, disputesY, by='id')
vars <- c('date', 'sourceNum', 'tarNum')
# make sure dispute is reflexive
disputesYst <- disputesYall %>% select(one_of(vars))
disputesYts <- disputesYst
colnames(disputesYts) <- c('date', 'tarNum', 'sourceNum')

disputesYall <- bind_rows(disputesYst, disputesYts)
disputesYall$dispute <- 1

# there are duplicates because there are sometimes muliple disputes
# we only want an indicator so clean these up
disputesYall <- unique(disputesYall)

write_csv(disputesYall, 'disputesYall.csv')


# construct dyad-month data
varsM <- c('sourceNum', 'tarNum', 'startYM', 'endYM')

disputesM <- select(hensel1995, one_of(varsM))
end <- as.yearmon(paste0(as.character(201701), '01'), format='%Y%m%d')
disputesM$endYM <- if_else(is.na(disputesM$endYM), end, disputesM$endYM)
disputesM$id <- seq(1, nrow(disputesM))
disputesM$startYM <- as.Date(disputesM$startYM)
disputesM$endYM <- as.Date(disputesM$endYM)
allM <- disputesM %>%
  rowwise() %>%
  do(data.frame(id=.$id, date=seq.Date(.$startYM,.$endYM,by="month")))
disputesMall <- left_join(allM, disputesM, by='id')
vars <- c('date', 'sourceNum', 'tarNum')
# make sure dispute is reflexive
disputesMst <- disputesMall %>% select(one_of(vars))
disputesMts <- disputesMst
colnames(disputesMts) <- c('date', 'tarNum', 'sourceNum')

disputesMall <- bind_rows(disputesMst, disputesMts)
disputesMall$date <- as.yearmon(disputesMall$date)
disputesMall$dispute <- 1

disputesMall <- unique(disputesMall)

write_csv(disputesMall, 'disputesMall.csv')

