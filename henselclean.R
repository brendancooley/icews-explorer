library(readr)
library(dplyr)

hensel <- read_csv('icow_ext2017.csv')

### MOVE ALL OF THIS TO SEPARATE SCRIPT ###
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
