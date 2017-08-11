packages <- c('readr', 'dplyr', 'tidyr', 'zoo', 'lubridate', 'countrycode')
lapply(packages, require, character.only = TRUE)

# remap NAs
empty_as_na <- function(x) {
  ifelse(as.character(x)!="", x, NA)
}

# Import Data -------------------------------------------------------------

reducedFiles <- list.files("reducedICEWS")
events.Y <- list()
for (i in 1:length(reducedFiles)) {
  print(reducedFiles[i])
  events.Y[[i]] <- read.delim(paste('reducedICEWS/', reducedFiles[i], sep=""), header = F)
  colnames(events.Y[[i]]) <- c("date", "sourceName", "sourceNum", "sourceSec",
                               "tarName", "tarNum", "tarSec", "CAMEO", "Goldstein", "quad")
  events.Y[[i]] %>% mutate_if(is.factor, as.character) %>% mutate_all(funs(empty_as_na)) %>% as_tibble() -> events.Y[[i]]
}
events <- bind_rows(events.Y)

# check unmatched events
events997 <- events[events$sourceNum == 997, ] # Hong Kong
events0 <- events[events$sourceNum == 0, ] # IGO

### country name conversion ###
events$sourceNameFull <- countrycode(events$sourceNum, 'cown', 'country.name')
events$tarNameFull <- countrycode(events$tarNum, 'cown', 'country.name')
events$sourceNameFull[events$sourceNum == 997] <- 'Hong Kong'
events$sourceNameFull[events$sourceName == 'PSE'] <- 'Occupied Palestinian Territory'
events$tarNameFull[events$tarNum == 997] <- 'Hong Kong'
events$tarNameFull[events$tarName == 'PSE'] <- 'Occupied Palestinian Territory'

events$sourceName <- events$sourceNameFull
events$tarName <- events$tarNameFull

name_conv <- read_csv('name_conv.csv', col_names = F)

# reduce complexity of selected country names using correspondence in name_conv.csv
colnames(name_conv) <- c('sourceName', 'newname')
events <- left_join(events, name_conv, by = 'sourceName')
colnames(events)[colnames(events) == 'newname'] <- 'sourceName2'

colnames(name_conv) <- c('tarName', 'newname')
events <- left_join(events, name_conv, by = 'tarName')
colnames(events)[colnames(events) == 'newname'] <- 'tarName2'

events$sourceName <- ifelse(is.na(events$sourceName2), events$sourceName, events$sourceName2)
events$tarName <- ifelse(is.na(events$tarName2), events$tarName, events$tarName2)
# Ivory Coast manual conversion
events$tarName[events$tarName == "Côte D'Ivoire"] <- "Cote d'Ivoire"
events$sourceName[events$sourceName == "Côte D'Ivoire"] <- "Cote d'Ivoire"

### filter NAs ###
events <- filter(events, sourceName != '---' & tarName != '---')
events <- filter(events, !is.na(sourceName) & !is.na(tarName))

### filter self-dyads ###
events <- filter(events, sourceName != tarName)

# add year month category
events$ym <- as.yearmon(events$date)
events$y <- year(events$date)

write_csv(events, 'events.csv')