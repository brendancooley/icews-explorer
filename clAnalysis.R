library(repmis)

e <- source_data('https://www.dropbox.com/s/koctuiks16uyxal/events.csv?dl=1', header=T, sep=",")

sectors <- c('GOV')
eventsSub <- filter(e, sourceSec %in% sectors & tarSec %in% sectors)
eventsSub$date <- eventsSub$y

eCounts <- event.counts(eventsSub, 'date', 'sourceName', 'tarName', 'CAMEO')