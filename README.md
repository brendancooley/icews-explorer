# ICEWS Explorer

Shiny application for filtering, scaling, and visualizing dyadic event data from [International Crisis Early Warning System (ICEWS)](https://dataverse.harvard.edu/dataverse/icews). To run:

```{r}
require(shiny)
runGitHub("icews-explorer", user="brendancooley")
```

The application begins with raw event data. Each observation is a 'dyadic event', in which actor A takes some action X toward actor B on some date Y. The application performs 4 tasks:

1. Filter the raw event data.
2. Aggregate the data by dyad and time.
3. Scale the data to produce a low dimensional score for each dyad-time.
4. Visualize the data.

This document describes the data filtering and scaling procedures underlying the construction of the dynamic plots rendered in the application. Raw event data resides in the folder 'reducedICEWS' and was constructed using [software](https://github.com/philip-schrodt/text_to_CAMEO) developed by Phil Schrodt. The software converts the raw ICEWS files into a format suitable for analysis - associating each dyadic event with a source and target [Correlates of War](http://www.correlatesofwar.org/) country code, a source and target sector (e.g. Government, Military, or Non-Governmental Organization), and event code (CAMEO and quad). Quad codes correspond to:

- 1: Verbal Cooperation
- 2: Material Cooperation
- 3: Verbal Conflict
- 4: Material Conflict

The script 'icewsReadr.R' processes the .txt files produced by the Schrodt software, stripping out events that have no country associated with them and events that occur within countries (self-dyads). The 'events.csv' file produced by this script lives in the repository and is also hosted on [dropbox](https://www.dropbox.com/sh/eo5gmfeyq7cpk0y/AABNqIPY5dTzvNdDygocEMiSa?dl=0&preview=events.csv). It consists of ~7,100,000 dyadic events from 1995-2015. These are the data used by the application.

The application allows users flexibility in how they filter and scale this data. 

[[https://raw.githubusercontent.com/brendancooley/icews-explorer/master/app_setup.png]]

In the first step, users load the raw data. This can take a few minutes due to the size of the underlying event data. Once the data is loaded, a table of event counts by source sector is produced. In step 2, users filter and scale the data. Filtering can occur by continent and sector. For example, if only 'Africa' and 'GOV' are selected under 'Continents' and 'Sectors,' respectively, then only events that occured between governments in Africa will be included. In server.R, 

```{r}
# filter by sector
sectors <- input$sectors
continents <- input$continents
eventsSub <- filter(master$events0, sourceSec %in% sectors & tarSec %in% sectors & 
                    sourceContinent %in% continents & tarContinent %in% continents)
```

Then users choose how to aggregate the data. Data can be aggregated either by year or by month. If users choose to aggregate by year, the application converts the filtered event data into directed dyad-year-counts. Each row in the count data frame gives the number of events of each category that country A took toward country B in year Y. The 'Event Count Floor' field filters out dyad-years that experienced less than N events. Using N > 1 can improve the stability of the scaling algorithm, particularly when using the CAMEO event coding. The function `event.counts()` converts the filtered event data into count data.

```{r}
event.counts <- function(events, agg.date, source, target, code) {
  counts <- events %>%
    group_by_(agg.date, source, target, code) %>%
    summarise(n = n()) %>%
    ungroup()
  output <- spread_(counts, code, 'n')
  output[is.na(output)] <- 0
  return(output)
}
```

This aggregated count data is used to produce the scaling. I use correspondence analysis to convert the matrix of count data into a low-dimensional representation, with the number of dimensions chosen by the user in the application. Correspondence analysis first standardizes the count data, and then conducts a singular value decomposition on the residuals. See [Greenacre 2016](https://www.crcpress.com/Correspondence-Analysis-in-Practice-Third-Edition/Greenacre/p/book/9781498731775) for a full treatment of Correspondence Analysis and  [Lowe](http://dl.conjugateprior.org/preprints/mmfed.pdf) for its application to analysis of event data. I use Greenacre et al's `ca` library to implement the algorithm. In this case, the 'row coordinates' of the correspondence analysis are an N-dimensional score for each directed dyad-time. The 'column coordinates' are an N-dimensional score for each event category. Interpretation of the row coordinates depends on the values of the column coordinates and will vary based on which data are being used and how they are being aggregated. The values for the 'column coordinates' tell us 'how much' of each event category a given dimension consists of. When using the quad count event categories with two dimensions, this tends to produce a conflict-cooperation scale on the first dimension and a material-verbal scale on the second dimension (see below). The following code shows how the application converts the count data into scaled data. 

```{r}
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
```

Once the data has been produced, users can either download it or proceed to the visualization on the second tab, an example of which is reproduced here:

