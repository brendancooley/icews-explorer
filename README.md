# ICEWS Explorer

Shiny application for filtering, scaling, and visualizing dyadic event data from [International Crisis Early Warning System (ICEWS)](https://dataverse.harvard.edu/dataverse/icews). To run:

```{r}
require(shiny)
runGitHub("icews-explorer", user="brendancooley")
```

This document describes the data filtering and scaling procedures underlying the construction of the dynamic plots rendered in the application. Raw event data resides in the folder 'reducedICEWS' and was constructed using [software](https://github.com/philip-schrodt/text_to_CAMEO) developed by Phil Schrodt. The software converts the raw ICEWS files into a format suitable for analysis - associating each dyadic event with a source and target [Correlates of War](http://www.correlatesofwar.org/) country code, a source and target sector (e.g. Government, Military, or Non-Governmental Organization), and event code (CAMEO and quad). Quad codes correspond to:

- 1: Verbal Cooperation
- 2: Material Cooperation
- 3: Verbal Conflict
- 4: Material Conflict

The script 'icewsReadr.R' processes the .txt files produced by the Schrodt software, stripping out events that have no country associate with them and events that occur within countries (self-dyads). The 'events.csv' file produced by this script lives in the repository and is also hosted on [dropbox](https://www.dropbox.com/sh/eo5gmfeyq7cpk0y/AABNqIPY5dTzvNdDygocEMiSa?dl=0&preview=events.csv). It consists of ~7,600,000 dyadic events from 1995-2015. This is the data used by the application.

The application allows users flexibility in how they filter and scale this data.