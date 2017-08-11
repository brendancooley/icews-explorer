# ICEWS Explorer

Shiny application for filtering, scaling, and visualizing dyadic event data from [International Crisis Early Warning System (ICEWS)](https://dataverse.harvard.edu/dataverse/icews). To run:

```{r}
require(shiny)
runGitHub("icews-explorer", user="brendancooley")
```

This document describes the data filtering and scaling procedures underlying the construction of the dynamic plots rendered in the application. Raw event data is hosted [here](https://dataverse.harvard.edu/dataverse/icews) and was constructed using [software](https://github.com/philip-schrodt/text_to_CAMEO) developed by Phil Schrodt. The software converts the raw ICEWS files into a format suitable for analysis - associating each dyadic event with a source and target [Correlates of War](http://www.correlatesofwar.org/) country code, a sector (e.g. Government, Military, or Non-Governmental Organization), and event code (CAMEO and quad). Quad codes correspond to:

- 1: Verbal Cooperation
- 2: Material Cooperation
- 3: Verbal Conflict
- 4: Material Conflict