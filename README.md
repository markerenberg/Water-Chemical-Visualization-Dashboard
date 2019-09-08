# Water-Chemical Visualization Dashboard

## Dashboard Link
https://markkerenberg.shinyapps.io/final_app/

## Purpose
This dashboard was designed to investigate factors impacting chemical contaminants in US county municipal water supplies. It uses multiple forms of data visualizations to identify the influence of geography, population, drought severity, working population, and water usage in a county's chemical contaminant levels.

## Data
The underlying data for this dashboard contains information about the chemical concentration in community water systems from 2010 to 2016. The data is broken up into the following tables:
   1) Chemical Contaminants: Mean concentration of a particular chemical in a community water system through the counties in the US from 2010-2016.
   2) Droughts: Percentages of various ranges of drought severities for counties from 2010 - 2016.
   3) Industry Occupation: Estimated working population (16 yrs and over) for the various industries indexed by county, for 2010-2016.
   4) Water Usage: Information aobut water usage (irrigation, public supply, crop, etc.) and thermoelectric power generated for counties in the year 2010.

## Implementation
This dashboard was implented using R Shiny's shinydashboard package. It utilizes Plotly, Highcharter, GGplot, and many more packages for its visualizations.
