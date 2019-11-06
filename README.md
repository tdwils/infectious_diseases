# Infectious Diseases

This Shiny app explores global mortality from infectious diseases using data from the World Health Organization for the year 2016. The app features drill-down capability by country and disease, a map highlighting the selected country, a distribution by sex, and summary numbers in infoboxes.

Packages used include plotly, ggplot, and rworldmap.

A working version of the app can be found here: https://tdwils.shinyapps.io/infectious_diseases/

## Data

The original files were downloaded from the WHO website as Excel template (.xlt) files. They were then opened in Excel and saved as Excel worksheets (.xlsx) to decrease the file size.

The script process_data.R was used to process the data and create the datasets used by the application.

Two files were downloaded from https://www.who.int/healthinfo/global_burden_disease/estimates/en/ :

1. Death rates: 
https://www.who.int/healthinfo/global_burden_disease/GHE2016_Death-Rates-country.xls?ua=1 , worksheet labeled "ASDR2016", titled "Age-standardized rate per 100 000 population by cause, sex and WHO Member State (1), 2016".

2. Number of deaths: 
https://www.who.int/healthinfo/global_burden_disease/GHE2016_Deaths_2016-country.xls?ua=1 , Estimated deaths ('000) by cause, sex and WHO Member State (1), 2016

### Data citation

Global Health Estimates 2016: Deaths by Cause, Age, Sex, by Country and by Region, 2000-2016. Geneva, World Health Organization; 2018.

