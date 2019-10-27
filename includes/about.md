# Mortality Due to Infectious Diseases, 2016

This is an interactive application for exploring data on mortality from infectious diseases.

## Instructions

Select a country by clicking on a bar in the graph on the left titled "Mortality Rate by Country". This will filter the middle graph and the graphs on the right by the country selected.

Then, select a disease by clicking on a bar in the center graph titled "Mortality Rate by Disease". This will filter the "Total Deaths by Sex" graph by the disease selected.

Scroll down the page to find x-axis range sliders below the left and middle graphs. Click and drag the endpoints of the sliders to zoom in to a specific range of the x-axis.

Click the yellow Reset button at the top to return to the default view.

All graphs except the world graph have hover-over text with context-specific information. When hovering over a graph, an icon appears in the upper right-hand corner to download the plot as a png.

## Data

The data are from the World Health Organization Global Health Estimates, and were downloaded from https://www.who.int/healthinfo/global_burden_disease/estimates/en/. 

**Citation:** Global Health Estimates 2016: Deaths by Cause, Age, Sex, by Country and by Region, 2000-2016. Geneva, World Health Organization; 2018. 

## Code

This application was created with R/Shiny using the packages plotly, ggplot, and rworldmap. The code is available at <github link>.
