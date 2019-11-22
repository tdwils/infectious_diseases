# functions.R

# Functions in this file:
#
# is.notNull
# get_plot_height
# clear_click

is.notNull <- function(x)!is.null(x)


# Finds height of horizontal bar plot based on number of bars
get_plot_height <- function(nbars) {
  pt_height <- 25 * nbars + 150
  return (pt_height)
}


# Clear click data so the next click will be recognized 
# if it is a click on the same value
clear_click <- function(source) {
  input_value <- paste0("plotly_click-", source)
  command <- paste0("Shiny.setInputValue('", input_value, "', null);")
  runjs(command)
}

