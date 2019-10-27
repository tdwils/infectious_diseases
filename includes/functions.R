# functions.R

# Functions in this file:
#
# is.notNull
# get_plot_height
# 

is.notNull <- function(x)!is.null(x)


# Finds height of horizontal bar plot based on number of bars
get_plot_height <- function(nbars) {
  pt_height <- 25 * nbars + 150
  return (pt_height)
}

