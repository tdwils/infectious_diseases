# ui_about.R


about <- function() tagList(
  fluidRow(
    column(width = 2),
    column(width = 8,
           includeMarkdown("includes/about.md")
    ),
    column(width = 2)
  )
)

