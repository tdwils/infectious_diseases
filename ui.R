# ui.R

source("includes/ui_dashboard.R")
source("includes/ui_about.R")


ui <- navbarPage("Global Health Estimates",
                 tabPanel("Infectious Diseases",
                          dashboard()
                 ),
                 tabPanel("About",
                          about()
                 )
)


