# ui_dashboard.R


dashboard <- function() tagList(
  fluidPage(
    useShinyjs(),
    includeCSS("www/style.css"),
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        
        # br(),
        fluidRow(
          box(class = "title-text",
              paste0("Mortality ",
                     "Due to Infectious Diseases, 2016"),
              width = 3),
          # deaths worldwide from infectious diseases,
          valueBoxOutput("deaths_box",
                         width = 3),
          # deaths worldwide from infectious diseases: Females,
          valueBoxOutput("numbers_females_box",
                         width = 3),
          # deaths worldwide from infectious diseases: Males,
          valueBoxOutput("numbers_males_box",
                         width = 3)
        ),
        
        fluidRow(
          column(width = 4,
                 # Reset button
                 tags$div(class = "btnLeftAlign reset-button", 
                          actionButton(inputId = "reset", 
                                       label = "Reset")),
                 # Mortality rate by country
                 h4(paste("Mortality Rate by Country"), 
                    style = "text-align: center"),
                 h5("(age-standardized rate per 100,000 population)", 
                    style = "text-align: center"),
                 h5("Click on bar to filter by country.", 
                    class = "instructionsText"),
                 uiOutput("ui_plot_country")
          ),
          column(width = 4,
                 # Mortality rate by disease
                 uiOutput("disease_plot_title") %>%
                   withSpinner(type = 8, size = 0.4, color = spinner_color),
                 h5("Click on bar to filter by disease", 
                    class = "instructionsText"),
                 plotlyOutput("plot_disease") %>%
                   withSpinner(type = 8, size = 0.4, color = spinner_color)
          ),
          column(width = 4,
                 uiOutput("plot_title") %>%
                   withSpinner(type = 8, size = 0.4, color = spinner_color),
                 
                 # Map
                 br(),
                 plotOutput("world_map", height = "230px") %>%
                   withSpinner(type = 8, size = 0.4, color = spinner_color),
                 
                 # Mortality by sex
                 br(),
                 h4(paste("Total Deaths by Sex"), 
                    style = "text-align: center"),
                 plotlyOutput("plot_sex", height = "250px") %>%
                   withSpinner(type = 8, size = 0.4, color = spinner_color)
          )
        )
      )
    )
  )
)




