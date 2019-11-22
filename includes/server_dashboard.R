# server_dashboard.R


# Dataset of mortality rates ---------------------------------------------------
get_dataset_rates <- reactive ({

  dataset <- readRDS("data/death_rates.Rds") 
  
  return (dataset)
})


# Dataset of mortality numbers ---------------------------------------------------
get_dataset_numbers <- reactive ({

  dataset <- readRDS("data/death_numbers.Rds") 
  
  return (dataset)
})


# Infoboxes -------------------------------------------------------------------

output$deaths_box <- renderValueBox({
  
  dataset_numbers <- get_dataset_numbers() %>%
    filter(Sex == "All") 
  total_deaths <- sum(dataset_numbers$deaths, na.rm = TRUE) %>% 
    format(big.mark = ",", trim = TRUE, digits = 0, scientific = FALSE)
  
  valueBox(
    HTML(
      paste0(total_deaths, " <br/>Persons")
    ), 
    "deaths worldwide from infectious diseases", 
    icon = icon("globe"),
    color = "blue"
  )
})

output$numbers_females_box <- renderValueBox({
  
  total_numbers_females <- get_dataset_numbers() %>%
    filter(Sex == "Female") %>% 
    select(deaths) %>%
    sum(na.rm = TRUE) %>% 
    format(big.mark = ",", trim = TRUE, digits = 0, scientific = FALSE)
  
  valueBox(
    HTML(
      paste0(total_numbers_females, " <br/>Females")
    ), 
    "deaths worldwide from infectious diseases", 
    icon = icon("venus"),
    color = "green"
  )
})

output$numbers_males_box <- renderValueBox({
  
  total_numbers_males <- get_dataset_numbers() %>%
    filter(Sex == "Male") %>% 
    select(deaths) %>%
    sum(na.rm = TRUE) %>% 
    format(big.mark = ",", trim = TRUE, digits = 0, scientific = FALSE)
  
  valueBox(
    HTML(
      paste0(total_numbers_males, " <br/>Males")
    ), 
    "deaths worldwide from infectious diseases", 
    icon = icon("mars"),
    color = "purple"
  )
})

# Reactive values -----------------------------------------------------------
values <- reactiveValues(
  
  reset_country_plot = TRUE,   # Regenerate Country plot
  reset_disease_plot = TRUE,   # Regenerate Disease plot
  
  country_clicked = NULL,       # Country clicked by user
  disease_clicked = NULL        # Disease clicked by user
)


# Reset --------------------------------------------------------------------
observeEvent(input$reset, {

  values$reset_country_plot <- TRUE
  values$reset_disease_plot <- TRUE

  values$country_clicked <- NULL
  values$disease_clicked <- NULL

})


# Plot: rate by country ------------------------------------------------

rate_country <- reactive({

  # This redraws the graph when the Reset button has been clicked
  if (values$reset_country_plot) {
    isolate(values$reset_country_plot <- FALSE)
  }

  data <- get_dataset_rates()
  
  sum_by_country <- data %>% 
    filter(Sex == "All") %>%
    group_by(Country) %>%
    summarize(total = sum(rate, na.rm = TRUE)) 
  
  # Highlight the bar selected
  hk <- highlight_key(sum_by_country, ~Country)
  
  # Alphabetical order from top
  g <- ggplot(hk, aes(x = reorder(Country, total), y = total, key = Country,
                      text = paste(Country, ": ", round(total, digits = 0)))) +
    geom_bar(stat = "identity",
             position = position_stack(reverse = FALSE),
             fill = country_color) + 
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(axis.title.y = element_text(size = 20)) +
    theme(legend.position = "none") +
    theme(axis.text.y = element_text(size = 8)) +
    aes(reorder(stringr::str_wrap(Country, 22), total), total) +
    xlab(NULL) +
    coord_flip()
  
  # Determine size of rangeslider
  ncountries <- length(unique(sum_by_country$Country))
  pt_height <- get_plot_height(nbars = ncountries)
  rangeslider_thickness <- rangeslider_height / pt_height
  
  gg <- g %>% 
    ggplotly(source = "country", height = pt_height, tooltip = c("text")) %>%
    layout(margin = list(l = 60)) %>%
    rangeslider(start = 0, end = max(sum_by_country$total, na.rm = TRUE),
                thickness = rangeslider_thickness)
  
  # Highlight bar clicked
  ggg <- highlight(gg, 
                   # color = highlight_color, 
                   dynamic = FALSE, 
                   on = "plotly_click",
                   off = NULL) %>% 
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", 
                                      "lasso2d", "zoomIn2d", "zoomOut2d",
                                      "autoScale2d", 
                                      "resetScale2d",
                                      "hoverClosestCartesian", 
                                      "hoverCompareCartesian", 
                                      "autoScale2d", "autoScale2d", 
                                      "toggleSpikelines"))
  
  return (ggg)
})


output$plot_country <- renderPlotly({
  rate_country()
})

output$ui_plot_country <- renderUI({
  req(nrow(get_dataset_rates()) > 0)

  data <- get_dataset_rates()
  
  ncountries <- length(unique(data$Country))
  
  plot_height <- paste0(get_plot_height(nbars = ncountries), "px")
  
  tagList(
    plotlyOutput("plot_country", height = plot_height) %>%
      withSpinner(type = 8, size = 0.4, color = spinner_color)
  )
})

# Plot: rate by disease ------------------------------------------------

rate_disease <- reactive({

  # This redraws the graph when the Reset button has been clicked
  if (values$reset_disease_plot) {
    values$reset_disease_plot <- FALSE
  }

  data <- get_dataset_rates()
  if(is.notNull(values$country_clicked)) {
    data <- data %>%
      filter(Country == values$country_clicked)
  }

  countries <- unique(data$Country)
  if (length(countries) == 1) {
    countries_text <- paste0("Country: ", countries)
  } else {
    countries_text <- "All Countries"
  }
  
  sum_by_disease <- data %>% 
    filter(Sex == "All") %>%
    group_by(Disease) %>%
    summarize(total = sum(rate, na.rm = TRUE)) 
  
  # Highlight the bar selected
  hk <- highlight_key(sum_by_disease, ~Disease)
  
  # Alphabetical order from top
  g <- ggplot(hk, aes(x = reorder(Disease, total), y = total, key = Disease,
                      text = paste(Disease, ": ", 
                                   format(total, 
                                          big.mark = ",", 
                                          trim = TRUE, 
                                          digits = 0, 
                                          scientific = FALSE),
                                   "<br>", countries_text
                      )
  )
  ) +
    geom_bar(stat = "identity",
             position = position_stack(reverse = FALSE),
             fill = disease_color) + 
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(axis.title.y = element_text(size = 20)) +
    theme(legend.position = "none") +
    theme(axis.text.y = element_text(size = 8)) +
    aes(reorder(stringr::str_wrap(Disease, 18), total), total) +
    xlab(NULL) +
    coord_flip() 
  
  # Determine size of rangeslider
  ndiseases <- length(unique(sum_by_disease$Disease))
  pt_height <- get_plot_height(nbars = ndiseases)
  rangeslider_thickness <- rangeslider_height / pt_height
  
  gg <- g %>% 
    ggplotly(source = "disease", height = pt_height, tooltip = c("text")) %>%
    layout(margin = list(l = 50)) %>%
    rangeslider(start = 0, end = max(sum_by_disease$total, na.rm = TRUE),
                thickness = rangeslider_thickness) 

  # Highlight bar clicked
  ggg <- highlight(gg, 
                   # color = highlight_color, 
                   dynamic = FALSE, 
                   on = "plotly_click",
                   off = NULL) %>% 
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", 
                                      "lasso2d", "zoomIn2d", "zoomOut2d",
                                      "autoScale2d", "resetScale2d", 
                                      "hoverClosestCartesian", 
                                      "hoverCompareCartesian", 
                                      "autoScale2d", "autoScale2d", 
                                      "toggleSpikelines"))
  
  return (ggg)
})


output$plot_disease <- renderPlotly({

  rate_disease()
})


# Country and disease clicked -----------------------------------------------
observeEvent(event_data("plotly_click", source = "country"), {

  clicked <- event_data("plotly_click", source = "country")
  values$country_clicked <- clicked$key

  # Clear disease that was clicked
  values$disease_clicked <- NULL

  # Clear click data
  clear_click(source = "country")
})

observeEvent(event_data("plotly_click", source = "disease"), {

  clicked <- event_data("plotly_click", source = "disease")
  values$disease_clicked <- clicked$key
  
  # Clear click data
  clear_click(source = "disease")
  
})


# Dataset for Disease plot ---------------------------------------------------
# Filtered by Country clicked
data_country <- reactive({

  dataset <- get_dataset_rates() 
  
  if(is.notNull(values$country_clicked)) {
    dataset <- dataset %>% 
      filter(Country == values$country_clicked)
  }
  

  return (dataset)
})


# Dataset for small plots ---------------------------------------------------
# Filtered by Disease clicked
data_disease <- reactive ({

  # These data are numbers, not rates
  dataset <- get_dataset_numbers() 
  
  if(is.notNull(values$country_clicked)) {
    dataset <- dataset %>% 
      filter(Country == values$country_clicked)
  }
  
  if(is.notNull(values$disease_clicked)) {
    dataset <- dataset %>% 
      filter(Disease == values$disease_clicked)
  }
  
  return (dataset)
  
})

country_text <- reactive({

  data <- data_country()
  countries <- unique(data$Country)
  if (length(countries) == 1) {
    out <- paste0("Country: ", countries)
  } else {
    out <- "All Countries"
  }
  return (out)
})

disease_text <- reactive({

  data <- data_disease()
  diseases <- unique(data$Disease)
  if (length(diseases) == 1) {
    out <- paste0("Disease: ", diseases)
  } else {
    out <- "All Diseases"
  }
  return (out)
})



# Title: disease plot --------------------------------------------------------
  output$disease_plot_title <- renderUI({
    req(nrow(data_disease()) > 0)
    
    tagList(
      h4("Mortality Rate by Disease", 
         style = "text-align: center"),
      h5("(age-standardized rate per 100,000 population)", 
         style = "text-align: center"),
      h4(country_text(), 
         style = "text-align: center")
      )
  })



# SMALL PLOTS ---------------------------------------------------------------

# Title: small plots --------------------------------------------------------
  output$plot_title <- renderUI({
    req(nrow(data_disease()) > 0)

    tagList(
      h4(country_text(), style = "text-align: center"),
      h4(disease_text(), style = "text-align: center")
    )
  })


# Map ------------------------------------------------------------------------

country_codes <- readRDS("data/country_codes.Rds") 

map_codes <- reactive({
  if(is.notNull(values$country_clicked)) {
    codes <- country_codes %>% 
      filter(country == values$country_clicked)
  } else {
    codes <- country_codes
  }
  
  codes$selected <- 1
  
  return (codes)
})

draw_map <- reactive ({
  
  my_map <- joinCountryData2Map(map_codes(),
                                joinCode = "ISO3",
                                nameJoinColumn = "code")
  
  op <- palette(c("red", "yellow"))
  par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  mapCountryData(my_map, nameColumnToPlot = "selected",
                 numCats = 1,
                 catMethod = "categorical",
                 colourPalette = map_color,
                 missingCountryCol = "white",
                 addLegend = FALSE,
                 mapTitle = "")
  
})

output$world_map <- renderPlot({
  draw_map()
})


# Plot: deaths by sex -----------------------------------------------------------

numbers_sex <- reactive({

  data <- data_disease() %>% 
    filter(Sex != "All") %>%
    group_by(Sex) %>%
    summarize(total = sum(deaths, na.rm = TRUE))

  g <- ggplot(data, aes(x = Sex, y = total, fill = Sex, 
                        text = paste(Sex, ": ", 
                                     format(total, 
                                            big.mark = ",", 
                                            trim = TRUE, 
                                            digits = 0, 
                                            scientific = FALSE),
                                     "<br>", country_text(),
                                     "<br>", disease_text())
                        )
              ) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, angle = 45)
    ) +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("Female" = female_color, 
                                 "Male" = male_color)) +
    scale_y_continuous(labels = scales::comma)

  gg <- ggplotly(g, tooltip = c("text")) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c(#"zoom2d",
                                      "pan2d", "select2d",
                                      "lasso2d",
                                      "zoomIn2d", "zoomOut2d",
                                      "autoScale2d",
                                      # "resetScale2d",
                                      "hoverClosestCartesian",
                                      "hoverCompareCartesian",
                                      "toggleSpikelines"))

  return (gg)
})

output$plot_sex <- renderPlotly({
  req(nrow(get_dataset_numbers()) > 0)

  numbers_sex()
})


