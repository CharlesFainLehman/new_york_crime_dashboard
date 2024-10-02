library(shiny)
library(dplyr)
library(bslib)
library(bsicons)
library(plotly)

shinyOptions(bslib = T)

MI_style_plotly <- function(plot) {
  plot %>%
    layout(plot_bgcolor = 'rgba(0, 0, 0, 0)',
           paper_bgcolor = 'rgba(0, 0, 0, 0)',
           xaxis = list(title = ""),
           yaxis = list(title = "", tickformat = ","),
           font = list(family = "Le Monde Livre"),
           hoverlabel = list(font = list(family = "Le Monde Livre"))
    ) %>% 
    config(displayModeBar = FALSE) %>%
    return()
}

MI_theme = bs_theme(bg = '#f0f3ff', fg = '#20222e', base_font = "Le Monde Livre") %>%
  bs_add_rules(
    "
    .bslib-navbar .nav-link, .bslib-navbar .navbar-brand {
        padding-top: 0 !important;
        padding-bottom: 0 !important;  /* Adjusts padding for the navbar */
    }
    .navbar {
        margin-bottom: 5px !important;
        margin-top: 1px !important;
        /* Reduce space below the navbar */
    }
    "
  )

weekly_crime_counts <- read.csv("dat/weekly_crime_counts_post_processed.csv") %>%
  mutate(Date = as.Date(Date))

most_recent_week <- weekly_crime_counts %>%
  arrange(Year, Week) %>%
  slice_tail(n = 1) %>% pull(Week)

most_recent_year <- weekly_crime_counts %>%
  arrange(Year, Week) %>%
  slice_tail(n = 1) %>% pull(Year)

most_recent_date = weekly_crime_counts %>%
  arrange(Date) %>%
  slice_tail(n = 1) %>% pull(Date)

# Define the UI
ui <- page_navbar(
  
  title = div(
    class = "title-block",
    h3("Crime in New York"),
    p(paste("(Updated ", most_recent_date, ")", sep = ""), style = "font-size: 14px; margin-bottom: 0px;"),
    style = "margin-bottom: 1px;"
    ),
  
  theme = MI_theme,
  
  card(
    navset_card_tab(
      nav_panel("This Week", plotlyOutput("week")),
      nav_panel("Year to Date", plotlyOutput("ytd")),
      nav_panel("Trend (Month-to-Month)", plotlyOutput("mtm")),
      nav_panel("12-Month Rolling Sum", plotlyOutput("rs"))
    )
  ),
  
  sidebar = sidebar(
    selectInput(inputId = "offense", 
                label = tooltip(
                  trigger = list(
                    "Offense",
                    bs_icon("info-circle")
                  ),
                  "Select an offense to view",
                  placement = 'top'
                ), 
                choices = list(
                "Major Crimes" = c("Murder", "Rape", "Robbery", "Fel. Assault", "Burglary", "Gr. Larceny", "G.L.A."),
                "Minor Crimes" = c("Petit Larceny", "Misd. Assault")
                )
                ),
    sliderInput(inputId = "years",
                label = tooltip(
                  trigger = list(
                    "Years",
                    bs_icon("info-circle")
                  ),
                  "Select a range of years to view",
                  placement = 'top'
                ),
                min = 2006,
                max = 2024,
                value = c(2018, 2024), sep = ""),
  )
  
  tags$div(
    tags$img(src = "Master-MI-Black-RGB.svg", style = "position: absolute; bottom: 10px; left: 10px; width: 100px;"),
    style = "position: relative; height: 100vh;"
  )
)

# Define the server logic
server <- function(input, output) {
  
  visualized_data <- reactive({
    filter(weekly_crime_counts, 
           Offense == input$offense,
           Year >= input$years[1],
           Year <= input$years[2])
  })
  
  output$week <- renderPlotly({
    visualized_data() %>%
      filter(Week == most_recent_week) %>%
      plot_ly(x= ~Year, y= ~n, marker = list(color = "#2fa8ff")) %>%
      add_bars(hovertemplate = "%{y}<extra></extra>") %>%
      MI_style_plotly()
  })
  
  output$ytd <- renderPlotly({
    visualized_data() %>%
      filter(Week == most_recent_week) %>%
      plot_ly(x =~Year, y = ~ytd, marker = list(color = "#2fa8ff")) %>%
      add_bars(hovertemplate = "%{y}<extra></extra>") %>%
      MI_style_plotly()
  })
  
  output$mtm <- renderPlotly({
    visualized_data() %>%
      group_by(Year, Month) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      plot_ly(x = ~Date, y = ~monthly_n, line = list(color = "#2fa8ff")) %>%
      add_lines() %>%
      MI_style_plotly()
  })
  
  
  output$rs <- renderPlotly({
    visualized_data() %>% 
      filter(!is.na(rollingavg)) %>%
      plot_ly(x = ~Date, y = ~rollingavg, line = list(color = "#2fa8ff")) %>%
      add_lines() %>%
      MI_style_plotly()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
