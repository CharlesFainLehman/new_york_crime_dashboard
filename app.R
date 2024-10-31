library(shiny)
library(dplyr)
library(bslib)
library(bsicons)
library(plotly)

shinyOptions(bslib = T)
#load the bs theme
source("bin/MI_theme.R")

MI_style_plotly <- function(plot) {
  plot %>%
    layout(plot_bgcolor = 'rgba(0, 0, 0, 0)',
           paper_bgcolor = 'rgba(0, 0, 0, 0)',
           xaxis = list(title = ""),
           yaxis = list(title = "", tickformat = ","),
           font = list(family = "Le Monde Livre"),
           hovermode = "closest",  # Ensure the hovermode is set properly
           hoverlabel = list(font = list(family = "Le Monde Livre", color = 'black'),
                             bordercolor = 'rgba(255,255,255,0.75)',
                             bgcolor = 'rgbargba(255,255,255,0.75)')
    ) %>% 
    config(displayModeBar = FALSE) %>%
    return()
}

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
  
  theme = MI_theme,
  title = "Crime in New York",
  
  nav_panel(paste("Last Week (", strftime(most_recent_date, "%m/%d"), ")", sep = ""), plotlyOutput("week", height = 200, width = 650)),
  nav_panel("Year to Date", plotlyOutput("ytd", height = 200, width = 650)),
  nav_panel("Trend (Month-to-Month)", plotlyOutput("mtm", height = 200, width = 650)),
  nav_panel("12-Month Rolling Sum", plotlyOutput("rs", height = 200, width = 650)),
  nav_item(a(href = "https://github.com/CharlesFainLehman/new_york_crime_dashboard/blob/main/dat/weekly_crime_counts_post_processed.csv", "Get the Data", target = "_blank"), ""),
  
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
                  "Violent Crimes" = c("Murder", "Rape", "Robbery", "Fel. Assault", "Misd. Assault"),
                  "Property Crimes" = c("Burglary", "Gr. Larceny", "Gr. Lar. Auto", "Petit Larceny")
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
                value = c(2018, 2024),
                sep = ""),
    tags$text(p(tags$b("About:")),
              
              p("This dashboard lets users visualize crime in New York City between the most recently available week and 2006."),
              
              p("Users can select one of nine crimes, and view offense counts last week, year-to-date, and on a rolling basis."),
              
              p("Murder counts prior to Q3 2024 may be slightly inaccurate due to data source issues."),
    ),
    img(src="Master-MI-Black-RGB.png")
  )
)

# Define the server logic
server <- function(input, output) {
  
  visualized_data <- reactive({
    filter(weekly_crime_counts, 
           Offense == ifelse(input$offense == "Gr. Lar. Auto", "G.L.A.", input$offense),
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
      #grab the last row of each month, which is the final value for that month
      slice_tail(n = 1) %>%
      ungroup() %>%
      #drop the last entry, because the most recent month is often incomplete
      slice_head(n = nrow(.) - 1) %>%
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
