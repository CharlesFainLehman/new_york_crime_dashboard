# Load the shiny package
library(shiny)
library(dplyr)
library(bslib)
library(tidyr)
library(zoo)
library(plotly)

weekly_crime_counts <- read.csv("dat/weekly_crime_counts_post_processed.csv") %>%
  mutate(Date = as.Date(Date))

most_recent_week <- weekly_crime_counts %>%
  arrange(Year, Week) %>%
  slice_tail(n = 1) %>% pull(Week)

most_recent_year <- weekly_crime_counts %>%
  arrange(Year, Week) %>%
  slice_tail(n = 1) %>% pull(Year)

# Define the UI
ui <- page_navbar(
  title = "Crime in New York",
  card(
    navset_card_tab(
      nav_panel("This Week", plotlyOutput("week")),
      nav_panel("Year to Date", plotlyOutput("ytd")),
      nav_panel("Trend (Month-to-Month)", plotlyOutput("mtm")),
      nav_panel("Rolling Average", plotlyOutput("ra"))
    )
  ),
  sidebar = card(
    selectInput(inputId = "offense", 
                label = "Offense:", 
                unique(weekly_crime_counts$Offense)),
    sliderInput(inputId = "years",
                label = "",
                min = 2006,
                max = 2024,
                value = c(2018, 2024), sep = "")
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
      plot_ly(x= ~Year, y= ~n) %>%
      add_bars(hovertemplate = "%{y}<extra></extra>")
  })
  
  output$ytd <- renderPlotly({
    visualized_data() %>%
      filter(Week == most_recent_week) %>%
      plot_ly(x =~Year, y = ~ytd) %>%
      add_bars(hovertemplate = "%{y}<extra></extra>")
  })
  
  output$mtm <- renderPlotly({
    visualized_data() %>%
      group_by(Year, Month) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      plot_ly(x = ~Date, y = ~monthly_n) %>%
      add_lines()
  })
  
  
  output$ra <- renderPlotly({
    visualized_data() %>% 
      drop_na() %>%
      plot_ly(x = ~Date, y = ~rollingavg) %>%
      add_lines()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
