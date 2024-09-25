# Load the shiny package
library(shiny)
library(dplyr)
library(bslib)
library(tidyr)
library(zoo)
library(plotly)
theme_set(theme_MI_clear())

weekly_crime_counts <- read.csv("dat/weekly_crime_counts.csv") %>%
  group_by(Offense) %>%
  arrange(Year, Week) %>%
  mutate(rolln = rollsum(n, 52, align = 'right', fill = NA))

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
      nav_panel("This Week", plotOutput("week")),
      nav_panel("Rolling Average", plotOutput("ra"))
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
                value = c(2006, 2024))
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
      add_bars()
  })
  
  output$ra <- renderPlotly({
    plot_ly(visualized_data(), x = ~Year + (~Week - 1)/52, y= ~rolln) %>%
      add_lines()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
