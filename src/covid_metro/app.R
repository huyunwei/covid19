#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(jsonlite)
library(tidyverse)
library(lubridate)
library(conflicted)
library(gghighlight)
library(shiny)
library(plotly)
conflict_prefer("filter", "dplyr")
conflict_prefer("lead", "dplyr")
source("../util.R")
##########
load("../../data/metro.RDA")
ts_us <- get_jhu_covid_usts()
metro_ts <- metro_census %>% 
    mutate(fips = as.numeric(fips)) %>% 
    select(metro, fips) %>% 
    right_join(ts_us, by = "fips") %>% 
    group_by(metro, date) %>%
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% 
    filter(!metro == "NA") %>% 
    filter(confirmed > 10) %>% 
    mutate(days_from_confirmed = row_number()) %>% 
    ungroup() 

metro_total <- metro_ts %>% 
    group_by(metro) %>% 
    summarise(last_confirmed = max(confirmed)) %>% 
    arrange(desc(last_confirmed)) %>% 
    ungroup()

print(metro_total)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("COVID-19 Dashboard - Metropolitans in USA"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("top_n",
                        "Number of Metros to plot:",
                        min = 5,
                        max = 50,
                        value = 10),
            checkboxGroupInput("show_metro", strong("Select Metro to Highlight"),
                        choices = metro_total$metro[1:10],
                        selected = metro_total$metro[1]
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("casePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$top_n,{
        choices <- metro_total$metro[1:input$top_n]
        updateCheckboxGroupInput(session, "show_metro", 
                                 choices = choices,
                                 selected = choices[1])
    })

    output$casePlot <- renderPlotly({
        p <- metro_ts %>% 
            filter(metro %in% metro_total$metro[1:input$top_n]) %>% 
            ggplot(aes(x = days_from_confirmed, 
                       y = confirmed, 
                       group = metro)) + 
            geom_line(alpha = 0.1)  + geom_point(alpha = 0.1) +
            geom_line(data = filter(metro_ts, metro %in% input$show_metro),
                      aes(x = days_from_confirmed, 
                          y = confirmed, 
                          color = metro)) + 
            theme(legend.position = "bottom") +
            scale_y_log10() + 
            labs(title = "Confirmed Case", x = "Days from 10th confirmed case")
        ggplotly(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
