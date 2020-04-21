library(shinydashboard)
# library(directlabels) # it appears plotly does not support directlabels. 
library(tidyverse)
library(lubridate)
library(conflicted)
library(plotly)
library(ggmap)
conflict_prefer("filter", "dplyr")
conflict_prefer("lead", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("box", "shinydashboard")
source("util.R")
##########
load("./data/metro.RDA")
# load("./data/ts.RDA")

### trendlines
baselines <- tibble(
    days = 1:50,
    double_every_2_days = (1 + log(2)/2)^days,
    double_every_3_days = (1 + log(2)/3)^days,
    double_every_4_days = (1 + log(2)/4)^days,
    double_every_5_days = (1 + log(2)/5)^days
) %>% 
    pivot_longer(-days, names_to = "rate", values_to = "count")

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Dashboard"),
    dashboardSidebar(        
        sidebarMenu(
            menuItem("Metropolitans Dashboard", 
                     tabName = "dashboard", 
                     icon = icon("dashboard")
                     ),
            menuItem("Maps", tabName = "maps", icon = icon("th")),
            actionButton(inputId = "reload", label = "Reload data"),
            sliderInput("top_n",
                        "Number of Metros to plot:",
                        min = 5,
                        max = 50,
                        value = 10),
            checkboxGroupInput("show_metro", strong("Select Metro to Highlight"),
                               choices = NULL,
                               selected = NULL
            )
            
        )
    ),

## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(plotlyOutput("casePlot", width = 800, height = 600)),
                        box(plotlyOutput("deathPlot", width = 800, height = 600)),
                        box(plotlyOutput("newCasePlot", width = 800, height = 600)),
                        box(plotlyOutput("newDeathPlot", width = 800, height = 600)),
                        box(DT::dataTableOutput("table1"))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "maps",
                    fluidRow(
                        box(plotlyOutput("mapPlot"), width = 800, height = 600),
                        box(DT::dataTableOutput("table"))
                    )
            )
        )
    )
)

server <- function(input, output, session) {
    metro_ts <- reactive({
        get_metro_ts(metro_fips)
    })
    observeEvent(input$reload, {
        print(paste("Updating ", format(Sys.time(), "%a %b %d %X %Y")))
        metro_ts <- reactive({
            get_metro_ts(metro_fips)
            })
    })
    metro_total <- reactive({
      metro_ts() %>% 
        group_by(metro) %>% 
        summarise(confirmed = max(confirmed),
                  deaths = max(deaths),
                  population = max(population),
                  last_updated = max(date),
                  lat = mean(lat),
                  long = mean(long)) %>% 
        arrange(desc(confirmed)) %>% 
        ungroup()
    })
    metro_background <- reactive({
        metro_ts() %>% 
            filter(metro %in% metro_total()$metro[1:input$top_n])
    })
    observeEvent(input$top_n, {
        choices <- metro_total()$metro[1:input$top_n]
        updateCheckboxGroupInput(session, "show_metro", 
                                 choices = choices,
                                 selected = choices[1])
    })
    observeEvent(metro_total, {
        choices <- metro_total()$metro[1:input$top_n]
        updateCheckboxGroupInput(session, "show_metro", 
                                 choices = choices,
                                 selected = choices[1])
    })
    output$casePlot <- renderPlotly({
        p_backgroud <- ggplot(data = metro_background(),
                              aes(x = days_from_nconfirmed, 
                                  y = confirmed)) + 
            geom_line(aes(group = metro), alpha = 0.1)  + 
            geom_point(alpha = 0.1)
        ggplotly( p_backgroud +
            geom_line(data = filter(metro_ts(), metro %in% input$show_metro),
                      aes(x = days_from_nconfirmed, 
                          y = confirmed, 
                          color = metro)) + 
            scale_y_log10() +             
            geom_line(data = baselines, 
                      aes(x = days, y = 10 * count, group = rate),
                      alpha = 0.1,
                      show.legend = FALSE) + 
            labs(title = "Confirmed Case", 
                 x = "Days from 10th confirmed case")
        ) %>% 
            plotly::layout(legend = list(orientation = "h", x = 0, y = -0.3))
    })
    output$newCasePlot <- renderPlotly({
        ggplotly( ggplot() + 
                      geom_line(data = metro_background(),
                                aes(x = days_from_nconfirmed, 
                                    y = daily_new_case, 
                                    group = metro), alpha = 0.1)  + 
                      geom_point(data = metro_background(), 
                                 aes(x = days_from_nconfirmed, y = daily_new_case),
                                 alpha = 0.1) +
                      geom_line(data = filter(metro_ts(), metro %in% input$show_metro),
                                aes(x = days_from_nconfirmed, 
                                    y = daily_new_case_ma, 
                                    color = metro)) + 
                      geom_point(data = filter(metro_ts(), metro %in% input$show_metro),
                                aes(x = days_from_nconfirmed, 
                                    y = daily_new_case, 
                                    color = metro)) + 
                      scale_y_log10() +             
                      labs(title = "Daily New Cases (7-day Moving Average", 
                           x = "Days from 10th confirmed case")
        ) %>% 
            plotly::layout(legend = list(orientation = "h", x = 0, y = -0.3))
    })
    output$deathPlot <- renderPlotly({
        dp <- metro_ts() %>% 
            filter(metro %in% metro_total()$metro[1:input$top_n]) %>% 
            ggplot(aes(x = days_from_ndeath, 
                       y = deaths, 
                       group = metro)) + 
            geom_line(alpha = 0.1)  + geom_point(alpha = 0.1) +
            geom_line(data = filter(metro_ts(), metro %in% input$show_metro),
                      aes(x = days_from_ndeath, 
                          y = deaths, 
                          color = metro)) + 
            theme(legend.position = "bottom") +
            scale_y_log10() + 
            labs(title = "Deaths", x = "Days from 3rd death")
        ggplotly(dp) %>% 
            plotly::layout(legend = list(orientation = "h", x = 0, y = -0.3))
    })
    output$newDeathPlot <- renderPlotly({
        ggplotly( ggplot() + 
                      geom_line(data = metro_background(),
                                aes(x = days_from_ndeath, 
                                    y = daily_new_death, 
                                    group = metro), alpha = 0.1)  + 
                      geom_point(data = metro_background(), 
                                 aes(x = days_from_ndeath, y = daily_new_death),
                                 alpha = 0.1) +
                      geom_line(data = filter(metro_ts(), metro %in% input$show_metro),
                                aes(x = days_from_ndeath, 
                                    y = daily_new_death_ma, 
                                    color = metro)) + 
                      geom_point(data = filter(metro_ts(), metro %in% input$show_metro),
                                aes(x = days_from_ndeath, 
                                    y = daily_new_death, 
                                    color = metro)) + 
                      scale_y_log10() +             
                      labs(title = "Daily New Deaths (7-day Moving Average)", 
                           x = "Days from 3rd death")
        ) %>% 
            plotly::layout(legend = list(orientation = "h", x = 0, y = -0.3))
    })
    output$mapPlot <- renderPlotly({

        us <- c(left = -125, bottom = 20, right = -60, top = 49)
        ggmap(get_stamenmap(us, zoom = 5, maptype = "toner-lite")) +
            geom_point( aes(x = long, y = lat, size = log(confirmed)), color = "red",
                        data = head(metro_total(), input$top_n))
            
    })
    output$table <- output$table1 <- DT::renderDataTable(DT::datatable({
        data <- head(select(metro_total(), -lat, -long),
                     max(input$top_n,30))}))
}

shinyApp(ui, server)