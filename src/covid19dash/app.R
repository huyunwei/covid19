library(shinydashboard)
# library(directlabels) # it appears plotly does not support directlabels. 
library(tidyverse)
library(lubridate)
library(conflicted)
library(plotly)
library(ggmap)
# remotes::install_github("kjhealy/covdata")
# library(covdata)
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
            menuItem("Testing", tabName = "testing", icon = icon("th")),
            menuItem("Mobility", tabName = "mobil", icon = icon("th")),
            menuItem("Reproduction Numbers", 
                     tabName = "reproduction", 
                     icon = icon("th")),
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
                        box(plotlyOutput("casePlot"), width = 6),
                        box(plotlyOutput("deathPlot"), width = 6),
                        box(plotlyOutput("newCasePlot"), width = 6),
                        box(plotlyOutput("newDeathPlot"), width = 6),
                        box(DT::dataTableOutput("table1"))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "maps",
                    fluidRow(
                        box(plotOutput("mapPlot"), width = 12),
                        box(DT::dataTableOutput("table"))
                    )
            ),
            # Third tab content
            tabItem(tabName = "mobil",
                    fluidRow(
                      box(plotlyOutput("mobilPlot"), width = 12, height = 72),
                      # box(DT::dataTableOutput("table"))
                    )
            ),
            # Another tab
            tabItem(tabName = "testing",
                    fluidRow(
                      box(plotOutput("testingPlot"), width = 6),
                      box(DT::dataTableOutput("testTable"))
                    )
            ),
            # Another tab content
            tabItem(tabName = "reproduction",
                    fluidRow(
                      box(plotlyOutput("rePlot"), width = 12),
                      box(DT::dataTableOutput("reTable"))
                    )
            )
        )
    ),
)



server <- function(input, output, session) {
  apple_mobility <- reactive({
    get_appleMobility()
  })
  states_history <- reactive({
    get_covidtracking_states()
  })  
  metro_ts <- reactive({
        get_metro_ts(metro_fips)
    })
    observeEvent(input$reload, {
        # print(paste("Updating ", format(Sys.time(), "%a %b %d %X %Y")))
      apple_mobility <- reactive({
        get_appleMobility()
      })
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
    metro_rt_rki <- reactive({
      get_metro_rt_rki(metro_ts() , metro_total()$metro[1:input$top_n])
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
            geom_point(alpha = 0.1, size = 0.2)
        ggplotly( p_backgroud +
            geom_line(data = filter(metro_ts(), metro %in% input$show_metro),
                      aes(x = days_from_nconfirmed, 
                          y = confirmed, 
                          color = metro)) +
            geom_point(data = filter(metro_ts(), metro %in% input$show_metro),
                        aes(x = days_from_nconfirmed, 
                            y = confirmed, 
                            color = metro), 
                       size = 0.5 ) + 
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
                                    y = daily_new_case_ma, 
                                    group = metro), alpha = 0.05)  + 
                      geom_point(data = metro_background(), 
                                 aes(x = days_from_nconfirmed, y = daily_new_case),
                                 alpha = 0.05, size = 0.2) +
                      geom_line(data = filter(metro_ts(), metro %in% input$show_metro),
                                aes(x = days_from_nconfirmed, 
                                    y = daily_new_case_ma, 
                                    color = metro)) + 
                      geom_point(data = filter(metro_ts(), metro %in% input$show_metro),
                                aes(x = days_from_nconfirmed, 
                                    y = daily_new_case, 
                                    color = metro), 
                                size = 0.5) + 
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
          geom_line(alpha = 0.05)  + 
          geom_point(alpha = 0.05, size = 0.2) +
          geom_line(data = filter(metro_ts(), metro %in% input$show_metro),
                      aes(x = days_from_ndeath, 
                          y = deaths, 
                          color = metro)) + 
          geom_point(data = filter(metro_ts(), metro %in% input$show_metro),
                    aes(x = days_from_ndeath, 
                        y = deaths, 
                        color = metro), 
                    size = 0.5) + 
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
                                    y = daily_new_death_ma, 
                                    group = metro), 
                                alpha = 0.05)  + 
                      geom_point(data = metro_background(), 
                                 aes(x = days_from_ndeath, y = daily_new_death),
                                 alpha = 0.05, size = 0.2) +
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
    output$mapPlot <- renderPlot({

        us <- c(left = -125, bottom = 20, right = -60, top = 49)
        ggmap(get_stamenmap(us, zoom = 5, maptype = "toner-lite")) +
            geom_point( aes(x = long, y = lat, size = log(confirmed)), 
                        color = "red",
                        data = head(metro_total(), input$top_n))
            
    })
    output$table <- output$table1 <- DT::renderDataTable(DT::datatable({
        data <- head(select(metro_total(), -lat, -long),
                     max(input$top_n,30))
        })
    )
    output$testTable <- DT::renderDataTable(DT::datatable({
    data <- states_history() %>% 
        filter(state %in% c("TX", "NY", "NJ", "CT", "LA", "FL", "CO",
                            "IL","WA", "GA", "MS", "AZ"),
               date > today() - days(15)) %>% 
        mutate(pos_perc = if_else(((pos_perc > 0.99 )|(pos_perc <0.01)), 
                                  NA_real_, pos_perc)) %>% 
        select(date, state, positive, hospitalizedCurrently,
              positiveIncrease, totalTestResultsIncrease,
              pos_perc)
    })
    )        
    output$reTable <- DT::renderDataTable(DT::datatable({
      metro_rt_rki() %>% 
        group_by(metro) %>% 
        summarise(latest_Re = tail(rt, 1))
    })
    )

    output$rePlot <- renderPlotly({
      ggplotly(
      ggplot() +  
        geom_line(data = metro_rt_rki(), 
                  aes(x=days, y =rt, group = metro), 
                  alpha = 0.1) + 
        geom_hline(yintercept = 1) +
        geom_line(data = filter(metro_rt_rki(), metro %in% input$show_metro),
                  aes(x=days, y =rt, color = metro)) +
        scale_y_log10() +
        theme(legend.position = "top")
      ) %>% 
        plotly::layout(legend = list(orientation = "h", x = 0, y = -0.1))
    })        
    output$mobilPlot <- renderPlotly({
      cities <- c("Columbus", "Los Angeles", "Cleveland" ,"Atlanta" ,"Washington DC",
                  "Baltimore", "Boston", "Hartford","Indianapolis", "Chicago",
                  "Philadelphia", "San Francisco - Bay Area", "Dallas",
                  "New York City", "Denver", "Miami", "Seattle", "Houston",
                  "St. Louis", "Detroit", "New Orleans")
      ggplotly(
       apple_mobility() %>% 
         filter(geo_type == "city") %>% 
         filter(region %in% cities) %>% 
         filter(month(date) %in% c(3, 4, 5)) %>% 
         rename(mode = transportation_type) %>%
         ggplot(aes(x = date, y = index, group = mode, color = mode)) +
         geom_point(size = 0.5, alpha = 0.5) +
         # scale_color_manual(values = my.colors("bly")) +
         facet_wrap(~ region, ncol = 3) +
         labs(x = "Date", y = "Trend",
              color = "Mode",
              title = "All Modes, All Cities, Base Data",
              caption = "Data: Apple") + 
         # scale_y_log10() +
         theme(legend.position = "top") + 
         ylim(0, 150) +
         geom_smooth(se = FALSE, size = 0.3) ,
       height = 1600, width = 800
      ) %>% 
        plotly::layout(legend = list(orientation = "h", x = 0.5, y = 100))
    }) 
    output$testingPlot <- renderPlot({
      focus_states <- c("TX", "NY", "NJ", "CT", "LA", "FL", "CO",
                        "IL","WA", "GA", "MS", "AZ")
      states_history() %>% 
          filter(state %in% focus_states,
                 date > ymd(20200305)) %>% 
          mutate(pos_perc = if_else(((pos_perc > 0.99 )|(pos_perc <0.01)), NA_real_, pos_perc)) %>%   ggplot(aes(x = date)) +
          geom_point(aes(y = pos_perc), alpha = 0.5, size = 0.5) + 
          geom_smooth(aes(y = pos_perc), se = T) +
          geom_bar(aes(y = totalTestResultsIncrease / 100000), 
                   stat="identity", size=.1, 
                   fill="black", color="black", alpha=.4) +
          scale_y_continuous(
            limits = c(0,0.6),
            sec.axis = sec_axis(trans = ~ . * 100000, 
                                name="Tests Reported" ) ) + 
          ylab("Positive Rate") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.text.y = element_text(color = "blue"),
                axis.text.y.right = element_text(color = "Blue"),
                legend.position="bottom") +
          labs(title = "Daily COVID-19 Testing by State") + 
          facet_wrap(~state, ncol = 3)
    })
}

shinyApp(ui, server)