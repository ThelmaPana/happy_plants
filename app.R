library(shiny)
library(tidyverse)
library(lubridate)
library(DT)

source("gs4_auth.R")

## Read data ----
#gs <- "" # your google sheet link goes here

# Name of columns
names <- colnames(read_sheet(gs))

# Read table
plants_raw <- read_sheet(
  gs,
  sheet = 1,
  skip = 2,
  na = "/",
  col_names = names,
)  %>% # convert date of last watering and feeding to date objects
  mutate(
    watered_last = date(watered_last),
    fed_last = date(fed_last)
  )

# Select relevant columns
plants <- plants_raw %>% 
  select(
    name, room, # Plants info
    watered_last, fed_last, # Dates of last watering and feeding
    water_freq_summer, water_freq_winter, water_freq_mid, # watering frequencies
    food_freq_summer, food_freq_winter, food_freq_mid, # feeding frequencies
  ) %>% 
  # ignore all plants below an empty line for last date of watering or feeding
  mutate(keep = cumsum(is.na(watered_last) | is.na(fed_last))) %>% 
  filter(keep == 0) %>% 
  select(-keep)


# Make a list of rooms
room_choices <- plants_raw %>% pull(room) %>% unique() %>% sort()

# Make an educated guess for preselected season in northern hemisphere
season_names <- c("winter", "mid", "summer", "mid") # names of seasons

season_dates <-c( # end date of seasons in julian day
  yday(ymd('2020-03-20')), # end of winter
  yday(ymd('2020-06-21')), # end of spring
  yday(ymd('2020-09-22')), # end of summer
  yday(ymd('2020-12-21'))  # end of automn
  )

# find which season matches today in julian day
season_pred <- season_names[(max(which(yday(today()) > season_dates)) + 1) %% 4]


## UI ----
ui <- fluidPage(
  titlePanel("Happy Plants \U0001f331"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("season", h3("Season"),
                   choices = list(
                     "Summer ☀️" = "summer", 
                     "Winter ❄️" = "winter",
                     "Spring 🌱 / Fall 🍂 " = "mid"
                     ),
                   selected = season_pred),
      
      checkboxGroupInput("rooms", 
                         h3("Rooms"), 
                         choices = room_choices,
                         selected = room_choices),
      
      dateInput("date", 
                h3("Date"), 
                value = today())
    ),
    
    
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plants", 
                           fluidRow(
                             # Overdue plants
                             h2("Overdue Plants"),
                             column(6,
                                    h3("Thursty"),
                                    checkboxGroupInput(
                                      "to_water_past", 
                                      "",
                                      choices = plants$name %>% sort()
                                    )
                             ),
                             column(6,
                                    h3("Hungry"),
                                    checkboxGroupInput(
                                      "to_feed_past", 
                                      "",
                                      choices = plants$name
                                    )
                             ),
                           ),
                           
                           br(),
                           
                           fluidRow(
                             # Todays plants
                             h2("Today’s Plants"),
                             column(6,
                                    h3("Thursty"),
                                    checkboxGroupInput(
                                      "to_water_today", 
                                      "",
                                      choices = plants$name
                                    )
                             ),
                             column(6,
                                    h3("Hungry"),
                                    checkboxGroupInput(
                                      "to_feed_today", 
                                      "",
                                      choices = plants$name
                                    )
                             ),
                           ),
                           
                           br(),
                           
                           fluidRow(
                             # Coming days plants
                             h2("Coming soon"),
                             column(6,
                                    h3("Thursty"),
                                    checkboxGroupInput(
                                      "to_water_future", 
                                      "",
                                      choices = plants$name
                                    )
                             ),
                             column(6,
                                    h3("Hungry"),
                                    checkboxGroupInput(
                                      "to_feed_future", 
                                      "",
                                      choices = plants$name
                                    )
                             ),
                           ),
                           
                           br(),
                           
                           actionButton("done", "Done"),
                           
                           br(),
                           br(),
                           br(),
                  ),
    
                  tabPanel("Table", DTOutput("table"))
  
))))

## Server ----
server <- function(input, output, session) {
  
  # Display table
  output$table <- renderDT(
    plants_raw, options = list(scrollX = TRUE)
  )
  
  # Select plants based on room, compute next watering and feeding based on season
  df <- reactive(
    if (input$season == "summer"){
      df <- plants %>% 
      filter(room %in% input$rooms) %>% 
      mutate(
        water_next = watered_last + water_freq_summer,
        food_next = fed_last + food_freq_summer
        )%>% 
        select(name, room, water_next, food_next)
      
    } else if (input$season == "winter"){
     df <- plants %>% 
       filter(room %in% input$rooms) %>% 
       mutate(
         water_next = watered_last + water_freq_winter,
         food_next = fed_last + food_freq_winter
       )%>% 
       select(name, room, water_next, food_next)
     
    } else if (input$season == "mid"){
      df <- plants %>% 
        filter(room %in% input$rooms) %>% 
        mutate(
          water_next = watered_last + water_freq_mid,
          food_next = fed_last + food_freq_mid
        ) %>% 
        select(name, room, water_next, food_next)
    }
  )
  
  # Plants overdue for water
  over_water <- reactive(df() %>% filter(water_next < today()) %>% pull(name) %>% sort())
  # Plants overdue for food
  over_food <- reactive(df() %>% filter(food_next < today()) %>% pull(name) %>% sort())
  # Plants to water today
  today_water <- reactive(df() %>% filter(water_next == today()) %>% pull(name) %>% sort())
  # Plants to feed today
  today_food <- reactive(df() %>% filter(food_next == today()) %>% pull(name) %>% sort())
  # Plants to water in the 3 coming days
  future_water <- reactive(df() %>% filter(between(water_next, today() + days(1), today() + days(3))) %>% pull(name) %>% sort())
  # Plants to feed in the 3 coming days
  future_food <- reactive(df() %>% filter(between(food_next, today() + days(1), today() + days(3))) %>% pull(name) %>% sort())
  
  # Update check boxes according to season and room
  observe({
    # Plants overdue for water
    s_w_over <- input$to_water_past
    updateCheckboxGroupInput(
      session, "to_water_past",
      # label = paste("To Water"),
      choices = union(over_water(), over_food()), # show both plants to feed and to water
      selected = s_w_over
    )
    
    # Plants overdue for food
    s_f_over <- input$to_feed_past
    updateCheckboxGroupInput(
      session, "to_feed_past",
      # label = paste("To Feed"),
      choices = intersect(over_food(), over_water()), # show only plants which also need water
      selected = s_f_over
    )
    
    # Plants to water today
    s_w_today <- input$to_water_today
    updateCheckboxGroupInput(
      session, "to_water_today",
      # label = paste("To Water"),
      choices = union(today_water(), today_food()), # show both plants to feed and to water
      selected = s_w_today
    )
    
    # Plants to feed today
    s_f_today <- input$to_feed_today
    updateCheckboxGroupInput(
      session, "to_feed_today",
      # label = paste("To Feed"),
      choices = intersect(today_food(), today_water()), # show only plants which also need water
      selected = s_f_today
    )
    
    # Plants to water in coming days
    s_w_future <- input$to_water_future
    updateCheckboxGroupInput(
      session, "to_water_future",
      # label = paste("To Water"),
      choices = setdiff(future_water(), future_food()), # show both plants to feed and to water
      selected = s_w_future
    )
    
    # Plants to feed in coming days
    s_f_future <- input$to_feed_future
    updateCheckboxGroupInput(
      session, "to_feed_future",
      # label = paste("To Feed"),
      choices = intersect(future_food(), future_water()), # show only plants which also need water
      selected = s_f_future
    )
  })
  
  # Save when "done" button is clicked
  observeEvent(input$done,{
    # Get names of selected plants
    fed <- c(input$to_feed_past, input$to_feed_today, input$to_feed_future)
    watered <- c(input$to_water_past, input$to_water_today, input$to_water_future, fed) # add fed plants to watered plants
    
    # If no plant was watered or fed, close the app without writing in spreadsheet
    if (length(watered) + length(fed) == 0){
      stopApp()
    }
    
    # Update dates of last watering and feeding of selected plants
    plants_raw <- plants_raw %>% 
      mutate(
        watered_last = ifelse(name %in% watered | name %in% fed, input$date, watered_last),
        watered_last = as_date(watered_last),
        fed_last = ifelse(name %in% fed, input$date, fed_last),
        fed_last = as_date(fed_last),
      ) 
    
    # Find where to write update dated in google sheet
    w_i <- which(names == "watered_last")
    f_i <- which(names == "fed_last")
    col_w <- paste0(LETTERS[w_i %/% 26], LETTERS[w_i %% 26])
    col_f <- paste0(LETTERS[f_i %/% 26], LETTERS[f_i %% 26])
    
    # Write updates to google sheet
    # for watered plants
    range_write(
      plants_raw %>% select(watered_last),
      ss = gs,
      col_names = FALSE,
      range = paste0("Feuille 1!", col_w, "3:", col_w), 
      reformat = FALSE,
    )
    
    # for fed plants
    range_write(
      plants_raw %>% select(fed_last),
      ss = gs,
      col_names = FALSE,
      range = paste0("Feuille 1!", col_f, "3:", col_f), 
      reformat = FALSE,
    )
    
    # Close app when saved
    stopApp()
  })
  
}


shinyApp(ui, server)