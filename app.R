library(shiny)
library(tidyverse)
library(lubridate)
library(googlesheets4)
gs4_deauth() # spreadsheet is public, no need for authentification

### Read plant data ----
# Link to google sheet
gs <- "" # your google sheet link goes here

# Name of columns
names <- colnames(read_sheet(gs))

# Read data
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


# Compute next water and food days for summer and winter
plants <- plants_raw %>% 
  select(
    name, room, # Plants info
    watered_last, fed_last, # Dates of last watering and feeding
    water_freq_summer, water_freq_winter, food_freq_summer, food_freq_winter, # water and food frequencies
    ) %>% 
  filter(!is.na(watered_last) & !is.na(fed_last)) %>% # ignore plants for which last watering or feeding day is NA
  mutate(
    water_next_summer = watered_last + days(water_freq_summer),
    food_next_summer = fed_last + days(food_freq_summer),
    water_next_winter = watered_last + days(water_freq_winter),
    food_next_winter = fed_last + days(food_freq_winter),
  ) 

# Make a list of rooms
room_choices <- plants_raw %>% pull(room) %>% unique() %>% sort()

# Make an educated guess for preselected season in northern hemisphere
season_pred <- ifelse(quarter(today()) %in% c(1, 4), "winter", "summer") 


## UI ----
ui <- fluidPage(
  titlePanel("Happy Plants \U0001f331"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("season", h3("Season"),
                   choices = list("Summer ☀️" = "summer", "Winter ❄️" = "winter"),
                   selected = season_pred),
      
      checkboxGroupInput("rooms", 
                         h3("Rooms"), 
                         choices = room_choices,
                         selected = room_choices)
    ),
      
    mainPanel(# Overdue plants
              h2("Overdue Plants"),
              
              h3("Thursty"),
              checkboxGroupInput(
                "to_water_past", 
                "",
                choices = plants$name
              ),
              
              br(),
              
              h3("Hungry"),
              checkboxGroupInput(
                "to_feed_past", 
                "",
                choices = plants$name
              ),
              
              br(),
              
              # Today plants
              h2("Today's Plants"),
              
              h3("Thursty"),
              checkboxGroupInput(
                "to_water_today", 
                "",
                choices = plants$name
                ),
              
              br(),
              
              h3("Hungry"),
              checkboxGroupInput(
                "to_feed_today", 
                "",
                choices = plants$name
              ),
              
              br(),
              
              # Coming days plants
              h2("Coming soon"),
              
              h3("Thursty"),
              checkboxGroupInput(
                "to_water_future", 
                "",
                choices = plants$name
              ),
              
              br(),
              
              h3("Hungry"),
              checkboxGroupInput(
                "to_feed_future", 
                "",
                choices = plants$name
              ),
              
              br(),
              
              actionButton("done", "Done"),
    )
  )

)

## Server ----
server <- function(input, output, session) {
  observe({
    
    # Extract inputs
    season <- input$season
    r <- input$rooms
    w_past <- input$to_water_past
    f_past <- input$to_feed_past
    w_today <- input$to_water_today
    f_today <- input$to_feed_today
    w_future <- input$to_water_future
    f_future <- input$to_feed_future
    
    # Compute next water and food day according to selected season
    plants <- plants %>% 
      mutate(
        season = season,
        water_next = ifelse(season == "winter", water_next_winter, water_next_summer),
        water_next = as_date(water_next),
        food_next = ifelse(season == "winter", food_next_winter, food_next_summer),
        food_next = as_date(food_next),
        )
    
    ## Water overdue
    # List plants overdue for water
    plants_water_past <- plants %>% 
      filter(room %in% r) %>% # plants in selected rooms
      filter(!is.na(water_next)) %>% # ignore plants with no date for next watering
      filter(water_next < today()) %>% # plants to water today and before
      pull(name)
    
    if(is.null(plants_water_past))
      plants_water_past <- character(0)
    
    # Update selection of overdue plants for water
    updateCheckboxGroupInput(
      session, "to_water_past",
      label = paste("To Water"),
      choices = plants_water_past,
      selected = w_past
    )
    
    ## Water today
    # List plants to water today
    plants_water_today <- plants %>% 
      filter(room %in% r) %>% # plants in selected rooms
      filter(!is.na(water_next)) %>% # ignore plants with no date for next watering
      filter(water_next == today()) %>% # plants to water today
      pull(name)
    
    if(is.null(plants_water_today))
      plants_water_today <- character(0)
    
    # Update selection of today plants for water
    updateCheckboxGroupInput(
      session, "to_water_today",
      label = paste("To Water"),
      choices = plants_water_today,
      selected = w_today
    )
    
    ## Water coming soon
    # List plants to water in the three next days
    plants_water_future <- plants %>% 
      filter(room %in% r) %>% # plants in selected rooms
      filter(!is.na(water_next)) %>% # ignore plants with no date for next watering
      #filter(water_next <= today() + days(3)) %>% # plants to water today and before
      filter(between(water_next, today() + days(1), today() + days(3))) %>% # plants to water in the 3 next days
      pull(name)
    
    if(is.null(plants_water_future))
      plants_water_future <- character(0)
    
    # Update selection of future plants for water
    updateCheckboxGroupInput(
      session, "to_water_future",
      label = paste("To Water"),
      choices = plants_water_future,
      selected = w_future
    )
    
    ## Food overdue
    # List plants overdue for food
    plants_food_past <- plants %>% 
      filter(room %in% r) %>% # plants in selected rooms
      filter(!is.na(food_next)) %>% # ignore plants with no date for next feeding
      filter(food_next < today()) %>% # plants to feed before today
      pull(name)
    
    if(is.null(plants_food_past))
      plants_food_past <- character(0)
    
    # Update selection of overdue plants for food
    updateCheckboxGroupInput(
      session, "to_feed_past",
      label = paste("To Feed"),
      choices = plants_food_past,
      selected = f_past
    )
    
    ## Food today
    # List plants to feed today
    plants_food_today <- plants %>% 
      filter(room %in% r) %>% # plants in selected rooms
      filter(!is.na(food_next)) %>% # ignore plants with no date for next feeding
      filter(food_next == today()) %>% # plants to feed today
      pull(name)
    
    if(is.null(plants_food_today))
      plants_food_today <- character(0)
    
    # Update selection of today plants for food
    updateCheckboxGroupInput(
      session, "to_feed_today",
      label = paste("To Feed"),
      choices = plants_food_today,
      selected = f_today
    )
    
    ## Food coming soon
    # List plants to feed in the three next days
    plants_food_future <- plants %>% 
      filter(room %in% r) %>% # plants in selected rooms
      filter(!is.na(food_next)) %>% # ignore plants with no date for next feeding
      filter(between(food_next, today() + days(1), today() + days(3))) %>% # plants to fedd in the 3 next days
      pull(name)
    
    if(is.null(plants_food_future))
      plants_food_future <- character(0)
    
    # Update selection of future plants for food
    updateCheckboxGroupInput(
      session, "to_feed_future",
      label = paste("To Feed"),
      choices = plants_food_future,
      selected = f_future
    )
    
    ## Save
    # When the Done button is clicked, save data to google sheet
    observeEvent(input$done, {
      # Get names of selected plants
      watered <- c(input$to_water_past, input$to_water_today, input$to_water_future)
      fed <- c(input$to_feed_past, input$to_feed_today, input$to_feed_future)
      
      # If no plant was watered or fed, close the app without writing in spreadsheet
      if (length(watered) + length(fed) == 0){
        stopApp()
      }
      
      # Update dates of last watering and feeding of selected plants
      plants_raw <- plants_raw %>% 
        mutate(
          watered_last = ifelse(name %in% watered, today(), watered_last),
          watered_last = as_date(watered_last),
          fed_last = ifelse(name %in% fed, today(), fed_last),
          fed_last = as_date(fed_last),
          ) %>% 
        mutate_all(as.character) %>% # convert all columns to character
        mutate(across(everything(), ~replace_na(.x, "/"))) # replace all NA by "/"
      
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
      
      stopApp()
    })
      
  })
}


shinyApp(ui, server)