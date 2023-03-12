
library(shiny)
library(baseballr)
library(tidyverse)





# Data frame for all player names & IDs----------------------------------------

  people.0 <- read.csv("~/Coding stuff/people-0.csv")
  people.1 <- read.csv("~/Coding stuff/people-1.csv")
  people.2 <- read.csv("~/Coding stuff/people-2.csv")
  people.3 <- read.csv("~/Coding stuff/people-3.csv")
  people.4 <- read.csv("~/Coding stuff/people-4.csv")
  people.5 <- read.csv("~/Coding stuff/people-5.csv")
  people.6 <- read.csv("~/Coding stuff/people-6.csv")
  people.7 <- read.csv("~/Coding stuff/people-7.csv")
  people.8 <- read.csv("~/Coding stuff/people-8.csv")
  people.9 <- read.csv("~/Coding stuff/people-9.csv")
  people.a <- read.csv("~/Coding stuff/people-a.csv")
  people.b <- read.csv("~/Coding stuff/people-b.csv")
  people.c <- read.csv("~/Coding stuff/people-c.csv")
  people.d <- read.csv("~/Coding stuff/people-d.csv")
  people.e <- read.csv("~/Coding stuff/people-e.csv")
  people.f <- read.csv("~/Coding stuff/people-f.csv")


  # Binding together all above data frames and cleaning
  Names <- rbind(people.0, people.1, people.2, people.3, people.4, people.5,
               people.6, people.7, people.8, people.9, people.a, people.b,
               people.c, people.d, people.e, people.f) %>%
  filter(pro_played_last > 2015) %>% 
    filter(key_mlbam > 0, key_fangraphs > 0) %>%
  select(key_mlbam, name_first, name_last, pro_played_last) %>% na.omit()

  
  # Column for full names
  Names$Full <- paste(Names$name_first, Names$name_last, sep = " ")

# -----------------------------------------------------------------------------


  
# Coloring for pitch types in HV plot-------------------------------------------

TMcolors <- c("4-Seam Fastball" = "black",
              "Cutter" = "purple",
              "Sinker" = "#E50E00",
              "Slider" = "#4575FF",
              "Changeup" = "#009A05",
              "Split-Finger" = "#00BA88",
              "Curveball" = "orange",
              "Knuckle Curve" = "orange")

# ------------------------------------------------------------------------------
  
  
  
  
  
  
# UI----------------------------------------------------------------------------

ui <- fluidPage(
  headerPanel("Pitcher HV Plot and Heatmap"),
  sidebarPanel(
    textInput("name", label = "Pitcher Name(First Last)"),
    # selectizeInput("names", choices = NULL),
    actionButton("go", "Enter"),
    width = 3),
  sidebarPanel(
    dateRangeInput("Date", label = "Date Range(yyyy-mm-dd)",
                   format = "yyyy-mm-dd",
                   start = "2022-01-01", end = "2022-12-31",
                   min = "2015-03-01"),
    radioButtons("side", "Batter Side",
                 choices = c("All", "Right", "Left"), selected = "All"),
    width = 3
  ),
  mainPanel(
    h1("HV Plot"),
    h5("Pitcher's POV"),
    plotOutput("HV"),
    h1("Heatmap"),
    h5("Catcher's POV"),
    plotOutput("Heatmap"),
    h1("Data by pitch type"),
    dataTableOutput("Table")
  )
)
# ------------------------------------------------------------------------------





# Server------------------------------------------------------------------------

server <- function(input, output, session){
  
    # updateSelectizeInput(session, "names", label = NULL, multiple = FALSE,
                         # choices = unique(Names$Full), server = TRUE)

  
  # Splitting full name input into first and last names
  # firstname <- reactive(sapply(strsplit(input$name, " "),
  #                                       function(x) x[1]))
  # lastname <- reactive(sapply(strsplit(input$name, " "),
  #                               function(x) x[length(x)]))
  
  
  
  # Making date input into reactive function
  Date1 <- reactive(input$Date[1])
  Date2 <- reactive(input$Date[2])
  
  
  # Pulling MLBAM ID of player
  ID <- eventReactive(input$go, Names[Names$Full == input$name, 1])
  

  
  
  # Use mlbam_id and dates to load all baseball savant data

  # dataset <- reactive(scrape_statcast_savant_pitcher(start_date = Date1(),
  #                                            end_date = Date2(),
  #                                            pitcherid = ID()) %>%
  #   mutate(pfx_x = -pfx_x*12) %>% mutate(pfx_z = pfx_z*12) %>%
  #   filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
  #   !pitch_name == ""))
    
  
  
  
  # Pulling baseball savant data and filtering the data 
  # based on batter side input (pitcher facing RHH/LHH)
  
  dataset <- reactive({
      if(input$side == "Right"){
        scrape_statcast_savant_pitcher(start_date = Date1(),
                                       end_date = Date2(),
                                       pitcherid = ID()) %>%
          mutate(pfx_x = -pfx_x*12) %>% mutate(pfx_z = pfx_z*12) %>%
          filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
                 !pitch_name == "", stand == "R")
  } else if(input$side == "Left"){
    scrape_statcast_savant_pitcher(start_date = Date1(),
                                   end_date = Date2(),
                                   pitcherid = ID()) %>%
      mutate(pfx_x = -pfx_x*12) %>% mutate(pfx_z = pfx_z*12) %>%
      filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
             !pitch_name == "", stand == "L")
  } else {
    scrape_statcast_savant_pitcher(start_date = Date1(),
                                   end_date = Date2(),
                                   pitcherid = ID()) %>%
      mutate(pfx_x = -pfx_x*12) %>% mutate(pfx_z = pfx_z*12) %>%
      filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
             !pitch_name == "")
  }
})
  
  
  
  # Creating HV plot using dataset dataframe/reactive function
  
  output$HV <- renderPlot(
    ggplot(data = dataset(), aes(pfx_x, pfx_z)) +
    geom_point(aes(color = pitch_name)) +
    geom_segment(x=-30, xend=30, y=0, yend=0) +
    geom_segment(x=0, xend=0, y=-30, yend=30) +
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
    labs(x = "Horizontal Movement(in.)", y = "Vertical Movement(in.)") +
    scale_colour_manual(values = TMcolors)
  )
    
    
  output$Heatmap <- renderPlot(
    ggplot(data = dataset(), aes(plate_x, plate_z), na.rm = TRUE) + 
    facet_wrap(~ pitch_name, nrow = 1) +
    geom_density_2d_filled(na.rm = TRUE, contour_var = "ndensity", 
                           show.legend = FALSE, bins = 8) +
    coord_equal(xlim= c(-2,2), ylim = c(-1,5)) + 
    geom_segment(x=-0.71, xend=0.71, y=3.5, yend=3.5, col = "gray") + 
    geom_segment(x=-0.71, xend=0.71, y=1.5, yend=1.5, col = "gray") + 
    geom_segment(x=-0.71, xend=-0.71, y=1.5, yend=3.5, col = "gray") + 
    geom_segment(x=0.71, xend=0.71, y=1.5, yend=3.5, col = "gray")
  )
  
  

    output$Table <- renderDataTable(
     dataset() %>% group_by(pitch_name) %>% 
    summarize(
      UsagePct = round(100*(n()/nrow(dataset())), 1),                      # usage rate
      AvgVelo = round(mean(release_speed, na.rm = TRUE),1),            # average velo
      "Velo Range (max / min)" = paste(round(max(release_speed),1),    # Max and Min Velo
                                       round(min(release_speed),1), sep = " / "),
      AvgSpinRate = round(mean(release_spin_rate, na.rm = TRUE), 0),   # avg spin rate
      BU = round((AvgSpinRate/AvgVelo), 1),                            # Bauer Units
      "Avg Vert Break" = round(mean(pfx_z, na.rm = TRUE),1),           # avg vert break
      "Avg Horz Break" = round(mean(pfx_x, na.rm = TRUE),1),           # avg horz break
      wOBA = round((sum(woba_value, na.rm = TRUE)/
                      sum(woba_denom, na.rm = TRUE)), 3),              # wOBA
      "Whiff%" = round(100*(sum(description == "swinging_strike")/     # Whiff Rate
                              sum(description == "swinging_strike",
                                  description == "foul",
                                  description == "hit_into_play")), 1),
      "HH%" = round(100*(sum(launch_speed >= 95 &                      # Hard Hit Rate
                               description == "hit_into_play"
                             , na.rm = TRUE) /
                           sum(description == "hit_into_play", na.rm = TRUE)), 1)
    ) %>% arrange(desc(UsagePct))
    )
}
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
