library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(dashboardthemes)
library(plotly)
library(DBI)

# Default image URL
default_image_url <- "https://cdn0.iconfinder.com/data/icons/baseball-outline-2/64/baseball_player-user-boy-sports-avatar-profile-man-people-baseball_team-512.png"

# Sample connection for PostgreSQL database
# Connect to PostgreSQL database
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "dbname",
                 host = "host",
                 port = port,
                 user = "user",
                 password = "password")

# Load and process data from the database
combined_data <- dbGetQuery(con, 'SELECT * FROM "combined_data_2";')

# Convert the date from serial format to Date format
combined_data$Internal_Date <- as.Date(combined_data$Internal_Date, origin = "1899-12-30")
# end of sample PostgreSQL database connection



# Mapping of names to image URLs
name_to_image <- list(
  "Matthew Tkachuk" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4024854.png",
  "Connor McDavid" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3895074.png&w=350&h=254",
  "Connor Bedard" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5149125.png&w=350&h=254",
  "Nathan MacKinnon" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3041969.png&w=350&h=254",
  "Sidney Crosby" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3114.png&w=350&h=254",
  "Alexander Ovechkin" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3101.png&w=350&h=254",
  "Auston Matthews" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4024123.png&w=350&h=254",
  "David Pastrnak" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3114778.png&w=350&h=254",
  "Cale Makar" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4233563.png&w=350&h=254",
  "Mikko Rantanen" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3899938.png&w=350&h=254",
  "Victor Hedman" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5157.png&w=350&h=254",
  "Roman Josi" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5436.png&w=350&h=254",
  "Charlie McAvoy" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3988803.png&w=350&h=254",
  "Patrick Kane" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3735.png&w=350&h=254"
)


# Default image URL
default_image_url <- "https://static.thenounproject.com/png/35224-200.png"



# Mapping of names to image URLs - Four
name_to_image_2 <- list(
  "Sergei Bobrovsky" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5571.png&h=80&w=110&scale=crop",
  "Igor Shesterkin" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3151297.png&w=350&h=254",
  "Connor Hellebuyck" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3020225.png&w=350&h=254",
  "Jeremy Swayman" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4712036.png&w=350&h=254",
  "Linus Ullmark" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3069285.png&w=350&h=254"
)

# Default image URL
default_image_url_2 <- "https://static.thenounproject.com/png/35224-200.png"



# Mapping of names to image URLs - Five
name_to_image_3 <- list(
  "Sergei Bobrovsky" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5571.png&h=80&w=110&scale=crop",
  "Igor Shesterkin" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3151297.png&w=350&h=254",
  "Connor Hellebuyck" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3020225.png&w=350&h=254",
  "Jeremy Swayman" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4712036.png&w=350&h=254",
  "Linus Ullmark" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3069285.png&w=350&h=254"
)

# Default image URL
default_image_url_3 <- "https://static.thenounproject.com/png/35224-200.png"















# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "NHL_Logo_New.png", height = "55px"),
      tags$strong("NHL Workload Management")
    ),
    titleWidth = 400
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Skater Readiness", tabName = "One", icon = icon("dashboard")),
      menuItem("Skater Workload", tabName = "Two", icon = icon("th")),
      menuItem("Skater Distance Z Scores", tabName = "Three", icon = icon("th")),
      menuItem("Goalie Readiness", tabName = "Four", icon = icon("dashboard")),
      menuItem("Goalie Workload", tabName = "Five", icon = icon("th"))
    ),
    collapsible = TRUE,
    selected = "One"
  ),
  
  dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
    
    tabItems(
      tabItem(tabName = "One", # Player Workload Tab
              fluidRow(
                box(
                  title = tags$div(style = "color: black; font-weight: bold;", "Filters"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 1, uiOutput("playerImage")),
                    column(width = 3,
                           selectInput("nameFilter", "Athlete",
                                       choices = c("All", unique(workload$Name)),
                                       selected = "All", width = "100%")
                    ),
                    column(width = 4,
                           dateRangeInput("dateRange", "Date Range",
                                          start = NULL, end = NULL, width = "100%")
                    ),
                    column(width = 2,
                           selectInput("sessionFilter", "Session Type",
                                       choices = c("All", unique(workload$Session)),
                                       selected = "All",
                                       width = "100%",
                                       multiple = TRUE)
                    ),
                    column(width = 2, uiOutput("positionUI"))
                  )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = HTML("<b>Readiness Score Over Time</b>"),
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("readinessChart")
                       )
                ),
                column(width = 6,
                       box(
                         title = HTML("<b>Readiness Radar Chart</b>"),
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("radarChart")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = HTML("<b># of High Intensity Accelerations, Decelerations, and Top Speed Over Time</b>"),
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("highIntAccelChart")
                       )
                )
              )
      ),
      
      tabItem(tabName = "Two", # Team Workload Tab
              fluidRow(
                box(
                  title = tags$div(style = "color: black; font-weight: bold;", "Filters"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 4,
                           dateRangeInput("teamDateRange", "Date Range",
                                          start = NULL, end = NULL, width = "100%")
                    ),
                    column(width = 4,
                           selectInput("teamSessionFilter", "Session Type",
                                       choices = c("All", unique(workload$Session)),
                                       selected = "All",
                                       width = "100%",
                                       multiple = TRUE)
                    ),
                    column(width = 4,
                           selectInput("teamPositionFilter", "Position",
                                       choices = c("All", unique(workload$Position)),
                                       selected = "All",
                                       width = "100%",
                                       multiple = TRUE)
                    )
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = HTML("<b># of Accelerations</b>"),
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("accelBarChart")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = HTML("<b># of Decelerations</b>"),
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("decelBarChart")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = HTML("<b>Total Distances</b>"),
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("distanceBarChart")
                       )
                )
              )
      ),
      
      tabItem(tabName = "Three", # Team Workload Z/T Scores Tab
              fluidRow(
                box(
                  title = tags$div(style = "color: black; font-weight: bold;", "Filters"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 4,
                           dateRangeInput("ztDateRange", "Date Range",
                                          start = NULL, end = NULL, width = "100%")
                    ),
                    column(width = 4,
                           selectInput("ztSessionFilter", "Session Type",
                                       choices = c("All", unique(workload$Session)),
                                       selected = "All",
                                       width = "100%",
                                       multiple = TRUE)
                    ),
                    column(width = 4,
                           selectInput("ztPositionFilter", "Position",
                                       choices = c("All", unique(workload$Position)),
                                       selected = "All",
                                       width = "100%",
                                       multiple = TRUE)
                    ),
                    fluidRow(
                      column(width = 12,
                             box(
                               title = HTML("<b>Distance by Intensity - Z Scores</b>"),
                               status = NULL,
                               solidHeader = TRUE,
                               width = 'auto',
                               reactableOutput("ztScoreTable")  # Placeholder for the output chart
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             box(
                               title = HTML("<b>Low Intensity Distance</b>"),
                               status = NULL,
                               solidHeader = TRUE,
                               width = 'auto',
                               plotlyOutput("zLowDistScoreChart")  # Placeholder for the output chart
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             box(
                               title = HTML("<b>Moderate Intensity Distance</b>"),
                               status = NULL,
                               solidHeader = TRUE,
                               width = 'auto',
                               plotlyOutput("zModDistScoreChart")  # Placeholder for the output chart
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             box(
                               title = HTML("<b>High Intensity Distance</b>"),
                               status = NULL,
                               solidHeader = TRUE,
                               width = 'auto',
                               plotlyOutput("zHighDistScoreChart")  # Placeholder for the output chart
                             )
                      )
                    ),
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = HTML("<b>Sprint Distance</b>"),
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("ztScoreChart")
                       )
                )
              )
      ),
      
      tabItem(tabName = "Four", # Goalie Workload Tab
              fluidRow(
                box(
                  title = tags$div(style = "color: black; font-weight: bold;", "Filters"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  
                  
                  # Image column
                  column(width = 1,
                         uiOutput("goalieplayerImage")  # Player image output
                  ),
                  
                  
                  column(width = 3,
                         selectInput("goalieNameFilter2", "Goalie Name", 
                                     choices = c("All", unique(goalieworkload$Name)), 
                                     selected = "All", width = "100%")
                  ),
                  fluidRow(
                    column(width = 4,
                           dateRangeInput("GoalieReadinessDateRange", "Date Range",
                                          start = NULL, end = NULL, width = "100%")
                    ),
                    
                    column(width = 2,
                           selectInput("goalieWorkloadSessionFilter", "Session Type",
                                       choices = c("All", unique(goalieworkload$Session)),
                                       selected = "All",
                                       width = "100%",
                                       multiple = TRUE)
                    )
                    
                  )
                )
              ),
              # Row for graphs side by side
              fluidRow(
                column(width = 6,
                       box(
                         title = HTML("<b>Goalie Readiness Score Over Time</b>"),  
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("goaliereadinessChart", height = "550px")  # Output for goalie readiness score chart
                       )
                ),
                column(width = 6,
                       box(
                         title = HTML("<b>Goalie Readiness Radar Chart</b>"),  
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("goalieradarChart", height = "550px")  # Output for goalie readiness radar chart
                       )
                )
              )
      ),
      
      tabItem(tabName = "Five", # Goalie Readiness Tab
              fluidRow(
                box(
                  title = tags$div(style = "color: black; font-weight: bold;", "Filters"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 1, 
                           uiOutput("goalieplayerImage2")),
                    column(width = 3,
                           selectInput("goalieNameFilter", "Athlete",
                                       choices = c("All", unique(goalieworkload$Name)),
                                       selected = "All", width = "100%")
                    ),
                    column(width = 4,
                           dateRangeInput("GoalieWorkloadDateRange", "Date Range",
                                          start = min(goalieworkload$Date),
                                          end = max(goalieworkload$Date),
                                          width = "100%")  # `width` should be inside `dateRangeInput()`
                    ),
                    column(width = 2,
                           selectInput("goalieSessionFilter", "Session Type",
                                       choices = c("All", unique(goalieworkload$Session)),
                                       selected = "All",
                                       width = "100%",
                                       multiple = TRUE)
                    )
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = HTML("<b>Goalie Avg. Quad EMG (N) by Time In Zone (Mins)</b>"),
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("goalieWorkloadChart", height = "550px")
                       )
                )
              )
      )
    )
  )
)













# Server
server <- function(input, output, session) {
  # Update date range dynamically based on workload dataset
  observe({
    updateDateRangeInput(session, "dateRange",
                         start = min(workload$Date, na.rm = TRUE),
                         end = max(workload$Date, na.rm = TRUE),
                         min = min(workload$Date, na.rm = TRUE),
                         max = max(workload$Date, na.rm = TRUE))
  })
  
  # Update date range dynamically based on workload dataset for Team Load
  observe({
    updateDateRangeInput(session, "teamDateRange",
                         start = min(workload$Date, na.rm = TRUE),
                         end = max(workload$Date, na.rm = TRUE),
                         min = min(workload$Date, na.rm = TRUE),
                         max = max(workload$Date, na.rm = TRUE))
  })
  
  # Update date range dynamically based on workload dataset for Z/T Scores
  observe({
    updateDateRangeInput(session, "ztDateRange",
                         start = min(workload$Date, na.rm = TRUE),
                         end = max(workload$Date, na.rm = TRUE),
                         min = min(workload$Date, na.rm = TRUE),
                         max = max(workload$Date, na.rm = TRUE))
  })
  
  
  
  
  
  
  
  
  
  
  # Update date range dynamically based on workload dataset for Goalie Readiness
  observe({
    updateDateRangeInput(session, "GoalieReadinessDateRange",
                         start = min(goalieworkload$Date, na.rm = TRUE),
                         end = max(goalieworkload$Date, na.rm = TRUE),
                         min = min(goalieworkload$Date, na.rm = TRUE),
                         max = max(goalieworkload$Date, na.rm = TRUE))
  })
  
  
  
  
  
  
  
  
  # Update date range dynamically based on workload dataset for Goalie Readiness
  observe({
    updateDateRangeInput(session, "GoalieWorkloadDateRange",
                         start = min(goalieworkload$Date, na.rm = TRUE),
                         end = max(goalieworkload$Date, na.rm = TRUE),
                         min = min(goalieworkload$Date, na.rm = TRUE),
                         max = max(goalieworkload$Date, na.rm = TRUE))
  })
  
  
  
  
  
  
  # Dynamic UI for the position dropdown based on the name selected
  output$positionUI <- renderUI({
    selected_name <- input$nameFilter
    if (selected_name == "All") {
      selectInput("positionFilter", "Position",
                  choices = c("All", unique(workload$Position)),
                  selected = "All", width = "100%", multiple = TRUE)
    } else {
      position <- workload %>% filter(Name == selected_name) %>% pull(Position) %>% unique()
      selectInput("positionFilter", "Position",
                  choices = position,
                  selected = position, width = "100%", multiple = TRUE)
    }
  })
  
  
  
  
  
  
  
  
  
  
  # Display image dynamically based on athlete selection
  output$playerImage <- renderUI({
    selected_name <- input$nameFilter
    img_url <- name_to_image[[selected_name]]
    if (is.null(img_url)) {
      img_url <- default_image_url
    }
    tags$img(src = img_url, height = "100px")
  })
  
  
  
  
  # Display image dynamically based on athlete selection - Goalie Readiness - Four
  output$goalieplayerImage <- renderUI({
    selected_name <- input$goalieNameFilter2
    img_url <- name_to_image_2[[selected_name]]
    if (is.null(img_url)) {
      img_url <- default_image_url_2
    }
    tags$img(src = img_url, height = "100px")
  })
  
  
  # Display image dynamically based on athlete selection - Goalie Readiness - Five
  output$goalieplayerImage2 <- renderUI({
    selected_name <- input$goalieNameFilter
    img_url <- name_to_image_3[[selected_name]]
    if (is.null(img_url)) {
      img_url <- default_image_url_3
    }
    tags$img(src = img_url, height = "100px")
  })
  
  
  
  
  
  
  
  
  
  
  # Graphs for Goalie Readiness
  # Fixed overall average values (all set to 8)
  overall_avg_metrics <- rep(8, 6)  # Vector of 8's for 6 metrics
  
  # Create radar chart
  output$goalieradarChart <- renderPlotly({
    filtered_data <- goalieworkload %>%
      filter((Name == input$goalieNameFilter2 | input$goalieNameFilter2 == "All") &  
               (Session %in% input$goalieWorkloadSessionFilter | "All" %in% input$goalieWorkloadSessionFilter)) %>%
      filter(Date >= input$GoalieReadinessDateRange[1] & Date <= input$GoalieReadinessDateRange[2]) %>%
      select(RPE, Sleep.Quality, Sleep.Duration, Fatigue, Soreness, Stress)
    
    filtered_avg_metrics <- colMeans(filtered_data, na.rm = TRUE)
    
    radar_data <- data.frame(
      Metric = c("RPE", "Sleep Quality", "Sleep Duration", "Fatigue", "Soreness", "Stress"),
      Filtered_Avg = filtered_avg_metrics,
      Overall_Avg = overall_avg_metrics
    )
    
    
    
    
    
    
    # Determine line color based on selected goalie
    if (input$goalieNameFilter2 == "All") {
      # Default color when "All" is selected
      radar_color <- "red"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- goalieworkload %>%
        filter(Name == input$goalieNameFilter2) %>%
        select(Primary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      radar_color <- ifelse(!is.na(selected_colors$Primary.Color), selected_colors$Primary.Color, "blue")
    }
    
    
    # Determine line color based on selected goalie
    if (input$goalieNameFilter2 == "All") {
      # Default color when "All" is selected
      radar_color_2 <- "blue"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- goalieworkload %>%
        filter(Name == input$goalieNameFilter2) %>%
        select(Secondary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      radar_color_2 <- ifelse(!is.na(selected_colors$Secondary.Color), selected_colors$Secondary.Color, "red")
    }
    
    
    
    plot_ly(type = 'scatterpolar') %>%
      add_trace(
        r = radar_data$Overall_Avg,
        theta = radar_data$Metric,
        fill = 'toself',
        name = 'Target = 8',
        fillcolor = radar_color,
        line = list(color = radar_color),
        marker = list(color = radar_color),
        opacity = 0.5  # Set opacity for target fill
      ) %>%
      add_trace(
        r = radar_data$Filtered_Avg,
        theta = radar_data$Metric,
        fill = 'toself',
        name = paste(input$goalieNameFilter2),
        fillcolor = radar_color_2,
        line = list(color = radar_color_2),
        marker = list(color = radar_color_2),
        opacity = 0.5  # Set opacity for filtered average fill
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 10)),  # Adjust range
          bgcolor = '#FFFFFF'  # Set background color
        ),
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        showlegend = TRUE,
        font = list(color = 'black')
      )
  })
  
  # Readiness score chart
  output$goaliereadinessChart <- renderPlotly({
    # Apply filters for Name, Session, and Date Range
    readiness_data <- goalieworkload %>%
      filter(
        (input$goalieNameFilter2 == "All" | Name == input$goalieNameFilter2) &  # Handle "All" case properly
          (input$goalieWorkloadSessionFilter == "All" | Session %in% input$goalieWorkloadSessionFilter) &  # Handle "All" for session
          Date >= input$GoalieReadinessDateRange[1] & Date <= input$GoalieReadinessDateRange[2]  # Date range filter
      ) %>%
      select(Date, Readiness.Score)  # Select the relevant columns
    
    # If no data is available after filtering, return an empty plot
    if (nrow(readiness_data) == 0) {
      return(plot_ly() %>%
               layout(
                 title = "No data available for the selected filters",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Avg. Readiness Score")
               ))
    }
    
    # Calculate the average readiness score by Date
    avg_readiness_data <- readiness_data %>%
      group_by(Date) %>%
      summarise(Average_Readiness_Score = mean(Readiness.Score, na.rm = TRUE), .groups = 'drop')
    
    
    
    # Determine line color based on selected goalie
    if (input$goalieNameFilter2 == "All") {
      # Default color when "All" is selected
      line_color <- "red"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- goalieworkload %>%
        filter(Name == input$goalieNameFilter2) %>%
        select(Primary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      line_color <- ifelse(!is.na(selected_colors$Primary.Color), selected_colors$Primary.Color, "blue")
    }
    
    
    
    
    # Generate the plot
    plot_ly(data = avg_readiness_data, x = ~Date, y = ~Average_Readiness_Score, type = 'scatter', mode = 'lines',
            line = list(color = line_color)
    ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Avg. Readiness Score"),
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        font = list(color = 'black'),
        showlegend = FALSE
      ) %>%
      add_trace(
        y = rep(80, nrow(avg_readiness_data)),  # Add a reference line
        mode = 'lines',
        line = list(color = 'grey', width = 2)
      )
  })
  
  
  
  
  
  
  
  
  
  # Goalie Workload graph
  output$goalieWorkloadChart <- renderPlotly({
    filtered_data <- goalieworkload %>%
      # Filter by goalie name
      filter(if (input$goalieNameFilter != "All") Name == input$goalieNameFilter else TRUE) %>%
      # Filter by selected date range
      filter(Date >= input$GoalieWorkloadDateRange[1] & Date <= input$GoalieWorkloadDateRange[2]) %>%
      # Filter by selected session types
      filter(if ("All" %in% input$goalieSessionFilter) TRUE else Session %in% input$goalieSessionFilter) %>%
      # Group by Date and summarize Average EMG for both legs and Time.In.Zone
      group_by(Date) %>%
      summarise(
        Right.Leg.EMG = mean(Right.Leg.EMG, na.rm = TRUE),
        Left.Leg.EMG = mean(Left.Leg.EMG, na.rm = TRUE),
        Time.In.Zone = mean(Time.In.Zone, na.rm = TRUE),  # Add Time.In.Zone to summary
        .groups = 'drop'  # Drop the grouping after summarizing
      )
    
    
    
    
    # Determine line color based on selected goalie
    if (input$goalieNameFilter == "All") {
      # Default color when "All" is selected
      goalie_workload_chart_color <- "red"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- goalieworkload %>%
        filter(Name == input$goalieNameFilter) %>%
        select(Primary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      goalie_workload_chart_color <- ifelse(!is.na(selected_colors$Primary.Color), selected_colors$Primary.Color, "blue")
    }
    
    
    # Determine line color based on selected goalie
    if (input$goalieNameFilter == "All") {
      # Default color when "All" is selected
      goalie_workload_chart_color_2 <- "blue"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- goalieworkload %>%
        filter(Name == input$goalieNameFilter) %>%
        select(Secondary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      goalie_workload_chart_color_2 <- ifelse(!is.na(selected_colors$Secondary.Color), selected_colors$Secondary.Color, "red")
    }
    
    
    # Determine line color based on selected goalie
    if (input$goalieNameFilter == "All") {
      # Default color when "All" is selected
      goalie_workload_chart_color_3 <- "grey"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- goalieworkload %>%
        filter(Name == input$goalieNameFilter) %>%
        select(Third.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      goalie_workload_chart_color_3 <- ifelse(!is.na(selected_colors$Third.Color), selected_colors$Third.Color, "red")
    }
    
    
    # Plot the data
    plot_ly(data = filtered_data, x = ~Date) %>%
      add_trace(y = ~Right.Leg.EMG, type = 'scatter', mode = 'lines+markers', name = "Right Quad EMG (N)", 
                line = list(color = goalie_workload_chart_color), 
                marker = list(color = 'transparent', size = 4)) %>%  # Make dots transparent and smaller
      add_trace(y = ~Left.Leg.EMG, type = 'scatter', mode = 'lines+markers', name = "Left Quad EMG (N)", 
                line = list(color = goalie_workload_chart_color_2), 
                marker = list(color = 'transparent', size = 4)) %>%  # Make dots transparent and smaller
      add_trace(y = ~Time.In.Zone, type = 'bar', name = "Time In Zone (Mins)", 
                yaxis = "y2", 
                marker = list(color = goalie_workload_chart_color_3, opacity = 0.5)) %>%  # Adjust opacity for transparency
      layout(
        #title = "Average Quad EMG by Time In Zone",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Avg. EMG (N)"),
        yaxis2 = list(title = "Time In Zone (Mins)", overlaying = "y", side = "right"),  # Secondary Y-axis for Time.In.Zone
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        font = list(color = 'black'),
        showlegend = TRUE,  # Show legend for identifying lines and bars
        legend = list(
          x = 0.5,  # Center the legend horizontally
          y = -0.2,  # Position it below the chart
          xanchor = 'center',  # Anchor at the center of the legend horizontally
          yanchor = 'top',  # Anchor at the top of the legend vertically
          orientation = 'h'  # Set legend orientation to horizontal
        )
      )
  })
  
  
  
  
  
  
  
  
  
  # Reactive to update the image based on name selection
  output$playerImage <- renderUI({
    selected_name <- input$nameFilter
    
    # If no player is selected or 'All' is selected, display default image
    if (is.null(selected_name) || selected_name == "All") {
      image_url <- default_image_url
    } else {
      image_url <- name_to_image[[selected_name]]
    }
    
    tags$img(src = image_url, height = "100px", width = "100px")  # Display the image
  })
  
  
  
  
  
  
  
  # Reactive to update the image based on name selection GOALIE
  output$goalieImage <- renderUI({
    selected_goalie_name <- input$nameFilter
    
    # If no player is selected or 'All' is selected, display default image
    if (is.null(selected_goalie_name) || selected_goalie_name == "All") {
      image_url <- default_image_url
    } else {
      image_url <- name_to_image[[selected_goalie_name]]
    }
    
    tags$img(src = image_url, height = "100px", width = "100px")  # Display the image
  })
  
  
  
  # NEWWWW GOALIE 5????
  # Reactive to update the image based on name selection GOALIE
  output$goalieImage2 <- renderUI({
    selected_goalie_name <- input$nameFilter
    
    # If no player is selected or 'All' is selected, display default image
    if (is.null(selected_goalie_name) || selected_goalie_name == "All") {
      image_url <- default_image_url
    } else {
      image_url <- name_to_image[[selected_goalie_name]]
    }
    
    tags$img(src = image_url, height = "100px", width = "100px")  # Display the image
  })
  
  
  
  
  
  
  
  
  
  
  # Fixed overall average values (all set to 8)
  overall_avg_metrics <- rep(8, 6)  # Create a vector of 8's for 6 metrics
  
  # Create radar chart
  output$radarChart <- renderPlotly({
    # Filter the data based on the selected player, session, and date range
    filtered_data <- workload %>%
      filter((Name == input$nameFilter | input$nameFilter == "All") &  
               (Session %in% input$sessionFilter | "All" %in% input$sessionFilter) &
               (Position == input$positionFilter | input$positionFilter == "All")) %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
      select(RPE, Sleep.Quality, Sleep.Duration, Fatigue, Soreness, Stress)
    
    # Get the mean for each metric for the filtered data
    filtered_avg_metrics <- colMeans(filtered_data, na.rm = TRUE)
    
    # Prepare data for radar chart
    radar_data <- data.frame(
      Metric = c("RPE", "Sleep Quality", "Sleep Duration", "Fatigue", "Soreness", "Stress"),
      Filtered_Avg = filtered_avg_metrics,
      Overall_Avg = overall_avg_metrics
    )
    
    
    
    
    # Determine line color based on selected goalie
    if (input$nameFilter == "All") {
      # Default color when "All" is selected
      skating_radar_color_1 <- "red"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- workload %>%
        filter(Name == input$nameFilter) %>%
        select(Primary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      skating_radar_color_1 <- ifelse(!is.na(selected_colors$Primary.Color), selected_colors$Primary.Color, "blue")
    }
    
    
    # Determine line color based on selected goalie
    if (input$nameFilter == "All") {
      # Default color when "All" is selected
      skating_radar_color_2 <- "blue"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- workload %>%
        filter(Name == input$nameFilter) %>%
        select(Secondary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      skating_radar_color_2 <- ifelse(!is.na(selected_colors$Secondary.Color), selected_colors$Secondary.Color, "red")
    }
    
    
    
    
    # Radar chart with plotly using hex colors and opacity
    plot_ly(type = 'scatterpolar') %>%
      add_trace(
        r = radar_data$Overall_Avg,
        theta = radar_data$Metric,
        fill = 'toself',
        name = 'Target = 8',
        fillcolor = skating_radar_color_1,  
        line = list(color = skating_radar_color_1),  
        marker = list(color = skating_radar_color_1),  
        opacity = 0.5  # Set opacity for target fill
      ) %>%
      add_trace(
        r = radar_data$Filtered_Avg,
        theta = radar_data$Metric,
        fill = 'toself',
        name = paste(input$nameFilter),
        fillcolor = skating_radar_color_2,  
        line = list(color = skating_radar_color_2),  
        marker = list(color = skating_radar_color_2),  
        opacity = 0.5  # Set opacity for filtered average fill
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 10)), # Adjust range based on your data
          bgcolor = '#FFFFFF'  # Set the radar chart area background color to white
        ),
        plot_bgcolor = '#FFFFFF',  # Set plot background color to white
        paper_bgcolor = '#FFFFFF',  # Set paper background color to white
        showlegend = TRUE,
        font = list(color = 'black')  # Set text color to black
      )
  })
  
  # Create readiness chart
  output$readinessChart <- renderPlotly({
    # Filter data for Readiness Score based on selected player, session, and date range
    readiness_data <- workload %>%
      filter((Name == input$nameFilter | input$nameFilter == "All") &  
               (Session %in% input$sessionFilter | "All" %in% input$sessionFilter) &
               (Position == input$positionFilter | input$positionFilter == "All")) %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
      select(Date, Readiness.Score)  
    
    # Calculate average Readiness Score by Date
    avg_readiness_data <- readiness_data %>%
      group_by(Date) %>%
      summarise(Average_Readiness_Score = mean(Readiness.Score, na.rm = TRUE), .groups = 'drop')
    
    
    
    
    # Determine line color based on selected goalie
    if (input$nameFilter == "All") {
      # Default color when "All" is selected
      skating_readiness_color_1 <- "blue"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- workload %>%
        filter(Name == input$nameFilter) %>%
        select(Primary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      skating_readiness_color_1 <- ifelse(!is.na(selected_colors$Primary.Color), selected_colors$Primary.Color, "red")
    }
    
    
    # Line graph for Average Readiness Score over Time
    plot_ly(data = avg_readiness_data, x = ~Date, y = ~Average_Readiness_Score, type = 'scatter',  
            mode = 'lines',  # Only show lines, no markers
            line = list(color = skating_readiness_color_1)  # Set color for the line
    ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Avg. Readiness Score"),
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        font = list(color = 'black'),  # Set text color to black for light theme
        showlegend = FALSE  # Hide the legend
      ) %>%
      # Add constant line at y = 80
      add_trace(
        y = rep(80, nrow(avg_readiness_data)),  # Create a constant y value of 80 for all x values
        mode = 'lines',  # Only show the constant line, no markers
        line = list(color = 'grey', width = 2)  # Customize the line appearance
      )
  })
  
  
  
  # Create high intensity acceleration, deceleration count chart, and top sprint speed bar chart
  output$highIntAccelChart <- renderPlotly({
    # Filter data for High Intensity Acceleration, Deceleration Counts, and Top Sprint Speed
    high_int_data <- workload %>%
      filter((Name == input$nameFilter | input$nameFilter == "All") &  
               (Session %in% input$sessionFilter | "All" %in% input$sessionFilter) &
               (Position == input$positionFilter | input$positionFilter == "All")) %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
      select(Date, High.Int..Accel.Count, High.Intensity.Decel.Counts, Top.Sprint.Speed)  
    
    # Calculate average counts by Date
    avg_high_int_data <- high_int_data %>%
      group_by(Date) %>%
      summarise(
        Average_High_Accel_Count = mean(High.Int..Accel.Count, na.rm = TRUE),
        Average_High_Decel_Count = mean(High.Intensity.Decel.Counts, na.rm = TRUE),
        Average_Top_Sprint_Speed = mean(Top.Sprint.Speed, na.rm = TRUE),  # Average sprint speed
        .groups = 'drop'
      )
    
    
    
    
    # Determine line color based on selected goalie
    if (input$nameFilter == "All") {
      # Default color when "All" is selected
      skating_work_color_1 <- "blue"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- workload %>%
        filter(Name == input$nameFilter) %>%
        select(Primary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      skating_work_color_1 <- ifelse(!is.na(selected_colors$Primary.Color), selected_colors$Primary.Color, "red")
    }
    
    
    # Determine line color based on selected goalie
    if (input$nameFilter == "All") {
      # Default color when "All" is selected
      skating_work_color_2 <- "red"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- workload %>%
        filter(Name == input$nameFilter) %>%
        select(Secondary.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      skating_work_color_2 <- ifelse(!is.na(selected_colors$Secondary.Color), selected_colors$Secondary.Color, "blue")
    }
    
    # Determine line color based on selected goalie
    if (input$nameFilter == "All") {
      # Default color when "All" is selected
      skating_work_color_3 <- "red"
    } else {
      # Get the primary color from the dataset based on selected goalie
      selected_colors <- workload %>%
        filter(Name == input$nameFilter) %>%
        select(Third.Color) %>%
        slice(1)  # Select the first entry in case of duplicates
      
      skating_work_color_3 <- ifelse(!is.na(selected_colors$Third.Color), selected_colors$Third.Color, "blue")
    }
    
    
    
    # Line graph for Average High Intensity Acceleration and Deceleration Counts over Time
    plot_ly(data = avg_high_int_data, x = ~Date) %>%
      add_trace(y = ~Average_High_Accel_Count, type = 'scatter', mode = 'lines+markers', 
                line = list(color = skating_work_color_1),  # Color for high intensity acceleration
                marker = list(color = skating_work_color_1, size = 5), name = "# of High Intensity Accels.") %>%
      add_trace(y = ~Average_High_Decel_Count, type = 'scatter', mode = 'lines+markers', 
                line = list(color = skating_work_color_2),  # Color for high intensity deceleration
                marker = list(color = skating_work_color_2, size = 5), name = "# of High Intensity Decels.") %>%
      
      # Bar chart for Top Sprint Speed
      add_trace(y = ~Average_Top_Sprint_Speed, type = 'bar', 
                marker = list(color = skating_work_color_3), name = "Top Sprint Speed (MPH)", opacity = 0.6) %>%
      
      # Layout settings
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Avg. Counts / Speed"),
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        font = list(color = 'black'),  # Set text color to black
        legend = list(
          orientation = 'h',  # Horizontal legend
          x = 0.01,          # x-position of the legend
          y = 1.1            # y-position of the legend
        )
      )
  })
  
  
  # Create horizontal stacked bar chart for acceleration counts
  output$accelBarChart <- renderPlotly({
    # Filter the dataset based on user input (Date Range, Session, Position)
    filtered_data <- workload %>%
      filter((Session %in% input$teamSessionFilter | "All" %in% input$teamSessionFilter) &
               (Position %in% input$teamPositionFilter | "All" %in% input$teamPositionFilter) &
               (Date >= input$teamDateRange[1] & Date <= input$teamDateRange[2])) %>%
      group_by(Name) %>%  # Group by Name 
      summarise(
        Med_Intensity_Accel = sum(Med..Intensity.Accel.Count, na.rm = TRUE),
        High_Intensity_Accel = sum(High.Int..Accel.Count, na.rm = TRUE),
        Max_Intensity_Accel = sum(Max.Int..Accel.Count, na.rm = TRUE),
        .groups = 'drop'  # Ungroup after summarising
      ) %>%
      pivot_longer(cols = c(Med_Intensity_Accel, High_Intensity_Accel, Max_Intensity_Accel),
                   names_to = "Metric", values_to = "Count")  # Use pivot_longer
    
    # Round Count values to the nearest whole number
    filtered_data$Count <- round(filtered_data$Count)
    
    # Define the order of the metrics
    filtered_data$Metric <- factor(filtered_data$Metric, 
                                   levels = c("Med_Intensity_Accel", 
                                              "High_Intensity_Accel", 
                                              "Max_Intensity_Accel"))  
    
    # Create the horizontal stacked bar chart
    plot_ly(
      data = filtered_data,
      x = ~Count,
      y = ~Name,  
      color = ~Metric,
      colors = c("#69BE28", "#FFD700", "#FF4500"),  
      type = 'bar',
      orientation = 'h',  # Horizontal bar chart
      text = ~Count,  # Display the rounded Count values
      textposition = 'middle',  # Position the text in the middle of the bars
      textfont = list(color = 'white')  # Set text color to white
    ) %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = "Acceleration Count"),  
        yaxis = list(title = ""),  # Update y-axis title
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        legend = list(
          orientation = 'h',  # Horizontal legend
          x = 0.01,          # x-position of the legend
          y = 1.1            # y-position of the legend
        )
      )
  })
  
  
  
  # Create horizontal stacked bar chart for deceleration counts
  output$decelBarChart <- renderPlotly({
    # Filter the dataset based on user input (Date Range, Session, Position)
    filtered_data <- workload %>%
      filter((Session %in% input$teamSessionFilter | "All" %in% input$teamSessionFilter) &
               (Position %in% input$teamPositionFilter | "All" %in% input$teamPositionFilter) &
               (Date >= input$teamDateRange[1] & Date <= input$teamDateRange[2])) %>%
      group_by(Name) %>%  # Group by Name
      summarise(
        Med_Intensity_Decel = sum(Med..Intensity.Decel.Count, na.rm = TRUE),
        High_Intensity_Decel = sum(High.Intensity.Decel.Counts, na.rm = TRUE),
        Max_Intensity_Decel = sum(Max.Intensity.Decel.Counts, na.rm = TRUE),
        .groups = 'drop'  # Ungroup after summarising
      ) %>%
      pivot_longer(cols = c(Med_Intensity_Decel, High_Intensity_Decel, Max_Intensity_Decel),
                   names_to = "Metric", values_to = "Count")  # Use pivot_longer
    
    # Round Count values to the nearest whole number
    filtered_data$Count <- round(filtered_data$Count)
    
    # Define the order of the metrics
    filtered_data$Metric <- factor(filtered_data$Metric, 
                                   levels = c("Med_Intensity_Decel", 
                                              "High_Intensity_Decel", 
                                              "Max_Intensity_Decel"))  
    
    # Create the horizontal stacked bar chart
    plot_ly(
      data = filtered_data,
      x = ~Count,
      y = ~Name,  
      color = ~Metric,
      colors = c("#69BE28", "#FFD700", "#FF4500"),  
      type = 'bar',
      orientation = 'h',  # Horizontal bar chart
      text = ~Count,  # Display the rounded Count values
      textposition = 'middle',  # Position the text in the middle of the bars
      textfont = list(color = 'white')  # Set text color to white
    ) %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = "Deceleration Count"),  
        yaxis = list(title = ""),  
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        legend = list(
          orientation = 'h',  # Horizontal legend
          x = 0.01,          # x-position of the legend
          y = 1.1            # y-position of the legend
        )
      )
  })
  
  
  
  # Create horizontal stacked bar chart for distance metrics
  output$distanceBarChart <- renderPlotly({
    # Filter the dataset based on user input (Date Range, Session, Position)
    filtered_data <- workload %>%
      filter((Session %in% input$teamSessionFilter | "All" %in% input$teamSessionFilter) &
               (Position %in% input$teamPositionFilter | "All" %in% input$teamPositionFilter) &
               (Date >= input$teamDateRange[1] & Date <= input$teamDateRange[2])) %>%
      group_by(Name) %>%  # Group by Name
      summarise(
        Low_Intensity_Distance = sum(Low.Intensity.Distance, na.rm = TRUE),
        Moderate_Intensity_Distance = sum(Moderate.Intensity.Distance, na.rm = TRUE),
        High_Intensity_Distance = sum(High.Intensity.Distance, na.rm = TRUE),
        Sprint_Distance = sum(Sprint.Distance, na.rm = TRUE),
        .groups = 'drop'  # Ungroup after summarising
      ) %>%
      pivot_longer(cols = c(Low_Intensity_Distance, Moderate_Intensity_Distance, High_Intensity_Distance, Sprint_Distance),
                   names_to = "Metric", values_to = "Distance")  # Use pivot_longer
    
    # Round Distance values to the nearest whole number
    filtered_data$Distance <- round(filtered_data$Distance)
    
    # Define the order of the metrics
    filtered_data$Metric <- factor(filtered_data$Metric, 
                                   levels = c("Low_Intensity_Distance", 
                                              "Moderate_Intensity_Distance", 
                                              "High_Intensity_Distance", 
                                              "Sprint_Distance"))  
    
    # Create the horizontal stacked bar chart
    plot_ly(
      data = filtered_data,
      x = ~Distance,
      y = ~Name,  
      color = ~Metric,
      colors = c("#7CB9E8", "#69BE28", "#FFD700", "#FF4500"),  
      type = 'bar',
      orientation = 'h',  # Horizontal bar chart
      text = ~Distance,  # Display the rounded Distance values
      textposition = 'middle',  # Position the text in the middle of the bars
      textfont = list(color = 'white')  # Set text color to white
    ) %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = "Distance (Yards)"),  
        yaxis = list(title = ""),  
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        legend = list(
          orientation = 'h',  # Horizontal legend
          x = 0.01,          # x-position of the legend
          y = 1.1            # y-position of the legend
        )
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Z Scores Table 
  output$ztScoreTable <- renderReactable({
    # Print input values for debugging
    cat("Session Filter:", input$ztSessionFilter, "\n")
    cat("Position Filter:", input$ztPositionFilter, "\n")
    cat("Date Range:", input$ztDateRange, "\n")
    
    # Filter the dataset based on user input (Date Range, Session, Position) 
    filtered_data <- workload %>%
      filter(
        (input$ztSessionFilter == "All" | Session %in% input$ztSessionFilter) &
          (input$ztPositionFilter == "All" | Position %in% input$ztPositionFilter) &
          (Date >= input$ztDateRange[1] & Date <= input$ztDateRange[2])
      )
    
    # Print filtered data for debugging
    print(filtered_data)
    
    # Check if there is any data before calculating z-scores
    if (nrow(filtered_data) == 0) {
      return(NULL)  # or return a message indicating no data available
    }
    
    # Calculate overall means and standard deviations from the entire dataset
    overall_means <- workload %>%
      summarise(
        MeanSprintDistance = mean(Sprint.Distance, na.rm = TRUE),
        MeanLowIntensityDistance = mean(Low.Intensity.Distance, na.rm = TRUE),
        MeanModerateIntensityDistance = mean(Moderate.Intensity.Distance, na.rm = TRUE),
        MeanHighIntensityDistance = mean(High.Intensity.Distance, na.rm = TRUE)
      )
    
    overall_sds <- workload %>%
      summarise(
        SDSprintDistance = sd(Sprint.Distance, na.rm = TRUE),
        SDLowIntensityDistance = sd(Low.Intensity.Distance, na.rm = TRUE),
        SDModerateIntensityDistance = sd(Moderate.Intensity.Distance, na.rm = TRUE),
        SDHighIntensityDistance = sd(High.Intensity.Distance, na.rm = TRUE)
      )
    
    # Calculate Z-scores for the filtered data
    z_scores <- filtered_data %>%
      group_by(Name) %>%
      summarise(
        SprintDistance = mean(Sprint.Distance, na.rm = TRUE),
        LowIntensityDistance = mean(Low.Intensity.Distance, na.rm = TRUE),
        ModerateIntensityDistance = mean(Moderate.Intensity.Distance, na.rm = TRUE),
        HighIntensityDistance = mean(High.Intensity.Distance, na.rm = TRUE)
      ) %>%
      mutate(
        SprintDistanceZ = (SprintDistance - overall_means$MeanSprintDistance) / overall_sds$SDSprintDistance,
        LowIntensityDistanceZ = (LowIntensityDistance - overall_means$MeanLowIntensityDistance) / overall_sds$SDLowIntensityDistance,
        ModerateIntensityDistanceZ = (ModerateIntensityDistance - overall_means$MeanModerateIntensityDistance) / overall_sds$SDModerateIntensityDistance,
        HighIntensityDistanceZ = (HighIntensityDistance - overall_means$MeanHighIntensityDistance) / overall_sds$SDHighIntensityDistance
      ) %>%
      arrange(SprintDistanceZ)
    
    # Remove raw distance columns and round Z-scores to 2 decimal places 
    z_scores <- z_scores %>%
      select(Name, LowIntensityDistanceZ, ModerateIntensityDistanceZ, HighIntensityDistanceZ, SprintDistanceZ) %>%
      mutate(
        SprintDistanceZ = round(SprintDistanceZ, 2),
        LowIntensityDistanceZ = round(LowIntensityDistanceZ, 2),
        ModerateIntensityDistanceZ = round(ModerateIntensityDistanceZ, 2),
        HighIntensityDistanceZ = round(HighIntensityDistanceZ, 2)
      )
    
    # Define a function to assign colors based on Z-score values
    z_score_color <- function(value) {
      if (is.na(value)) {
        return("transparent")  # Handle NA case with transparent color
      } else if (value <= -1.5) {
        return("#DC3545")  # Red 
      } else if (value <= -1) {
        return("#FFA500")  # Orange 
      } else if (value <= -0.5) {
        return("#FFC107")  # Yellow 
      } else if (value <= -0.3) {
        return("#FFFACD")  # Lighter Yellow 
      } else if (value <= 0.3) {
        return("#FFFFFF")  # White 
      } else if (value <= 0.5) {
        return("#98FB98")  # Lighter Green 
      } else if (value <= 1) {
        return("#90EE90")  # Light Green 
      } else if (value <= 1.5) {
        return("#28A745")  # Green 
      } else {
        return("#006400")  # Dark Green 
      }
    }
    
    # Render the table with centered values 
    reactable(z_scores,
              columns = list(
                Name = colDef(name = "Player Name", align = "center"),  # Center player name
                LowIntensityDistanceZ = colDef(
                  name = "Low Intensity Distance",
                  style = function(value) {
                    list(background = z_score_color(value), color = "black")
                  },
                  align = "center"  # Center Z-scores
                ),
                ModerateIntensityDistanceZ = colDef(
                  name = "Moderate Intensity Distance",
                  style = function(value) {
                    list(background = z_score_color(value), color = "black")
                  },
                  align = "center"  # Center Z-scores
                ),
                HighIntensityDistanceZ = colDef(
                  name = "High Intensity Distance",
                  style = function(value) {
                    list(background = z_score_color(value), color = "black")
                  },
                  align = "center"  # Center Z-scores
                ),
                SprintDistanceZ = colDef(
                  name = "Sprint Distance",
                  style = function(value) {
                    list(background = z_score_color(value), color = "black")
                  },
                  align = "center"  # Center Z-scores
                )
              ),
              pagination = FALSE 
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Calculate Z-scores for Low Intensity Distance and plot for Tab 3
  output$zLowDistScoreChart <- renderPlotly({
    # Calculate overall mean and standard deviation for Low Intensity Distance across the entire dataset
    overall_mean <- mean(workload$Low.Intensity.Distance, na.rm = TRUE)
    overall_sd <- sd(workload$Low.Intensity.Distance, na.rm = TRUE)
    
    # Filter the data based on the selected date range, session type, and position
    filtered_data <- workload %>%
      filter(
        (Session %in% input$ztSessionFilter | "All" %in% input$ztSessionFilter) &
          (Position %in% input$ztPositionFilter | "All" %in% input$ztPositionFilter) &
          (Date >= input$ztDateRange[1] & Date <= input$ztDateRange[2])
      )
    
    # Check if there is any data before calculating z-scores
    if (nrow(filtered_data) == 0) {
      return(NULL)  # or return a message indicating no data available
    }
    
    # Calculate Z-scores for Low Intensity Distance by Name using overall mean and SD
    z_scores <- filtered_data %>%
      group_by(Name) %>%
      summarise(LowIntensityDistance = mean(Low.Intensity.Distance, na.rm = TRUE)) %>%
      mutate(
        LowIntensityDistanceZ = (LowIntensityDistance - overall_mean) / overall_sd
      ) %>%
      arrange(LowIntensityDistanceZ)  # Sort by Z-scores in ascending order
    
    # Reorder the Name factor based on the sorted Z-scores
    z_scores <- z_scores %>%
      mutate(Name = factor(Name, levels = Name))  # Reorder based on LowIntensityDistanceZ
    
    # Create the bar chart for Z-scores with the custom color gradient
    plot_ly(z_scores, x = ~Name, y = ~LowIntensityDistanceZ, type = 'bar',
            marker = list(color = ~LowIntensityDistanceZ,
                          colorscale = list(
                            list(0, '#DC3545'),    # Red for Z <= -1.5
                            list(0.1667, '#FFA500'),  # Orange for -1.5 < Z <= -1
                            list(0.3333, '#FFC107'),  # Yellow for -1 < Z <= -0.5
                            list(0.5, '#FFFFFF'),   # White for -0.5 < Z <= 0.5
                            list(0.6667, '#90EE90'), # Light Green for 0.5 < Z <= 1
                            list(0.8333, '#28A745'), # Green for 1 < Z <= 1.5
                            list(1, '#006400')      # Dark Green for Z > 1.5
                          ),
                          cmin = -1.5,  # Minimum Z-score value
                          cmax = 1.5,   # Maximum Z-score value
                          colorbar = list(title = "Z-Score")),
            text = ~paste(round(LowIntensityDistanceZ, 2))) %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "Z-Score"))#, range = c(-2, 2)))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  # Calculate Z-scores for Moderate Intensity Distance and plot for Tab 3
  output$zModDistScoreChart <- renderPlotly({
    # Calculate overall mean and standard deviation for Moderate Intensity Distance across the entire dataset
    overall_mean <- mean(workload$Moderate.Intensity.Distance, na.rm = TRUE)
    overall_sd <- sd(workload$Moderate.Intensity.Distance, na.rm = TRUE)
    
    # Filter the data based on the selected date range, session type, and position
    filtered_data <- workload %>%
      filter((Session %in% input$ztSessionFilter | "All" %in% input$ztSessionFilter) &
               (Position %in% input$ztPositionFilter | "All" %in% input$ztPositionFilter) &
               (Date >= input$ztDateRange[1] & Date <= input$ztDateRange[2]))
    
    # Check if the filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL to avoid errors when there's no data
    }
    
    # Calculate Z-scores for Moderate Intensity Distance by Name using the overall mean and SD
    z_scores <- filtered_data %>%
      group_by(Name) %>%
      summarise(ModerateIntensityDistance = mean(Moderate.Intensity.Distance, na.rm = TRUE)) %>%
      mutate(ModerateIntensityDistanceZ = (ModerateIntensityDistance - overall_mean) / overall_sd) %>%
      arrange(ModerateIntensityDistanceZ)  # Sort by Z-scores in ascending order
    
    # Reorder the Name factor based on the sorted Z-scores
    z_scores <- z_scores %>%
      mutate(Name = factor(Name, levels = Name))  # Reorder based on ModerateIntensityDistanceZ
    
    # Create the bar chart for Z-scores with the custom color gradient
    plot_ly(z_scores, x = ~Name, y = ~ModerateIntensityDistanceZ, type = 'bar',
            marker = list(color = ~ModerateIntensityDistanceZ,
                          colorscale = list(
                            list(0, '#DC3545'),    # Red for Z <= -1.5
                            list(0.1667, '#FFA500'),  # Orange for -1.5 < Z <= -1
                            list(0.3333, '#FFC107'),  # Yellow for -1 < Z <= -0.5
                            list(0.5, '#FFFFFF'),   # White for -0.5 < Z <= 0.5
                            list(0.6667, '#90EE90'), # Light Green for 0.5 < Z <= 1
                            list(0.8333, '#28A745'), # Green for 1 < Z <= 1.5
                            list(1, '#006400')      # Dark Green for Z > 1.5
                          ),
                          cmin = -1.5,  # Minimum Z-score value
                          cmax = 1.5,   # Maximum Z-score value
                          colorbar = list(title = "Z-Score")),
            text = ~paste(round(ModerateIntensityDistanceZ, 2))) %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "Z-Score"))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Calculate Z-scores for High Intensity Distance and plot for Tab 3
  output$zHighDistScoreChart <- renderPlotly({
    # Calculate overall mean and standard deviation for High Intensity Distance across the entire dataset
    overall_mean <- mean(workload$High.Intensity.Distance, na.rm = TRUE)
    overall_sd <- sd(workload$High.Intensity.Distance, na.rm = TRUE)
    
    # Filter the data based on the selected date range, session type, and position
    filtered_data <- workload %>%
      filter(
        (Session %in% input$ztSessionFilter | "All" %in% input$ztSessionFilter) &
          (input$ztPositionFilter == "All" | Position %in% input$ztPositionFilter) &
          (Date >= input$ztDateRange[1] & Date <= input$ztDateRange[2])
      )
    
    # Check if the filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL to avoid errors when there's no data
    }
    
    # Calculate Z-scores for High Intensity Distance by Name using the overall mean and SD
    z_scores <- filtered_data %>%
      group_by(Name) %>%
      summarise(HighIntensityDistance = mean(High.Intensity.Distance, na.rm = TRUE)) %>%
      mutate(
        HighIntensityDistanceZ = (HighIntensityDistance - overall_mean) / overall_sd
      ) %>%
      arrange(HighIntensityDistanceZ)  # Sort by Z-scores in ascending order
    
    # Reorder the Name factor based on the sorted Z-scores
    z_scores <- z_scores %>%
      mutate(Name = factor(Name, levels = Name))  # Reorder based on HighIntensityDistanceZ
    
    # Create the bar chart for Z-scores with the custom color gradient
    plot_ly(
      z_scores, 
      x = ~Name, 
      y = ~HighIntensityDistanceZ, 
      type = 'bar',
      marker = list(
        color = ~HighIntensityDistanceZ,
        colorscale = list(
          list(0, '#DC3545'),    # Red for Z <= -1.5
          list(0.1667, '#FFA500'),  # Orange for -1.5 < Z <= -1
          list(0.3333, '#FFC107'),  # Yellow for -1 < Z <= -0.5
          list(0.5, '#FFFFFF'),   # White for -0.5 < Z <= 0.5
          list(0.6667, '#90EE90'), # Light Green for 0.5 < Z <= 1
          list(0.8333, '#28A745'), # Green for 1 < Z <= 1.5
          list(1, '#006400')      # Dark Green for Z > 1.5
        ),
        cmin = -1.5,  # Minimum Z-score value
        cmax = 1.5,   # Maximum Z-score value
        colorbar = list(title = "Z-Score")
      ),
      text = ~paste(round(HighIntensityDistanceZ, 2))
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "Z-Score")
      )
  })
  
  
  
  
  
  
  
  # Calculate Z-scores for Sprint Distance and plot for Tab 3
  output$ztScoreChart <- renderPlotly({
    # Calculate overall mean and standard deviation for Sprint Distance from the entire dataset
    overall_mean <- mean(workload$Sprint.Distance, na.rm = TRUE)
    overall_sd <- sd(workload$Sprint.Distance, na.rm = TRUE)
    
    # Filter the data based on the selected date range, session type, and position
    filtered_data <- workload %>%
      filter(Date >= input$ztDateRange[1], Date <= input$ztDateRange[2]) %>%
      filter(
        (input$ztSessionFilter == "All" | Session %in% input$ztSessionFilter) &
          (input$ztPositionFilter == "All" | Position %in% input$ztPositionFilter)
      )
    
    # Check if the filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL to avoid errors when there's no data
    }
    
    # Calculate Z-scores for Sprint Distance by Name using the overall mean and sd
    z_scores <- filtered_data %>%
      group_by(Name) %>%
      summarise(SprintDistance = mean(Sprint.Distance, na.rm = TRUE)) %>%
      mutate(
        SprintDistanceZ = (SprintDistance - overall_mean) / overall_sd
      ) %>%
      arrange(SprintDistanceZ)  # Sort by Z-scores in ascending order
    
    # Reorder the Name factor based on the sorted Z-scores
    z_scores <- z_scores %>%
      mutate(Name = factor(Name, levels = Name))  # Reorder based on SprintDistanceZ
    
    # Create the bar chart for Z-scores with the custom color gradient
    plot_ly(z_scores, x = ~Name, y = ~SprintDistanceZ, type = 'bar',
            marker = list(color = ~SprintDistanceZ,
                          colorscale = list(
                            list(0, '#DC3545'),    # Red for Z <= -1.5
                            list(0.1667, '#FFA500'),  # Orange for -1.5 < Z <= -1
                            list(0.3333, '#FFC107'),  # Yellow for -1 < Z <= -0.5
                            list(0.5, '#FFFFFF'),   # White for -0.5 < Z <= 0.5
                            list(0.6667, '#90EE90'), # Light Green for 0.5 < Z <= 1
                            list(0.8333, '#28A745'), # Green for 1 < Z <= 1.5
                            list(1, '#006400')      # Dark Green for Z > 1.5
                          ),
                          cmin = -1.5,  # Minimum Z-score value
                          cmax = 1.5,   # Maximum Z-score value
                          colorbar = list(title = "Z-Score")),
            text = ~paste(round(SprintDistanceZ, 2))) %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "Z-Score"))
  })
  
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
