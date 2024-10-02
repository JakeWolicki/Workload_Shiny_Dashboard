library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(dashboardthemes)
library(plotly)
library(DBI)

# Default image URL
default_image_url <- "https://static.thenounproject.com/png/35224-200.png"

# Connect to PostgreSQL database
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "dbname",
                 host = "host",
                 port = port,
                 user = "user",
                 password = "password")

# Load and process data from the database
combined_data <- dbGetQuery(con, 'SELECT * FROM "combined_data_2";')

# Convert the date from Excel serial format to Date format
combined_data$Internal_Date <- as.Date(combined_data$Internal_Date, origin = "1899-12-30")

# Mapping of names to image URLs
name_to_image <- list(
  "Matthew Tkachuk" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4024854.png",
  "Aleksander Barkov" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3041970.png&w=350&h=254",
  "Sam Reinhart" = "https://a.espncdn.com/i/headshots/nhl/players/full/3114722.png",
  "Carter Verhaeghe" = "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3042088.png&w=350&h=254",
  "Gustav Forsling" = "https://a.espncdn.com/i/headshots/nhl/players/full/3151784.png",
  "Niko Mikkola" = "https://a.espncdn.com/i/headshots/nhl/players/full/3942354.png",
  "Uvis Balinskis" = "https://a.espncdn.com/i/headshots/nhl/players/full/5142461.png",
  "Anton Lundell" = "https://a.espncdn.com/i/headshots/nhl/players/full/4697395.png"
)

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "FL_Panthers_Logo_2.jpg", height = "50px"),  
      "Florida Panthers combined_data Management"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Workload", tabName = "One", icon = icon("dashboard")),
      menuItem("Team Workload", tabName = "Two", icon = icon("th"))
    ),
    collapsible = TRUE,
    selected = "One"
  ),
  
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),  # Set theme
    
    tabItems(
      tabItem(tabName = "One", # Player combined_data Tab
              fluidRow(
                box(
                  title = tags$div(style = "color: black;", "Filters"),  # Set title color to black
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 1,  
                           uiOutput("playerImage")  # Output for dynamic image based on selection
                    ),
                    column(width = 3,
                           selectInput("nameFilter", "Athlete",  
                                       choices = c("All", unique(combined_data$Internal_Name)),  
                                       selected = "All", width = "100%"
                           )
                    ),
                    column(width = 4,
                           dateRangeInput("dateRange", "Date Range",  
                                          start = NULL, end = NULL, width = "100%"
                           )
                    ),
                    column(width = 2,
                           selectInput("sessionFilter", "Session Type",  
                                       choices = c("All", unique(combined_data$Session)),  
                                       selected = "All",     
                                       width = "100%",     
                                       multiple = TRUE)  # Allow multiple selections
                    ),
                    column(width = 2,
                           uiOutput("positionUI")  # Position filter will be dynamically generated
                    )
                  )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "Readiness Score Over Time",  
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("readinessChart") 
                       )
                ),
                column(width = 6,
                       box(
                         title = "Readiness Radar Chart",  
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("radarChart") 
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = "# of High Intensity Acceleration, Deceleration, and Top Speed Over Time",  
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("highIntAccelChart") 
                       )
                )
              )
      ),
      tabItem(tabName = "Two", # Team combined_data Tab
              fluidRow(
                box(
                  title = tags$div(style = "color: black;", "Filters"),  # Set title color to black
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 4,
                           dateRangeInput("teamDateRange", "Date Range",  
                                          start = NULL, end = NULL, width = "100%"
                           )
                    ),
                    column(width = 4,
                           selectInput("teamSessionFilter", "Session Type",  
                                       choices = c("All", unique(combined_data$Session)),  
                                       selected = "All",     
                                       width = "100%",     
                                       multiple = TRUE)  # Allow multiple selections
                    ),
                    column(width = 4,
                           selectInput("teamPositionFilter", "Position",  
                                       choices = c("All", unique(combined_data$Position)),  
                                       selected = "All",     
                                       width = "100%",     
                                       multiple = TRUE)  # Static position filter
                    )
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = "# of Acceleration",  
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("accelBarChart")  
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = "# of Deceleration",  
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("decelBarChart")  
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = "Total Distances",  
                         solidHeader = TRUE,
                         width = 'auto',
                         plotlyOutput("distanceBarChart")  
                       )
                )
              )
      )
    )
  )
)












server <- function(input, output, session) {
  # Connect to the database once
  db_connection <- dbConnect(RPostgres::Postgres(),
                             dbname = 'postgres',
                             host = 'localhost',
                             port = 5432,
                             user = 'postgres',
                             password = 'Test')
  
  # Reactive function to fetch combined_data from the database
  combined_data_reactive <- reactive({
    combined_data <- dbGetQuery(db_connection, "SELECT * FROM \"combined_data_2\";")
    combined_data$Internal_Date <- as.Date(combined_data$Internal_Date, origin = "1899-12-30")
    return(combined_data)
  })
  
  # Update date range dynamically based on combined_data dataset
  observe({
    combined_data <- combined_data_reactive()
    
    # Update the date range input for player tab
    updateDateRangeInput(session, "dateRange",
                         start = min(combined_data$Internal_Date, na.rm = TRUE),
                         end = max(combined_data$Internal_Date, na.rm = TRUE),
                         min = min(combined_data$Internal_Date, na.rm = TRUE),
                         max = max(combined_data$Internal_Date, na.rm = TRUE))
    
    # Update the date range input for team tab
    updateDateRangeInput(session, "teamDateRange",
                         start = min(combined_data$Internal_Date, na.rm = TRUE),
                         end = max(combined_data$Internal_Date, na.rm = TRUE),
                         min = min(combined_data$Internal_Date, na.rm = TRUE),
                         max = max(combined_data$Internal_Date, na.rm = TRUE))
  })
  
  # Dynamically update Position filter based on selected Name
  output$positionUI <- renderUI({
    combined_data <- combined_data_reactive()
    selected_name <- input$nameFilter
    choices <- if (selected_name == "All") {
      c("All")
    } else {
      c("All", unique(combined_data$Position[combined_data$Internal_Name == selected_name]))
    }
    selectInput("positionFilter", "Position", choices = choices, selected = "All")
  })
  
  # Reactive to update the image based on name selection
  output$playerImage <- renderUI({
    selected_name <- input$nameFilter
    image_url <- if (is.null(selected_name) || selected_name == "All") {
      default_image_url
    } else {
      name_to_image[[selected_name]]
    }
    tags$img(src = image_url, height = "100px", width = "100px")
  })
















# Fixed overall average values (all set to 8) 
overall_avg_metrics <- rep(8, 6)  # Create a vector of 8's for 6 metrics 

# Create radar chart 
output$radarChart <- renderPlotly({ 
  # Filter the data based on the selected player, session, and date range 
  filtered_data <- combined_data %>% 
    filter((`Internal_Name` == input$nameFilter | input$nameFilter == "All") &  
             (`Session` %in% input$sessionFilter | "All" %in% input$sessionFilter) & 
             (`Position` == input$positionFilter | input$positionFilter == "All")) %>% 
    filter(`Internal_Date` >= input$dateRange[1] & `Internal_Date` <= input$dateRange[2]) %>% 
    select(RPE, `Sleep Quality`, `Sleep Duration`, Fatigue, Soreness, Stress) 
  
  # Get the mean for each metric for the filtered data 
  filtered_avg_metrics <- colMeans(filtered_data, na.rm = TRUE) 
  
  # Prepare data for radar chart 
  radar_data <- data.frame( 
    Metric = c("RPE", "Sleep Quality", "Sleep Duration", "Fatigue", "Soreness", "Stress"), 
    Filtered_Avg = filtered_avg_metrics, 
    Overall_Avg = overall_avg_metrics 
  ) 
  
  # Radar chart with plotly using hex colors and opacity 
  plot_ly(type = 'scatterpolar') %>% 
    add_trace( 
      r = radar_data$Overall_Avg, 
      theta = radar_data$Metric, 
      fill = 'toself', 
      name = 'Target = 8', 
      fillcolor = '#c8102E',   
      line = list(color = '#c8102E'),   
      marker = list(color = '#c8102E'),   
      opacity = 0.5  # Set opacity for target fill 
    ) %>% 
    add_trace( 
      r = radar_data$Filtered_Avg, 
      theta = radar_data$Metric, 
      fill = 'toself', 
      name = paste(input$nameFilter), 
      fillcolor = '#041E42',   
      line = list(color = '#041E42'),   
      marker = list(color = '#041E42'),   
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
  readiness_data <- combined_data %>% 
    filter((`Internal_Name` == input$nameFilter | input$nameFilter == "All") &   
             (`Session` %in% input$sessionFilter | "All" %in% input$sessionFilter) & 
             (`Position` == input$positionFilter | input$positionFilter == "All")) %>% 
    filter(`Internal_Date` >= input$dateRange[1] & `Internal_Date` <= input$dateRange[2]) %>% 
    select(`Internal_Date`, `Readiness Score`)  
  
  # Calculate average Readiness Score by Date 
  avg_readiness_data <- readiness_data %>% 
    group_by(`Internal_Date`) %>% 
    summarise(Average_Readiness_Score = mean(`Readiness Score`, na.rm = TRUE), .groups = 'drop') 
  
  # Line graph for Average Readiness Score over Time 
  plot_ly(data = avg_readiness_data, x = ~`Internal_Date`, y = ~Average_Readiness_Score, type = 'scatter',   
          mode = 'lines',  # Only show lines, no markers 
          line = list(color = '#041E42')  # Set color for the line 
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
      line = list(color = '#B9975B', width = 2)  # Customize the line appearance 
    ) 
})



# Create high intensity acceleration, deceleration count chart, and top sprint speed bar chart 
output$highIntAccelChart <- renderPlotly({ 
  # Filter data for High Intensity Acceleration, Deceleration Counts, and Top Sprint Speed 
  high_int_data <- combined_data %>% 
    filter((`Internal_Name` == input$nameFilter | input$nameFilter == "All") &   
             (`Session` %in% input$sessionFilter | "All" %in% input$sessionFilter) & 
             (`Position` == input$positionFilter | input$positionFilter == "All")) %>% 
    filter(`Internal_Date` >= input$dateRange[1] & `Internal_Date` <= input$dateRange[2]) %>% 
    select(`Internal_Date`, `High Int. Accel Count`, `High Intensity Decel Counts`, `Top Sprint Speed`)  
  
  # Calculate average counts by Date 
  avg_high_int_data <- high_int_data %>% 
    group_by(`Internal_Date`) %>% 
    summarise( 
      Average_High_Accel_Count = mean(`High Int. Accel Count`, na.rm = TRUE), 
      Average_High_Decel_Count = mean(`High Intensity Decel Counts`, na.rm = TRUE), 
      Average_Top_Sprint_Speed = mean(`Top Sprint Speed`, na.rm = TRUE),  # Average sprint speed 
      .groups = 'drop' 
    ) 
  
  # Line graph for Average High Intensity Acceleration and Deceleration Counts over Time 
  plot_ly(data = avg_high_int_data, x = ~`Internal_Date`) %>% 
    add_trace(y = ~Average_High_Accel_Count, type = 'scatter', mode = 'lines+markers',  
              line = list(color = '#041E42'),  # Color for high intensity acceleration 
              marker = list(color = '#041E42', size = 5), name = "# of High Intensity Accels.") %>% 
    add_trace(y = ~Average_High_Decel_Count, type = 'scatter', mode = 'lines+markers',  
              line = list(color = '#c8102E'),  # Color for high intensity deceleration 
              marker = list(color = '#c8102E', size = 5), name = "# of High Intensity Decels.") %>% 
    
    # Bar chart for Top Sprint Speed 
    add_trace(y = ~Average_Top_Sprint_Speed, type = 'bar',  
              marker = list(color = '#B9975B'), name = "Top Sprint Speed (MPH)", opacity = 0.6) %>% 
    
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
  filtered_data <- combined_data %>% 
    filter((`Session` %in% input$teamSessionFilter | "All" %in% input$teamSessionFilter) & 
             (`Position` %in% input$teamPositionFilter | "All" %in% input$teamPositionFilter) & 
             (`Internal_Date` >= input$teamDateRange[1] & `Internal_Date` <= input$teamDateRange[2])) %>% 
    group_by(`Internal_Name`) %>%  # Group by Name 
    summarise( 
      Med_Intensity_Accel = mean(`Med. Intensity Accel Count`, na.rm = TRUE), 
      High_Intensity_Accel = mean(`High Int. Accel Count`, na.rm = TRUE), 
      Max_Intensity_Accel = mean(`Max Int. Accel Count`, na.rm = TRUE), 
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
    y = ~`Internal_Name`,   
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
  filtered_data <- combined_data %>% 
    filter((`Session` %in% input$teamSessionFilter | "All" %in% input$teamSessionFilter) & 
             (`Position` %in% input$teamPositionFilter | "All" %in% input$teamPositionFilter) & 
             (`Internal_Date` >= input$teamDateRange[1] & `Internal_Date` <= input$teamDateRange[2])) %>% 
    group_by(`Internal_Name`) %>%  # Group by Name 
    summarise( 
      Med_Intensity_Decel = mean(`Med. Intensity Decel Count`, na.rm = TRUE), 
      High_Intensity_Decel = mean(`High Intensity Decel Counts`, na.rm = TRUE), 
      Max_Intensity_Decel = mean(`Max Intensity Decel Counts`, na.rm = TRUE), 
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
    y = ~`Internal_Name`,   
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
      yaxis = list(title = ""),   # Update y-axis title 
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
  filtered_data <- combined_data %>% 
    filter((`Session` %in% input$teamSessionFilter | "All" %in% input$teamSessionFilter) & 
             (`Position` %in% input$teamPositionFilter | "All" %in% input$teamPositionFilter) & 
             (`Internal_Date` >= input$teamDateRange[1] & `Internal_Date` <= input$teamDateRange[2])) %>% 
    group_by(`Internal_Name`) %>%  # Group by Name 
    summarise( 
      Low_Intensity_Distance = mean(`Low Intensity Distance`, na.rm = TRUE), 
      Moderate_Intensity_Distance = mean(`Moderate Intensity Distance`, na.rm = TRUE), 
      High_Intensity_Distance = mean(`High Intensity Distance`, na.rm = TRUE), 
      Sprint_Distance = mean(`Sprint Distance`, na.rm = TRUE), 
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
    y = ~`Internal_Name`,   
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
      yaxis = list(title = ""),   # Update y-axis title 
      plot_bgcolor = '#FFFFFF', 
      paper_bgcolor = '#FFFFFF', 
      legend = list( 
        orientation = 'h',  # Horizontal legend 
        x = 0.01,          # x-position of the legend 
        y = 1.1            # y-position of the legend 
      ) 
    ) 
})

}

# Run the application 
shinyApp(ui = ui, server = server)
