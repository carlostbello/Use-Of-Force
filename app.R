# Project for DATA 3320 by Eddie Avendano, Benediktus Albert, 
# Marlowe Hussey, Samuel Monson, Rachel Ah Nee, Tran Bui,
# Anais Barja, Mia Rodgers, Richard Pallangyo, Matthew Leusic,
# Hoang Do, Carson Vache, Carlos Bello

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(dashboardthemes)
library(leaflet)
library(shinyWidgets)
library(gridExtra)
library(leaflet.esri)
library(geojson)
library(geojsonio)
library(tigris)


# Define our main dataframe structure
raw_data <- data.frame(ID = character(),
                       Incident_Num = double(),
                       Incident_Type = character(),
                       Occured_date_time = POSIXct(),
                       Precinct = character(),
                       Sector = character(),
                       Beat = character(),
                       Officer_ID = double(),
                       Subject_ID = double(),
                       Subject_Race = character(),
                       Subject_Gender = character())

colors_force <- c("#FF101F","#D8274C","#9E488F","#6369D1")
colors_demographics <- c("#EDDB1D", "#FF540A", "#00C685", "#FAB700", "#555555", "#77D151", "#FD8605")

PAGE_SIZE <- 15000 # How big we want each page to be
index <- 0 # The last processed record

query_api <- function() {
    repeat {
        query <- paste("https://data.seattle.gov/resource/ppi5-g2bj.csv?",
                       "$order=uniqueid&",
                       "$limit=", PAGE_SIZE,
                       "&$offset=", index, sep = "")
        
        # Load a page of the dataset, also do some error handling so a network
        #  timeout during a refresh does not take out the application
        error_bit <- FALSE
        df_page <- tryCatch({ read_csv(query) },
                            error = function(err) {
                                error_bit <<- TRUE
                            })
        #df_page <- read_csv(query)
        
        # DEBUG: Shows current index and new indexed rows, -1 implies network error
        message("`df_page`: Current index is ", index, "; adding ",
                ifelse(is.data.frame(df_page), nrow(df_page), -1), " new rows")
        
        # If the current page is blank or there was an error: break the loop
        if (error_bit || nrow(df_page) == 0) {
            break
        }
        
        # Rename column to match old csv
        df_page <- df_page %>%
            rename(ID = uniqueid,
                   Incident_Num = incident_num,
                   Incident_Type = incident_type,
                   Occured_date_time = occured_date_time,
                   Precinct = precinct,
                   Sector = sector,
                   Beat = beat,
                   Officer_ID = officer_id,
                   Subject_ID = subject_id,
                   Subject_Race = subject_race,
                   Subject_Gender = subject_gender)
        
        # Bind our new data to the end of the main dataframe and remove duplicates (just incase)
        raw_data <<- distinct(rbind(raw_data, df_page), ID, .keep_all = TRUE)
        
        # Increment index to the tail of df
        index <<- nrow(raw_data)
    }
    return(raw_data)
}

derive_columns <- function(df) {
    # Create New colums for year, month, day, hour, date from POSIX date time column
    df_tidy <- df %>%
        mutate(year = year(Occured_date_time),
               month = month(Occured_date_time),
               monthNum = month(Occured_date_time, label = TRUE),
               day = wday(Occured_date_time, label = TRUE),
               hour = hour(Occured_date_time),
               date = as_date(Occured_date_time),
               weekday = strftime(Occured_date_time, format="%A", "UTC"),
               monthName = strftime(Occured_date_time, format="%B", "UTC"))
    df_tidy$Incident_Type <- recode(df_tidy$Incident_Type,
                                    "Level 1 - Use of Force" = "Level 1: Temporary Pain",
                                    "Level 2 - Use of Force" = "Level 2: Physical Injury",
                                    "Level 3 - Use of Force" = "Level 3: Substantial Injury",
                                    "Level 3 - OIS" = "Level 3 (IOS): Officer Involved Shooting")
    df_tidy$Sector <- recode(df_tidy$Sector,
                             .missing = "No Information")
    df_tidy$Subject_Race <- recode(df_tidy$Subject_Race,
                                   "Nat Hawaiian/Oth Pac Islander" = "Pacific Islander",
                                   "American Indian/Alaska Native" = "Native American")
    return(df_tidy)
}

Use_Of_Force <- derive_columns(query_api())

ForceLevels <- unique(Use_Of_Force$Incident_Type)
Beat <- unique(Use_Of_Force$Beat)
Sector<- unique(Use_Of_Force$Sector)
Precinct <- unique(Use_Of_Force$Precinct)
Race <- unique(Use_Of_Force$Subject_Race)
Gender <- unique(Use_Of_Force$Subject_Gender)
Weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")
Months <- c("January", "February", "March", "April", "May","June","July", "August","September","October","November","December")

beats <- geojsonio::geojson_read("https://opendata.arcgis.com/datasets/36378b7acb8a464c8019b9618fecd0dd_2.geojson", what = "sp")
class(beats)

# Remove this when we get colors working
num_cols <- 17
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(num_cols)

ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Use of Force in Seattle"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Filter by Beat", icon = icon("fist-raised"), startExpanded = FALSE,
                     selectInput(inputId = "precinct", 
                                 label = "Choose a precinct",
                                 choices = c("All", "North",
                                             "West", "East", "Southwest", "South"),
                                 selected = "All"),
                     
                     selectInput(inputId = "beat",
                                 label = "Choose a beat",
                                 choices = c("All", "99", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", 
                                             "D3","E1", "E2", "E3", "F1", "F2", "F3", "G1", "G2", "G3", "H1",
                                             "H2", "H3", "J1", "J2", "J3", "K1", "K2", "K3", "L1", "L2", "L3",
                                             "M1", "M2", "M3", "N1", "N2", "N3", "O1", "O2", "O3", "Q1", "Q2",
                                             "Q3", "R1", "R2", "R3", "S1", "S2", "S3", "U1", "U2", "U3", "W1",
                                             "W2", "W3"),
                                 selected = "All"),
                     
                     
                     selectInput(inputId = "time", 
                                 label = "Choose amount of time",
                                 choices = c("Last 4 quarters", "Last six months",
                                             "Last month"),
                                 selected = "Last 4 quarters")
            ),
            menuItem("Filter by Date and Time", icon = icon("calendar"), startExpanded = FALSE,
                     dateRangeInput(inputId = 'date',
                                    label = 'Filter by Date',
                                    min = min(Use_Of_Force$date), max = max(Use_Of_Force$date),
                                    width = "100%",
                                    start = min(Use_Of_Force$date),
                                    end = max(Use_Of_Force$date)
                     ),
                     checkboxGroupInput(inputId = 'months',
                                        label = "Filter by Month",
                                        choices = Months,
                                        selected = Months
                                        
                     ),
                     actionButton("resetMonths", label = "Reset"),
                     
                     checkboxGroupInput(inputId = 'days',
                                        label = "Filter by Weekday",
                                        choices = Weekdays,
                                        selected = Weekdays,
                     ), 
                     actionButton("resetWeekdays", label = "Reset"),
                     sliderInput(inputId = 'hour',
                                 label = 'Filter by Hour',
                                 min = min(Use_Of_Force$hour), 
                                 max = max(Use_Of_Force$hour),
                                 value = c(min(Use_Of_Force$hour),max(Use_Of_Force$hour)),
                                 width = "100%",
                                 step = 1
                     )
            ),
            menuItem("Filter by Demographs", icon = icon("user"), startExpanded = FALSE,
                     checkboxGroupInput(inputId = 'gender',
                                        label = "Filter by Gender",
                                        choices = Gender,
                                        selected = Gender
                     ),
                     actionButton("resetGender", label = "Reset"),
                     checkboxGroupInput(inputId = 'race',
                                        label = "Filter by Race",
                                        choices = Race,
                                        selected = Race,
                     ),
                     actionButton("resetRace", label = "Reset")
            ),
            menuItem("Filter by Region", icon = icon("globe"), startExpanded = FALSE,
                     menuItem("Filter by Precinct",  startExpanded = FALSE,
                              checkboxGroupInput(inputId = 'precinct',
                                                 label = "",
                                                 choices = Precinct,
                                                 selected = Precinct
                              ),
                              actionButton("resetPrecinct", label = "Reset")
                     ),
                     menuItem("Filter by Sector",  startExpanded = FALSE,
                              checkboxGroupInput(inputId = 'sector',
                                                 label = "",
                                                 choices = Sector,
                                                 selected = Sector
                              ),
                              actionButton("resetSector", label = "Reset")
                     ),
                     menuItem("Filter by Beat",  startExpanded = FALSE,
                              checkboxGroupInput(inputId = 'beat',
                                                 label = "",
                                                 choices = Beat,
                                                 selected = Beat,
                                                 inline =TRUE
                              ),
                              actionButton("resetBeat", label = "Reset")
                     )
            ),
            checkboxGroupInput(inputId = 'force',
                               label = "Filter by Type of Force",
                               choices = unique(Use_Of_Force$Incident_Type),
                               selected = unique(Use_Of_Force$Incident_Type)
            ),
            actionButton("resetForce", label = "Reset")
        )
    ),
    dashboardBody(
        shinyDashboardThemes(theme = "grey_dark"),
        leafletOutput('forceMap'),
        plotOutput("mapGraphs"),
        plotOutput('forceByHour'),
        plotOutput('forceByMonth'),
        fluidRow(
            box(status = "warning", plotOutput('distributionByRace')),
            box(status = "warning", plotOutput('staticDemographics'))
        ),
        fluidRow(
            box(status = "warning", plotOutput('forceByOfficer')),
            box(status = "warning", plotOutput('forceByIncident'))
        ),
        fluidRow(
            box(status = "warning", plotOutput('reportsPerOfficer'))
        ),
        fluidRow(
            box(status = "warning", plotOutput('useOfForcePie'))
        )
    )
)

server <- function(input, output, session) {
    
    generate_df <- reactive({
        invalidateLater(1.2e+6, session) # Reload every 20 minutes
        #invalidateLater(1.5e+4, session) # Reload every 15 seconds
        
        df <- derive_columns(query_api()) %>%
            filter(hour >= input$hour[1] & hour <= input$hour[2]) %>%
            filter(date >= input$date[1] & date <= input$date[2]) %>%
            filter(monthName %in% input$months) %>%
            filter(weekday %in% input$days) %>%
            filter(Incident_Type %in% input$force) %>%
            filter(Subject_Gender %in% input$gender) %>%
            filter(Subject_Race %in% input$race) %>%
            filter(Precinct %in% input$precinct) %>%
            filter(Sector %in% input$sector) %>%
            filter(Beat %in% input$beat)
        
        result <- df
    })
    
    filtered_df <- reactive({
        if (input$precinct != "All"){
            result <- df %>% filter(Precinct == input$precinct)
        } else {
            result <- df
        }
        if (input$beat != "All"){
            result <- df %>% filter(Beat == input$beat)
        } else {
            result <- df
        }
    })  
    
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Menu item", icon = icon("calendar"))
        )
    })
    output$forceByHour <- renderPlot({
        
        Use_Of_Force <- generate_df()
        
        Use_Of_Force_fbh <- Use_Of_Force %>%
            group_by(hour, Incident_Type) %>%
            summarise(count = n())
        
        p <- ggplot(data = Use_Of_Force_fbh, mapping = aes(x = hour, y = count, color = Incident_Type))
        
        p + geom_line() + 
            scale_x_continuous(breaks = seq(0,23, by = 1)) +
            labs(x = "Hour", y = "Count of Incidents") + 
            scale_color_manual(values = colors_force)
    })
    output$forceByOfficer <- renderPlot({
        
        Use_Of_Force <- generate_df()
        
        Use_Of_Force %>%
            group_by(Officer_ID) %>%
            summarize(n = n()) %>%
            ggplot(mapping = aes(x = n)) +
            geom_histogram(binwidth=2,color="#FFFFFF",fill="#077187") +
            labs(title="Number of Subjects by Officer",
                 y = "Count of Officers", x = "Number of Subjects") +
            theme_bw()
    })
    output$forceByIncident <- renderPlot({
        
        Use_Of_Force <- generate_df()
        
        officer_summary <- Use_Of_Force %>%
            group_by(Officer_ID, Incident_Num) %>%
            count() %>%
            group_by(Officer_ID) %>%
            summarize(total_subjects = sum(n), 
                      total_incidents = length(unique(Incident_Num)), 
                      mean_subjects_per_incident = mean(n)) 
        
        officer_summary %>%
            ggplot(aes(x = mean_subjects_per_incident, y = total_subjects)) + 
            geom_point(color = "#30C5FF") +
            labs(title="Number of Subjects versus Mean Incident Size by Officer",
                 x="Mean Number of Subjects per Incident", y="Number of Subjects") +
            theme_bw()
    })
    output$staticDemographics <- renderPlot({
        # Create test data.
        data <- data.frame(
            category=c("Native American", "Asian", "Black", "Pacific Islander","Hispanic", "Two or More Races", "White"),
            count=c(0.5, 15.4, 7.3, 0.3, 6.7, 6.9, 63.8)
        )
        
        # Compute percentages
        data$fraction = data$count / sum(data$count)
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax = cumsum(data$fraction)
        
        # Compute the bottom of each rectangle
        data$ymin = c(0, head(data$ymax, n=-1))
        
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Race)) +
            geom_rect() +
            coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
            annotate("text", label = "753,675", x = 2, y = 0)+
            xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
            theme_void() + ggtitle("Total Seattle Race Demographics") +
            theme(plot.title = element_text(hjust = 0.5))+
            scale_fill_manual(values = colors_demographics)
    })
    output$distributionByRace <- renderPlot({
        
        Use_Of_Force <<- generate_df()
        
        df_percentage <- Use_Of_Force %>% 
            count(Subject_Race) %>%
            mutate(Percentage = n / sum(n))
        
        
        # Create test data.
        ## New names for Race to match Race Names from Seattle Demographics Donut Chart (consistency)
        ## Exception: Two or More Races (in Seattle Demographics Donut Chart) == "Not Specified" (in Here)
        currRace <- unique(Use_Of_Force$Subject_Race)
        
        temp <- c()
        for(i in 1:length(currRace)) {
            
            y <- df_percentage$Percentage[i]
            temp <- c(temp, y)
        }
        
        data <- data.frame(
            Race = currRace,
            #Race=c("Native American", "Asian", "Black or African American", "Pacific Islander", "Hispanic or Latino", "Not Specified", "White"),
            count <-temp
            #count=c(df_percentage$Percentage[1], df_percentage$Percentage[2], df_percentage$Percentage[3], df_percentage$Percentage[5], df_percentage$Percentage[4], df_percentage$Percentage[6], df_percentage$Percentage[7])
        )
        
        # Compute percentages
        data$fraction = data$count / sum(data$count)
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax = cumsum(data$fraction)
        
        # Compute the bottom of each rectangle
        data$ymin = c(0, head(data$ymax, n=-1))
        
        # Make the plot
        ## Use of Force Demographics Donut Chart
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Race)) +
            geom_rect() +
            coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
            annotate("text", label = sum(df_percentage$n), x = 2, y = 0) +
            xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
            theme_void() +
            labs(title = "Distribution by Race (Demographics)")+
            scale_fill_manual(values = colors_demographics)
    })
    output$reportsPerOfficer <- renderPlot({
        generate_df() %>%
            mutate(Officer_ID = as.character(Officer_ID)) %>%
            count(Officer_ID) %>%
            mutate(Officer_ID = fct_reorder(Officer_ID, n, .desc = FALSE)) %>%
            top_n(25, Officer_ID) %>%
            ggplot(aes(x = n, y = Officer_ID)) +
            geom_col() +
            theme_bw() +
            labs(x = "Number of Incidents Reported", y = "Anonymized Officer Number", title="Officers With the Most Incidents")
    })
    output$useOfForcePie <- renderPlot({
        
        p2df <- generate_df() %>%
            group_by(Incident_Type) %>%
            summarise(count = n())
        
        p2df$fraction = p2df$count / sum(p2df$count)
        p2df$ymax = cumsum(p2df$fraction)
        p2df$ymin = c(0, head(p2df$ymax, n=-1))
        p2df$label <- paste0(p2df$Incident_Type, "\n percentage: ", round(100 * p2df$fraction, 2))
        p2df$labelPosition <- (p2df$ymax + p2df$ymin) / 2
        
        
        ggplot(p2df, aes(ymax = ymax, ymin = ymin,
                         xmax = (nrow(p2df) + 1), xmin = nrow(p2df),
                         fill = Incident_Type)) +
            geom_rect(labels = FALSE) +
            geom_label(x=5, aes(y=labelPosition, label=label), size=4) +
            coord_polar(theta="y") +
            theme_void() + theme(legend.position = "none") +
            labs(title = "Distribution of Incident Type") +
            scale_fill_manual(values = colors_force)
    })
    
    output$mapGraphs <- renderPlot({
        #Get the reactive dataframe
        df <- generate_df()
        
        #Make the plots
        p1 <- df %>% 
            count(Incident_Type) %>% 
            mutate(Incident_Type = fct_reorder(Incident_Type, n, .desc = TRUE)) %>% 
            ggplot(aes(x = Incident_Type, y = n, fill = Incident_Type)) +
            geom_col() +
            labs(x = "Type of Use of Force", y = "Total") +
            scale_fill_manual(values = colors_force)
        
        p2 <- df %>% 
            group_by(Subject_Gender) %>%  
            count(Incident_Type) %>% 
            mutate(Incident_Type = fct_reorder(Incident_Type, n, .desc = TRUE)) %>% 
            ggplot(aes(x = Incident_Type, y = n, fill = Subject_Gender)) +
            geom_col(position = "dodge") +
            labs(x = "Type of Use of Force", y = "Total") +
            scale_fill_manual(values = c('#ffc0cb','#ADD8E6', '#d3d3d3'))
        
        grid.arrange(p1,p2)
    })
    
    output$forceMap <- renderLeaflet({
        df <- generate_df()
        
        df_beats <- geo_join(beats, df, "beat", "Beat")
        
        inci <- df %>%
            group_by(Beat) %>%
            count(Incident_Type)
        
        pal <- colorFactor(mycolors, df_beats$beat)
        
        labels <- paste(
            "<strong>", df_beats$beat,
            "</strong><br />Total: ", inci$n
        ) %>% lapply(htmltools::HTML)
        
        df_beats %>%
            leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(lng = -122.330412, lat = 47.609056, zoom = 10) %>%
            addPolygons(fillColor = ~pal(df_beats$beat),
                        layerId = ~beat,
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        fillOpacity = 0.2,
                        highlight = highlightOptions(weight = 3,
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        popup = ~labels,
                        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                    textsize = "15px",
                                                    direction = "auto"))
        
    })
    output$incidentTypeDistribution <- renderPlot({
        generate_df() %>% group_by(hour, Incident_Type) %>%
            summarise(count = n()) %>% ggplot(aes(x = hour, y = count, fill = Incident_Type)) + geom_col() + 
            scale_x_continuous(breaks = seq(0,23, by = 1)) +
            labs(title = "Distribution of Incident Type over Time", x = "Hour", y = "Count of Incidents") + 
            scale_fill_manual(values = colors_force)
    })
    output$forceByMonth <- renderPlot({
        generate_df() %>%
            group_by(month, Incident_Type) %>%
            summarise(count = n()) %>% 
            ggplot(aes(x = month, y = count, color = Incident_Type)) + geom_line() + 
            scale_x_continuous(breaks = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
            labs(title = "Incident Trends over months", x = "Month", y = "Count of Incidents") +
            scale_color_manual(values = colors_force)
    })
    observeEvent(
        input$resetMonths, {
            if (is.null(input$months) == TRUE) {
                updateCheckboxGroupInput(session, "months", choices = Months, selected = Months)
                updateActionButton(session, "resetMonths", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "months", choices = Months, selected = NULL)
                updateActionButton(session, "resetMonths", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$months) == TRUE) {
            updateActionButton(session, "resetMonths", label = "Select All")
        } else {
            updateActionButton(session, "resetMonths", label = "Reset")
        }
    })
    observeEvent(
        input$resetWeekdays, {
            if (is.null(input$days) == TRUE) {
                updateCheckboxGroupInput(session, "days", choices = Weekdays, selected = Weekdays)
                updateActionButton(session, "resetWeekdays", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "days", choices = Weekdays, selected = NULL)
                updateActionButton(session, "resetWeekdays", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$days) == TRUE) {
            updateActionButton(session, "resetWeekdays", label = "Select All")
        } else {
            updateActionButton(session, "resetWeekdays", label = "Reset")
        }
    })
    observeEvent(
        input$resetGender, {
            if (is.null(input$gender) == TRUE) {
                updateCheckboxGroupInput(session, "gender", choices = Gender, selected = Gender)
                updateActionButton(session, "resetGender", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "gender", choices = Gender, selected = NULL)
                updateActionButton(session, "resetGender", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$gender) == TRUE) {
            updateActionButton(session, "resetGender", label = "Select All")
        } else {
            updateActionButton(session, "resetGender", label = "Reset")
        }
    })
    observeEvent(
        input$resetRace, {
            if (is.null(input$race) == TRUE) {
                updateCheckboxGroupInput(session, "race", choices = Race, selected = Race)
                updateActionButton(session, "resetRace", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "race", choices = Race, selected = NULL)
                updateActionButton(session, "resetRace", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$race) == TRUE) {
            updateActionButton(session, "resetRace", label = "Select All")
        } else {
            updateActionButton(session, "resetRace", label = "Reset")
        }
    })
    observeEvent(
        input$resetPrecinct, {
            if (is.null(input$precinct) == TRUE) {
                updateCheckboxGroupInput(session, "precinct", choices = Precinct, selected = Precinct)
                updateActionButton(session, "resetPrecinct", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "precinct", choices = Precinct, selected = NULL)
                updateActionButton(session, "resetPrecinct", label = "Select All")
            }
        }        
    )
    observe({
        if (is.null(input$precinct) == TRUE) {
            updateActionButton(session, "resetPrecinct", label = "Select All")
        } else {
            updateActionButton(session, "resetPrecinct", label = "Reset")
        }
    })
    observeEvent(
        input$resetSector, {
            if (is.null(input$sector) == TRUE) {
                updateCheckboxGroupInput(session, "sector", choices = Sector, selected = Sector)
                updateActionButton(session, "resetSector", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "sector", choices = Sector, selected = NULL)
                updateActionButton(session, "resetSector", label = "Select All")
            }
        }        
    )
    observe({
        if (is.null(input$sector) == TRUE) {
            updateActionButton(session, "resetSector", label = "Select All")
        } else {
            updateActionButton(session, "resetSector", label = "Reset")
        }
    })
    observeEvent(
        input$resetBeat, {
            if (is.null(input$beat) == TRUE) {
                updateCheckboxGroupInput(session, "beat", choices = Beat, selected = Beat, inline =TRUE)
                updateActionButton(session, "resetBeat", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "beat", choices = Beat, selected = NULL, inline =TRUE)
                updateActionButton(session, "resetBeat", label = "Select All")
            }
        }        
    )
    observe({
        if (is.null(input$beat) == TRUE) {
            updateActionButton(session, "resetBeat", label = "Select All")
        } else {
            updateActionButton(session, "resetBeat", label = "Reset")
        }
    })
    observeEvent(
        input$resetForce, {
            if (is.null(input$force) == TRUE) {
                updateCheckboxGroupInput(session, "force", choices = ForceLevels, selected = ForceLevels)
                updateActionButton(session, "resetForce", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "force", choices = ForceLevels, selected = NULL)
                updateActionButton(session, "resetForce", label = "Select All")
            }
        }
        
    )
    observe({
        if (is.null(input$force) == TRUE) {
            updateActionButton(session, "resetForce", label = "Select All")
        } else {
            updateActionButton(session, "resetForce", label = "Reset")
        }
    })
    observe({ 
        event <- input$map_shape_click
        print(event$id)
        
        updateSelectInput(inputId = "beat",
                          selected = event$id)
    })
    
}

shinyApp(ui, server)