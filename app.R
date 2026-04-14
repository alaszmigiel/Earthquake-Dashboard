library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Wczytanie danych z pliku CSV
quakes <- read_csv("all_quakes.csv")

# Pobranie danych o kontynentach
world <- ne_countries(scale = "medium", returnclass = "sf")[, c("continent", "geometry")]

# Funkcja przypisująca trzęsieniu ziemi odpowiedni kontynent na podstawie współrzędnych
assign_continent <- function(lat, lon) {
  points <- st_as_sf(data.frame(lon = lon, lat = lat),
                     coords = c("lon", "lat"), crs = 4326)
  joined <- st_join(points, world, join = st_within)
  ifelse(is.na(joined$continent), "Ocean", joined$continent)
}

# Definicja interfejsu użytkownika
ui <- dashboardPage(
  dashboardHeader(title = "Earthquake Dashboard"),
  
  # Pasek boczny z nawigacją
  dashboardSidebar(
    sidebarMenu(
      menuItem("Earthquakes", tabName = "earthquakes", icon = icon("globe")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Główna część aplikacji z zakładkami
  dashboardBody(
    tags$head(
      # Dodanie stylu CSS
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") 
    ),
    tabItems(
      
      # Zakładka z analizami
      tabItem(tabName = "earthquakes",
              fluidRow(
                column(width = 4,
                       # Filtry daty i magnitudy + licznik trzęsień
                       box(title = "Filters", status = "info", solidHeader = TRUE, width = 12,
                           dateRangeInput("date_range", "Date range:",
                                          start = max(quakes$time) - 7*24*60*60,
                                          end = max(quakes$time),
                                          min = min(quakes$time),
                                          max = max(quakes$time)),
                           sliderInput("mag_range", "Magnitude:",
                                       min = floor(min(quakes$mag, na.rm = TRUE)),
                                       max = ceiling(max(quakes$mag, na.rm = TRUE)),
                                       value = c(3.5, 8), step = 0.1),
                           tags$div(style = "text-align: right; font-weight: bold;",
                                    "Total earthquakes: ", textOutput("n_quakes", inline = TRUE))
                       ),
                       # Najsilniejsze trzęsienie
                       box(title = "Strongest Earthquake", status = "info", solidHeader = TRUE, width = 12,
                           htmlOutput("max_quake"))
                ),
                column(width = 8,
                       # Mapa trzęsień ziemi
                       box(title = "Earthquake Map", status = "info", solidHeader = TRUE, width = 12,
                           leafletOutput("quake_map", height = 375))
                )
              ),
              fluidRow(
                column(width = 6,
                       # Histogram magnitud
                       box(title = "Magnitude Histogram", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("mag_hist", height = 250))
                ),
                column(width = 6,
                       # Liczba trzęsień dziennie
                       box(title = "Daily Earthquakes", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("daily_plot", height = 250))
                )
              ),
              fluidRow(
                column(width = 5,
                       # Wykres kołowy – trzęsienia według kontynentu
                       box(title = "Earthquakes by Continent", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("continent_pie", height = 250)),
                       # Tabela – kraje z największą liczbą trzęsień
                       box(title = "Countries with the most Earthquakes",
                           status = "info", solidHeader = TRUE, width = 12,
                           div(style = "display: flex; justify-content: center;",
                               tableOutput("top_countries_by_quakes"))
                       )
                ),
                column(width = 7,
                       # Zależność głębokość vs magnituda
                       box(title = "Depth vs. Magnitude", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("depth_vs_mag", height = 540))
                )
              )
      ),
      
      # Zakładka z tabelą danych
      tabItem(tabName = "table",
              fluidRow(
                box(title = "Earthquake Data Table", width = 12,
                    dataTableOutput("quake_table"))
              )
      ),
      
      # Zakładka informacyjna About
      tabItem(tabName = "about",
              div(id = "about-tab",
                  h3("About this App"),
                  p("This dashboard displays earthquake data from the last 90 days."),
                  p("Data is sourced from the USGS Earthquake Catalog."),
                  p("Built with R Shiny and shinydashboard.")
              )
      )
    )
  )
)

# Logika serwera
server <- function(input, output, session) {
  
  # Filtrowanie danych na podstawie daty i magnitudy
  filtered_data <- reactive({
    req(input$date_range, input$mag_range)
    quakes %>%
      filter(time >= as.POSIXct(input$date_range[1]) &
               time <= as.POSIXct(input$date_range[2]) &
               mag >= input$mag_range[1] &
               mag <= input$mag_range[2])
  })
  
  # Liczba trzęsień w filtrze
  output$n_quakes <- renderText({
    nrow(filtered_data())
  })
  
  # Najmocniejsze trzęsienie
  output$max_quake <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return("No data available")
    top <- df %>% filter(mag == max(mag, na.rm = TRUE)) %>% slice(1)
    place_clean <- ifelse(grepl(",", top$place),
                          trimws(sub(".*,(.*)", "\\1", top$place)),
                          top$place)
    HTML(paste0(
      "<strong>Magnitude:</strong> ", top$mag, "<br>",
      "<strong>Depth:</strong> ", top$depth, " km<br>",
      "<strong>Date:</strong> ", format(top$time, "%Y-%m-%d %H:%M"), "<br>",
      "<strong>Location:</strong> ", place_clean
    ))
  })
  
  # Mapa trzęsień
  output$quake_map <- renderLeaflet({
    df <- filtered_data()
    pal <- colorNumeric("viridis", df$depth)
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 5, color = ~pal(depth),
        stroke = FALSE, fillOpacity = 0.7,
        label = ~paste0("Mag: ", mag, "<br>Depth: ", depth, " km<br>", place)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~depth, title = "Depth (km)")
  })
  
  # Histogram magnitud
  output$mag_hist <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      plot.new(); title(main = "No data", col.main = "red")
    } else {
      ggplot(df, aes(x = mag)) +
        geom_histogram(fill = "#448ccc", color = "white", bins = 20) +
        labs(x = "Magnitude", y = "Count") +
        theme_minimal(base_size = 14)
    }
  })
  
  # Liczba trzęsień dziennie
  output$daily_plot <- renderPlot({
    df <- filtered_data() %>%
      mutate(day = as.Date(time)) %>%
      count(day)
    if (nrow(df) == 0) {
      plot.new(); title(main = "No data", col.main = "red")
    } else {
      ggplot(df, aes(x = day, y = n)) +
        geom_line(color = "#664581", size = 1.2) +
        geom_point(color = "#664581", size = 2) +
        labs(x = "Date", y = "Count") +
        theme_minimal(base_size = 14)
    }
  })
  
  # Wykres kołowy – kontynenty
  output$continent_pie <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      plot.new(); title(main = "No data", col.main = "red")
    } else {
      df$continent <- assign_continent(df$latitude, df$longitude)
      df_summary <- df %>%
        count(continent) %>%
        mutate(percent = round(100 * n / sum(n), 1),
               label = paste0(continent, " (", percent, "%)"))
      ggplot(df_summary, aes(x = "", y = n, fill = label)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Continent") +
        theme_void(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  # Tabela top 5 krajów
  output$top_countries_by_quakes <- renderTable({
    df <- filtered_data()
    if (nrow(df) == 0) return()
    df %>%
      mutate(country = ifelse(grepl(",", place), trimws(sub(".*,", "", place)), place)) %>%
      group_by(country) %>%
      summarise(
        `Earthquake Count` = n(),
        `Avg. Depth (km)` = round(mean(depth, na.rm = TRUE), 1),
        `Avg. Magnitude` = round(mean(mag, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(`Earthquake Count`)) %>%
      slice_head(n = 5)
  })
  
  # Wykres głębokość vs magnituda
  output$depth_vs_mag <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      plot.new(); title(main = "No data", col.main = "red")
    } else {
      ggplot(df, aes(x = depth, y = mag, color = status)) +
        geom_point(alpha = 0.5, size = 1.7) +
        labs(x = "Depth (km)", y = "Magnitude", color = "Status") +
        scale_color_manual(values = c("automatic" = "#f8766d", "reviewed" = "#664581")) +
        theme_minimal(base_size = 14)
    }
  })
  
  # Tabela danych
  output$quake_table <- renderDataTable({
    filtered_data() %>%
      select(time, latitude, longitude, depth, mag, place, status)
  })
}

# Uruchomienie aplikacji
shinyApp(ui, server)
