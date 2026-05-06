
library(tidyverse)
library(shiny)
library(leaflet)
library(plotly)
library(lubridate)
library(janitor)
library(geojsonio)

# =========================
# 1. LOAD DATA
# =========================

janssen <- read_csv("data/janssen.csv")
janssen <- janitor::clean_names(janssen)

pfizer <- read_csv("data/pfizer.csv")
pfizer <- janitor::clean_names(pfizer)

moderna <- read_csv("data/moderna.csv")
moderna <- janitor::clean_names(moderna)



# =========================
# 2. CLEAN / RENAME DATA
# =========================

pfizer <- pfizer %>%
  rename(
    Jurisdiction = 1,
    Week.of.Allocations = 2,
    Pfizer.X1st.Dose.Allocations = 3,
    Pfizer.X2nd.Dose.Allocations = 4
  )

moderna <- moderna %>%
  rename(
    Jurisdiction = 1,
    Week.of.Allocations = 2,
    Moderna.X1st.Dose.Allocations = 3,
    Moderna.X2nd.Dose.Allocations = 4
  )

janssen <- janssen %>%
  rename(
    Jurisdiction = 1,
    Week.of.Allocations = 2,
    Janssen.X1st.Dose.Allocations = 3
  )


# =========================
# 3. MERGE DATA
# =========================

all_data <- pfizer %>%
  full_join(moderna, by = c("Jurisdiction", "Week.of.Allocations")) %>%
  full_join(janssen, by = c("Jurisdiction", "Week.of.Allocations")) %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)),
    Week.of.Allocations = mdy(Week.of.Allocations),
    All.Dose.Allocations =
      0.5 * (
        Pfizer.X1st.Dose.Allocations +
          Pfizer.X2nd.Dose.Allocations +
          Moderna.X1st.Dose.Allocations +
          Moderna.X2nd.Dose.Allocations
      ) +
      Janssen.X1st.Dose.Allocations
  )


# =========================
# 4. ADD POPULATION DATA
# =========================

features_state_clean <- features.state %>%
  mutate(
    Jurisdiction = as.character(State),
    Jurisdiction = str_replace_all(Jurisdiction, "([a-z])([A-Z])", "\\1 \\2"),
    Jurisdiction = recode(
      Jurisdiction,
      "Districtof Columbia" = "District of Columbia",
      "District Of Columbia" = "District of Columbia",
      "DistrictofColumbia" = "District of Columbia"
    )
  ) %>%
  select(Jurisdiction, pop)

all_data <- all_data %>%
  left_join(features_state_clean, by = "Jurisdiction") %>%
  filter(!is.na(pop))


# =========================
# 5. CUMULATIVE ALLOCATION + DOSE PER POP
# =========================

all_data <- all_data %>%
  arrange(Jurisdiction, Week.of.Allocations) %>%
  group_by(Jurisdiction) %>%
  mutate(
    Cum.Allocation = cumsum(All.Dose.Allocations),
    DosePerPop = Cum.Allocation / pop
  ) %>%
  ungroup()


# =========================
# 6. TIME SERIES FUNCTION
# =========================

state_or_us_ts_plot <- function(data, state_name) {
  
  if (state_name == "United States") {
    
    plot_data <- data %>%
      group_by(Week.of.Allocations) %>%
      summarise(
        DosePerPop = sum(Cum.Allocation, na.rm = TRUE) / sum(pop, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    
    plot_data <- data %>%
      filter(Jurisdiction == state_name) %>%
      select(Week.of.Allocations, DosePerPop)
  }
  
  plot_ly(
    plot_data,
    x = ~Week.of.Allocations,
    y = ~DosePerPop,
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = paste(
      "Week: %{x}<br>",
      "Dose per population: %{y:.3f}<extra></extra>"
    )
  ) %>%
    layout(
      title = paste("Cumulative Vaccine Allocation per Population -", state_name),
      xaxis = list(title = "Week"),
      yaxis = list(title = "Dose per Population")
    )
}


# =========================
# 7. MAP FUNCTION
# =========================

map_leaflet <- function(data, selected_date) {
  
  df <- data %>%
    filter(Week.of.Allocations == selected_date)
  
  states <- geojson_read(
    "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
    what = "sp"
  )
  
  states@data <- states@data %>%
    mutate(Jurisdiction = name)
  
  states@data <- left_join(states@data, df, by = "Jurisdiction")
  
  pal <- colorNumeric(
    palette = "viridis",
    domain = states@data$DosePerPop,
    na.color = "grey80"
  )
  
  labels <- sprintf(
    "<strong>%s</strong><br/>
     Population: %s<br/>
     Dose per population: %.3f<br/>
     Week: %s",
    states@data$name,
    format(states@data$pop, big.mark = ","),
    states@data$DosePerPop,
    selected_date
  ) %>%
    lapply(htmltools::HTML)
  
  leaflet(states) %>%
    addTiles() %>%
    setView(lng = -96, lat = 37.8, zoom = 4) %>%
    addPolygons(
      fillColor = ~pal(DosePerPop),
      weight = 1,
      color = "white",
      fillOpacity = 0.8,
      label = labels,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = pal,
      values = states@data$DosePerPop,
      title = "Doses / Population",
      position = "bottomright"
    )
}


# =========================
# 8. UI
# =========================

ui <- fluidPage(
  titlePanel("COVID-19 Vaccine Allocation per Population"),
  
  p("Explore cumulative COVID-19 vaccine allocations by state and week. 
    The map shows dose allocations adjusted for population, and the chart tracks trends over time."),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "plot_date",
        label = "Select week",
        min = min(all_data$Week.of.Allocations, na.rm = TRUE),
        max = max(all_data$Week.of.Allocations, na.rm = TRUE),
        value = max(all_data$Week.of.Allocations, na.rm = TRUE),
        step = 7,
        timeFormat = "%d %b %Y",
        animate = animationOptions(interval = 2000, loop = FALSE)
      ),
      
      selectInput(
        inputId = "state_show",
        label = "Select state",
        choices = c(
          "United States",
          sort(unique(all_data$Jurisdiction))
        ),
        selected = "Virginia"
      )
    ),
    
    mainPanel(
      leafletOutput("vacc_map", height = "600px"),
      br(),
      plotlyOutput("vacc_ts", height = "300px")
    )
  )
)


# =========================
# 9. SERVER
# =========================

server <- function(input, output, session) {
  
  output$vacc_map <- renderLeaflet({
    map_leaflet(all_data, input$plot_date)
  })
  
  output$vacc_ts <- renderPlotly({
    state_or_us_ts_plot(all_data, input$state_show)
  })
}


# =========================
# 10. RUN APP
# =========================

shinyApp(ui = ui, server = server)