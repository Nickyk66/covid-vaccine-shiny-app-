# COVID-19 Vaccine Allocation Shiny App

Interactive dashboard built in R using Shiny to visualize COVID-19 vaccine allocation trends across U.S. states.

Built as part of a data science project and later refactored into a fully deployable interactive analytics dashboard.

## Live App
[View the deployed app here](https://nickkaufman.shinyapps.io/covid-vaccine-shiny-app/)

---

## Features

- Interactive U.S. choropleth map using Leaflet
- Weekly allocation tracking with animated time slider
- State-level cumulative vaccine allocation trends using Plotly
- Per-capita vaccine allocation metrics
- Dynamic Top 10 / Bottom 10 state ranking table
- Hover-based state statistics and map interaction

---

## Tech Stack

- **R**
- **Shiny**
- **tidyverse**
- **Leaflet**
- **Plotly**
- **lubridate**
- **janitor**
- **geojsonio**

---

## Preview

![App Preview](screenshot.png)

---

## How to Run Locally

```r
shiny::runApp()
```

---

## Data Sources

The application combines Pfizer, Moderna, and Janssen vaccine allocation datasets and normalizes allocations by state population to generate per-capita metrics.

---

## Project Goals

This project was designed to:
- practice data cleaning and transformation workflows in R
- build interactive dashboards using Shiny
- integrate geospatial and time-series visualization
- communicate public health allocation trends through interactive analytics
