# app.R
library(shiny)
library(plotly)
library(tidyverse)
library(readr)
library(RColorBrewer)

# Read the CSV data
data <- read_csv("2025-03-18 Historical Health Fund RVG Unit Values.csv")

# Reshape the data for plotting
data_long <- data %>%
  pivot_longer(cols = -Year, names_to = "Fund", values_to = "Value") %>%
  mutate(
    IndexType = case_when(
      str_detect(Fund, "_Indexed at All Groups CPI") ~ "All Groups CPI",
      str_detect(Fund, "_Indexed at Health CPI") ~ "Health CPI",
      TRUE ~ "Actual"
    ),
    Fund = str_remove(Fund, "_Indexed at All Groups CPI|_Indexed at Health CPI")
  )%>%
  group_by(Fund, IndexType) %>%
  arrange(Year)%>%
  mutate(
    PctChange = (Value / lag(Value) - 1) * 100,
    PctChangeLabel = ifelse(is.na(PctChange),"", sprintf("%.2f%%", PctChange))
  )

# Get sorted unique fund names
sorted_funds <- sort(unique(data_long$Fund[data_long$IndexType == "Actual"]))

# Create a fixed color palette for all funds
fixed_color_scale <- c(
  "AMA Fees List" = "gold",
  "Medicare Benefits Schedule" = "darkgreen",
  "Department of Veteran Affairs" = "darkblue",
  "Bupa Medical Gap Scheme" = "blue",
  "AHSA Access Gap Cover" = "lightgreen",
  "Medibank Private GapCover" = "red",
  "HCF Medicover" = "salmon",
  "nib Medigap" = "green",
  "HBF (WA only)" = "lightblue",
  "HBF Specialist Anaesthetists (WA only)" = "grey",
  "$500 Known Gap" = "royalblue"
)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #logo {
        max-height: 40px;
        margin-top: -10px;
      }
      .navbar, .navbar-default, .navbar-static-top {
        background-color: black !important;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a,
      .navbar-light .navbar-brand,
      .navbar-light .navbar-nav .nav-link {
        color: orange !important;
      }
    "))
  ),
  navbarPage(
    title = span(img(src = "ASALogo.png", id = "logo"),
                 ""),
    id = "nav",
    header = tags$style(HTML("
      #nav {
        background-color: black !important;
      }
      #nav > li > a {
        color: black !important;
      }
    ")),  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("funds", "Select Health Funds:",
                         choices = sorted_funds,
                         selected = c("Medicare Benefits Schedule", "AMA Fees List")),
      checkboxGroupInput("indexTypes", "Select Index Types:",
                         choices = c("Actual", "All Groups CPI", "Health CPI"),
                         selected = "Actual")
    ),
    mainPanel(
      plotlyOutput("lineChart")
    )
  )
))

# Define server logic
server <- function(input, output) {
  output$lineChart <- renderPlotly({
    filtered_data <- data_long %>%
      filter(Fund %in% input$funds, IndexType %in% input$indexTypes)
    
    p <- ggplot(filtered_data, aes(x = Year, y = Value, color = Fund, group = interaction(Fund, IndexType),
                                   text = paste("Fund:", Fund, 
                                                "<br>Index Type:", IndexType,
                                                "<br>Year:", Year,
                                                "<br>Value:", round(Value, 2),
                                                "<br>Change:", PctChangeLabel))) +
      geom_line(aes(alpha = IndexType)) +
      geom_point(aes(alpha = IndexType)) +
      scale_color_manual(values = fixed_color_scale) +
      scale_alpha_manual(values = c("Actual" = 1, "All Groups CPI" = 0.6, "Health CPI" = 0.3)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme_minimal() +
      labs(x = "Year", y = "Unit Value ($)", color = "Health Fund", alpha = "Index Type") +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("Fund", "IndexType", "Year", "Value", "PctChangeLabel")) %>%
  layout(legend = list(orientation = "h", y = -0.2),
         images = list(list(
           source = "ASALogo2.png",
           x = 0.8, y = 0.15,
           sizex = 0.25, sizey = 0.25,
           xref = "paper", yref = "paper",
           opacity = 0.3
         )))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)