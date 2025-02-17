# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)

# Load data
df <- read.csv("data/clean/marketshare.csv")
df2 <- read.csv("data/clean/marketshare_municipality.csv")

# Convert period to date for better plotting (assuming period is in "YYYY-MM" format)
df$period <- as.Date(paste0(df$period, "-01"))
df2$period <- as.Date(paste0(df2$period, "-01"))

# Define UI for the app
ui <- fluidPage(
  titlePanel("Marknadsandel Rusta och Matcha"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("leverantor", "Välj leverantör:", 
                  choices = unique(df$leverantor), 
                  selected = "Curonova Consulting AB"),
      dateRangeInput("daterange", "Välj datum:",
                     start = as.Date(paste0("2024-01-01")),  # Start date defaults to January 2024
                     end = max(df$period),    # End date defaults to the latest period
                     min = min(df$period),
                     max = max(df$period),
                     format = "yyyy-mm")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Grafer",
                 plotOutput("andelNyPlot"),
                 plotOutput("sumNyPlot"),       
                 plotOutput("totalSumNyPlot"),
                 plotOutput("andelPagaendePlot"),
                 plotOutput("sumPagaendePlot"),
                 plotOutput("totalSumPagaendePlot")),
        tabPanel("Tabell", DTOutput("fullDataTable")),
        tabPanel("Topplista", DTOutput("leverantorTable")),
        
        # Kommuner Tab with detached totals row below the main table
        tabPanel("Kommuner", 
                 DTOutput("municipalityTable"),
                 uiOutput("totalsRowUI")  # Display totals row as a separate UI element
        )
      )
    )
  ),
  
  # Add custom CSS for the totals row, scoped to the "totals-row-container" class
  tags$style(HTML("
    .totals-row-container .totals-row-table {
      width: 100%;
      border-collapse: collapse;
      margin-top: 5px;
      font-size: 14px;
    }
    .totals-row-container .totals-row-table th, 
    .totals-row-container .totals-row-table td {
      padding: 8px;
      text-align: left;
      border: 1px solid #ddd;
    }
    .totals-row-container .totals-row-table th {
      background-color: #f2f2f2;
      font-weight: bold;
    }
    /* Lighten the background color for the totals row */
    .totals-row-container .totals-row-table tr:last-child {
      font-weight: bold;
      background-color: #f9f9f9;
    }
  "))
)

# Define server logic for the app
server <- function(input, output, session) {
  
  # Filter data based on selected leverantor and date range for the plots
  filtered_data <- reactive({
    df %>%
      filter(leverantor == input$leverantor) %>%
      filter(period >= input$daterange[1] & period <= input$daterange[2])
  })
  
  # Filter data for municipality-specific table, showing only the latest period
  filtered_municipality_data <- reactive({
    latest_period <- max(df2$period)  # Find the latest available period in df2
    
    df2 %>%
      filter(leverantor == input$leverantor) %>%
      filter(period == latest_period)  # Filter only for the latest period
  })
  
  # Define the theme with larger axis text and titles
  larger_text_theme <- theme_minimal() +
    theme(
      axis.text = element_text(size = 12),            # Increase axis text size
      plot.title = element_text(size = 16)  # Increase title size
    )
  
  # Plot for andel_ny
  output$andelNyPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = period, y = andel_ny)) +
      geom_line(color = "cadetblue4", size = 1) +
      labs(title = "Andel av nya", x = "Månad", y = "Marknadsandel (%)") +
      scale_y_continuous(labels = scales::percent) +
      larger_text_theme
  })
  
  # Plot for andel_pagaende
  output$andelPagaendePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = period, y = andel_pagaende)) +
      geom_line(color = "chocolate3", size = 1) +
      labs(title = "Andel av pågående", x = "Månad", y = "Marknadsandel (%)") +
      scale_y_continuous(labels = scales::percent) +
      larger_text_theme
  })
  
  # Plot for sum of tjanstdelt_ny over time, filtered by leverantor
  output$sumNyPlot <- renderPlot({
    sum_data <- df %>%
      filter(leverantor == input$leverantor) %>%
      filter(period >= input$daterange[1] & period <= input$daterange[2]) %>%
      group_by(period) %>%
      summarise(sum_tjanstdelt_ny = sum(tjanstdelt_ny, na.rm = TRUE))
    
    ggplot(sum_data, aes(x = period, y = sum_tjanstdelt_ny)) +
      geom_line(color = "cadetblue4", size = 1) +
      labs(title = "Summa nya", x = "Månad", y = "Summa nya") +
      larger_text_theme
  })
  
  # Plot for sum of tjanstdelt_pagaende over time, filtered by leverantor
  output$sumPagaendePlot <- renderPlot({
    sum_data <- df %>%
      filter(leverantor == input$leverantor) %>%
      filter(period >= input$daterange[1] & period <= input$daterange[2]) %>%
      group_by(period) %>%
      summarise(sum_tjanstdelt_pagaende = sum(tjanstdelt_pagaende, na.rm = TRUE))
    
    ggplot(sum_data, aes(x = period, y = sum_tjanstdelt_pagaende)) +
      geom_line(color = "chocolate3", size = 1) +
      labs(title = "Summa pågående", x = "Månad", y = "Summa pågående") +
      larger_text_theme
  })
  
  # Plot for total sum of tjanstdelt_ny over time (no filter on leverantor)
  output$totalSumNyPlot <- renderPlot({
    total_sum_data <- df %>%
      filter(period >= input$daterange[1] & period <= input$daterange[2]) %>%
      group_by(period) %>%
      summarise(total_sum_tjanstdelt_ny = sum(tjanstdelt_ny, na.rm = TRUE))
    
    ggplot(total_sum_data, aes(x = period, y = total_sum_tjanstdelt_ny)) +
      geom_line(color = "cadetblue4", size = 1) +
      labs(title = "Total nya (hela marknaden)", x = "Månad", y = "Total nya marknaden") +
      larger_text_theme
  })
  
  # Plot for total sum of tjanstdelt_pagaende over time (no filter on leverantor)
  output$totalSumPagaendePlot <- renderPlot({
    total_sum_data <- df %>%
      filter(period >= input$daterange[1] & period <= input$daterange[2]) %>%
      group_by(period) %>%
      summarise(total_sum_tjanstdelt_pagaende = sum(tjanstdelt_pagaende, na.rm = TRUE))
    
    ggplot(total_sum_data, aes(x = period, y = total_sum_tjanstdelt_pagaende)) +
      geom_line(color = "chocolate3", size = 1) +
      labs(title = "Total pågående (hela marknaden)", x = "Månad", y = "Total pågående marknaden") +
      larger_text_theme
  })
  
  # Render Full Data Table without Custom Styling
  output$fullDataTable <- renderDT({
    filtered_data() %>%
      select(-tjanst, -leverantor) %>%
      mutate(period = format(period, "%Y-%m")) %>%
      rename(
        `Månad` = period,
        `Nya deltagare` = tjanstdelt_ny,
        `Pågående deltagare` = tjanstdelt_pagaende,
        `Avslutade deltagare` = tjanstdelt_avslutat,
        `Andel av nya` = andel_ny,
        `Andel av pågående` = andel_pagaende,
        `Total nya` = total_ny,
        `Total pågående` = total_pagaende
      ) %>%
      datatable(
        options = list(pageLength = -1),
        rownames = FALSE
      ) %>%
      formatPercentage(c("Andel av nya", "Andel av pågående"), digits = 2)
  })
  
  # Render Leverantor Table with Custom Column Names and Independent Row Numbering
  output$leverantorTable <- renderDT({
    latest_period <- max(df$period[df$period >= input$daterange[1] & df$period <= input$daterange[2]])
    
    latest_leverantors <- df %>%
      filter(period == latest_period) %>%
      select(leverantor, andel_ny, andel_pagaende) %>%
      arrange(desc(andel_pagaende)) %>%
      mutate(Row = NA) %>%       # Add an empty Row column for numbering
      select(Row, everything()) %>%  # Place Row as the first column
      rename(
        Position = Row,
        Leverantör = leverantor,
        `Andel av nya` = andel_ny,
        `Andel av pågående` = andel_pagaende
      )
    
    datatable(
      latest_leverantors,
      options = list(
        pageLength = 10,
        order = list(list(2, 'desc')),  # Default sorting on `Andel av pågående`
        rowCallback = JS(
          "function(row, data, index) {",
          "$('td:eq(0)', row).html(index + 1);",  # Populate Position column with row number
          "}"
        )
      ),
      rownames = FALSE
    ) %>%
      formatPercentage(c('Andel av nya', 'Andel av pågående'), digits = 2) %>%
      formatStyle(
        'Leverantör', 
        target = 'row',
        fontWeight = styleEqual("Curonova Consulting AB", "bold")
      )
  })
  
  # Municipality Table without Totals Row
  output$municipalityTable <- renderDT({
    
    # Filter data for the table
    municipality_data <- filtered_municipality_data() %>%
      select(placering_kommun, tjanstdelt_ny, tjanstdelt_pagaende, total_ny, total_pagaende, andel_ny, andel_pagaende) %>%
      rename(
        `Kommun` = placering_kommun,
        `Nya deltagare` = tjanstdelt_ny,
        `Pågående deltagare` = tjanstdelt_pagaende,
        `Total nya` = total_ny,
        `Total pågående` = total_pagaende,
        `Andel nya` = andel_ny,
        `Andel pågående` = andel_pagaende
      )
    
    datatable(
      municipality_data,
      options = list(pageLength = -1),  # Display all rows without pagination
      rownames = FALSE
    ) %>%
      formatPercentage(c("Andel nya", "Andel pågående"), digits = 2)
  })
  
  # Separate totals row displayed as a separate UI element
  output$totalsRowUI <- renderUI({
    totals_row <- filtered_municipality_data() %>%
      summarise(
        Kommun = "Totalt",
        `Nya deltagare` = sum(tjanstdelt_ny, na.rm = TRUE),
        `Pågående deltagare` = sum(tjanstdelt_pagaende, na.rm = TRUE),
        `Total nya` = sum(total_ny, na.rm = TRUE),
        `Total pågående` = sum(total_pagaende, na.rm = TRUE)
      ) %>%
      mutate(
        `Andel nya` = `Nya deltagare` / `Total nya`,
        `Andel pågående` = `Pågående deltagare` / `Total pågående`
      )
    
    div(class = "totals-row-container",  
        tags$table(class = "totals-row-table",
                   tags$thead(
                     tags$tr(
                       tags$th("Kommun"), tags$th("Nya deltagare"), tags$th("Pågående deltagare"),
                       tags$th("Total nya"), tags$th("Total pågående"), tags$th("Andel nya"), tags$th("Andel pågående")
                     )
                   ),
                   tags$tbody(
                     tags$tr(
                       tags$td(totals_row$Kommun),
                       tags$td(totals_row$`Nya deltagare`),
                       tags$td(totals_row$`Pågående deltagare`),
                       tags$td(totals_row$`Total nya`),
                       tags$td(totals_row$`Total pågående`),
                       tags$td(sprintf("%.2f%%", totals_row$`Andel nya` * 100)),
                       tags$td(sprintf("%.2f%%", totals_row$`Andel pågående` * 100))
                     )
                   )
        )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)