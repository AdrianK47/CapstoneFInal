library(shiny)
library(ggplot2)
library(plotly)

# ── 1. UI Definition ─────────────────────────────────────────
ui <- fluidPage(
  
  titlePanel(" Prototype 2 "),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Import Data"),
      
      # ── CSV import controls ──
      fileInput("uploaded_file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      checkboxInput("header", "Header Present", TRUE),
      
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      
      hr(),
      h4("Controls"),
      
      # Dropdowns — populated dynamically from uploaded file's columns
      uiOutput("x_var_ui"),
      uiOutput("y_var_ui"),
      
      # Bubble size (only shown on Bubble Chart tab)
      uiOutput("size_var_ui"),
      
      # Checkbox
      uiOutput("color_var_ui"),
      
      # Slider — populated dynamically from selected x variable
      uiOutput("filter_ui"),
      
      hr()
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # Tab 1 — Scatter Plot
        tabPanel(
          title = "Scatter Plot",
          br(),
          plotlyOutput(outputId = "scatter_plot", height = "400px"),
          br(),
          textOutput(outputId = "point_count")
        ),
        
        # Tab 2 — Bar Chart
        tabPanel(
          title = "Bar Chart",
          br(),
          plotlyOutput(outputId = "bar_chart", height = "400px"),
          br(),
          helpText("X: categorical or numeric column used as groups. Y: numeric column summed per group.")
        ),
        
        # Tab 3 — Box Plot
        tabPanel(
          title = "Box Plot",
          br(),
          plotlyOutput(outputId = "box_plot", height = "400px"),
          br(),
          helpText("X: grouping column. Y: numeric column whose distribution is shown.")
        ),
        
        # Tab 4 — Pie Chart
        tabPanel(
          title = "Pie Chart",
          br(),
          plotlyOutput(outputId = "pie_chart", height = "400px"),
          br(),
          helpText("Uses the X-axis column as labels. Counts occurrences per category.")
        ),
        
        # Tab 5 — Bubble Chart
        tabPanel(
          title = "Bubble Chart",
          br(),
          plotlyOutput(outputId = "bubble_chart", height = "400px"),
          br(),
          helpText("X and Y as axes; bubble size driven by the Size variable selected in the sidebar.")
        ),
        
        # Tab 6 — Data Table
        tabPanel(
          title = "Data Table",
          br(),
          tableOutput(outputId = "data_table")
        )
      )
    )
  )
)


# ── 2. Server Logic ───────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Read uploaded CSV ────────────────────
  imported_data <- reactive({
    req(input$uploaded_file)
    read.csv(input$uploaded_file$datapath,
             header = input$header,
             sep    = input$sep)
  })
  
  # ── Numeric columns only ──────────────
  numeric_cols <- reactive({
    df <- imported_data()
    names(df)[sapply(df, is.numeric)]
  })
  
  # ── All columns ─────────────
  all_cols <- reactive({
    names(imported_data())
  })
  
  # ── Dynamic UI: X-axis dropdown ───────────────────────────
  output$x_var_ui <- renderUI({
    req(numeric_cols())
    cols <- numeric_cols()
    selectInput("x_var", "X-axis variable",
                choices  = cols,
                selected = cols[min(2, length(cols))])
  })
  
  # ── Dynamic UI: Y-axis dropdown ───────────────────────────
  output$y_var_ui <- renderUI({
    req(numeric_cols())
    cols <- numeric_cols()
    selectInput("y_var", "Y-axis variable",
                choices  = cols,
                selected = cols[1])
  })
  
  # ── Dynamic UI: bubble size column ────────────────────────
  output$size_var_ui <- renderUI({
    req(numeric_cols())
    cols <- numeric_cols()
    selectInput("size_var", "Bubble size variable",
                choices  = cols,
                selected = cols[min(3, length(cols))])
  })
  
  # ── Dynamic UI: optional colour-by column ─────────────────
  output$color_var_ui <- renderUI({
    req(all_cols())
    tagList(
      checkboxInput("color_on", "Colour by a column", value = FALSE),
      conditionalPanel(
        condition = "input.color_on == true",
        selectInput("color_var", "Colour column",
                    choices = all_cols(),
                    selected = all_cols()[1])
      )
    )
  })
  
  # ── Dynamic UI: filter slider for X variable ──────────────
  output$filter_ui <- renderUI({
    req(imported_data(), input$x_var)
    df  <- imported_data()
    col <- input$x_var
    req(col %in% names(df))
    vals <- df[[col]]
    sliderInput("x_range",
                label = paste("Filter by", col),
                min   = floor(min(vals,   na.rm = TRUE)),
                max   = ceiling(max(vals, na.rm = TRUE)),
                value = c(floor(min(vals,   na.rm = TRUE)),
                          ceiling(max(vals, na.rm = TRUE))))
  })
  
  # ── Filtered data shared by all outputs ───────────────────
  filtered_data <- reactive({
    req(imported_data(), input$x_var, input$x_range)
    df  <- imported_data()
    col <- input$x_var
    df[!is.na(df[[col]]) &
         df[[col]] >= input$x_range[1] &
         df[[col]] <= input$x_range[2], ]
  })
  
  # ── Scatter plot ──────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    req(filtered_data(), input$x_var, input$y_var)
    df <- filtered_data()
    x  <- input$x_var
    y  <- input$y_var
    
    p <- ggplot(df, aes_string(x = x, y = y))
    
    if (isTRUE(input$color_on) && !is.null(input$color_var) &&
        input$color_var %in% names(df)) {
      p <- p + geom_point(aes_string(colour = paste0("factor(", input$color_var, ")")),
                          size = 3, alpha = 0.8) +
        labs(colour = input$color_var)
    } else {
      p <- p + geom_point(colour = "#2563EB", size = 3, alpha = 0.8)
    }
    
    p <- p + geom_smooth(method = "lm", se = TRUE,
                         colour = "grey40", linetype = "dashed")
    p <- p + theme_minimal()
    p <- p + labs(
      title = paste(y, "vs", x),
      subtitle = paste(nrow(df), "rows shown"),
      x = x, y = y
    )
    
    ggplotly(p) %>% config(displayModeBar = TRUE, scrollZoom = TRUE)
  })
  
  # ── Bar chart ─────────────────────────────────────────────
  output$bar_chart <- renderPlotly({
    req(filtered_data(), input$x_var, input$y_var)
    df <- filtered_data()
    x  <- input$x_var
    y  <- input$y_var
    
    # Aggregate: sum y per x group
    agg <- aggregate(df[[y]], by = list(group = df[[x]]), FUN = sum, na.rm = TRUE)
    names(agg) <- c("group", "value")
    
    fig <- plot_ly(
      agg,
      x    = ~group,
      y    = ~value,
      type = "bar",
      marker = list(color = "#2563EB", opacity = 0.85)
    ) %>%
      layout(
        title  = paste("Sum of", y, "by", x),
        xaxis  = list(title = x),
        yaxis  = list(title = paste("Sum of", y)),
        bargap = 0.3
      ) %>%
      config(displayModeBar = TRUE, scrollZoom = TRUE)
    
    fig
  })
  
  # ── Box plot ───────────────────────────────────────────────
  output$box_plot <- renderPlotly({
    req(filtered_data(), input$x_var, input$y_var)
    df <- filtered_data()
    x  <- input$x_var
    y  <- input$y_var
    
    fig <- plot_ly(
      df,
      x    = ~df[[x]],
      y    = ~df[[y]],
      type = "box",
      boxpoints = "outliers",
      marker    = list(color = "#2563EB"),
      line      = list(color = "#1e3a8a")
    ) %>%
      layout(
        title = paste("Distribution of", y, "by", x),
        xaxis = list(title = x),
        yaxis = list(title = y)
      ) %>%
      config(displayModeBar = TRUE, scrollZoom = TRUE)
    
    fig
  })
  
  # ── Pie chart ─────────────────────────────────────────────
  output$pie_chart <- renderPlotly({
    req(filtered_data(), input$x_var)
    df  <- filtered_data()
    col <- input$x_var
    
    # Count occurrences per category
    counts <- as.data.frame(table(df[[col]]))
    names(counts) <- c("label", "count")
    
    fig <- plot_ly(
      counts,
      labels = ~label,
      values = ~count,
      type   = "pie",
      textinfo      = "none", # or label+percent
      hoverinfo     = "label+value+percent",
      marker = list(line = list(color = "#ffffff", width = 0.1))
    ) %>%
      layout(
        title = paste("Composition of", col),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
    
    fig
  })
  
  # ── Bubble chart ──────────────────────────────────────────
  output$bubble_chart <- renderPlotly({
    req(filtered_data(), input$x_var, input$y_var, input$size_var)
    df   <- filtered_data()
    x    <- input$x_var
    y    <- input$y_var
    sz   <- input$size_var
    
    # Normalise size to 5–40 range for readability
    raw   <- df[[sz]]
    sizes <- 5 + 35 * (raw - min(raw, na.rm = TRUE)) /
      (max(raw, na.rm = TRUE) - min(raw, na.rm = TRUE) + 1e-9)
    
    color_vals <- if (isTRUE(input$color_on) && !is.null(input$color_var) &&
                      input$color_var %in% names(df)) {
      as.factor(df[[input$color_var]])
    } else {
      df[[y]]
    }
    
    fig <- plot_ly(
      df,
      x    = ~df[[x]],
      y    = ~df[[y]],
      type = "scatter",
      mode = "markers",
      marker = list(
        size    = sizes,
        opacity = 0.6,
        color   = color_vals,
        colorscale = "Viridis",
        showscale  = TRUE
      ),
      text = ~paste0(x, ": ", df[[x]], "<br>",
                     y, ": ", df[[y]], "<br>",
                     sz, ": ", df[[sz]])
    ) %>%
      layout(
        title = paste("Bubble Chart:", y, "vs", x, "(size =", sz, ")"),
        xaxis = list(title = x),
        yaxis = list(title = y)
      ) %>%
      config(displayModeBar = TRUE, scrollZoom = TRUE)
    
    fig
  })
  
  # ── Text summary below scatter chart ─────────────────────
  output$point_count <- renderText({
    req(filtered_data(), imported_data(), input$x_var)
    n     <- nrow(filtered_data())
    total <- nrow(imported_data())
    paste0("Showing ", n, " of ", total, " rows",
           if (!is.null(input$x_range))
             paste0(" (filtered by ", input$x_var, ": ",
                    input$x_range[1], " \u2013 ", input$x_range[2], ")")
           else "", ".")
  })
  
  # ── Data table ────────────────────────────────────────────
  output$data_table <- renderTable({
    req(filtered_data())
    head(filtered_data(), 50)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
}


# ── 3. Launch ─────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
