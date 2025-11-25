#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# ---- Packages ----
library(shiny)
library(bslib)
library(tsibble)
library(fable)
library(fabletools)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(DT)
library(stringr)

# ---- Load & Prepare Data ----
set.seed(123)

# Read the real AustralianWines.csv (wide format)
wines_raw <- read_csv(
  "AustralianWines.csv",
  na = "*",
  show_col_types = FALSE
)

# Convert wide -> long, parse Month "Jan-80" -> yearmonth, ensure numeric Sales
wines <- wines_raw %>%
  pivot_longer(
    cols     = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) %>%
  mutate(
    Month = yearmonth(parse_date_time(Month, "b-y")),
    Sales = as.numeric(Sales)
  ) %>%
  as_tsibble(index = Month, key = Varietal)

varietals <- unique(wines$Varietal)

# ---- UI ----
ui <- page_sidebar(
  title = "Australian Wines Forecasting App",
  
  sidebar = sidebar(
    width = 350,
    
    card(
      card_header("Data Selection"),
      selectInput(
        "varietal", 
        "Select Varietal(s):",
        choices  = varietals,
        selected = varietals[1],
        multiple = TRUE
      ),
      
      dateRangeInput(
        "dates", 
        "Select Date Range:",
        start = as.Date(min(wines$Month)),
        end   = as.Date(max(wines$Month))
      )
    ),
    
    card(
      card_header("Model Configuration"),
      sliderInput(
        "train_end", 
        "Training End Year:",
        min   = year(as.Date(min(wines$Month))),
        max   = year(as.Date(max(wines$Month))),
        value = year(as.Date(max(wines$Month))) - 2,
        step  = 1,
        sep   = ""
      ),
      
      numericInput(
        "h", 
        "Forecast Horizon (Months):", 
        value = 12, 
        min   = 3, 
        max   = 60
      ),
      
      checkboxGroupInput(
        "models",
        "Select Models:",
        choices  = c("TSLM" = "TSLM", "ETS" = "ETS", "ARIMA" = "ARIMA"),
        selected = c("TSLM", "ETS", "ARIMA")
      )
    ),
    
    card(
      card_header("Actions"),
      actionButton(
        "update_forecast", 
        "Update Forecast", 
        class = "btn-primary",
        width = "100%"
      )
    )
  ),
  
  navset_card_tab(
    nav_panel(
      "Forecast Plot",
      card(
        card_header("Forecast Visualization"),
        plotOutput("forecast_plot", height = "600px")
      )
    ),
    
    nav_panel(
      "Accuracy Metrics",
      card(
        card_header("Model Accuracy Comparison"),
        DTOutput("acc_table")
      )
    ),
    
    nav_panel(
      "Model Specifications",
      card(
        card_header("Fitted Model Details"),
        DTOutput("model_specs")
      )
    ),
    
    nav_panel(
      "Data Overview",
      card(
        card_header("Time Series Data"),
        DTOutput("data_table")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  values <- reactiveValues(
    current_forecast = NULL,  # horizon forecasts (for plot)
    current_models   = NULL   # fitted models (mable)
  )
  
  # Filtered data based on user selections
  filtered_data <- reactive({
    req(input$varietal, input$dates)
    
    start_ym <- yearmonth(as.Date(input$dates[1]))
    end_ym   <- yearmonth(as.Date(input$dates[2]))
    
    wines %>%
      filter(
        Varietal %in% input$varietal,
        Month >= start_ym,
        Month <= end_ym
      )
  })
  
  # Train/validation split
  train_valid_split <- reactive({
    df <- filtered_data()
    train_end <- yearmonth(paste0(input$train_end, "-12"))
    
    list(
      train = df %>% filter(Month <= train_end),
      valid = df %>% filter(Month >  train_end)
    )
  })
  
  # Helper to fit models and forecast
  fit_models_and_forecast <- function() {
    req(input$models, input$varietal)
    
    split <- train_valid_split()
    train <- split$train
    
    validate(
      need(nrow(train) > 0, "No data in training window. Adjust date range or training end year.")
    )
    
    model_list <- list()
    if ("TSLM"  %in% input$models) model_list$TSLM  <- TSLM(Sales ~ trend() + season())
    if ("ETS"   %in% input$models) model_list$ETS   <- ETS(Sales)
    if ("ARIMA" %in% input$models) model_list$ARIMA <- ARIMA(Sales)
    
    validate(
      need(length(model_list) > 0, "Please select at least one model.")
    )
    
    fitted_models <- train %>% model(!!!model_list)
    forecast_data <- fitted_models %>% forecast(h = input$h)
    
    values$current_models   <- fitted_models
    values$current_forecast <- forecast_data
  }
  
  # Button-triggered update
  observeEvent(input$update_forecast, {
    withProgress(message = "Fitting models...", value = 0, {
      incProgress(0.4, detail = "Estimating models...")
      fit_models_and_forecast()
      incProgress(0.6, detail = "Generating forecasts...")
    })
  })
  
  # Also run once at startup
  observeEvent(TRUE, {
    withProgress(message = "Initializing models...", value = 0, {
      incProgress(0.4, detail = "Estimating models...")
      fit_models_and_forecast()
      incProgress(0.6, detail = "Generating forecasts...")
    })
  }, once = TRUE)
  
  # ---- Forecast plot ----
  output$forecast_plot <- renderPlot({
    req(values$current_forecast)
    
    values$current_forecast %>%
      autoplot(filtered_data(), level = c(80, 95)) +
      labs(
        title    = "Time Series Forecasts by Model",
        subtitle = paste("Forecast horizon:", input$h, "months"),
        y        = "Sales",
        x        = "Month"
      ) +
      facet_wrap(~ Varietal, scales = "free_y") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  
  # ---- Accuracy table ----
  output$acc_table <- renderDT({
    req(values$current_models)
    
    split  <- train_valid_split()
    models <- values$current_models
    valid  <- split$valid
    
    # Training accuracy
    acc_train <- accuracy(models) %>%
      mutate(Dataset = "Training")
    
    # Validation accuracy via forecasts over validation window
    if (nrow(valid) > 0) {
      fc_valid <- forecast(models, new_data = valid)
      acc_valid <- accuracy(fc_valid, valid) %>%
        mutate(Dataset = "Validation")
    } else {
      acc_valid <- tibble()
    }
    
    result <- bind_rows(acc_train, acc_valid) %>%
      select(Dataset, Varietal, Model = .model, RMSE, MAE, MAPE) %>%
      mutate(
        RMSE = round(RMSE, 2),
        MAE  = round(MAE, 2),
        MAPE = round(MAPE, 2)
      ) %>%
      mutate(across(where(~ inherits(.x, "yearmonth")), as.character)) %>%
      as.data.frame()
    
    datatable(
      result,
      options = list(
        pageLength = 15,
        scrollX    = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(columns = c("RMSE", "MAE", "MAPE"), backgroundColor = "#f8f9fa")
  })
  
  # ---- Model specs table: only ETS & ARIMA forms ----
  output$model_specs <- renderDT({
    req(values$current_models)
    
    specs <- values$current_models %>%
      as_tibble() %>%
      # pivot model columns (TSLM / ETS / ARIMA) into long format
      pivot_longer(
        cols = -Varietal,
        names_to = "Model",
        values_to = "mdl_obj"
      ) %>%
      # remove TSLM rows from this table
      filter(Model != "TSLM") %>%
      mutate(
        # model objects print like "<ETS(M,N,A)>" or "<ARIMA(1,0,0)(0,1,1)[12] w/ drift>"
        Specification = str_replace_all(
          as.character(mdl_obj),
          "^<|>$", ""   # strip leading/trailing angle brackets
        )
      ) %>%
      select(Varietal, Model, Specification)
    
    datatable(
      specs,
      options = list(
        pageLength = 10,
        scrollX    = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ---- Data overview table ----
  output$data_table <- renderDT({
    data_summary <- filtered_data() %>%
      as_tibble() %>%
      mutate(
        Month_Date = as.Date(Month),
        Year       = year(Month_Date),
        Month_Name = month(Month_Date, label = TRUE, abbr = FALSE)
      ) %>%
      select(Year, Month_Name, Varietal, Sales) %>%
      arrange(desc(Year), Varietal) %>%
      as.data.frame()
    
    datatable(
      data_summary,
      options = list(
        pageLength = 15,
        scrollX    = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Sales", currency = "", digits = 0)
  })
}

# ---- RUN APP ----
shinyApp(ui, server)
