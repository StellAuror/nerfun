library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(MASS) 
library(shinydashboard)

folders <- list.files()[-(list.files() |> grep("\\.", "", x = _))]
lapply(folders, function(folder) {
  scripts <- list.files(path = folder)[list.files(path = folder) |>
              sub("^.*?\\.", "", x = _) |> 
              tolower() == "r"]
  lapply(paste(folder, scripts, sep = "/"), function(script) {
    source(script)
  })
}) |> invisible()


list.files(path = "Outliers-Mean")


ui <- fluidPage(
  titlePanel("Zaawansowana Analiza Funkcji Straty Tukey's Biweight"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Wybierz Zbiór Danych", choices = c("airquality", "mtcars", "Wgraj Własny")),
      conditionalPanel(
        condition = "input.dataset == 'Wgraj Własny'",
        fileInput("file", "Wgraj Plik CSV")
      ),
      uiOutput("var_select"),
      sliderInput("threshold", "Próg (t)", min = 1, max = 100, value = 20),
      selectInput("loss_function", "Funkcja Straty", choices = c("Tukey's Biweight", "Hubera", "Metoda Najmniejszych Kwadratów")),
      checkboxInput("robust", "Użyj Regresji Odpornościowej", value = FALSE),
      actionButton("update", "Aktualizuj Model")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Podsumowanie Modelu", verbatimTextOutput("model_summary")),
        tabPanel(
          "Analiza",
          plotlyOutput("plot_residuals"),
          plotlyOutput("plot_loss"),
          plotlyOutput("plot_influence")
        ),
      )
    )
  )
)

server <- function(input, output, session) {
  dataInput <- reactive({
    if (input$dataset == "airquality") {
      data <- airquality
    } else if (input$dataset == "mtcars") {
      data <- mtcars
    } else {
      req(input$file)
      data <- read.csv(input$file$datapath)
    }
    data
  })
  
  output$var_select <- renderUI({
    data <- dataInput()
    var_choices <- names(data)
    fluidRow(
      column(6, selectInput("response", "Zmienna Objaśniana", choices = var_choices)),
      column(6, uiOutput("predictor_select"))
    )
  })
  
  output$predictor_select <- renderUI({
    req(input$response)
    data <- dataInput()
    var_choices <- setdiff(names(data), input$response)
    selectizeInput("predictors", "Zmienna(y) Objaśniająca(e)", choices = var_choices, multiple = TRUE)
  })
  
  modelFit <- eventReactive(input$update, {
    data <- dataInput()
    req(input$response, input$predictors)
    
    predictors <- setdiff(input$predictors, input$response)
    req(length(predictors) > 0, "Proszę wybrać co najmniej jedną zmienną objaśniającą inną niż zmienna objaśniana.")
    
    formula <- as.formula(paste(input$response, "~", paste(predictors, collapse = "+")))
    
    data <- data %>% dplyr::select(any_of(c(input$response, predictors))) %>% na.omit()
    
    if (input$robust) {
      model <- rlm(formula, data = data)
    } else {
      model <- lm(formula, data = data)
    }
    list(model = model, data = data)
  })
  
  output$model_summary <- renderPrint({
    fit <- modelFit()
    summary(fit$model)
  })
  
  output$plot_residuals <- renderPlotly({
    fit <- modelFit()
    model <- fit$model
    data <- fit$data
    residuals <- residuals(model)
    predictions <- predict(model, newdata = data)
    
    # Obliczanie straty
    if (input$loss_function == "Tukey's Biweight") {
      loss <- tukey_biweight_loss(residuals, input$threshold)
    } else if (input$loss_function == "Hubera") {
      loss <- huber_loss(residuals, input$threshold)
    } else {
      loss <- 0.5 * residuals^2 # MNK
    }
    
    df <- data.frame(Predykcje = predictions, Reszty = residuals, Strata = loss)
    
    p <- ggplot(df, aes(x = Predykcje, y = Reszty, color = Strata)) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Reszty vs Predykcje", x = "Wartości Przewidywane", y = "Reszty") +
      scale_color_gradient(low = "blue", high = "red")
    
    ggplotly(p)
  })
  
  output$plot_loss <- renderPlotly({
    fit <- modelFit()
    model <- fit$model
    data <- fit$data
    residuals <- residuals(model)
    
    # Obliczanie straty
    if (input$loss_function == "Tukey's Biweight") {
      loss <- tukey_biweight_loss(residuals, input$threshold)
    } else if (input$loss_function == "Hubera") {
      loss <- huber_loss(residuals, input$threshold)
    } else {
      loss <- 0.5 * residuals^2 # MNK
    }
    
    df <- data.frame(Reszty = residuals, Strata = loss)
    
    p <- ggplot(df, aes(x = Reszty, y = Strata)) +
      geom_point(size = 2, color = "darkgreen") +
      theme_minimal() +
      labs(title = paste(input$loss_function, "- Funkcja Straty"), x = "Reszty", y = "Strata")
    
    ggplotly(p)
  })
  
  output$plot_influence <- renderPlotly({
    fit <- modelFit()
    model <- fit$model
    data <- fit$data
    residuals <- residuals(model)
    threshold <- input$threshold
    
    if (input$loss_function == "Tukey's Biweight") {
      u <- residuals / threshold
      influence <- ifelse(abs(u) <= 1, residuals * (1 - u^2)^2, 0)
    } else if (input$loss_function == "Hubera") {
      influence <- ifelse(abs(residuals) <= threshold, residuals, threshold * sign(residuals))
    } else {
      influence <- residuals # MNK
    }
    
    df <- data.frame(Reszty = residuals, Wpływ = influence)
    
    p <- ggplot(df, aes(x = Reszty, y = Wpływ)) +
      geom_point(size = 2, color = "purple") +
      theme_minimal() +
      labs(title = "Funkcja Wpływu", x = "Reszty", y = "Wpływ")
    
    ggplotly(p)
  })
}

shinyApp(ui, server)



