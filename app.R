 library(shiny)
 source("run.R", local = TRUE)

ui <- page_navbar(
  title = tags$span(
    tags$img(),
    "Anomalyze"
  ),
  sidebar = sidebar(
    
  ),
  nav_spacer(),
  nav_panel(
    "Analysis",
    layout_columns(
      col_widths = c(4, 8),
      max_height = "600px",
      card(
        h3(strong("Key Statistics")),
        tagList(
          tags$dl(
            class = "row",
            f.pred_txtstats(ldfRQ$Test$Ozone, ldfRQ$Test$Prediction, 5)
          )
        )
      ),
      card(
        plotOutput("plot_distributuin")
      )
    ),
    layout_columns(
      col_widths = 12,
      navset_card_tab(
        nav_panel(title = "Actuals vs. Predicted", plotOutput("plot_actualvpredicted")),
        nav_panel(title = "Residuals vs. Predicted", plotOutput("plot_residualvpredicted"))
      )
    )
  ),
  nav_panel(
    "Initialize",
    layout_column_wrap(
      width = 1,
      card(
      )
    )
  ),
)

server <- function(input, output, session) {
  
  dfs_RQ <- reactiveValues(Test = ldfRQ$Test, Learn = ldfRQ$Learn)
  
  output$plot_distributuin <- renderPlot({
    req(dfs_RQ)
    
    dfs_RQ$Test |> 
      select(Ozone, Prediction) |>
      pivot_longer(names_to = "Type", values_to = "Value", cols = 1:2) |>
      ggplot(aes(
        y = Value,
        x = Type,
        color = Type,
        fill = Type
      )) +
      geom_half_boxplot(
        side = "l", alpha = 0.5,
        outlier.shape = NA
      ) +
      geom_half_violin(
        side = "r", alpha = 0.5,
        trim = TRUE
      ) +
      geom_half_point(
        side = "r", alpha = 0.8,
        position = position_jitter(width = 0.1), 
        size = 2, color = "black"
      ) +
      facet_wrap(~Type, scale = "free_x") +
      scale_color_viridis_d(direction = -1) +
      scale_fill_viridis_d(direction = -1) +
      theme_classic() +
      theme(legend.position = "none") 
    
  })
  
  output$plot_residualvpredicted <- renderPlot({
    req(dfs_RQ)
    
    dfs_RQ$Test |> 
      ggplot(aes(
        y = Prediction - Ozone,
        x = Prediction,
      )) + 
      geom_segment(aes(yend = 0)) +
      geom_point(size = 5) +
      geom_hline(yintercept = 0) +
      theme_minimal()
  })
  
  output$plot_actualvpredicted <- renderPlot({
    req(dfs_RQ)
    
    dfs_RQ$Test |> 
      ggplot(aes(
        y = Prediction,
        x = Ozone,
      )) + 
      geom_segment(aes(yend = Ozone)) +
      geom_point(size = 3) +
      geom_abline(slope = 1) +
      scale_x_continuous(limits = c(0, 125)) + 
      scale_y_continuous(limits = c(0, 125)) +
      theme_minimal()
  })
  
}

shinyApp(ui, server)
