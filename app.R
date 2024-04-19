

library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)

data <- readxl::read_excel("Shiny_Data.xlsx")

# Start of App
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("What Works Clearinghouse Sensitivity Analysis Benchmarks (Beta)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedStudyDesign", "Choose a Study Design:",
                  choices = c("All", setdiff(unique(data$s_Study_Design), c(NA, "?", "Uncertain"))),
                  selected = "All"),
      selectInput("selectedDomain", "Choose an Outcome Domain Group:",
                  choices = c("All", sort(setdiff(unique(data$`Outcome Domain Group Expanded`), c(NA, "?", "Uncertain")))),
                  selected = "All"),
      selectInput("selectedDichCont", "Choose Dichotomous or Continuous Outcome Measures:",
                  choices = c("All", setdiff(unique(data$`Dichotomous or Continuous`), c(NA, "?", "Uncertain"))),
                  selected = "All"),
      selectInput("selectedFindingRating", "Choose WWC Finding Rating:",
                  choices = c("All", setdiff(unique(data$f_Finding_Rating), c(NA, "?", "Uncertain"))),
                  selected = "All"),
      selectInput("selectedVariable", "Choose a Sensitivity Measure:",
                  choices = c("Robustness of Inferences to Replacement (RIR)" = "RIR.g", "RIR as a percentage of Sample Size" = "RIR_percent", "Unselected"),
                  selected = "Unselected")
    ),
    mainPanel(
      plotOutput("histPlot", height = "400px"),
      br(),
      wellPanel(
        h4("Descriptive Statistics"),
        div(
          style = "overflow-x: auto;",
          tableOutput("descriptiveStatsTable")
        ),
        br(),
        h4("Place Your Value in Distribution"),
        numericInput("userValue", "Enter your value:", value = NULL),
        textOutput("percentileResult")
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    filter_condition <- rep(TRUE, nrow(data))
    if (input$selectedDomain != "All") {
      filter_condition <- filter_condition & data$`Outcome Domain Group Expanded` == input$selectedDomain
    }
    if (input$selectedStudyDesign != "All") {
      filter_condition <- filter_condition & data$s_Study_Design == input$selectedStudyDesign
    }
    if (input$selectedDichCont != "All") {
      filter_condition <- filter_condition & data$`Dichotomous or Continuous` == input$selectedDichCont
    }
    if (input$selectedFindingRating != "All") {
      filter_condition <- filter_condition & data$f_Finding_Rating == input$selectedFindingRating
    }
    data[filter_condition, ]
  })
  
  output$histPlot <- renderPlot({
    variable_to_plot <- input$selectedVariable
    if (variable_to_plot != "Unselected" && nrow(filtered_data()) > 0) {
      p <- ggplot(filtered_data(), aes_string(x = variable_to_plot)) +
        geom_histogram(bins = 30, fill = "#619CFF", color = "#619CFF", alpha = 0.8) +
        theme_minimal() +
        labs(x = case_when(
          variable_to_plot == "RIR.g" ~ "RIR values",
          variable_to_plot == "RIR_percent" ~ "RIR as a percentage of Sample Size values",
          TRUE ~ ""
        ),
        y = "Frequency") +
        theme(plot.title = element_text(size = 16, face = "bold"))
      
      if (variable_to_plot == "RIR.g") {
        p <- p + scale_x_continuous(labels = scales::comma, limits = c(0, 1000))
      } else {
        p <- p + scale_x_continuous(labels = scales::comma)
      }
      
      if (!is.null(input$userValue) && !is.na(input$userValue) && 
          (input$selectedVariable == "RIR.g" || input$selectedVariable == "RIR_percent")) {
        p <- p + geom_vline(xintercept = input$userValue, color = "red", linetype = "solid")
      }
      
      print(p)
    } else {
      plot.new()
      text(0.5, 0.5, "", cex = 1.2)
    }
  })
  
  output$descriptiveStatsTable <- renderTable({
    variable_to_analyze <- input$selectedVariable
    filtered_data_subset <- filtered_data()
    
    if (variable_to_analyze == "Unselected") {
      data.frame(Message = "Please select a sensitivity measure from the drop down box to the left to retrieve information for desired reference distribution.")
    } else if (nrow(filtered_data_subset) > 0) {
      numeric_values <- filtered_data_subset[, variable_to_analyze]
      numeric_values <- numeric_values[!is.na(numeric_values)]
      
      if (length(numeric_values) > 0) {
        if (variable_to_analyze == "RIR.g") {
          numeric_values <- round(numeric_values)
          stats <- summary(numeric_values)
          stats <- round(stats)
        } else if (variable_to_analyze == "RIR_percent") {
          stats <- summary(numeric_values)
          stats <- round(stats, 2)
        }
        
        count <- length(numeric_values)
        sd_value <- sd(numeric_values)
        
        if (variable_to_analyze == "RIR.g") {
          sd_value <- round(sd_value)
        } else if (variable_to_analyze == "RIR_percent") {
          sd_value <- round(sd_value, 2)
        }
        
        stat_names <- c("Count", "Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", "Standard Deviation")
        stat_table <- data.frame(
          Statistic = stat_names,
          Value = c(count, stats[1], stats[2], stats[3], stats[4], stats[5], stats[6], sd_value)
        )
        colnames(stat_table) <- c("Statistic", paste("Value"))
        
        stat_table
      } else {
        data.frame(Message = "No numeric data available for the selected variable.")
      }
    } else {
      data.frame(Message = "No data available for the selected filters.")
    }
  }, spacing = "xs", width = "100%",
  bordered = TRUE, hover = TRUE, striped = TRUE, responsive = TRUE)
  
  output$percentileResult <- renderText({
    req(input$selectedVariable, input$userValue)
    
    if (input$selectedVariable %in% c("RIR.g", "RIR_percent")) {
      user_value <- input$userValue
      filtered_data_subset <- filtered_data()[, input$selectedVariable]
      
      if (is.null(user_value)) {
        "Input your value"
      } else if (!is.na(user_value) && user_value >= 0) {
        percentile <- mean(filtered_data_subset <= user_value, na.rm = TRUE) * 100
        paste0("Your value of ", user_value, " is in the ", round(percentile, 2), "th percentile.")
      } else {
        ""
      }
    } else {
      ""
    }
  })
}

shinyApp(ui = ui, server = server)