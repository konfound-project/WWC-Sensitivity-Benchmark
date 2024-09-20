library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)

library(shinyjs)
library(shinyscreenshot)

# install.packages("remotes")
#remotes::install_github("deepanshu88/shinyDarkmode")
library(shinyDarkmode)


################################################################################

#data <- readxl::read_excel("Shiny_Data.xlsx")
data <- readRDS("wwc-shiny-13Sep2023.RDS")

################################################################################



jscode <- "shinyjs.refresh_page = function() { history.go(0); }" 


server <- function(input, output, session) {
  
  darkmode(buttonColorDark = "#7f9f3d",  # Background color of the button while in lightmode
           buttonColorLight = "#639dad",  # Background color of the button while in darkmode
           backgroundColor = "#fff",  # Background color of the page while in lightmode
           mixColor = "#fff",  # Color used to generate darkmode: lighter colors create darker darkmode
           label = "<strong>|</strong>",  # Text that shows up on the darkmode button
           bottom = "32px",
           right = "16px",
           autoMatchOsTheme = TRUE
  )
  
################################################################################
  ###### GENERATE LINEAR RIR/ITCV RESULTS ###### 
################################################################################
  
  filtered_data <- reactive({
    filter_condition <- rep(TRUE, nrow(data))
    if (input$selectedDomain != "All") {
      if (input$selectedDomain == "Academic Readiness, Knowledge, or Skills (Pre-K through Postsecondary)") {
        filter_condition <- filter_condition & data$`Outcome Domain Group` == input$selectedDomain  
        } else {
            filter_condition <- filter_condition & data$`Outcome Domain Group Expanded` == input$selectedDomain  
        }
    }
    if (input$selectedStudyDesign != "All") {
      filter_condition <- filter_condition & data$s_Study_Design == input$selectedStudyDesign
    }
    if (input$selectedDichCont != "All") {
      #filter_condition <- filter_condition & data$`Dichotomous or Continuous` == input$selectedDichCont
      filter_condition <- filter_condition & data$outcome_type == input$selectedDichCont
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
          variable_to_plot == "RIR_primary" ~ "RIR values",
          variable_to_plot == "RIR_percent" ~ "RIR as a percentage of Sample Size values",
          TRUE ~ ""
        ),
        y = "Frequency") +
        theme(plot.title = element_text(size = 16, face = "bold"))
      
      if (variable_to_plot == "RIR_primary") {
        p <- p + scale_x_continuous(labels = scales::comma, limits = c(0, 1000))
      } else {
        p <- p + scale_x_continuous(labels = scales::comma)
      }
      
      if (!is.null(input$userValue) && !is.na(input$userValue) && 
          (input$selectedVariable == "RIR_primary" || 
           input$selectedVariable == "RIR_percent" || 
           input$selectedVariable == "fragility_primary.lo")) {
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
            numeric_values <- round(numeric_values)
            stats <- summary(numeric_values)
            stats <- round(stats,2)
            count <- length(numeric_values)
            sd_value <- sd(numeric_values,2)
        
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
    
    if (input$selectedVariable %in% c("RIR_primary", "RIR_percent","fragility_primary.lo")) {
      user_value <- input$userValue
      filtered_data_subset <- filtered_data()[, input$selectedVariable]
      
      if (is.null(user_value)) {
        "Input your value"
      } else if (!is.na(user_value) && user_value >= 0) {
        percentile <- mean(filtered_data_subset <= user_value, na.rm = TRUE) * 100
       # paste0("Your value of ", user_value, " is in the ", round(percentile, 2), "th percentile.")
        paste0("Your value of ", user_value, " is equal to or greater than ", round(percentile), "% of the values in the selected reference distribution.")
      } else {
        ""
      }
    } else {
      ""
    }
  })
  
  
  
}
