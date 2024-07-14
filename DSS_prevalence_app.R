#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# # Load necessary libraries
library(dplyr)
library(ggplot2)
library(shiny)
library(readxl)
library(shiny)

# Load data
path_to_file <- "C:/Users/maria/OneDrive/Asztali gép/Work/Manawanui/Descriptives/manawanui_output1_total_for_release.xlsx"
data_2021 <- read_excel(path_to_file, sheet = "2021 confidentialised data")
data_2018 <- read_excel(path_to_file, sheet = "2018 confidentialised")
data_2018$pop_group <- ifelse(data_2018$pop_group == "DSS in 2018", "DSS in Given Year", data_2018$pop_group)
merged_data <- bind_rows(data_2021, data_2018)



# Function to clean 'n' column only
clean_transition_data <- function(df) {
  df %>%
    mutate(n = replace(n, n == "S", "0"),  # Replace "S" with "0" in column 'n'
           n = as.numeric(n))              # Convert column 'n' to numeric
}

# Clean data
data_2021 <- clean_transition_data(data_2021)
data_2018 <- clean_transition_data(data_2018)
merged_data <- bind_rows(data_2021, data_2018)

calculate_prevalence_ag <- function(data, target_indicator, target_value) {
  # Step 1: Filter the data for the specified indicator
  prepared_data <- data %>%
    filter(indicator == target_indicator)
  
  # Step 2: Convert target_value to the appropriate type based on data column type
  if (is.numeric(prepared_data$value[1])) {
    target_value <- as.numeric(target_value)
  }
  
  # Step 3: Calculate the total population for each year and age group
  total_population <- prepared_data %>%
    filter(pop_group %in% c("DSS in Given Year", "DSS in past", "Non-DSS disabled", "Rest of pop")) %>%
    group_by(Year, age_group) %>%
    summarise(Total_Population = sum(as.numeric(n), na.rm = TRUE))
  
  # Step 4: Calculate total counts with the specified value in the total population
  total_counts_with_value <- prepared_data %>%
    filter(value == target_value, pop_group %in% c("DSS in Given Year", "DSS in past", "Non-DSS disabled", "Rest of pop")) %>%
    group_by(Year, age_group) %>%
    summarise(Count_with_value_total = sum(as.numeric(n), na.rm = TRUE))
  
  # Step 5: Calculate prevalence for "DSS in Given Year" only
  prevalence_data <- prepared_data %>%
    filter(pop_group == "DSS in Given Year") %>%
    group_by(Year, pop_group, age_group) %>%
    summarise(
      Total_n = sum(as.numeric(n), na.rm = TRUE),
      Count_with_value = sum(as.numeric(n[value == target_value]), na.rm = TRUE),
      Prevalence = (Count_with_value / Total_n) * 100,
      .groups = 'drop'
    ) %>%
    left_join(total_population, by = c("Year", "age_group")) %>%
    left_join(total_counts_with_value, by = c("Year", "age_group")) %>%
    mutate(
      Prevalence_in_total_pop = (Count_with_value_total / Total_Population) * 100,
      Difference = Prevalence - Prevalence_in_total_pop
    )
  
  # Print the prevalence data
  print(prevalence_data)
  
  # Plotting the prevalence data with an additional facet for age groups
  plot <- ggplot(prevalence_data, aes(x = pop_group, y= Prevalence, fill = as.factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) +
    labs(title = paste("Prevalence of", target_indicator, "by Population Group, Age Group, and Year (value =", target_value, ")"),
         x = "Population Group",
         y = "Prevalence (%)",
         fill = "Year") +
    facet_wrap(~age_group, scales = "free_x") +  # Facet by age group
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")
  
  return(list(Prevalence_Data = prevalence_data, Plot = plot))
}

## Shiny App: Prevalence of Indicators

# Shiny UI
ui <- fluidPage(
  titlePanel("Prevalence Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Select Indicator:", choices = unique(merged_data$indicator)),
      uiOutput("value_ui"),
      actionButton("update", "Update")
    ),
    
    mainPanel(
      plotOutput("prevalencePlot", width = "100%", height = "800px"),
      tableOutput("prevalenceTable")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Update the value dropdown based on the selected indicator
  output$value_ui <- renderUI({
    req(input$indicator)
    values <- unique(merged_data %>% filter(indicator == input$indicator) %>% pull(value))
    selectInput("value", "Select Value:", choices = values)
  })
  
  result <- eventReactive(input$update, {
    calculate_prevalence_ag(merged_data, input$indicator, input$value)
  })
  
  output$prevalencePlot <- renderPlot({
    result()$Plot
  })
  
  output$prevalenceTable <- renderTable({
    result()$Prevalence_Data %>%
      select(Year, pop_group, age_group, Total_n, Count_with_value, Prevalence, Prevalence_in_total_pop, Difference)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
