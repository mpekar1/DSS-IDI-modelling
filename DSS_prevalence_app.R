# Load necessary libraries
library(dplyr)
library(ggplot2)
library(shiny)
library(readxl)

# Load data with error handling
path_to_file <- "C:/Users/maria/OneDrive/Asztali gép/Work/Manawanui/Descriptives/manawanui_output1_total_for_release.xlsx"
data_2021 <- tryCatch(read_excel(path_to_file, sheet = "2021 confidentialised data"), error = function(e) stop("Error loading 2021 data"))
data_2018 <- tryCatch(read_excel(path_to_file, sheet = "2018 confidentialised"), error = function(e) stop("Error loading 2018 data"))

# Correct population group names
data_2018 <- data_2018 %>% mutate(pop_group = ifelse(pop_group == "DSS in 2018", "DSS in Given Year", pop_group))

# Clean 'n' column in data
clean_transition_data <- function(df) {
  df %>% mutate(n = as.numeric(replace(n, n == "S", "0")))
}

data_2021 <- clean_transition_data(data_2021)
data_2018 <- clean_transition_data(data_2018)
merged_data <- bind_rows(data_2021, data_2018)

# Precalculate unique categories and indicators for UI
category_values <- unique(merged_data$indicator_category)
indicator_values <- merged_data %>% group_by(indicator_category, indicator) %>% summarize(values = list(unique(value)))

# Calculate prevalence and generate plot
calculate_prevalence_ag <- function(data, target_indicator, target_value) {
  prepared_data <- data %>% filter(indicator == target_indicator)
  target_value <- if (is.numeric(prepared_data$value[1])) as.numeric(target_value) else target_value
  
  total_population <- prepared_data %>%
    filter(pop_group %in% c("DSS in Given Year", "DSS in past", "Non-DSS disabled", "Rest of pop")) %>%
    group_by(Year, age_group) %>%
    summarise(Total_Population = sum(n, na.rm = TRUE))
  
  total_counts_with_value <- prepared_data %>%
    filter(value == target_value, pop_group %in% c("DSS in Given Year", "DSS in past", "Non-DSS disabled", "Rest of pop")) %>%
    group_by(Year, age_group) %>%
    summarise(Count_with_value_total = sum(n, na.rm = TRUE))
  
  prevalence_data <- prepared_data %>%
    filter(pop_group == "DSS in Given Year") %>%
    group_by(Year, pop_group, age_group) %>%
    summarise(
      Total_n = sum(n, na.rm = TRUE),
      Count_with_value = sum(n[value == target_value], na.rm = TRUE),
      Prevalence = (Count_with_value / Total_n) * 100,
      .groups = 'drop'
    ) %>%
    left_join(total_population, by = c("Year", "age_group")) %>%
    left_join(total_counts_with_value, by = c("Year", "age_group")) %>%
    mutate(
      Year = as.integer(Year),
      Prevalence_in_total_pop = (Count_with_value_total / Total_Population) * 100,
      Difference = Prevalence - Prevalence_in_total_pop
    )
  
  # Create the reshaped data for plotting
  prevalence_data_plot <- prevalence_data %>%
    pivot_longer(cols = c("Prevalence", "Prevalence_in_total_pop"),
                 names_to = "Prevalence_Type", 
                 values_to = "Prevalence_Value")
  
  # Custom color palette
  berry_hues <- c("#5A0046", "#56C5CB", "#ED7D9B", "#FACBDB") # Add more colors if needed
  
  plot <- ggplot(prevalence_data_plot, aes(x = pop_group, y = Prevalence_Value, fill = interaction(Prevalence_Type, Year))) +
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) +
    labs(title = paste("Prevalence of", target_indicator, "by Population Group, Age Group, and Year (value =", target_value, ")"),
         x = "Population Group",
         y = "Prevalence (%)",
         fill = "Prevalence Type / Year") +
    facet_wrap(~age_group, scales = "free_x") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = berry_hues)
  
  list(Prevalence_Data = prevalence_data, Plot = plot)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Prevalence Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator_category", "Select Indicator Category:", choices = category_values),
      uiOutput("indicator_ui"),
      uiOutput("value_ui"),
      actionButton("update", "Update")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("prevalencePlot", width = "100%", height = "800px")),
        tabPanel("Data Table", tableOutput("prevalenceTable"))
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  output$indicator_ui <- renderUI({
    req(input$indicator_category)
    indicators <- indicator_values %>% filter(indicator_category == input$indicator_category) %>% pull(indicator)
    selectInput("indicator", "Select Indicator:", choices = indicators)
  })
  
  output$value_ui <- renderUI({
    req(input$indicator)
    values <- indicator_values %>% filter(indicator == input$indicator) %>% pull(values) %>% unlist()
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
      select(Year, pop_group, age_group, Total_n, Count_with_value, Prevalence, Prevalence_in_total_pop, Difference) %>%
      setNames(c("Year", "Population Group", "Age Group", "Total DSS Population", 
                 "DSS Population with Selected Indicator Value", "Prevalence in DSS Population", 
                 "Prevalence in Total Population", "Difference"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
