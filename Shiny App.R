library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

ui <- fluidPage(
  "Cumulative Paid Claims",
  fileInput("upload",
            label = "Upload one CSV or Excel file",
            multiple = FALSE,
            accept = c(".xlsx",".csv"),
            buttonLabel = "Upload"),
  dataTableOutput("original_table"),
  sliderInput("tail_factor",
              label = "Tail Factor:",
              min = 0,
              max = 2,
              value = 1.1,
              step = 0.1),
  tableOutput("cumulative_table"),
  plotOutput("cumulative_plot")
)

server <- function(input, output, session) {
  
  uploaded_file <- reactive({
    req(input$upload)
    file <- input$upload$datapath
    if (grepl(".csv", input$upload$name)) {
      read.csv(file, stringsAsFactors = FALSE)
    } else if (grepl(".xlsx", input$upload$name)) {
      readxl::read_excel(file) %>%
        mutate(
          `Amount of Claims Paid ($)` = comma(`Amount of Claims Paid ($)`, accuracy = 1),
          `Development Year` = comma(`Development Year`, accuracy = 1)
        )
    }
  })
  
  cumulative_data <- reactive({
    claims <- uploaded_file()
    tail_factor <- input$tail_factor
    if (!is.null(claims)) {
      claims <- claims %>%
        mutate(`Amount of Claims Paid ($)` = as.numeric(gsub(",", "", `Amount of Claims Paid ($)`)))
      
      unique_loss_years <- unique(claims$`Loss Year`)
      unique_dev_years <- c(1, 2, 3, 4)
      
      cumulative_matrix <- matrix(0, nrow = length(unique_loss_years), ncol = length(unique_dev_years) + 1)
      colnames(cumulative_matrix) <- c("Loss Year", paste("Development Year", unique_dev_years))
      cumulative_matrix[, 1] <- unique_loss_years
      
      for (j in 1:length(unique_dev_years)) {
        dev_year <- unique_dev_years[j]
        
        for (i in 1:length(unique_loss_years)) {
          loss_year <- unique_loss_years[i]
          claims_subset <- claims[claims$`Loss Year` == loss_year & claims$`Development Year` <= dev_year, ]
          cumulative_matrix[i, j + 1] <- sum(claims_subset$`Amount of Claims Paid ($)`)
          
          if (dev_year == max(unique_dev_years) && !is.null(tail_factor)) {
            cumulative_matrix[i, j + 1] <- cumulative_matrix[i, j] * tail_factor
          }
          
          if (loss_year == max(unique_loss_years) - 1 && dev_year == max(unique_dev_years) - 1 && loss_year > min(unique_loss_years) && dev_year > min(unique_dev_years) + 1) {
            cumulative_matrix[i, j + 1] <- cumulative_matrix[i, j] *
              cumulative_matrix[i - 1, j+1] / cumulative_matrix[i - 1, j]
          }
          
          if (loss_year == max(unique_loss_years) && dev_year == max(unique_dev_years) - 2) {
            cumulative_matrix[i, j + 1] <- ((cumulative_matrix[i-1, j+1] + cumulative_matrix[i-2, j+1]) / (cumulative_matrix[i-1, j] + cumulative_matrix[i-2, j])) *
              cumulative_matrix[i, j]
          }
          
          if (loss_year == max(unique_loss_years) && dev_year == max(unique_dev_years) - 1) {
            cumulative_matrix[i, j + 1] <- cumulative_matrix[i, j] * cumulative_matrix[i-2, j+1] / cumulative_matrix[i-2, j]
          }
        }
      }
      
      as.data.frame(cumulative_matrix)
    } else {
      NULL
    }
  })
  
  output$original_table <- renderDataTable({
    uploaded_file()
  })
  
  output$cumulative_table <- renderTable({
    cumulative_data()
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot_data <- cumulative_data()
    
    if (!is.null(cumulative_plot_data)) {
      plot_data <- cumulative_plot_data %>%
        gather(key = "Development_Year", value = "Cumulative_Paid_Claims", starts_with("Development Year")) %>%
        mutate(Development_Year = as.numeric(gsub("Development Year ", "", Development_Year)))
      
      ggplot(data = plot_data, aes(x = Development_Year, y = Cumulative_Paid_Claims, color = factor(`Loss Year`))) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = format(round(Cumulative_Paid_Claims), big.mark = ",")), vjust = -0.5) +
        labs(x = "Development Year", y = "Cumulative Paid Claims ($)",
             title = "Cumulative Paid Claims by Development Year") +
        scale_color_discrete(name = "Loss Year")
    }
  })
}

shinyApp(ui, server)


