library(arrow)
library(tidyverse)
Subset_V4<- test_data
  #read_parquet("Subset_V4.parquet")



# Assuming Subset_V2 is your dataframe
library(e1071)
# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets (e.g., 60% training, 40% test)
train_indices <- sample(1:nrow(Subset_V4), size = 0.6 * nrow(Subset_V4))
train_data <- Subset_V4[train_indices, ]
test_data <- Subset_V4[-train_indices, ]

library (xgboost)
# Convert training data to DMatrix format
dtrain <- xgb.DMatrix(data = data.matrix(train_data[, -which(names(train_data) == "Final_Energy_KWH")]), 
                      label = train_data$Final_Energy_KWH)

params2 <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 8,
  subsample = 0.5,
  colsample_bytree = 0.5
)

nrounds2 <- 1000  # Number of boosting rounds. Adjust based on your dataset and needs

xgb_model2 <- xgboost(params = params2, data = dtrain, nrounds = nrounds2)

# Assuming you have a trained XGBoost model 'xgb_model' and a test set 'test_data'

# Predict on the test set
dtest2 <- xgb.DMatrix(data = data.matrix(test_data[, -which(names(test_data) == "Final_Energy_KWH")]))
predictions2 <- predict(xgb_model2, dtest2)


(predictions2)
# Compute RMSE
rmse2 <- sqrt(mean((predictions2 - test_data$Final_Energy_KWH)^2))
print(paste("RMSE:", rmse2))

# Compute R-squared
SST2 <- sum((test_data$Final_Energy_KWH - mean(test_data$Final_Energy_KWH))^2)
SSR2 <- sum((predictions2 - test_data$Final_Energy_KWH)^2)
r_squared2<- 1 - SSR2/SST2
print(paste("R-squared:", r_squared2))

# Ensure that the feature names match those used in the model
# You might need to adjust this to match the exact features used
feature_names <- colnames(train_data[, -which(names(train_data) == "Final_Energy_KWH")])

# Obtain feature importance
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_model2)
print(importance_matrix)


importance_matrix <- xgb.importance(model = xgb_model2)
print(importance_matrix)
# Visualize feature importance
xgb.plot.importance(importance_matrix)

library(shiny)
library(shinythemes)
library(ggplot2)
library(readr)

# Load your dataset
# my_data <- read.csv("/Users/harshada/Desktop/Harshada/IST 687 - Introduction to Data Science (IDS)/Project/Sample_Modeling_DF.csv")
test_data <- na.omit(test_data)

# Define UI for the Shiny app
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Energy Consumption Visualization and Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("citySelect", "Select City:", 
                  choices = c("All Cities", unique(test_data$in.weather_file_city)), # Add "All Cities" option
                  selected = "All Cities"),  # Set "All Cities" as the default
      sliderInput("tempRange", "Temperature Range:",
                  min = min(test_data$Dry.Bulb.Temperature...C.), max = max(test_data$Dry.Bulb.Temperature...C.),
                  value = c(min(test_data$Dry.Bulb.Temperature...C.), max(test_data$Dry.Bulb.Temperature...C.))),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Energy Consumption by City", plotOutput("plot2")),
        tabPanel("Fuels Used in Each City", plotOutput("plot3")),
        tabPanel("Energy Consumed by Day of the Month", plotOutput("plot4")),
        tabPanel("Energy Consumption by Hour", plotOutput("plot5")),
        tabPanel("Energy Consumed in Relation to Temperature and Tenure Status", plotOutput("plot6")),
        tabPanel("Energy Consumption in One and Two Story Buildings", plotOutput("plot7")),
        tabPanel("Predictions", 
                 verbatimTextOutput("TestConfusionMatrix"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    if (input$citySelect == "All Cities") {
      data <- test_data
    } else {
      data <- test_data[test_data$in.weather_file_city == input$citySelect & 
                          test_data$Dry.Bulb.Temperature...C. >= input$tempRange[1] & 
                          test_data$Dry.Bulb.Temperature...C. <= input$tempRange[2], ]
    }
    return(data)
  })
  
  # Install and load necessary packages
  # install.packages("shinythemes")
  library(shiny)
  library(shinythemes)
  library(ggplot2)
  library(readr)
  
  # Load your dataset
  # my_data <- read.csv("/Users/harshada/Desktop/Harshada/IST 687 - Introduction to Data Science (IDS)/Project/Sample_Modeling_DF.csv")
  test_data <- na.omit(my_data)
  
  # Define UI for the Shiny app
  ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Energy Consumption Visualization and Prediction"),
    sidebarLayout(
      sidebarPanel(
        selectInput("citySelect", "Select City:", 
                    choices = c("All Cities", unique(test_data$in.weather_file_city)), # Add "All Cities" option
                    selected = "All Cities"),  # Set "All Cities" as the default
        sliderInput("tempRange", "Temperature Range:",
                    min = min(test_data$Dry.Bulb.Temperature...C.), max = max(test_data$Dry.Bulb.Temperature...C.),
                    value = c(min(test_data$Dry.Bulb.Temperature...C.), max(test_data$Dry.Bulb.Temperature...C.))),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Energy Consumption by City", plotOutput("plot2")),
          tabPanel("Fuels Used in Each City", plotOutput("plot3")),
          tabPanel("Energy Consumed by Day of the Month", plotOutput("plot4")),
          tabPanel("Energy Consumption by Hour", plotOutput("plot5")),
          tabPanel("Energy Consumed in Relation to Temperature and Tenure Status", plotOutput("plot6")),
          tabPanel("Energy Consumption in One and Two Story Buildings", plotOutput("plot7")),
          tabPanel("Predictions", 
                   verbatimTextOutput("TestConfusionMatrix"))
        )
      )
    )
  )
  
  server <- function(input, output) {
    
    # Reactive expression for filtered data
    filteredData <- reactive({
      if (input$citySelect == "All Cities") {
        data <- test_data
      } else {
        data <- test_data[test_data$in.weather_file_city == input$citySelect & 
                            test_data$Dry.Bulb.Temperature...C. >= input$tempRange[1] & 
                            test_data$Dry.Bulb.Temperature...C. <= input$tempRange[2], ]
      }
      return(data)
    })
    
    
    output$plot2 <- renderPlot({
      data <- filteredData()
      ggplot(data, aes(x = in.weather_file_city, y = out.total.energy_consumption)) + 
        geom_bar(stat = "identity", fill = "maroon") + 
        labs(title = "Energy Consumption by City", x = "City", 
             y = "Total Energy Consumption") +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$plot3 <- renderPlot({
      data <- filteredData()
      ggplot(data, aes(x = in.weather_file_city, fill = in.heating_fuel)) +
        geom_bar() +
        labs(title = "Types of Fuels", x = "City") +
        guides(color = guide_legend(title = "Fuel Type")) +
        theme(plot.title = element_text(hjust = 0.5))
    })
    output$plot4 <- renderPlot({
      data <- filteredData()
      ggplot(data, aes(x = as.factor(Day), 
                       y = out.total.energy_consumption)) + 
        geom_line() +
        labs(title = "Energy Consumed by Day of the Month", x = "Day of the Month", 
             y = "Energy Consumed") +
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    output$plot5 <- renderPlot({
      data <- filteredData()
      ggplot(data, aes(x = as.factor(Hour), 
                       y = out.total.energy_consumption)) + 
        geom_line() + 
        labs(title = "Energy Consumption by Hour", x = "Hour", 
             y = "Energy Consumption") +
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    output$plot6 <- renderPlot({
      data <- filteredData()
      ggplot(data, aes(x = Dry.Bulb.Temperature...C., 
                       y = out.total.energy_consumption, 
                       color = in.tenure)) + 
        geom_point() + 
        labs(title = "Energy Consumed in Relation to Temperature and Tenure Status", 
             x = "Dry Bulb Temperatures", y = "Total Energy Consumption") + 
        guides(color = guide_legend(title = "Tenure Status")) +
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    output$plot7 <- renderPlot({
      data <- filteredData()
      ggplot(data, aes(x = as.factor(in.geometry_stories), 
                       y = out.total.energy_consumption)) + 
        geom_boxplot() + 
        labs(title = "Energy Consumption in One and Two Story Buildings", 
             x = "Stories", y = "Energy Consumption") +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
  }
  shinyApp(ui = ui, server=server)

