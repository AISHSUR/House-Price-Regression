library(shiny)
library(tidyverse)
library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')
library('naniar')
library(shinydashboard)
library(DT)

## Importing dataset
testData <-  read.csv("test.csv")
trainData <- read.csv("train.csv")
priceData <- read.csv("House_Price_Ash.csv")

## Training
categorical_var <- names(trainData)[which(sapply(trainData, is.character))]
cat_car <- c(categorical_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(trainData)[which(sapply(trainData, is.numeric))]

## Creating one training dataset for categorical variable and one for numeric variable. We will use this for data visualization.
train1_cat<-trainData[categorical_var]
train1_num<-trainData[numeric_var]


#Dashboard Logic
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "House Price Regression Dashboard", titleWidth = 350 ),
    dashboardSidebar(
      width = 350,
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Price Prediction", tabName = "housePricePrediction", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                      h3("Dataset Summary"),
                      h4("The Ames Housing Dataset contains 79 variables. The dashboard attempts to capture the Explanatory
                          Data Analysis of the dataset.House price is predicted by fitting an appropriate prediction model on the dataset"),
                      h3("EDA Analysis of the Data Set"),
                      fluidRow(
                        box(
                            title = "Heatmap to visualize correlation", status = "primary", solidHeader = TRUE,collapsible = TRUE,
                            plotOutput("corPlot"),
                            h4("Observations:Features that show high correlation would predict the House Price more accurately")
                           ),
                        box(
                          title = "Scatter Plot", status = "primary", solidHeader = TRUE,collapsible = TRUE,
                          plotOutput("scatterPlot"),
                          h4("Observations:Scatter Plot helped visualise the positive correlation between features")
                          ),
                        box(
                          title = "Percentage of Missing Data in Dataset ", status = "primary", solidHeader = TRUE,collapsible = TRUE,
                          plotOutput("missingData"),
                          h4("Observations: Features like FireplaceQu, Fence, Alley, PoolQC and MiscFeature have more than 50cent missing data. Reason for 
                             missing data would need to be further investigated")
                        ),
                      )
            ),
            tabItem(tabName = "housePricePrediction",
                    h3("Displaying Predicted House price against each ID"),
                    h4("Prediction Model used- Random Forest"),
                    fluidRow(
                      dataTableOutput("table"),
                      plotOutput("predictionPlot")
                    )
                    )
        )
        )
    )



# Defining server logic required to draw a histogram
server <- function(input, output) {
    # BarPlot for categorical features
    output$catPlot1 <- renderPlot({
        plotHist <- function(data_in, i) 
        {
            catPlotdata <- data.frame(x=data_in[[i]])
            pd <- ggplot(data=catPlotdata, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + 
                theme(axis.text.x = element_text(angle = 90, hjust =1))
            return (pd)
        }
        ## Density plot function
        plotDen <- function(data_in, i){
            plotDenData <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
            pd <- ggplot(data= plotDenData) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
                xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2)))
            return(pd)
            
        }
        doPlots <- function(data_in, fun, ii, ncol=3) 
        {
            pp <- list()
            for (i in ii) {
                p <- fun(data_in=data_in, i=i)
                pp <- c(pp, list(p))
            }
            do.call("grid.arrange", c(pp, ncol=ncol))
        }
        ## Barplots for the categorical features
        doPlots(train1_cat, fun = plotHist, ii = 1:4, ncol = 2)
    })
    output$catPlot2 <- renderPlot({
      doPlots(train1_cat, fun = plotHist, ii  = 4:8, ncol = 2)
    })
    
    
    #Correlation Plot
    output$corPlot <- renderPlot({
      train_cont <- trainData[numeric_var]
      correlations <- cor(na.omit(train_cont))
      rowIndex <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
      correlations<- correlations[rowIndex ,rowIndex]
      corrplot(correlations, method="circle")
    })
    
    #ScatterPlot
    output$scatterPlot <- renderPlot({
      plotCorr <- function(data_in, i){
        data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
        p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
        return(suppressWarnings(p))
      }
      highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)], names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])
      data_corr <- trainData[,highcorr]
      doPlots(data_corr, fun = plotCorr, ii = 1:3)
    })
    
    #MissingData
    output$missingData <- renderPlot({
      gg_miss_var(testData,show_pct = TRUE)
    })
    
    output$table <- renderDataTable({
      priceData
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
