#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)

# folder setup
setwd("~/R/nba_forecasting")
data_folder <- "data"
output_folder <- "output"
df_file <- file.path(data_folder, "modeling_df.csv")

# load data
df <- read.csv(df_file)
df

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Live NBA Predictions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("games_day",
                      "Date: ",
                      value = Sys.Date())),


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# predict on new data set




# Define server logic required to draw a histogram
server <- function(input, output) {
    # input$Season <- "2019-20"ç
    output$games_preds <- renderValueBox({
        
        predictions <- readRDS(model_file) %>% 
            predict(new_games)
        
        probabilities <- readRDS(model_file) %>% 
            predict(new_games, type = "prob")
        
        pred_df <- new_games %>% 
            select(idGame, dateGame, isWin, slugTeam, slugOpponent) %>% 
            bind_cols(predictions) %>% 
            bind_cols(probabilities) %>% 
            mutate(correct = case_when(isWin == .pred_class ~ "Correct", TRUE ~ "Incorrect"))
    }
    )
        pred_df
    })
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        this_day    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
