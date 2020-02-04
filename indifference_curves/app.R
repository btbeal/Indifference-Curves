# ---------------------------------------------------------- #
#
#   Indifference Curves
#
# ---------------------------------------------------------- #


library(shiny)
library(plotly)
library(tidyverse)
library(shinyWidgets)

source("indifference_functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  setSliderColor("purple", sliderId = 1),
  sidebarPanel(
         wellPanel(
         chooseSliderSkin("Modern"),
         sliderInput("budget",
                     "Input your proposed budget: ",
                     min = 10,
                     max = 50,
                     value = 30))),
  mainPanel(
         plotlyOutput("budget_plot", height = "450px"),
         textOutput("optimal_points"))
)

server <- function(input, output) {
   
  
  
   output$budget_plot <- renderPlotly({             # rendering plotly with budget
     
     user_defined_data <- budget_plot(input$budget) # budget_plot function from "indifference_functions.R"
     plot_ly() %>% 
       add_surface(z = ~goods_matrix, showscale = FALSE,
                   contours = list(
                     z = list(
                       show=TRUE,
                       usecolormap=TRUE,
                       highlightcolor="#ff0000",
                       project=list(z=TRUE)
                     ),
                     size = 90
                   )) %>%
       add_trace(data = user_defined_data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
                 line = list(color = 'black', width = 9)) %>% 
       layout(
         showlegend = FALSE,
         scene = list(
           camera=list(
             eye = list(x=1.87, y=0.88, z=1)
           ),
           xaxis = x_axis,
           yaxis = y_axis,
           zaxis = z_axis
         )
       )
   })
   
   
   output$optimal_points <- renderText({
     # this is the systems of equations solved for:
     # ---- Utility = x^0.6y^0.4
     # ---- Budget  = (1/4)x + (1/2)y
     #
     # notably, if these change, so will the dynamics of x and y
     I <- input$budget
     
     x <- (12/5)*I
     y <- 2*(I - (12/20)*I)
     
     print(paste("Your optimal points are x =", x,"and y =", y))
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

