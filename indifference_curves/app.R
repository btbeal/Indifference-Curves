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
           "Once you input your budget, drag your cursor to see where the slopes of both align! (It may be helpful to drage the graph around to view from the bottom)",
           br(),
           br(),
         chooseSliderSkin("Modern"),
         sliderInput("budget",
                     "Input your proposed budget: ",
                     min = 10,
                     max = 50,
                     value = 30))),
  mainPanel(
         plotlyOutput("budget_plot", height = "450px"))
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
                 line = list(color = 'black', width = 2)) %>% 
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
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

