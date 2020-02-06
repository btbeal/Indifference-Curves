# --- Proof of concept
library(plotly)
library(ggthemes)
library(Hmisc)
library(tidyverse)
# utility function where:
# --- x = Q_x        = quantity of bread
# --- y = Q_y        = quantity of wine
# --- z = U(Q_x,Q_y) = Utility | x amount of bread and y amount of wine

# --- Cobb-Douglas z(x,y) = x^a * y^(1-a)
# because plotly can only graph a budget plane on Z, we need to let y be our utility
# will need some mathematical transformation

a <- 0.6
utility <- function(x, y){
  (x^a)*(y^(1-a))
}


# create a goods array where:
# the first plane [,,1] = utilities
# the second plane [,,2] = price
goods_matrix <- matrix(0,
                     nrow = 200,
                     ncol = 200)


for(r in 1:nrow(goods_matrix)){
  for(c in 1:ncol(goods_matrix)){
      goods_matrix[r,c] <- utility(r,c)
      
  }
}

# budget function: 2x + 3y = z
budget <- function(x,y){
  z <- (1/4)*x + (1/2)*y
  z
}


budget_matrix <- matrix(0,
                      nrow = 100,
                      ncol = 100)

for(r in 1:nrow(budget_matrix)){
  for(c in 1:ncol(budget_matrix)){
    x <- r
    y <- c
    budget_matrix[r,c] <-  budget(x,y)
  }
}

plot_ly() %>% 
  add_surface(z = ~budget_matrix, showscale = FALSE,
              contours = list(
                z = list(
                  show=TRUE,
                  usecolormap=TRUE,
                  highlightcolor="#ff0000",
                  project=list(z=TRUE)
                ),
                size = 90
              ))


budget_plot <- function(budget_input){
  # the budget function will be defined as z(x,y) = (1/4)x + (1/2)y
  # where budget_input = z
  x_intercept <- floor(budget_input/0.25)    # solve for maximum x coordinate (0,x)
  if(x_intercept > 200){
    x_intercept <- 200
  }
  y <- c()
  for(i in 1:x_intercept){                # iterate through all x up to x_intercept
    y_coord <- (budget_input - 0.25*i)/0.5
    if(y_coord > 200){
      y <- NA
    }
    y <- c(y, y_coord)
  }
  
  data <- data.frame(
    x = 1:x_intercept,
    y = y,
    z = rep(budget_input, x_intercept)
  )
 return(data)
}





# let us assume that z = 60x + 90y; the budget constraint under complete saturation
# when z = 8 and x = 0, y should = 100... so 8 = 0.08x + 0.08y  |  y = budget - 0.08 * i)





# --------- Creating Plot_ly
font_list <- list(
  family = "Helvetica, sans-serif",
  size = 10,
  color = "#4b2e83"
)

x_axis <- list(
  title = "Units of Wine",
  titlefont  = font_list
)

y_axis <- list(
  title = "Units of Bread",
  titlefont  = font_list
)

z_axis <- list(
  title = "Utility | Budget",
  titlefont = font_list
)



user_defined_data <- budget_plot(50)

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
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=1)
      ),
      xaxis = x_axis,
      yaxis = y_axis,
      zaxis = z_axis
    )
  )


systems_solver <-function(budget_input){
  x <- (12/5)*budget_input
  y <- 
  
  
}





budget_plot <- function(budget_input){
  # the budget function will be defined as z(x,y) = (1/4)x + (1/2)y
  # where budget_input = z
  x_intercept <- floor(budget_input/0.25)    # solve for maximum x coordinate (0,x)
  if(x_intercept > 200){
    x_intercept <- 200
  }
  y <- c()
  for(i in 1:x_intercept){                # iterate through all x up to x_intercept
    y_coord <- (budget_input - 0.25*i)/0.5
    if(y_coord > 200){
      y <- NA
    }
    y <- c(y, y_coord)
  }
  
  data <- data.frame(
    x = 1:x_intercept,
    y = y,
    z = rep(budget_input, x_intercept)
  )
  return(data)
}

budget_list <- lapply(seq(from = 10, to = 60, by = 10), budget_plot)


full_df <- do.call(rbind, budget_list)

ggplot() +
  geom_point(data = full_df, aes(x = x, y = y, color = z)) +
  theme_minimal() +
  ylim(0,100) +
  scale_color_continuous(name = "Budget")




utility_plot <- function(utility_input){
  # the utility function will be defined as z(x,y) = (x^0.6)(y^0.4)
  # where budget_input = z
  
  y <- c()
  for(i in 1:100){                # iterate through all x up to 100
    y_coord <- (utility_input/(i^(0.6)))^(10/4)
    y <- c(y, y_coord)
  }
  
  data <- data.frame(
    x = 1:100,
    y = y,
    z = rep(utility_input, 100)
  )
  return(data)
}

utility_list <- lapply(seq(from = 10, to = 60, by = 10), utility_plot)


full_df <- do.call(rbind, utility_list)

ggplot() +
  geom_point(data = full_df, aes(x = x, y = y, color = z)) +
  geom_path(data = full_df, aes(x = x, y = y, color = z)) +
  theme_minimal() +
  ylim(0,100) +
  scale_color_continuous(name = "Utility")
  
