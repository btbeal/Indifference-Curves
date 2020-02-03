# --- Functions indifference curves shiny app 
#

# ----- utility function (to loop over goods_matrix - a matrix of utilities for plotly)
# ----- budget_plot; takes a user budget and creates dataframe based on budget function
# ----- goods matrix (creates matrix for utility plot)
# ----- Plot_ly axes

# ---- Creating a budget matrix for plotly
a <- 0.6                      # -- Cobb-Douglas Utility Function (a is a random choice by me)
utility <- function(x, y){    # --- will use this function to create utility values from a 100x100 matrix
  (x^a)*(y^(1-a))
}

budget_plot <- function(budget_input){    # ---- Creating aa budget function
  # the budget function will be defined as z(x,y) = (1/4)x + (1/2)y
  # where budget_input = z
  x_intercept <- floor(budget_input/0.25)    # solve for maximum x coordinate (0,x)
  if(x_intercept > 100){
    x_intercept <- 100
  }
  y <- c()
  for(i in 1:x_intercept){                # iterate through all x up to x_intercept
    y_coord <- (budget_input - 0.25*i)/0.5
    if(y_coord > 100){                    # because the matrix is 100 x 100, 100 is the max
      y <- NA
    }
    y <- c(y, y_coord)
  }
  
  data <- data.frame(                     # data frame to be passed to plotly
    x = 1:x_intercept,
    y = y,
    z = rep(budget_input, x_intercept)
  )
  return(data)
}





# ---- Creating necessary matrix below ---- #
# create a goods matrix for utility values
goods_matrix <- matrix(0,
                       nrow = 100,
                       ncol = 100)


for(r in 1:nrow(goods_matrix)){
  for(c in 1:ncol(goods_matrix)){
    goods_matrix[r,c] <- utility(r,c)
    
  }
}



# --------- Creating Plot_ly axes ----- # 
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




