
## ---------------------------------------------------------------------------------------------

library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(tidyverse)
library(magrittr)

## ---------------------------------------------------------------------------------------------

## linear programming (lp) problem
# decision variables: x1, x2
# objective function coefficients: w1 = 2, w2 = 3
# objective function: f = w1*x1 + w2*x2 = 2*x1 + 3*x2 
# constraints: 
# 3*x1 + 4*x2 <= 100
# 3*x1 + 4*x2 >= 10
# x2 - 2*x1 <= 5
# x1 >= 0; x2 >= 0
# objective: maximize f

# objective function coefficients
ofc <- list(2, 3)

# number of decision variables
n <- 2

# weights 
w <- list(c(3, 4), c(3, 4), c(-2, 1))

# lp model
model <- MIPModel()%>%
  add_variable(x[i], i = 1:n, type = "continuous")%>%
  set_objective(sum_expr(ofc[[i]]*x[i], i = 1:n), "max")%>%
  add_constraint(sum_expr(w[[1]][i]*x[i], i = 1:n) <= 100)%>%
  add_constraint(sum_expr(w[[2]][i]*x[i], i = 1:n) >= 10)%>%
  add_constraint(sum_expr(w[[3]][i]*x[i], i = 1:n) <= 5)%>%
  solve_model(with_ROI(solver = "glpk"))

# optimum solution
model%>%
  get_solution(x[i])

# objective function value
model%>%
  objective_value()

## ---------------------------------------------------------------------------------------------