#' The function of 3D partial dependence plot
#' 
#' @importFrom data.table
#' @importFrom dplyr
#' 
#' @param object the model object
#' @param data the dataset
#' @param var1 the name of first variable
#' @param var2 the name of second variable
#' @param n the number of grids
#' @param theta the angles of graph
#' @param xlab the name of x-axis
#' @param ylab the name of y-axis


library(dplyr)

partial_plot_3d <- function(object, data, var1, var2, n = 19, theta = -45, xlab, ylab){
  if( !data.table::is.data.table(x = data) ) stop(" 'data' must be data.table ")
  
  # define x1, x2
  var1_val <- data[, var1, with = FALSE][[1]]
  var2_val <- data[, var2, with = FALSE][[1]]
  
  # create each grid value
  var1_vals <- seq(  from = min(var1_val)
                   , to = max(var1_val)
                   , by = (max(var1_val) - min(var1_val))/n)
  var2_vals <- seq(  from = min(var2_val)
                   , to = max(var2_val)
                   , by = (max(var2_val) - min(var2_val))/n)
  
  # create a n X n grid
  two_vals <- expand.grid(var1_vals, var2_vals)
  two_rep  <- data[rep(1:nrow(x = data), nrow(x = two_vals)), ]
  
  # set true value
  data.table::set(  x = two_rep, i = 1:nrow(two_rep), j = which(names(two_rep) == var1)
                  , value = rep(two_vals$Var1, each = nrow(x = data)) )
  data.table::set(  x = two_rep, i = 1:nrow(two_rep), j = which(names(two_rep) == var2)
                  , value = rep(two_vals$Var2, each = nrow(x = data)) )
  
  # predict
  if(class(x = object) == "xgb.Booster"){
    two_pred <- predict(  object = object
                        , newdata = as.matrix(select(  .data = two_rep
                                                     , -batch, -group, -filtration)))    
  }else{
    two_pred <- predict(object = object, data = two_rep)
    if(class(x = object) == "ranger") two_pred <- two_pred$predictions
  }
  two_rep$pred <- two_pred
  data.table::setnames(x = two_rep, old = c(var1, var2), new = c("var1", "var2"))
  
  two_agg <- two_rep %>%
    group_by(var1, var2) %>%
    summarise(mean_pred = mean(pred), .groups = "drop")
  
  z <- matrix(two_agg$mean_pred, nrow = length(var1_vals), byrow = TRUE)
  
  # set color range
  ncol <- 100
  jet.colors <- colorRampPalette( c("orange", "white") )
  color <- jet.colors(n = ncol)
  
  zfacet <- z[-1, -1]
  facetcol <- cut(zfacet, ncol)
  
  # use persp for 3D plotting
  persp(  x = var1_vals, y = var2_vals, z = z, theta = theta, phi = 25
        , xlab = paste0('\n', xlab)
        , ylab = paste0('\n', ylab), zlab = "\nPredicted Value"
        , cex.lab = 1, ticktype = "detailed"
        , col = color[facetcol], xaxs = "i", yaxs = "i"
        )
}

#' The function of 2D partial dependence plot
#' 
#' @importFrom data.table
#' @importFrom dplyr
#' 
#' @param object the model object
#' @param data the dataset
#' @param var1 the name of first variable
#' @param n the number of grids
#' @param ylim the lower limit and the upper limit of y-axis
#' @param xlab the name of x-axis

partial_plot <- function(object = fit, data, var1, n = 19, ylim = NULL, xlab){
  if( !data.table::is.data.table(x = data) ) stop(" 'data' must be data.table ")
  
  var1_val <- Train[, var1, with = FALSE][[1]]
  
  var1_vals <- seq(  from = min(var1_val)
                   , to   = max(var1_val)
                   , by   = (max(var1_val) - min(var1_val))/n)
  
  # create a nXn grid
  one_rep <- data[rep(1:nrow(x = data), length(x = var1_vals)), ]
  
  data.table::set(  x = one_rep, i = 1:nrow(one_rep), j = which(names(one_rep) == var1)
                  , value = rep(var1_vals, each = nrow(data)))
  
  # predict
  if(class(object) == "xgb.Booster"){
    one_pred <- predict(  object = object
                        , newdata = as.matrix(select(  .data = one_rep
                                                     , -batch, -group, -filtration)))    
  }else{
    one_pred <- predict(object = object, data = one_rep)
    if(class(object) == "ranger") one_pred <- one_pred$predictions
  }
  one_rep$pred <- one_pred
  
  data.table::setnames(x = one_rep, old = var1, new = "var1")
  
  one_agg <- group_by(one_rep, var1) %>%
    summarise(mean_pred = mean(pred))
  
  # plot
  plt <- ggplot(data = one_agg, aes(x = var1, y = mean_pred)) + geom_line(size = 1) + 
    ggtitle(label = "Partial Dependence Plot") + 
    xlab(label = paste0("\n", xlab)) + ylab(label = "Predicted Value\n") + 
    p
  plt + theme(axis.title = element_text(size = 15, face = "bold"))
}


#' A function that calculate MAPE, RMSE (Quantitative evaluation)
#' 
#' @param actual the name of target variable
#' @param predict the name of predicted variable

eval_quantitative <- function(actual, predict, digits = 3){
  
  # calculate MAPE, RMSE
  MAPE = mean(x = abs(actual - predict)/actual) * 100
  RMSE = sqrt(x = mean(x = (actual - predict)^2 ))
  MAE  = mean(x = abs(x = actual - predict))
  
  MAPE = round(x = MAPE, digits = digits)
  RMSE = round(x = RMSE, digits = digits)
  MAE  = round(x = MAE, digits = digits)
  
  return( list(MAPE = MAPE, RMSE = RMSE, MAE = MAE) )
}
