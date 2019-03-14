#' @title keras_r2
#'
#' @description This function is to calculate rsquared value for regression models.
#'
#' @param y_true : tensor of ground truth   
#'
#' @param  y_pred: tensor of predicted values
#'
#' @return keras tensor, double for rsquared
#'
#' @export keras_r2
keras_r2=function(y_true, y_pred) {
  SS_res =keras::k_sum(keras::k_square(y_true-y_pred ))
  SS_tot =keras::k_sum(keras::k_square(( y_true - keras::k_mean(y_true))))
  return ( 1 - SS_res/(SS_tot + keras::k_epsilon()))
}
