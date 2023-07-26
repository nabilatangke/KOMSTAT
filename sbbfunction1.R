#' Perform linear regression.
#'
#' This function performs linear regression analysis using the lm() function in R.
#'
#' @param dependent_var A vector containing the dependent variable.
#' @param independent_var A data frame or matrix containing the independent variables.
#' @return The linear regression model.
#' @export
perform_regression <- function(dependent_var, independent_var) {
  if (!is.data.frame(independent_var)) {
    independent_var <- as.data.frame(independent_var)
  }
  model <- lm(dependent_var ~ ., data = independent_var)
  return(model)
}

#' Predict values based on a linear regression model.
#'
#' This function predicts values using a linear regression model obtained from perform_regression().
#'
#' @param model The linear regression model obtained from perform_regression().
#' @param new_data A data frame with the independent variables for prediction.
#' @return A vector containing the predicted values.
#' @export
predict_values <- function(model, new_data) {
  if (!is.data.frame(new_data)) {
    new_data <- as.data.frame(new_data)
  }
  predictions <- predict(model, newdata = new_data)
  return(predictions)
}

#' Find the maximum value in a numeric vector.
#'
#' This function finds the maximum value in a given numeric vector.
#'
#' @param input_vector A numeric vector.
#' @return The maximum value in the vector.
#' @export
find_max_value <- function(input_vector) {
  max_value <- max(input_vector)
  return(max_value)
}

#' Calculate the mean of a numeric vector.
#'
#' This function calculates the mean of a given numeric vector.
#'
#' @param input_vector A numeric vector.
#' @return The mean value of the vector.
#' @export
calculate_mean <- function(input_vector) {
  mean_value <- mean(input_vector)
  return(mean_value)
}
