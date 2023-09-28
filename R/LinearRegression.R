#'Linear Regression Function.
#'
#'@param formula The formula for the linear regression with dependent and independent variables.
#'
#'@param data The data includes a data set for the linear regression.
#'
#'@description 
#'The function takes two arguments formula and data. It returns an object with class linreg as an S3 class.
#'The "y_value_name" extracts the name of the dependent variable.
#'The "y" has the values of the dependent variable.
#'The "x" is a matrix which contains independent variables.
#'The linear regression model is assigned to "model_lr".
#'The calculation of the p-values of each regression coefficient is done and assigned to "p_values".
#'Coeffients, call, model and p_values are stored in a list which is called "result".
#'The function resid returns the vector of residual values of the regression model.
#'The function pred returns the predicted values y^.
#'The coef function returns the coefficients as a named vector.
#'The summary function prints the coefficients with their standard error , t-value,  σ^ and the degrees of freedom in the model.
#'The class assigns the linreg to the object result.
#'Creating two graphs , first with Residuals in y axis and Fitted values in the x axis and the second with square root of Standardized residuals in y axis and Fitted values in the x axis, using the function ggplot.
#'@return The function returns an object with the class linreg.
#'
#'@export
# linreg function with two arguments
linreg <- function(formula, data) {

  y_value_name <- all.vars(formula)[1]#extract name of the dependent variable
  y <- data[[y_value_name]]#values of dependent variable
  x <- model.matrix(formula, data) #matrix contains independent variables

  model_lr <- #linear regression
    lm(y ~ x - 1)  # adding linear regression model/exclude intercept

  # sqrt(diag(vcov(model_lr))) #calculate the p values of each regression coefficient
  t_stats <- coef(model_lr)
  df <- model_lr$df.residual
  p_values <- 2 * (1 - pt(abs(t_stats), df))

  result <- list(
    coefficients = model_lr$coefficients,
    call = model_lr$call,
    model = model_lr,
    p_values = p_values
  )# store the coefficients, call, model of lin. reg. and p values

  result$resid <- function() {# return vector of residual values of regression model
    return(model_lr$residuals)
  }
  result$pred <- function() {
    return(predict(model_lr))#predicted values y^
  }

  result$coef <- function() { #coefficients as a named vector
    coefficient_names <- names(result$coefficients)
    coefficient_values <- result$coefficients
    coefficient_vector <- as.vector(coefficient_values)
    names(coefficient_vector) <- coefficient_names
    return(coefficient_vector)
  }

  result$summary <- function() { #the summary print; coefficient, standard error, t-value, p-value, estimate of sigma and degrees of freedom of the model
    model <- result$model
    coefficient_summary <- summary(model)
    return(coefficient_summary)
  }

  class(result) <- "linreg" #assign the class linreg to result object
  return(result)
}

# print method
print.linreg <- function(obj, ...) {
  cat("Call:\n")
  print(obj$call)
  cat("\nCoefficients:\n")
  print(obj$coefficients)
}
# plot method 1 for first graph
plot.linreg <- function(obj, ...) {
  df <-
    data.frame(Residuals = obj$model$residuals,
               Fitted = obj$model$fitted.values)# daraframe for plotted data; resudials, fitted-predicted values from the model

  ggplot(df, aes(x = Fitted, y = Residuals)) + # assign x to fitted value and assign y to the residuals for aestatic method
    geom_point() + #adding the points to the ggplot
    stat_summary(
      fun = median,
      fun.args = list(trim = 0.25),
      colour = "red",
      geom = "line",
    ) +
    labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    theme(axis.title.y = element_text(
      vjust = 0.5,
      size = 10,
    )) +
    theme(axis.title.x = element_text(
      vjust = 0.5,
      size = 10,
    )) +
    theme(plot.title = element_text(
      size = 10,
      hjust = 0.5
    )) +
    theme_bw()
}
# plot method 2 for second graph
plot.linreg_v2 <- function(obj, ...) {
  std_residuals <- obj$model$residuals / sd(obj$model$residuals)
  sqrt_std_residuals <- sqrt(abs(std_residuals))
  
  df <-
    data.frame(Fitted = obj$model$fitted.values,
               Sqrt_Std_Residuals = sqrt_std_residuals)
  
  ggplot(df, aes(x = Fitted, y = Sqrt_Std_Residuals)) +
    geom_point() +
    stat_summary(
      fun = mean,
      fun.args = list(trim = 0.25),
      colour = "red",
      geom = "line"
    ) +
    labs(title = "Scale-Location",
         x = "Fitted Values",
         y = expression(sqrt("Standardized residuals"))) +
    theme(axis.title.y = element_text(vjust = 0.5,
                                      size = 10)) +
    theme(axis.title.x = element_text(vjust = 0.5,
                                      size = 10)) +
    theme(plot.title = element_text(size = 10,
                                    hjust = 0.5)) +
    theme_bw()
}


data(iris)
general_data <- linreg(Petal.Length ~ Species, data = iris)
print(general_data)  # Commented out the print as it's not necessary for the plot


residuals <- general_data$resid()
print(head(residuals))

predicted_values <- general_data$pred()
cat("Predicted Values:\n")
print(head(predicted_values))
plot.linreg(general_data)  # Plot the first graph
plot.linreg_v2 (general_data)

coefficient_values <- general_data$coef()# Get coefficients
cat("Coefficient values:\n")
print(coefficient_values)


# Get summary
summary <- general_data$summary()
cat("Summary:\n")
print(summary)
