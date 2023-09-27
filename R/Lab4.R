library(ggplot2)

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
