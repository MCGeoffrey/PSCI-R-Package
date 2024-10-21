data_analysis <- function(data, response_var) {
  # Check if the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Check if the response variable exists in the data
  if (!response_var %in% colnames(data)) {
    stop("Response variable not found in the dataset.")
  }

  # Fit the linear regression model
  model <- lm(as.formula(paste(response_var, "~ .")), data = data)

  # Display the summary of the linear regression model
  cat("\nLinear Regression Summary:\n")
  print(summary(model))

  return(model)  # Return the model object for further analysis if needed
}

