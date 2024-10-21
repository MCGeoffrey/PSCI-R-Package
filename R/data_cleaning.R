
clean_data <- function(data) {
  # Check if the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # 1. Remove rows with missing values
  cleaned_data <- na.omit(data)

  # 2. Convert categorical variables to factors
  # Assuming that columns with character type are categorical
  char_columns <- sapply(cleaned_data, is.character)
  cleaned_data[char_columns] <- lapply(cleaned_data[char_columns], as.factor)

  # 3. Remove duplicate rows
  cleaned_data <- cleaned_data[!duplicated(cleaned_data), ]

  # 4. Rename columns (Example: replace spaces with underscores and convert to lowercase)
  colnames(cleaned_data) <- gsub(" ", "_", colnames(cleaned_data))
  colnames(cleaned_data) <- tolower(colnames(cleaned_data))

  return(cleaned_data)
}
