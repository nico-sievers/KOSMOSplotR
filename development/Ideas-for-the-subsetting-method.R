subset_dataset <- function(dataset, ...) {
  # Capture the named arguments as a list
  subset.data <- list(...)

  # Check if subset.data is not empty
  if (length(subset.data) > 0) {
    # Ensure all names in subset.data exist in the dataset
    if (!all(names(subset.data) %in% names(dataset))) {
      stop("Not all column names given to 'subset.data' were found in the data table!")
    }

    # Loop through each item in subset.data and subset the dataset
    for (i in names(subset.data)) {
      # Use %in% to allow for multiple matching values
      dataset <- dataset[dataset[[i]] %in% subset.data[[i]], ]
    }
  }
  return(dataset)
}

# Example usage
example_data <- data.frame(Set = c("All cells", "Some cells", "All cells"),
                           Settings = c("large", "small", "large"),
                           Value = c(1, 2, 3))

# User-friendly function call
result <- subset_dataset(example_data, Set = "All cells", Settings = "large")
print(result)










  # Helper function to apply subsetting based on inclusion or exclusion
  apply_subset <- function(data, column, criteria) {
    if (is.character(criteria) && grepl("^!", criteria)) {
      exclusion_value <- sub("^!", "", criteria)
      data <- data[data[[column]] != exclusion_value, ]
    } else {
      data <- data[data[[column]] %in% criteria, ]
    }
    return(data)
  }
