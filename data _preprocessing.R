# Load necessary libraries
library(dplyr)

# Load the dataset
train_data <- read.csv("train.csv")

# Remove irrelevant columns
train_data <- train_data %>%
  select(-PatientID, -DoctorInCharge)  # Remove columns unlikely to provide predictive value

# Confirm Diagnosis is binary and convert to a factor if needed
if (length(unique(train_data$Diagnosis)) == 2) {
  train_data$Diagnosis <- as.factor(train_data$Diagnosis)
} else {
  stop("Diagnosis must be a binary variable.")
}

# Ensure categorical variables are factors
train_data$Gender <- as.factor(train_data$Gender)
train_data$Ethnicity <- as.factor(train_data$Ethnicity)
train_data$EducationLevel <- as.factor(train_data$EducationLevel)

# Check for missing values and handle them
missing_summary <- colSums(is.na(train_data))
print("Missing values summary:")
print(missing_summary)

train_data <- na.omit(train_data)  # Drop rows with any missing values


# Combine point-biserial correlation threshold and p-value testing
numeric_results <- lapply(numeric_features, function(x) {
  test <- cor.test(x, as.numeric(train_data$Diagnosis) - 1)  # Convert Diagnosis to numeric (0/1)
  list(correlation = test$estimate, p_value = test$p.value)
})

# Convert results to a data frame for filtering
numeric_df <- data.frame(
  Feature = names(numeric_results),
  Correlation = sapply(numeric_results, function(res) res$correlation),
  P_Value = sapply(numeric_results, function(res) res$p_value)
)

# Filter features based on correlation threshold and p-value
correlation_threshold <- 0.03
numeric_significant <- numeric_df %>%
  filter(abs(Correlation) > correlation_threshold & P_Value < 0.05) %>%
  pull(Feature)

# Print significant numeric features
print("Significant Numeric Features:")
print(numeric_significant)


# Combine selected numeric features and Diagnosis column
all_selected_features <- c(numeric_significant, "Diagnosis")

# Subset the dataset with the selected features
selected_data <- train_data %>%
  dplyr::select(all_of(all_selected_features))

# Display the first few rows of the selected dataset
head(selected_data)
