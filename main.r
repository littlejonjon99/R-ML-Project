library(data.table)
library(zoo)
library(nnet)
library(dplyr)
library(caret)




# load data
data_1 <- fread("train_data1.csv") # nolint
data_2 <- fread("train_data2.csv") # nolint
new_data <- rbind(data_1, data_2)




# PREPROCESSING OF DATA


# Replace any blank value with NA
new_data[new_data == ""] <- NA


# Check for any missing values in data (including target variable)
cat("Number of missing values or NA in data before changes: ", sum(is.na(new_data)), "\n") # nolint


# Select only the unique rows (remove duplicates)
duplicates <- duplicated(new_data)
new_data <- new_data[!duplicates, ]


# Convert TARGET to a factor
new_data$TARGET <- as.factor(new_data$TARGET)


# Remove column named "col_to_remove"
new_data <- subset(new_data, select = -SK_ID_CURR)


# Replace all blank values with 'Unknown' for character cols
for (col in names(new_data)) {
    # get the number of missing values in the column
    num_missing <- sum(is.na(new_data[[col]]))
    # Column has missing values
    if (num_missing > 0) {
        # get the column type
        col_type <- class(new_data[[col]])[1]
        # replace missing values with column mean
        if (col_type == "character") {
        # replace missing values with "unknown"
            new_data[[col]][is.na(new_data[[col]])] <- "Unknown"
        }
    }
}


# ENCODING CATEGORICAL DATA
# Encodes then puts as factors
new_data <- new_data %>% mutate_if(is.character, function(x) as.factor(as.integer(as.factor(x)) - 1)) # nolint


# FILL MISSING VALUES
for (col in names(new_data)) {
    if (is.numeric(new_data[[col]])) {
        new_data[[col]][is.na(new_data[[col]])] <- mean(new_data[[col]], na.rm = TRUE) # nolint
    }
}


# NORMALIZING THE DATA
for (col in names(new_data)) {
    if (is.numeric(new_data[[col]])) {
        new_data[[col]] <- scale(new_data[[col]], center = TRUE, scale = TRUE)
    }
}


# Check for any missing values in data
cat("Number of missing values or NA in data after changes: ", sum(is.na(new_data)), "\n") # nolint


# SPLIT DATA FOR TRAINING AND TESTING


# Split new_data
new_index <- createDataPartition(new_data$TARGET, p = 0.7, list = FALSE)
new_train <- new_data[new_index, ]
new_test <- new_data[-new_index, ]




# MACHINE LEARNING ALGORITHMS TRAINING


# Shallow Neural Net
nn_model <- nnet(TARGET ~ ., data = new_train, size = 2, maxit = 100, decay = 0.1) # nolint
nn_pred <- predict(nn_model, newdata = new_train)
nn_accuracy <- mean((nn_pred > 0.5) == new_train$TARGET)


# MACHINE LEARNING ALGORITHMS TESTING


# Shallow Neural Net
nn_test_pred <- predict(nn_model, newdata = new_test)
nn_test_accuracy <- mean((nn_test_pred > 0.5) == new_test$TARGET)


# Print accuracies
cat("Shallow Neural Net Accuracy on Train Data: ", round(nn_accuracy * 100, 4), "%", "\n",  sep = "") # nolint
cat("Shallow Neural Net Accuracy on Test Data: ", round(nn_test_accuracy * 100, 4), "%", "\n",  sep = "") # nolint