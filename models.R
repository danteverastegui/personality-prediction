# Load data
personality_data <- read.csv("personality_dataset.csv")

# Change output, Personality, from string to categorical factor
personality_data$Personality <- as.factor(personality_data$Personality)

# Creates logistic regression model
log_reg <- glm(Personality ~ ., data = personality_data, family = binomial)

# Calculates probabilites for each row
probs <- predict(log_reg, type = "response")

# Predicts class based on probability
predicted_class <- ifelse(probs > 0.5, "Introvert", "Extrovert")

# Removes rows with na's and extracts actual labels
clean_data <- na.omit(personality_data)
actual_class <- clean_data$Personality

# Checks accuracy
mean(predicted_class == actual_class)
