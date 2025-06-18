library(dplyr)

# Load data
personality_data <- read.csv("personality_dataset.csv")

colnames(personality_data) <- tolower(colnames(personality_data)) # Makes col names lowercase

# Change output, Personality, from string to categorical factor
personality_data$personality <- as.factor(personality_data$personality)

personality_data$stage_fear <- factor(
  personality_data$stage_fear,
  levels = c("No", "Yes")
)

personality_data$drained_after_socializing <- factor(
  personality_data$drained_after_socializing,
  levels = c("No", "Yes"))

# Checks NAs in all columns
for (i in colnames(personality_data)) {
  print(sum(is.na(personality_data[[i]])))
}

# Because both perfectly correlated, use each other to impute
personality_data$stage_fear[is.na(personality_data$stage_fear)] <- personality_data$drained_after_socializing[is.na(personality_data$stage_fear)]
personality_data$drained_after_socializing[is.na(personality_data$drained_after_socializing)] <- personality_data$stage_fear[is.na(personality_data$drained_after_socializing)]

# Returns where both are missing and can't easily impute
which(
  is.na(personality_data$stage_fear) & 
  is.na(personality_data$drained_after_socializing))

# Removes 1 instance of above happening
personality_data <- personality_data[-1518, ]

# Removes stage_fear because perfectly colinear with drained_after_socializing
personality_data <- personality_data %>%
  select(-stage_fear)

# Build linear regression models to impute: time_spent_alone, social_event_attendance, going_outside, friends_circle_size, post_frequency
impute_lms <- list(
  time_spent_alone = lm(time_spent_alone ~ ., personality_factors),
  social_event_attendance = lm(social_event_attendance ~ ., personality_factors),
  going_outside = lm(going_outside ~ ., personality_factors),
  friends_circle_size = lm(friends_circle_size ~ ., personality_factors),
  post_frequency = lm(post_frequency ~ ., personality_factors)
)

# Checks summarys for all impute models
#for (i in impute_lms) {
#  print(summary(i))
#}

na_col <- c("time_spent_alone", "social_event_attendance", "going_outside", "friends_circle_size", "post_frequency")

for (name in names(impute_lms)) {
  model <- impute_lms[[name]]
  
  missing_values <- is.na(personality_data[[name]])
  
  predicted_values <- predict(model, newdata = personality_data[missing_values, ])
  
  personality_data[[name]][missing_values] <- predicted_values
}

# Checks again for NAs in all columns
for (i in colnames(personality_data)) {
  print(sum(is.na(personality_data[[i]])))
}
# Reduced # NAs but some still exist where there were multiple NAs in a single row and lm couldn't compute
# Will remove these for now, but can explore mice package in future

final_personality_data <- na.omit(personality_data)

# Final checks for NAs in all columns
for (i in colnames(final_personality_data)) {
  print(sum(is.na(final_personality_data[[i]])))
}


# Create factor df
personality_factors <- final_personality_data[0:6]

# Creates logistic regression model
log_reg <- glm(personality ~ ., data = final_personality_data, family = binomial)

# Calculates probabilities for each row
probs <- predict(log_reg, type = "response")

# Predicts class based on probability
predicted_class <- ifelse(probs > 0.5, "Introvert", "Extrovert")

actual_class <- final_personality_data$personality

# Checks accuracy
accuracy <- mean(predicted_class == actual_class)
print(accuracy)
