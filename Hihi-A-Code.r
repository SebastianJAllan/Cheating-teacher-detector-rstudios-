# Load required libraries
library(tidyverse)

# Read the CSV file containing children's scores
data <- read.csv("ChildrenScores.csv")

# Calculate the differences between each year's score and the mean score for each student
data <- data %>%
  rowwise() %>%
  mutate(diffY3 = scoreY3 - meanScore,
         diffY4 = scoreY4 - meanScore,
         diffY5 = scoreY5 - meanScore,
         diffY6 = scoreY6 - meanScore,
         diffY7 = scoreY7 - meanScore)

# Store only the positive differences; set negative or zero differences to 0
data <- data %>%
  rowwise() %>%
  mutate(pdiffY3 = case_when(diffY3 > 0 ~ diffY3, TRUE ~ 0),
         pdiffY4 = case_when(diffY4 > 0 ~ diffY4, TRUE ~ 0),
         pdiffY5 = case_when(diffY5 > 0 ~ diffY5, TRUE ~ 0),
         pdiffY6 = case_when(diffY6 > 0 ~ diffY6, TRUE ~ 0),
         pdiffY7 = case_when(diffY7 > 0 ~ diffY7, TRUE ~ 0))

# Group data by each teacher for respective years and calculate the mean of positive differences
data_Y3 <- data %>%
  group_by(teacherY3) %>%
  summarise(mean_diffY3 = mean(pdiffY3)) %>%
  filter(mean_diffY3 > 1.9)

data_Y4 <- data %>%
  group_by(teacherY4) %>%
  summarise(mean_diffY4 = mean(pdiffY4)) %>%
  filter(mean_diffY4 > 1.9)

data_Y5 <- data %>%
  group_by(teacherY5) %>%
  summarise(mean_diffY5 = mean(pdiffY5)) %>%
  filter(mean_diffY5 > 1.9)

data_Y6 <- data %>%
  group_by(teacherY6) %>%
  summarise(mean_diffY6 = mean(pdiffY6)) %>%
  filter(mean_diffY6 > 1.9)

data_Y7 <- data %>%
  group_by(teacherY7) %>%
  summarise(mean_diffY7 = mean(pdiffY7)) %>%
  filter(mean_diffY7 > 1.9)

# Combine all suspected cheater teachers from each year into a single data frame
final_cheater <- as.data.frame(c(data_Y3$teacherY3, data_Y4$teacherY4, data_Y5$teacherY5, data_Y6$teacherY6, data_Y7$teacherY7)) %>%
  rename(CheatID = 1)

# Visualise the distribution of mean positive score differences for Year 3 teachers
ggplot(data_Y3, aes(x = mean_diffY3)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Average Positive Score Differences (Year 3 Teachers)",
       x = "Mean Positive Score Differences (mean_diffY3)",
       y = "Frequency") +
  theme_minimal()

# Load cheater, non-cheater, and children scores data
cheater_scores <- read.csv("cheater-scores.csv")
non_cheater_scores <- read.csv("Non-cheaters.csv")
children_scores <- read.csv("ChildrenScores.csv")

# Define the binwidth for histograms
binwidth <- 1

# Plot histograms and overlay them for all teachers, non-cheaters, and cheaters
ggplot() +
  geom_histogram(data = children_scores, aes(x = meanScore, y = ..count.., fill = "All Teachers"), 
                 binwidth = binwidth, color = "green", alpha = 0.5) +
  geom_histogram(data = non_cheater_scores, aes(x = meanScore, y = ..count.., fill = "Non-cheaters"), 
                 binwidth = binwidth, color = "blue", alpha = 0.5) +
  geom_histogram(data = cheater_scores, aes(x = meanScore, y = ..count.., fill = "Cheaters"), 
                 binwidth = binwidth, color = "red", alpha = 0.5) +
  labs(title = "Frequency Distribution of Mean Scores for Students",
       x = "Mean Scores",
       y = "Number of Students") +
  scale_fill_manual(name = "Teachers", values = c("Non-cheaters" = "blue", "Cheaters" = "red", "All Teachers" = "green")) +
  theme_minimal() +
  theme(legend.position = "right")

# Separate cheater and non-cheater data, updating scores based on cheater teacher IDs
cheaters <- read.csv("Hihi-A-Cheaters.csv")
teacher_cheater_ids <- cheaters$CheatID

# Update non-cheater data
scores_clean_non <- child_scores %>%
  mutate(across(scoreY3:scoreY7, ~ ifelse(get(paste0("teacherY", substr(cur_column(), 6, 6))) %in% teacher_cheater_ids, NA, .))) %>%
  mutate(meanScore_new = rowMeans(select(., scoreY3:scoreY7), na.rm = TRUE))

# Save cleaned non-cheater data
write.csv(scores_clean_non, "ChildrenScores_Non.csv", row.names = FALSE)

# Update cheater data
scores_clean_cheat <- child_scores %>%
  mutate(across(scoreY3:scoreY7, ~ ifelse(!(get(paste0("teacherY", substr(cur_column(), 6, 6))) %in% teacher_cheater_ids), NA, .))) %>%
  mutate(meanScore_new = rowMeans(select(., scoreY3:scoreY7), na.rm = TRUE))

# Save cleaned cheater data
write.csv(scores_clean_cheat, "ChildrenScores_Cheater.csv", row.names = FALSE)

# Convert wide-format cheater and non-cheater data into long format for residual analysis
non_cheater_long_data <- non_cheater_data %>%
  gather(key = "grade", value = "actual_score", scoreY3:scoreY7) %>%
  filter(!is.na(actual_score)) %>%
  mutate(residual = meanScore - actual_score)

cheater_long_data <- cheater_data %>%
  gather(key = "grade", value = "actual_score", scoreY3:scoreY7) %>%
  filter(!is.na(actual_score)) %>%
  mutate(residual = meanScore - actual_score)

# Save long-format data for future use
write.csv(cheater_long_data, "Cheater_Hihi-A.csv", row.names = FALSE)
write.csv(non_cheater_long_data, "Non-Cheater_Hihi-A.csv", row.names = FALSE)

# Combine cheater and non-cheater residuals for final analysis
combined_residuals <- bind_rows(cheater_long_data, non_cheater_long_data)

# Visualise residuals distribution
ggplot(combined_residuals, aes(x = residual)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Define the likelihood function for the mixture of two normal distributions
likelihood_mixture_distr <- function(pars, child_scores_data) {
  q <- pars[1]  
  mu1 <- pars[2]
  sigma1 <- pars[3] 
  mu2 <- pars[4] 
  sigma2 <- pars[5]
  
  if(sigma1 <= 0 || sigma2 <= 0 || q < 0 || q > 1) {
    return(Inf)
  }
  
  likelihood <- q * dnorm(child_scores_data, mean = mu1, sd = sigma1, log = FALSE) + 
    (1 - q) * dnorm(child_scores_data, mean = mu2, sd = sigma2, log = FALSE)
  
  log_likelihood <- sum(log(likelihood))
  
  return(-log_likelihood)
}

# Set initial parameters for MLE
init_params <- c(0.5,
                 mean(combined_residuals$residual, na.rm = TRUE),
                 sd(combined_residuals$residual, na.rm = TRUE),
                 mean(combined_residuals$residual, na.rm = TRUE) + 5,
                 sd(combined_residuals$residual, na.rm = TRUE) + 2)

# Perform MLE optimisation
mle_results <- optim(init_params, likelihood_mixture_distr, child_scores_data = combined_residuals$residual)

# Output MLE results
cat("MLE estimates for the mixture of two normal distributions (combined residuals):\n")
cat("Mixing proportion (q):", mle_results$par[1], "\n")
cat("Mean of first normal (u1):", mle_results$par[2], "\n")
cat("Standard deviation of first normal (sigma1):", mle_results$par[3], "\n")
cat("Mean of second normal (u2):", mle_results$par[4], "\n")
cat("Standard deviation of second normal (sigma2):", mle_results$par[5], "\n")
    