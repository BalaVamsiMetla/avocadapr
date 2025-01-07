# Load required libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stats")
install.packages("car")
library(ggplot2)
library(tidyr)
library(dplyr)
library(car)  # For ANOVA

df <- read.csv("C:/Users/sriram/git/avocadapr/avocado/avocado.csv")

# Reshape the data for easier comparison
df_melted <- df %>%
  pivot_longer(cols = c(X4046, X4225, X4770), 
               names_to = "PLU", 
               values_to = "PLU_value")

# Visualization: Boxplot of AveragePrice for each PLU
ggplot(df_melted, aes(x = PLU, y = AveragePrice)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Comparison of Average Price for Different PLUs",
       x = "PLU",
       y = "Average Price") +
  theme_minimal()

# Perform ANOVA to compare means across PLUs
anova_results <- aov(AveragePrice ~ PLU, data = df_melted)
summary(anova_results)
df_plu <- df %>%
  select(AveragePrice, X4046, X4225, X4770)

# Melt the data for easier plotting
df_melted <- df_plu %>%
  pivot_longer(cols = c(X4046, X4225, X4770), 
               names_to = "PLU", 
               values_to = "PLU_value")

# Create a color palette for PLUs
plu_colors <- c("X4046" = "blue", "X4225" = "green", "X4770" = "red")

# Plot the histogram
ggplot(df_melted, aes(x = AveragePrice, fill = PLU)) +
  geom_histogram(position = "stack", bins = 30, alpha = 0.7, color = "black") +
  geom_density(alpha = 0.3, aes(y = ..count..), position = "stack") +
  scale_fill_manual(values = plu_colors) +
  labs(title = "Comparison of Average Prices for Different PLUs",
       x = "Average Price",
       y = "Frequency",
       fill = "PLU") +
  theme_minimal() +
  theme(legend.position = "top")

