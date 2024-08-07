# Iteration and Conditional Statements -----------------------------------------

# Load the Iris dataset
data(iris)

## Conditional Statements with if -----------------------------------------------
# Here’s an example of using if statements to classify each flower 
# based on its sepal length:

# Add a new column to classify based on sepal length
iris$Classification <- ifelse(iris$Sepal.Length > 5.5, "Long", "Short")

# Print the first few rows of the dataset to see the classification
head(iris)

## Iteration --------------------------------------------------------------------

#Load the necessary package
library(dplyr)

# Create an empty list to store results
mean_sepal_length <- list()

# Iterate over each species in the dataset


species = 'virginica'
for (species in levels(iris$Species)) {
  # Filter the data for the current species
  species_data <- iris %>% filter(Species == species) # %>% <- Ctrl + Shift+ M
  
  # Calculate the mean sepal length
  mean_length <- mean(species_data$Sepal.Length)
  
  # Store the result in the list
  mean_sepal_length[[species]] <- mean_length
}

# Print the results
print(mean_sepal_length)

## Purpose of the Pipe Operator (%>%) ------------------------------------------

#The %>% operator is known as the pipe operator and is a key feature of the dplyr
#and tidyverse packages in R. 
#It is used to pass the result of one function directly into the next function, 
#making your code more readable and easier to follow.

# Before Pipe: Code often involves nested functions, which can become 
#difficult to read and understand.

mean(filter(iris, Species == "setosa")$Sepal.Length)

# With Pipe: The pipe operator helps in breaking down the process into a 
#series of steps, making it more intuitive.

iris %>%
  filter(Species == "setosa") %>%
  summarize(Mean_Sepal_Length = mean(Sepal.Length))

#  Example Breakdown

# Here’s a detailed breakdown of a common dplyr pipe example:

iris %>%
filter(Species == "setosa") %>%
  group_by(Species) %>%
  summarize(Mean_Sepal_Length = mean(Sepal.Length))

# 1. iris: The dataset we start with.
# 2. %>%: Passes iris to the next function.
# 3. filter(Species == "setosa"): Filters rows where Species is "setosa".
# 4. %>%: Passes the filtered data to the next function.
# 5. group_by(Species): Groups data by Species (though in this case it’s already filtered).
# 6. %>%: Passes the grouped data to the next function.
# 7. summarize(Mean_Sepal_Length = mean(Sepal.Length)): Calculates the mean of Sepal.Length for each species.

# Creating Plots ---------------------------------------------------------------

# Install ggplot2 if not already installed
install.packages("ggplot2")

# Load ggplot2
library(ggplot2)

# Plotting Sepal Length vs. Sepal Width

# Scatter plot of Sepal Length vs Sepal Width
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Sepal Length vs Sepal Width",
       x = "Sepal Length (cm)",
       y = "Sepal Width (cm)") +
  theme_minimal()

# Plotting Sepal Length Distribution by Species

# Boxplot of Sepal Length by Species
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Distribution of Sepal Length by Species",
       x = "Species",
       y = "Sepal Length (cm)") +
  theme_minimal()

## Undestanding ggplot 2 -------------------------------------------------------



