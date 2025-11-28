# Install necessary packages if they aren't already installed
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("corrplot")) install.packages("corrplot")
if (!require("GGally")) install.packages("GGally")


# Load the packages into the session
library(readxl)
library(dplyr)

# Set the file path (modify this path according to your local setup)
file_path <- "C:/Users/jueel/Jueeli_rit/Visual_Analytics/FinalProject/country_information.xlsx"

# Read the data from the Excel file
country_data <- read_excel(file_path)

# Check for missing values
sum(is.na(country_data))

# View the first few rows of the dataset
head(country_data)

# Get a summary of the dataset
summary(country_data)

# Check the structure of the dataset
str(country_data)

# Calculate correlation matrix
cor_matrix <- cor(country_data %>% select(where(is.numeric)), use = "complete.obs")
cor_matrix

# Optionally, visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle")


# Histograms of selected variables
library(ggplot2)
ggplot(country_data, aes(x = gini_index)) + geom_histogram(bins = 30, fill = "blue", color = "black") + ggtitle("Distribution of Gini Index")

# Add the HDI category based on HDI values
country_data <- country_data %>%
  mutate(hdi_category = case_when(
    hdi < 2 ~ "Low",
    hdi >= 2 & hdi < 3 ~ "Medium",
    TRUE ~ "High"
  ))

# Now plot the scatter plot with the newly created hdi_category variable
ggplot(country_data, aes(x = gini_index, y = corruption_perceptions_index, color = hdi_category)) +
  geom_point(alpha = 0.5) +
  labs(x = "Gini Index", y = "Corruption Perceptions Index", title = "Gini vs. Corruption by HDI Category")


# Interactive scatter plot using Plotly
library(plotly)
plot_ly(data = country_data, x = ~gini_index, y = ~corruption_perceptions_index, type = 'scatter', mode = 'markers',
        marker = list(size = 10, color = ~hdi, colorscale = 'Viridis')) %>%
  layout(title = "Interactive Scatter Plot: Gini Index vs Corruption")


# Box plot of Corruption Perceptions Index
ggplot(country_data, aes(y = corruption_perceptions_index, x = 1)) +
  geom_boxplot(fill = "cyan", color = "black") +
  ggtitle("Box Plot of Corruption Perceptions Index") +
  xlab("") +
  ylab("Corruption Perceptions Index")


# Create a correlation matrix
cor_matrix <- cor(country_data %>% select(where(is.numeric)), use = "complete.obs")

# Load libraries
library(ggplot2)
library(reshape2)

# Convert the correlation matrix to a long format for ggplot
cor_data <- melt(cor_matrix)

# Plotting the heat map
ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", title = "Heat Map of Correlations", fill = "Correlation")

library(ggplot2)
library(reshape2) # for melting data frames
library(RColorBrewer) # for color palettes

# Assuming 'data' is your dataset after computing correlations
cor_data <- cor(data[, -1], use = "complete.obs") # compute correlations excluding the first column if it's categorical
melted_cor_data <- melt(cor_data) # melt the data frame for use with ggplot

# Create the heatmap
ggplot(melted_cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") + # add borders to tiles
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  labs(x = "", y = "", title = "Enhanced Heat Map of Correlations") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") # add correlation values


# Creating a summary of average trust in government by HDI category
avg_trust <- country_data %>%
  group_by(hdi_category) %>%
  summarize(avg_trust_in_government = mean(trust_in_government_index, na.rm = TRUE))

# Bar chart
ggplot(avg_trust, aes(x = hdi_category, y = avg_trust_in_government, fill = hdi_category)) +
  geom_bar(stat = "identity", color = "black") +
  ggtitle("Average Trust in Government by HDI Category") +
  xlab("HDI Category") +
  ylab("Average Trust in Government Index")

# First, categorize HDI into levels
country_data$hdi_category <- cut(country_data$hdi, breaks=c(0, 1.5, 3, 4), labels=c("Low", "Medium", "High"))

# Stacked bar chart
ggplot(country_data, aes(x = hdi_category, fill = hdi_category)) +
  geom_bar(aes(y = corruption_perceptions_index), stat = "identity") +
  geom_bar(aes(y = -trust_in_government_index), stat = "identity") +
  labs(y = "Index Value", x = "HDI Category", title = "Corruption and Trust Indices by HDI Category")




