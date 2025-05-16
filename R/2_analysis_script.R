#################################################################################
#                                                                               #
# This script is used to conduct the required analysis using the data from the  #
# National Malaria Indicator Survey 2019/2020.                                  # 
#                                                                               #
# Author : S. Zavala                                                            #
# Date   : 2025-15-05                                                           #
#                                                                               # 
#################################################################################

###########################
# Load required libraries #
###########################

# Load the tidyverse collection of packages for data manipulation and visualization
library(tidyverse)

# Load the haven package to read and work with Stata, SPSS, and SAS data files
library(haven)

# Load the gtsummary package to create publication-ready summary tables
# Useful for descriptive statistics and model result tables with clean formatting
library(gtsummary)

#############
# Load data #
#############

# Load the (clean) household-level data 
household_df <- haven::read_dta("../data/cleanR_household_with_village.dta")

# Load the (clean) household-level data 
# ...

#################
# Shell table 1 #
#################

library(lubridate)
clean_household_df$month <- floor_date(clean_household_df$date_clean, "month") #Date as months

library(ggplot2)
library(dplyr)

#Groups by month
monthly_counts <- clean_household_df %>%
  group_by(month) %>%
  summarize(count = n())

#Method 1 with cleaned data
ggplot(monthly_counts, aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Household Interviews by Month",
       x = "Month",
       y = "Number of Interviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability


#Method 2: Using base R
monthly_counts <- aggregate(rep(1, nrow(clean_household_df)) ~ month, clean_household_df, sum)
colnames(monthly_counts)[2] <- "count"

barplot(monthly_counts$count, 
        names.arg = format(monthly_counts$month, "%b %Y"),
       main = "Household Interviews by Month",
       xlab = "Month", 
       ylab = "Number of Interviews",
       col = "blue",
       las = 2)  # Rotate labels

#M3
monthly_counts$month <- format(monthly_counts$month, "%b %Y")  # e.g., "Jan 2023"


ggplot(monthly_counts, aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "#4A90E2") +  # Changed to a nicer color
  geom_text(aes(label = count), vjust = -0.5, size = 3) +  # Display counts on bars
  labs(title = "Household Interviews by Month",
       x = "Month",
       y = "Number of Interviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14))

monthly_counts$month <- factor(monthly_counts$month, levels = unique(monthly_counts$month))

#################
# Shell table 2 #
#################

household_df |>
  dplyr::select(altitude) |>
  gtsummary::tbl_summary(label = list("altitude" ~ "Altitude (m)"))

#################
# Shell table 3 #
#################

##############
# Question 1 #
##############

# How many household interviews were conducted per month? 

count_df <- household_df |>
  dplyr::group_by(month) |>
  dplyr::count()

# Modify this figure

# Step 1: Add color to bars
# Step 2: Add data labels to the bars
# Step 3: Adjust the y-axis limits
# Step 4: Improve x-axis date formatting
# Step 5: Add labels to the axes
# Step 6: Highlight year transitions with a vertical line
# Step 7: Apply a minimal theme
# Step 8: Customize theme elements for cleaner look

count_df |>
  ggplot(mapping = ggplot2::aes(x = month, y = n)) +
  ggplot2::geom_bar(stat = "identity")

##############
# Question 2 #
##############

# Are members of households that own at least one long-lasting insecticidal net (LLIN) less likely to be infected with malaria parasites?

##############
# Question 3 #
##############

# Does prevalence of Plasmodium vivax and P. falciparum differ between altitudinal zones?

##############
# Question 4 #
##############

# What is the sensitivity and specificity of RDTs compared to microscopy (any species)?
