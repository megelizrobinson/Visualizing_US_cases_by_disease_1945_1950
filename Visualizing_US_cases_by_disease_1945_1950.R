# Load tidyverse package
library(tidyverse)

# Load dataset
data("us_contagious_diseases", package = "dslabs")

# View structure of us_contagious_diseases data and data types of variables
str(us_contagious_diseases)

# Tidy data 

# Check duplicates
sum(duplicated(us_contagious_diseases))

# Check missng values using sum
sum(is.na(us_contagious_diseases))

# Load visdat package 
library(visdat)

# View missing values 
vis_miss(us_contagious_diseases)

# Remove missing values 
us_contagious_diseases <- us_contagious_diseases %>% 
  filter(!is.na(population))

# Filter for only data between 1945-1950 and Measles
subdata_measles <- us_contagious_diseases %>% 
  filter(year %in% c("1945": "1950"),
         disease == "Measles") %>% 
  select(-weeks_reporting, -state)

subdata_measles

# Find proportion of cases within each year
subdata_measles <- subdata_measles %>% 
  group_by(disease, year) %>% 
  summarize(total_cases = sum(count),
            total_pop = sum(population)) %>% 
  mutate(proportion_cases = total_cases/total_pop * 1000)

# Load ggplot package
library(ggplot2)

# Plot a line graph of year vs proportion of cases 
ggplot(subdata_measles, aes(year, proportion_cases)) +
  geom_line() +
  labs(y = "Cases per 1,000",
       title = "US Measles Cases per 1,000",
       subtitle = "Years. Period: 1945-1950")

# Include all diseases except smallpox from dataset during years 1945-1950
subdata_all <- us_contagious_diseases %>% 
  filter(year %in% c("1945":"1950"),
         disease != "Smallpox") %>% 
  select(-weeks_reporting, -state)

# Find proportion of cases for all diseases within each year
subdata_all <- subdata_all %>% 
  group_by(disease, year) %>% 
  summarize(total_cases = sum(count),
            total_pop = sum(population)) %>%
  mutate(proportion_cases = total_cases/total_pop * 10000)

# Plot line graph year vs proportion cases for all disease with different 
# line color for each disease 
ggplot(subdata_all, aes(year, proportion_cases, color = disease)) +
  geom_line (show.legend = FALSE) +
  labs(y = "Cases per 10,000",
       title = "US cases by disease per 10,000 ",
       subtitle = "Years. Period: 1945-1950") +
  facet_grid(rows = vars(disease),
             scales = "free_y") +
  theme_bw()
