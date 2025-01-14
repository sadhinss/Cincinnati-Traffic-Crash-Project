#----------------------------------------------------------------
# Traffic Crash Data Analysis Project
#
# Created:  12/20/2024
#
# Last Modified:  12/30/2024  
#
# Authors: Shams Sadhin
#----------------------------------------------------------------

#----------------------------------------------------------------
# This code is open source feel free to add any modification you wish
#----------------------------------------------------------------
# Set up R 
#----------------------------------------------------------------
# Clear environment, which means totally clear R environment
# of objects and loaded packages
rm(list=ls())

# To clear just the console window, type "Ctrl+L" or use Edit pull down menu

# Specify a display option
#options("scipen"=999, digits=2)

# === Set the working directory.  Note that R uses "/" not "\"
# === So the command is setwd("your directory path") or use the Session pull down menu
setwd("~/R/")
# === NOTE:  If using a MAC computer, you might need to replace the above command with something like
# === setwd("Desktop/R/")
#----------------------------------------------------------------

#----------------------------------------------------------------
# List of packages 
packages <- c("tidyverse", "readr", "dplyr","ggplot2") # INSERT THE PACKAGE YOU WANT TO USE IN HERE!

# Install the packages
# Run this code to install packages you do not have installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) 
{install.packages(packages[!installed_packages])}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))
search()

# Set the number of digits to display when using the jtools commands, like summ
options("jtools-digits"=4) 
options(scipen = 999)

# You can use these methods if you need a example of how to use a package
browseVignettes("AER")  # Short documents on how to use the packages
?mean # Opens the help page for the mean function
?"+" #  Opens the help page for addition
?"if" # Opens the help page for if, used for branching code
??plotting #  Searches for topics containing words like "plotting"
??"regression model" #  Searches for topics containing phrases like this
#--------------------------------------------------------

#--------------------------------------------------------
# Traffic Data
#--------------------------------------------------------
# Load in the datasets

### LOAD DATA
Traffic_Crash_Reports_CPD_20241220 <- read_csv("Traffic_Crash_Reports__CPD__20241220.csv")

# Data set inspection 
View(Traffic_Crash_Reports_CPD_20241220)

#--------------------------------------------------------
# Data cleaning
#--------------------------------------------------------
#Check minimum and maximum values of latitude and logitude

lat_min <- min(Traffic_Crash_Reports_CPD_20241220$LATITUDE_X, na.rm = TRUE)
lat_max <- max(Traffic_Crash_Reports_CPD_20241220$LATITUDE_X, na.rm = TRUE)

long_min <- min(Traffic_Crash_Reports_CPD_20241220$LONGITUDE_X, na.rm = TRUE)
long_max <- max(Traffic_Crash_Reports_CPD_20241220$LONGITUDE_X, na.rm = TRUE)

#Set cincinnati geographic range 

lat_min <- 38.9
lat_max <- 39.5
lon_min <- -85.0
lon_max <- -84.2

#Filter rows within Cincinnati
cincinnati_traffic_crash <- Traffic_Crash_Reports_CPD_20241220 %>% 
  filter(
    LATITUDE_X >= lat_min & LATITUDE_X <= lat_max &
      LONGITUDE_X >= lon_min & LONGITUDE_X <= lon_max
  )


view(cincinnati_traffic_crash)

#total count of missing values in each column 

missing_summary <- colSums(is.na(Traffic_Crash_Reports_CPD_20241220
                                 ))
print(missing_summary)

#Add a CRASH_ID column

cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  mutate(CRASH_ID = row_number())

head(cincinnati_traffic_crash$CRASH_ID)

#Filter rows where GENDER and AGE are not missing
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  filter(!is.na(GENDER) & !is.na(AGE))

#Drop the CRASHDATE and ROADCLASS columns
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  select(-CRASHDATE, -ROADCLASS)

# View the cleaned dataset
head(cincinnati_traffic_crash)

cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  select(-CRASHLOCATION)
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  select(-ROADCLASSDESC)
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  filter(!is.na(ADDRESS_X) & !is.na(UNITTYPE))

#Total count of the missing values in each column 

missing_summary <- colSums(is.na(cincinnati_traffic_crash))
print(missing_summary)


# Clean columns with symbols infront
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  mutate(across(
    c(CRASHSEVERITY, GENDER, INJURIES, UNITTYPE, LIGHTCONDITIONSPRIMARY, 
      MANNEROFCRASH, ROADCONDITIONSPRIMARY,ROADCONTOUR, WEATHER, ROADSURFACE, UNITTYPE, TYPEOFPERSON),
    ~ sub("^\\d+ - ", "", .)
  ))

# Clean TYPEOFPERSON column
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  mutate(TYPEOFPERSON = sub("^\\w+ - ", "", TYPEOFPERSON))
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  mutate(GENDER = sub("^\\w+ - ", "", GENDER))


# Check columns
columns_to_check <- c("CRASHSEVERITY", "GENDER", "INJURIES", "UNITTYPE", 
                      "LIGHTCONDITIONSPRIMARY", "MANNEROFCRASH", 
                      "ROADCONDITIONSPRIMARY", "ROADCONTOUR", "WEATHER", 
                      "ROADSURFACE", "TYPEOFPERSON")

# Check unique values for each column
unique_values <- lapply(cincinnati_traffic_crash[columns_to_check], unique)

# Print unique values for each column
unique_values

# Create age groups

cincinnati_traffic_crash <- cincinnati_traffic_crash %>% 
  mutate(Age_Group = cut(AGE,
                         breaks = c(0,17,25,30,40,50,60,70,Inf),
                         labels = c("0-17", "17-25", "25-30", "30-40", "40-50", "50-60","60-70", "70+"),
                         right = TRUE,
                         include.lowest = TRUE))


# View the first few rows to verify
head(cincinnati_traffic_crash$Age_Group)

# Combine DARK - ROADWAY NOT LIGHTED and UNKNOWN + OTHER

cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  mutate(LIGHTCONDITIONSPRIMARY = case_when(
    LIGHTCONDITIONSPRIMARY %in% c("DARK – ROADWAY NOT LIGHTIED", "DARK - NOT LIGHTED") ~ "DARK - NOT LIGHTED",
    LIGHTCONDITIONSPRIMARY %in% c("UNKNOWN", "OTHER") ~ "UNKNOWN",
    TRUE ~ LIGHTCONDITIONSPRIMARY 
  ))


# View unique values in LIGHTCONDITIONSPRIMARY
unique(cincinnati_traffic_crash$LIGHTCONDITIONSPRIMARY)


# Combine duplicate values in ROADCONDITIONSPRIMARY
cincinnati_traffic_crash <- cincinnati_traffic_crash %>%
  mutate(ROADCONDITIONSPRIMARY = case_when(
    ROADCONDITIONSPRIMARY %in% c("UNKNOWN", "OTHER") ~ "UNKNOWN",
    ROADCONDITIONSPRIMARY %in% c("SLUSH", "SNOW") ~ "SNOW",
    ROADCONDITIONSPRIMARY %in% c("WATER (STANDING, MOVING)", "WET") ~ "WET",
    TRUE ~ ROADCONDITIONSPRIMARY  
  ))

# View unique values in ROADCONDITIONSPRIMARY
unique(cincinnati_traffic_crash$ROADCONDITIONSPRIMARY)

# Export the dataset to a CSV file
write.csv(cincinnati_traffic_crash, "Cincinnati_Traffic_Crash_Cleaned_Final.csv", row.names = FALSE)

# Data visualization

#filter data where injuries are fatal and not fatal

fatal_injuries <- cincinnati_traffic_crash %>%
  filter(INJURIES %in% c("FATAL","INCAPACITATING"))

not_fatal_injuries <- cincinnati_traffic_crash %>%
  filter(!(INJURIES %in% c("FATAL","INCAPACITATING")))

# How does crash injury fatality compare between different age groups?

unique(fatal_injuries$AGE)

max_age <- max(fatal_injuries$AGE, na.rm = TRUE)
min_age <- min(fatal_injuries$AGE, na.rm = TRUE)

print(max_age)
print(min_age)

# Count the number of occurrences for each age group
age_group_counts <- fatal_injuries %>%
  group_by(Age_Group) %>%
  summarize(Count = n(), .groups = 'drop')

# View the counts
print(age_group_counts)


# Plot Bar Graph of fatal injuries by age group
ggplot(age_group_counts, aes(x = Age_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Fatal Injuries by Age Group",
       x = "Age Group",
       y = "Count") +
  scale_fill_brewer(palette = "Paired") 

# Filter data set for drivers
driver_data <- fatal_injuries %>%
  filter(TYPEOFPERSON == "DRIVER")

# Count the number of occurrences for each age group
age_group_counts <- driver_data %>%
  group_by(Age_Group) %>%
  summarize(Count = n(), .groups = 'drop')

# View the counts
print(age_group_counts)

# Bar plot of fatal injuries by age group for drivers
ggplot(age_group_counts, aes(x = Age_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Driver Fatal Injuries by Age Group",
       x = "Age Group",
       y = "Count") +
  scale_fill_brewer(palette = "Paired")

# Which manners of crash leads to fatalities

# Count the number of occurances for each manner of crash
mannerofcrash_counts <- fatal_injuries %>% 
  group_by(MANNEROFCRASH) %>% 
  summarize(Count = n(), groups = 'drop')

print(mannerofcrash_counts)

# Bar plot of fatal injuries by manners of crash
ggplot(mannerofcrash_counts, aes(x = MANNEROFCRASH, y = Count, fill = MANNEROFCRASH)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Manners of Crash for Fatal Injuries",
       x = "Manners of Crash",
       y = "Count") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Count the number of occurances for each manner of crash for drivers
mannerofcrash_counts <- driver_data %>% 
  group_by(MANNEROFCRASH) %>% 
  summarize(Count = n(), groups = 'drop')

print(mannerofcrash_counts)

# Bar plot of driver fatal injuries by manners of crash
ggplot(mannerofcrash_counts, aes(x = MANNEROFCRASH, y = Count, fill = MANNEROFCRASH)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Manners of Crash for Driver Fatal Injuries",
       x = "Manners of Crash",
       y = "Count") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 


# How does road conditions, light conditions and road contour vary between high fatal and non-fatal crashes


# How does Road Contour Vary between fatal and non fatal crashes?

# Aggregate counts for fatal injuries
fatal_road_contour <- fatal_injuries %>%
  group_by(ROADCONTOUR) %>%  
  summarise(Count = n()) %>%
  mutate(InjuryType = "Fatal")

# Aggregate counts for non-fatal injuries
nonfatal_road_contour <- not_fatal_injuries %>%
  group_by(ROADCONTOUR) %>% 
  summarise(Count = n()) %>%
  mutate(InjuryType = "Non-Fatal")

# Combine the datasets
road_contour_combined <- bind_rows(fatal_road_contour, nonfatal_road_contour)

# Calculate percentages
road_contour_combined <- road_contour_combined %>%
  group_by(InjuryType) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

#Visualization
ggplot(road_contour_combined, aes(x = ROADCONTOUR, y = Percentage, fill = InjuryType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~InjuryType, scales = "fixed") +  
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Road Contour: Fatal vs Non-Fatal Injuries (Percentage Labels)",
       x = "Road Contour",
       y = "Percentage",
       fill = "Injury Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# How light condition varies for fata and non fatal injuries

# Aggregate counts for fatal injuries
fatal_light_conditions <- fatal_injuries %>%
  group_by(LIGHTCONDITIONSPRIMARY) %>% 
  summarise(Count = n()) %>%
  mutate(InjuryType = "Fatal")

# Aggregate counts for non-fatal injuries
nonfatal_light_conditions <- not_fatal_injuries %>%
  group_by(LIGHTCONDITIONSPRIMARY) %>%  
  summarise(Count = n()) %>%
  mutate(InjuryType = "Non-Fatal")

# Combine the datasets
light_conditions_combined <- bind_rows(fatal_light_conditions, nonfatal_light_conditions)

# Calculate percentages
light_conditions_combined <- light_conditions_combined %>%
  group_by(InjuryType) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# visualization
ggplot(light_conditions_combined, aes(x = LIGHTCONDITIONSPRIMARY, y = Percentage, fill = InjuryType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~InjuryType, scales = "fixed") + 
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Light Conditions: Fatal vs Non-Fatal Injuries (Percentage Labels)",
       x = "Light Condition",
       y = "Percentage",
       fill = "Injury Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# How Road Conditions vary for fatal and non fatal injuries

# Aggregate counts for fatal injuries
fatal_road_conditions <- fatal_injuries %>%
  group_by(ROADCONDITIONSPRIMARY) %>%  
  summarise(Count = n()) %>%
  mutate(InjuryType = "Fatal")

# Aggregate counts for non-fatal injuries
nonfatal_road_conditions <- not_fatal_injuries %>%
  group_by(ROADCONDITIONSPRIMARY) %>%  
  summarise(Count = n()) %>%
  mutate(InjuryType = "Non-Fatal")

# Combine the datasets
road_conditions_combined <- bind_rows(fatal_road_conditions, nonfatal_road_conditions)

# Calculate percentages
road_conditions_combined <- road_conditions_combined %>%
  group_by(InjuryType) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Visualization
ggplot(road_conditions_combined, aes(x = ROADCONDITIONSPRIMARY, y = Percentage, fill = InjuryType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~InjuryType, scales = "fixed") +  
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Road Conditions: Fatal vs Non-Fatal Injuries (Percentage Labels)",
       x = "Road Condition",
       y = "Percentage",
       fill = "Injury Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))








