# process_data.R
#
# This script transforms the original data into a format that can be
# easily analyzed.

# The original data files were downloaded from
# https://www.who.int/healthinfo/global_burden_disease/estimates/en/

library(tibble)
library(readxl)
library(reshape2)
library(dplyr)


# Input files --------------------------------------------------------------

# Numbers of deaths (* 1000)
datafile_numbers <- "data/GHE2016_Deaths_2016-country.xlsx"

# Mortality rates (per 100 000 population)
datafile_rates <- "data/GHE2016_Death-Rates-country.xlsx"


# Functions ----------------------------------------------------------------

# Total population numbers
get_pop_data <- function(indata) {
  
  # Extract population rows
  pop_total <- indata[3, ]
  pop_males <- indata[210, ]
  pop_females <- indata[417, ]
  
  pop <- rbind(pop_total, pop_males, pop_females)
  
  # Remove unnecessary columns
  pop <- pop[-c(2, 3, 4, 5, 6, 7)]
  
  # Convert strings to numbers
  pop[, 2:184] <- lapply(pop[, 2:184], as.numeric)
  
  # Transpose
  out_data <- melt(pop, id = c("Sex"),
                   factorsAsStrings = FALSE) %>%
    mutate(Age = age_group) %>%
    rename(Country = variable,
           pop = value)
  
  out_data$Country <- as.character(out_data$Country)
  
  return (out_data)
}

# Create transposed version of numbers of deaths
get_numbers <- function(indata, agegp) {
  
  # Delete unnecessary columns
  indata <- indata[-c(2, 3, 4)]
  
  # Convert strings to numbers
  indata[, 5:187] <- lapply(indata[, 5:187], as.numeric)
  
  # Rename columns
  names(indata)[2:4] <- c("Group", "Category", "Disease")
  
  
  # Populate category names
  
  # Populate Group
  indata$Group[-1] <- indata$Group[1]
  
  # STDs excluding HIV
  indata$Category[4:9] <- indata$Category[3]
  
  # Childhood-cluster diseases
  indata$Category[13:16] <- indata$Category[12]
  
  # Hepatitis
  indata$Category[20:23] <- indata$Category[19]
  
  # Parasitic and vector diseases
  indata$Category[25:37] <- indata$Category[24]
  
  # Intestinal nematode infections
  indata$Category[39:42] <- indata$Category[38]
  
  
  # Delete summary rows
  indata <- indata[-c(1, 3, 12, 19, 24, 38), ]
  
  # Populate disease as category if empty
  indata$Disease[is.na(indata$Disease)] <- indata$Category[is.na(indata$Disease)]
  
  # Transpose
  out_data <- melt(indata, 
                   id = c("Sex", "Group", "Category", "Disease"),
                   factorsAsStrings = FALSE) %>%
    mutate(Age = agegp) %>%
    rename(Country = variable,
           deaths = value)
  
  out_data$Country <- as.character(out_data$Country)
  
  return (out_data)
} 

# Create transposed version of mortality rates
get_rates <- function(indata) {
  
  # Delete unnecessary columns
  indata <- indata[-c(2, 3, 4)]
  
  # Convert strings to numbers
  indata[, 5:187] <- lapply(indata[, 5:187], as.numeric)
  
  # Rename columns
  names(indata)[2:4] <- c("Group", "Category", "Disease")
  
  
  # Populate category names
  
  # Populate Group
  indata$Group[-1] <- indata$Group[1]
  
  # STDs excluding HIV
  indata$Category[4:9] <- indata$Category[3]
  
  # Childhood-cluster diseases
  indata$Category[13:16] <- indata$Category[12]
  
  # Hepatitis
  indata$Category[20:23] <- indata$Category[19]
  
  # Parasitic and vector diseases
  indata$Category[25:37] <- indata$Category[24]
  
  # Intestinal nematode infections
  indata$Category[39:42] <- indata$Category[38]
  
  
  # Delete summary rows
  indata <- indata[-c(1, 3, 12, 19, 24, 38), ]
  
  # Populate disease as category if empty
  indata$Disease[is.na(indata$Disease)] <- indata$Category[is.na(indata$Disease)]
  
  # Transpose
  out_data <- melt(indata, 
                   id = c("Sex", "Group", "Category", "Disease"),
                   factorsAsStrings = FALSE) %>%
    # mutate(Age = agegp) %>%
    rename(Country = variable,
           rate = value)
  
  out_data$Country <- as.character(out_data$Country)
  
  return (out_data)
}


# End functions


# Process data: Numbers of deaths -------------------------------------

datafile <- datafile_numbers
all_sheets <- excel_sheets(datafile)

# Sheets with age ranges
data_sheets <- all_sheets[3:9]


# Cycle through each Age sheet 
for (s in 1:length(data_sheets)) {
  age_group <- sub("Deaths ", "", data_sheets[s]) 
  sheet_name <- data_sheets[s] 
  
  data <- read_excel(path = datafile, 
                     sheet = sheet_name, 
                     skip = 6)
  
  # Recode Sex values
  data <- data %>%
    mutate(Sex = case_when(
      Sex == "Persons" ~ "All",
      Sex == "Females" ~ "Female",
      Sex == "Males" ~ "Male"
    )
    )
  
  # Extract rows with infectious diseases 
  persons <- data[6:49, ] 
  males <- data[213:256, ]
  females <- data[420:463, ]
  
  persons_numbers <- get_numbers(indata = persons, agegp = age_group)
  males_numbers <- get_numbers(indata = males, agegp = age_group)
  females_numbers <- get_numbers(indata = females, agegp = age_group)
  
  sheet_data <- rbind(persons_numbers, males_numbers, females_numbers)
  
  pop <- get_pop_data(indata = data)
  
  # Append currrent sheet data to cumulative data
  if (s == 1) {
    all_numbers <- sheet_data
    all_pop <- pop
  } else {
    all_numbers <- rbind(all_numbers, sheet_data)
    all_pop <- rbind(all_pop, pop)
  }
  
} # End for sheet loop

# Multiply deaths by 1000 
all_numbers$deaths <- all_numbers$deaths * 1000 

# Multiply population by 1000 
all_pop$pop <- all_pop$pop * 1000



# Process data: Mortality rates -------------------------------------

datafile <- datafile_rates

# Use sheet named "ASDR2016" for "Age-standardized rate per 100 000 population "
sheet_name <- "ASDR2016" 

data <- read_excel(path = datafile, 
                   sheet = sheet_name, 
                   skip = 6)

# Extract country codes
country_codes_row <- data[1, -c(1:7)] 
country_codes <- as.data.frame(t(country_codes_row), stringsAsFactors = FALSE) %>%
  rename(code = V1)
country_codes <- tibble::rownames_to_column(country_codes, "country")

# Recode Sex values
data <- data %>%
  mutate(Sex = case_when(
    Sex == "Persons" ~ "All",
    Sex == "Females" ~ "Female",
    Sex == "Males" ~ "Male"
  )
  )

# Extract rows with infectious diseases 
persons <- data[6:49, ] 
males <- data[213:256, ]
females <- data[420:463, ]

persons_rates <- get_rates(indata = persons)
males_rates <- get_rates(indata = males)
females_rates <- get_rates(indata = females)

all_rates <- rbind(persons_rates, males_rates, females_rates)

# Keep mortality rate as deaths per 100,000 population


# Save datasets --------------------------------------------------------------

saveRDS(all_numbers, file="data/death_numbers.Rds")
saveRDS(all_rates, file="data/death_rates.Rds")
saveRDS(all_pop, file="data/pop.Rds")
saveRDS(country_codes, file="data/country_codes.Rds")


