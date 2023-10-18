# Install packages
library(tidyverse)
library(readxl)

# Read in dataset
customer_df <- read_excel("Data/Customer_Data.xlsx")

# Select variables to be shared 
customer_selects_df <- customer_df %>% 
  select(gender = Gender, age = Age, phoneco_tenure = PhoneCoTenure, 
         education_years = EducationYears, marital_status = MaritalStatus)


# Perform data quality control - Check for outliers and null values -----------

cat(ifelse(any(customer_selects_df$gender != 0 & customer_selects_df$gender != 1), 
           paste("Numbers in 'gender' column that are not 0 or 1:", 
                 customer_selects_df$gender[customer_selects_df$gender != 0 & customer_selects_df$gender != 1], sep = "\n"), 
           "None found in 'gender' column"), "\n")
cat(ifelse(length(customer_selects_df$gender[is.na(customer_selects_df$gender)]) > 0,
           paste("NULL or NA values in 'gender' column:", 
                 customer_selects_df$gender[is.na(customer_selects_df$gender)], 
                 sep = "\n"), "None found in 'gender' column"), "\n")
# No outliers or null values found in the gender column 

customer_selects_df %>% 
  arrange(age) %>% 
  head(5) %>% 
  print()
customer_selects_df %>% 
  arrange(desc(age)) %>% 
  head(5) %>% 
  print()
cat(ifelse(length(customer_selects_df$age[is.na(customer_selects_df$age)]) > 0,
           paste("NULL or NA values in 'age' column:", 
                 customer_selects_df$age[is.na(customer_selects_df$age)], 
                 sep = "\n"), "None found in 'age' column"), "\n")
# No outliers or null values found in the age column 

customer_selects_df %>% 
  arrange(phoneco_tenure) %>% 
  head(5) %>% 
  print()
customer_selects_df %>% 
  arrange(desc(phoneco_tenure)) %>% 
  head(5) %>% 
  print()
cat(ifelse(length(customer_selects_df$phoneco_tenure[is.na(customer_selects_df$phoneco_tenure)]) > 0,
           paste("NULL or NA values in 'phoneco_tenure' column:", 
                 customer_selects_df$phoneco_tenure[is.na(customer_selects_df$phoneco_tenure)], 
                 sep = "\n"), "None found in 'phoneco_tenure' column"), "\n")
# No outliers or null values found in the phoneco_tenure column 

customer_selects_df %>% 
  arrange(education_years) %>% 
  head(5) %>% 
  print()
customer_selects_df %>% 
  arrange(desc(education_years)) %>% 
  head(5) %>% 
  print()
cat(ifelse(length(customer_selects_df$education_years[is.na(customer_selects_df$education_years)]) > 0,
           paste("NULL or NA values in 'education_years' column:", 
                 customer_selects_df$education_years[is.na(customer_selects_df$education_years)], 
                 sep = "\n"), "None found in 'education_years' column"), "\n")
# No outliers or null values found in the education_years column 

cat(ifelse(any(customer_selects_df$marital_status != 0 & customer_selects_df$marital_status != 1), 
           paste("Numbers in 'marital_status' column that are not 0 or 1:", 
                 customer_selects_df$marital_status[customer_selects_df$marital_status != 0 & customer_selects_df$marital_status != 1], sep = "\n"), 
           "None found in 'marital_status' column"), "\n")
cat(ifelse(length(customer_selects_df$marital_status[is.na(customer_selects_df$marital_status)]) > 0,
           paste("NULL or NA values in 'marital_status' column:", 
                 customer_selects_df$marital_status[is.na(customer_selects_df$marital_status)], 
                 sep = "\n"), "None found in 'marital_status' column"), "\n")
# No outliers or null values found in the marital_status column 


# Check current equivalence classes ---------------------------------------

equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))
# There are currently many equivalence classes with only one customer, and a maximum of 6 customers in an equivalence class


# Perform grouping to decrease the number of unique customers -------------

# Group age by a 15 year range, with <25 and 55+ as the upper and lowers
customer_selects_df$age <- cut(customer_selects_df$age, 
                                     breaks = c(0, 30, 45, 60, 100),
                                     labels = c("<30", "30-44", "45-59", "60+"))

# Check equivalence classes
equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))
# There are still many unique customers, so more grouping needs to be done

# Group education_years by <15 (no college degree) or 15+ (college degree)
customer_selects_df$education_years <- cut(customer_selects_df$education_years, 
                               breaks = c(0, 15, 100),
                               labels = c("<15", "15+"))

# Check equivalence classes
equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))
# There are still many unique customers, so we will now group within the phoneco_tenure column

# Group phoneco_tenure by 24 months
customer_selects_df$phoneco_tenure <- cut(customer_selects_df$phoneco_tenure, 
                                          breaks = c(-1, 24, 48, 72, 100),
                                          labels = c("<24", "24-47", "48-72", "72+"))

# Check equivalence classes
equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))

# Add in column for probability of re-identification ----------------------

equivalence_classes <- equivalence_classes %>%
  mutate(p_re_id = round(1/ class_size, 2))
