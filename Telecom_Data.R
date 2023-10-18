# Install packages
library(tidyverse)
library(readxl)

# Read in dataset
customer_df <- read_excel("Data/Customer_Data.xlsx")

# Select variables to be shared 
customer_selects_df <- customer_df %>% 
  select(gender = Gender, age = Age, phoneco_tenure = PhoneCoTenure, 
         education_years = EducationYears, marital_status = MaritalStatus)