# Exercise
#
# Using R, clean this data set to make it easier to visualize and analyze.
# Specifically, these are the tasks you need to do:
#
# 0: Load the data in RStudio
#
# Save the data set as a CSV file called refine_original.csv and load it in
# RStudio into a data frame.

# Note: These did not work: 
# read.csv(file="refine_original.csv", header=TRUE, sep=",")
# myfile = read.csv([FILE_PATH_FOR_DATACAMP_FOLDER]/refine_original.csv")


f <- file.choose()
d <- read.csv(f)
d

library(dplyr)
library(tidyr)
library(lubridate)

# 1: Clean up brand names
#
# Clean up the 'company' column so all of the misspellings of the brand names
# are standardized. For example, you can transform the values in the column to
# be: philips, akzo, van houten and unilever (all lowercase).

d1 <- (
  d%>%
    mutate(company = sub('[Aa][Kk].*', 'akzo', company))%>%
    mutate(company = sub('[Pp]h.*|f.*', 'philips', company))%>%
    mutate(company = sub('[Vv]a.*', 'van houten', company))%>% 
    mutate(company = sub('[Uu]n.*', 'unilever', company)))
d1

# Note: .* means "anything" https://campus.datacamp.com/courses/intermediate-r/chapter-5-utilities?ex=10 

  # https://foundationsdscommunity.springboard.com/t/m2j24b/data-wrangling-ex1
  
# 2: Separate product code and number

# Separate the product code and product number into separate columns i.e. add
# two new columns called product_code and product_number, containing the product
# code and number respectively

d2 <- separate(d1, Product.code...number., c("product_code", "product_number"), sep = "-")
d2

# 3: Add product categories
#
# You learn that the product codes actually represent the following product
# categories:
#
# p = Smartphone
# v = TV
# x = Laptop
# q = Tablet
#
# In order to make the data more readable, add a column with the product
# category for each record.

d3 <- (
  d2%>%
    mutate(product_code = sub('p', 'p = Smartphone', product_code))%>%
    mutate(product_code = sub('v', 'v = TV', product_code))%>%
    mutate(product_code = sub('x', 'x = Laptop', product_code))%>%
    mutate(product_code = sub('q', 'q = Tablet', product_code)))
d3  

d4 <- separate(d3, product_code, c("product_code", "product_category"), sep = " = ")  
d4

  
# 4: Add full address for geocoding
#
# You'd like to view the customer information on a map. In order to do that, the
# addresses need to be in a form that can be easily geocoded. Create a new
# column full_address that concatenates the three address fields (address, city,
# country), separated by commas.
  
d5 <- unite(d4, "full_address", address, city, country, sep = ', ')
d5

#Tools: tidyr

# 5: Create dummy variables for company and product category
#
# Both the company name and product category are categorical variables i.e. they
# take only a fixed set of values. In order to use them in further analysis you
# need to create dummy variables. Create dummy binary variables for each of them
# with the prefix company_ and product_ i.e.,
#
# Add four binary (1 or 0) columns for company: company_philips, company_akzo,
# company_van_houten and company_unilever.
#
# Add four binary (1 or 0) columns for product category: product_smartphone,
# product_tv, product_laptop and product_tablet.


d5$company_philips <- ifelse(d5$company == "philips", 1, 0)
d5$company_akzo <- ifelse(d5$company == "akzo", 1, 0)
d5$company_van_houten <- ifelse(d5$company == "van houten", 1, 0)
d5$company_company_unilever <- ifelse(d5$company == "unilever", 1, 0)
d5$product_smartphone <- ifelse(d5$product_category == "akzo", 1, 0)
d5$product_tv <- ifelse(d5$product_category == "tv", 1, 0)
d5$product_laptop <- ifelse(d5$product_category == "laptop", 1, 0)
d5$product_tablet <- ifelse(d5$product_category == "tablet", 1, 0)
d5

# https://stackoverflow.com/questions/19970009/create-a-column-with-binary-data-based-on-another-column 

# 6: Submit the project on Github
#
# Include your code, the original data as a CSV file refine_original.csv, and
# the cleaned up data as a CSV file called refine_clean.csv.

refine_clean <- write.table(d5)
write.csv(refine_clean, file = "refine_clean.csv")
refine_clean


# Note: cannot find file, found one and it was empty...?
