# Set working directory and files

setwd('~/Documents/Programming Projects/Data Wrangling in R/Exercise 1/Basic Data Manipulation - Company Products/')
d = read.csv("refine_original.csv")
head(d)

library(dplyr)
library(tidyr)
library(lubridate)

# 1: Clean up brand names

d1 <- (
  d%>%
    mutate(company = sub('[Aa][Kk].*', 'akzo', company))%>%
    mutate(company = sub('[Pp]h.*|f.*', 'philips', company))%>%
    mutate(company = sub('[Vv]a.*', 'van houten', company))%>% 
    mutate(company = sub('[Uu]n.*', 'unilever', company)))
d1
  
# 2: Separate product code and number

d2 <- separate(d1, Product.code...number., c("product_code", "product_number"), sep = "-")
d2

# 3: Add product categories

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
  
d5 <- unite(d4, "full_address", address, city, country, sep = ', ')
d5


# 5: Create dummy variables for company and product category

d5$company_philips <- ifelse(d5$company == "philips", 1, 0)
d5$company_akzo <- ifelse(d5$company == "akzo", 1, 0)
d5$company_van_houten <- ifelse(d5$company == "van houten", 1, 0)
d5$company_company_unilever <- ifelse(d5$company == "unilever", 1, 0)
d5$product_smartphone <- ifelse(d5$product_category == "akzo", 1, 0)
d5$product_tv <- ifelse(d5$product_category == "tv", 1, 0)
d5$product_laptop <- ifelse(d5$product_category == "laptop", 1, 0)
d5$product_tablet <- ifelse(d5$product_category == "tablet", 1, 0)
d5

# 6: Submit the project on Github

refine_clean = data.frame(d5)
write.csv(refine_clean, file = "refine_clean.csv", row.names = FALSE)

