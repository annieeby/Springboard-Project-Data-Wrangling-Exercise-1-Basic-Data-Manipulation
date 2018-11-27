# Assignment: 
# https://campus.datacamp.com/courses/data-visualization-with-ggplot2-1/chapter-5-qplot-and-wrap-up?ex=8 
# https://www.springboard.com/workshops/data-science/learn#/curriculum/792

# load libraries
library(dplyr)
library(ggplot2)

# select titanic3.csv
setwd('~/Documents/Programming Projects/Data Wrangling in R/Exercise 2 Dealing with missing values/')
titanic = read.csv("titanic3.csv")
titanic

# titanic is avaliable in your workspace
# 1 - Check the structure of titanic
str(titanic)

# 2 - Use ggplot() for the first instruction
ggplot(titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge")

# 3 - Plot 2, add facet_grid() layer
ggplot(titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge")+
  facet_grid(. ~ survived) 

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
ggplot(titanic, aes(x = pclass, y = age, col = sex)) +
  geom_point(size=3, alpha = 0.5, position = posn.jd)+
  facet_grid(. ~ survived) 

