###############################
##        Assignment 2       ##
##   for Web scraping class  ##
##                           ##
##      Getting data from    ##
##          PAYSCALE         ##
##                           ##
##      III. - Analysis      ##
###############################

# Clean the environment and load packages
rm(list = ls())

library(tidyverse)
library(plotly)


# Importing the data tables
overview <- readRDS("C:/Users/admin/Documents/CEU/Fall_semester/Coding_2/Webscraping/Assignment_2/Data/Overview.rds")
locations <- readRDS("C:/Users/admin/Documents/CEU/Fall_semester/Coding_2/Webscraping/Assignment_2/Data/All_locations.rds")
employers <- readRDS("C:/Users/admin/Documents/CEU/Fall_semester/Coding_2/Webscraping/Assignment_2/Data/All_employers.rds")
experience <- readRDS("C:/Users/admin/Documents/CEU/Fall_semester/Coding_2/Webscraping/Assignment_2/Data/All_experiences.rds")
skills <- readRDS("C:/Users/admin/Documents/CEU/Fall_semester/Coding_2/Webscraping/Assignment_2/Data/All_skills.rds")

unique(overview$industry)

# Salaries by industries
overview %>% 
  group_by(industry) %>% 
  summarise("Average_salary" = mean(Salary, na.rm = T)) %>% 
  ggplot( aes(x = reorder(industry, -Average_salary), y = Average_salary)) + 
  geom_col(fill = "deepskyblue4") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Average salary by industry", x = "Industry", y = "Salary")

# Distribution of salaries
overview %>% 
  filter(Gets == "Salary") %>% 
  filter(Sample_size > 10) %>% 
  ggplot(aes(x = Salary) ) +
  geom_histogram(fill = "deeppink4") +
  labs(title = "Salary distribution", y = "Number of jobs")


# Scatter plot for Salaries and job satisfaction
overview %>% 
  filter(Gets == "Salary") %>% 
  filter(Sample_size > 10) %>% 
ggplot(aes(x = Satisfaction, y = Salary) ) +
  geom_point( color = "deepskyblue4", alpha = 0.7) +
  theme_minimal() +
  labs( title = "Scatterplot for Salary and Satisfaction of the job")

# Salary by states 
locations %>% 
  group_by(State) %>% 
  summarise("Average_salary" = mean(range.50, na.rm = T)) %>% 
  filter(!is.na(State)) %>% 
  arrange(-Average_salary) %>% 
  ggplot( aes(x = reorder(State, -Average_salary), y = Average_salary)) + 
  geom_col(fill = "deeppink4") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title =  "Average salary by state", x = "State", y = "Salary")

# Salary by companies
employers %>% 
  group_by(name) %>% 
  summarise("Average_salary" = mean(range.50, na.rm = T)) %>% 
  arrange(-Average_salary) %>% 
  head(20) %>% 
  ggplot( aes(x = reorder(name, -Average_salary), y = Average_salary)) + 
  geom_col( fill = "deepskyblue4") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title =  "Average salary by employer (top 20)", x = "Employer", y = "Salary")

# Salary by experience level
experience %>% 
  group_by(name) %>% 
  summarise("Average_salary" = mean(range.50, na.rm = T)) %>% 
  filter(!is.na(name)) %>% 
  ggplot( aes(x = reorder(name, Average_salary), y = Average_salary)) + 
  geom_col( fill = "deeppink4") +
  labs(title =  "Average salary by experience level", x = "Experince", y = "Salary")


# Salary for skills by the number they appeared in job descriptions
skills %>% 
  group_by(name) %>% 
  summarise( "number" = n(), "Average_salary" = mean(range.50, na.rm = T) ) %>% 
  filter(!is.na(name)) %>% 
  ggplot(aes(x = number, y = Average_salary)) +
  geom_point(color = "deeppink4", alpha = 0.7) +
  labs(title = "Average salary to number of jobs asked for a given skill", x = "Number of jobs that listed the skill", y = "Average salary")

# Most wanted skills
skills %>% 
  group_by(name) %>% 
  summarise( "number" = n()) %>% 
  filter(!is.na(name)) %>% 
  arrange(-number) %>% 
  head(20) %>% 
  ggplot( aes(x = reorder(name, -number), y = number)) + 
  geom_col( fill = "deepskyblue4") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title =  "Number of jobs needing the skills", x = "Skill", y = "Number of jobs")

# Salary by skills
skills %>% 
  group_by(name) %>% 
  summarise("Average_salary" = mean(range.50, na.rm = T)) %>% 
  arrange(-Average_salary) %>% 
  head(20) %>% 
  ggplot( aes(x = reorder(name, -Average_salary), y = Average_salary)) + 
  geom_col( fill = "deeppink4") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title =  "Average salary by skills (top 20)", x = "Skill", y = "Salary")

skills %>% 
  group_by(name) %>% 
  summarise("Average_salary" = mean(range.50, na.rm = T)) %>% 
  filter(!is.na(name)) %>% 
  filter(Average_salary != 0) %>% 
  arrange(-Average_salary) %>% 
  tail(20) %>% 
  ggplot( aes(x = reorder(name, -Average_salary), y = Average_salary)) + 
  geom_col( fill = "deepskyblue4") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title =  "Average salary by skills (bottom 20)", x = "Skill", y = "Salary")








