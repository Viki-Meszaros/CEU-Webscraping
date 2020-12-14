###############################
##        Assignment 2       ##
##   for Web scraping class  ##
##                           ##
##      Getting data from    ##
##          PAYSCALE         ##
##                           ##
## II. - Creating the tables ##
##   by location, employer,  ##
##   experience and skills.  ##
###############################


# Clean the environment and load packages
rm(list = ls())

library(tidyverse)
library(rvest)
library(data.table)
library(jsonlite)
library(pbapply)

my_path <- "C:/Users/admin/Documents/CEU/Fall_semester/Coding_2/Webscraping/Assignment_2/Data/"

# import job_links table
job_links <- readRDS(paste0(my_path, "Jobs.rds"))

files <- list.files(paste0(my_path, "Htmls"), full.names = T)
print(files, max = 3000)

#####################################
##  Function to get overview table ##
#####################################

get_jobs <- function(i) {
  tdf <- data.table()
  html <- read_html(i)
  job <- gsub("_", " ", 
              gsub(".html", "", 
                   gsub(paste0(my_path, "Htmls/"), "", i)))
  t_file <- gsub(".html", "", tail(strsplit(i, "/")[[1]], 1))
  
  salary_list <- fromJSON(html %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  job_industry <- job_links[File_names == t_file]$Industry[1]
 
  
  tdf$Job <- salary_list$props$pageProps$pageData$dimensions$job
  tdf$industry <- job_industry
  tdf$Gets <- salary_list$props$pageProps$pageData$reportType
  tdf$Salary <- salary_list$props$pageProps$pageData$byDimension$`Average Salary Overall`$rows$range.50
  tdf$Hourly_wage <- salary_list$props$pageProps$pageData$byDimension$`Average Hourly Rate Overall`$rows$range.50
  tdf$Description <- salary_list$props$pageProps$pageData$narratives$description
  tdf$Tasks <- paste0(salary_list$props$pageProps$pageData$tasks, collapse = ' ')
  tdf$Skills <- salary_list$props$pageProps$pageData$occupationalDetails$skills
  tdf$Satisfaction <- salary_list$props$pageProps$pageData$ratings$`Job Satisfaction Overall`$score
  tdf$Sample_size <- salary_list$props$pageProps$pageData$occupationalDetails$sampleSize
  
  return(tdf)
}

overview <- rbindlist(pblapply(files, get_jobs), fill = T)

saveRDS(overview, paste0(my_path, "Overview.rds"))



#####################################
##  Function to get all locations  ##
#####################################

get_location <- function(i) {
  html <- read_html(i)
  job <- gsub("_", " ", 
              gsub(".html", "", 
                   gsub(paste0(my_path, "Htmls/"), "", i)))
  
  salary_list <- fromJSON(html %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  
  tdf <- data.table(salary_list$props$pageProps$pageData$byDimension$`Job by Location`$rows)
  t_file <- gsub(".html", "", tail(strsplit(i, "/")[[1]], 1))
  job_industry <- job_links[File_names == t_file]$Industry[1]
  job_category <- job_links[File_names == t_file]$Category[1]
  
  tdf$Job <- job
  tdf$industry <- job_industry
  tdf$category <- job_category
  tdf$displayName <- NULL
  tdf$isEstimated <- NULL
  tdf$url <- NULL
  
  return(tdf)
}

all_location <- rbindlist(pblapply(files, get_location), fill = T)

all_location <- separate(all_location, name, c("City", "State"), sep = ", ")

saveRDS(all_location, paste0(my_path, "All_locations.rds"))

# for (i in 1:nrow(all_location)) {
#   if (all_location$Job[i] %in% job_links$Job) { 
#     all_location$industry[i] <- job_links$Industry[job_links$Job == all_location$Job[i]]
#     all_location$Category[i] <- job_links$Category[job_links$Job == all_location$Job[i]]
#     }
# }

#####################################
## Function to get all experiences ##
#####################################

get_experiences <- function(i) {
  html <- read_html(i)
  job <- gsub("_", " ", 
              gsub(".html", "", 
                   gsub(paste0(my_path, "Htmls/"), "", i)))
  
  salary_list <- fromJSON(html %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  
  tdf <- data.table(salary_list$props$pageProps$pageData$byDimension$`Job by Experience`$rows)
  t_file <- gsub(".html", "", tail(strsplit(i, "/")[[1]], 1))
  job_industry <- job_links[File_names == t_file]$Industry[1]
  job_category <- job_links[File_names == t_file]$Category[1]
  
  tdf$Job <- job
  tdf$industry <- job_industry
  tdf$category <- job_category
  tdf$displayName <- NULL
  tdf$isEstimated <- NULL
  tdf$url <- NULL
  
  return(tdf)
}

all_experiences <- rbindlist(pblapply(files, get_experiences), fill = T)

saveRDS(all_experiences, paste0(my_path, "All_experiences.rds"))



#####################################
##  Function to get all Employers  ##
#####################################

get_employers <- function(i) {
  html <- read_html(i)
  job <- gsub("_", " ", 
              gsub(".html", "", 
                   gsub(paste0(my_path, "Htmls/"), "", i)))
  
  salary_list <- fromJSON(html %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  
  tdf <- data.table(salary_list$props$pageProps$pageData$byDimension$`Job by Employer`$rows)
  t_file <- gsub(".html", "", tail(strsplit(i, "/")[[1]], 1))
  job_industry <- job_links[File_names == t_file]$Industry[1]
  job_category <- job_links[File_names == t_file]$Category[1]
  
  tdf$Job <- job
  tdf$industry <- job_industry
  tdf$category <- job_category
  tdf$displayName <- NULL
  tdf$isEstimated <- NULL
  tdf$url <- NULL
  
  return(tdf)
}

all_employers <- rbindlist(pblapply(files, get_employers), fill = T)

saveRDS(all_employers, paste0(my_path, "All_employers.rds"))


#####################################
##    Function to get all Skills   ##
#####################################

get_skills <- function(i) {
  html <- read_html(i)
  job <- gsub("_", " ", 
              gsub(".html", "", 
                   gsub(paste0(my_path, "Htmls/"), "", i)))
  
  salary_list <- fromJSON(html %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  
  tdf <- data.table(salary_list$props$pageProps$pageData$byDimension$`Job by Skill`$rows)
  t_file <- gsub(".html", "", tail(strsplit(i, "/")[[1]], 1))
  job_industry <- job_links[File_names == t_file]$Industry[1]
  job_category <- job_links[File_names == t_file]$Category[1]
  
  tdf$Job <- job
  tdf$industry <- job_industry
  tdf$category <- job_category
  tdf$displayName <- NULL
  tdf$isEstimated <- NULL
  tdf$url <- NULL
  
  return(tdf)
}

all_skills <- rbindlist(pblapply(files, get_skills), fill = T)

saveRDS(all_skills, paste0(my_path, "All_skills.rds"))
















