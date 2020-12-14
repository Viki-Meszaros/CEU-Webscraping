###############################
##        Assignment 2       ##
##   for Web scraping class  ##
##                           ##
##      Getting data from    ##
##          PAYSCALE         ##
##                           ##
## I. - Downloading all the  ##
##   htmls for all the jobs  ##
###############################

# Clean the environment and load packages
rm(list = ls())

library(tidyverse)
library(rvest)
library(data.table)
library(jsonlite)
library(pbapply)

my_path <- "C:/Users/admin/Documents/CEU/Fall_semester/Coding_2/Webscraping/Assignment_2/Data/"

# Get the links for all the industries
t_industry <- read_html("https://www.payscale.com/research/US/Job")
industry_list <- list()

industry_list[['Industry']] <- 
  t_industry %>% 
  html_nodes('.related-content-card__title') %>% 
  html_text()

industry_list[['Link']] <- 
  t_industry %>% 
  html_nodes('.related-content-card') %>% 
  html_attr('href')

industry_links <- data.frame(industry_list)

saveRDS(industry_links, paste0(my_path, "Industries.rds"))

# Get the links for all the jobs
jobs_list <- list()

link <- industry_links$Link

get_job_links <- function(i){    

  t_jobs <- read_html(paste0("https://www.payscale.com", i))
  
  jobs_list[["Job"]] <- c(jobs_list[["Job"]],
    t_jobs %>% 
    html_nodes(".subcats__links__item") %>% 
    html_text())
  
  jobs_list[["Link"]] <- c(jobs_list[["Link"]],
    t_jobs %>% 
    html_nodes(".subcats__links__item") %>% 
    html_attr('href'))
  
  job_links <- data.frame(jobs_list) 
  job_links$Industry <- gsub("/research/US/Job/", "", i) 

  return(job_links)
}

# job_links will contain all the jobs, the industry they are in and the link for their own separate pages
job_links <- rbindlist(pblapply(link, get_job_links))

# we add three more columns to this table, File_names which will help with downloading the htmls, 
# and category that shows if the amount of money is salary or an hourly wage
job_links$File_names <- gsub("/", "_", gsub(" ", "_", gsub(",", "", gsub(" / ", "_", job_links$Job, fixed = T)), fixed = T))
job_links$Category <- sub('.*/', '', job_links$Link)

saveRDS(job_links, paste0(my_path, "Jobs.rds"))

# If you don't want to run the codes above, you can import the data with importing the job_links data set 
# job_links <- readRDS(my_path, "Jobs.rds")


#####################################
##      Getting all the htmls      ##
#####################################

# Download all the Json files for all the jobs
salary_lists <- list()

for (i in 1:nrow(job_links)) {
  ds <- read_html(paste0("https://www.payscale.com", job_links$Link[i]))
  write_html(ds, paste0(my_path, "Htmls/", job_links$File_names[i], ".html" ))
}


rm(t_industry, industry_list, jobs_list)

