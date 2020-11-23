
rm(list=ls())

#install.packages("rvest")
library(rvest)
#install.packages("data.table")
library(data.table)
#install.packages("xtable")
library(xtable)
# install.packages("kableExtra")
library(kableExtra)


my_url <- 'https://www.theverge.com/search?q=lego'

# check if I have the data on the url 
# t <- read_html(my_url)
# write_html(t, 'verge.html')

get_one_page <- function(my_url) {
  print(my_url)
  
  t <- read_html(my_url)
  
  boxes <- 
    t %>% 
    html_nodes('.c-entry-box--compact')
  
  box_dfs <- lapply(boxes, function(x){
    tlist <- list()
    tlist[['Title']] <- 
      x %>% 
      html_nodes('.c-entry-box--compact__title a')%>%
      html_text()
    
    tlist[['Link']]  <- 
      x%>% 
      html_nodes('.c-entry-box--compact__title a')%>%
      html_attr('href')
    
    tlist[['Author']] <-
      x %>% 
      html_nodes('.c-byline__author-name')%>%
      html_text()
    
    tlist[['Date']] <-
      trimws(
      x %>% 
      html_nodes('.c-byline__item .c-byline__item')%>%
      html_text())

    tlist[['Num_of_comments']] <-
     as.numeric(trimws(
        x %>%
      html_node('.c-entry-stat__comment-data')%>%
      html_text()))
    
    return(tlist)
    
  })
  
  df <- rbindlist(box_dfs, fill = T)
  return(df)
}

df <- get_one_page(my_url)

# what to output
print(paste('We have',nrow(df), 'observations in our data frame.'))

df %>%
  kable("html") %>%
  kable_styling(font_size = 10)

# get multiple pages
get_data_from_verge <- function(searchterm = "lego", num_of_page=5) {
  pages <- 
    c(paste0('https://www.theverge.com/search?q=', searchterm), 
      paste0('https://www.theverge.com/search?page=', 2:num_of_page, '&q=', searchterm))
  
  ret_df <- rbindlist(lapply(pages, get_one_page))
  return(ret_df)
}

df <- get_data_from_verge(searchterm = "cat" ,num_of_page = 3)

# what to output
print(paste('We have',nrow(df), 'observations in our data frame.'))

df %>% 
  kable("html") %>%
  kable_styling(font_size = 10)

write.csv(df, 'Verge_scraped_data.csv')
