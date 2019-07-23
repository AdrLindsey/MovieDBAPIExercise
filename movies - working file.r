

#### Setting the stage ####
rm(list=ls(all=TRUE))

## INSTALL AND LOAD MISSING PACKAGES 
List_Of_Packages <- c("ggplot2","dplyr","tidyr","scales","lubridate","data.table","jsonlite")

New_Packages <- List_Of_Packages[!(List_Of_Packages %in% installed.packages()[,"Package"])]
if(length(New_Packages)) install.packages(New_Packages)

invisible(lapply(List_Of_Packages, library, character.only = TRUE))

## SOME FUNCTIONS TO EXTRACT THE DATA FROM API
getPageCount <- function(start_date,end_date){
  fromJSON(paste0("http://api.themoviedb.org/3/discover/movie?release_date.gte=",start_date,"&release_date.lte=",end_date,"&api_key=606aaffd7ca10f0b80804a1f0674e4e1"))$total_pages
}

get20 <- function(page_number,start_date,end_date){
  dat <- as.data.frame(fromJSON(paste0("http://api.themoviedb.org/3/discover/movie?release_date.gte=",start_date,"&release_date.lte=",end_date,"&api_key=606aaffd7ca10f0b80804a1f0674e4e1&page=",page_number)))
  df <- dat %>% select(results.genre_ids,results.id,results.vote_count,results.vote_average,results.popularity,results.release_date)
  return(df)
}

## QUICK FUNCTION TO CLEAN AND PREPARE DATAFRAME [list(genre) --> seq(genre)]
unlistGenres <- function(results){
  df <- data.frame()
  new_results <- data.frame()
  results$key <- paste0(as.character(results$results.id),"_",results$Month,"_",results$Year)
  ukey <- unique(results$key)
  counter <- 0
  for(i in 1:length(ukey)){
    # remove rows where genre_id is missing
    if(length(unlist(test[i,1])) == 0){
      counter <- counter +1
      next
    }
    foo <- results[i, ]
    mylist <- unlist(foo$results.genre_ids)
    df2 <- data.frame()
    for(j in 1:length(mylist)){
      df2[j,1] <- mylist[[j]]
    }
    df <- cbind(df2,foo[,-1])
    new_results <- rbind(new_results,df)
  }
}

## A COUPLE FUNCITONS FOR QUICK PLOTTING
plotPopularity <- function(dat, val, func, val_divide, topid, titletext){
  dat %>%
    aggregate(val ~ genre_id, data = ., FUN = func) %>%
    mutate(highlight_flag = ifelse(genre_id == topid,F,T)) %>%
    ggplot(aes(reorder(genre_id,-val),val/val_divide)) +
    geom_bar(aes(fill = highlight_flag),stat = "identity") +
    scale_fill_manual(values = c("dodgerblue","darkgray")) +
    
    labs(x = "Genre IDs",
         y = "",
         title = titletext) +
    
    theme_classic() +
    theme(text = element_text(family = 'Helvetica Neue', color = '#444444')
          ,plot.title = element_text(size = 14, face = 'bold')
          ,legend.position = 'none'
          ,axis.title = element_text(face = 'bold')
          ,axis.title.y = element_text(angle = 0, vjust = .5))
}


plotPopularityOverTime <- function(dat, val, func, val_divide, tim, topid, titletext){
  dat %>%
    aggregate(val ~ genre_id + tim, data = ., FUN = func) %>%
    mutate(highlight_flag = ifelse(genre_id == topid,F,T)) %>%
    ggplot(aes(reorder(genre_id,-val),val/val_divide)) +
    geom_bar(aes(fill = highlight_flag),stat = "identity") +
    scale_fill_manual(values = c("dodgerblue","darkgray")) +
    facet_grid(tim~.) +
    
    labs(x = "Genre IDs",
         y = "",
         title = titletext) +
    
    theme_classic() +
    theme(text = element_text(family = 'Helvetica Neue', color = '#444444')
          ,plot.title = element_text(size = 14, face = 'bold')
          ,legend.position = 'none'
          ,axis.title = element_text(face = 'bold')
          ,axis.title.y = element_text(angle = 0, vjust = .5))
}



## PERFORM EXTRACT
this_window <- 24 # looking at 2 yrs
start_dates <- seq(as.Date("2017-01-01"),length=this_window,by="months")
end_dates <- seq(as.Date("2017-02-01"),length=this_window,by="months")-1
results <- data.frame()
limit <- 30 # API only allows 40 queries at a time

system.time(
  #---- ATTENTION: THIS LOOP TAKES 4 HOURS TO RUN
  for(i in 1:this_window){
    print(paste0("Extracting: ",start_dates[i]," through ", end_dates[i]))
    temp_month <- month.abb[month(start_dates[i])]
    temp_year <- year(start_dates[i])
    pages <- getPageCount(start_dates[i],end_dates[i])
    for(page in 1:pages){
      if(page%%limit == 0){
        Sys.sleep(10)
      }
      foo <- get20(page,start_dates[i],end_dates[i])
      foo$Month <- temp_month
      foo$Year <- temp_year
      
      results <- rbind(results,foo)
    }
  }
)

## PERFORM DATA CLEANING
unlistGenres(results)

new_results <- new_results %>% select(-key)
colnames(new_results) <- c("genre_id","id","vote_count","vote_average","popularity","release_date","Month","Year")

plt <- new_results %>% mutate(genre_id = as.factor(genre_id))
dec_plt <- plt %>% filter(Month == "Dec", Year == 2017)



## What is the most popular genre?
## December 2017 ONLY

# Total Interaction
pltTotalInteractionDecember <- plotPopularity(dec_plt, dec_plt$vote_count, sum, 1e3, "18", "18 (Drama) has the most total interactions in Dec 2017 (000s)")
# further inspection
dec_plt2 <- dec_plt %>% group_by(genre_id, id) %>% summarize(movie_count = n()) %>% group_by(genre_id) %>% summarize(d_movie_count = n())
pltTotalMovieCountDecember <- plotPopularity(dec_plt2, dec_plt2$d_movie_count, sum, 1, "18", "18 (Drama) had more movies showing in Dec 2017")

# Average Popularity 
pltAveragePopularityDecember <- plotPopularity(dec_plt, dec_plt$popularity, mean, 1, "18", "10752 (War) has the highest average popularity in Dec 2017")

## How does genre popularity vary by time of year?
## 2 Yrs View

# Total ineraction
pltTotalInteractionOverTime <- plotPopularityOverTime(plt, plt$vote_count, sum, 1e6, plt$Year, "18", "18 (Drama) has the most total interactions (in Millions)")
# Qtry
plt_q <- plt %>% 
  mutate(qtr = ifelse(Month %in% c("Jan", "Feb", "Mar"), "Q1", ifelse(Month %in% c("Apr", "May", "Jun"), "Q2", ifelse(Month %in% c("Jul", "Aug", "Sep"), "Q3", "Q4")))) %>%
  mutate(QtrYr = paste0(qtr,as.character(Year)))
pltTotalInteractionOverTimeQtrly <- plotPopularityOverTime(plt_q, plt_q$vote_count, sum, 1e3, plt_q$QtrYr, "18", "18 (Drama) consistantly has the most total interactions (000s)")
# further inspection
plt2 <- plt_q %>% group_by(genre_id, id, QtrYr) %>% summarize(movie_count = n()) %>% group_by(genre_id, QtrYr) %>% summarize(d_movie_count = n())
pltTotalMovieCountOverTime <- plotPopularityOverTime(plt2, plt2$d_movie_count, sum, 1, plt2$QtrYr, "18", "18 (Drama) had more movies showing over last 2 yrs")

# Average Popularity 
pltAveragePopularityOverTime <- plotPopularityOverTime(plt, plt$popularity, mean, 1, plt$Year, "18", "12 (Adventure) has the highest average popularity")
#Qtrly
pltAveragePopularityOverTimeQtrly <- plotPopularityOverTime(plt_q, plt_q$popularity, mean, 1, plt_q$QtrYr, "12", "12 (Adventure) has the highest average popularity")
