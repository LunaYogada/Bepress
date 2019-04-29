library(jsonlite)
library(tidyverse)
library(stringr)

# retrieve data from times world ranking from 2017-2019
result <- fromJSON("https://www.timeshighereducation.com/sites/default/files/the_data_rankings/world_university_rankings_2019_limit0_7216a250f6ae72c71cd09563798a9f18.json")
times_2019<-result$data

result2018 <- fromJSON("https://www.timeshighereducation.com/sites/default/files/the_data_rankings/world_university_rankings_2018_limit0_369a9045a203e176392b9fb8f8c1cb2a.json")
times_2018<-result2018$data

result2017<-fromJSON("https://www.timeshighereducation.com/sites/default/files/the_data_rankings/world_university_rankings_2017_limit0_94aa8e595206a1cd1284e2808330a79c.json")
times_2017 <-result2017$data

times_2019<-times_2019%>%
  mutate(year = 2019)

times_2018<-times_2018%>%
  mutate(year = 2018)

times_2017<-times_2017%>%
  mutate(year = 2017)

# combine the table together
times17_19_raw<-rbind(times_2017, times_2018, times_2019)

# Clean data and manipulation. 

# times17_19_raw$scores_overall<-

times17_19_raw$scores_overall <- str_split(times17_19_raw$scores_overall,
                                           fixed("—")) %>%
  map_dbl(~mean(parse_number(.x)))

parse_table<-function(str){
  library(readr)
  
  # don't parse ratio
  if(mean(str_detect(string = str, pattern = ".+:.+") ,na.rm = T)> 0.9){
    return(str)
  }
  # parse number with ,
  else if(mean(str_detect(string = str, pattern = ".+,.+") , na.rm = T)> 0.9){
    return(parse_number(str))
  }
  
  # parse "n%"
  else if(mean(str_detect(string = str, pattern = ".+%$"), na.rm = T)> 0.9){
    return(parse_number(ifelse(str_detect(string = str, pattern = "^%.*"), 
                               str_replace(string = str, pattern = "^%.*", replacement = "0%"),
                               str)))
  } 
  
  # parse any other number
  else if(mean(str_detect(string = str, pattern = "[0-9]+.+"), na.rm =  T)>0.9){
    return(as.numeric(str))
  }
  
  else{
    return(str)
  }
}


times17_19<-times17_19_raw%>%
  select(rank_order, name, location, scores_teaching, scores_international_outlook, scores_research, scores_citations, scores_industry_income,
         scores_overall, stats_number_students, stats_student_staff_ratio, stats_pc_intl_students, stats_female_male_ratio, year
         )%>%
  mutate_all(parse_table)%>%
  mutate(rank_order = rank_order/10)

colnames(times17_19) <- c('world_rank',	'university_name', 'country',	'teaching' ,	'international', 	'research', 'citations',	'income',	'total_score',	'num_students',	'student_staff_ratio',	'international_students',	'female_male_ratio',	'year')

# import data from times2011-2016

times11_16<-read_csv('timesData11-16.csv')

times11_19<-as.data.frame(rbind(times11_16, times17_19))

# write to csv
write_csv(times11_19, "timesData11-19.csv")
