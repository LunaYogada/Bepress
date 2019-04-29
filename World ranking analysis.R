library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(Hmisc)
library(fuzzyjoin)
library(stringdist)
library(stringr)
library(CausalImpact)
library(rccdates)
library(tseries)

# select from multiple packages, define dplyr select
select1<-dplyr::select

# read data
times<-read_csv("timesData11-19.csv")
subscription<-read_csv("subscription.csv")
years<-data.frame("year" = 2011:2019)

# create cross join for institution and year
sub<-subscription%>%
  dplyr::select(`Parent Account`)%>%
  distinct(`Parent Account`)

sub$fake <- 1
years$fake <- 1
my_cross_join<-full_join(sub, years, by = "fake")%>%
  select1(-fake)

#my_cross_join<-rename(my_cross_join, Parent_account = `Parent Account` )
#subscription<-rename(subscription,Parent_account = `Parent Account` )

# remove all the special characters
my_cross_join<-my_cross_join%>%
  mutate("name" = str_replace_all(string = `Parent Account`, pattern = "[^0-9a-zA-Z]+", replacement = ""))

subscription<-subscription%>%
  mutate("name" = str_replace_all(string = `Parent Account`, pattern = "[^0-9a-zA-Z]+", replacement = ""))

times<-times%>%
  mutate(name = str_replace_all(string = university_name, 
                                pattern = "[^0-9a-zA-Z]+", replacement = ""),
         total_score = parse_number(total_score))
  
# join dc institution with all year and times ranking
dc_insitution<-my_cross_join%>%
  left_join(subscription, by =c("name", "year"))%>%
  left_join(times, by = c("name", "year"))%>%
  dplyr::select( name, year, "world_rank", "total_score", "decision")

# select dc institution that has an increase in ranking as test group
Monash<-dc_insitution%>%
  filter(name == "MonashUniversity")%>%
  distinct(year,name,total_score)%>%
  spread(name, total_score)

# select non dc insitution as control group 
non_dc<-times%>%
  left_join(subscription)%>%
  filter(is.na(`Account ID`) & total_score!="-")%>%
  select1(year,name,total_score)%>%
  slice(1:1266, 1773:2189,2875:3230)%>%
  spread(name, total_score)%>%
  select_if( ~ !any(is.na(.)))

# combine two varialbes and create timeseries data  
my_data<-Monash%>%
  inner_join(non_dc)%>%
  select1(-year)%>%
  ts()%>%
  zoo(year(as.Date(c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"), format = "%Y")))

# set pre/post period
pre.period <- year(as.Date(c("2011", "2013"), format = "%Y"))
post.period <- year(as.Date(c("2014", "2019"), format = "%Y"))

# build impact model and assess impact
impact <- CausalImpact(my_data, pre.period, post.period)
plot(impact)






