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
library(data.table)

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
dc_institution<-my_cross_join%>%
  left_join(subscription, by =c("name", "year"))%>%
  left_join(times, by = c("name", "year"))%>%
  dplyr::select( name, year, "world_rank", "total_score", "decision")

describe(dc_institution)

filled<-dc_institution%>%
  group_by(name)%>%
  filter(!all(is.na(world_rank)))%>%
  tidyr::fill(world_rank, .direction = c("down"))%>%
  tidyr::fill(world_rank, .direction = c("up"))%>%
  tidyr::fill(total_score, .direction = c("down"))%>%
  tidyr::fill(total_score, .direction = c("up"))
  
filled$decision[is.na(filled$decision)] <- 0

filled%>%
  filter(!year ==2019)%>%
  group_by(name, decision)%>%
  summarise(avg_score = mean(total_score))%>%
  mutate(diff_after := avg_score - shift(avg_score))%>%
  ungroup()%>%
 # View()
  filter(!is.na(diff_after))%>%
  mutate(increase = ifelse(diff_after > 0, 1, ifelse(diff_after == 0, 0, -1)))%>%
  group_by(increase)%>%
  summarise(count = n())
  
# # A tibble: 3 x 2
# increase count
# <dbl> <int>
# 1       -1    29
# 2        0    16
# 3        1    29

increase_ins<-filled%>%
  filter(!year ==2019)%>%
  group_by(name, decision)%>%
  summarise(avg_score = mean(total_score))%>%
  mutate(diff_after := avg_score - shift(avg_score))%>%
  ungroup()%>%
  # View()
  filter(!is.na(diff_after))%>%
  mutate(increase = ifelse(diff_after > 0, 1, ifelse(diff_after == 0, 0, -1)))%>%
  filter(increase == 1)%>%
  arrange(desc(diff_after))
  
  
# select dc institution that has an increase in ranking as test group
Monash<-dc_institution%>%
  filter(name == "MonashUniversity")%>%
  distinct(year,name,total_score)%>%
  spread(name, total_score)

# select non dc insitution as control group 
non_dc<-
  times%>%
  left_join(subscription)%>%
  filter(is.na(`Account ID`) & total_score!="-")%>%
  filter(!(university_name == "Northeastern University" & country == "China"))%>%
  select1(year,name,total_score)%>%
  spread(name, total_score)%>%
  select_if( ~ !any(is.na(.)))

# combine two varialbes and create timeseries data  
my_data<-Monash%>%
  inner_join(non_dc)%>%
  select1(-year)%>%
  ts()%>%
  zoo(year(as.Date(c("2011", "2012",  "2013", "2014", "2015", "2016", "2017", "2018", "2019"), format = "%Y")))

# set pre/post period
pre.period <- year(as.Date(c("2011", "2013"), format = "%Y"))
post.period <- year(as.Date(c("2014", "2019"), format = "%Y"))

# build impact model and assess impact
impact <- CausalImpact(my_data, pre.period, post.period)
plot(impact)






