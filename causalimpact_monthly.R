library(tidyverse)
library(lubridate)
library(Hmisc)
library(fuzzyjoin)
library(stringdist)
library(CausalImpact)
library(glue)
library(zoo)
library(scales)
library(RColorBrewer)
library(GenSA)


select1<-dplyr::select

#load data-----
uploads<-read_csv("uploads_for_causual.csv")
downloads_m<-read_csv("monthly_downloads_2002.csv")
downloads<-read_csv("downloads_for_causual.csv")

min(downloads_m$date)
# "2019-01-05"

# calculate cummulative uploads---- 

cum_m_uploads <-uploads %>%
  group_by(site_key)%>%
  mutate(years = year(date),
         months = month(date))%>%
  ungroup()%>%
  group_by(site_key, years, months)%>%
  summarise(ttl_uploads = sum(uploads))%>%
  ungroup()%>%
  group_by(site_key)%>%
  mutate(cum_uploads = cumsum(ttl_uploads))%>%
  ungroup()
  
  
# join cummulative upload with download----  
content<-downloads_m%>%
  full_join(cum_m_uploads, by = c("site_key", "years", "months"))

content$ttl_uploads[is.na(content$ttl_uploads)] <- 0

# map downloads with total uploads on that date-----
content_repo<-content%>%  
  group_by(site_key)%>%
  mutate(cum_uploads = ifelse(is.na(cum_uploads), lead(cum_uploads) - lead(ttl_uploads), cum_uploads))%>%
  tidyr::fill(cum_uploads, .direction = c("up"))%>%
  mutate(cum_uploads = ifelse(is.na(cum_uploads), lag(cum_uploads)+ ttl_uploads, cum_uploads))%>%
  tidyr::fill(cum_uploads, .direction = c("down"))%>%
  ungroup()%>%
  filter(!site_key %in% c(2,5,8, 1794,2730))


describe(content_repo)

# ---------------------------------------------------------------------------------------------------------------------------------------
#  downloads 
# n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 40300      827    21249        1    20136    29547       46      178     1068     5240    20648    51731    87637 
# 
# lowest :      0      1      2      3      4, highest: 683342 684201 730888 768666 783331
# -------------------------------------------------------------------------------------------------------------------------
#   ttl_uploads 
# n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 41127        0     1712    0.996    185.4    317.6        0        0        3       24      103      294      532 
# 
# lowest :      0      1      2      3      4, highest:  75654  81458  89479  90640 101245
# -------------------------------------------------------------------------------------------------------------------------
#   cum_uploads 
# n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 39891     1236     4828        1     1275     2129        0        5       42      218      773     2062     3576 
# 
# lowest :      0      1      2      3      4, highest: 341087 376227 387021 391966 396066
# ---------------------------------------------------------------------------------------------------------------------------------------


# Quantile segments---
uploads_2019<-
  uploads%>%
  filter(site_key != 2)%>%
  group_by(site_key)%>%
  mutate(ttl_uploads = cumsum(uploads))%>%
  filter(date >= "2019-01-05")

content_2019<-downloads%>%
  left_join(uploads_2019, by = c("site_key", "date"))

content_2019$uploads[is.na(content_2019$uploads)] <- 0

# map downloads with total uploads on that date
content_repo_2019<-content_2019%>%  
  group_by(site_key)%>%
  mutate(ttl_uploads = ifelse(is.na(ttl_uploads), lead(ttl_uploads) - lead(uploads), ttl_uploads))%>%
  tidyr::fill(ttl_uploads, .direction = c("up"))%>%
  mutate(ttl_uploads = ifelse(is.na(ttl_uploads), lag(ttl_uploads)+ uploads, ttl_uploads))%>%
  tidyr::fill(ttl_uploads, .direction = c("down"))%>%
  ungroup()

tmp<-content_repo_2019%>%
  filter(date == "2019-01-05")

quantiles_2019 <- quantile(tmp$ttl_uploads, na.rm = T )
quantiles_2019
repo_quantile <- content_repo_2019 %>%
  filter(!is.na(ttl_uploads))%>%
  mutate(repo_group =case_when(ttl_uploads <= quantiles_2019[2] ~ "low",
                               ttl_uploads >= quantiles_2019[4] ~ "high",
                               T ~ "medium"
  ))
group<-repo_quantile%>%
  distinct(site_key, repo_group)%>%
  ungroup()

##plot histogram----

content_repo%>%
  ggplot()+
  geom_histogram( aes(x = downloads), bins = 30, fill = "orange" ) +
  theme_bw()+
  ggtitle("Downloads Histogram Diagram")
  
content_repo%>%
  ggplot()+
  geom_histogram(aes(x = ttl_uploads), bins = 30, fill = "green")+
  theme_bw()+
  ggtitle("Monthly Uploads Histogram Diagram")+
  xlab("Monthly Uploads")

content_repo%>%
  ggplot()+
  geom_histogram(aes(x = cum_uploads), bins = 30)+
  theme_bw()+
  ggtitle("Total Mumber of Content Histogram Diagram")+
  xlab("Total Contents")

#Plot cum_uploads line chart by group----
content_repo%>%
  group_by(site_key)%>%
  arrange(site_key, years, months)%>%
  filter(!is.na(years))%>%
  mutate(month_since_subscription = row_number())%>%
  mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
  left_join(group, by = c("site_key"))%>%
  ungroup()%>%
  filter(
    #month_since_subscription <= 190 & 
      !is.na(repo_group))%>%
  group_by(repo_group, date)%>%
  summarise(average = mean(cum_uploads, na.rm = T))%>%
  ggplot(aes(x = date, y= average, color = repo_group))+
  geom_line(size = 1.5, alpha = 0.8)+
  theme_bw() +
  labs(x = "Years since subscription", y = "Total content in repository", 
       title = "Total content increase since subscription by group")+
  scale_y_continuous(labels = comma)+
  scale_color_manual(values=c("medium" = "#3366FF", "high"="light green", "low"= "red"))+
  scale_x_date(
    breaks = seq(min(date), max(date), by = "5 year"),
    labels = c("0 year", "5 years", "10 years", "15 years"))
  


# Create a function to plot total uploads and downloads
plot_updown<-function(database, sitekey){
  database%>%
    filter(site_key ==sitekey)%>%
    select1(`ttl_uploads`, `downloads`)%>%
    ggplot(aes(x = ttl_uploads, y = downloads))+
    geom_point()+
    geom_smooth(method = "lm")
}

for(i in sites$site_key){
  print(i)
  ggsave(paste0( "plots/up_down",i, "up_down.jpg"), plot = plot_updown(content_repo, i))
}


# boxplot----
up_down<-up_down%>%
  mutate(date=date(glue("{years}-{months}-01")))

g= up_down %>%
  gather(key = "type",value = "count",downloads,uploads) %>%
  filter(date > "2016-01-01") %>%
  ggplot() +
  geom_boxplot(aes(x=date, y=count, group=date)) +
  facet_grid(type~.,scales = "free_y")
g

repo_quantile%>%
  filter(repo_group == "low" )%>%
  ggplot()+
  geom_boxplot(aes(x = date, y = downloads, group = date))

# plot downloads of different group----
segment_repo<- function(group){
  repo_quantile %>%
    ungroup()%>%
    filter(repo_group == group) %>%
    group_by(date) %>%
    summarise(average= mean(downloads)) %>%
    ungroup()
}


low_downloads<-segment_repo("low")
medium_downloads<-segment_repo("medium")
high_downloads<-segment_repo("high")


x = high_downloads$date
y1 = low_downloads$average
y2 = medium_downloads$average
y3 = high_downloads$average
  
# First curve is plotted
plot(x,y1, type="o", col="red", pch="o", lty=1, ylim=c(0,3000), ylab = "downloads", xlab = "date",
     main = "Average Daily Downloads in 2019 Jan - Apr by Group")
axis(at = c(as.Date("2019-01-05")), labels = c("Jan 05"),side = 1)

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(x, y2, col="blue", pch="*")
lines(x, y2, col="blue",lty=2)

points(x, y3, col="green", pch="+")
lines(x, y3, col="green",lty=3)

legend("topleft", legend=c("low","medium","high"), col=c("red","blue","green"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1)

mean(y3)/mean(y1) # 28.59818


# of total files over times---------

uploads%>%
  arrange(desc(date))

# create dataframe for all sites with full date
date = seq(as.Date("2002/5/14"), as.Date("2019/4/5"), by = 1)
dates<-as.data.frame(date)
dates$dummy = 1
sites$dummy = 1
date_site<-sites%>%
  full_join(dates, by = c("dummy"))%>%
  select1(-dummy)


full_uploads <-date_site%>%
  left_join(uploads, by = c("site_key", "date"))
full_uploads$uploads[is.na(full_uploads$uploads)] <- 0

full_uploads<-full_uploads%>%  
  group_by(site_key)%>%
  mutate(ttl_uploads = cumsum(uploads))%>%
  mutate(ttl_uploads = ifelse(is.na(ttl_uploads), lead(ttl_uploads) - lead(uploads), ttl_uploads))%>%
  tidyr::fill(ttl_uploads, .direction = c("up"))%>%
  mutate(ttl_uploads = ifelse(is.na(ttl_uploads), lag(ttl_uploads)+ uploads, ttl_uploads))%>%
  tidyr::fill(ttl_uploads, .direction = c("down"))%>%
  ungroup()%>%
  filter(site_key !=2)%>%
  inner_join(group, by = c("site_key"))

full_uploads%>%
  filter(date == "2019-01-05")%>%
  ggplot(aes(x = ttl_uploads, fill = repo_group))+
  geom_density(alpha = .6) + theme(legend.position = "right")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw()+
  scale_fill_manual(values=c("medium" = "#3366FF", "high"="green", "low"= "red"))+
  ggtitle("Distribution of groups for total contents ")+
  xlab("total contents")

full_uploads%>%
  group_by(repo_group, date)%>%
  summarise(average= mean(ttl_uploads, na.rm = T))%>%
  ggplot(aes(x = date, y = average, color = repo_group))+
  geom_line(size = 2, alpha = 0.8)+
  theme_bw()+
  labs(x = "Date", y = "Average total contents per day", 
       title = "Average total contents in year 2002-2019 by group")+
  scale_color_manual(values=c("medium" = "#3366FF", "high"="light green", "low"= "red"))
  
              

low_uploads<-full_uploads%>%
  filter(repo_group == "low")%>%
  group_by(date)%>%
  summarise(average= mean(ttl_uploads, na.rm = T),
            counts = n())%>%
  ungroup()

medium_uploads<-full_uploads%>%
  filter(repo_group == "medium")%>%
  group_by(date)%>%
  summarise(average= mean(ttl_uploads, na.rm = T),
            counts = n())%>%
  ungroup()

high_uploads<-full_uploads%>%
  filter(repo_group == "high")%>%
  group_by(date)%>%
  summarise(average= mean(ttl_uploads, na.rm = T),
            counts = n())%>%
  ungroup()


plot(high_uploads$date, high_uploads$average, type="o", col="dark green", pch="*", lty=1, 
     ylim = c(0,50000), xlab = "Date", ylab = "Accumulated Uploads",
     main = "Average Total contents per day from year 2002 to 2019 by group")
axis(at = c(as.Date("2002-06-01"),as.Date("2019-01-01")), 
     labels = c(2002, 2019),side = 1)

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(medium_uploads$date, medium_uploads$average, col="blue", pch="*")
lines(medium_uploads$date, medium_uploads$average, col="blue",lty=2)

points(low_uploads$date, low_uploads$average, col="red", pch="*")
lines(low_uploads$date, low_uploads$average, col="red",lty=3)

legend("topleft", legend=c("low","medium", "high"), col=c("red","blue","green"),
       pch=c("*","*","*"),lty=c(1,2,3), ncol=1)

# calculate ratio of download/total files per institution in each group----

downloads%>%
  left_join(group, by = c("site_key"))%>%
#  filter(date == "2019-04-05")%>%
  group_by(repo_group)%>%
  summarise(downloads = min(downloads),
            counts = n())

# A tibble: 4 x 4
# Groups:   date [1]
# date       repo_group ttl_downloads counts
# <date>     <chr>              <dbl>  <int>
#   1 2019-01-05 NA                 602     21
# 2 2019-01-05 high              178614    121
# 3 2019-01-05 low                 5406    117
# 4 2019-01-05 medium             93924    238

full_uploads%>%
  #filter(date == "2019-01-05")%>%
  group_by(repo_group, date)%>%
  summarise(average= mean(ttl_uploads, na.rm = T),
            counts = n())

# A tibble: 3 x 4
# Groups:   date [1]
# date       repo_group average counts
# <date>     <chr>        <dbl>  <int>
# 1 2019-01-05 high       47999.   121
# 2 2019-01-05 low        620     129
# 3 2019-01-05 medium     5447.   242

# high group
178614/47999 #3.721203
# low group
5406/620  #8.719355
# medium group
93924/5447  #17.24

#
# rate of high/low
3.721203/8.719355  #0.426775
#rate of medium/low
17.24325/8.719355  # 1.97

# rate of content medium/low
5447/620  # 8.785484

93924/5406 # 17.37403

# high/low
178614/5406

downloads_m%>%
  left_join(group, by = c("site_key"))%>%
  filter(!site_key %in% c(2,5,8, 1794,2730))%>%
  group_by(site_key)%>%
  mutate(ttl_downloads = cumsum(downloads))%>%
  filter(years == 2019 & months == 1)%>%
  group_by(repo_group, years, months)%>%
  summarise(average = mean(ttl_downloads),
            counts = n())

# # A tibble: 4 x 5
# # Groups:   repo_group, years [4]
# repo_group years months  average counts
# <chr>      <dbl>  <dbl>    <dbl>  <int>
# 1 NA          2019      1   65607.     29
# 2 high        2019      1 4014606.    121
# 3 low         2019      1   84330.    124
# 4 medium      2019      1 1051563.    242

# downloads/uploads high
4014606/47999  #83
# downloads/uploads low
84330/620   #136
#downloads/upload medium
1051563/5447 # 193

#rate of ratio
83/136 #0.61

193/136 # 1.419


plot_all<-function(key){
  content_repo%>%
    left_join(group)%>%
    mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
    filter(site_key == key)%>%
    ggplot()+
    geom_line(aes(x = date, y = cum_uploads))+
    geom_line(aes(x = date, y = ttl_uploads), color = "green")+
    geom_line(aes(x = date, y = downloads), color = "blue")+
    ylab("Counts")+
    theme(legend.position="right")
}

plot_all(7245055)
plot_beauty( 2435816)

content_repo%>%
  filter(site_key == 2064690)
  filter(years == 2014 & months == 05)

content_repo%>%
  filter(site_key == 3338972)%>%
  mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
  filter(date <= "2019-02-01" & date >= "2013-02-01")%>%
  group_by(site_key)%>%
  summarise(ttl = sum(downloads)) #  56843 19513

plot_beauty<-function(key){
  data <- content_repo%>%
    left_join(group)%>%
    mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
    filter(site_key == key)%>%
    filter(date <= "2018-05-01" & date >= "2012-03-01")%>%
    gather(key = "metrics", value = "value", 4,5,6)
  data%>%
    ggplot()+
    geom_line(aes(x = date, y = value, color = metrics ))+
    ylab("Counts")+
#    ylim(0, 15000)+
   scale_x_date(
                breaks = seq(min(data$date), max(data$date), by = "2 years"),
                labels = c("0 year", "2 years", "4 years", "6 years"))+

    theme(legend.position="right")+
    theme_bw()+
   # ggtitle("Total content, monthly download and monthly upload in year 2014 - 2018")+
    scale_color_manual(values=c("cum_uploads"="black","downloads"= "blue", "ttl_uploads" = "green"),
                       name = "Legend", labels = c("Total contents", "Downloads", "Uploads"))
}

content_repo<-content_repo%>%
  left_join(group, by = c("site_key"))

for(i in sites$site_key){
  gp <- content_repo%>%
    filter(site_key == i)
  if(all(gp$repo_group == "low", na.rm = T)){
    print(paste0(i, "low"))
    ggsave(paste0( "plots/low/",i, "total_content_vs_download.jpg"), plot = plot_all(i))
  }
    else if(all(gp$repo_group == "medium", na.rm = T)){  
    print(paste0(i, "medium"))
    ggsave(paste0( "plots/medium/",i, "total_content_vs_download.jpg"), plot = plot_all(i))
    }
    else{
    print(paste0(i, "high"))
    ggsave(paste0( "plots/high/",i, "total_content_vs_download.jpg"), plot = plot_all(i))
    }
}


content_repo%>%
  left_join(group)%>%
  mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
  filter(site_key == 6297493)%>%
  ggplot()+
  geom_line(aes(x = date, y = cum_uploads))+
  geom_line(aes(x = date, y = ttl_uploads), color = "green")+
  geom_line(aes(x = date, y = downloads), color = "blue")+
  ylab("counts")+
  theme(
    legend.justification = c("left", "top")
  )



no_uploads<-content_repo%>%
  filter(is.na(ttl_uploads ))%>%
  distinct(site_key)


content_repo%>%
  arrange(ttl_uploads)%>%
  distinct(site_key)

# causual impact analysis-----

## High group
# select two institutions: 3042133 as control group, 3000565 as test group
ctrl<-content_repo%>%
  filter(site_key == 3042133) %>%
  mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
  select1(date, downloads)

max(ctrl$date) # 2019-04-01

test<-content_repo%>%
  filter(site_key == 3000565)%>%
  mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
  select1(date, downloads)%>%
  filter(date > "2005-02-01")

max(test$date) #2019-03-01

# combine test group and control group and convert to time series
time.points <-seq.Date(as.Date("2005-03-01"), by = "month", length.out = 169)
test_ctrl<-test%>%
  inner_join(ctrl, by = c("date"))%>%
  select1(-date)%>%
  plyr::rename(replace = c("downloads.x" = "test", "downloads.y" = "control" ))%>%
  ts()%>%
  zoo(time.points)
  
head(test_ctrl)

# set pre/post period

content_repo%>%
  filter(site_key == 3000565) %>%
  filter(ttl_uploads > 10000)

# years months downloads ttl_uploads cum_uploads
# <dbl>  <dbl>     <dbl>       <dbl>       <dbl>
# 2013     12     17696       17422       20443

pre.period <- as.Date(c("2005-03-01", "2013-12-01"))
post.period <- as.Date(c("2014-01-01", "2019-03-01"))

# build impact model and assess impact
impact <- CausalImpact(test_ctrl, pre.period, post.period)
plot(impact)


## low group
# select two institutions: 3042133 as control group, 5071139 as test group
plot_beauty(5071139)

test_l<-content_repo%>%
  filter(site_key == 5071139)%>%
  mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
  select1(date, downloads)

min(test_l$date) # "2014-05-01"
max(test_l$date) #2018-10-01

ctrl_l<-ctrl%>%
  filter(date >= "2014-05-01" & date <= "2018-10-01")

# combine test group and control group and convert to time series
time.points <-seq.Date(as.Date("2014-05-01"), by = "month", length.out = 54)
test_ctrl_l<-test_l%>%
  inner_join(ctrl_l, by = c("date"))%>%
  select1(-date)%>%
  plyr::rename(replace = c("downloads.x" = "test", "downloads.y" = "control" ))%>%
  ts()%>%
  zoo(time.points)

head(test_ctrl_l)

# set pre/post period

content_repo%>%
  filter(site_key == 5071139) %>%
  filter(ttl_uploads > 3000)

# # A tibble: 3 x 6
# site_key years months downloads ttl_uploads cum_uploads
# <dbl> <dbl>  <dbl>     <dbl>       <dbl>       <dbl>
# 1  5071139  2016      6     18144        3308        8661
# 2  5071139  2016      7     19213        3221       11882
# 3  5071139  2018      2     24774        3544       17057

pre.period <- as.Date(c("2014-05-01", "2016-06-01"))
post.period <- as.Date(c("2016-07-01", "2018-10-01"))

# build impact model and assess impact
impact <- CausalImpact(test_ctrl_l, pre.period, post.period)
plot(impact)


## Medium
# select two institutions: 3042133 as control group, 3247467 as test group

test_m<-content_repo%>%
  filter(site_key == 3247467)%>%
  mutate(date = as.Date(paste(years, months, "01", sep = "-")))%>%
  select1(date, downloads)

min(test_m$date) # "2012-09-01"
max(test_m$date) #2019-04-01

ctrl_m<-ctrl%>%
  filter(date >= "2012-09-01" & date <= "2019-04-01")

# combine test group and control group and convert to time series
time.points <-seq.Date(as.Date("2012-09-01"), by = "month", length.out = 79)
test_ctrl_m<-test_m%>%
  inner_join(ctrl_m, by = c("date"))%>%
  select1(-date)%>%
  plyr::rename(replace = c("downloads.x" = "test", "downloads.y" = "control" ))%>%
  ts()%>%
  zoo(time.points)

head(test_ctrl_m)
is.na(test_ctrl_m)
# set pre/post period

content_repo%>%
  filter(site_key == 3247467) %>%
  filter(ttl_uploads > 5000)

# # A tibble: 3 x 6
# site_key years months downloads ttl_uploads cum_uploads
# <dbl> <dbl>  <dbl>     <dbl>       <dbl>       <dbl>
#3247467  2017      2      6702        6002        8922

pre.period <- as.Date(c("2012-09-01", "2017-02-01"))
post.period <- as.Date(c("2017-03-01", "2019-03-01"))

# build impact model and assess impact
impact <- CausalImpact(test_ctrl_m, pre.period, post.period)
plot(impact)


