library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(Hmisc)
library(fuzzyjoin)
library(stringdist)
library(stringr)

#load data-----
renewal_data<-read_csv("renewal 2009.csv")
cancellation_data<-read_csv("cancellation 2009.csv")
institution<-read_csv("institution.csv")
website<-read_csv("dc_site_list.csv")


# Parse data-----

parse_table<-function(str){
  library(readr)
  # parse date
  if(all(str_detect(string = str, pattern = "[0-9]+/[0-9]+/[0-9]{2}"),na.rm = T)){
    return(mdy(str))
    }
    else if(all(str_detect(string = str, pattern = "[0-9]{4}/[0-9]+/[0-9]+"),na.rm = T)){
      return(ymd(str))
    }
    else{
      return(str)
    }
    #else if(all(str_detect(string = str, pattern = ""))){}
}

renewal_data<-
  renewal_data%>%mutate_all(parse_table)

cancellation_data<-
  cancellation_data%>%mutate_all(parse_table)


#time series plot-----
cancellation_dt<-
  cancellation_data%>%
  filter(`Asset Name` == "Digital Commons")%>%
  mutate(cancel_year = year(`Cancellation Date`),
         cancel_month = month(`Cancellation Date`)
         )%>%
  select(`Cancellation Date`, `Account Name`,cancel_year, cancel_month)%>%
  group_by(`Cancellation Date`)%>%
  mutate(daily_cancel = n())%>%
  ungroup()%>%
  group_by(cancel_year, cancel_month)%>%
  mutate(monthly_cancel = n())%>%
  ungroup()%>%
  group_by(cancel_year)%>%
  mutate(yearly_cancel = n())%>%
  ungroup()%>%
  group_by(cancel_year)%>%
  mutate(decision = 0)%>%
  arrange(`Cancellation Date`)

ggplot(data = cancellation_dt, aes(x = cancel_year, y = yearly_cancel)) + 
  geom_col()+ xlab("year") + ylab("yearly Cancellation")

plot(x = cancellation_dt$cancel_year, y = cancellation$yearly_cancel)

ggplot(data = cancellation_dt, aes(x = cancel_month, y = monthly_cancel))+
  geom_col(aes(fill = cancel_month)) + facet_grid(cancel_year ~ .) 


renewal_dt<-
  renewal_data%>%
  filter(`Asset Name` == "Digital Commons")%>%
  mutate(renewal_year = year(`Renewal Date`),
         renewal_month = month(`Renewal Date`)
  )%>%
  select(`Renewal Date`, `Account Name`,renewal_year, renewal_month)%>%
  group_by(`Renewal Date`)%>%
  mutate(daily_renew = n())%>%
  ungroup()%>%
  group_by(renewal_year, renewal_month)%>%
  mutate(monthly_renew = n())%>%
  ungroup()%>%
  group_by(renewal_year)%>%
  mutate(yearly_renew = n())%>%
  ungroup()%>%
  group_by(renewal_year)%>%
  mutate(decision = 1)%>%
  arrange(`Renewal Date`)

ggplot(data = renewal_dt, aes(x = `Renewal Date`, y = daily_renew)) + 
  geom_line()+ scale_x_date(date_labels  = "%Y-%m-%d")+ xlab("year") + ylab("Daily Renewal")

ggplot(data = renewal_dt, aes(x = renewal_month, y = monthly_renew))+
  geom_col() + facet_grid(renewal_year ~ .) 
#  +theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = renewal_dt, aes(x = paste(renewal_year, renewal_month, sep = "/"), y = monthly_renew))+
  geom_col() +theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("month")


# Fill NA parent acount with account name----
cancellation_data <-
  cancellation_data %>%
  mutate(`Parent Account` = str_replace(`Parent Account`, "(.+System$)|(.+Consortium$)", ""),
         `Parent Account` = ifelse(`Parent Account` =="", NA, `Parent Account`))


cancellation_data$`Parent Account`[is.na(cancellation_data$`Parent Account`)] <- 
  cancellation_data$`Account Name`[is.na(cancellation_data$`Parent Account`)]

renewal_data <-
  renewal_data %>%
  mutate(`Parent Account` = str_replace(`Parent Account`, "(.+System$)|(.+Consortium)", ""),
         `Parent Account` = ifelse(`Parent Account` =="", NA, `Parent Account`))

renewal_data$`Parent Account`[is.na(renewal_data$`Parent Account`)]<-
  renewal_data$`Account Name`[is.na(renewal_data$`Parent Account`)]

# Fill NA Purchase date with Usage start date----
cancellation_data$`Purchase Date`[is.na(cancellation_data$`Purchase Date`)] <- 
  cancellation_data$`Usage Start Date`[is.na(cancellation_data$`Purchase Date`)]

renewal_data$`Purchase Date`[is.na(renewal_data$`Purchase Date`)] <- 
  renewal_data$`Usage Start Date`[is.na(renewal_data$`Purchase Date`)]

#calculate usage days and onboarding days----

# cancellation

cancellation<-
  cancellation_data%>%
  filter(`Asset Name` == "Digital Commons",
         `Cancellation Date` > "2011-01-01")%>%
  mutate(year = year(`Cancellation Date`),
         month = month(`Cancellation Date`),
         day = day(`Cancellation Date`),
         wday = wday(`Cancellation Date`),
         onboarding_days =  `Usage Start Date` - `Purchase Date`,
         usage_days = `Cancellation Date`- `Usage Start Date`
  )%>%
  arrange(`Account ID`, `Usage Start Date`)%>%
  select(`Account ID`,`Account Name`, `Parent Account`, `Carnegie #`, `Billing Country`, `Employees`, `Industry`,`Order Type`,          
         `year`, `month`, `day`, `wday`, `Usage Start Date`, onboarding_days, usage_days ,`Price`)%>%
  group_by(year, month)%>%
  mutate(decision = 0)

 # remove duplicate records and only keep the earilest record(usage start date)
cancellation<-cancellation[!duplicated(cancellation[,c('Account ID', 'Parent Account')]),]


# tmp<-
#   cancellation_data%>%
#   filter(`Asset Name` == "Digital Commons")%>%
#   group_by(`Account ID`)%>%
#   mutate(count = n())%>%
#   filter(count >1 )%>%
#   arrange(`Usage Start Date`)
# tmp[!duplicated(tmp[,c('Account ID', 'Parent Account')]),]%>%
#   glimpse()

# renewal

renewal<-
  renewal_data%>%
  filter(`Asset Name` == "Digital Commons")%>%
  arrange(`Account ID`, `Usage Start Date`)%>%
  mutate(year = year(`Renewal Date`),
         month = month(`Renewal Date`),
         day = day(`Renewal Date`),
         wday = wday(`Renewal Date`),
         onboarding_days =  `Usage Start Date` - `Purchase Date`,
         pre_renewal = `Renewal Date` - `Usage Start Date`
  )%>%
  group_by(`Account ID`)%>% 
  mutate(usage_days = cumsum(as.numeric(pre_renewal)))%>%
  ungroup()%>%
  select(`Account ID`,`Account Name`, `Parent Account`, `Carnegie #`, `Billing Country`, `Employees`, `Industry`,`Order Type`,          
          `year`, `month`, `day`, `wday`, `Usage Start Date`, onboarding_days, usage_days ,`Price`)%>%
  group_by(year, month)%>%
  mutate(decision = 1)






# Union join renewal and cancellation----
subscription<-as.data.frame(rbind(renewal, cancellation))
describe(subscription)


# Carnegie # 
# n  missing distinct 
# 2254      348       20  

# Employees 
# n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 1983      619      398        1     9076     8956      701     1404     2436     6000    13669    22005    26384 
# 
# lowest :     2    32    44    70    72, highest: 35707 37441 40549 44082 44337

# Order Type 
# n  missing distinct 
# 2591       11        3 

# write to csv
write_csv(subscription, "subscription.csv")

subscription%>%
  summary(decision)

# Mean   :0.9784  


# join institution info
## keep segments 
institution<-
  institution%>%
  select("NAME","BASIC2018", "IPUG2018", "IPGRAD2018", "UGPROFILE2018", "ENRPROFILE2018", "SIZESET2018", "CONTROL",  
         "OBEREG", "LOCALE", "LANDGRNT", "HBCU", "TRIBAL", "HSI", "MSI", "WOMENS", "FAITHFLAG","OTHSFFLAG")



sub_in<-subscription%>%
  stringdist_left_join(institution, by = c(`Parent Account` = 'NAME'))

write_csv(sub_in, "subscription_institution.csv")
describe(sub_in)


## join website
website <- website%>%
  filter(!is.na(`Institution Name`))

sub_in_web<-sub_in%>%
  stringdist_left_join(website, by = c("Account Name" = "Institution Name"), max_dist = 4, distance_col = "diff")
describe(sub_in_web)

# Digital Commons URL 
# n  missing distinct 
# 4051      372      428  



tmp<-sub_in%>%
  left_join(website, by = c("Account Name" = "Institution Name"))

describe(tmp)

# Digital Commons URL 
# n  missing distinct 
# 1689      933      336 

NAs <- sub_in_web%>%
  filter(is.na(diff))%>%
  distinct(`Account Name`)

# 165
write_csv(NAs, "NA institution.csv")


# create a function to calculate the distance between the different versions of same institution name
string_distance<-function(a, b){
  for(m in c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"))
    {
      print(paste('methods is',m," ", 'distance is', stringdist(a, b, method = m)))
    }
}

string_distance("City University of New York (CUNY)","City University of New York System (CUNY)")
# [1] "methods is osa   distance is 7"
# [1] "methods is lv   distance is 7"
# [1] "methods is dl   distance is 7"
# [1] "methods is hamming   distance is Inf"
# [1] "methods is lcs   distance is 7"
# [1] "methods is qgram   distance is 7"
# [1] "methods is cosine   distance is 0.0239277650896533"
# [1] "methods is jaccard   distance is 0.0952380952380952"
# [1] "methods is jw   distance is 0.0569105691056911"
# [1] "methods is soundex   distance is 0"

string_distance("University of Minnesota Morris Digital Well","University of Minnesota-Morris")
# [1] "methods is osa   distance is 14"
# [1] "methods is lv   distance is 14"
# [1] "methods is dl   distance is 14"
# [1] "methods is hamming   distance is Inf"
# [1] "methods is lcs   distance is 15"
# [1] "methods is qgram   distance is 15"
# [1] "methods is cosine   distance is 0.0928803653877152"
# [1] "methods is jaccard   distance is 0.263157894736842"
# [1] "methods is jw   distance is 0.119638242894057"
# [1] "methods is soundex   distance is 0"

string_distance("The College at Brockport: State University of New York","State University of New York College at Brockport")
# [1] "methods is osa   distance is 45"
# [1] "methods is lv   distance is 45"
# [1] "methods is dl   distance is 45"
# [1] "methods is hamming   distance is Inf"
# [1] "methods is lcs   distance is 47"
# [1] "methods is qgram   distance is 5"
# [1] "methods is cosine   distance is 0.0104493311023486"
# [1] "methods is jaccard   distance is 0.111111111111111"
# [1] "methods is jw   distance is 0.180524061476443"
# [1] "methods is soundex   distance is 1"

# use regex to join the column
subscription%>%
  mutate(regex = str_extract())


str_extract(string = "The College at Brockport: State University of New York", 
            pattern = "([a-zA-Z_]+?)\\sUniversity of\\s([a-zA-Z_\\s]+)")
str_extract(string = "State University of New York College at Brockport", 
            pattern = "([a-zA-Z_]+?)\\sUniversity of\\s([a-zA-Z_\\s]+)")


str_replace(string = "The College at Brockport: State University of New York", 
            pattern = "^/(of)(New York)$/" ,
            replacement = '$2')



