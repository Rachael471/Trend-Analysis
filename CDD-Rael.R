rm(list = ls())
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)


dry_day_def <- 1 #dry day definition: less than 1mm of rainfall



#your data should be in 3 columns : year, month-date, rainfall amount. My 3 columns are called 'Year, Day, Rain'

dat_weather_Rain1 <- read.csv("C:/Users/user/Downloads/Old AIMS laptop/Documents/Documents/Nigeria/dat_weather_Rain1.csv")
dat_weather_Rain1 <- filter(dat_weather_Rain1,StationID=="330001")

#Desired date format
dat_weather_Rain1$Date <-as.Date(dat_weather_Rain1$Date)
 #format(as.Date(dat_weather_Rain1$Date), "%m - %d")
as.mmdd <- function(x, ...) UseMethod("as.mmdd")
as.mmdd.Date <- function(x, ...) structure(x, class = c("mmdd", "Date"))

as.Date.mmdd <- function(x, ...) structure(x, class = "Date")

#format date function

format.mmdd <- function(x, format = "%m-%d", ...) format(as.Date(x), format = format, ...)

dat_weather_Rain <- transform(dat_weather_Rain1, Day = as.mmdd(Date))


dat_weather_Rain<- dat_weather_Rain[c("year", "Day", "Rain")]
dat_weather_Rain <- dat_weather_Rain[complete.cases(dat_weather_Rain),]


#dt_bougouni <- dt_bougouni %>%
  #mutate(DD = ifelse(Rain < dry_day_def, 1, 0)) %>% #create a new column to check if the rainfall amount in a day makes it a dry day. 1 if it's a dry day, 0 if not
  #select(-Rain) #drop the column with rainfall amounts
#dat_weather <- select(c("year", "Day"))
dat_weather <- dat_weather_Rain %>%
  mutate(DD = ifelse(Rain < dry_day_def, 1, 0)) %>% #create a new column to check if the rainfall amount in a day makes it a dry day. 1 if it's a dry day, 0 if not
  select(-Rain) #drop the column with rainfall amounts

#dt2_bougouni <- dt_bougouni %>% spread(Day, DD) #changing the format of my data to make it easier to manipulate
dat2_weather <- dat_weather %>% spread(Day, DD) #changing the format of my data to make it easier to manipulate

#dd_bougouni <- dt2_bougouni%>% transmute(Year = Year) #creating a new dataframe to store the number of dry days
dat_weather <- dat2_weather%>% transmute(Year = Year) #creating a new dataframe to store the number of dry days

dd_bougouni$Total_DD = as.numeric(apply(dt2_bougouni[, 2:ncol(dt2_bougouni)],1,sum)) #total number of dry days

dd_bougouni$Jun_DD = as.numeric(apply(dt2_bougouni[, 2:31],1,sum))

dd_bougouni$Jul_DD = as.numeric(apply(dt2_bougouni[, 32:62],1,sum))

dd_bougouni$Aug_DD = as.numeric(apply(dt2_bougouni[, 63:93],1,sum))

dd_bougouni$Sep_DD = as.numeric(apply(dt2_bougouni[, 94:123],1,sum))

dd_bougouni$Mean_Monthly_DD = round(apply(dd_bougouni[, 3:ncol(dd_bougouni)],1,mean))



#Calculate cumulative dry days. Now this is the main task
df1_d = data.frame(col1 = dt_bougouni$Year, col2 = dt_bougouni$DD) #create a dataframe with the years and dry day columns from the original dataframe

years = quickr::tab_to_df(table(df1_d$col1),add_totals = F)$Variable #this line is to make the years distinct. If you don't have quickr installed, ask Steven for help

dat_merged = list() #the following for loop and the ones in-between are checking the dataframe for consecutive 1's and sums them. Sum restarts whenever it encounters a zero
for (y in 1:length(years)){
  df1 = df1_d %>%  filter(col1 == as.numeric(years[y]))
  df1_d[1,3] = NA 
  
  # the second row is known - extract it
  df1[2,3] = df1[2,2]
  
  
  for (i in 2:dim(df1)[1]){
    if(!is.na(df1[i+1,2])){
      if(df1[i,2] == 1 & df1[i+1,2] == 1){
        df1[i+1,3] = df1[i,3] + 1
      } else if((df1[i,2] == 1 & df1[i+1,2] == 0)){
        df1[i+1,3] = 0
      } else if((df1[i,2] == 0 & df1[i+1,2] == 0)){
        df1[i+1,3] = 0
      } else if((df1[i,2] == 0 & df1[i+1,2] == 1)){
        df1[i+1,3] = 1
      }
    }
    
    dat_merged[[y]] = df1
  }
  
}

data_merged = do.call(rbind,dat_merged) #merging all the lists from the different years

dt3_bougouni <- dt_bougouni %>%
  select(-DD) %>% #drop the colums with dry days (ones and zeros)
  mutate(CDD = data_merged$V3) %>% #add a new column of the cumulative dry days
  spread(Day, CDD) #spread data for easy manipulation


dd_bougouni$Max_CDD = as.numeric(apply(dt3_bougouni[, 3:ncol(dt3_bougouni)],1,max))

dd_bougouni$Jun_MaxCDD = as.numeric(apply(dt3_bougouni[, 3:31],1,max))

dd_bougouni$Jul_MaxCDD = as.numeric(apply(dt3_bougouni[, 32:62],1,max))

dd_bougouni$Aug_MaxCDD = as.numeric(apply(dt3_bougouni[, 63:93],1,max))

dd_bougouni$Sep_MaxCDD = as.numeric(apply(dt3_bougouni[, 94:123],1,max))

dd_bougouni$Mean_Monthly_CDD = round(apply(dd_bougouni[, 9:ncol(dd_bougouni)],1,mean))

mean_total_dd <-round(mean(dd_bougouni$Total_DD))

mean_cdd <-round(mean(dd_bougouni$Max_CDD))
max_cdd <- round(max(dd_bougouni$Max_CDD))

mean_bougouni <- tibble(col1=NA)
mean_bougouni <- mean_bougouni %>%
  transmute(mean_total_dd = mean_total_dd) %>%
  mutate(mean_cdd = mean_cdd) %>%
  mutate(max_cdd = max_cdd)

