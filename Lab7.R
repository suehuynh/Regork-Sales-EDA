#set up
library(ggplot2)
library(dplyr)
library(completejourney)
library(lubridate)
library(scales)
library(zoo)
library(forcats)
library("gridExtra")

#get data
campaigns <- campaigns
campaign_descriptions <- campaign_descriptions
demographics <- demographics
transactions <- get_transactions()

#merge data
df <- campaigns %>% 
     left_join(campaign_descriptions, by = 'campaign_id') %>% 
     left_join(demographics, by='household_id') %>% 
     left_join(transactions, by ='household_id')

#add Is_Holiday feature
df <- df %>% mutate(IsHoliday = case_when(lubridate::date(transaction_timestamp) == '2017-02-05' |
                                          lubridate::date(transaction_timestamp) == '2017-09-04' |
                                          lubridate::date(transaction_timestamp) == '2017-11-23' |
                                          lubridate::date(transaction_timestamp) == '2017-12-24' ~ 1, 
                                          TRUE ~ 0))

#explore data

##outlier significant
boxplot(df$sales_value)
df[df$sales_value > 200,] %>% 
     group_by(IsHoliday) %>% 
     summarize(n_holiday = n())
##remove outlier
df <- df %>% 
     filter(sales_value <= 200)

df_week <- df %>% 
     group_by(week) %>% 
     summarise(weekly_sales = sum(sales_value),
               weekly_qty = sum(quantity),
               holiday_week = sum(IsHoliday))
df_week <-mutate(df_week, IsHolidayWeek = ifelse(holiday_week == 0, 0,1))
df_week %>% 
     group_by(IsHolidayWeek) %>% 
     summarize(avg_sales = mean(weekly_sales))
     #ggplot(aes(x=IsHolidayWeek,y=avg_sales)) +
     #geom_bar(stat="identity")

#holiday week does affect week sales on average

#what is the top sales by holiday? sales value on each holiday compared to that of non-holiday average
##campaign run features?

df_holiday <- df %>% 
               filter(IsHoliday == 1) %>% 
               group_by(lubridate::date(transaction_timestamp)) %>% 
               summarize(sales = sum(sales_value))
colnames(df_holiday)[1] <- "date"
avg_daily_sales<- df %>% 
                    filter(IsHoliday == 0) %>% 
                    group_by(lubridate::date(transaction_timestamp)) %>% 
                    summarize(daily_sales = sum(sales_value)) %>% 
                    summarise_if(is.numeric, mean)

new_row <- c('2017-01-01',avg_daily_sales)
new_row <- as.data.frame(new_row)
colnames(new_row) <- c("date","sales")
new_row <- transform(new_row, date = as.Date(date))
df_holiday <- bind_rows(df_holiday,new_row)

#correlation between number of campaigns a week and the weekly sales
#for holidays specifically

plot_3 <- df_week %>% 
     mutate(IsHolidayWeek = case_when(IsHolidayWeek==0 ~ 'Non-Holidays',
                                      IsHolidayWeek==1 ~ 'Holidays')) %>% 
     group_by(IsHolidayWeek) %>% 
     summarize(avg_sales = mean(weekly_sales)) %>% 
     ggplot(aes(x=IsHolidayWeek, 
                y=avg_sales,
                fill=factor(ifelse(IsHolidayWeek=="Non-Holidays","Highlighted","Normal")))) +
     geom_bar(stat="identity",show.legend = FALSE) +
     scale_fill_manual(name = "typeofdays", values=c("#89CFF0","#002D62")) +
     labs(title = "Week Sales on holidays and non-holidays",
          subtitle = "Disclaimer: Weekly sales for non-holidays measured by average",
          x = "Type",
          y = "Total Sales ($)") +
     scale_y_continuous(labels=comma) +
     theme(plot.title = element_text(face = "bold"),
           plot.subtitle = element_text(face = "italic"),
           panel.background = element_rect(fill = "white"),
           panel.grid.major = element_line(color = "lightgray"),
           axis.title.x=element_blank())
plot_4 <- df_week %>% group_by(weektype) %>% 
     summarize(avg_sales = mean(weekly_sales)) %>% 
     ggplot(aes(x=reorder(weektype,-avg_sales), 
                y=avg_sales,
                fill=factor(ifelse(weektype=="Non-Holidays","Highlighted","Normal")))) +
     geom_bar(stat="identity",show.legend = FALSE) +
     scale_fill_manual(name = "typeofdays", values=c("#89CFF0","#002D62")) +
     labs(title = "Week Sales on holidays and non-holidays",
          subtitle = "Disclaimer: Weekly sales for non-holidays measured by average",
          x = "Type",
          y = "Total Sales ($)") +
     scale_y_continuous(labels=comma) +
     theme(plot.title = element_text(face = "bold"),
           plot.subtitle = element_text(face = "italic"),
           panel.background = element_rect(fill = "white"),
           panel.grid.major = element_line(color = "lightgray"),
           axis.title.x=element_blank())
plot_5 <-  grid.arrange(plot_3,plot_4,nrow=2)
`
df_week <- df %>% 
     group_by(week) %>% 
     summarize(weekly_sales = sum(sales_value),
               weekly_qty = sum(quantity),
               holiday_week = sum(IsHoliday)) 
df_week$weektype <- as.factor(ifelse(week==6, "Super Bowl", 
                       ifelse(week==37, "Labor Day",
                       ifelse(week==48, "Thanksgiving",
                       ifelse(week==52, "Christmas","Non-Holidays"))))) 
