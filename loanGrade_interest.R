# LendingClub is a US peer-to-peer lending company

library(tidyverse)
library(caTools)

setwd("C:/Users/user/OneDrive - Moe, Inc/Desktop/myproject/Loan-Grade-and-Interest-Rate")

# Get data
loan_data_2017=read.csv("loan_data_2017.csv")


# In LendingClub, loan grade, or credit grade, 
# is the letter (A-G or AA-HR) that is assigned 
# to a borrower and corresponds with
# the interest rate that is charged for the loan.

# default rates
countbased_default=loan_data_2017 %>%
  filter(loan_status=="Default") %>% 
  group_by(grade) %>% 
  summarise(default_count=n())


countbased_default


# A tibble: 4 x 2
# grade default_count
# <chr>         <int>
# 1 B               2
# 2 C               4
# 3 D               1
# 4 E               2

# deaultRate_group


rates=loan_data_2017 %>% 
  group_by(grade) %>%
  summarise(count=n())
  
# rates

# # A tibble: 8 x 2
# grade count
# <chr> <int>
#   1 ""    4
# 2 "A"   26482
# 3 "B"   40267
# 4 "C"   36777
# 5 "D"   16454
# 6 "E"    9540
# 7 "F"    3482
# 8 "G"     885



# Clear indication of percentage default in grade b c d e
# leftjoin rate to default_rate and find the percentage

rates
countbased_default

percentage_default=
  rates %>% left_join(countbased_default) %>%
  mutate(default_rate = 100*default_count/count) %>%
  select(grade,count,default_count,default_rate)
  by = "grade" 
 
  percentage_default
  # grade count default_count default_rate
  # <chr> <int>         <int>        <dbl>
  #   1 ""        4            NA     NA      
  # 2 "A"   26482            NA     NA      
  # 3 "B"   40267             2      0.00497
  # 4 "C"   36777             4      0.0109 
  # 5 "D"   16454             1      0.00608
  # 6 "E"    9540             2      0.0210 
  # 7 "F"    3482            NA     NA      
  # 8 "G"     885            NA     NA      
  
  
  # let plot grade to default_rate
ggplot(percentage_default, aes(x=grade, y=count, fill=default_rate)) + geom_bar(stat="identity")  
  
ggplot(percentage_default, aes(x=grade, y=default_rate, fill=grade)) + geom_bar(stat="identity") +ggtitle(" defaultRateby grade") 

# Cleaning attributes 
tail(loan_data_2017$int_rate)
 # "15.41%" "11.22%" ""       ""       ""       ""  


# removing % and ""
loan_data_2017$int_rate=as.numeric(gsub(pattern="%",replacement = "",x=loan_data_2017$int_rate))
  
tail(loan_data_2017$int_rate)
# [1] 15.41 11.22    NA    NA    NA    NA

# let find mean,max,and min interesrate
x1=loan_data_2017 %>% 
  filter(loan_status=="Default") %>% 
  group_by(grade) %>% 
  summarise(averageRate=mean(int_rate),
         MaxRate=max(int_rate),
         minRate=min(int_rate))
# A tibble: 4 x 4
# grade averageRate MaxRate minRate
# <chr>       <dbl>   <dbl>   <dbl>
# 1 B            11.1    11.5    10.8
# 2 C            12.9    13.7    12.0
# 3 D            17.3    17.3    17.3
# 4 E            21.7    22.0    21.5


# range of interest rates who defaulted 

range(x1$averageRate)
# 11.110 21.725
range(x1$MaxRate)
# 11.47 21.97

range(x1$minRate)

# 10.75 21.48

range(na.omit(loan_data_2017$int_rate))
# 5.32  to 28.99 % interest rate

# from this we know interest rate from 5.32 to 28.99 % 
# highest default's in range of 11.47 to 21.97 

# let plot default based on mean,max & min 

ggplot(x1, aes(x=grade, y=averageRate, fill=grade)) + geom_bar(stat="identity",position="dodge")
ggplot(x1, aes(x=grade, y=MaxRate, fill=grade)) + geom_bar(stat="identity",position="dodge")
ggplot(x1, aes(x=grade, y=minRate, fill=grade)) + geom_bar(stat="identity",position="dodge")


range(na.omit(loan_data_2017$int_rate))
# 5.32  to 28.99 % interest rate



