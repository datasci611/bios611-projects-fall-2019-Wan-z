library('tidyverse') 
library(stringr)
library(ggplot2)
library(RColorBrewer)

#import data
df <- read_tsv('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/data/UMD_Services_Provided_20190719.tsv')

summary(df)
#Client File Number and Client File Merge: factor variables
#Bus Tickets (Number of), Diapers, School Kits and Hygiene Kits: large part is NA
#Food Provided for, Food Pounds and Clothing Items: need to tidy and analyze
#Financial Support: large part equal to 0

#split date into month, day, year
date <- str_split_fixed(df$Date, '/', 3)
df$month = as.numeric(date[,1])
df$day = as.numeric(date[,2])
df$year = as.numeric(date[,3])

#drop NA and 0 in Bus Tickets (Number of) and group it by year and month
df_bus <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('Bus Tickets (Number of)') %>%
  filter('Bus Tickets (Number of)' > 0) %>%
  group_by(year, month) %>%
  summarise(count = n(),
            number_bus_tickets = sum(`Bus Tickets (Number of)`)
  )
df_bus 

#myColors = c(brewer.pal(6,"YlOrRd"), brewer.pal(6,"Spectral")) # produces an array/list of colors
#names(myColors) = df_bus$month
ggplot(df_bus, aes(x=year, y=number_bus_tickets, fill = month)) +
  geom_col() +
  scale_fill_gradient2(low="blue", mid="white", high="red") +
  labs(title = 'Bus Tickets (Number of)',
       y = 'Number of bus tickets by month')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/bus_tickets.png', height=4, width=6)

ggplot(df_bus, aes(x=month, y=number_bus_tickets)) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(1:12)) +
  geom_point(color = 'darkblue') +
  labs(title = 'Bus Tickets (Number of) Varied in different month',
       y = 'Number of bus tickets by month')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/bus_tickets_smooth.png', height=4, width=6)

#It is shown that the bus tickets support was mainly provided in 2002 and 2003, and only provided once in June 2006 and August 2011 respectively, which implies that the service discontinued.

#diapers
df_diapers <- df %>%
  filter(year>1996 & year<2019 & Diapers<5000) %>%
  drop_na('Diapers') %>%
  group_by(year) %>%
  summarise(count = n(),
            diapers = sum(Diapers * 44)
  )
df_diapers

ggplot(df_diapers) +
  geom_line(mapping = aes(x = year, y = diapers)) +
  geom_point(mapping = aes(x = year, y = diapers),size = 2, alpha = 0.6, color='red') +
  labs(title = 'Diapers (average amount)',
       y = 'Average amount of diapers')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/diapers_ave.png', height=4, width=6)

#ggplot(df_diapers) +
  #geom_point(mapping = aes(x = year, y = count), size = 2, alpha = 0.6, color = 'orange') +
  #geom_line(mapping = aes(x = year, y = count)) +
  #labs(title = 'Diapers (times of service)',
  #     y = 'count')
#ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/diapers_count.png', height=4, width=6)

#schoolkits
df_schoolkits <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('School Kits') %>%
  filter(`School Kits` > 0) %>%  #we treat 0 as NA
  group_by(year, month) %>%
  summarise(count = n(),
            school_kits = sum(`School Kits`)
  )
 df_schoolkits

ggplot(df_schoolkits, mapping = aes(x = year, y = school_kits)) +
  geom_col(aes(fill=month)) +
  labs(title = 'School Kits',
       y = 'Annual Number of School Kits')

ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/schoolkits.png', height=4, width=6)

df_schoolkits.month <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('School Kits') %>%
  filter(`School Kits` > 0) %>%  #we treat 0 as NA
  group_by(month) %>%
  summarise(count = n(),
            school_kits = sum(`School Kits`)
  ) %>%
  arrange(desc(school_kits))
df_schoolkits.month

ggplot(df_schoolkits.month, mapping = aes(x = month, y= school_kits)) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(1:12)) +
  geom_point(color = 'darkblue') +
  labs(title = 'School Kits',
       y = 'Number of School Kits')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/schoolkits_month.png', height=4, width=6)

#hygienekits
df_hgkit <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('Hygiene Kits') %>%
  group_by(year) %>%
  summarise(count = n(),
            hgkit = sum(`Hygiene Kits`)
  )
df_hgkit

ggplot(df_hgkit) +
  geom_line(mapping = aes(x = year, y = hgkit)) +
  geom_point(mapping = aes(x = year, y = hgkit), size = 2, alpha = 0.6, color='darkred') +
  labs(title = 'Hygiene Kits (average amount)',
       y = 'Annual amount of hgyiene kits')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/hgkit.png', height=4, width=6)

#financial support
df_finance <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('Financial Support') %>%
  filter(`Financial Support` > 0) %>%
  group_by(year) %>%
  summarise(count = n(),
            finance = sum(`Financial Support`)
  )
df_finance

ggplot(df_finance) +
  geom_line(mapping = aes(x = year, y = finance)) +
  geom_point(mapping = aes(x = year, y = finance),size = 2, alpha = 0.6, color='green') +
  labs(title = 'Financial Support',
       y = 'Financial Support') 
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/finance.png', height=4, width=6)



#food and clothing
df_main <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na(`Food Provided for`, `Food Pounds`, `Clothing Items`) %>%
  filter(`Food Provided for`>0 & `Food Pounds`<450000) %>%
  select(year, month, day, `Food Provided for`, `Food Pounds`, `Clothing Items`) %>%
  mutate(ave_food_pounds = `Food Pounds`/`Food Provided for`) %>%  #add a variable of food pounds per person
  filter(!is.na(ave_food_pounds))      #remove NA caused by 0 in `Food Provided for`

df_food <- df_main %>%
  group_by(year, month) %>%
  summarise(food = sum(`Food Pounds`),
            food_provide = sum(`Food Provided for`)
            )
pairs(df_food)
cor(df_food[,1:4])

ggplot(df_food, aes(x=year, y=food)) +
  geom_col(aes(fill=month)) +
  labs(title = 'Pounds of Food Provided to the homeless',
       y = 'Pounds of Food')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/food.png', height=4, width=6)

ggplot(df_food, aes(x=year, y=food_provide)) +
  geom_col(aes(fill=month)) +
  labs(title = 'Number of People in the Family for Which Food was Provided',
       y = 'Number of People been supported')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/food_provided.png', height=6, width=8)

df_food2 <- df_main %>%
  group_by(year) %>%
  summarise(food_ave = mean(ave_food_pounds))

ggplot(df_food2, aes(x=year, y=food_ave)) +
  geom_line() +
  geom_point(size = 3, alpha = 0.6, color='red') +
  labs(title = 'Pounds of Food Provided Per Person',
       y = 'Average Pounds of Food Provided')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/food_ave.png', height=6, width=8)

df_cloth <- df_main %>%
  group_by(year, month) %>%
  summarise(clothing = sum(`Clothing Items`))

ggplot(df_cloth, aes(x=year, y=clothing)) +
  geom_col(aes(fill=month)) +
  labs(title = 'Amount of Provided Clothing Items',
       y = 'Clothing items')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/clothing.png', height=6, width=8)

ggplot(df_cloth, aes(x=month, y=clothing)) +
  geom_smooth() +
  geom_point(color = 'lightblue') +
  scale_x_continuous(breaks = seq(1:12)) +
  labs(title = 'Pounds of Food Provided Per Person',
       y = 'Average Pounds of Food Provided')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/cloth_smooth.png', height=6, width=8)

#df_food_cloth <- df_main %>%
#  group_by(year, month) %>%
#  summarise(count = n(),
#            food = mean(ave_food_pounds),
#            cloth = mean(`Clothing Items`))
#ggplot(df_food_cloth, aes(x=food, y=cloth)) +
#  geom_bin2d(aes(fill=count))
  

#predict with linear regression
df_client <- df %>%
  filter(year>2010 & year<2019) %>%
  group_by(year) %>%
  summarise(cases = n(),
            clients =length(unique(`Client File Number`)))

lmod <- lm(cases ~ year, data = df_client) #predict the demand in 2019
predict_cases2019 <- lmod$coefficients %*% c(1,2019)
ggplot(df_client, aes(x = year, y = cases)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = 'Trend of Amount of Cases')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/predict_cases.png', height=6, width=8)

lmod2 <- lm(clients ~ year, data = df_client) #predict the clients in 2019
predict_clients2019 <- lmod2$coefficients %*% c(1,2019)
ggplot(df_client, aes(x = year, y = clients)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = 'Trend of Number of clients')
ggsave('/Users/zhangwan/Documents/GitHub/bios611-projects-fall-2019-Wan-z/project_1/results/predict_clients.png', height=6, width=8)



