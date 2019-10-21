library('tidyverse') 
library(stringr)
library(ggplot2)
library(RColorBrewer)

#import data
df <- read_tsv('./data/UMD_Services_Provided_20190719.tsv')
date <- str_split_fixed(df$Date, '/', 3)
df$month = as.numeric(date[,1])
df$day = as.numeric(date[,2])
df$year = as.numeric(date[,3])

df_bus <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('Bus Tickets (Number of)') %>%
  filter('Bus Tickets (Number of)' > 0) %>%
  group_by(year, month) %>%
  summarise(count = n(),
            number_bus_tickets = sum(`Bus Tickets (Number of)`)
  )

df_diapers <- df %>%
  filter(year>1996 & year<2019 & Diapers<5000) %>%
  drop_na('Diapers') %>%
  group_by(year, month) %>%
  summarise(count = n(),
            diapers = sum(Diapers * 44)
  )

df_schoolkits <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('School Kits') %>%
  filter(`School Kits` > 0) %>%  #we treat 0 as NA
  group_by(year, month) %>%
  summarise(count = n(),
            school_kits = sum(`School Kits`)
  )

#arranged school kits
df_schoolkits.month <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('School Kits') %>%
  filter(`School Kits` > 0) %>%  #we treat 0 as NA
  group_by(month) %>%
  summarise(count = n(),
            school_kits = sum(`School Kits`)
  ) %>%
  arrange(desc(school_kits))

#hygienekits
df_hgkit <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('Hygiene Kits') %>%
  group_by(year, month) %>%
  summarise(count = n(),
            hgkit = sum(`Hygiene Kits`)
  )

df_finance <- df %>%
  filter(year>1996 & year<2019) %>%
  drop_na('Financial Support') %>%
  filter(`Financial Support` > 0) %>%
  group_by(year, month) %>%
  summarise(count = n(),
            finance = sum(`Financial Support`)
  )

l_disc <- list(df_bus, df_diapers, df_schoolkits, df_hgkit, df_finance)
l_index <- c('number_bus_tickets', 'diapers', 'school_kits', 'hgkit', 'finance')
l_title <- c('Bus Tickets (Number of)', 'Diapers (average amount)', 'School Kits', 'Hygiene Kits', 'Financial Support')
l_label <- c('Number of bus tickets', 'Amount of diapers', 'Annual Number of School Kits', 'Annual amount of hgyiene kits', 'Financial Support')

#function for trends plots
f <- function(i){
  df_1 <- as.data.frame(l_disc[i])
  df_new <- df_1 %>%
    group_by(year) %>%
    summarise(count = n(),
              variable = sum(.data[[l_index[i]]])
    )
  p<- ggplot(df_new) +
        geom_line(mapping = aes(x = year, y = variable)) +
        geom_point(mapping = aes(x = year, y = variable),size = 2, alpha = 0.6, color='red') +
        labs(title = l_title[i],
             y = l_label[i])
  return(p)
}

#function for distribution
f_distri <- function(i){
  df_1 <- as.data.frame(l_disc[i])
  p<- ggplot(df_1, aes_string(x='year', y=l_index[i], fill = 'month')) +
    geom_col() +
    scale_fill_gradient2(low="blue", mid="white", high="red") +
    labs(title = l_title[i],
         y = l_label[i])
  return(p)
}

f_smooth <- function(i){
  df_1 <- as.data.frame(l_disc[i])
  p <- ggplot(df_1, aes_string(x='month', y=l_index[i])) + 
    geom_smooth(se = FALSE) +
    scale_x_continuous(breaks = seq(1:12)) +
    geom_point(color = 'darkblue') +
    labs(title = l_title[i],
         y = l_label[i])
  return(p)
}

tx <- function(i){
  if(i == 1 |i == 3){
    print('This service is provided seasonally.')
  }else{
    print('This service is not related to month.')
  }
}

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

f_food <- function(i){
  p1 <- ggplot(df_food, aes(x=year, y=food)) +
    geom_col(aes(fill=month)) +
    labs(title = 'Pounds of Food Provided to the homeless',
         y = 'Pounds of Food')
  p2 <- ggplot(df_food, aes(x=year, y=food_provide)) +
    geom_col(aes(fill=month)) +
    labs(title = 'Number of People in the Family for Which Food was Provided',
         y = 'Number of People been supported')
  df_food2 <- df_main %>%
    group_by(year) %>%
    summarise(food_ave = mean(ave_food_pounds))
  p3 <-  ggplot(df_food2, aes(x=year, y=food_ave)) +
    geom_line() +
    geom_point(size = 3, alpha = 0.6, color='red') +
    labs(title = 'Pounds of Food Provided Per Person',
         y = 'Average Pounds of Food Provided')
  if(i==1){
    return(p1)
  }
  if(i==2){
    return(p2)
  }
  else{
    return(p3)
  }
}


#clothing
df_cloth <- df_main %>%
  group_by(year, month) %>%
  summarise(clothing = sum(`Clothing Items`))

p1 <- ggplot(df_cloth, aes(x=year, y=clothing)) +
        geom_col(aes(fill=month)) +
        labs(title = 'Amount of Provided Clothing Items',
             y = 'Clothing items')

p2 <- ggplot(df_cloth, aes(x=month, y=clothing)) +
        geom_smooth() +
        geom_point(color = 'lightblue') +
        scale_x_continuous(breaks = seq(1:12)) +
        labs(title = 'Pounds of Food Provided Per Person',
             y = 'Average Pounds of Food Provided')

f_cloth <- function(i){
  if(i==1){
    return(p1)
  }else{
    return(p2)
  }
}

#clients
df_client <- df %>%
  filter(year>1996 & year<2019) %>%
  group_by(year) %>%
  summarise(cases = n(),
            clients =length(unique(`Client File Number`)))

