library('tidyverse')
library('ggplot2')
library(MASS)
df_client <- read.csv('./data/CLIENTS.csv')
df_case <- read.csv('./data/CASE.csv')
df_n <- read.csv('./data/number.csv')

ggplot(df_client, aes(x = frequency)) +
  geom_histogram(fill = 'red', alpha = 0.5, color = 'red') +
  labs(title = 'How Many Time Clients Come to UMD')
ggsave('./results/freq_hist.png', height=4, width=6)

ggplot(df_client, aes(x = Period)) +
  geom_histogram(fill = 'orange', alpha = 0.5, color = 'orange') +
  labs(title = 'How Long Clients Stay with UMD Per Time (days)')
ggsave('./results/period_hist.png', height=4, width=6)

#disable
df_client$disability <- as.factor(df_client$disability)
ggplot(df_client, aes(x = disability, y = frequency)) +
  geom_boxplot(aes(group = cut_width(disability, 0.25)), outlier.alpha = 0.1) +
  labs(title = 'How Disabiltiy Status Influence Frequency')
ggsave('./results/disability_freq.png', height=4, width=6)

ggplot(df_client, aes(x = disability, y = Period)) +
  geom_boxplot(aes(group = cut_width(disability, 0.25)), outlier.alpha = 0.1) +
  labs(title = 'How Disabiltiy Status Influence Period')
ggsave('./results/disability_period.png', height=4, width=6)

#healins
df_client$healthins_types <- as.factor(df_client$healthins_types)
ggplot(df_client, aes(x = healthins_types, y = frequency)) +
  geom_boxplot(aes(group = cut_width(healthins_types, 0.25)), outlier.alpha = 0.1) +
  labs(title = 'How Health Insurance Covering Status Influence Frequency')
ggsave('./results/healthins_freq.png', height=4, width=6)

ggplot(df_client, aes(x = healthins_types, y = Period)) +
  geom_boxplot(aes(group = cut_width(healthins_types, 0.25)), outlier.alpha = 0.1) +
  labs(title = 'How Health Insurance Covering Status Influence Period')
ggsave('./results/healthins_peri.png', height=4, width=6)


#income
ggplot(df_client, aes(x = Monthly.Amount..Exit., y = frequency)) +
  geom_bin2d() +
  labs(title = 'How Income Influence Frequency', x = 'Monthly Income')
ggsave('./results/income_freq.png', height=4, width=6)

ggplot(df_client, aes(x = Monthly.Amount..Exit., y = Period)) +
  geom_bin2d() +
  labs(title = 'How Income Influence Period', x = 'Monthly Income')
ggsave('./results/income_peri.png', height=4, width=6)

#change
ggplot(df_client, aes(x = frequency, y = changes)) +
  geom_col(fill = 'red', alpha = 0.5) +
  labs(title = 'Changes in Income V.S. Frequency', y='Changes in Income')
ggsave('./results/incomechanges_freq.png', height=4, width=6)

ggplot(df_client, aes(x = Period, y = changes)) +
  geom_point(color = 'blue', alpha = 0.5) +
  labs(title = 'Changes in Income V.S. Period', y='Changes in Income')
ggsave('./results/incomechanges_peri.png', height=4, width=6)

#cases
#destination after leave
ggplot(df_case %>% filter(Destination != ""), aes(x = Destination)) +
  geom_bar(position = position_stack(reverse = TRUE), fill = 'blue', alpha = 0.6) +
  coord_flip() +
  theme(legend.position = "top")
ggsave('./results/distination.png', height=4, width=10)

#period vs entry age
ggplot(df_case %>% filter(housing_status != ""), aes(x = Client.Age.at.Entry, y = Period)) +
  geom_col(aes(fill = housing_status)) +
  labs(title = 'Period vs Client Age at Entry',
       x = 'Client Age at Entry')
ggsave('./results/period_age.png', height=4, width=10)

df_casenew <- df_case %>%
  filter(housing_status != "") %>%
  group_by(Client.Age.at.Entry) %>%
  summarise(period = mean(Period))
ggplot(df_casenew, aes(x = Client.Age.at.Entry, y = period)) +
  geom_smooth(se = FALSE, color = 'darkblue') +
  geom_point(color = 'blue', size = 0.8, alpha = 0.6) + 
  labs(title = 'How Period Influenced by Client Age at Entry',
       x = 'Client Age at Entry')
ggsave('./results/period_age_smooth.png', height=4, width=6)
  
#month on street
df_case$months_on_street <- factor(df_case$months_on_street, levels=c('<1', '2', '3', '4','5','6','7','8', '9','10','11','12','>12'))
ggplot(df_case %>% filter(months_on_street != ""&domestic_violence != ""), aes(x = months_on_street)) +
  geom_bar(aes(fill = domestic_violence)) +
  labs(title = 'How Many Months the Clients Homeless on Street in Past 3 Years',
       x = 'Number of Months Homeless on Street')
ggsave('./results/month_on_street.png', height=4, width=6)

df_case_sub <- df_case %>%
  group_by(prior_living_situation) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 
df_case_sub <- df_case %>%
  filter(prior_living_situation %in% c("Place not meant for habitation (HUD)", "Emergency shelter, incl. hotel/motel paid for w/ ES voucher, or RHY-funded Host Home shelter (HUD)", "Staying or living in a friend's room, apartment or house (HUD)", "Staying or living in a family member's room, apartment or house (HUD)"))
ggplot(df_case_sub %>% filter(months_on_street != ""&prior_living_situation != ""), aes(x = months_on_street)) +
  geom_bar(aes(fill = prior_living_situation)) +
  theme(legend.position = "bottom", legend.direction = 'vertical') +
  labs(title = 'How Many Months the Clients Homeless on Street in Past 3 Years',
       x = 'Number of Months Homeless on Street')
ggsave('./results/month_on_street_priorliving.png', height=6, width=6)



#distribution of clients and cases
df_ns <- df_n %>% 
  gather(type, number, c(number_cases, number_clients)) 
ggplot(df_ns, aes(x = Entry_Year, y = number)) +
  geom_line(aes(color = type)) +
  geom_point(size = 0.8, aes(shape = type)) +
  scale_fill_discrete(breaks=c("number_cases", "number_clients"),
                      labels = c("Number of Cases", "Number of Clients")) +
  labs(title = 'Distribution of Amount of Clients and Cases')
ggsave('./results/distrib.png', height=4, width=6)

#df_n$Entry_Year <- as.numeric(df_n$Entry_Year)-2014
#fitdistr(as.vector(df_n$number_cases), 'poisson')
#df_n <- df_n %>%
#  filter(Entry_Year < 5)
#fit <- glm(number_cases ~ Entry_Year, family = poisson(link = "log"), data = df_n)
#exp(6.75071 + 0.04924*6)

