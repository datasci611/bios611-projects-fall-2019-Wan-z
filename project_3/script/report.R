library('tidyverse')
library('ggplot2')
library(rmarkdown)

df_client <- read.csv('../data/CLIENTS.csv')
df_case <- read.csv('../data/CASE.csv')
df_n <- read.csv('../data/number.csv')

render('./report.Rmd', output_file='../results/report.html')
