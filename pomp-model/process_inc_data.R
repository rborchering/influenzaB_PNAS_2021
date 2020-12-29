library(tidyverse)

real_data_3s <- read_csv(file = "./flu_inc_2016_18.csv") %>% 
  mutate(time = Year - 2014) %>% 
  select(time, starts_with("total_"))


df_ma_2019 <- read_csv(file = "./flu_inc_2019.csv")




