library(tidyverse)
library(here)
source('get_data.R')

data <- read_csv(get_data_path(data_type = 'processed', 
                               data_name = 'phd3_deathsbyyear.csv'))

#get leading causes of death before covid

data_to_2019 <- data %>% 
  pivot_longer(`Malignant neoplasms`:Suicide, 
                names_to = 'disease', 
                values_to = 'num_deaths') %>% 
  filter(year < 2020) %>% 
  group_by(county_residence, disease) %>% 
  summarize('total_deaths' = sum(num_deaths)) %>% 
  group_by(county_residence) %>% 
  arrange(desc(total_deaths), .by_group = TRUE)

