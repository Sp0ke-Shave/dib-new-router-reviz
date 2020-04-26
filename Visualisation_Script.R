#Creating visualisation

#loading libraries
library(tidyverse)
library(ggthemes)

raw <- read_delim('D-I-B-new-router-revis-raw-data.csv', delim = ',')

# Boxplots ----

# I tried some box plots but I don't like them - too many categories. 


raw %>%
  #Filtering Up only
  filter(Direction == "Up") %>%

  ggplot(aes(y = `Speed (Mbps)`, x = Hardware)) +
  geom_boxplot()+ 
  facet_grid(Connection~Device_1) + 
  theme_few() + 
  theme(axis.text = element_text())+
  theme(axis.text.x = element_text(angle = 90))

# re-wrangling the data for scatter plots ---- 

# Way forward is to have comparisons so there are two numbers for each test old and new. 
# This means that the old values will have to double as old for 2.4g and 5g. But that is OK. 

# HW data ----

Base_line_HW <- raw %>%
  mutate(Old_Speed = raw$`Speed (Mbps)`) %>%
  filter(Tech == "Arris Router", Connection == "Old-HW") %>%
  select(-'Speed (Mbps)',
         -Hardware,
         -Tech, 
         -Connection,
         -Device)

New_HW <- raw %>%
  mutate(New_Speed = raw$`Speed (Mbps)`) %>%
  filter(Connection == "New-HW") %>%
  select(-'Speed (Mbps)',
         -Hardware,
         -Tech, 
         -Connection,
         -Device)

HW <- left_join(Base_line_HW, New_HW)%>%
  mutatey(Type = "HW")

rm(New_HW, Base_line_HW)

# 2.4G ----

Base_line_2.4_and_5 <- raw %>%
  mutate(Old_Speed = raw$`Speed (Mbps)`) %>%
  filter(Tech == "Arris Router", Connection == "Old") %>%
  select(-'Speed (Mbps)',
         -Hardware,
         -Tech, 
         -Connection,
         -Device)

New_2.4 <- raw %>%
  mutate(New_Speed = raw$`Speed (Mbps)`) %>%
  filter(Connection == "2.4G") %>%
  select(-'Speed (Mbps)',
         -Hardware,
         -Tech, 
         -Connection,
         -Device)

mobile_2.4 <- left_join(Base_line_2.4_and_5, New_2.4) %>%
  mutate(Type = "G2.4")

rm(New_2.4)

# 5G---- 

New_5 <- raw %>%
  mutate(New_Speed = raw$`Speed (Mbps)`) %>%
  filter(Connection == "5G") %>%
  select(-'Speed (Mbps)',
         -Hardware,
         -Tech, 
         -Connection,
         -Device)

# Then the 5 g stuff

mobile_5 <- mobile_2.4 <- left_join(Base_line_2.4_and_5, New_5) %>%
  mutate(Type = "G5")

rm(Base_line_2.4_and_5, New_5)

# Joining connection types

tidy_pairs <- HW %>%
  rbind(mobile_2.4,
        mobile_5)

rm(HW, mobile_2.4, mobile_5, raw)
