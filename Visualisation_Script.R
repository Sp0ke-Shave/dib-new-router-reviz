#Setting and load

#loading libraries
library(tidyverse)
library(ggthemes)
library(lemon)

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
  mutate(Type = "HW")

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

mobile_5 <- left_join(Base_line_2.4_and_5, New_5) %>%
  mutate(Type = "G5")

rm(Base_line_2.4_and_5, New_5)

# Joining connection types

tidy_pairs <- HW %>%
  rbind(mobile_2.4,
        mobile_5)

rm(HW, mobile_2.4, mobile_5, raw)

# Trying a scatter plot ----

#Down only
tidy_pairs %>%
  filter(Direction == "Up" ) %>%
  ggplot(aes(x= Old_Speed, y = New_Speed, col = Type, fill = Type, shape = Device_1))  +
  geom_point() +
  geom_jitter() +
  facet_wrap(~Location)

# Don't really like it 

# Boxplot again ----
tidy_pairs %>%
  mutate(Plot = paste(tidy_pairs$Location, tidy_pairs$Device_1, tidy_pairs$Type, sep = "-")) %>%
 
  mutate(Pct_change = (New_Speed -
          Old_Speed) / Old_Speed) %>%
  ggplot(aes(y = Pct_change, x = Direction, fill = Type)) +
  geom_boxplot() +
  facet_wrap(~Plot)

# Try again, plotting new vs old rather than pct difference ----

tidy_pairs %>%  
  # Gathering the old and new speed into one column
  gather(key = "System", value = "Speed", New_Speed, Old_Speed)  %>%
  # Adding a combo variable for each different plot I want to see
  mutate(plot = paste(Location, Device_1, Type, sep = " ")) %>%
  # Plotting
  ggplot(aes(x = Direction, y = Speed, fill = System, col = System)) +
  geom_point(size = 2) +
  geom_jitter(size = 2, width = .25) + 
  facet_rep_wrap(~plot, repeat.tick.labels = 'bottom') +
  theme(legend.position = c(.8,.1), 
        panel.background = element_rect(colour = 'black', fill = 'white'), 
        strip.background = element_blank(),
        plot.caption = element_text(hjust = .1)) +
  labs(title = "Re-vis of speed testing new router", 
       subtitle = "https://www.reddit.com/r/dataisbeautiful/comments/g263d8/oc_got_a_new_routerwireless_setup_ill_take_any/",
       caption = "Code and dtails available from here: https://github.com/Sp0ke-Shave/dib-new-router-reviz",
       x = NULL) 

# I like this one 