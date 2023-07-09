######################################################
#   US Partisan Asymetries by Gender Replication     #
######################################################

#--------------------------------------------------------------------------------
# The objective of this code is to replicate a plot that shows the differences  -
# ammong democrats and republican members in terms of social metrics in Facebook-
#--------------------------------------------------------------------------------


# Load packages here
suppressMessages(library("tidyverse"))
suppressMessages(library("ggthemes"))
suppressMessages(library("magrittr"))
suppressMessages(library("openxlsx"))
suppressMessages(library("data.table"))
suppressMessages(library("ggpubr"))

# Data for the plot
fb <- read.csv("/data/fb-congress-data.csv", stringsAsFactors=FALSE)


# Plot-------
data_set = fb
data_set$party %<>% str_replace_all("Democrat", "D") 
data_set$party %<>% str_replace_all("Republican", "R") 
data_set %<>% mutate(party_gender = paste0(party,"-", gender))
data_RF = data_set %>% filter(party_gender == "R-F")
data_NRF = data_set %>% filter(party_gender != "R-F")
data_RF %<>% filter(complete.cases(.))
data_set = data_NRF %>% rows_append(data_RF)

data_set%<>% pivot_longer(cols = likes_count:sad_count, names_to = "interaction_type", values_to = "total_count") %>%
  filter(party != "Independent") 

data_set %<>% group_by(party_gender,interaction_type) %>% summarise(average = mean(total_count, na.rm = TRUE))

data_set_2 <- data_set
data_set_2$interaction_type <-  factor(data_set_2$interaction_type, levels = c("likes_count", "comments_count", "shares_count","love_count", "haha_count", "wow_count", "angry_count", "sad_count"))

p2 <- ggplot(data_set_2, aes(x = party_gender, y = average))
p2 + geom_col(aes (fill = party_gender)) + theme_minimal() + facet_wrap(~ interaction_type, nrow=2, scales = "free_y") + 
  labs(x = "Party and gender of Member of Congress",
       y = "Average of each type of social metric",
       title = "Partisan asymetries by gender in Facebook popularity metrics",
       subtitle = "Female Democrats receive more engagement than Male Democrats. The opposite is true for Republicans") +
  theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9)) + 
  scale_fill_manual(values = c("D-F" = "darkblue", "D-M" = "blue", "R-F"= "darkred", "R-M" = "red"))
