######################################################
#              Chaos Plot Replication                #
######################################################

#--------------------------------------------------------------------------------
# The objective of this code is to replicate a plot that represents the         -
# chaos theory, which can be found in https://en.wikipedia.org/wiki/Logistic_map-
#--------------------------------------------------------------------------------


# Load packages here
suppressMessages(library("tidyverse"))
suppressMessages(library("ggthemes"))
suppressMessages(library("magrittr"))
suppressMessages(library("openxlsx"))
suppressMessages(library("data.table"))
suppressMessages(library("ggpubr"))

# Creating the data required for the Creation of the Chaos Plot -----

# Note: This code was provided as an input for the data for data scientist class
# at the aplied social data science program 

# x observations for each r value
n <- 1000
# Step between each r value
r_step <- 0.001

r_range <- seq(2.5, 4, by = r_step)
to_discard <- 500 # numbers of observations discarded before the n which are stored
logistic_map_data <- matrix(0, nrow = n*length(r_range), 2)
for (r in r_range) {
  
  current_logistic_map_series <- numeric(n+to_discard)
  current_logistic_map_series[1] <- 0.5
  
  for (k in 1:(n+to_discard-1)) {
    
    current_logistic_map_series[k+1] <- r*current_logistic_map_series[k]*(1-current_logistic_map_series[k])
    
  }
  
  start_index <- 1+n*(match(r, r_range) - 1)
  end_index <- n*match(r, r_range)
  
  logistic_map_data[start_index:end_index,1] <- r
  logistic_map_data[start_index:end_index,2] <- tail(current_logistic_map_series,n)
  
}

logistic_map_data <- as_tibble(data.frame(logistic_map_data))
colnames(logistic_map_data) <- c("r", "x")


# Plot-------
data_set <- logistic_map_data %>% mutate(group = if_else(r < 3.5,"group_1", if_else(r <= 3.6,  "group_2", 
                                                 if_else(r <= 3.7, "group_3", 
                                                 ifelse(r <= 3.8, "group_4",
                                                 ifelse(r <= 3.9, "group_5", "group_6"))))))

p1 <- ggplot(data = data_set, mapping = aes(x = r, y = x, color = group))
p1 + geom_point(size = 0.01, alpha = 0.01) + labs(x= "r") + theme_classic() + 
  theme(panel.background = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none", axis.line.y = element_blank())

