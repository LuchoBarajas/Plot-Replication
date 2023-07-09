######################################################
#    Ideology of presidential candidates in the US   #
######################################################

#--------------------------------------------------------------------------------
# The objective of this code is to replicate a plot made by Pablo Barbera for 
# The Washington Post. The plot can be found in: 
# https://www.washingtonpost.com/news/monkey-cage/wp/2015/06/16/who-is-the-most-conservative-republican-candidate-for-president/?utm_term=.081a276328ad
#--------------------------------------------------------------------------------

# Load packages here
suppressMessages(library("tidyverse"))
suppressMessages(library("ggthemes"))
suppressMessages(library("magrittr"))
suppressMessages(library("openxlsx"))
suppressMessages(library("data.table"))
suppressMessages(library("ggpubr"))


# Data for main plot
ideology <- read.csv("/Users/luchobarajas/Documents/ASDS/Michelmas Term/DDS/assignment-02-LuchoBarajas/data/ideology_1.csv")

# Data for background plots
bg <- read.csv("/Users/luchobarajas/Documents/ASDS/Michelmas Term/DDS/assignment-02-LuchoBarajas/data/ideology_2.csv")
bg %>% filter(ideology >= -2.5 & ideology <= 2.5) %>% group_by(type) %>% summarise(mean = mean(ideology))

# Plot-------

suppressMessages(ggplot() + 
                   
# Point Plot
geom_pointrange(data = ideology, aes(x = twscore, y =fct_rev(as.factor(twscore)),fatten = 2,color = party, xmin = twscore - twscore.sd*1.96, xmax = twscore + twscore.sd*1.96)) + 
 scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red", "Z" = "black")) + 
 geom_text(data = ideology,aes(x = twscore- twscore.sd*1.96, y =fct_rev(as.factor(twscore)),label = if_else(twscore < 0, screen_name, ""),hjust=1, vjust=0.5, color = party),nudge_x = -0.03, size = 2, family = "Times") + 
 geom_text(data = ideology,aes(x = twscore + twscore.sd*1.96, y =fct_rev(as.factor(twscore)),label = if_else(twscore > 0, screen_name, ""),hjust= 0, vjust=0.5,color = party,size = 0.01),nudge_x = 0.03, size = 2, family = "Times") + 
                   
# Density Plot
geom_density(data =bg, aes(x=ideology, y= ..density..*20, fill = type),colour = NA, alpha = 0.2)+
xlim(c(-2.5, 2.5))+ 
scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red", "Z" = "black")) +
theme_tufte()+ 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none", axis.line.y = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 11)) + 
  labs(x = "Position on latent ideological scale",
  title = "Twitter ideology scores of potential Democratic and Republican presidential primary candidates")+
  geom_vline(xintercept = -1.01212725, color = "blue", size= 0.5, alpha = 0.2) +
  geom_vline(xintercept = 0, color = "black", size= 0.5, alpha = 0.2)+
  geom_vline(xintercept = 0.78739357	, color = "red", size= 0.5, alpha = 0.2)+
  geom_text(aes(x=-1.18, label="\nAverage Democrat", y=5), colour="dark blue", angle=90, size = 2)+
  geom_text(aes(x=-1.1, label="\nin 114th Congress", y=5), colour="dark blue", angle=90, size = 2)+
  geom_text(aes(x=-0.1, label="\nAverage Twitter User", y=5), colour="black", angle=90, size = 2)+
  geom_text(aes(x=0.62, label="\nAverage Republican", y=27), colour="black", angle=90, size = 2)+
  geom_text(aes(x=0.7, label="\nin 114th Congress", y=27), colour="black", angle=90, size = 2))
