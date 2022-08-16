# Clear the work environment
rm(list=ls())

# Load required packages
library(readxl)
library(reshape2)
library(ggplot2)

# Set working directory to location of result files and load them
# setwd(...)
wood <- read_excel("wood.xlsx")
food_prod <- read_excel("ResEx regeneration-time-table.xlsx")

# Data Preparation

## Wood harvesting 
data_prep <- wood[-c(1:28),]
data_prep2 <- data_prep[-c(3,4,5,7,8,9,11,12,13,15,16,17,19,20,21,23,24,25,27,28,29,31,32,33,35,36,37,39,40,41,43,44,45,47,48,49,51,52,53,55,56,57,59,60)]                  
data_prep3 <- data_prep2[-c(1,2),]
colnames(data_prep3) <- c('timestep', 'community_0', 'community_1', 'community_2', 'community_3', 'community_4', 'community_5', 
                          'community_6', 'community_7', 'community_8', 'community_9', 'community_10', 'community_11', 'community_12', 'community_13',
                          'community_14')
dfplot <- reshape2::melt(data_prep3, id.vars = "timestep")

## Food harvesting
food_prep <- food_prod[-c(1:8),]
food_prep2 <- food_prep[-c(2)]
food_prep3 <- subset(food_prep2, ...3 != '0')
colnames(food_prep3) <- c('run', 'timestep', 'community_0', 'community_1', 'community_2', 'community_3', 'community_4', 'community_5', 
                          'community_6', 'community_7', 'community_8', 'community_9', 'community_10', 'community_11', 'community_12', 'community_13',
                          'community_14')

dfplot2 <- reshape2::melt(food_prep3, id.vars = c("timestep", 'run'))
dfplot2$regeneration_time <- cut(as.numeric(dfplot2$run),
                                 breaks=c(-Inf,10,20,Inf),
                                 labels=c("1","2","3"))

# Graphing

## Wood harvesting
ggplot(data = dfplot) +
  aes(x = as.numeric(timestep), y = as.numeric(value), color = as.factor(variable)) +
  geom_line(size = 0.5) +
  labs(x = "Timestep",
       y = 'Wood harvest (m³ per year and per capita)') +
  ylim(c(0,40))+ # high y-values from first timesteps excluded for clarity
  theme_minimal()+
  guides(color="none")+
  theme(axis.title.x = element_text(margin = margin(t = 30)),
        axis.title.y = element_text(margin = margin(r = 30)))
  
## Food harvesting 
ggplot(data=dfplot2)+
  aes(x=as.numeric(timestep),y=as.numeric(value),color=as.factor(variable))+
  facet_wrap(~regeneration_time)+
  geom_smooth()+
  labs(x = "Timestep",
       y = "Food harvest (ton per year and per capita)")+
  theme_minimal()+
  guides(color="none")+
  theme(axis.title.x = element_text(margin = margin(t = 30)),
        axis.title.y = element_text(margin = margin(r = 30)))
