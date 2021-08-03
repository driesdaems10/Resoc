rm(list=ls())

setwd('C:/Users/daems/OneDrive/Work/Code/GitHub/Resoc/Results')

library(readxl)
library(reshape2)
library(ggplot2)

wood <- read_excel("wood.xlsx")

data_prep <- wood[-c(1:28),]
data_prep2 <- data_prep[-c(3,4,5,7,8,9,11,12,13,15,16,17,19,20,21,23,24,25,27,28,29,31,32,33,35,36,37,39,40,41,43,44,45,47,48,49,51,52,53,55,56,57,59,60)]                  
data_prep3 <- data_prep2[-c(1,2),]
colnames(data_prep3) <- c('timestep', 'community_0', 'community_1', 'community_2', 'community_3', 'community_4', 'community_5', 
'community_6', 'community_7', 'community_8', 'community_9', 'community_10', 'community_11', 'community_12', 'community_13',
'community_14')

dfplot <- reshape2::melt(data_prep3, id.vars = "timestep")
str(data_prep3)

ggplot(data = dfplot) +
  aes(x = as.numeric(timestep), y = as.numeric(value), color = as.factor(variable)) +
  geom_line(size = 1) +
 # geom_point() +
  labs(x = "Timestep",
       y = 'Wood harvest') +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.text.x = element_text(color = "grey10", size = 16, face = "plain"),
        axis.text.y = element_text(color = "grey10", size = 16, face = "plain"),  
        axis.title.x = element_text(color = "grey10", size = 20, face = "italic"),
        axis.title.y = element_text(color = "grey10", size = 20, face = "italic")) +
   scale_colour_manual(values = rep('#77777a', 15))

food_prod <- read_excel("ResEx regeneration-time-table.xlsx")
food_prep <- food_prod[-c(1:8),]
food_prep2 <- food_prep[-c(2)]
food_prep3 <- subset(food_prep2, ...3 != '0')
colnames(food_prep3) <- c('run', 'timestep', 'community_0', 'community_1', 'community_2', 'community_3', 'community_4', 'community_5', 
                          'community_6', 'community_7', 'community_8', 'community_9', 'community_10', 'community_11', 'community_12', 'community_13',
                          'community_14')

dfplot2 <- reshape2::melt(food_prep3, id.vars = c("timestep", 'run'))

ggplot(data = dfplot2) +
  aes(x = as.numeric(timestep), y = as.numeric(value), color = as.factor(variable)) + 
  facet_wrap(vars(as.numeric(run))) +
  geom_line(size = 0.1) +
  # geom_point() +
  labs(x = "Timestep",
       y = 'Food harvest') +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.text.x = element_text(color = "grey10", size = 8, face = "plain"),
        axis.text.y = element_text(color = "grey10", size = 10, face = "plain"),  
        axis.title.x = element_text(color = "grey10", size = 8, face = "italic"),
        axis.title.y = element_text(color = "grey10", size = 10, face = "italic")) +
  scale_colour_manual(values = rep('#77777a', 15))