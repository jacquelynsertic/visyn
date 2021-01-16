# Importing visyn data from excel

library('readxl')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('dslabs')
library('ggthemes')
library("gridExtra")
library("cowplot")
library("showtext")
library("gridExtra")
library("ggpubr")

#load the data into R from excel
visyn <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Visyn\\Visyn MVP App Testing Data-C-Clean_Konczak edited.xlsx', sheet = 'fourremoved') #load excel sheet into R

########################################################################################
######## Individual Data ###############################################################
########################################################################################

#### Scatterplots to compare individual data
### Overall Data
### condition as color; subject as shape

## Release Time
visyn %>% ggplot(aes(movementTime, trial, group = condition)) + 
  geom_point(size = 4, 
             aes(shape = subject, color = condition)) +
  scale_x_continuous(name = "Movement Time (s)", 
                     limit = c(0.3, 1.3), 
                     breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3)) +
  scale_y_continuous(name = "Trial Number", 
                     breaks = c(1:20)) +
  scale_shape_manual(values = c(15,0,16,1,17,2)) + 
  scale_fill_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_color_manual(values = c("black", "#3300CC", "#FFD700")) +
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15),
        legend.title = element_blank())

## Velocity
visyn %>% ggplot(aes(velocity, trial, group = condition)) + 
  geom_point(size = 4, 
             aes(shape = subject, color = condition)) +
  scale_x_continuous(name = "Velocity at Release (mph)", 
                     limit = c(20, 55), 
                     breaks = c(20, 25, 30, 35, 40, 45, 50, 55)) +
  scale_y_continuous(name = "Trial Number", 
                     breaks = c(1:20)) +
  scale_shape_manual(values = c(15,0,16,1,17,2)) + 
  scale_fill_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_color_manual(values = c("black", "#3300CC", "#FFD700")) +
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15),
        legend.title = element_blank())

## Accuracy
visyn %>% ggplot(aes(accuracy, trial, group = condition)) + 
  geom_point(size = 4, 
             aes(shape = subject, color = condition)) +
  scale_x_continuous(name = "Distance from Target (inches))", 
                     limit = c(0,60), 
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
  scale_y_continuous(name = "Trial Number", 
                     breaks = c(1:20)) +
  scale_shape_manual(values = c(15,0,16,1,17,2)) + 
  scale_fill_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_color_manual(values = c("black", "#3300CC", "#FFD700")) +
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15),
        legend.title = element_blank())

########################################################################################
######## Conditions only ###############################################################
########################################################################################

######## Scatterplots

## Release Time
(g <- visyn %>% ggplot(aes(movementTime, trial, group = condition)) + 
   geom_point(size = 5, shape = 16,
              aes(color = condition)) +
   scale_x_continuous(name = "Movement Time (s)", 
                      limit = c(0, 1.1), 
                      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
   scale_y_continuous(name = "Trial Number", 
                      breaks = c(1, 5, 10, 15, 20)) +
   labs(tag = "A")+ 
   scale_color_manual(labels = c("Baseline", "Physical\nPractice Only", "Visyn &\nPhysical Practice"), 
                      values = c("black", "#3300CC", "#FFD700")) +
   theme_bw() + #sets the theme to plane
   theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black")) +
   theme(axis.title.y = element_text(vjust = 2, size = 30), 
         axis.text = element_text(color = "black", size = 25), 
         axis.title.x = element_text(color = "black", vjust = 0, size = 30),
         legend.title = element_blank()) +
   theme(legend.text = element_text(size = 20),
         legend.position = "bottom",
         legend.key.size = unit(1.5, "cm"),
         legend.title = element_blank()))

## Velocity
(h <- visyn %>% ggplot(aes(velocity, trial, group = condition)) + 
    geom_point(size = 5, shape = 16,
               aes(color = condition)) +
    scale_x_continuous(name = "Velocity (mph)", 
                       limit = c(20, 55), 
                       breaks = c(20, 25, 30, 35, 40, 45, 50, 55)) +
    scale_y_continuous(name = "Trial Number", 
                       breaks = c(1, 5, 10, 15, 20)) +
    labs(tag = "A") +
    scale_fill_manual(values = c("black", "black", "black", "black", "black", "black")) +
    scale_color_manual(labels = c("Baseline", "Physical\nPractice Only", "Visyn &\nPhysical Practice"), 
                       values = c("black", "#3300CC", "#FFD700")) +
    theme_bw() + #sets the theme to plane
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 30), 
          axis.text = element_text(color = "black", size = 25), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 30),
          legend.title = element_blank()) +
    theme(legend.text = element_text(size = 20),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_blank()))

## Accuracy
(i <- visyn %>% ggplot(aes(accuracy, trial, group = condition)) + 
    geom_point(size = 5, shape = 16,
               aes(color = condition)) +
    scale_x_continuous(name = "Accuracy (inches)", 
                       limit = c(0,60), 
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
    scale_y_continuous(name = "Trial Number", 
                       breaks = c(1, 5, 10, 15, 20)) +
    labs(tag = "A") +
    scale_shape_manual(values = c(15,0,16,1,17,2)) + 
    scale_fill_manual(values = c("black", "black", "black", "black", "black", "black")) +
    scale_color_manual(labels = c("Baseline", "Physical\nPractice Only", "Visyn &\nPhysical Practice"), 
                       values = c("black", "#3300CC", "#FFD700")) +
    theme_bw() + #sets the theme to plane
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 30), 
          axis.text = element_text(color = "black", size = 25), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 30),
          legend.title = element_blank()) +
    theme(legend.text = element_text(size = 20),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_blank()))

#######################################
######## Smooth Density Plots #########
#######################################

## Release Time
(j <- visyn %>% ggplot(aes(movementTime, group = condition, fill = condition)) + 
   geom_density(alpha = 0.5) + 
   labs(y = "Density",
        tag = "B") +
   scale_x_continuous(name = "Movement Time (s)",
                      limit = c(0.2,1.1)) +
   scale_fill_manual(labels = c("Baseline", "Physical practice only", "Visyn & Physical practice"), 
                     values = c("black", "#3300CC", "#FFD700")) +
   theme_bw() +
   theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black")) +
   theme(axis.title.y = element_text(vjust = 2, size = 30),
         axis.title.x = element_text(vjust = 0, size = 30),
         axis.text = element_text(color = "black", size = 25)) +
   theme(legend.text = element_text(size = 25),
         legend.position = "none",
         legend.title = element_blank()))

## Velocity
(k <- visyn %>% ggplot(aes(velocity, group = condition, fill = condition)) + 
    geom_density(alpha = 0.5) + 
    labs(y = "Density",
         tag = "B") +
    scale_x_continuous(name = "Velocity (mph)",
                       limit = c(19, 56)) +
    scale_fill_manual(labels = c("Baseline", "Physical practice only", "Visyn & Physical practice"), 
                      values = c("black", "#3300CC", "#FFD700")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 30),
          axis.title.x = element_text(vjust = 0, size = 30),
          axis.text = element_text(color = "black", size = 25)) +
    theme(legend.text = element_text(size = 25),
          legend.position = "none",
          legend.title = element_blank()))

## Accuracy
(l <- visyn %>% ggplot(aes(accuracy, group = condition, fill = condition)) + 
    geom_density(alpha = 0.5) + 
    labs(y = "Density",
         tag = "B") +
    scale_x_continuous(name = "Accuracy (inches)",
                       limit = c(-15, 70)) +
    scale_fill_manual(labels = c("Baseline", "Physical practice only", "Visyn & Physical practice"), 
                      values = c("black", "#3300CC", "#FFD700")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 30),
          axis.title.x = element_text(vjust = 0, size = 30),
          axis.text = element_text(color = "black", size = 25)) +
    theme(legend.text = element_text(size = 25),
          legend.position = "none",
          legend.title = element_blank()))

########################################################################################
######## Combining Plots ###############################################################
########################################################################################

## Release Time (FIGURE 1)
legend <- get_legend(g) #creates the variable legend with the plot legends
g <- g + theme(legend.position = "none") #sets the plot back to having no legend
figureone <- grid.arrange(g, j, m, legend, ncol = 2, nrow = 3, #creates the layout of figures
                          layout_matrix = rbind(c(1,2), 
                                                c(1,3), 
                                                c(4)), #creates the matrix. Each number corresponds to the number in the plot
                          widths = c(3,3), heights = c(3, 3, 0.7))

### Saved size: 1700 x 1200

#legend <- get_legend(g) #creates the variable legend with the plot legends
#g <- g + theme(legend.position = "none") #sets the plot back to having no legend
#figureone <- grid.arrange(g, j, m, ncol = 2, nrow = 2, #creates the layout of figures
#                          layout_matrix = rbind(c(1,2), 
#                                                c(1, 3)), #creates the matrix. Each number corresponds to the number in the plot
#                          widths = c(6,4), heights = c(3, 3))

## Velocity (FIGURE 3)
legend <- get_legend(h) #creates the variable legend with the plot legends
h <- h + theme(legend.position = "none") #sets the plot back to having no legend
figurethree <- grid.arrange(h, k, n, legend, ncol = 2, nrow = 3, #creates the layout of figures
                            layout_matrix = rbind(c(1,2), 
                                                  c(1,3), 
                                                  c(4)), #creates the matrix. Each number corresponds to the number in the plot
                            widths = c(3,3), heights = c(3, 3, 0.7))

## Accuracy (FIGURE 5)
legend <- get_legend(i) #creates the variable legend with the plot legends
i <- i + theme(legend.position = "none") #sets the plot back to having no legend
figurefive <- grid.arrange(i, l, o, legend, ncol = 2, nrow = 3, #creates the layout of figures
                           layout_matrix = rbind(c(1,2), 
                                                 c(1,3), 
                                                 c(4)), #creates the matrix. Each number corresponds to the number in the plot
                           widths = c(3,3), heights = c(3, 3, 0.7))

########################################################################################
######## Tests for Outliers (BOXPLOTS) #################################################
########################################################################################

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
} #adds the outliers

## Release Time
(m <- visyn %>%
    filter(!is.na(movementTime)) %>% #filters out NANs
    mutate(outlier = ifelse(is_outlier(movementTime), movementTime, as.numeric(NA))) %>% #sets outliers
    ggplot(aes(x = condition, y = movementTime)) + 
    geom_boxplot(alpha = 0.8, 
                 color = "black", 
                 fill = c("black", "#3300CC", "#FFD700")) + 
    scale_y_continuous(name = "Movement Time (s)",
                       limits = (0:1.2),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4)) + 
    scale_x_discrete(labels = c("Baseline\n  \n  ", "Physical\nPractice Only\n ", "Visyn &\nPhysical\nPractice")) +
    labs(x = "",
         tags = "C") +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 30), 
          axis.text.x = element_text(color = "black", vjust = 0, size = 25), #element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 25),
          axis.title.x = element_text(color = "white", vjust = 0, size = 30)))

## Velocity
(n <- visyn %>%
    filter(!is.na(velocity)) %>% #filters out NANs
    mutate(outlier = ifelse(is_outlier(velocity), velocity, as.numeric(NA))) %>% #sets outliers
    ggplot(aes(x = condition, y = velocity)) + 
    geom_boxplot(alpha = 0.8, 
                 color = "black",
                 fill = c("black", "#3300CC", "#FFD700")) + 
    scale_y_continuous(name = "Velocity (mph)",
                       limits = c(20, 55),
                       breaks = c(20, 25, 30, 35, 40, 45, 50, 55)) + 
    scale_x_discrete(labels = c("Baseline\n  \n  ", "Physical\nPractice Only\n ", "Visyn &\nPhysical\nPractice")) +
    labs(x = "",
         tags = "C") +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 30), 
          axis.text.x = element_text(color = "black", vjust = 0, size = 25), #element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 25),
          axis.title.x = element_text(color = "white", vjust = 0, size = 30)))

## Accuracy
(o <- visyn %>%
    filter(!is.na(accuracy)) %>% #filters out NANs
    mutate(outlier = ifelse(is_outlier(accuracy), accuracy, as.numeric(NA))) %>% #sets outliers
    ggplot(aes(x = condition, y = accuracy)) + 
    geom_boxplot(alpha = 0.8, 
                 color = "black",
                 fill = c("black", "#3300CC", "#FFD700")) + 
    scale_y_continuous(name = "Accuracy (inches)",
                       limits = c(0, 45),
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)) + 
    scale_x_discrete(labels = c("Baseline\n  \n  ", "Physical\nPractice Only\n ", "Visyn &\nPhysical\nPractice")) +
    labs(x = "",
         tags = "C") +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 30), 
          axis.text.x = element_text(color = "black", vjust = 0, size = 25), #element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 25),
          axis.title.x = element_text(color = "white", vjust = 0, size = 30)))

########################################################################################
######## ORDER EFFECT ##################################################################
########################################################################################

## MovementTime
(MT <- visyn %>% ggplot(aes(x = week, y = movementTime, color = group)) + 
   stat_boxplot(geom = "errorbar", width = 0.2, size = 1.5) +
   stat_summary(fun.x = median, #creates a mean value for each group
                aes(group = group), 
                geom = "point", size = 4, #changes mean dot size,
                position = position_dodge(width = 0.2), #moves the median dot away from the midline
                stroke = 2) +  #stroke controls the boarder thickness
   stat_summary(fun.x = median, 
                geom = "line", size = 1.5, 
                aes(group = group), 
                position = position_dodge(width = 0.2)) +
   scale_color_manual(labels = c("Group 1", "Group 2"),
                      values = c("#00AFBB", "#E7B800")) +
   scale_y_continuous(name = "Movement Time (s)",
                      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), #adds tick marks for each value
                      limit = c(0,1.0)) + #set the min and max values for y-axis
   scale_shape_manual(values = c(16,17)) +
   scale_x_discrete(labels = c("Baseline", "Week 4", "Week 8")) +
   theme_bw() +
   theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black")) +
   theme(axis.title.y = element_text(vjust = 2, size = 20), 
         axis.text.x = element_text(color = "black", vjust = 0, size = 15), #element_text(color = "black", size = 15),
         axis.text.y = element_text(color = "black", size = 15),
         axis.title.x = element_text(color = "white", vjust = 0, size = 20)) + 
   theme(legend.title = element_blank()) + 
   guides(color = guide_legend(override.aes = list(size = 1.3))) +
   theme(legend.text = element_text(size = 20),
         legend.position = "right",
         legend.key.size = unit(1.5, "cm"),
         legend.title = element_blank()))

##Velocity
(VELO <- visyn %>% ggplot(aes(x = week, y = velocity, color = group)) + 
    stat_boxplot(geom = "errorbar", width = 0.2, size = 1.5) +
    stat_summary(fun.x = median, #creates a mean value for each group
                 aes(group = group), 
                 geom = "point", size = 4, #changes mean dot size,
                 position = position_dodge(width = 0.2), #moves the median dot away from the midline
                 stroke = 2) +  #stroke controls the boarder thickness
    stat_summary(fun.x = median, 
                 geom = "line", size = 1.5, 
                 aes(group = group), 
                 position = position_dodge(width = 0.2)) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_y_continuous(name = "Velocity (mph)",
                       limits = c(0, 53),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_shape_manual(values = c(16,17)) +
    scale_x_discrete(labels = c("Baseline", "Week 4", "Week 8")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 20), 
          axis.text.x = element_text(color = "black", vjust = 0, size = 15), #element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 15),
          axis.title.x = element_text(color = "white", vjust = 0, size = 20)) + 
    theme(legend.title = element_blank()) + 
    guides(color = guide_legend(override.aes = list(size = 1.3))) +
    theme(legend.text = element_text(size = 20),
          legend.position = "right",
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_blank())) 

##Accuracy
(ACC <- visyn %>% ggplot(aes(x = week, y = accuracy, color = group)) + 
    stat_boxplot(geom = "errorbar", width = 0.2, size = 1.5) +
    stat_summary(fun.x = median, #creates a mean value for each group
                 aes(group = group), 
                 geom = "point", size = 4, #changes mean dot size,
                 position = position_dodge(width = 0.2), #moves the median dot away from the midline
                 stroke = 2) +  #stroke controls the boarder thickness
    stat_summary(fun.x = median, 
                 geom = "line", size = 1.5, 
                 aes(group = group), 
                 position = position_dodge(width = 0.2)) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_y_continuous(name = "Accuracy (inches)",
                       limits = c(0, 45),
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)) + 
    scale_shape_manual(values = c(16,17)) +
    scale_x_discrete(labels = c("Baseline", "Week 4", "Week 8")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 20), 
          axis.text.x = element_text(color = "black", vjust = 0, size = 15), #element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 15),
          axis.title.x = element_text(color = "white", vjust = 0, size = 20)) + 
    theme(legend.title = element_blank()) + 
    guides(color = guide_legend(override.aes = list(size = 1.3))) +
    theme(legend.text = element_text(size = 20),
          legend.position = "right",
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_blank())) 


########################################################################################
######## Group 1 and Group 2 Correlations ##################################################################
########################################################################################
###########
## Baseline
###########
## Release Time & Velocity 

(aa <- visyn %>% filter(condition == "Baseline") %>% 
   ggplot(aes(movementTime, velocity)) +
   geom_point(aes(color = group), 
              size = 3) + #changes the size of the dots
   geom_smooth(aes(group = group, color = group), 
               method=lm, 
               se = FALSE) +
   scale_color_manual(labels = c("Group 1", "Group 2"),
                      values = c("#00AFBB", "#E7B800")) +
   scale_y_continuous(name = "Velocity at Baseline (mph)", #names y-axis
                      limit = c(0,50),
                      breaks = c(0, 10, 20, 30, 40, 50)) + 
   scale_x_continuous(name = "Movement Time at Baseline (s)",
                      limit = c(0,1.0),
                      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
   theme_bw() +
   theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"))+
   labs(tag = "A")+
   theme(axis.title.y = element_text(vjust = 2, size = 18), 
         axis.text = element_text(color = "black", size = 12), 
         axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
   theme(legend.title = element_blank()) + 
   theme(legend.text = element_text(size = 18),
         legend.position = "bottom",
         legend.key.size = unit(1.5, "cm"),
         legend.title = element_blank()))

## Velocity & Accuracy
#Group1
#fit <- 0.081
#rsq_labelG1 <- paste('R^2 == ', fit)
#Group2
#fit <- 
#rsq_labelG2 <- paste('R^2 == ', fit)

(bb <- visyn %>% filter(condition == "Baseline") %>% 
    ggplot(aes(velocity, accuracy)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(name = "Velocity at Baseline (mph)", #names y-axis
                       limit = c(0,50),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_y_continuous(name = "Accuracy at Baseline (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "B")+
  #  annotate(geom="text",x=22,y=17,label= rsq_labelG1, hjust = 0, vjust = 1, parse = TRUE) +
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.text = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank()))

## Movement Time and Accuracy
(cc <- visyn %>% filter(condition == "Baseline") %>% 
    ggplot(aes(accuracy, movementTime)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_y_continuous(name = "Movement Time at Baseline (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
    scale_x_continuous(name = "Accuracy at Baseline (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "C")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.text = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank()))

###########
## Physical Practice Only
###########
## Release Time & Velocity
#Group1
fit <- 0.179
rsq_labelG1 <- paste('R^2 == ', fit)
#Group2
#fit <- 
#rsq_labelG2 <- paste('R^2 == ', fit)

(dd <- visyn %>% filter(condition == "PO") %>% 
    ggplot(aes(movementTime, velocity)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_y_continuous(name = "Velocity after\nPhysical Practice Only (mph)", #names y-axis
                       limit = c(0,52),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_x_continuous(name = "Movement Time after\nPhysical Practice Only (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "A")+
    annotate(geom="text",x=.84,y=34,label= rsq_labelG1, hjust = 0, vjust = 1, parse = TRUE) +
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.title = element_blank()) + 
    theme(legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_blank()))

## Accuracy and Velocity
#Group1
#fit <- 0.094
#rsq_labelG1 <- paste('R^2 == ', fit)
#Group2
#fit <- 
#rsq_labelG2 <- paste('R^2 == ', fit)

(ee <- visyn %>% filter(condition == "PO") %>% 
    ggplot(aes(velocity, accuracy)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(name = "Velocity after\nPhysical Practice Only (mph)", #names y-axis
                       limit = c(0,50),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_y_continuous(name = "Accuracy after\nPhysical Practice Only (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "B")+
 #   annotate(geom="text",x=21,y=20,label= rsq_labelG1, hjust = 0, vjust = 1, parse = TRUE) +
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.text = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank()))

## Accuracy & Movement Time
(ff <- visyn %>% filter(condition == "PO") %>% 
    ggplot(aes(accuracy, movementTime)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_y_continuous(name = "Movement Time after\nPhysical Practice Only (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
    scale_x_continuous(name = "Accuracy after\nPhysical Practice Only (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "C")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.text = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank()))

##########
## Visyn & Physical Practice
##########
## Release Time & Velocity
fit <- 0.203
rsq_labelG1 <- paste('R^2 == ', fit)
#Group2
#fit <- 
#rsq_labelG2 <- paste('R^2 == ', fit)

(gg <- visyn %>% filter(condition == "VP") %>% 
    ggplot(aes(movementTime, velocity)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_y_continuous(name = "Velocity after\nVisyn & Physical Practice (mph)", #names y-axis
                       limit = c(0,52),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_x_continuous(name = "Movement Time after\nVisyn & Physical Practice (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "A")+
    annotate(geom="text",x=0.82,y=39,label= rsq_labelG1, hjust = 0, vjust = 1, parse = TRUE) +
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.title = element_blank()) + 
    theme(legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_blank()))


## Accuracy and Velocity
(hh <- visyn %>% filter(condition == "VP") %>% 
    ggplot(aes(velocity, accuracy)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(name = "Velocity after\nVisyn & Physical Practice (mph)", #names y-axis
                       limit = c(0,50),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_y_continuous(name = "Accuracy after\nVisyn & Physical Practice (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "B")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.text = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank()))

## Release Time & Accuracy
(ii <- visyn %>% filter(condition == "VP") %>% 
    ggplot(aes(accuracy, movementTime)) +
    geom_point(aes(color = group), 
               size = 3) + #changes the size of the dots
    geom_smooth(aes(group = group, color = group), 
                method=lm, 
                se = FALSE) +
    scale_color_manual(labels = c("Group 1", "Group 2"),
                       values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(labels = c("Group 1", "Group 2"),
                      values = c("#00AFBB", "#E7B800")) +
    
    scale_y_continuous(name = "Movement Time after\nVisyn & Physical Practice (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
    scale_x_continuous(name = "Accuracy after\nVisyn & Physical Practice (in)",
                       limit = c(0, 31),
                       breaks = c(0, 10, 20, 30)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "C")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)) +
    theme(legend.text = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank()))

###### Combining Correlation Plots #######

legend <- get_legend(aa) #creates the variable legend with the plot legends
aa <- aa + theme(legend.position = "none") #sets the plot back to having no legend
figureone <- grid.arrange(aa, bb, cc, legend, ncol = 3, nrow = 2, #creates the layout of figures
                          layout_matrix = rbind(c(1,2, 3), 
                                                c(NA, 4, NA)), #creates the matrix. Each number corresponds to the number in the plot
                          widths = c(3,3, 3), heights = c(3, 0.7))

legend <- get_legend(dd) #creates the variable legend with the plot legends
dd <- dd + theme(legend.position = "none") #sets the plot back to having no legend
figureone <- grid.arrange(dd, ee, ff, legend, ncol = 3, nrow = 2, #creates the layout of figures
                          layout_matrix = rbind(c(1,2, 3), 
                                                c(NA, 4, NA)), #creates the matrix. Each number corresponds to the number in the plot
                          widths = c(3,3, 3), heights = c(3, 0.7))

legend <- get_legend(gg) #creates the variable legend with the plot legends
gg <- gg + theme(legend.position = "none") #sets the plot back to having no legend
figureone <- grid.arrange(gg, hh, ii, legend, ncol = 3, nrow = 2, #creates the layout of figures
                          layout_matrix = rbind(c(1,2, 3), 
                                                c(NA, 4, NA)), #creates the matrix. Each number corresponds to the number in the plot
                          widths = c(3,3, 3), heights = c(3, 0.7))

#############################################################################
######## Pooled Correlations ####################################################################
#############################################################################

###########
## Baseline
###########
## Release Time & Velocity 

fit <- 0.144
rsq_label <- paste('R^2 == ', fit)

(jj <- visyn %>% filter(condition == "Baseline") %>% 
    ggplot(aes(movementTime, velocity)) +
    geom_point(size = 3,
               fill = "black",
               color = "black") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_y_continuous(name = "Velocity at Baseline (mph)", #names y-axis
                       limit = c(0,50),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_x_continuous(name = "Movement Time at Baseline (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "A")+
    annotate(geom="text",x=0.79,y=48,label= rsq_label, hjust = 0, vjust = 1, parse = TRUE) +
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

## Release Time & Accuracy
(kk <- visyn %>% filter(condition == "Baseline") %>% 
    ggplot(aes(velocity, accuracy)) +
    geom_point(size = 3,
               fill = "black",
               color = "black") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_x_continuous(name = "Velocity at Baseline (mph)", #names y-axis
                       limit = c(0,50),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_y_continuous(name = "Accuracy at Baseline (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "B")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

## Velocity and Accuracy
(ll <- visyn %>% filter(condition == "Baseline") %>% 
    ggplot(aes(accuracy, movementTime)) +
    geom_point(size = 3,
               fill = "black",
               color = "black") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_y_continuous(name = "Movement Time at Baseline (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
    scale_x_continuous(name = "Accuracy at Baseline (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "C")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

###########
## Physical Practice Only
###########
## Release Time & Velocity

fit <- 0.421
rsq_label <- paste('R^2 == ', fit)

(mm <- visyn %>% filter(condition == "PO") %>% 
    ggplot(aes(movementTime, velocity)) +
    geom_point(size = 3,
               fill = "#3300CC",
               color = "#3300CC") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_y_continuous(name = "Velocity after\nPhysical Practice Only (mph)", #names y-axis
                       limit = c(0,52),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_x_continuous(name = "Movement Time after\nPhysical Practice Only (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "A")+
    annotate(geom="text",x=.77,y=48,label= rsq_label, hjust = 0, vjust = 1, parse = TRUE) +
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

## Accuracy and Velocity
fit <- 0.082
rsq_label <- paste('R^2 == ', fit)

(nn <- visyn %>% filter(condition == "PO") %>% 
    ggplot(aes(velocity, accuracy)) +
    geom_point(size = 3,
               fill = "#3300CC",
               color = "#3300CC") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_x_continuous(name = "Velocity after\nPhysical Practice Only (mph)", #names y-axis
                       limit = c(0,50),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_y_continuous(name = "Accuracy after\nPhysical Practice Only (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "B")+
    annotate(geom="text",x=19,y=18,label= rsq_label, hjust = 0, vjust = 1, parse = TRUE) +
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

## Accuracy & Movement Time
(oo <- visyn %>% filter(condition == "PO") %>% 
    ggplot(aes(accuracy, movementTime)) +
    geom_point(size = 3,
               fill = "#3300CC",
               color = "#3300CC") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_y_continuous(name = "Movement Time after\nPhysical Practice Only (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
    scale_x_continuous(name = "Accuracy after\nPhysical Practice Only (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "C")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

##########
## Visyn & Physical Practice
##########
## Release Time & Velocity
(pp <- visyn %>% filter(condition == "VP") %>% 
   ggplot(aes(movementTime, velocity)) +
   geom_point(size = 3,
              fill = "#FFD700",
              color = "#FFD700") + #changes the size of the dots
   geom_smooth(method=lm, se = FALSE, color = "black") +
   scale_y_continuous(name = "Velocity after\nVisyn & Physical Practice (mph)", #names y-axis
                      limit = c(0,52),
                      breaks = c(0, 10, 20, 30, 40, 50)) + 
   scale_x_continuous(name = "Movement Time after\nVisyn & Physical Practice (s)",
                      limit = c(0,1.0),
                      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
   theme_bw() +
   theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"))+
   labs(tag = "A")+
   theme(axis.title.y = element_text(vjust = 2, size = 18), 
         axis.text = element_text(color = "black", size = 12), 
         axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

## Accuracy and Velocity
(qq <- visyn %>% filter(condition == "VP") %>% 
    ggplot(aes(velocity, accuracy)) +
    geom_point(size = 3,
               fill = "#FFD700",
               color = "#FFD700") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_x_continuous(name = "Velocity after\nVisyn & Physical Practice (mph)", #names y-axis
                       limit = c(0,50),
                       breaks = c(0, 10, 20, 30, 40, 50)) + 
    scale_y_continuous(name = "Accuracy after\nVisyn & Physical Practice (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "B")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

## Release Time & Accuracy
(rr <- visyn %>% filter(condition == "VP") %>% 
    ggplot(aes(accuracy, movementTime)) +
    geom_point(size = 3,
               fill = "#FFD700",
               color = "#FFD700") + #changes the size of the dots
    geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_y_continuous(name = "Movement Time after\nVisyn & Physical Practice (s)",
                       limit = c(0,1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
    scale_x_continuous(name = "Accuracy after\nVisyn & Physical Practice (in)",
                       limit = c(0, 40),
                       breaks = c(0, 10, 20, 30, 40)) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    labs(tag = "C")+
    theme(axis.title.y = element_text(vjust = 2, size = 18), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 18)))

grid.arrange(jj, kk, ll, ncol = 3, widths =c(3.5, 3.5, 3.5)) #baseline
grid.arrange(mm, nn, oo, ncol = 3, widths =c(3.5, 3.5, 3.5)) #PO
grid.arrange(pp, qq, rr, ncol = 3, widths =c(3.5, 3.5, 3.5)) #VP

########################################################################################
######## Group Data ####################################################################
########################################################################################
#################
### Boxplots ####
#################

stack <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Visyn\\Visyn MVP App Testing Data-C-Clean_Konczak edited.xlsx', sheet = 'stackmean') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
} #adds the outliers

## Reaction Time
(a <- stack %>%
    filter(!is.na(movementTime)) %>% #filters out NANs
    mutate(outlier = ifelse(is_outlier(movementTime), movementTime, as.numeric(NA))) %>% #sets outliers
    ggplot(aes(x = condition, y = movementTime)) + 
    geom_boxplot(alpha = 0.5, 
                 color = "black",
                 fill = c("black", "#3300CC", "#FFD700")) + 
    scale_y_continuous(name = "Release Time (s)",
                       limits = c(0, 1.0),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
    scale_x_discrete(labels = c("Baseline", "Physical\nPractice\nOnly", "Visyn &\nPhysical\nPractice")) +
    labs(x = "",
         tags = "A") +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 15), 
          axis.text.x = element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.x = element_text(color = "black", vjust = 0, size = 15)))

## Velocity
(b <- stack %>%
    filter(!is.na(velocity)) %>% #filters out NANs
    mutate(outlier = ifelse(is_outlier(velocity), velocity, as.numeric(NA))) %>% #sets outliers
    ggplot(aes(x = condition, y = velocity)) + 
    geom_boxplot(alpha = 0.5, 
                 color = "black",
                 fill = c("black", "#3300CC", "#FFD700")) + 
    scale_y_continuous(name = "Velocity at Release (mph)",
                       limits = c(30, 50),
                       breaks = c(30, 35, 40, 45, 50)) + 
    scale_x_discrete(labels = c("Baseline", "Physical\nPractice\nOnly", "Visyn &\nPhysical\nPractice")) +
    labs(x = "",
         tags = "B") +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 15), 
          axis.text.x = element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.x = element_text(color = "black", vjust = 0, size = 15)))

## Accuracy
(c <- stack %>%
    filter(!is.na(accuracy)) %>% #filters out NANs
    mutate(outlier = ifelse(is_outlier(accuracy), accuracy, as.numeric(NA))) %>% #sets outliers
    ggplot(aes(x = condition, y = accuracy)) + 
    geom_boxplot(alpha = 0.5, 
                 color = "black",
                 fill = c("black", "#3300CC", "#FFD700")) + 
    scale_y_continuous(name = "Distance from Target (inches)",
                       limits = c(0, 25),
                       breaks = c(0, 5, 10, 15, 20, 25)) + 
    scale_x_discrete(labels = c("Baseline", "Physical\nPractice\nOnly", "Visyn &\nPhysical\nPractice")) +
    labs(x = "",
         tags = "C") +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 15), 
          axis.text.x = element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.x = element_text(color = "black", vjust = 0, size = 15)))

grid.arrange(a, b, c, ncol = 3, nrow = 1, #creates the layout of figures
             widths = c(3,3,3), heights = c(3))

##################
### Normality ####
##################

### density plots

## Release Time
(d <- stack %>% ggplot(aes(movementTime, group = condition, fill = condition)) + 
   geom_density(alpha = 0.5) + 
   labs(y = "Density",
        tag = "A") +
   scale_x_continuous(name = "Release Time (s)",
                      limit = c(0.2 ,1.1)) +
   scale_fill_manual(values = c("black", "#3300CC", "#FFD700")) +
   theme_bw() +
   theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black")) +
   theme(axis.title.y = element_text(vjust = 2, size = 15),
         axis.title.x = element_text(vjust = 0, size = 15),
         axis.text = element_text(color = "black", size = 12)) +
   theme(legend.text = element_text(size = 12),
         legend.position = "none",
         legend.title = element_blank()))

## Velocity
(e <- stack %>% ggplot(aes(velocity, group = condition, fill = condition)) + 
    geom_density(alpha = 0.5) + 
    labs(y = "",
         tag = "B") +
    scale_x_continuous(name = "Velocity at Release (mph)",
                       limit = c(20, 60)) +
    scale_fill_manual(values = c("black", "#3300CC", "#FFD700")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 15),
          axis.title.x = element_text(vjust = 0, size = 15),
          axis.text = element_text(color = "black", size = 12)) +
    theme(legend.text = element_text(size = 12),
          legend.position = "none",
          legend.title = element_blank()))

## Accuracy
(f <- stack %>% ggplot(aes(accuracy, group = condition, fill = condition)) + 
    geom_density(alpha = 0.5) + 
    labs(y = "", 
         tag = "C") +
    scale_x_continuous(name = "Distance from Target (inches)",
                       limit = c(0, 30)) +
    scale_fill_manual(values = c("black", "#3300CC", "#FFD700")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(axis.title.y = element_text(vjust = 2, size = 15),
          axis.title.x = element_text(vjust = 0, size = 15),
          axis.text = element_text(color = "black", size = 12)) +
    theme(legend.text = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_blank()))

legend <- get_legend(f) #creates the variable legend with the plot legends

f <- f + theme(legend.position = "none") #sets the plot back to having no legend

grid.arrange(d, e, f, legend, ncol = 3, nrow = 2, #creates the layout of figures
             layout_matrix = rbind(c(1,2,3), c(NA,4,NA)), #creates the matrix. Each number corresponds to the number in the plot
             widths = c(3,3,3), heights = c(3, 0.4))