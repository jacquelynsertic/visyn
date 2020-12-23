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
visyn <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Visyn\\Visyn MVP App Testing Data-C-Clean_Konczak edited.xlsx', sheet = 'Data') #load excel sheet into R

#### Scatterplots to compare individual data

#(p <- filter(visyn, condition == "Baseline"))

########################################################################################
######## Individual Data ###############################################################
########################################################################################

### Overall Data
### condition as color; subject as shape

## Release Time
visyn %>% ggplot(aes(movementTime, trial, group = condition)) + 
    geom_point(size = 4, 
               aes(shape = subject, color = condition)) +
  scale_x_continuous(name = "Release Time (s)", 
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
  scale_x_continuous(name = "Release Time (s)", 
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
  scale_x_continuous(name = "Velocity at Release (mph)", 
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
  scale_x_continuous(name = "Distance from Target (inches)", 
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
  geom_density(#color = "condition",
               #fill = "#800000",
               alpha = 0.5) + 
  labs(y = "Density",
       tag = "B") +
  scale_x_continuous(name = "Release Time (s)",
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
  geom_density(#color = "condition",
    #fill = "#800000",
    alpha = 0.5) + 
  labs(y = "Density",
       tag = "B") +
  scale_x_continuous(name = "Velocity at Release (mph)",
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
  scale_x_continuous(name = "Distance from Target (inches)",
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
    scale_y_continuous(name = "Release Time (s)",
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
  scale_y_continuous(name = "Velocity at\nRelease (mph)",
                     limits = c(0, 55),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) + 
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
  scale_y_continuous(name = "Distance from\nTarget (inches)",
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