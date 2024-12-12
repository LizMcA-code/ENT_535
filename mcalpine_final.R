###################################################################################################
################################# TEMPERATURE ANALYSIS ############################################
###################################################################################################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(MASS)
setwd("C:/Users/ekm5556/OneDrive - The Pennsylvania State University/Desktop/PhD/R_script/SVF/")

#### 1. Analysis of Stand Temperature Differences ####

#### clean data 
# assign data 
two = read.csv("stats_two.csv") # stands from 2000 to 2010
ten = read.csv("stats_ten.csv") # stands from 2010 to 2020

# change column names because the ArcGIS imported ones are stupid
colnames(two)[6] = "treatment"
colnames(two)[18] = "LST_count"
colnames(two)[20] = "LST_min"
colnames(two)[21] = "LST_max"
colnames(two)[22] = "LST_range"
colnames(two)[23] = "LST_mean"
colnames(two)[24] = "LST_stdev"

colnames(ten)[6] = "treatment"
colnames(ten)[18] = "LST_count"
colnames(ten)[20] = "LST_min"
colnames(ten)[21] = "LST_max"
colnames(ten)[22] = "LST_range"
colnames(ten)[23] = "LST_mean"
colnames(ten)[24] = "LST_stdev"

#### Stats analysis 
### Question: is there a difference between treatment temperatures in stand types? 
## ANOVA assumptions 

# 1. observations are independent (assumed)

# 2. data are normally distributed 
hist(two$LST_mean) # these look abnormal but check QQ plot 
hist(ten$LST_mean)

ggplot(data=ten,aes(sample=LST_mean))+
  geom_qq()+
  geom_qq_line() # this is terrible. Don't want to remove outliers because we don't have good information on what is causing them. 
# Find a different test. 

ggplot(data=ten,aes(sample=LST_mean))+
  geom_qq()+
  geom_qq_line()

## non-parametric test for more than two samples - Kruskal-Wallis H Test
# rank my data
unique(two$treatment) # see how many different values there are 
two$treatment = ordered(two$treatment,
                      levels = c("Clearcut", "Cleanup", "Mow", "Salvage", "Thinning", 
                                 "Single Tree", "Seed Tree", "Shelterwood 1", "Shelterwood FR", 
                                 "Shelterwood Prep"))
unique(ten$treatment) 
ten$treatment = ordered(ten$treatment,
                        levels = c("Clearcut", "Habitat Improvement", "Salvage", "Thinning", 
                                   "Shelterwood FR"))


kruskal.test(LST_mean ~ treatment, data = two)
kruskal.test(LST_mean ~ treatment, data = ten) #yikes. 

## This is not significant at all but let's plot it anyway. 
# plot 2000 - 2010 data 
ggplot(data = two, aes(x = treatment, y = LST_mean)) +
  geom_boxplot()

# plot 2010 - 2024 data
ggplot(data = ten, aes(x = treatment, y = LST_mean)) +
  geom_boxplot()

# this doesn't really tell us much but let's make it fancy anyway
# 2000 - 2010
ggplot(data = two, aes(x = treatment, y = LST_mean, fill = treatment)) +
  geom_boxplot() +
  labs(
    title = "Land Surface Temperature by Stand Type",
    subtitle = "2000 - 2010",
    x = "Stand Treatment",
    y = "Land Surface Temperature (C)",
    fill = "Stand Treatment") +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5))
  
# 2010 - 2024
ggplot(data = ten, aes(x = treatment, y = LST_mean, fill = treatment)) +
  geom_boxplot() +
  labs(
    title = "Land Surface Temperature by Stand Type",
    subtitle = "2010 - 2024",
    x = "Stand Treatment",
    y = "Land Surface Temperature (C)",
    fill = "Stand Treatment") +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5))


#### 2. Analysis of Animal Locations ####

### clean data 
## read in data 
data = read.csv("june_sept_table.csv") # table of animal location 
view(data)

## add pixel values for the control temperatures 
# hard coding this but there has GOT to be a faster way to do this 
control = data.frame(Group = rep("Control", 86716)) #create a bunch of entries for the control group
temperature_values = data.frame(Group = c(rep(20, 247), rep(21, 6189), rep(22, 28866), rep(23, 25222),
                        rep(24, 7277), rep(25, 5002), rep(26, 4000), rep(27, 3151), rep(28, 2618),
                        rep(29, 1884), rep(30, 1146), rep(31, 499), rep(32, 357), rep(33, 247), rep(34, 11))) #add values for each entry 

new = cbind(control, temperature_values) # bind new dataframes together 
colnames(new) = c("treatment", "mean_temp") #name columns 
# Combine the original data with the new rows
data = rbind(data, new)
#subset just June data 
jun = data %>% filter(treatment %in% c("June", "Control"))
# change to numeric class
jun$mean_temp = as.integer(jun$mean_temp)


### Stats analysis 

# visualize the data 
ggplot(data = jun, aes(x = treatment, y = mean_temp)) +
  geom_boxplot()
# see if it's normally distributed 
hist(new$mean_temp)
jun_sub = jun %>% filter(treatment == "June") #subset only June data to look at distribution
hist(jun_sub$mean_temp)
# look at qq values 
ggplot(data=jun_sub,aes(sample=mean_temp))+
  geom_qq()+
  geom_qq_line() # this looks terrible so we'll try a non-parametric test 

# Mann-Whitney U test 
wilcox.test(mean_temp~treatment, data = jun, exact = FALSE) # results here are slightly significant. 

# updated boxplot to show variation in treatments 
ggplot(data = jun, aes(x = treatment, y = mean_temp, fill = treatment)) +
  geom_boxplot() +
  labs(
    title = "Land Surface Temperature",
    subtitle = "LST at Animal and Control Locations",
    x = "Treatment",
    y = "Land Surface Temperature (C)") +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    axis.text.x=element_blank()) +
  scale_fill_discrete(name = "Treatment", labels = c("Control Points", "Animal Locations"))

# Numbers 
st1 =sd(jun_sub$mean_temp) # standard deviation in animal locations 
st2 =sd(new$mean_temp) # standard deviation in control points 
mn1 = mean(jun_sub$mean_temp) # mean in animal locations 
mn2 = mean(new$mean_temp) #mean in control locations 

st1
st2
mn1
mn2
