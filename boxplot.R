setwd() #set directory
getwd() #get the directory
Sys.setenv(LANG = "en") #set the rstudio error to english

## install.packages("ggplot2")
library(ggplot2)

read.csv("area.csv")

#######length#########
length_icno<- ggplot(read.csv("area.csv"), 
                   aes(group = lesion_type, x = lesion_type, y = length)) + 
      geom_boxplot(width = .2, coef = 1000, position = position_nudge(.2), color = "grey") +
      geom_jitter(width = .05,alpha = .4,size = 1,color = "brown")
length_icno

#######height############
height_icno<- ggplot(read.csv("area.csv"), 
                     aes(group = lesion_type, x = lesion_type, y = height)) + 
      geom_boxplot(width = .2, coef = 1000, position = position_nudge(.2), color = "grey") +
      geom_jitter(width = .05,alpha = .4,size = 1,color = "brown")
height_icno

#######lxH############
LxH_icno<- ggplot(read.csv("area.csv"), 
                     aes(group = lesion_type, x = lesion_type, y = LxH)) + 
      geom_boxplot(width = .2, coef = 1000, position = position_nudge(.2), color = "grey") +
      geom_jitter(width = .05,alpha = .4,size = 1,color = "brown")
LxH_icno

#######area############
area_icno<- ggplot(read.csv("area.csv"), 
                  aes(group = lesion_type, x = lesion_type, y = area)) + 
      geom_boxplot(width = .2, coef = 1000, position = position_nudge(.2), color = "grey") +
      geom_jitter(width = .05,alpha = .4,size = 1,color = "brown")
area_icno

#######perimeter############
perimeter_icno<- ggplot(read.csv("area.csv"), 
                   aes(group = lesion_type, x = lesion_type, y = perimeter)) + 
      geom_boxplot(width = .2, coef = 1000, position = position_nudge(.2), color = "grey") +
      geom_jitter(width = .05,alpha = .4,size = 1,color = "brown")
perimeter_icno

#######percentage area############
percentage_area_icno<- ggplot(read.csv("area.csv"), 
                        aes(group = lesion_type, x = lesion_type, y = percentage_area)) + 
      geom_boxplot(width = .2, coef = 1000, position = position_nudge(.2), color = "grey") +
      geom_jitter(width = .05,alpha = .4,size = 1,color = "brown")
percentage_area_icno

#######ost_type############
ost_type_percentage<- ggplot(read.csv("area.csv"), 
                              aes(group = osteo_type, x = osteo_type, y = percentage_area)) + 
      geom_boxplot(width = .2, coef = 1000, position = position_nudge(.2), color = "grey") +
      geom_jitter(width = .05,alpha = .4,size = 1,color = "brown")
ost_type_percentage

