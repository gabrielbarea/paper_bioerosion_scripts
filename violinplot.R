setwd() ## set directory
getwd() ## get the directory
Sys.setenv(LANG = "en") ## set the rstudio error to english

## install.packages("ggplot2")
library(ggplot2)

read.csv("area.csv")

#######area############
area_icno<- ggplot(read.csv("area.csv"), 
                   aes(group = lesion_type, x = lesion_type, 
                       y = area, color = factor(lesion_type))) + 
      geom_violin()
area_icno
area_icno + geom_jitter(shape=16, position=position_jitter(0.2))
