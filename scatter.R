setwd() ## set directory
getwd() ## get the directory
Sys.setenv(LANG = "en") ## set the rstudio error to englishh

## install.packages("ggplot2")
library(ggplot2)

read.csv("area.csv")

#######l_d_h############
l_d_h_icno <- ggplot(read.csv("area.csv"), 
                     aes(group = lesion_type, x = length, y = height)) + 
      geom_point(aes(color = factor(lesion_type)))

l_d_h_icno + geom_smooth(method=lm, aes(color = factor(lesion_type)))
l_d_h_icno
