#### Header ####

setwd() ## set directory
getwd() ## get the directory
Sys.setenv(LANG = "en") ## set the rstudio error to english

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(readxl)) install.packages("readxl")

library(ggplot2)
library(readxl)

#### Reading the Data ####

data_ichnos <- read_excel("data.xlsx", trim_ws = FALSE, sheet = "Data")
stat_ichnos <- read_excel("data.xlsx", trim_ws = FALSE, sheet = "Stat")

#### Boxplot area ####

ichnos_box <- ggplot(data = data_ichnos, mapping = 
                       aes(group = lesion_type, y = lesion_area, 
                           x = lesion_type)) +
      geom_boxplot(fill='#A4A4A4', color="black") + 
      theme_bw() + scale_y_log10() + 
      scale_x_discrete(limits = c("1", "2", "3")) +
      geom_jitter(position=position_jitter(0.2))
ichnos_box

#### Circular or Eliptical ####

lw_box <- ggplot(data = data_ichnos, mapping = 
                        aes(y = lesion_lw, 
                            x = lesion_type)) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() +
   geom_jitter(position=position_jitter(0.2))
lw_box

#### Lesion type - Osteoderm ####

ost_box <- ggplot(data = data_ichnos, mapping = 
                           aes(y = lesion_percentage, x = ost_type)) +
   geom_point(aes(color = factor(lesion_type))) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
      theme_bw() + scale_y_log10() + 
      geom_jitter(aes(color = factor(lesion_type)))
   
ost_box

#### Lesion percentage ####

lesion_box <- ggplot(data = data_ichnos, mapping = 
                           aes(group = lesion_type, y = lesion_percentage, 
                               x = lesion_type)) +
      geom_boxplot(outlier.alpha = 1, fill='#A4A4A4', color="black") + 
      theme_bw() + scale_y_log10() + 
      scale_x_discrete(limits = c("1", "2", "3")) +
      geom_jitter(shape=16, position=position_jitter(0.2), alpha = 1)
lesion_box

#### T test ####
## Area

t.test(x = stat_ichnos$t1_area, y = stat_ichnos$t2_area)
t.test(x = stat_ichnos$t1_area, y = stat_ichnos$t3_area)
t.test(x = stat_ichnos$t2_area, y = stat_ichnos$t3_area)

## Area Percentage

t.test(x = stat_ichnos$t1_percent, y = stat_ichnos$t2_percent)
t.test(x = stat_ichnos$t1_percent, y = stat_ichnos$t3_percent)
t.test(x = stat_ichnos$t2_percent, y = stat_ichnos$t3_percent)

#### Mann Whitney Wilcoxon test ####
## Area

wilcox.test(x = stat_ichnos$t1_area, y = stat_ichnos$t2_area)
wilcox.test(x = stat_ichnos$t1_area, y = stat_ichnos$t3_area)
wilcox.test(x = stat_ichnos$t2_area, y = stat_ichnos$t3_area)

## Area Percentage

wilcox.test(x = stat_ichnos$t1_percent, y = stat_ichnos$t2_percent)
wilcox.test(x = stat_ichnos$t1_percent, y = stat_ichnos$t3_percent)
wilcox.test(x = stat_ichnos$t2_percent, y = stat_ichnos$t3_percent)


#### Density ####
t1_area_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t1_area)) + 
   geom_density() + 
   theme_bw()
t1_area_plot

t2_area_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t2_area)) + 
   geom_density() + 
   theme_bw()
t2_area_plot

t3_area_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t3_area)) + 
   geom_density() + 
   theme_bw()
t3_area_plot

t1_percent_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t1_percent)) + 
   geom_density() + 
   theme_bw()
t1_percent_plot

t2_percent_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t2_percent)) + 
   geom_density() + 
   theme_bw()
t2_percent_plot

t3_percent_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t3_percent)) + 
   geom_density() + 
   theme_bw()
t3_percent_plot

t1_len_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t1_len)) + 
   geom_density() + 
   theme_bw()
t1_len_plot

t1_wid_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t1_wid)) + 
   geom_density() + 
   theme_bw()
t1_wid_plot

t1_ecc_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t1_ecc)) + 
   geom_density() + 
   theme_bw()
t1_ecc_plot



