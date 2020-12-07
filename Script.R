## Script author: Gabriel E. Baréa de Barros
## Git: https://github.com/gabrielbarea

## Scripts in 'Damaged armor: ichnotaxonomy and paleoparasitology of bioerosion lesions in
## osteoderms of Quaternary extinct armadillos'

## Scripts and data used for statistical analysis and graphic production of the paper.

## Script.R: Script in R with the functions that were used in the paper

## data.xlsx: Data with modified header for reading in R on two different sheets: data = 
##   data used to create the graphs; stat = data used in statistical analysis and density 
##   graphs

#### Header ####
setwd() ## set directory
getwd() ## get the directory
Sys.setenv(LANG = "en") ## set the Rstudio error to English

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(readxl)) install.packages("readxl")
if(!require(multimode)) install.packages("multimode")
if(!require(moments)) install.packages("moments")

library(ggplot2)  ## Graphics production
library(readxl)   ## Read .xlsx files
library(multimode)## Multimodal analysis 
library(moments)  ## Identify data skewness

#### Reading the Data ####
data_ichnos <- read_excel("data.xlsx", trim_ws = FALSE, sheet = "Data") ## Read data sheet
stat_ichnos <- read_excel("data.xlsx", trim_ws = FALSE, sheet = "Stat") ## Read stat sheet


#### Figure 3a - Boxplot of Area ####
ichnos_box <- ggplot(data = data_ichnos, mapping = 
                       aes(group = lesion_type, y = lesion_area, 
                           x = lesion_type)) +
      geom_boxplot(fill='#A4A4A4', color="black") + 
      theme_bw() + scale_y_log10() + 
      scale_x_discrete(limits = c("M", "V", "U")) +
      geom_jitter(position=position_jitter(0.2))
ichnos_box

#### Figure 3b - Boxplot of Lesion percentage ####
lesion_box <- ggplot(data = data_ichnos, mapping = 
                        aes(group = lesion_type, y = lesion_percentage, 
                            x = lesion_type)) +
   geom_boxplot(outlier.alpha = 1, fill='#A4A4A4', color="black") + 
   theme_bw() + scale_y_log10() + 
   scale_x_discrete(limits = c("M", "V", "U")) +
   geom_jitter(shape=16, position=position_jitter(0.2), alpha = 1)
lesion_box

#### Figure 3c - Boxplot of Eccentricity ####
lw_box <- ggplot(data = data_ichnos, mapping = 
                        aes(y = lesion_lw, 
                            x = lesion_type)) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() +
   geom_jitter(position=position_jitter(0.2))
lw_box

#### Figure 3d - Boxplot of Lesion type/Osteoderm ####
ost_box <- ggplot(data = data_ichnos, mapping = 
                           aes(y = lesion_percentage, x = ost_type)) +
   geom_point(aes(color = factor(lesion_type))) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
      theme_bw() + scale_y_log10() + 
      geom_jitter(aes(color = factor(lesion_type)))
ost_box


#### Supplementary Material Figure S1 - Density Plots####

## Lesion area
area_lesion <- ggplot(data = stat_ichnos, aes(y = lesion_t, x = lesion_a)) + 
   geom_density_ridges() + scale_x_log10() +
   theme_ridges()
area_lesion

# Lesion percentage
lesion_perc <- ggplot(data = stat_ichnos, aes(y = lesion_t, x = lesion_p)) + 
   geom_density_ridges() + scale_x_log10() +
   theme_ridges()
lesion_perc

## Minicirculichnus area
t1_area_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t1_area)) + 
   geom_density() + 
   theme_bw()
t1_area_plot

## Violinichnus area
t2_area_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t2_area)) + 
   geom_density() + 
   theme_bw()
t2_area_plot

## Ulcerative Lesions area
t3_area_plot <- ggplot(data = stat_ichnos, 
                       aes(x = t3_area)) + 
   geom_density() + 
   theme_bw()
t3_area_plot

## Minicirculichnus percentage area
t1_percent_plot <- ggplot(data = stat_ichnos, 
                          aes(x = t1_percent)) + 
   geom_density() + 
   theme_bw()
t1_percent_plot

## Violinichnus percentage area
t2_percent_plot <- ggplot(data = stat_ichnos, 
                          aes(x = t2_percent)) + 
   geom_density() + 
   theme_bw()
t2_percent_plot

## Ulcerative Lesion percentage area
t3_percent_plot <- ggplot(data = stat_ichnos, 
                          aes(x = t3_percent)) + 
   geom_density() + 
   theme_bw()
t3_percent_plot

## Minicirculichnus Length
t1_len_plot <- ggplot(data = stat_ichnos, 
                      aes(x = t1_len)) + 
   geom_density() + 
   theme_bw()
t1_len_plot

## Minicirculichnus Width
t1_wid_plot <- ggplot(data = stat_ichnos, 
                      aes(x = t1_wid)) + 
   geom_density() + 
   theme_bw()
t1_wid_plot

## Minicirculichnus Eccentricity
t1_ecc_plot <- ggplot(data = stat_ichnos, 
                      aes(x = t1_ecc)) + 
   geom_density() + 
   theme_bw()
t1_ecc_plot


#### Supplementary Material Table S4 - Shapiro-Wilks ####
shapiro.test(stat_ichnos$t1_area)      ## Minicirculichnus area 
shapiro.test(stat_ichnos$t2_area)      ## Violinichnus area 
shapiro.test(stat_ichnos$t3_area)      ## Ulcerative Lesions area 
shapiro.test(stat_ichnos$t1_percent)   ## Minicirculichnus percentage area 
shapiro.test(stat_ichnos$t2_percent)   ## Violinichnus percentage area 
shapiro.test(stat_ichnos$t3_percent)   ## Ulcerative Lesions percentage area 
shapiro.test(stat_ichnos$t1_len)       ## Minicirculichnus Length 
shapiro.test(stat_ichnos$t1_wid)       ## Minicirculichnus Width 
shapiro.test(stat_ichnos$t1_ecc)       ## Minicirculichnus Eccentricity 

#### Supplementary Material Table S4 - ACR test ####
modetest(stat_ichnos$t1_area)    ## Minicirculichnus area
modetest(stat_ichnos$t2_area)    ## Violinichnus area
modetest(stat_ichnos$t3_area)    ## Ulcerative Lesions area
modetest(stat_ichnos$t1_percent) ## Minicirculichnus percentage area
modetest(stat_ichnos$t2_percent) ## Violinichnus percentage area
modetest(stat_ichnos$t3_percent) ## Ulcerative Lesions percentage area
modetest(stat_ichnos$t1_len)     ## Minicirculichnus Length
modetest(stat_ichnos$t1_wid)     ## Minicirculichnus Width
modetest(stat_ichnos$t1_ecc)     ## Minicirculichnus Eccentricity

#### Supplementary Material Table S4 - D'Agostino ####
agostino.test(stat_ichnos$t1_area)     ## Minicirculichnus area
agostino.test(stat_ichnos$t2_area)     ## Violinichnus area
agostino.test(stat_ichnos$t3_area)     ## Ulcerative Lesions area
agostino.test(stat_ichnos$t1_percent)  ## Minicirculichnus percentage area
agostino.test(stat_ichnos$t2_percent)  ## Violinichnus percentage area
agostino.test(stat_ichnos$t3_percent)  ## Ulcerative Lesions percentage area
agostino.test(stat_ichnos$t1_len)      ## Minicirculichnus Length
agostino.test(stat_ichnos$t1_wid)      ## Minicirculichnus Width
agostino.test(stat_ichnos$t1_ecc)      ## Minicirculichnus Eccentricity


#### Supplementary Material Table S5 - Kruskal-Wallis ####
## Area
kruskal.test(stat_ichnos$lesion_a, stat_ichnos$lesion_t, data = stat_ichnos)
pairwise.wilcox.test(stat_ichnos$lesion_a, stat_ichnos$lesion_t, p.adjust.method = "BH")

## Area Percentage
kruskal.test(stat_ichnos$lesion_p, stat_ichnos$lesion_t, data = stat_ichnos)
pairwise.wilcox.test(stat_ichnos$lesion_p, stat_ichnos$lesion_t, p.adjust.method = "BH")

#### Supplementary Material Table S5 - Mann Whitney Wilcoxon ####
## Area
## Minicirculichnus x Violinichnus
wilcox.test(x = stat_ichnos$t1_area, y = stat_ichnos$t2_area)

## Minicirculichnus x Ulcerative Lesions
wilcox.test(x = stat_ichnos$t1_area, y = stat_ichnos$t3_area)

## Violinichnus x Ulcerative Lesions
wilcox.test(x = stat_ichnos$t2_area, y = stat_ichnos$t3_area)


## Area Percentage
## Minicirculichnus x Violinichnus
wilcox.test(x = stat_ichnos$t1_percent, y = stat_ichnos$t2_percent)

## Minicirculichnus x Ulcerative Lesions
wilcox.test(x = stat_ichnos$t1_percent, y = stat_ichnos$t3_percent)

## Violinichnus x Ulcerative Lesions
wilcox.test(x = stat_ichnos$t2_percent, y = stat_ichnos$t3_percent)


#### Paper Information ####
## Damaged armor: ichnotaxonomy and paleoparasitology of bioerosion lesions in osteoderms 
## of Quaternary extinct armadillos

## Moura, J.F. 1; Nascimento, C.S.I. 1; Peixoto, B.C.P.M. 1; de Barros, G.E.B. 2, 
## Robbi, B. 3; Fernandes, M. A. 1

## 1 Programa de Pós-Graduação em Ecologia e Recursos Naturais, Laboratório de 
##   Paleoecologia e Paleoicnologia, Departamento de Ecologia e Biologia Evolutiva, 
##   Universidade Federal de São Carlos, Rod. Washington Luís, KM 235, São Carlos, SP, 
##   Brasil. mouradejesus@gmail.com; carolina.staisabel@gmail.com; b.peixoto@protonmail.com; 
##   mafernandes@ufscar.com

## 2 Programa de Pós-Graduação em Biologia Comparada, Departamento de Biologia, Faculdade 
##   de Filosofia, Ciências e Letras de Ribeirão Preto (FFCLRP), Universidade de São Paulo 
##   (USP), Av. Bandeirantes, 3900, Monte Alegre, Ribeirão Preto, SP, Brasil. 
##   gbareabarros@gmail.com;

## 3 Programa de Pós-Graduação em Conservação da Fauna, Universidade Federal de São Carlos
##   Rod. Washington Luís, km 235, São Carlos, SP, Brasil. bia_robbi_93@gmail.com.
