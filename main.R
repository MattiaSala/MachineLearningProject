install.packages("dplyr")
install.packages("forcats")
install.packages("ggplot2")
install.packages("skimr")
install.packages("scales")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("varImp")
install.packages("GGally")
install.packages(c("FactoMineR", "factoextra")) #da slide

library(dplyr)
library(forcats)
library(ggplot2)
library(skimr)
library(scales)
library(tidyverse)
library(ggpubr)
library(corrplot)
library(RColorBrewer)
library(varImp)
library(GGally)
library(FactoMineR) #da slide
library(factoextra) #da slide

water_potability <- read.csv("water_potability.csv")

water_potability$Potability <- as.factor(water_potability$Potability)
print(water_potability)

water_potability %>% summarise_all(~ sum(is.na(.)))

#Remove NA values and substitute them with mean
water_potability <- water_potability %>% 
  group_by(Potability) %>%
  mutate(across(where(is.numeric), 
                ~if_else(is.na(.), 
                         mean(., na.rm = T),   
                         as.numeric(.)))) %>% 
  ungroup()

#check if there is any NA value
water_potability %>% summarise_all(~ sum(is.na(.)))

#PCA
pca.res <- PCA(water_potability[1:9], ncp=6, graph = TRUE)

#eigenvalues
eig.val <- get_eigenvalue(pca.res)

fviz_eig(pca.res, addlabels = TRUE, ylim = c(0, 20)) #scree plot

var <- get_pca_var(pca.res)
head(var$coord, 6)

ind <- get_pca_ind(pca.res)