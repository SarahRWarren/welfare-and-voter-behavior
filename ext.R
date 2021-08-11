library(tidyverse)
library(readxl)
library(survey)
library(stargazer)
library(ggpubr)
library(MASS)

max_sub <- read_csv("Data/max_sub.csv")

ex1 <- lm(PEOPSAY ~ aid_dummy + PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF,
               weights = allwt, data =  max_sub)
ex4 <- lm(PEOPSAY ~ aid_dummy + PTYIDd + aid_dummy*PTYIDd + white + EDUC + sex + 
            FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub)
ex2 <- lm(PEOPSAY ~ total_loans + total_uni + total_ent + total_means + PTYIDd + 
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF,
               weights = allwt, data =  max_sub)
ex3 <- lm(PEOPSAY ~ Total + PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF,
               weights = allwt, data =  max_sub)

stargazer(ex1, ex4, ex2, ex3,
          title="Effect of Aid on Self-Efficacy", type="latex", style = "apsr",
          align=TRUE, out="Tables/ext.tex")

lm(PEOPSAY ~ total_uni + PTYIDd + 
     white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)

lm(PEOPSAY ~ total_loans + PTYIDd + 
     white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)

lm(PEOPSAY ~  total_ent + PTYIDd + 
     white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)

lm(PEOPSAY ~  total_means + PTYIDd + 
     white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)