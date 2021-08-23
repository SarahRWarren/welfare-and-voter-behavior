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
ex2 <- lm(PEOPSAY ~ aid_dummy + PTYIDd + aid_dummy*PTYIDd + white + EDUC + sex + 
            FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub)
ex3 <- lm(PEOPSAY ~ Total + PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF,
               weights = allwt, data =  max_sub)

ex4 <- lm(PEOPSAY ~ total_uni + PTYIDd + 
      white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)

ex5 <- lm(PEOPSAY ~ total_loans + PTYIDd + 
      white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)

ex6 <- lm(PEOPSAY ~  total_ent + PTYIDd + 
      white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)

ex7 <- lm(PEOPSAY ~  total_means + PTYIDd + 
      white + EDUC + sex + FOLLOWPA + TRUSTOFF,
   weights = allwt, data =  max_sub)

stargazer(ex1, ex2, ex3, ex4, ex5, ex6, ex7,
          title="Effect of Aid on Self-Efficacy", type="latex", style = "apsr",
          align=TRUE, out="Tables/ext.tex")

ext9 <- lm(PEOPSAY ~ white + EDUC + sex + PTYIDd +
              FOLLOWPA + TRUSTOFF  + FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
              WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
              HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
              GIBILLd + MTGDEDTd,
           data =  max_sub)

stargazer(ext9,
          title="Effect of Aid on Self-Efficacy", type="latex", style = "apsr",
          align=TRUE, out="Tables/ext2.tex")

tex1 <- glm(PEOPSAY ~ aid_dummy + PTYIDd + white + EDUC + sex + 
             FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub, family="binomial")
tex2 <- glm(PEOPSAY ~ aid_dummy + PTYIDd + aid_dummy*PTYIDd + white + EDUC + sex + 
             FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub, family="binomial")
tex3 <- glm(PEOPSAY ~ Total + PTYIDd + white + EDUC + sex + 
             FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub, family="binomial")

tex4 <- glm(PEOPSAY ~ total_uni + PTYIDd + 
             white + EDUC + sex + FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub, family="binomial")

tex5 <- glm(PEOPSAY ~ total_loans + PTYIDd + 
             white + EDUC + sex + FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub, family="binomial")

tex6 <- glm(PEOPSAY ~  total_ent + PTYIDd + 
             white + EDUC + sex + FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub, family="binomial")

tex7 <- glm(PEOPSAY ~  total_means + PTYIDd + 
             white + EDUC + sex + FOLLOWPA + TRUSTOFF,
          weights = allwt, data =  max_sub, family="binomial")

stargazer(tex1, tex2, tex3, tex4, tex5, tex6, tex7,
          title="Effect of Aid on Self-Efficacy", type="latex", style = "apsr",
          align=TRUE, out="Tables/tab2.tex")
