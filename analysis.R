library(tidyverse)
library(readxl)
library(survey)
library(stargazer)
library(ggpubr)
library(MASS)
library(sjPlot)


max_sub <- read_csv("Data/max_sub.csv")



#DV: VOTE
#TABLE
#Model 1: Vote ~ all programs + party ID
#Model 2: Vote ~ all programs + controls (race, income, education, sex, followpa, 
              #trustoff, peopsay)
#Model 3: Vote ~ recieves aid dummy**partyID + controls

#TABLE
#Model 4: Vote ~ count of loans + count of entitlements + county of means + 
              #count of universal + party ID + controls
#Model 5: Vote ~ controls + count of loans**party ID
#Model 6: Vote ~ controls + count of entitlements**party ID
#Model 7: Vote ~ controls + count of universal**party ID
#Model 8: Vote ~ controls + count of means**party ID

#OLS unweighted
#Model 1: Vote ~ all programs + party ID
ols_uw_1 <- lm(VOTE ~ PTYIDd +
                FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
                WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
                HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
                GIBILLd + MTGDEDTd,
              data =  max_sub)
#Model 2: Vote ~ all programs + controls (race, income, education, sex, followpa, 
#trustoff, peopsay)
ols_uw_2 <- lm(VOTE ~ white + EDUC + sex + PTYIDd +
                FOLLOWPA + TRUSTOFF + PEOPSAY + FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
                WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
                HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
                GIBILLd + MTGDEDTd,
              data =  max_sub)
#Model 3: Vote ~ recieves aid dummy**partyID + controls
ols_uw_3 <- lm(VOTE ~ aid_dummy + PTYIDd + aid_dummy*PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF + PEOPSAY,
               data =  max_sub)
#Model 4: Vote ~ count of loans + count of entitlements + county of means + 
#count of universal + party ID + controls
ols_uw_4 <- lm(VOTE ~ total_loans + total_uni + total_ent + total_means + PTYIDd + 
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               data =  max_sub)
#Model 5: Vote ~ controls + count of loans**party ID
ols_uw_5 <- lm(VOTE ~ total_loans + PTYIDd + total_loans*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               data =  max_sub)
#Model 6: Vote ~ controls + count of entitlements**party ID
ols_uw_6 <- lm(VOTE ~ total_ent + PTYIDd + total_ent*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               data =  max_sub)
#Model 7: Vote ~ controls + count of universal**party ID
ols_uw_7 <- lm(VOTE ~ total_uni + PTYIDd + total_uni*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               data =  max_sub)
#Model 8: Vote ~ controls + count of means**party ID
ols_uw_8 <- lm(VOTE ~ total_means + PTYIDd + total_means*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               data =  max_sub)


#OLS weighted
#Model 1: Vote ~ all programs + party ID
ols_wt_1 <- lm(VOTE ~ PTYIDd +
                 FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
                 WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
                 HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
                 GIBILLd + MTGDEDTd,
               weights = allwt, data =  max_sub)
#Model 2: Vote ~ all programs + controls (race, income, education, sex, followpa, 
#trustoff, peopsay)
ols_wt_2 <- lm(VOTE ~ PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY + FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
                 WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
                 HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
                 GIBILLd + MTGDEDTd,
               weights = allwt, data =  max_sub)
#Model 3: Vote ~ recieves aid dummy**partyID + controls
ols_wt_3 <- lm(VOTE ~ aid_dummy + PTYIDd + aid_dummy*PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)
#Model 4: Vote ~ count of loans + count of entitlements + county of means + 
#count of universal + party ID + controls
ols_wt_4 <- lm(VOTE ~ total_loans + total_uni + total_ent + total_means + PTYIDd + 
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)
#Model 5: Vote ~ controls + count of loans**party ID
ols_wt_5 <- lm(VOTE ~ total_loans + total_uni + total_ent + total_means + PTYIDd + total_loans*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)
#Model 6: Vote ~ controls + count of entitlements**party ID
ols_wt_6 <- lm(VOTE ~ total_loans + total_uni + total_ent + total_means + PTYIDd + total_ent*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)
#Model 7: Vote ~ controls + count of universal**party ID
ols_wt_7 <- lm(VOTE ~ total_loans + total_uni + total_ent + total_means + PTYIDd + total_uni*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)
#Model 8: Vote ~ controls + count of means**party ID
ols_wt_8 <- lm(VOTE ~ total_loans + total_uni + total_ent + total_means + PTYIDd + total_means*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)

ols_wt_9 <- lm(VOTE ~ Total + PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)

ols_wt_10 <- lm(VOTE ~ Total + PTYIDd + Total*PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub)



##ROBUST to: Dichotomous measure of Turnout
##high/low turnout groups
rob1 <- max_sub %>%
  mutate(VOTE_D = as.numeric(VOTE),
         VOTE_D = recode(VOTE,
                        "1" = 0,
                        "2" = 0,
                        "3" = 1,
                        "4" = 1)) %>%
  glimpse()

hilo1 <- glm(VOTE_D ~ PTYIDd +
                 FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
                 WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
                 HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
                 GIBILLd + MTGDEDTd, data =  rob1, weights = allwt,
               family="binomial")
hilo2 <- glm(VOTE_D ~ PTYIDd +
     white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY + FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
     WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
     HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
     GIBILLd + MTGDEDTd,
   weights = allwt, data =  rob1, family="binomial")

#Model 3: Vote ~ recieves aid dummy**partyID + controls
hilo3 <- glm(VOTE_D ~ aid_dummy + PTYIDd + aid_dummy*PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  rob1, family="binomial")

#Model 4: Vote ~ count of loans + count of entitlements + county of means + 
#count of universal + party ID + controls
hilo4 <- glm(VOTE_D ~ total_loans + total_uni + total_ent + total_means + PTYIDd + 
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  rob1, family="binomial")
#Model 5: Vote ~ controls + count of loans**party ID
hilo5 <- glm(VOTE_D ~ total_loans  + PTYIDd + total_loans*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  rob1, family="binomial")
#Model 6: Vote ~ controls + count of entitlements**party ID
hilo6 <- glm(VOTE_D ~ total_ent + PTYIDd + total_ent*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
            weights = allwt, data =  rob1, family="binomial")
#Model 7: Vote ~ controls + count of universal**party ID
hilo7 <- glm(VOTE_D ~  total_uni + PTYIDd + total_uni*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  rob1, family="binomial")
#Model 8: Vote ~ controls + count of means**party ID
hilo8 <- glm(VOTE_D ~ total_means + PTYIDd + total_means*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  rob1, family="binomial")

hilo9 <- glm(VOTE_D ~ Total + PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF + PEOPSAY,
             weights = allwt, data =  rob1, family="binomial")

hilo10 <- glm(VOTE_D ~ Total + PTYIDd + Total*PTYIDd + white + EDUC + sex + 
                  FOLLOWPA + TRUSTOFF + PEOPSAY,
              weights = allwt, data =  rob1, family="binomial")

theme_set(theme_sjplot())

plot_model(hilo10, type = "pred", terms = c("Total", "PTYIDd"))
plot_model(hilo3, type = "pred", terms = c("aid_dummy", "PTYIDd"))

##ROBUST TO ORDINAL LOGIT
max_sub$VOTE = factor(max_sub$VOTE, levels = c("1", "2", "3", "4", "5"), 
                      ordered = TRUE)
#model 1
ord1 <- polr(VOTE ~ PTYIDd +
     FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
     WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
     HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
     GIBILLd + MTGDEDTd, weights = allwt, data =  max_sub, Hess=TRUE)
#Model 2: Vote ~ all programs + controls (race, income, education, sex, followpa, 
#trustoff, peopsay)
ord2 <- polr(VOTE ~ white + EDUC + sex + PTYIDd +
                 FOLLOWPA + TRUSTOFF + PEOPSAY + FDSTMPd + SOCSECd + MEDAIDd + MEDICAREd +
                 WELFAREd + EITCd + UNEMPLOYd + GOVTPENd + PUBHOUd + DISABITYd + WICd +
                 HEADd + COLLGRNTd + STULOANSd + VETBENd + WRKCOMPd + BUSLOANd +
                 GIBILLd + MTGDEDTd,
             weights = allwt, data =  max_sub, Hess=TRUE)
#Model 3: Vote ~ recieves aid dummy**partyID + controls
ord3 <- polr(VOTE ~ aid_dummy + PTYIDd + aid_dummy*PTYIDd + white + EDUC + sex + 
                 FOLLOWPA + TRUSTOFF + PEOPSAY,
             weights = allwt, data =  max_sub, Hess=TRUE)
#Model 4: Vote ~ count of loans + count of entitlements + county of means + 
#count of universal + party ID + controls
ord4 <- polr(VOTE ~ total_loans + total_uni + total_ent + total_means + PTYIDd + 
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
             weights = allwt, data =  max_sub, Hess=TRUE)
#Model 5: Vote ~ controls + count of loans**party ID
ord5 <- polr(VOTE ~ total_loans + PTYIDd + total_loans*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
             weights = allwt, data =  max_sub, Hess=TRUE)
#Model 6: Vote ~ controls + count of entitlements**party ID
ord6 <- polr(VOTE ~ total_ent + PTYIDd + total_ent*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
             weights = allwt, data =  max_sub, Hess=TRUE)
#Model 7: Vote ~ controls + count of universal**party ID
ord7 <- polr(VOTE ~ total_uni + PTYIDd + total_uni*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
                 weights = allwt, data =  max_sub, Hess=TRUE)
#Model 8: Vote ~ controls + count of means**party ID
ord8 <- polr(VOTE ~ total_means + PTYIDd + total_means*PTYIDd +
                 white + EDUC + sex + FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub, Hess=TRUE)

ord9 <- polr(VOTE ~ Total + PTYIDd + white + EDUC + sex + 
               FOLLOWPA + TRUSTOFF + PEOPSAY,
              weights = allwt, data =  max_sub, Hess=TRUE)

ord10 <- polr(VOTE ~ Total + PTYIDd + Total*PTYIDd + white + EDUC + sex + 
                FOLLOWPA + TRUSTOFF + PEOPSAY,
               weights = allwt, data =  max_sub, Hess=TRUE)


##TABLES
stargazer(ols_uw_1, ols_uw_2, ols_uw_3, ols_uw_4,
          title="Unweighted Models 1-4", type="latex", style = "apsr",
          align=TRUE, out="Tables/unweighted_1.tex")

stargazer(ols_uw_5, ols_uw_6, ols_uw_7, ols_uw_8,
          title="Unweighted Models 5-8", type="latex", style = "apsr",
          align=TRUE, out="Tables/unweighted_2.tex")

stargazer(hilo2, ord2,
          title="Weighted Models 1-3", type="latex", style = "apsr",
          align=TRUE, out="Tables/weighted_1.tex")

stargazer(hilo3, ord3,
          title="Weighted Models 1-3", type="latex", style = "apsr",
          align=TRUE, out="Tables/weighted_2.tex")

stargazer(ord9, hilo9, ord10, hilo10,
          title="test", type="latex", style = "apsr",
          align=TRUE, out="Tables/weighted_4.tex")

stargazer(ols_wt_2,
          title="Weighted Models 1-3", type="latex", style = "apsr",
          align=TRUE, out="Tables/robust_1.tex")

stargazer(ols_wt_3,
          title="Weighted Models 1-3", type="latex", style = "apsr",
          align=TRUE, out="Tables/robust_2.tex")

stargazer(ols_wt_9, ols_wt_10,
          title="test", type="latex", style = "apsr",
          align=TRUE, out="Tables/robust_4.tex")

stargazer(ord4, ord5, ord6, ord7, ord8,
          title="Weighted Models 1-3", type="latex", style = "apsr",
          align=TRUE, out="Tables/weighted_5.tex")

stargazer(hilo4, hilo5, hilo6, hilo7, hilo8,
          title="Weighted Models 1-3", type="latex", style = "apsr",
          align=TRUE, out="Tables/weighted_6.tex")

plot_model(hilo6, type = "pred", terms = c("total_ent", "PTYIDd"))
