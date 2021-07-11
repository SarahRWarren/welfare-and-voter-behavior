library(tidyverse)
library(readxl)
library(survey)
library(stargazer)
library(ggpubr)

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

stargazer(ols_uw_1, ols_uw_2, ols_uw_3, ols_uw_4,
          title="Unweighted Models 1-4", type= "text", style = "apsr",
          align=TRUE, out="unweighted_1.txt")
          
stargazer(ols_uw_5, ols_uw_6, ols_uw_7, ols_uw_8,
          title="Unweighted Models 5-8", type="text", style = "apsr",
          align=TRUE, out="unweighted_2.txt")

stargazer(ols_wt_1, ols_wt_2, ols_wt_3,
          title="Weighted Models 1-3", type="text", style = "apsr",
          align=TRUE, out="weighted_1.txt")

stargazer(ols_wt_4, ols_wt_5, ols_wt_6, ols_wt_7, ols_wt_8,
          title="Weighted Models 4-8", type="text", style = "apsr",
          align=TRUE, out="weighted_2.txt")