library(tidyverse)

max <- read_csv("Data/max_sub.csv")

#table measures of central tendency
##VOTE
max$mean.vote <- mean(max$VOTE)
max$min.vote <- min(max$VOTE)
max$max.vote <- max(max$VOTE)
max$med.vote <- median(max$VOTE)


#aid_dummy
max$min.aid_dum <- min(max$aid_dummy)
max$max.aid_dum <- max(max$aid_dummy)


#total_means
max$mean.total_mean <- mean(max$total_means)
max$min.total_mean <- min(max$total_means)
max$max.total_mean <- max(max$total_means)
max$med.total_mean <- median(max$total_means)

#total_uni
max$mean.total_uni <- mean(max$total_uni)
max$min.total_uni <- min(max$total_uni)
max$max.total_uni <- max(max$total_uni)
max$med.total_uni <- median(max$total_uni)

#total_loans
max$mean.total_loan <- mean(max$total_loans)
max$min.total_loan <- min(max$total_loans)
max$max.total_loan <- max(max$total_loans)
max$med.total_loan <- median(max$total_loans)

#total_ent
max$mean.total_ent <- mean(max$total_ent)
max$min.total_ent <- min(max$total_ent)
max$max.total_ent <- max(max$total_ent)
max$med.total_ent <- median(max$total_ent)

#Total
max$mean.total <- mean(max$Total)
max$min.total <- min(max$Total)
max$max.total <- max(max$Total)
max$med.total <- median(max$Total)

#EDUC
max$mean.ed <- mean(max$EDUC)
max$min.ed <- min(max$EDUC)
max$max.ed <- max(max$EDUC)
max$med.ed <- median(max$EDUC)

#FOLLOWPA
max$mean.pa <- mean(max$FOLLOWPA)
max$min.pa <- min(max$FOLLOWPA)
max$max.pa <- max(max$FOLLOWPA)
max$med.pa <- median(max$FOLLOWPA)

#TRUSTOFF 
max$mean.trust <- mean(max$TRUSTOFF)
max$min.trust <- min(max$TRUSTOFF)
max$max.trust <- max(max$TRUSTOFF)
max$med.trust <- median(max$TRUSTOFF)

#PEOPSAY
max$mean.say <- mean(max$PEOPSAY)
max$min.say <- min(max$PEOPSAY)
max$max.say <- max(max$EDUC)
max$med.say <- median(max$PEOPSAY)

#INCOME
max$mean.income <- mean(max$INCOME)
max$min.income <- min(max$INCOME)
max$max.income <- max(max$INCOME)
max$med.income <- median(max$INCOME)

df <- subset(max, select = -c(allwt, INCOME, white, VOTE, VOTE_F, EDUC, FDSTMPd, FDSTMP, SOCSECd, SOCSECf,
                              MEDAIDd, MEDICAID, MEDICAREd, MEDICAREf, WELFAREd, WELFARE, EITCd, EITCf,
                              UNEMPLOYd, UNEMPLOYf, GOVTPENd, GOVTPENf, PUBHOUd, PUBHOU, DISABITYd, DISABITYf,
                              WICd, WICf, HEADf, HEADd, COLLGRNTf, COLLGRNTd, STULOANSf,
                              STULOANSd, VETBENf, VETBENd, WRKCOMPf, WRKCOMPd, BUSLOANf, BUSLOANd, GIBILLf,
                              GIBILLd, MTGDEDTf, MTGDEDTd, PTYIDd, PTYIDf, FOLLOWPA, sex, 
                              TRUSTOFF, PEOPSAY, total_means, total_ent, total_loans, total_uni,
                              aid_dummy, Total, year))
write.csv(df, "Data/measures-of-central-tend.csv")