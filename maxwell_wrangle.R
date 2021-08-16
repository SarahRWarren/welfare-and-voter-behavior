library(tidyverse)
library(readxl)
##load data
#recode 5pt scale as factor
##create dummies for R recieves (1) and R doesn't recieve (0)
max <- read_xls("Data/allmax07.xls") %>%
  filter(FDSTMP2 < 5) %>%
  filter(WELFARE2 < 5) %>%
  filter(VOTE < 5) %>%
  mutate(PTYIDf = as.factor(PTYID),
         PTYIDf = recode(PTYID,
                         "1" = "Democrat",
                         "2" = "Independent",
                         "3" = "Republican",
                         "4" = "Other",
                         "5" = "No response")) %>%
  mutate(VOTE = as.numeric(VOTE),
         VOTE = recode(VOTE,
                           "1" = "4",
                           "2" = "3",
                           "3" = "2",
                           "4" = "1",
                           "5" = "5")) %>%
  mutate(PEOPSAY = as.numeric(PEOPSAY),
         PEOPSAY = recode(PEOPSAY,
                       "1" = 0,
                       "2" = 1,
                       "3" = 7)) %>%
  mutate(VOTE_F = as.factor(VOTE),
         VOTE_F = recode(VOTE,
                         "1" = "Not at all",
                         "2" = "Sometimes",
                         "3" = "Usually",
                         "4" = "Always",
                         "5" = "No response")) %>%
  mutate(FDSTMP = as.factor(FDSTMP2),
         FDSTMP = recode(FDSTMP2,
                         "1" = "Respondent",
                         "2" = "R's Family",
                         "3" = "R and R's Family",
                         "4" = "None",
                         "5" = "No response")) %>%
  mutate(SOCSECf = as.factor(SOCSEC),
         SOCSECf = recode(SOCSEC,
                         "1" = "Respondent",
                         "2" = "R's Family",
                         "3" = "R and R's Family",
                         "4" = "None",
                         "5" = "No response")) %>%
  mutate(MEDICAREf = as.factor(MEDICARE),
         MEDICAREf = recode(MEDICARE,
                          "1" = "Respondent",
                          "2" = "R's Family",
                          "3" = "R and R's Family",
                          "4" = "None",
                          "5" = "No response")) %>%
  mutate(MEDAID = as.factor(MEDAID2),
         MEDAID = recode(MEDAID2,
                            "1" = "Respondent",
                            "2" = "R's Family",
                            "3" = "R and R's Family",
                            "4" = "None",
                            "5" = "No response")) %>%
  mutate(WELFARE = as.factor(WELFARE2),
         WELFARE = recode(WELFARE2,
                         "1" = "Respondent",
                         "2" = "R's Family",
                         "3" = "R and R's Family",
                         "4" = "None",
                         "5" = "No response")) %>%
  mutate(EITCf = as.factor(EITC),
         EITCf = recode(EITC,
                          "1" = "Respondent",
                          "2" = "R's Family",
                          "3" = "R and R's Family",
                          "4" = "None",
                          "5" = "No response")) %>%
  mutate(UNEMPLOYf = as.factor(UNEMPLOY),
         UNEMPLOYf = recode(UNEMPLOY,
                        "1" = "Respondent",
                        "2" = "R's Family",
                        "3" = "R and R's Family",
                        "4" = "None",
                        "5" = "No response")) %>%
  mutate(GOVTPENf = as.factor(GOVTPEN),
         GOVTPENf = recode(GOVTPEN,
                            "1" = "Respondent",
                            "2" = "R's Family",
                            "3" = "R and R's Family",
                            "4" = "None",
                            "5" = "No response")) %>%
  mutate(PUBHOU = as.factor(PUBHOU2),
         PUBHOU = recode(PUBHOU2,
                           "1" = "Respondent",
                           "2" = "R's Family",
                           "3" = "R and R's Family",
                           "4" = "None",
                           "5" = "No response")) %>%
  mutate(DISABITYf = as.factor(DISABITY),
         DISABITYf = recode(DISABITY,
                         "1" = "Respondent",
                         "2" = "R's Family",
                         "3" = "R and R's Family",
                         "4" = "None",
                         "5" = "No response")) %>%
  mutate(WICf = as.factor(WIC),
         WICf = recode(WIC,
                         "1" = "Respondent",
                         "2" = "R's Family",
                         "3" = "R and R's Family",
                         "4" = "None",
                         "5" = "No response")) %>%
    mutate(HEADf = as.factor(HEADST2),
         HEADf = recode(HEADST2,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None",
                       "5" = "No response")) %>%
  mutate(COLLGRNTf = as.factor(COLLGRT2),
         COLLGRNTf = recode(COLLGRT2,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None",
                       "5" = "No response")) %>%
  mutate(STULOANSf = as.factor(STULOANS),
         STULOANSf = recode(STULOANS,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None",
                       "5" = "No response")) %>%
  mutate(VETBENf = as.factor(VETBEN),
         VETBENf = recode(VETBEN,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None",
                       "5" = "No response")) %>%
  mutate(WRKCOMPf = as.factor(WRKCOMP),
         WRKCOMPf = recode(WRKCOMP,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None",
                       "5" = "No response")) %>%
  mutate(BUSLOANf = as.factor(BUSLOAN),
         BUSLOANf = recode(BUSLOAN,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None")) %>%
  mutate(GIBILLf = as.factor(GIBILL),
         GIBILLf = recode(GIBILL,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None",
                       "5" = "No response")) %>%
  mutate(MTGDEDTf = as.factor(MTGDEDT),
         MTGDEDTf = recode(MTGDEDT,
                       "1" = "Respondent",
                       "2" = "R's Family",
                       "3" = "R and R's Family",
                       "4" = "None",
                       "5" = "No response")) %>%
  mutate(PTYIDd = as.numeric(PTYID),
         PTYIDd = recode(PTYID,
                         "3" = 1,
                         "1" = 0,
                         "2" = 55,
                         "4" = 55,
                         "5" = 55)) %>%
  mutate(FDSTMPd = as.numeric(FDSTMP2),
         FDSTMPd = recode(FDSTMP2,
                          "1" = 1,
                          "2" = 0,
                          "3" = 1,
                          "4" = 0,
                          "5" = 27)) %>%
  mutate(SOCSECd = as.numeric(SOCSEC),
         SOCSECd = recode(SOCSEC,
                          "1" = 1,
                          "2" = 0,
                          "3" = 1,
                          "4" = 0,
                          "5" = 27)) %>%
  mutate(MEDICAREd = as.numeric(MEDICARE),
         MEDICAREd = recode(MEDICARE,
                            "1" = 1,
                            "2" = 0,
                            "3" = 1,
                            "4" = 0,
                            "5" = 27)) %>%
  mutate(MEDAIDd = as.numeric(MEDAID2),
         MEDAIDd = recode(MEDAID2,
                          "1" = 1,
                          "2" = 0,
                          "3" = 1,
                          "4" = 0,
                          "5" = 27)) %>%
  mutate(WELFAREd = as.numeric(WELFARE2),
         WELFAREd = recode(WELFARE2,
                           "1" = 1,
                           "2" = 0,
                           "3" = 1,
                           "4" = 0,
                           "5" = 27)) %>%
  mutate(EITCd = as.numeric(EITC),
         EITCd = recode(EITC,
                        "1" = 1,
                        "2" = 0,
                        "3" = 1,
                        "4" = 0,
                        "5" = 27)) %>%
  mutate(UNEMPLOYd = as.numeric(UNEMPLOY),
         UNEMPLOYd = recode(UNEMPLOY,
                            "1" = 1,
                            "2" = 0,
                            "3" = 1,
                            "4" = 0,
                            "5" = 27)) %>%
  mutate(GOVTPENd = as.factor(GOVTPEN),
         GOVTPENd = recode(GOVTPEN,
                           "1" = 1,
                           "2" = 0,
                           "3" = 1,
                           "4" = 0,
                           "5" = 27)) %>%
  mutate(PUBHOUd = as.numeric(PUBHOU2),
         PUBHOUd = recode(PUBHOU2,
                          "1" = 1,
                          "2" = 0,
                          "3" = 1,
                          "4" = 0,
                          "5" = 27)) %>%
  mutate(DISABITYd = as.numeric(DISABITY),
         DISABITYd = recode(DISABITY,
                            "1" = 1,
                            "2" = 0,
                            "3" = 1,
                            "4" = 0,
                            "5" = 27)) %>%
  mutate(WICd = as.numeric(WIC),
         WICd = recode(WIC,
                       "1" = 1,
                       "2" = 0,
                       "3" = 1,
                       "4" = 0,
                       "5" = 27)) %>%
  mutate(HEADd = as.factor(HEADST2),
         HEADd = recode(HEADST2,
                        "1" = 1,
                        "2" = 0,
                        "3" = 1,
                        "4" = 0,
                        "5" = 27)) %>%
  mutate(COLLGRNTd = as.factor(COLLGRT2),
         COLLGRNTd = recode(COLLGRT2,
                            "1" = 1,
                            "2" = 0,
                            "3" = 1,
                            "4" = 0,
                            "5" = 27)) %>%
  mutate(STULOANSd = as.factor(STULOANS),
         STULOANSd = recode(STULOANS,
                            "1" = 1,
                            "2" = 0,
                            "3" = 1,
                            "4" = 0,
                            "5" = 27)) %>%
  mutate(VETBENd = as.factor(VETBEN),
         VETBENd = recode(VETBEN,
                          "1" = 1,
                          "2" = 0,
                          "3" = 1,
                          "4" = 0,
                          "5" = 27)) %>%
  mutate(WRKCOMPd = as.factor(WRKCOMP),
         WRKCOMPd = recode(WRKCOMP,
                           "1" = 1,
                           "2" = 0,
                           "3" = 1,
                           "4" = 0,
                           "5" = 27)) %>%
  mutate(BUSLOANd = as.factor(BUSLOAN),
         BUSLOANd = recode(BUSLOAN,
                           "1" = 1,
                           "2" = 0,
                           "3" = 1,
                           "4" = 0,
                           "5" = 27)) %>%
  mutate(GIBILLd = as.factor(GIBILL),
         GIBILLd = recode(GIBILL,
                          "1" = 1,
                          "2" = 0,
                          "3" = 1,
                          "4" = 0,
                          "5" = 27)) %>%
  mutate(white = as.factor(race2),
         white = recode(race2,
                        "1" = 0,
                        "2" = 1,
                        "3" = 1,
                        "4" = 0,
                        "5" = 0,
                        "6" = 0,
                        "7" = 27)) %>%
  mutate(MTGDEDTd = as.factor(MTGDEDT),
         MTGDEDTd = recode(MTGDEDT,
                           "1" = 1,
                           "2" = 0,
                           "3" = 1,
                           "4" = 0,
                           "5" = 27)) %>%

  filter(FDSTMPd < 5) %>%
  filter(SOCSECd < 5) %>%
  filter(MEDICAREd < 5) %>%
  filter(MEDAIDd < 5) %>%
  filter(WELFAREd < 5) %>%
  filter(EITCd < 5) %>%
  filter(UNEMPLOYd < 5) %>%
  filter(GOVTPENd < 5) %>%
  filter(PUBHOUd < 5) %>%
  filter(DISABITYd < 5) %>%
  filter(WICd < 5) %>%
  filter(HEADST2 < 5) %>%
  filter(COLLGRT2 < 5) %>%
  filter(STULOANS < 5) %>%
  filter(VETBEN < 5) %>%
  filter(WRKCOMP < 5) %>%
  filter(BUSLOAN < 5) %>%
  filter(GIBILL < 5) %>%
  filter(MTGDEDT < 5) %>%
  filter(PTYIDd < 2) %>%
  filter(TRUSTOFF < 3) %>%
  filter(INCOME < 8) %>%
  filter(PEOPSAY < 3) %>%
  filter(EDUC < 5) %>%
  filter(white < 3) %>%
  filter(FOLLOWPA < 5) %>%
  glimpse()

#count total aid participation for each participant
a <- max %>%
  mutate(Total = select(., FDSTMPd:MTGDEDTd) %>% rowSums(na.rm = TRUE))
summary(a$Total) #descriptive stats
table(a$Total,max$VOTE_F)

#count total means tested
a <- a %>%
  mutate(total_means = select(., FDSTMPd, MEDAIDd, WELFAREd,
                              EITCd, PUBHOUd, WICd, HEADd,
                              COLLGRNTd, MTGDEDTd) %>% 
           rowSums(na.rm=TRUE))
summary(a$total_means)

#count total loans
a <- a %>%
  mutate(total_loans = select(., STULOANSd, BUSLOANd) %>% 
           rowSums(na.rm=TRUE))
summary(a$total_loans)

#count total entitlements
a <- a %>%
  mutate(total_ent = select(., SOCSECd, MEDICAREd, GOVTPENd, VETBENd,
                            GIBILLd) %>% rowSums(na.rm=TRUE))
summary(a$total_ent)

#count total universal
a <- a %>%
  mutate(total_uni = select(., UNEMPLOYd, DISABITYd, WRKCOMPd) %>% 
           rowSums(na.rm=TRUE))
summary(a$total_uni)

#make aid dummy
a <- a %>%  
  mutate(aid_dummy = as.numeric(Total),
         aid_dummy = recode(Total,
                  "0" = 0,
                  "1" = 1,
                  "2" = 1,
                  "3" = 1,
                  "4" = 1,
                  "5" = 1,
                  "6" = 1,
                  "7" = 1,
                  "8" = 1,
                  "9" = 1,
                  "10" = 1,
                  '11' = 1,
                  '12' = 1,
                  '13' = 1)) %>%
  glimpse()

#order VOTE_F correctly
a$VOTE_F <- factor(a$VOTE_F, levels = c("Always", "Usually", "Sometimes",
                                        "Not at all"))
#check our work
#class(a$VOTE_F)

max_sub <- a %>%
  mutate(sex = as.numeric(sex),
         sex = recode(sex,
                       "1" = 0,
                       "2" = 1)) %>%
  select(allwt, INCOME, white, VOTE, VOTE_F, EDUC, FDSTMPd, FDSTMP, SOCSECd, SOCSECf,
         MEDAIDd, MEDICAID, MEDICAREd, MEDICAREf, WELFAREd, WELFARE, EITCd, EITCf,
         UNEMPLOYd, UNEMPLOYf, GOVTPENd, GOVTPENf, PUBHOUd, PUBHOU, DISABITYd, DISABITYf,
         WICd, WICf, HEADf, HEADd, COLLGRNTf, COLLGRNTd, STULOANSf,
         STULOANSd, VETBENf, VETBENd, WRKCOMPf, WRKCOMPd, BUSLOANf, BUSLOANd, GIBILLf,
         GIBILLd, MTGDEDTf, MTGDEDTd, PTYIDd, PTYIDf, FOLLOWPA, sex, 
         TRUSTOFF, PEOPSAY, total_means, total_ent, total_loans, total_uni,
         aid_dummy, Total, year)
write_csv(max_sub, "Data/max_sub.csv")
