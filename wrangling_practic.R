library(tidyverse)
library(haven)
library(labelled)

df <- read_dta("Data/98099043UnitedStates LAPOP AmericasBarometer 2014 v3.0_W.dta") 
write_csv(df, "2014.csv")
val_labels(df) <- NULL

tanf <- df %>%
  select(idnum, estratopri, upm, fecha, idiomaq, q1, q2, wt, ls3, soct1, idio1,
         idio2, cp13, l1b, b21, b21a, n15, m1, vb2, vb3n, vb4new, usvb1011, usvb20,
         cct1b, ocup4a, q10new, q12, q12c) %>%
  filter(cct1b < 3 & vb3n > 98) %>%
  filter(usvb1011 > 100 | usvb1011 == 77) %>%
  mutate(year = "2014") %>%
  mutate(pres_vote = as.factor(vb3n),
         pres_vote = recode(vb3n,
         "4001" = "Obama",
         "4002" = "Romney",
         "4077" = "Other",
         "99" = "Didn't Vote")) %>%
  mutate(pid = as.factor(usvb1011),
         pid = recode(usvb1011,
          "4001" = "Rep",
          "4002" = "Dem",
          "4003" = "Ind",
          "77" = "Didn't Vote")) %>%
  mutate(tanf = as.factor(cct1b),
         tanf = recode(cct1b,
          "1" = "TANF",
          "2" = "Not TANF")) %>%
  mutate(vote = as.factor(usvb1011),
         vote = recode(usvb1011,
                       "4001" = "Voted",
                       "4002" = "Voted",
                       "4003" = "Voted",
                       "77" = "Didn't Vote")) %>%
  
  glimpse()
write_csv(tanf, "2014_mod_tanf.csv")

tanf_sum <- tanf %>%
  # count the number of observations in each group
  group_by(tanf, vote) %>%   # make tanf or not tanf groups
  summarize(sum_tanf = n()) %>%  # n() just counts the number of observations in each group
  # create a varible that's the total number of responses to each survey
  group_by(tanf) %>%  # make each wording a group
  mutate(total = sum(sum_tanf)) %>% # just sum the responses to each option to obtain the total
  mutate(prop_category = sum_tanf/total) %>%  # <--- WHAT WE WANT!!! (just a fraction here)
  glimpse()

ggplot(tanf_sum, aes(x=vote, y=prop_category)) + geom_col() + facet_wrap("tanf")
  theme_bw() + #here's where we're adjusting font size
  labs(title = "Most Common Responses Very vs Extremely",
       x = "Likert Scale", y= "Number of Cases")

  ggplot(tanf_sum, aes(x=vote, y=sum_tanf)) + geom_col() + facet_wrap("tanf")
  theme_bw() + #here's where we're adjusting font size
    labs(title = "Most Common Responses Very vs Extremely",
         x = "Likert Scale", y= "Number of Cases")


  tanf_sum_2 <- tanf %>%
    # count the number of observations in each group
    group_by(tanf, pres_vote, pid) %>%   # make tanf or not tanf groups
    summarize(sum_tanf = n()) %>%  # n() just counts the number of observations in each group
    # create a varible that's the total number of responses to each survey
    group_by(tanf, pid) %>%  # make each wording a group
    mutate(total = sum(sum_tanf)) %>% # just sum the responses to each option to obtain the total
    mutate(prop_category = sum_tanf/total) %>%  # <--- WHAT WE WANT!!! (just a fraction here)
    glimpse()


ggplot(tanf_sum_2, aes(x=pres_vote, y=prop_category, fill=pid, color=pid)) + 
  geom_col() + facet_wrap("tanf") +
  theme_minimal()+ labs(title = "Vote Choice in 2012 by TANF Status",
         x = "2012 Presidential Vote Choice", y= "Proportion of Cases")

ggplot(tanf_sum_2, aes(x=pres_vote, y=..density..)) + 
  geom_density() + facet_wrap(vars(pid)) +
  theme_minimal()+ labs(title = "Vote Choice in 2012 by TANF Status",
                        x = "2012 Presidential Vote Choice", y= "Proportion of Cases")

ggplot(tanf, aes(x=tanf, y=..density..)) + geom_density() + facet_wrap("pid") +
  theme_minimal()

##WORKING - code for pension recipients
pension <- df %>%
  select(idnum, estratopri, upm, fecha, idiomaq, q1, q2, wt, ls3, soct1, idio1,
         idio2, cp13, l1b, b21, b21a, n15, m1, vb2, vb3n, vb4new, usvb1011, usvb20,
         cct1b, ocup4a, q10new, q12, q12c) %>%
  filter(cct1b < 3 & vb3n > 98) %>%
  filter(usvb1011 > 100 | usvb1011 == 77) %>%
  mutate(year = "2014") %>%
  mutate(pres_vote = as.factor(vb3n),
         pres_vote = recode(vb3n,
                            "4001" = "Obama",
                            "4002" = "Romney",
                            "4077" = "Other",
                            "99" = "Didn't Vote")) %>%
  mutate(pid = as.factor(usvb1011),
         pid = recode(usvb1011,
                      "4001" = "Rep",
                      "4002" = "Dem",
                      "4003" = "Ind",
                      "77" = "Didn't Vote")) %>%
  mutate(tanf = as.factor(cct1b),
         tanf = recode(cct1b,
                       "1" = "TANF",
                       "2" = "Not TANF")) %>%
  glimpse()
write_csv(tanf, "2014_mod_tanf.csv")


##WEIGHT PROPORTIONATELY
ggplot(tanf, aes(x=pres_vote, y=..density.., color=pres_vote)) + geom_density() + 
  facet_wrap(vars(tanf, pid)) + theme_bw()

