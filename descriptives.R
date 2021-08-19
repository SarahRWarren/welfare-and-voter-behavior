library(tidyverse)
library(qwraps2)


max <- read_csv("Data/max_sub.csv")
options(qwraps2_markup = "latex")


our_summary1 <-
  list("Vote" =
         list("Min."       = ~ min(VOTE),
              "Median"    = ~ median(VOTE),
              "Mean"    = ~ mean(VOTE),
              "Max."       = ~ max(VOTE)),
       "Republican" =
         list("Min."       = ~ min(PTYIDd),
              "Median"    = ~ median(PTYIDd),
              "Mean"    = ~ mean(PTYIDd),
              "Max."       = ~ max(PTYIDd)),
       "Self-Efficacy" =
         list("Min."       = ~ min(PEOPSAY),
              "Median"    = ~ median(PEOPSAY),
              "Mean"    = ~ mean(PEOPSAY),
              "Max."       = ~ max(PEOPSAY)),
       "Total Aid" =
         list("Min."       = ~ min(Total),
              "Median"    = ~ median(Total),
              "Mean"    = ~ mean(Total),
              "Max."       = ~ max(Total)),
       "Total Universal Aid" =
         list("Min."       = ~ min(total_uni),
              "Median"    = ~ median(total_uni),
              "Mean"    = ~ mean(total_uni),
              "Max."       = ~ max(total_uni)),
       "Total Means-Tested Aid" =
         list("Min."       = ~ min(total_means),
              "Median"    = ~ median(total_means),
              "Mean"    = ~ mean(total_means),
              "Min."       = ~ max(total_means)),
       "Total Entitlements" =
         list("Min."       = ~ min(total_ent),
              "Median"    = ~ median(total_ent),
              "Mean"    = ~ mean(total_ent),
              "Max."       = ~ max(total_ent)),
       "Total Loans" =
         list("Min."       = ~ min(total_loans),
              "Median"    = ~ median(total_loans),
              "Mean"    = ~ mean(total_loans),
              "Max."      = ~ max(total_loans)))


whole <- summary_table(max, our_summary1)
whole