library(tidyverse)
library(qwraps2)
library(gridExtra)


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

u <- ggplot(max, aes(x=total_uni, y=PEOPSAY)) + geom_point() + geom_jitter() +
  coord_flip() + theme_minimal() +
  xlim(0, 8) + scale_y_continuous(breaks=c(0,1)) +
  labs(title = "Universal Aid",
       x = "Total Programs",
       y = "")

m <- ggplot(max, aes(x=total_means, y=PEOPSAY)) + geom_point() + geom_jitter() +
  coord_flip() + theme_minimal() +
  xlim(0, 8) + scale_y_continuous(breaks=c(0,1)) +
  labs(title = "Means Tested Aid",
       x = "",
       y = "")


e <- ggplot(max, aes(x=total_ent, y=PEOPSAY)) + geom_point() + geom_jitter() +
  coord_flip() + theme_minimal() +
  xlim(0, 8) + scale_y_continuous(breaks=c(0,1)) +
  labs(title = "Entitlements",
       x = "Total Programs",
       y = "Self Efficacy")

l <- ggplot(max, aes(x=total_loans, y=PEOPSAY)) + geom_point() + geom_jitter() +
  coord_flip() + theme_minimal() +
  xlim(0, 8) + scale_y_continuous(breaks=c(0,1)) +
  labs(title = "Loan Aid",
       x = "",
       y = "Self Efficacy")

a <- grid.arrange(u, m, e, l)
ggsave("Figs/scatter_SE.png", plot = a)

summary(max$INCOME)

ggplot(max, aes(x=aid_dummy, y=INCOME)) + geom_point() + geom_jitter() +
  theme_minimal() +
  labs(title = "Relationship Between Income and Aid",
       y = "Income (7pt Scale)",
       x = "Aid (Dummy)")
ggsave("Figs/aid-and-income.png")

a <- ggplot(max, aes(x=PEOPSAY, y=..density..)) + 
  geom_histogram() + theme_minimal() + facet_grid(vars(DISABITYd)) +
  scale_x_continuous(breaks=c(0,1)) +
  labs(title = "SSDI and Self-Efficacy",
       y = "Density",
       x = "Self Efficacy")

b <- ggplot(max, aes(x=PEOPSAY, y=..density..)) + 
  geom_histogram() + theme_minimal() + facet_grid(vars(WRKCOMPd)) +
  scale_x_continuous(breaks=c(0,1)) +
  labs(title = "Worker's Comp. and Self-Efficacy",
       y = "Density",
       x = "Self Efficacy")

c<- ggplot(max, aes(x=PEOPSAY, y=..density..)) + 
  geom_histogram() + theme_minimal() + facet_grid(vars(UNEMPLOYd)) +
  scale_x_continuous(breaks=c(0,1)) +
  labs(title = "Unemployment Insurance and Self-Efficacy",
       y = "Density",
       x = "Self Efficacy")
q <- grid.arrange(a,b,c)
ggsave("Figs/uni_SE.png", plot = q, width = 5, height=6)
