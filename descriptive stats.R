#load packages
library(tidyverse)
library(gridExtra)
library(viridis)
#read data
max <- read_csv("Data/max_sub.csv")

#order VOTE_F correctly
max$VOTE_F <- factor(max$VOTE_F, levels = c("Always", "Usually", "Sometimes",
                                        "Not at all"))
#check our work
class(max$VOTE_F)

ggplot(max, aes(x=total_means, y=..density.., fill=VOTE_F)) + 
  geom_density(alpha=.3) + 
  theme_minimal() +
  scale_fill_manual(values=c("#FFFF00", "#FF0000", "#228B22", "#0000FF")) +
  labs(x = "Total Means Tested Programs",
       fill = "Voting Frequency",
       y = "Density")

ggplot(max, aes(x=total_uni, y=..density.., fill=VOTE_F)) + 
  geom_density(alpha=.3) + 
  theme_minimal() +
  scale_fill_manual(values=c("#FFFF00", "#FF0000", "#228B22", "#0000FF")) +
  labs(x = "Total Universal Aid Programs",
       fill = "Voting Frequency",
       y = "Density")

ggplot(max, aes(x=total_loans, y=..density.., fill=VOTE_F)) + 
  geom_density(alpha=.3) + 
  theme_minimal() +
  scale_fill_manual(values=c("#FFFF00", "#FF0000", "#228B22", "#0000FF")) +
  labs(x = "Total Loan Programs",
       fill = "Voting Frequency",
       y = "Density")

ggplot(max, aes(x=total_ent, y=..density.., fill=VOTE_F)) + 
  geom_density(alpha=.3) + 
  theme_minimal() +
  scale_fill_manual(values=c("#FFFF00", "#FF0000", "#228B22", "#0000FF")) +
  labs(x = "Total Entitlement Programs",
       fill = "Voting Frequency",
       y = "Density")
