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

max <- max %>%
  mutate(mean_dummy = as.character(total_means),
         mean_dummy = recode(total_means,
                         "0" = "N",
                         "1" = "Y",
                         "2" = "Y",
                         "3" = "Y",
                         "4" = "Y",
                         "5" = "Y",
                         "6" = "Y",
                         "7" = "Y",
                         "8" = "Y",
                         "9" = "Y")) %>%
  glimpse()
                         
one <- ggplot(max, aes(x=Total, y=VOTE_F, color=total_means)) + 
  geom_point(alpha=.4) + 
  geom_jitter() +
  scale_color_gradientn(colours = rainbow(5)) +
  theme_minimal() +
  labs(title = "Associaton Between Aid and Voting Frequency",
       x = "Total Aid Programs",
       y = "Voting Frequency",
       color = "Total Means-Tested Programs")
ggsave("Figs/scatter_total_vote.png", plot=one)

two <- ggplot(max, aes(x=total_means, y=VOTE_F, color=Total)) + 
  geom_point(alpha=.4) + 
  geom_jitter() +
  scale_color_gradientn(colours = rainbow(5)) +
  theme_minimal() +
  labs(title = "Associaton Between Aid and Voting Frequency",
       x = "Total Means-Tested Aid Programs",
       y = "Voting Frequency",
       color = "Total Aid Programs")

ggsave("Figs/scatter_means_vote.png", plot=two)