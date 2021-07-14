library(tidyverse)
library(readxl)
library(scales)
library(gridExtra)

eip <- read_excel("Data/EIP/EIP_1_w_election.xlsx") %>%
  mutate(State = as.factor(State))

eip$mean_aid <- mean(eip$total_dollar)
eip$med_aid <- median(eip$total_dollar)


eip$diff <- eip$total_dollar - eip$mean_aid
eip$diff2 <- eip$total_dollar - eip$med_aid

mean_line <- ggplot(eip, aes(x=State, y=total_dollar, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept=5286678) +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))
  
        
mean_diff <- ggplot(eip, aes(x=State, y=diff, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))

med_line <- ggplot(eip, aes(x=State, y=total_dollar, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept=3910130) +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))


med_diff <- ggplot(eip, aes(x=State, y=diff2, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))


##focus on children
kid <- eip
kid$mean_kid <- mean(kid$child_dollar)
kid$diff_kid <- kid$child_dollar - kid$mean_kid
kid$med_kid <- median(kid$child_dollar)
kid$diff_kid2 <- kid$child_dollar - kid$med_kid


mean_line2 <- ggplot(kid, aes(x=State, y=child_dollar, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept=583848.5) +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))


mean_diff2 <- ggplot(kid, aes(x=State, y=diff_kid, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))

med_line2 <- ggplot(kid, aes(x=State, y=child_dollar, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept=442637) +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))


med_diff2 <- ggplot(kid, aes(x=State, y=diff_kid2, fill=Vote_2020)) + 
  theme_minimal() +
  coord_flip() +
  geom_col() +
  theme(text = element_text(size=6)) + 
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))