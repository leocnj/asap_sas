#
# asap_data.R
#
# process ASRP_SAS data stored at /Data
#
library(dplyr)
library(magrittr)

train <- read.csv('Data/train_rel_2.tsv', sep="\t", stringsAsFactors = FALSE)

# pub-board
pub1 <- read.csv('Data/public_leaderboard_rel_2.tsv',
                 sep = "\t", stringsAsFactors = FALSE)

pub2 <- read.csv('Data/public_leaderboard_solution.csv', stringsAsFactors = FALSE)

pub <- merge(pub1, pub2[c("id", "essay_score")], by.x="Id", by.y="id")

# focus on set 1 
set1_train <- train %>%
  filter(EssaySet==1) %>%
  select(c(Id,Score1,EssayText))
write.csv(set1_train, "set1_train.csv", row.names = F)

set1_test <- pub %>%
  filter(EssaySet==1) %>%
  select(c(Id,essay_score,EssayText))
write.csv(set1_test, "set1_test.csv", row.names = F)








  