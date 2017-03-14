#
# asap_data.R
# process ASRP_SAS data stored at /Data
#
library(dplyr)
library(magrittr)
library(splitstackshape)


set.seed(12345)

train <- read.csv('Data/train_rel_2.tsv', sep="\t", stringsAsFactors = FALSE)

# pub-board
pub1 <- read.csv('Data/public_leaderboard_rel_2.tsv',
                 sep = "\t", stringsAsFactors = FALSE)

pub2 <- read.csv('Data/public_leaderboard_solution.csv', stringsAsFactors = FALSE)
pub <- merge(pub1, pub2[c("id", "essay_score")], by.x="Id", by.y="id")

oneset <- function(id){
  trainFile <- paste('asap/train', id, '.csv', sep='')
  testFile <- paste('asap/test', id, '.csv', sep='')
  dev_taFile <- paste0('asap/dev_train', id, '.csv')
  dev_tsFile <- paste0('asap/dev_test', id, '.csv')
  show(trainFile)

  set_train <- train %>%
    filter(EssaySet==id) %>%
    select(c(Score1,EssayText)) %>%
    rename(label=Score1, text=EssayText)
  write.csv(set_train, trainFile, row.names = F)
  dev <- stratified(set_train, "label", .2, bothSets=T)
  dev_ta <- dev$SAMP2
  dev_ts <- dev$SAMP1
  write.csv(dev_ta, dev_taFile, row.names = F)
  write.csv(dev_ts, dev_tsFile, row.names = F)
  
  set_test <- pub %>%
    filter(EssaySet==id) %>%
    select(c(essay_score,EssayText)) %>%
    rename(label=essay_score,text=EssayText)
  write.csv(set_test, testFile, row.names = F)
  
  if(max(set_train$label) != max(set_test$label)){
    print("WRONG")
  }
}

lapply(1:10, oneset)

# test BOW baseline performance.
# dplyr and the Metrics packages are so neat to work together.
bow <- read.csv('Data/bag_of_words_benchmark.csv')
colnames(bow) <- c("id", "pred")

bow_all <- merge(bow, pub[c("Id", "EssaySet", "essay_score")], by.x = "id",
                 by.y = "Id")

require(Metrics)
kappas <- bow_all %>%
          group_by(EssaySet) %>%
          summarize(kp=ScoreQuadraticWeightedKappa(essay_score, pred))

kp_bow <-MeanQuadraticWeightedKappa(kappas$kp)
show(kp_bow)
