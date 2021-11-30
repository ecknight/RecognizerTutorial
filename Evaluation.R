library(tidyverse)
library(readr)
library(data.table)

#1. Read in & wrangle validated data----
dat <- readr::read_tsv("RecognizerResults_Validated.txt", 
                       col_names = c("Filename", "TimeOffset", "Duration", "Level", "Quality", "Score", "Recognizer", "Comments"),
                       col_types = "ccddddcd") %>% 
  mutate(File = str_sub(Filename, -34, -1))

#2. Read in the "truth" and summarize number of detections----
hum <- read.csv("HumanProcessing.csv")
n <- sum(hum$n)

#3. Calculate precision & recall----
score <- seq(ceiling(min(dat$Score)), floor(max(dat$Score)), 1)

eval <- list()
for(i in 1:length(score)){
  
  dat.i <- dat %>% 
    dplyr::filter(Score >= score[i])
  
  eval[[i]] <- dat.i %>% 
    summarize(hits = n(),
              tp = sum(Comments)) %>% 
    mutate(recall = tp/n,
           precision = tp/hits,
           thresh = score[i]) 
  
}

evaluation <- data.table::rbindlist(eval)

#4. Plot----
precision <- ggplot(evaluation) +
  geom_line(aes(x=thresh, y=precision))
precision

recall <- ggplot(evaluation) +
  geom_line(aes(x=thresh, y=recall))
recall
