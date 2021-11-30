library(tidyverse)
library(readr)
library(lme4)
library(AICcmodavg)

#1. Read in & wrangle validated data----
dat <- readr::read_tsv("RecognizerResults_Validated.txt", 
                       col_names = c("Filename", "TimeOffset", "Duration", "Level", "Quality", "Score", "Recognizer", "Comments"),
                       col_types = "ccddddcd") %>% 
  mutate(File = str_sub(Filename, -34, -1))

dat.1 <- dat %>% 
  dplyr::filter(Comments==1)

dat.0 <- dat %>% 
  dplyr::filter(Comments==0)

#2. Plot----
ggplot(dat) +
  geom_point(aes(x=Score, y=Level)) +
  facet_wrap(~Comments)

#3. Test----
lm.int <- lmer(Score ~ Level*factor(Comments) + (1|File), data=dat)
lm.add <- lmer(Score ~ Level + factor(Comments) + (1|File), data=dat)
lm.level <- lmer(Score ~ Level + (1|File), data=dat)
lm.null <- lmer(Score ~ 1 + (1|File), data=dat)
aictab(list("interaction" = lm.int, "additive" = lm.add, "levelonly" = lm.level, "null" = lm.null))
