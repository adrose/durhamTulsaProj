### Load library(s)
library(ggplot2)
library(corrplot)
require(MASS)
library(Rmisc)
require(Hmisc)
require(reshape2)
library(psych)
library(visreg)
library(tidyverse)
source("~/GitHub/adroseHelperScripts/R/afgrHelpFunc.R")

### Load data
in.dat.ou <- read.csv("./data/TRM_OUsample.csv")
in.dat.uk <- read.csv("./data/TRM_UKsample.csv")


## Fix condition name for OU
names(in.dat.ou)[3] <- "condition"

## Make sure condition is a factor
in.dat.ou$condition <- factor(in.dat.ou$condition, levels=c(3, 1, 2))
# Now relevel so the mass == 3; riot ==2 and event ==1
## Now run an HLM or a MANOVA to explore multivariate effects of response patterns?
## Lets try to collapse this across the OU and UK cohorts
## First harmonize the column names
in.col.uk <- c("violence", "blame", "caused", "intentionality", "punishment", "allowedcommitted", "knew", "should_prevent", "could_prevent")
out.col.ou <- c("violence", "blame", "caused", "intention", "punish", "allow.commit", "knew","should", "could")
group.col <- c("black", "white", "police")
group.col.ou <- c("b", "w", "p")

## First make all column names lower case
colnames(in.dat.ou) <- tolower(colnames(in.dat.ou))
colnames(in.dat.uk) <- tolower(colnames(in.dat.uk))
## Now create the new uk dataset which will be used to combine the ou dataset into
data.target <- in.dat.uk
data.target.ou <- in.dat.ou
cols.to.kepp <- c()
for(i in 1:length(in.col.uk)){
  # NOw loop through group in question
  for(c in 1:length(group.col)){
    # print the column name
    col.name.uk <- paste(group.col[c], in.col.uk[i],sep='.')
    col.name.ou <- paste(out.col.ou[i], group.col.ou[c], sep='.')
    ## Identify the index
    index <- grep(col.name.uk, colnames(data.target))
    index.ou <- grep(col.name.ou, colnames(in.dat.ou))
    ## FLag for length greater than 1
    if(length(index)<1 | length(index.ou)<1){
      print("uh oh")
      print(paste("Column:", i, "Group:", c, sep=' '))
    }
    print(paste("Changing from", col.name.uk, "to", col.name.ou, sep=' '))
    ## Change the column names
    colnames(data.target)[index] <- col.name.ou
  }
}

## Now combine the data
## First make sure some final column name mathces
colnames(data.target)[c(1,2,3,32,33,34,35,36)] <- c("id", "duration", "condition", "reparation", "priorknowledge", "allow", "gender", "age")
data.target.ou <- data.target.ou[,colnames(data.target.ou) %in% colnames(data.target)]
data.target <- data.target[,colnames(data.target) %in% colnames(data.target.ou)]
## Add a cohort source
data.target.ou$cohort <- "OU"
data.target$cohort <- "UK"
data.target <- rbind(data.target, data.target.ou)
## Now fix the condition values here
data.target$condition <- plyr::revalue(data.target$condition, c("1" = "Riot", "2" = "Massacre", "3" = "Event"))
data.target$priorknowledge <- factor(data.target$priorknowledge)
data.target$priorknowledge <- plyr::revalue(data.target$priorknowledge, c("1" = "Yes", "2" = "No"))

# rm the ou target
#rm(data.target.ou)
## Now melt these so we can run some HLM models
data.hlm <- melt(data.target, id.vars = c("id", "duration", "condition", "reparation", "priorknowledge", "allow", "gender", "age","cohort"))
## Now grab the party to be blamed from the end of the variable names
data.hlm$variable <- as.character(data.hlm$variable)
data.hlm$questionTarg <- substr(data.hlm$variable, nchar(data.hlm$variable), nchar(data.hlm$variable))
data.hlm$questionVal <- substr(data.hlm$variable, 1, nchar(data.hlm$variable)-2)
## Now fix the ID overlap
data.hlm$id <- paste(data.hlm$id, data.hlm$cohort)
## Now fix the target varaible
data.hlm$questionTarg <- plyr::revalue(data.hlm$questionTarg, c("b" = "Black Tulsans", "p" = "Police", "w" = "White Tulsans"))

## Now create a scaled dataset
data.target.scale <- data.target

## Now train the big model here
mod.five <- lmerTest::lmer(value ~ (priorknowledge + questionTarg + questionVal + condition)^4 + gender + age + cohort + (1|id), data=data.hlm, REML=TRUE, na.action = "na.exclude") ## sig interaction here
## mod.five.sub
mod.five.pk.1 <- lmerTest::lmer(value ~ (questionTarg + questionVal + condition)^4 + gender + age + cohort + (1|id), data=data.hlm[which(data.hlm$priorknowledge=="Yes"),], REML=TRUE, na.action = "na.exclude") 
mod.five.pk.2 <- lmerTest::lmer(value ~ (questionTarg + questionVal + condition)^4 + gender + age + cohort + (1|id), data=data.hlm[which(data.hlm$priorknowledge=="No"),], REML=TRUE, na.action = "na.exclude") 


## Now plot these results
data.hlm$predVal <- predict(mod.five)
data.hlm$groupVar <- paste(data.hlm$priorknowledge, data.hlm$questionTarg)

## Now train the model within each ... condition
one.one <- data.hlm %>% filter(condition =="Riot")
one.two <- data.hlm %>% filter(condition =="Massacre")
one.thr <- data.hlm %>% filter(condition =="Event")
mod.pk.one <- lmerTest::lmer(value ~ (questionTarg + questionVal)^2 + cohort + (1|id), data=one.one)
mod.pk.two <- lmerTest::lmer(value ~ (questionTarg + questionVal)^2 + cohort + (1|id), data=one.two)
mod.pk.thr <- lmerTest::lmer(value ~ (questionTarg + questionVal)^2 + cohort + (1|id), data=one.thr)
one.one$predVal <- predict(mod.pk.one)
one.two$predVal <- predict(mod.pk.two)
one.thr$predVal <- predict(mod.pk.thr)
all.dat <- rbind(one.one, one.two, one.thr)
all.dat$Target <- all.dat$questionTarg
p4 <- summarySE(data = all.dat, measurevar = "predVal", groupvars = c("condition", "Target", "questionVal"), na.rm=T) %>% 
  ggplot(., aes(x=questionVal, y=predVal, group=Target, fill=Target)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(condition ~ .) +
  geom_errorbar(aes(ymin=predVal-sd, ymax=predVal+sd), stat="identity", position ="dodge") +
  theme_bw() +
  xlab("Judgment Item") +
  ylab("Overall Judgement") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_cartesian(ylim=c(1,7)) +
  scale_fill_grey()

ggsave(plot = p4, filename = "./figures/fig4.png", dpi=1000, width = 10, height = 9)
