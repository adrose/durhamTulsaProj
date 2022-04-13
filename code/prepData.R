rm(list=ls()) 
### Load library(s)
library(dplyr)
library(stats)
library(ggplot2)
library(corrplot)
library(MASS)
library(Hmisc)
library(reshape2)
library(psych)
library(visreg)
library(tidyverse)
library(lmerTest)
library(lme4)
library(melt)
library(ggeffects)
library(sjPlot)
library(sjmisc)
library(tidyr)
### Load data
in.dat.ou <- read.csv("./data/TRM_OUsample.csv")
in.dat.uk <- read.csv("./data/TRM_UKsample.csv")
# 
# # fix age differences
# #In OU - value is year of age; In UK - 1 = 18-24, 2 = 25-34, 3 = 35-44, 4 = 45-54, 5 = 55-64, 6 = 65-74, 7 = 75+, 8 = Prefer not to answer
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==1] <- 22
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==2] <- 29
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==3] <- 39
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==4] <- 49
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==5] <- 59
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==6] <- 69
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==7] <- 75
# in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==8] <- NA
# # 
#In OU - value is year of age; In UK - 1 = 18-24, 2 = 25-34, 3 = 35-44, 4 = 45-54, 5 = 55-64, 6 = 65-74, 7 = 75+, 8 = Prefer not to answer
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==1] <- "18-24"
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==2] <- "25-34"
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==3] <- "35-44"
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==4] <- "45-54"
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==5] <- "55-64"
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==6] <- "65-74"
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==7] <- "75+"
in.dat.uk$age_group1182253354455556657758na[in.dat.uk$age_group1182253354455556657758na==8] <- "Prefer not to answer"

#install.packages("labelled")
#library(labelled)
in.dat.ou$age[in.dat.ou$age<=24] <- "18-24"
in.dat.ou$age[in.dat.ou$age>=25 & in.dat.ou$age <=34] <- "25-34"
in.dat.ou$age[in.dat.ou$age>=35 & in.dat.ou$age <=44] <- "35-44"

## Source files
# Used to put multiple graphs on a single pdf 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## Fix condition name for OU
names(in.dat.ou)[3] <- "condition"

## Make sure condition is a factor
in.dat.ou$condition <- factor(in.dat.ou$condition, levels=c(1, 2, 3))
# Now relevel so the mass == 3; riot ==2 and event ==1


# ## First plot a correlation matrix of all of the levels of harm
# corrplot(cor(in.dat.ou[,4:38]))
# corrplot(cor(in.dat.ou[which(in.dat.ou$condition=="1"),4:38]))
# corrplot(cor(in.dat.ou[which(in.dat.ou$condition=="2"),4:38]))
# corrplot(cor(in.dat.ou[which(in.dat.ou$condition=="3"),4:38]))

## Now explore polychoric
# polychoric(in.dat.ou[,4:7])

## Kind of want to try a ordinal logistic regression here -- the outcome would be condition
## Lets first just look for any cursory patterns
pdf("./figures/cursoryBoxPlots.pdf")
for(i in 4:38){
  tmp.plot <- ggplot(in.dat.ou, aes(x=condition, y = in.dat.ou[,i])) +
    geom_boxplot(size=.75) +
    geom_jitter() + 
    theme_bw() +
    coord_cartesian(ylim = c(1,7)) +
    ggtitle(paste(names(in.dat.ou)[i]))
  print(tmp.plot)
}
dev.off()

## now facet wrap them
tmp.dat <- melt(in.dat.ou[,c(1, 3, 4:38)], id.vars = c("id", "condition"))
## Now plot it
all.plots <- tmp.dat %>% ggplot(., aes(x=condition, y=value)) +
  geom_boxplot() +
  geom_jitter() +
  theme_bw() +
  facet_wrap(variable ~ .)

## Train ordered logistic regression model
model.formula <- paste("condition ~ ", paste(names(in.dat.ou)[4:38], collapse = "+"), sep = "")
tmp <- polr(model.formula, data=in.dat.ou, Hess = TRUE)
# Not much signal in this model


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

# ## Plot demo vars
# hist(data.target$age)
# corrplot(cor(data.target[,4:30]))
# corrplot(cor(data.target[which(data.target$condition=="1"),4:30]))
# corrplot(cor(data.target[which(data.target$condition=="2"),4:30]))
# corrplot(cor(data.target[which(data.target$condition=="3"),4:30]))

# rm the ou target
#rm(data.target.ou)
## Now melt these so we can run some HLM models
data.hlm <- melt(data.target, id.vars = c("id", "duration", "condition", "reparation", "priorknowledge", "allow", "gender", "age","cohort"))
## Now plot it
tmp.plot <- tmp.dat %>% ggplot(., aes(x=condition, y=value)) +
  geom_boxplot() +
  geom_jitter() +
  theme_bw() +
  facet_wrap(variable ~ .)

## Now grab the party to be blamed from the end of the varaible names
data.hlm$variable <- as.character(data.hlm$variable)
data.hlm$questionTarg <- substr(data.hlm$variable, nchar(data.hlm$variable), nchar(data.hlm$variable))
data.hlm$questionVal <- substr(data.hlm$variable, 1, nchar(data.hlm$variable)-2)
## Now fix the ID overlap
data.hlm$id <- paste(data.hlm$id, data.hlm$cohort)
## factorize prior knowldge
data.hlm$priorknowledge <- recode(data.hlm$priorknowledge, "1" = 'Prior', "2"  = 'No Prior')
data.hlm$priorknowledge <- factor(data.hlm$priorknowledge, levels=c("Prior", "No Prior"))
##rename questiontarg and condition
data.hlm$questionTarg <- recode(data.hlm$questionTarg, "b" = 'Black Tulsans', "w"  = 'White Tulsans', "p" = "Tulsa Police")
data.hlm$condition <- recode(data.hlm$condition, "1" = 'Riot', "2"  = 'Massacre', "3" = "Event")
data.hlm<-data.hlm %>% 
  rename(Knowledge  = priorknowledge,
         Target = questionTarg,
         Item = questionVal,
         Valence = condition,
         Judgment = value
  )
head(data.hlm)

#data split by knowledge
data.hlm_know<-filter(data.hlm, Knowledge == "Prior")
data.hlm_noknow<-filter(data.hlm, Knowledge == "No Prior")
#data split by condition among prior knowled
data.hlm_event<-filter(data.hlm, Valence == "Event")
data.hlm_mass<-filter(data.hlm, Valence == "Massacre")
data.hlm_riot<-filter(data.hlm, Valence == "Riot")

data.hlm.K_event<-filter(data.hlm_know, Valence == "Event")
data.hlm.K_massacre<-filter(data.hlm_know, Valence == "Massacre")
data.hlm.K_riot<-filter(data.hlm_know, Valence == "Riot")
#library(pbkrtest)
## Now model these
mod.zero <- lmerTest::lmer(Judgment ~ (1|id), data=data.hlm)
mod.one <- lmerTest::lmer(Judgment ~ (Valence + Knowledge + Target + Item)^4 + cohort +(1|id), data=data.hlm)
mod.two <- lmerTest::lmer(Judgment ~ (Valence + Knowledge + Target + Item)^4 + cohort + age +(1|id), data=data.hlm)
#summary(mod.one)
aov<-anova(mod.one, ddf = "Satterthwaite", type = 3)
aov<-round(aov, 2)
aov<-aov[,-c(1:4)]
#aov reduced table
aov$`Pr(>F)`
library(qvalue)
#qobj <- qvalue(aov$`Pr(>F)`, lambda=0.5, pfdr=TRUE)
qobj <- qvalue(p = aov$`Pr(>F)`, fdr.level=0.05, pi0.method="bootstrap", adj=1.2)
aov$fdr.p.adj <- p.adjust(p = aov$`Pr(>F)`, method = "fdr", n = length(aov$`Pr(>F)`))
aov$fdr.p.adj <- round(aov$fdr.p.adj, 2)
aov$qvals <- round(qobj$qvalues, 2)
View(aov)

#### compute predvals and include in data.hlm ####
predict(mod.one)
data.hlm$predVals <- predict(mod.one)

#### ggplot ####
library(jtools)
library(ggplot2)
library(ggsignif)
#https://quantdev.ssri.psu.edu/tutorials/five-ish-steps-create-pretty-interaction-plots-multi-level-model-r
#barplot - ondition x target x knowledge
ggplot(data.hlm, aes(x=Target, y=predVals, color=Knowledge, group=as.factor(Knowledge), fill=as.factor(Knowledge))) +
  geom_bar(stat = "summary", position = "dodge", color = "black") + facet_wrap(.~condition) + labs(title = "", x= "Target Group", y="Judgment Value", color="", fill="") + 
  theme_apa() + scale_fill_manual(values = c("Prior" = "black", "No Prior" = "dark gray"))+scale_y_continuous(breaks=seq(1,7,1))
  #+geom_signif(comparisons = list(c("Prior", "No Prior")), map_signif_level=TRUE)
  #+theme(text=element_text(size=20))
##

#knowledge by target interaction
ggplot(data.hlm, aes(x=Knowledge, y=predVals, color=Target, group=as.factor(Target), fill=as.factor(Target))) +
  geom_bar(stat = "summary", position = "dodge", color = "black") + labs(title = "", x= "Knowledge", y="Judgment Value", color="", fill="") + 
  theme_apa() + scale_fill_manual(values = c("Black Tulsans" = "black", "Tulsa Police" = "dark gray","White Tulsans" = "White"))+
  scale_y_continuous(breaks=seq(1,7,1))
  #+geom_signif(comparisons = list(c("Prior", "No Prior", "Black Tulsans", "White Tulsans", "Tulsa Police")), map_signif_level=TRUE)

#knowledge x target x item
ggplot(data.hlm, aes(x=Item, y=predVals, color=Knowledge, group=as.factor(Knowledge), fill=as.factor(Knowledge))) +
  geom_bar(stat = "summary", position = "dodge", color = "black") + facet_wrap(.~Target) + labs(title = "", x= "Item", y="Judgment Value", color="", fill="") + 
  theme_apa() + scale_fill_manual(values = c("Prior" = "black", "No Prior" = "dark gray"))+
  scale_y_continuous(breaks=seq(1,7,1))

#target x rating x condition
ggplot(data.hlm, aes(x=Item, y=predVals, color=Target, group=as.factor(Target), fill=as.factor(Target))) +
  geom_bar(stat = "summary", position = "dodge", color = "black") + facet_wrap(.~condition) + labs(title = "", x= "Item", y="Judgment Value", color="", fill="") + 
  theme_apa() + scale_fill_manual(values = c("Black Tulsans" = "black", "Tulsa Police" = "dark gray","White Tulsans" = "White"))+
  scale_y_continuous(breaks=seq(1,7,1))




#### other plots #####

#difflsmeans(mod.one,test.effs="condition:Knowledge:Target")
library(emmeans)
library(see)
library(patchwork)
library(effects)
library(glmmTMB)
emmeans(mod.one, list(pairwise ~ condition:Knowledge:Target), adjust = "FDR", ddf = "Satterthwaite")
emmeans(mod.one, list(pairwise ~ condition:Knowledge:Target), adjust = "Tukey")
difflsmeans(mod.one, test.effs = "condition", ddf="Kenward-Roger")
library(rstatix)

# library(EMAtools)
# #formula version combines these functions
# dscores<-round(lme.dscore(mod = mod.one, data = data.hlm, type = "lme4"), 2)
# 
# data.hlm[ sum=sum(Knowledge)]  # Add grouped column
# data_grouped      

table(data.hlm$Knowledge, data.hlm$id)

plot_model(mod.one, type = "pred", terms = c("Item", "Target", "condition"), 
              axis.lim = c(1,8), colors = "gs", title = "", axis.title = "Judgment Value", dot.size = 4)+theme_apa()

options(max.print=c(9999999))
#emm_options(pbkrtest.limit = 8262)
#condition x knowledge x target
plot_model(mod.one, type = "int", terms = c("Target", "Knowledge", "condition"), axis.lim = c(1,8), )
#knowledge x target x item
plot_model(mod.one, type = "pred", terms = c("Item", "Knowledge", "Target"), axis.lim = c(1,8))
#target x item
plot_model(mod.one, type = "pred", terms = c("Item", "Target"), axis.lim = c(1,8))
#knowledge x target
plot_model(mod.one, type = "pred", terms = c("Target", "Knowledge"), axis.lim = c(1,8))
#target
plot_model(mod.one, type = "pred", terms = c("Target"), axis.lim = c(1,8))
#item
plot_model(mod.one, type = "pred", terms = c("Item"), axis.lim = c(1,8))
#cohort
plot_model(mod.one, type = "pred", terms = c("cohort"), axis.lim = c(1,8))
#target x condition 
plot_model(mod.one, type = "pred", terms = c("Target", "condition"), axis.lim = c(1,8))


plot_model(mod.one, type = "pred", axis.lim = c(1,8))
plot_model(mod.one, type = "pred", terms = c("Item", "Knowledge", "Target"), axis.lim = c(1,8))
plot_model(mod.one, type = "pred", terms = c("Target","condition"), axis.lim = c(1,8))
plot_model(mod.one, type = "pred", terms = c("condition","Knowledge","Target"), axis.lim = c(1,8))
#raw means
means<-data.hlm %>%
  group_by(Target, condition, Knowledge) %>%
  summarise(mean_run = mean(value))
view(means)



## No significant cohort effect or any demographic effects really
mod.two <- lmerTest::lmer(value ~ variable * condition + (1|id), data=data.hlm) ## No sig interaction
## Now see if different conditions assign different levels across targets
mod.three <- lmerTest::lmer(value ~ questionTarg * condition + priorknowledge + (1|id), data=data.hlm)
## Now try questionTarg by questionVal
mod.four <- lmerTest::lmer(value ~ (questionTarg + questionVal + condition)^2 + (1|id), data=data.hlm)
## Now try priorKnowledge effects
mod.five <- lmerTest::lmer(value ~ (priorknowledge + questionTarg + questionVal + condition)^3 + gender+cohort+age+(1|id), data=data.hlm) ## sig interaction here
## Looks like apotentially intresting sig interaction here
## For the three way effect
## lets try to plot it
p1 <- visreg(mod.five, "questionTarg", by="questionVal", cond=list(priorknowledge=1), gg=T) + theme_bw() + ggtitle("priorknowledge=1") + coord_cartesian(ylim=c(0,7))
p2 <- visreg(mod.five, "questionTarg", by="questionVal", cond=list(priorknowledge=2), gg=T) + theme_bw() + ggtitle("priorknowledge=2") + coord_cartesian(ylim=c(0,7))
multiplot(p1, p2, cols = 1)

## Now look at priorknowledge:questionTarg:condition interaction
p1 <- visreg(mod.five, "questionTarg", by="condition",cond=list(priorknowledge=1), gg=T) + ggtitle("priorknowledge==1")
p2 <- visreg(mod.five, "questionTarg", by="condition",cond=list(priorknowledge=2), gg=T) + ggtitle("priorknowledge==2")
multiplot(p1, p2, cols=1)
mod.six <- lmerTest::lmer(value ~ priorknowledge + (1|id), data=data.hlm)
mod.sev <- lmerTest::lmer(value ~ (condition+gender + questionTarg + questionVal + priorknowledge)^3 + (1|id), data=data.hlm)
## Now there is a clear gender effect
p1 <- visreg(mod.sev, "condition", by="questionTarg", cond=list(gender=1), gg = T)  + coord_cartesian(ylim=c(0,8)) + ggtitle("Gender==1")
p2 <- visreg(mod.sev, "condition", by="questionTarg", cond=list(gender=2), gg = T)  + coord_cartesian(ylim=c(0,8)) + ggtitle("Gender==2")
p3 <- visreg(mod.sev, "condition", by="questionTarg", cond=list(gender=4), gg = T)  + coord_cartesian(ylim=c(0,8)) + ggtitle("Gender==4")
multiplot(p1, p2, p3, cols = 1)
## Now look for a race effect? --> no

## Now look into prior knowledge specific interactions
data.hlm.pk.1 <- data.hlm[which(data.hlm$priorknowledge==1),]
data.hlm.pk.2 <- data.hlm[which(data.hlm$priorknowledge==2),]

## Now run these same interactions
mod.five.1 <- lmerTest::lmer(value ~ (questionTarg + questionVal + condition+ gender+ age + cohort)^2+(1|id), data=data.hlm.pk.1) ## sig interaction here
mod.five.2 <- lmerTest::lmer(value ~ (questionTarg + questionVal + condition+ gender+ age + cohort)^2 + gender+ age+(1|id), data=data.hlm.pk.2) ## sig interaction here
## No condition effects from prior knowledge == 2 - however big condition effects from ==1
visreg(mod.five.1, "condition", by="questionTarg", overlay=T)
visreg(mod.five.1, "age", by="questionTarg", overlay=T)
visreg(mod.five.2, "condition", by="questionTarg", overlay=T)

## Now run through each question to see if any univaraite differences
all.mods <- list()
for(i in unique(data.hlm.pk.1$questionTarg)){
  mod.tmp <- lm(value ~ questionVal+condition + gender + age, data = data.hlm.pk.1[which(data.hlm.pk.1$questionTarg==i),])
  print(anova(mod.tmp))
  all.mods[[i]] <- mod.tmp
}
## Now look into the condition main effect for racial assignment
summary(all.mods[[2]])
visreg(all.mods[[2]], "questionVal")
visreg(all.mods[[2]], "condition", gg=T) + coord_cartesian(ylim=c(5,8))


## Now try polychorics across datasets
cor.mat <- psych::polychoric(data.target[,4:30])$rho
cor.mat <- psych::polychoric(data.target[,seq(4, 30, 3)])$rho
psych::scree(cor.mat)
cor.mat <- psych::polychoric(data.target[,seq(5, 30, 3)])$rho
psych::scree(cor.mat)
cor.mat <- psych::polychoric(data.target[,seq(6, 30, 3)])$rho
psych::scree(cor.mat)

corrplot(cor.mat, title = "BLACK")
## Plot a scree plot
## Maybe run an FA on this?
mod.fa <- fa(r=data.target[,4:30], n.obs = 306, nfactors = 3)
mod.fa <- fa.poly(data.target[,4:30], n.obs = 306, nfactors = 3)
## Now grab the scores
score.dat <- mod.fa$scores$scores
data.target <- cbind(data.target, score.dat)
# Throw some lm's at these scores
mod.one <- lm(MR1 ~ condition + cohort * priorknowledge, data=data.target) # sig cohort effect - no interaction
mod.two <- lm(MR2 ~ condition + cohort, data=data.target)
mod.thr <- lm(MR3 ~ condition + cohort, data=data.target)
mod.fou <- lm(reparation ~ priorknowledge, data=data.target)
anova(mod.one)
anova(mod.two)
anova(mod.thr)

## Now write a csv for deep learning for shits and gigis
write.csv(data.target, "~/Desktop/forTpot.csv", quote=F, row.names=F)
