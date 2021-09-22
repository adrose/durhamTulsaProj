### Load library(s)
library(ggplot2)
library(corrplot)
require(MASS)
require(Hmisc)
require(reshape2)
library(psych)
library(visreg)
source("~/adroseHelperScripts/R/afgrHelpFunc.R")

### Load data
in.dat.ou <- read.csv("./data/TRM_OUsample.csv")
in.dat.uk <- read.csv("./data/TRM_UKsample.csv")


## Fix condition name for OU
names(in.dat.ou)[3] <- "condition"

## Make sure condition is a factor
in.dat.ou$condition <- factor(in.dat.ou$condition, levels=c(3, 1, 2))
# Now relevel so the mass == 3; riot ==2 and event ==1


## First plot a correlation matrix of all of the levels of harm
corrplot(cor(in.dat.ou[,4:38]))
corrplot(cor(in.dat.ou[which(in.dat.ou$condition=="1"),4:38]))
corrplot(cor(in.dat.ou[which(in.dat.ou$condition=="2"),4:38]))
corrplot(cor(in.dat.ou[which(in.dat.ou$condition=="3"),4:38]))

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
# rm the ou target
#rm(data.target.ou)
## Now melt these so we can run some HLM models
data.hlm <- melt(data.target, id.vars = c("id", "duration", "condition", "reparation", "priorknowledge", "allow", "gender", "age","cohort"))
## Now grab the party to be blamed from the end of the varaible names
data.hlm$variable <- as.character(data.hlm$variable)
data.hlm$questionTarg <- substr(data.hlm$variable, nchar(data.hlm$variable), nchar(data.hlm$variable))
data.hlm$questionVal <- substr(data.hlm$variable, 1, nchar(data.hlm$variable)-2)
## Now fix the ID overlap
data.hlm$id <- paste(data.hlm$id, data.hlm$cohort)

## Now model these
mod.one <- lmerTest::lmer(value ~ variable + cohort + gender + age + condition + priorknowledge + (1|id), data=data.hlm)
## No significant cohort effect or any demographic effects really
mod.two <- lmerTest::lmer(value ~ variable * condition + (1|id), data=data.hlm) ## No sig interaction
## Now see if different conditions assign different levels across targets
mod.three <- lmerTest::lmer(value ~ questionTarg * condition + priorknowledge + (1|id), data=data.hlm)
## Now try questionTarg by questionVal
mod.four <- lmerTest::lmer(value ~ (questionTarg + questionVal + condition)^2 + (1|id), data=data.hlm)
## Now try priorKnowledge effects
mod.five <- lmerTest::lmer(value ~ (priorknowledge * questionTarg + questionVal + condition)^3 + gender+ (1|id), data=data.hlm) ## sig interaction here
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
