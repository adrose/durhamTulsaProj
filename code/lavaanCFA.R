### Load library(s)
library(ggplot2)
library(corrplot)
require(reshape2)
library(psych)
library(visreg)
library(lavaan)
#source("~/adroseHelperScripts/R/afgrHelpFunc.R")
source("~/GitHub/adroseHelperScripts/R/afgrHelpFunc.R")

### Load data
in.dat.ou <- read.csv("./data/TRM_OUsample.csv")
in.dat.uk <- read.csv("./data/TRM_UKsample.csv")


## Fix condition name for OU
names(in.dat.ou)[3] <- "condition"

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
## Make sure condition is a factor
data.target$condition <- factor(data.target$condition)
data.target$priorknowledge <- factor(data.target$priorknowledge)

## Now create an interaction term
tmp.mod <- lm(duration ~ condition * priorknowledge, data=data.target)
data.target <- cbind(data.target, model.matrix(tmp.mod))

## Now specify the lavaan model
myModel.me <- ' # regressions
             f1 + f2 + f3 ~ condition2 + condition3 + priorknowledge2

             # latent variable definitions 
               f1 =~ violence.b + blame.b + caused.b + intention.b + punish.b + allow.commit.b + knew.b + should.b + could.b  
               f2 =~ violence.p + blame.p + caused.p + intention.p + punish.p + allow.commit.p + knew.p + should.p + could.p 
               f3 =~ violence.w + blame.w + caused.w + intention.w + punish.w + allow.commit.w + knew.w + should.w + could.w

             # variances and covariances 
               f1 ~~ f2 + f3
               f2 ~~ f3
               f1 ~~ 1*f1
               f2 ~~ 1*f2
               f3 ~~ 1*f3
  '

myModel.int <- ' # regressions
             f1 + f2 + f3 ~ condition2 + condition3 + priorknowledge2 + condition2:priorknowledge2 + condition3:priorknowledge2

             # latent variable definitions 
               f1 =~ violence.b + blame.b + caused.b + intention.b + punish.b + allow.commit.b + knew.b + should.b + could.b  
               f2 =~ violence.p + blame.p + caused.p + intention.p + punish.p + allow.commit.p + knew.p + should.p + could.p 
               f3 =~ violence.w + blame.w + caused.w + intention.w + punish.w + allow.commit.w + knew.w + should.w + could.w

             # variances and covariances 
               f1 ~~ f2 + f3
               f2 ~~ f3
               f1 ~~ 1*f1
               f2 ~~ 1*f2
               f3 ~~ 1*f3
  '
fit <- sem(myModel.me, data=data.target, std.lv=TRUE, group = "priorknowledge")
fit.l <- sem(myModel.me, data=data.target, std.lv=TRUE, group = "priorknowledge", group.equal="loadings")
fit.li <- sem(myModel.me, data=data.target, std.lv=TRUE, group = "priorknowledge", group.equal=c("loadings","intercepts"))
lavTestLRT(fit, fit.l, fit.li)
fit <- sem(myModel.me, data=data.target, std.lv=TRUE)
fit2 <- sem(myModel.int, data=data.target, std.lv=TRUE)
fit.cat <- sem(myModel.me, data=data.target, std.lv=TRUE, estimator ="ML")
lavTestLRT(fit, fit2)


### Now just try a CFA approach
myModel.cfa <- ' # regressions
             # latent variable definitions 
               f1 =~ violence.b + blame.b + caused.b + intention.b + punish.b + allow.commit.b + knew.b + should.b + could.b  
               f2 =~ violence.p + blame.p + caused.p + intention.p + punish.p + allow.commit.p + knew.p + should.p + could.p 
               f3 =~ violence.w + blame.w + caused.w + intention.w + punish.w + allow.commit.w + knew.w + should.w + could.w

             # variances and covariances 
               f1 ~~ f2 + f3
               f2 ~~ f3
               f1 ~~ 1*f1
               f2 ~~ 1*f2
               f3 ~~ 1*f3 
'
fit.cfa <- lavaan::cfa(model=myModel.cfa, data=data.target)
## Now grab the factor scores and train a model
data.target <- cbind(data.target, predict(fit.cfa))
# NOw try some linear models
mod.1 <- lm(f1 ~ condition * priorknowledge, data=data.target)
mod.2 <- lm(f2 ~ condition * priorknowledge, data=data.target)
mod.3 <- lm(f3 ~ condition * priorknowledge, data=data.target)
## Now try a HLM
tmp.dat <- data.target[,c("f1", "f2", "f3", "condition", "priorknowledge", "gender", "age", "reparation", "cohort", "id")]
# Now melt it
hlm.dat <- reshape::melt(tmp.dat, id.vars = c("id", "condition", "reparation", "priorknowledge", "gender", "age","cohort"))
# Now model it
mod.1 <- lm(value ~ (variable + condition + priorknowledge)^2 + cohort , data=hlm.dat)
