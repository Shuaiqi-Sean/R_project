# SET ENVIRONMENT
setwd("~/Documents/")

library(here)
library(data.table)
library(gmodels)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ROCR)

# LOAD & MASSAGE DATA
dt <- fread(here("titanic_data.csv"))
var_list <- c("Survived", "Pclass", "Sex", "Cabin", "Embarked")
for (var in var_list){
  dt[,(var):= as.factor(get(var))]
}

# SIMPLE TREE
  # RANDOM SAMPLE
  dt <- dt[sample(nrow(dt)),]

  # SPLIT
  ratio=0.7
  split = round(nrow(dt)*ratio)
  dt_train <- dt[1:split,]
  dt_test <- dt[(split+1):nrow(dt),]
  
  # BUILD TREE
  simple_tree <- rpart(Survived~ Sex+Age, dt_train, method = "class")
  simple_tree
  
  # RESULT
  plot(simple_tree)
  rpart.plot(simple_tree)
  
  op <- par(mfrow=c(2,4), pty="s")
  rpart.plot(simple_tree, extra=106, type=4, main="e106t4", clip.right.labs = T, faclen = 0)
  
    # RATTLE
    op <- par(mfrow=c(1,2), pty="s")
    fancyRpartPlot(simple_tree)
    
  # CHECK IMPORTANT
  simple_tree$variable.importance
  simple_tree$frame 
  summary(simple_tree)
  
  # VERIFY
  test <- predict(simple_tree, dt_test, type = "prob")[,2]
  pred <- prediction(as.double(as.vector(test)), dt_test$Survived)
  
  perf <- performance(pred, "tpr", "fpr")
  par(mar=c(5,5,2,2), xaxs="i", yaxs="i",cex.axis=1.3, cex.lab=1.4)
  plot(perf)
  abline(a=0, b=1, lwd=2, lty=2, col="gray")
  
  auc <- performance(pred, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc
    
  acc.perf <- performance(pred, measure="acc")
  plot(acc.perf)
  
  # CHECK CUT OFF
  ind <- which.max(slot(acc.perf, "y.values")[[1]])
  acc <- slot(acc.perf, "y.values")[[1]][ind]
  cutoff <- slot(acc.perf, "x.values")[[1]][ind]
  print(c(accuracy= acc, cutoff= cutoff))
  
  # F1 SCORES
  f1.perf <- performance(pred, measure = "f")
  plot(f1, pefr)
  ind <- which.max(slot(acc.perf, "y.values")[[1]])
  f1 <- slot(f1.perf, "y.values")[[1]][ind]
  cutoff <- slot(f1.perf, "x.values")[[1]][ind]
  print(c(f1=f1, cutoff=cutoff))
  
  # TEST ACCURACY
  res <- dt_test$Survived == ifelse(test>cutoff,1,0)
  correct = length(res[res==T])
  accuracy <- correct/nrow(dt_test)
  accuracy
  
  # PRECISION & RECURSIVE CURVE
  perf1 <- performance(pred, "prec", "rec")
  plot(perf1)
  
  # SENSITIVITY SPECIFICITY CURVE
  perf1 <- performance(pred, "sens", "spec")
  plot(perf1)
  
  # OVERFITTING & PRUNING
  plotcp(simple_tree)
  
    # PRUNE
    simple_tree_prune <- prune(simple_tree, cp = 0.005)
    fancyRpartPlot(simple_tree_prune)
    fancyRpartPlot(simple_tree)
  
# SAVE MODEL
save(simple_tree,file = here("simple_tree.model"))
rm(simple_tree)  
load(here("simple_tree.model"))
