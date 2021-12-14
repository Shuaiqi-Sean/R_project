# SET ENVIRONMENT
setwd("~/Documents/")

library(here)
library(data.table)
library(h2o)
library(ROCR)
h2o.init()

# LOAD DATA
dt <- h2o.importFile(here("SPrize_2014_data.csv"), destination_frame = "dt")

Y <- "TARG_CANCEL"
X <- c("ST_CD","UBI_IND","ANY_NON_INSTALLERS","AUTO_QT_IND",
       "OTHER_QT_IND","HO_QT_IND","RW","POL_EFF_DT","CRED_SCORE_CD","AGE_GRP1_IND","AGE_GRP2_IND","AGE_GRP3_IND",
       "AGE_GRP4_IND","AGE_GRP5_IND","AGE_GRP6_IND","AGE_GRP7_IND","AGE_GRP8_IND","INCP_DRVR_CNT","INCP_VEH_CNT",
       "DUI_VIOL_CNT","LENGTH_PRIR_INS_CD","OMITTED_INCIDENT_CNT","PNI_AGE_GRP","INCPT_POL_FULL_COV_CD",
       "POL_RT_STBL_FCT","INCP_PRFRD_IND","HO","BILL_PLN_CD","NBR_MO_CLN","NBR_OF_PAY","LATE_PAY_CNT",
       "ORG_REN_QT_PREM_R1","ORG_REN_QT_PREM_R2","ORG_PREM_CHNG_AMT_R2","MIN_MODEL_YR","MAX_MODEL_YR",
       "VEHICLES_TOTAL","VEHICLES_QUOTE","DRIVERS_TOTAL","DRIVERS_QUOTE","DRVR_CHG","VEH_CHG","RR_TYPE")

var_list <- c("TARG_CANCEL", "ST_CD", "UBI_IND", "AUTO_QT_IND", "OTHER_QT_IND", "HO_QT_IND", "RW", 
              "PNI_AGE_GRP", "MIN_MODEL_YR", "MAX_MODEL_YR", "DRVR_CHG", "VEH_CHG")
for(var in var_list){
  dt[[var]] <- as.factor(dt[[var]])
  print(var)
}

# SPLIT DATA
  # dt_split <- h2o.splitFrame(dt, ratio=0.7)
  # train <- dt_split[[1]]
  # test <- dt_split[[2]]
  # 
  # train_split <- h2o.splitFrame(train, ratio=0.8)
  # train_80 <- train_split[[1]]
  # train_20 <- train_split[[2]]
split <- h2o.runif(dt,1234)
train <- h2o.assign(dt[split<0.7,], "train")
test <- h2o.assign(dt[split>=0.7,], "test")

train_hold <- h2o.runif(train, 1234)
train_80 <- h2o.assign(train[train_hold < 0.8,], "train_80")
train_20 <- h2o.assign(train[train_hold >= 0.8,], "train_20")

# GBM
GBM_models <- c()

for (i in 1:10){
  numTrees <- sample(50:200,1)
  maxDepth <- sample(2:5,1)
  minRows <- sample(300:500,1)
  learnRate <- 0.01
  
  model_name <- paste0("GBM_", i, "_ntrees", numTrees, "_maxdepth", maxDepth, "_minrows", minRows)
  
  GBM_model <- h2o.gbm(x=X,
                       y=Y,
                       training_frame = train_80,
                       model_id = model_name,
                       ntrees = numTrees,
                       max_depth = maxDepth,
                       min_rows = minRows,
                       learn_rate = learnRate
                       )
  GBM_models <- c(GBM_models, GBM_model)
}

# FIND BEST MODEL
best_auc <- 0
for(i in 1:length(GBM_models)){
  auc <- h2o.auc(h2o.performance(GBM_models[[i]], train_20))
  if(auc>best_auc){
    best_auc <- auc 
    best_model <- GBM_models[[i]]
  }
}

best_model_par <- best_model@allparameters

# CHECK RESULT
check_list <- c("model_id", "ntrees", "max_depth", "min_rows")
for(check in check_list){
  print(paste(check,">>>",best_model_par[[check]]))
}

# REBUILD 
rebuild_numTrees <- best_model_par$ntrees
rebuild_maxDepth <- best_model_par$max_depth
rebuild_minRows <- best_model_par$min_rows

model_name <- paste0("GBM_rebuild","_ntrees",rebuild_numTrees,"_maxDepth",
                     rebuild_maxDepth,"_minrows",rebuild_minRows,"_learnRate", 0.01)

rebuild_GBM_model <- h2o.gbm(         y=Y,
                                     x=X,
                                     training_frame = train,
                                     model_id = model_name,
                                     ntrees = rebuild_numTrees,
                                     max_depth = rebuild_maxDepth,
                                     min_rows = rebuild_minRows,
                                     learn_rate = learnRate
)

# EVALUATE
GBM_predictions <- h2o.predict(rebuild_GBM_model, test)

GBM_predict <- as.vector(as.data.frame(GBM_predictions$p1))
actuals <- as.vector(as.data.frame(test$TARG_CANCEL))

GBM_pred <- prediction(GBM_predict,actuals)
GBM_perf <- performance(GBM_pred, "tpr", "fpr")
plot(GBM_perf)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

performance(GBM_pred, "auc")
h2o.auc(h2o.performance(rebuild_GBM_model, test))

# F1 SCORE VS CUTOFF
plot(performance(GBM_pred, "f"))

# DETERMIN F1 SCORE AND CUTOFF
f1_perf <- performance(GBM_pred, "f")
indicator <- which.max(slot(f1_perf,"y.values")[[1]])
f1 <- slot(f1_perf,"y.values")[[1]][indicator]
cutoff <- slot(f1_perf,"x.values")[[1]][indicator]
print(c(f1=f1, cutoff = cutoff))

# DOUBLE CHECK WITH H2O
h2o.performance(rebuild_GBM_model, test)

# SAVE MODEL
h2o.saveModel(rebuild_GBM_model, here("final_GBM_model"), force = T)

# DETERMINE IMPORTANT VAR
h2o.varimp(rebuild_GBM_model)

############################
# COMPARE TO RANDOM FOREST #
############################
RF_models <- c()
for (i in 1:5){
  numTrees <- sample(30:50,1)
  maxDepth <- sample(8:12,1)
  minRows <- sample(300:500,1)
  numVars <- -1
  
  model_name <- paste0("RF_",i,"_ntrees", numTrees, "_maxdepth", maxDepth,
                       "minrows", minRows, "numVars",numVars)
  
  RF_model <- h2o.randomForest(y = Y,
                               x = X,
                               training_frame = train_80,
                               model_id = model_name,
                               ntrees = numTrees,
                               max_depth = maxDepth,
                               min_rows = minRows,
                               mtries = numVars
  )
  RF_models <- c(RF_models, RF_model)
}

RF_models

# DETERMINE BEST TREE
best_AUC <- 0
for (i in 1:length(RF_models)){
  auc <- h2o.auc(h2o.performance(RF_models[[i]], train_20))
  if(auc > best_AUC){ 
    best_AUC <- auc
    best_model <- RF_models[[i]]
  }
}

best_model_par <- best_model@allparameters
best_model_par

# CHECK PARAM FROM BEST MODEL
check_list <- c("ntrees", "max_depth", "min_rows", "mtries")
for(check in check_list){
  print(paste(check,">>>",best_model_par[[check]]))
}

# REBUILD TREE ON BEST MODEL
rebuild_numTrees <- best_model_par$ntrees
rebuild_maxDepth <- best_model_par$max_depth
rebuild_minRows <- best_model_par$min_rows
rebuild_numVars <- best_model_par$mtries

model_name <- paste0("RFmodel","_ntrees",rebuild_numTrees,"_maxDepth",
                     rebuild_maxDepth,"_minrows",rebuild_minRows,"_numVars",rebuild_numVars)

rebuild_RF_model <- h2o.randomForest(y=Y,
                                     x=X,
                                     training_frame = train,
                                     model_id = model_name,
                                     ntrees = rebuild_numTrees,
                                     max_depth = rebuild_maxDepth,
                                     min_rows = rebuild_minRows,
                                     mtries = rebuild_numVars
)

# CHECK REBUILD 
RF_prediction <- h2o.predict(rebuild_RF_model, test)
RF_predict <- as.vector(as.data.frame(RF_prediction$p1))
actuals <- as.vector(as.data.frame(test$TARG_CANCEL))

RF_pred <- prediction(RF_predict,actuals)
RF_perf <- performance(RF_pred, "tpr", "fpr")
plot(RF_perf)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

performance(RF_pred, "auc")
h2o.auc(h2o.performance(rebuild_RF_model, test))

plot(performance(RF_pred, "f"))

# DETERMIN F1 SCORE AND CUTOFF
f1_perf <- performance(RF_pred, "f")
indicator <- which.max(slot(f1_perf,"y.values")[[1]])
f1 <- slot(f1_perf,"y.values")[[1]][indicator]
cutoff <- slot(f1_perf,"x.values")[[1]][indicator]
print(c(f1=f1, cutoff = cutoff))

# DOUBLE CHECK WITH H2O
h2o.performance(rebuild_RF_model, test)

# SAVE MODEL
h2o.saveModel(rebuild_RF_model, here("final_RFvsGBM_model"), force = T)

#####################
# RESULT COMPARSION #
#####################

# BEST GBM
# auc - 0.7067567
# f1 - 0.2853142
# cutoff - 0.1151240

# BEST RF
# auc - 0.702849
# f1 - 0.2824046
# cutoff - 0.1351809

