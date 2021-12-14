# SET ENVIRONMENT
setwd("~/Documents/")

library(here)
library(data.table)
library(h2o)
library(ROCR)
h2o.init()

# LOAD DATA
dt <- h2o.importFile(here("special_lines_churn_data.csv"), destination_frame = "dt")

Y <- "RENEW"
X <- c("AGNCY_TYP_CD", "ST_ABBR","CRED_SCORE_CD", "RISK_TYP_CD", "PNI_MRTL_STAT_CD", 
              "MKT","OWN_HM_IND", "RENW_QT_PREM_AMT","BPCURR","EFTCURR", "PREV_TERM_PREM_AMT", "RENW_PREM_CHNG_AMT", 
              "RENW_PREM_CHNG_PCT", "POLCNT", "BP", "CRED_GRP", "RENW_PREM_GRP", "RATECHG", "SYMBOL", "VEHCNT","ORGN_OWN", 
              "PAYEE", "AVG_VEHAGE", "VEHAGE_GRP", "DRVRCNT", "AGE_GRP", "EXPRNC_GRP", "POINT_GRP", "BI_PD", "COLL_COMP", 
              "MP_PIP", "TL", "CLAIM", "QB", "MULTI_POL", "BILLCHG", "NPS", "LOWER_DED", "HIGHER_DED", "DECLIM", "INCLIM", 
              "FUNCOV", "DD", "ADDVEH", "DELVEH", "ADDDRV", "DELDRV", "CHGCOV", "RENEWAL330", "VEHGRP", "DRVRGRP", "EMAIL", 
              "DELEFT", "NPSGRP", "ADDEND", "DELEND", "DEDCHG")

var_list <- c("RENEW","EFTCURR","POLCNT","VEHCNT","VEHAGE_GRP","DRVRCNT","LOWER_DED","HIGHER_DED","DECLIM",
              "INCLIM","FUNCOV","RENEWAL330","VEHGRP","DRVRGRP")
for(var in var_list){
  dt[[var]] <- as.factor(dt[[var]])
  print(var)
}

# SPLIT DATA
dt_split <- h2o.splitFrame(dt, ratio=0.7)
train <- dt_split[[1]]
test <- dt_split[[2]]

train_split <- h2o.splitFrame(train, ratio=0.8)
train_80 <- train_split[[1]]
train_20 <- train_split[[2]]

# RANDOM FOREST
RF_models <- c()
for (i in 1:10){
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
actuals <- as.vector(as.data.frame(test$RENEW))

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
h2o.saveModel(rebuild_RF_model, here("final_RF_model"), force = T)
