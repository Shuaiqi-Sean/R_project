library(data.table)
library(odbc)

# DEFINE VARIABLE
var_name <- 'BIPD_BASE_2021_COVID'

var_lib[[var_name]] <- list()

var_lib[[var_name]][['dependencies']] <- c()

var_lib[[var_name]][['builder']] <- function(...) {
  
  # GET ARGUMENTS
  args <- list(...)
  start_date <- args[['start_date']]
  end_date <- args[['end_date']]
  uid <- args[['uid']]
  pwd <- args[['pwd']]
  
  # ADDITIONAL ARGUMENTS
  # MID MONTH DATES
  mid_start_date <- as.Date(start_date)
  lubridate::day(mid_start_date) <- 15
  mid_end_date <- as.Date(end_date)
  lubridate::day(mid_end_date) <- 15
  # YEAR RANGE
  record_years <- seq(lubridate::year(mid_start_date), lubridate::year(today()))
  record_years_str <- paste0(paste0("'", record_years, "'"), collapse=',')
  # ACCIDENT YEAR LOOKUP
  seq_dates <- seq(as.Date(end_date) %m+% years(-7) %m+% days(1), as.Date(end_date) %m+% months(1), by='d')
  yr_dt <- data.table(ACC_DT=seq_dates, ACC_YR_LVL=7)
  for (i in rev(1:7)) {
    dt <- as.Date(end_date) %m+% years(-i)
    yr_dt[ACC_DT>dt, ACC_YR_LVL:=i]
  }
  # LINE COVERAGES
  lcbi <- c('1910','1983','1919','1901','1909','1908')
  lcpd <- c('2001','2004','2005','2009','2030')
  lcbd <- c(lcbi, lcpd)
  lcbd_str <- paste0(paste0("'", lcbd, "'"), collapse=',')

  # CONNECT TO CAW
  caw_con <- dbConnect(odbc(), 'DB2P', uid=uid, pwd=pwd)
  
  # CONNECT TO PROSTAR
  prostar_con <- dbConnect(odbc(), 'ProStar', uid=uid, pwd=pwd)
  
  # POLICY INFO QUERY
  pol_dates_qry <- "
    SELECT
    	CAW.POLICY.ST_CD,
      CAW.POLICY.POL_ID_NBR,
      CAW.POLICY.RENW_SFX_NBR,
      CAW.POLICY.POL_EXPR_YR,
      CAW.POLICY.PHYS_POL_KEY,
      CAW.POLICY.COH_ID_NBR,
      CAW.POLICY.RENW_CNT,
      CAW.POLICY.COH_INCP_DT,
      CAW.POLICY.COH_STOP_DT,
      CAW.POLICY.POL_RENW_IND,
      CAW.POLICY.POL_TERM,
      CAW.POLICY.RT_REV_DT,
      CAW.POLICY.POL_MOCK_IND,
      CAW.POL_DATES.POL_EFF_DT,
      CAW.POL_DATES.POL_EXPR_DT,
      CAW.POL_DATES.POL_STOP_DT
    FROM
      CAW.POLICY,
      CAW.POL_DATES
    WHERE
      CAW.POL_DATES.RISK_TYP_CD <> 'TN'
      and CAW.POL_DATES.POL_EFF_DT  BETWEEN {d 'startdate'} and {d 'enddate'}
      and CAW.POL_DATES.POL_ID_CHAR = CAW.POLICY.POL_ID_CHAR
      and CAW.POL_DATES.RENW_SFX_NBR = CAW.POLICY.RENW_SFX_NBR
      and CAW.POL_DATES.POL_EXPR_YR = CAW.POLICY.POL_EXPR_YR
    GROUP BY
      CAW.POLICY.ST_CD,
      CAW.POLICY.POL_ID_NBR,
      CAW.POLICY.RENW_SFX_NBR,
      CAW.POLICY.POL_EXPR_YR,
      CAW.POLICY.PHYS_POL_KEY,
      CAW.POLICY.COH_ID_NBR,
      CAW.POLICY.RENW_CNT,
      CAW.POLICY.COH_INCP_DT,
      CAW.POLICY.COH_STOP_DT,
      CAW.POLICY.POL_RENW_IND,
      CAW.POLICY.POL_TERM,
      CAW.POLICY.RT_REV_DT,
      CAW.POLICY.POL_MOCK_IND,
      CAW.POL_DATES.POL_EFF_DT,
      CAW.POL_DATES.POL_EXPR_DT,
      CAW.POL_DATES.POL_STOP_DT
    ;
  "
  # INSERT DATES
  pol_dates_qry <- str_replace_all(pol_dates_qry, 'startdate', start_date)
  pol_dates_qry <- str_replace_all(pol_dates_qry, 'enddate', end_date)
  # QUERY DATA
  pol_dt <- as.data.table(dbGetQuery(caw_con, pol_dates_qry))
  # REMOVE DUPLICATES
  first_inds <- pol_dt[, .I[1], by=.(ST_CD, PHYS_POL_KEY)]$V1
  pol_dt <- pol_dt[first_inds] # POLICY

  # COVERAGE INFO QUERY
  cov_qry <- "
    SELECT
      CAW.MNTHLY_LCL_PL.ST_CD,
      CAW.MNTHLY_LCL_PL.PHYS_POL_KEY,
      CAW.MNTHLY_LCL_PL.PHYS_VEH_KEY,
      CAW.MNTHLY_LCL_PL.PHYS_DRVR_KEY,
      CAW.MNTHLY_LCL_PL.PHYS_COV_KEY,
      CAW.MNTHLY_LCL_PL.VEH_POS_CNT,
      CAW.MNTHLY_LCL_PL.VEH_VRSN_NBR,
      CAW.MNTHLY_LCL_PL.DRVR_POS_CNT,
      CAW.MNTHLY_LCL_PL.DRVR_VRSN_NBR,
      CAW.MNTHLY_LCL_PL.LINE_COV_CD,
      CAW.MNTHLY_LCL_PL.LIM_CD,
      CAW.COV_DATES.COV_STRT_DT,
      CAW.COV_DATES.COV_STOP_DT,
      CAW.COV_DATES.SYS_MOD_DT,
      SUM(CAW.MNTHLY_LCL_PL.ERN_EXPS_CNT) AS ERN_EXPS_CNT,
      SUM(CAW.MNTHLY_LCL_PL.ERN_PREM_AMT) AS ERN_PREM_AMT,
      SUM(CAW.MNTHLY_LCL_PL.WRT_EXPS_CNT) AS WRT_EXPS_CNT,
      SUM(CAW.MNTHLY_LCL_PL.WRT_PREM_AMT) AS WRT_PREM_AMT
    FROM
      CAW.MNTHLY_LCL_PL,
      CAW.COV_DATES,
      CAW.POL_DATES
    WHERE
      CAW.MNTHLY_LCL_PL.CALEN_DT <= {d 'enddate'}
      AND CAW.POL_DATES.POL_EFF_DT BETWEEN {d 'startdate'} and {d 'enddate'}
      AND CAW.MNTHLY_LCL_PL.LINE_COV_CD IN ('1910','1983','1919','1901','1909','1908','2001','2004','2005','2009','2030')
      AND CAW.MNTHLY_LCL_PL.POL_ID_CHAR = CAW.COV_DATES.POL_ID_CHAR
      AND CAW.MNTHLY_LCL_PL.RENW_SFX_NBR = CAW.COV_DATES.RENW_SFX_NBR
      AND CAW.MNTHLY_LCL_PL.POL_EXPR_YR = CAW.COV_DATES.POL_EXPR_YR
      AND CAW.MNTHLY_LCL_PL.VEH_POS_CNT = CAW.COV_DATES.VEH_POS_CNT
      AND CAW.MNTHLY_LCL_PL.VEH_VRSN_NBR = CAW.COV_DATES.VEH_VRSN_NBR
      AND CAW.MNTHLY_LCL_PL.DRVR_POS_CNT = CAW.COV_DATES.DRVR_POS_CNT
      AND CAW.MNTHLY_LCL_PL.DRVR_VRSN_NBR = CAW.COV_DATES.DRVR_VRSN_NBR
      AND CAW.MNTHLY_LCL_PL.LINE_COV_CD = CAW.COV_DATES.LINE_COV_CD
      AND CAW.MNTHLY_LCL_PL.LIM_CD = CAW.COV_DATES.LIM_CD
      AND CAW.MNTHLY_LCL_PL.POL_ID_CHAR = CAW.POL_DATES.POL_ID_CHAR
      AND CAW.MNTHLY_LCL_PL.RENW_SFX_NBR = CAW.POL_DATES.RENW_SFX_NBR
      AND CAW.MNTHLY_LCL_PL.POL_EXPR_YR = CAW.POL_DATES.POL_EXPR_YR
    GROUP BY
      CAW.MNTHLY_LCL_PL.ST_CD,
      CAW.MNTHLY_LCL_PL.PHYS_POL_KEY,
      CAW.MNTHLY_LCL_PL.PHYS_VEH_KEY,
      CAW.MNTHLY_LCL_PL.PHYS_DRVR_KEY,
      CAW.MNTHLY_LCL_PL.PHYS_COV_KEY,
      CAW.MNTHLY_LCL_PL.VEH_POS_CNT,
      CAW.MNTHLY_LCL_PL.VEH_VRSN_NBR,
      CAW.MNTHLY_LCL_PL.DRVR_POS_CNT,
      CAW.MNTHLY_LCL_PL.DRVR_VRSN_NBR,
      CAW.MNTHLY_LCL_PL.LINE_COV_CD,
      CAW.MNTHLY_LCL_PL.LIM_CD,
      CAW.COV_DATES.COV_STRT_DT,
      CAW.COV_DATES.COV_STOP_DT,
      CAW.COV_DATES.SYS_MOD_DT
      ;
    "
  # INSERT DATES
  cov_qry <- str_replace_all(cov_qry, 'startdate', start_date)
  cov_qry <- str_replace_all(cov_qry, 'enddate', end_date)
  # QUERY DATA
  cov_dt <- as.data.table(dbGetQuery(caw_con, cov_qry))
  
  # SORT AND REMOVE DUPLICATES
  cov_dt <- cov_dt[order(ST_CD, PHYS_COV_KEY, SYS_MOD_DT)]
  last_inds <- cov_dt[, .I[.N], by=.(ST_CD, PHYS_COV_KEY)]$V1
  cov_dt <- cov_dt[last_inds]

  # LIST OF POLICIES IN DATASET
  pol_list <- unique(pol_dt[, .(ST_CD, PHYS_POL_KEY, POL_EXPR_YR, POL_ID_NBR, RENW_SFX_NBR)])

  # VEHICLE INFO QUERY
  veh_qry <- "
    SELECT
    	CAW.VEHICLE.ST_CD,
      CAW.VEHICLE.PHYS_POL_KEY,
      CAW.VEHICLE.PHYS_VEH_KEY,
      CAW.VEHICLE.VIN,
      CAW.BODYTYP.TRLR_OR_POWER
    FROM
      CAW.VEHICLE,
      CAW.POL_DATES,
      CAW.BODYTYP
    WHERE
      CAW.POL_DATES.POL_EFF_DT BETWEEN {d 'startdate'} and {d 'enddate'}
      AND CAW.POL_DATES.POL_ID_CHAR = CAW.VEHICLE.POL_ID_CHAR
      AND CAW.POL_DATES.RENW_SFX_NBR = CAW.VEHICLE.RENW_SFX_NBR
      AND CAW.POL_DATES.POL_EXPR_YR = CAW.VEHICLE.POL_EXPR_YR
      AND CAW.VEHICLE.VEH_BODY_TYP = CAW.BODYTYP.VEH_BODY_TYP
    GROUP BY
      CAW.VEHICLE.ST_CD,
      CAW.VEHICLE.PHYS_POL_KEY,
      CAW.VEHICLE.PHYS_VEH_KEY,
      CAW.VEHICLE.VIN,
      CAW.BODYTYP.TRLR_OR_POWER
    ;
  "
  # INSERT DATES
  veh_qry <- str_replace_all(veh_qry, 'startdate', start_date)
  veh_qry <- str_replace_all(veh_qry, 'enddate', end_date)
  # QUERY DATA
  veh_dt <- as.data.table(dbGetQuery(caw_con, veh_qry))
  # REMOVE DUPLICATES, IF ANY
  first_inds <- veh_dt[, .I[1], by=.(ST_CD, PHYS_POL_KEY, PHYS_VEH_KEY)]$V1
  veh_dt <- veh_dt[first_inds]
  # MERGE ONTO COVERAGE DATA
  cov_dt <- veh_dt[cov_dt, on=.(ST_CD, PHYS_POL_KEY, PHYS_VEH_KEY)]

  # SPLIT DATASET INTO POWER/NON-POWER UNITS
  pu_dt <- cov_dt[TRLR_OR_POWER=='P']
  trl_dt <- cov_dt[TRLR_OR_POWER!='P']

  # CLEAN UP POWER UNIT DATA USING VIN
  # GET MOST RECENT VIN FOR EACH VEHICLE
  pu_dt[, MAX_VIN:=VIN[which.max(VEH_VRSN_NBR)], by=.(ST_CD, PHYS_POL_KEY, VEH_POS_CNT)]
  # DELETE SAME DAY COVERAGE START/STOPS
  pu_dt <- pu_dt[COV_STRT_DT!=COV_STOP_DT]
  # DELETE DUPLICATE/SUBSET COVERAGE PERIODS FOR EACH VIN
  pu_dt <- pu_dt[order(PHYS_POL_KEY, MAX_VIN, LINE_COV_CD, COV_STRT_DT, -SYS_MOD_DT)]
  # GET START AND STOP DATE FOR MOST RECENT COVERAGE RECORD PER VIN
  pu_dt[, PR_COV_STRT:=COV_STRT_DT[1], by=.(PHYS_POL_KEY, MAX_VIN, LINE_COV_CD, COV_STRT_DT, SYS_MOD_DT)]
  pu_dt[, PR_COV_STOP:=COV_STOP_DT[1], by=.(PHYS_POL_KEY, MAX_VIN, LINE_COV_CD, COV_STRT_DT, SYS_MOD_DT)]
  # INDEX THE COVERAGE RECORDS WITHIN EACH VIN
  pu_dt[, I:=1:.N, by=.(PHYS_POL_KEY, MAX_VIN, LINE_COV_CD, COV_STRT_DT)]
  # FOR RECORDS OTHER THAN THE FIRST, IF THE STOP DATE IS WITHIN THE FIRST RECORD'S COVERAGE PERIOD, DELETE
  pu_dt <- pu_dt[!(I>1 & COV_STOP_DT > PR_COV_STRT & COV_STOP_DT <= PR_COV_STOP)]
  pu_dt[, c('PR_COV_STRT', 'PR_COV_STOP', 'MAX_VIN', 'I'):=NULL]

  # CLEAN UP TRAILER DATA
  trl_dt <- trl_dt[COV_STRT_DT!=COV_STOP_DT]
  trl_dt <- trl_dt[order(PHYS_POL_KEY, VEH_POS_CNT, LINE_COV_CD, COV_STRT_DT, -SYS_MOD_DT)]
  trl_dt[, PR_COV_STRT:=COV_STRT_DT[1], by=.(PHYS_POL_KEY, VEH_POS_CNT, LINE_COV_CD, COV_STRT_DT, SYS_MOD_DT)]
  trl_dt[, PR_COV_STOP:=COV_STOP_DT[1], by=.(PHYS_POL_KEY, VEH_POS_CNT, LINE_COV_CD, COV_STRT_DT, SYS_MOD_DT)]
  trl_dt[, I:=1:.N, by=.(PHYS_POL_KEY, VEH_POS_CNT, LINE_COV_CD, COV_STRT_DT)]
  trl_dt <- trl_dt[!(I>1 & COV_STOP_DT > PR_COV_STRT & COV_STOP_DT <= PR_COV_STOP)]
  trl_dt[, c('PR_COV_STRT', 'PR_COV_STOP', 'I'):=NULL]

  # RE-COMBINE POWER UNIT AND TRAILER DATA
  cov_dt <- rbindlist(list(pu_dt, trl_dt))
  
  # CREATE PHYSICAL VEHICLE + DRIVER KEY
  cov_dt[, PHYS_VEH_DRVR_KEY:=paste0(PHYS_VEH_KEY, substr(PHYS_DRVR_KEY, 14, 20))]
  cov_dt[, PHYS_VEH_DRVR_LC_KEY:=substr(PHYS_COV_KEY, 1, 29)]
  
  # SUM EXPOSURES AND PREMIUM
  cov_dt[, BI_COV:=ifelse(LINE_COV_CD %in% lcbi, 1, 0)]
  cov_dt[, PD_COV:=ifelse(LINE_COV_CD %in% lcpd, 1, 0)]
  sum_dt <- cov_dt[, .(PD_EE = sum(PD_COV*ERN_EXPS_CNT/12),
                       PD_EP = sum(PD_COV*ERN_PREM_AMT),
                       BI_EE = sum(BI_COV*ERN_EXPS_CNT/12),
                       BI_EP = sum(BI_COV*ERN_PREM_AMT),
                       PD_WE = sum(PD_COV*WRT_EXPS_CNT/12),
                       PD_WP = sum(PD_COV*WRT_PREM_AMT),
                       BI_WE = sum(BI_COV*WRT_EXPS_CNT/12),
                       BI_WP = sum(BI_COV*WRT_PREM_AMT)),
                   by=.(PHYS_VEH_DRVR_KEY)]
  
  # FILTER ON MOST RECENT PHYS_VEH_DRVR_LC_KEY
  cov_dt <- cov_dt[order(PHYS_VEH_DRVR_LC_KEY, -SYS_MOD_DT)]
  first_inds <- cov_dt[, .I[1], by=PHYS_VEH_DRVR_LC_KEY]$V1
  cov_dt <- cov_dt[first_inds]
  
  # CREATE SEPERATE BI AND PD DATASETS
  cov_cols <- c('PHYS_POL_KEY', 'PHYS_VEH_KEY', 'PHYS_DRVR_KEY', 'VIN', 'COV_STRT_DT', 'COV_STOP_DT', 'PHYS_COV_KEY', 'SYS_MOD_DT')
  bi_dt <- cov_dt[BI_COV==1, c('PHYS_VEH_DRVR_KEY', 'ST_CD', cov_cols), with=F]
  pd_dt <- cov_dt[PD_COV==1, c('PHYS_VEH_DRVR_KEY', 'ST_CD', cov_cols), with=F]
  bi_cols <- paste0('BI_', cov_cols)
  pd_cols <- paste0('PD_', cov_cols)
  setnames(bi_dt, cov_cols, bi_cols)
  setnames(pd_dt, cov_cols, pd_cols)
  
  # OVERLAY BI AND PD DATASETS
  cov_dt <- merge(bi_dt, pd_dt, all=TRUE, by=c('ST_CD', 'PHYS_VEH_DRVR_KEY'))
  cov_dt[, PHYS_POL_KEY:=ifelse(is.na(PD_PHYS_POL_KEY), BI_PHYS_POL_KEY, PD_PHYS_POL_KEY)]
  cov_dt[, PHYS_VEH_KEY:=ifelse(is.na(PD_PHYS_VEH_KEY), BI_PHYS_VEH_KEY, PD_PHYS_VEH_KEY)]
  cov_dt[, PHYS_DRVR_KEY:=ifelse(is.na(PD_PHYS_DRVR_KEY), BI_PHYS_DRVR_KEY, PD_PHYS_DRVR_KEY)]
  cov_dt[, VIN:=ifelse(is.na(PD_VIN), BI_VIN, PD_VIN)]
  cov_dt[, SYS_MOD_DT:=as.Date(pmax(PD_SYS_MOD_DT, BI_SYS_MOD_DT, na.rm=T), origin='1970-01-01')]
  cov_dt[, COV_STRT_DT:=as.Date(ifelse(is.na(PD_COV_STRT_DT), BI_COV_STRT_DT, PD_COV_STRT_DT), origin='1970-01-01')]
  cov_dt[, COV_STOP_DT:=as.Date(ifelse(is.na(PD_COV_STOP_DT), BI_COV_STOP_DT, PD_COV_STOP_DT), origin='1970-01-01')]
  cov_dt[, PD_LCL:=substr(PD_PHYS_COV_KEY, 26, 32)]
  cov_dt[, BI_LCL:=substr(BI_PHYS_COV_KEY, 26, 32)]
  bi_cols <- setdiff(bi_cols, 'BI_PHYS_COV_KEY')
  pd_cols <- setdiff(pd_cols, 'PD_PHYS_COV_KEY')
  cov_dt[, c(bi_cols):=NULL]
  cov_dt[, c(pd_cols):=NULL]
  
  # MA HAS MANDATORY (PRIMARY) AND OPTIONAL (EXCESS) LINE COVERAGES THAT WILL OVERLAP
  # USE THE OPTIONAL COVERAGES FOR ANY BI+PD TIME PERIOD IF INSURED HAD THEM ELSE MANDATORY
  cov_dt[, KEEP:=4]
  cov_dt[substr(PD_LCL,1,4)=='1908' & substr(BI_PHYS_COV_KEY,26,30) %in% c('2001','2004','2005','2030'), KEEP:=3]
  cov_dt[substr(PD_LCL,1,4)=='2009' & substr(BI_PHYS_COV_KEY,26,30) %in% c('1909', '1910', '1911', '1919', '1983'), KEEP:=2]
  cov_dt[substr(PD_LCL,1,4)=='2009' & substr(BI_PHYS_COV_KEY,26,30)=='1908', KEEP:=1]
  cov_dt <- cov_dt[order(PHYS_VEH_DRVR_KEY, KEEP)]
  first_inds <- cov_dt[, .I[1], by=.(PHYS_VEH_DRVR_KEY)]$V1
  cov_dt <- cov_dt[first_inds]
  cov_dt[, KEEP:=NULL]
  
  # JOIN BACK EXPOSURES AND PREMIUM
  cov_dt <- sum_dt[cov_dt, on=.(PHYS_VEH_DRVR_KEY)]
  cov_dt[, BIPD_ECY:=ifelse(is.na(PD_EE), BI_EE, PD_EE)]
  cov_dt[, BIPD_WCY:=ifelse(is.na(PD_WE), BI_WE, PD_WE)]
  cov_dt[, BIPD_EP:=BI_EP+PD_EP]
  cov_dt[, BIPD_WP:=BI_WP+PD_WP]
  
  # VEHICLE DATES QUERY
  veh_dates_qry <- "
    SELECT 
      CAW.VEH_DATES.ST_CD, 
      CAW.VEH_DATES.PHYS_POL_KEY, 
      CAW.VEH_DATES.PHYS_VEH_KEY, 
      CAW.VEH_DATES.VEH_STRT_DT, 
      CAW.VEH_DATES.VEH_STOP_DT 
    FROM 
      CAW.VEH_DATES 
    WHERE 
      CAW.VEH_DATES.VEH_STRT_DT BETWEEN {d 'startdate'} and {d 'enddate'}
  ;
  "
  # INSERT DATES
  veh_dates_qry <- str_replace_all(veh_dates_qry, 'startdate', start_date)
  veh_dates_qry <- str_replace_all(veh_dates_qry, 'enddate', end_date)
  # QUERY DATA
  veh_dates_dt <- as.data.table(dbGetQuery(caw_con, veh_dates_qry))
  
  # DRIVER DATES QUERY
  drvr_qry <- "
    SELECT 
    	CAW.DRVR_PUB_VIEW.ST_CD, 
      CAW.DRVR_PUB_VIEW.PHYS_POL_KEY, 
      CAW.DRVR_PUB_VIEW.PHYS_DRVR_KEY, 
      CAW.DRVR_PUB_VIEW.DRVR_MOCK_IND, 
      CAW.DRVR_DATES.DRVR_STRT_DT, 
      CAW.DRVR_DATES.DRVR_STOP_DT 
    FROM 
      CAW.DRVR_PUB_VIEW, 
      CAW.DRVR_DATES 
    WHERE 
      CAW.DRVR_DATES.DRVR_STRT_DT BETWEEN {d 'startdate'} and {d 'enddate'}
      AND CAW.DRVR_DATES.ST_CD = CAW.DRVR_PUB_VIEW.ST_CD 
      AND CAW.DRVR_DATES.POL_ID_CHAR = CAW.DRVR_PUB_VIEW.POL_ID_CHAR 
      AND CAW.DRVR_DATES.RENW_SFX_NBR = CAW.DRVR_PUB_VIEW.RENW_SFX_NBR 
      AND CAW.DRVR_DATES.POL_EXPR_YR = CAW.DRVR_PUB_VIEW.POL_EXPR_YR 
      AND CAW.DRVR_DATES.DRVR_POS_CNT = CAW.DRVR_PUB_VIEW.DRVR_POS_CNT 
      AND CAW.DRVR_DATES.DRVR_VRSN_NBR = CAW.DRVR_PUB_VIEW.DRVR_VRSN_NBR 
      ;
  "
  # INSERT DATES
  drvr_qry <- str_replace_all(drvr_qry, 'startdate', start_date)
  drvr_qry <- str_replace_all(drvr_qry, 'enddate', end_date)
  # QUERY DATA
  drvr_dt <- as.data.table(dbGetQuery(caw_con, drvr_qry))
  
  # JOIN POLICY INFO
  cov_dt <- pol_dt[cov_dt, on=.(ST_CD, PHYS_POL_KEY)]
  
  # JOIN VEHICLE DATES INFO
  cov_dt <- veh_dates_dt[cov_dt, on=.(ST_CD, PHYS_POL_KEY, PHYS_VEH_KEY)]
  
  # JOIN DRIVER DATES INFO
  cov_dt <- drvr_dt[cov_dt, on=.(ST_CD, PHYS_POL_KEY, PHYS_DRVR_KEY)]
  
  # LOSS AND LAE QUERY
  claim_qry <- "
    SELECT
      hist.ClaimFeatureId
      ,hist.PolicyNbr
      ,hist.PolicySuffixNbr
      ,hist.ClaimNbr
      ,hist.CPUTmstmpYear
      ,hist.FeatureSequenceNbr
      ,hist.LineCode
      ,hist.CovCode
      ,hist.AccidentDate
      ,hist.PolicyExpireDate
      ,hist.RiskPositionNbr
      ,hist.VehicleIdNbr
      ,hist.CauseLossCode
      ,stat.PolicyState
      ,stat.LimitDeductibleCode
      ,stat.Stat19 AS LOSS_PAID
      ,stat.Stat05 + stat.Stat19 AS LOSS_INCR
      ,stat.Stat14 AS DCC_PAID
      ,stat.Stat41 + stat.Stat14 AS DCC_INCR
      ,stat.Stat15 AS AO_PAID
      ,stat.Stat47 AS AO_CHRG
      ,stat.Stat42 + stat.Stat15 + stat.Stat47 AS AO_INCR
      ,stat.Stat20 AS FEA_CNT_INCR
		FROM
      ProStarLossDatamart.dbo.ClaimFeatureHist AS hist
    INNER JOIN
      ProStarLossDatamart.dbo.vw_WholeFeatureStatisticAddCriteria AS stat
    ON
      hist.ClaimFeatureId = stat.ClaimFeatureId
		WHERE
      hist.CurrRowInd = 1
      AND hist.CPUTmstmpYear IN (record_years)
      AND stat.Product = 'CL'
      AND stat.AcctMidMonthDate = (
        CASE WHEN stat.MaxAcctMidMonthDate < {d 'mid_end_date'}
        THEN stat.MaxAcctMidMonthDate
        ELSE {d 'mid_end_date'}
        END
      )
      AND stat.LineCoverage IN (lcbd)
  "
  # INSERT DATES AND LINE COVERAGE CRITERIA
  claim_qry <- str_replace_all(claim_qry, 'record_years', record_years_str)
  claim_qry <- str_replace_all(claim_qry, 'lcbd', lcbd_str)
  claim_qry <- str_replace_all(claim_qry, 'mid_end_date', as.character(mid_end_date))
  # QUERY DATA
  claim_dt <- as.data.table(dbGetQuery(prostar_con, claim_qry))

  # FORMAT DATA
  old_cols <- c('PolicyState', 'PolicyNbr', 'PolicySuffixNbr', 'PolicyExpireDate', 'ClaimNbr', 'CPUTmstmpYear',
                'FeatureSequenceNbr', 'ClaimFeatureId', 'VehicleIdNbr', 'RiskPositionNbr', 'LimitDeductibleCode',
                'CauseLossCode', 'AccidentDate')
  new_cols <- c('ST_CD', 'POL_ID_NBR', 'RENW_SFX_NBR', 'POL_EXPR_YR', 'CLM_NBR', 'LRD_STMP_DT_YR',
                'FEA_SEQ_NBR', 'CLM_FEA_ID', 'VIN', 'RISK_POS_NBR', 'LIM_CD',
                'CSE_LS_CD', 'ACC_DT')
  setnames(claim_dt, old_cols, new_cols)
  claim_dt[, POL_ID_NBR:=as.integer(POL_ID_NBR)]
  claim_dt[, RENW_SFX_NBR:=as.integer(RENW_SFX_NBR)]
  claim_dt[, POL_EXPR_YR:=lubridate::year(POL_EXPR_YR)]
  claim_dt[, POL_EXPR_YR:=substr(POL_EXPR_YR, 3, 4)]
  claim_dt[, CLM_NBR:=as.integer(CLM_NBR)]
  claim_dt[, LRD_STMP_DT_YR:=substr(LRD_STMP_DT_YR, 3, 4)]
  claim_dt[, LRD_STMP_DT_YR:=as.integer(LRD_STMP_DT_YR)]
  claim_dt[, FEA_SEQ_NBR:=as.integer(FEA_SEQ_NBR)]
  claim_dt[, RISK_POS_NBR:=as.integer(RISK_POS_NBR)]
  claim_dt[, LINE_COV_CD:=paste0(LineCode, CovCode)]
  claim_dt[, c('LineCode', 'CovCode'):=NULL]
  
  # JOIN PHYSICAL POLICY KEY ONTO PROSTAR CLAIM DATA
  claim_dt <- merge(pol_list, claim_dt, by=c('ST_CD', 'POL_ID_NBR', 'RENW_SFX_NBR', 'POL_EXPR_YR'))

  # GET CAW FEATURES
  feat_qry <- "
    SELECT 
      CAW.CLM_FEA.ST_CD,
      CAW.CLM_FEA.PHYS_POL_KEY, 
    	CAW.CLM_FEA.VEH_POS_CNT, 
      CAW.CLM_FEA.VEH_VRSN_NBR, 
      CAW.CLM_FEA.DRVR_POS_CNT, 
      CAW.CLM_FEA.DRVR_VRSN_NBR, 
      CAW.CLM_FEA.CLM_NBR, 
      CAW.CLM_FEA.FEA_SEQ_NBR,
      CAW.CLM_FEA.SYS_ADD_DT,
      CAW.CLM_FEA.SYS_MOD_DT
    FROM 
      CAW.POL_DATES, 
      CAW.POLICY, 
      CAW.CLAIM, 
      CAW.CLM_FEA 
    WHERE 
      CAW.CLM_FEA.LINE_COV_CD IN (lcbd)
      AND CAW.POL_DATES.POL_EFF_DT BETWEEN {d 'startdate'} and {d 'enddate'}
      AND CAW.CLM_FEA.POL_ID_CHAR = CAW.CLAIM.POL_ID_CHAR 
      AND CAW.CLM_FEA.RENW_SFX_NBR = CAW.CLAIM.RENW_SFX_NBR 
      AND CAW.CLM_FEA.POL_EXPR_YR = CAW.CLAIM.POL_EXPR_YR 
      AND CAW.CLM_FEA.CLM_NBR = CAW.CLAIM.CLM_NBR 
      AND CAW.CLAIM.POL_ID_CHAR = CAW.POLICY.POL_ID_CHAR 
      AND CAW.CLAIM.RENW_SFX_NBR = CAW.POLICY.RENW_SFX_NBR 
      AND CAW.CLAIM.POL_EXPR_YR = CAW.POLICY.POL_EXPR_YR 
      AND CAW.POLICY.POL_ID_CHAR = CAW.POL_DATES.POL_ID_CHAR 
      AND CAW.POLICY.RENW_SFX_NBR = CAW.POL_DATES.RENW_SFX_NBR 
      AND CAW.POLICY.POL_EXPR_YR = CAW.POL_DATES.POL_EXPR_YR 
    GROUP BY
      CAW.CLM_FEA.ST_CD,
      CAW.CLM_FEA.PHYS_POL_KEY, 
      CAW.CLM_FEA.VEH_POS_CNT, 
      CAW.CLM_FEA.VEH_VRSN_NBR, 
      CAW.CLM_FEA.DRVR_POS_CNT, 
      CAW.CLM_FEA.DRVR_VRSN_NBR, 
      CAW.CLM_FEA.CLM_NBR, 
      CAW.CLM_FEA.FEA_SEQ_NBR,
      CAW.CLM_FEA.SYS_ADD_DT,
      CAW.CLM_FEA.SYS_MOD_DT
  ;
  "
  # INSERT DATES AND LINE COVERAGE CRITERIA
  feat_qry <- str_replace_all(feat_qry, 'startdate', start_date)
  feat_qry <- str_replace_all(feat_qry, 'enddate', end_date)
  feat_qry <- str_replace_all(feat_qry, 'lcbd', lcbd_str)
  # QUERY DATA
  feat_dt <- as.data.table(dbGetQuery(caw_con, feat_qry))
  # GET MOST RECENT VERSION OF EACH FEATURE
  feat_dt <- feat_dt[order(ST_CD, PHYS_POL_KEY, CLM_NBR, -FEA_SEQ_NBR, -SYS_ADD_DT, -SYS_MOD_DT)]
  first_inds <- feat_dt[, .I[1], by=.(ST_CD, PHYS_POL_KEY, CLM_NBR, FEA_SEQ_NBR)]$V1
  feat_dt <- feat_dt[first_inds]
  # FILTER ON POLICY LIST
  feat_dt <- merge(feat_dt, pol_list[, .(ST_CD, PHYS_POL_KEY)], by=c('ST_CD', 'PHYS_POL_KEY'))
  # DROP UNNEEDED COLUMNS
  feat_dt[, c('ST_CD', 'SYS_ADD_DT', 'SYS_MOD_DT'):=NULL]
  # MERGE PROSTAR (LOSS) AND CAW FEATURES (VEHICLE + DRIVER INFO)
  claim_dt <- feat_dt[claim_dt, on=.(PHYS_POL_KEY, CLM_NBR, FEA_SEQ_NBR)]
  
  # SYNTHESIZE PHYS_VEH_KEY AND PHYS_VEH_DRVR_KEY
  claim_dt[, PHYS_VEH_KEY:=paste0(PHYS_POL_KEY, str_pad(VEH_POS_CNT, width=3, pad='0'), str_pad(VEH_VRSN_NBR, width=3, pad='0'))]
  claim_dt[, PHYS_VEH_DRVR_KEY:=paste0(PHYS_VEH_KEY, str_pad(DRVR_POS_CNT, width=3, pad='0'), str_pad(DRVR_VRSN_NBR, width=3, pad='0'))]
  claim_dt[is.na(VEH_POS_CNT)|is.na(VEH_VRSN_NBR), PHYS_VEH_KEY:=NA]
  claim_dt[is.na(VEH_POS_CNT)|is.na(VEH_VRSN_NBR)|is.na(DRVR_POS_CNT)|is.na(DRVR_VRSN_NBR), PHYS_VEH_DRVR_KEY:=NA]
  
  # GET PHYS_VEH_DRVR_KEY FOR FEA_SEQ_NBR==0 AND CLOSED WITHOUT PAYMENTS
  missing_dt <- claim_dt[is.na(PHYS_VEH_DRVR_KEY), .(CLM_FEA_ID, PHYS_POL_KEY, VIN, ACC_DT)]
  cov_cols <- c('PHYS_POL_KEY', 'PHYS_VEH_DRVR_KEY', 'COV_STRT_DT', 'COV_STOP_DT', 'VIN', 'SYS_MOD_DT')
  missing_dt <- cov_dt[, cov_cols, with=F][missing_dt, on=.(PHYS_POL_KEY, VIN)]
  missing_dt <- missing_dt[ACC_DT >= COV_STRT_DT & ACC_DT < COV_STOP_DT]
  missing_dt <- missing_dt[order(CLM_FEA_ID, -SYS_MOD_DT)]
  first_inds <- missing_dt[, .I[1], by=.(CLM_FEA_ID)]$V1
  missing_dt <- missing_dt[first_inds, .(CLM_FEA_ID, FOUND_PVDK=PHYS_VEH_DRVR_KEY)]
  claim_dt <- missing_dt[claim_dt, on=.(CLM_FEA_ID)]
  claim_dt[is.na(PHYS_VEH_DRVR_KEY), PHYS_VEH_DRVR_KEY:=FOUND_PVDK]
  claim_dt[, FOUND_PVDK:=NULL]
  
  # CAP LOSSES AT CLAIM LEVEL
  sum_cols <- c('LOSS_PAID', 'LOSS_INCR', 'DCC_PAID', 'DCC_INCR', 'AO_PAID', 'AO_CHRG', 'AO_INCR', 'FEA_CNT_INCR')
  by_cols <- c('ST_CD', 'PHYS_POL_KEY', 'CLM_NBR', 'LRD_STMP_DT_YR', 'ACC_DT', 'PHYS_VEH_DRVR_KEY')
  claim_dt <- claim_dt[, lapply(.SD, sum), by=c(by_cols), .SDcols=c(sum_cols)]
  claim_dt[, CLM_CNT_INCR:=ifelse(FEA_CNT_INCR>0, 1, 0)]
  claim_dt[, LOSS_CPD_PAID:=ifelse(LOSS_PAID>300000, 300000, LOSS_PAID)]
  claim_dt[, LOSS_CPD_INCR:=ifelse(LOSS_INCR>300000, 300000, LOSS_INCR)]
  
  # TREND AND LDF DATA
  ldf_dt <- fread(here(base_lib_path, 'BIPD_BASE_2021_COVID', 'ldf.csv'))
  trend_dt <- fread(here(base_lib_path, 'BIPD_BASE_2021_COVID', 'trend.csv'))
  
  # GET COLUMNS FOR TREND/DEV LOOKUPS
  # CAPPED BIPD
  # LOOKUP COLUMN FOR CAP
  claim_dt[, CAP_LVL:=300000]
  # LOOKUP COLUMN FOR LIMIT
  claim_dt[, LIMIT_LVL:='All']
  # LOOKUP COLUMN FOR BMT
  bmt_qry <- "
    SELECT 
    	CAW.POLICY.ST_CD,
      CAW.POLICY.PHYS_POL_KEY,
      CAW.BUSTYP.BMT AS BMT_LVL
    FROM 
      CAW.BUSTYP,
      CAW.POLICY
    WHERE 
      CAW.POLICY.SIC_CD = CAW.BUSTYP.CV_INDSTR_CLSS_CD
    GROUP BY
      CAW.POLICY.ST_CD,
      CAW.POLICY.PHYS_POL_KEY,
      CAW.BUSTYP.BMT
  "
  bmt_dt <- as.data.table(dbGetQuery(caw_con, bmt_qry))
  claim_dt <- bmt_dt[claim_dt, on=.(ST_CD, PHYS_POL_KEY)]
  claim_dt[, BMT_LVL:=substr(trimws(BMT_LVL), 1, 1)]
  claim_dt[! BMT_LVL %in% c('A','B','C','D','E'), BMT_LVL:='E']
  # LOOKUP COLUMN FOR ACCIDENT YEAR
  claim_dt <- yr_dt[claim_dt, on=.(ACC_DT)]
  # LOOKUP COLUMN FOR STATE
  claim_dt[, ST_LVL:='CW']
  # GET AGE TO ULT FACTOR
  claim_dt <- ldf_dt[claim_dt, on=.(CAP_LVL, LIMIT_LVL, BMT_LVL, ACC_YR_LVL, ST_LVL)]
  # GET TREND PERIOD
  claim_dt[, TREND_PERIOD:=as.integer(as.Date(end_date)-ACC_DT)/365]
  claim_dt[, TREND_PERIOD:=pmax(TREND_PERIOD,0)]
  # GET TREND FACTOR
  claim_dt <- trend_dt[claim_dt, on=.(CAP_LVL, LIMIT_LVL, BMT_LVL, ST_LVL)]
  # CALCULATE DEVELOPED, TRENDED LOSS
  claim_dt[, LOSS_CPD_DEV_INCR:=LOSS_CPD_INCR * AGE_TO_ULT]
  claim_dt[, LOSS_CPD_DEV_TRND_INCR:=LOSS_CPD_DEV_INCR * (TREND ^ TREND_PERIOD)]
  claim_dt[, LOSS_CPD_DEV_TRND_INCR:=round(LOSS_CPD_DEV_TRND_INCR,2)]
  # DROP COLUMN(S)
  claim_dt[, c('TREND', 'AGE_TO_ULT'):=NULL]

  # UNCAPPED BIPD
  # LOOKUP COLUMN FOR CAP
  claim_dt[, CAP_LVL:=1000000]
  # LOOKUP COLUMN FOR LIMIT
  lcl_dt <- unique(cov_dt[, .(ST_CD, PHYS_POL_KEY, BI_LCL, PD_LCL)])
  first_inds <- lcl_dt[, .I[1], by=.(ST_CD, PHYS_POL_KEY)]$V1
  lcl_dt <- lcl_dt[first_inds]
  claim_dt <- lcl_dt[claim_dt, on=.(ST_CD, PHYS_POL_KEY)]
  claim_dt[, BIPD_LCL:=paste0(BI_LCL, '_', PD_LCL)]
  high_lcls <- c('190854_200954', '190856_200956', '191054_200154', '191055_200155', '191056_200156', '191082_200119',
                 '191954_200154', '191956_200156', '198325_200154', '198388_200156')
  ultra_lcls <- c('190857_200101', '190857_200957', '190879_200979', '190896_200996', '190930_200196', '190931_200147',
                  '190943_200105', '190943_200483', '190944_200106', '190944_200484', '190945_200152', '190945_200485',
                  '190946_200153', '190946_200486', '190947_200154', '190947_200487', '191057_200157', '191079_200147',
                  '191079_200179', '191096_200196', '191957_200102', '191957_200157', '191979_200147', '191979_200179',
                  '191996_200196', '198362_200147', '198389_200157', '198396_200196')
  claim_dt[, LIMIT_LVL:='A5D5,A6D6,A7D7']
  claim_dt[BIPD_LCL %in% high_lcls, LIMIT_LVL:='A8D8']
  claim_dt[BIPD_LCL %in% ultra_lcls, LIMIT_LVL:='A9D9,ALDL,AUDU']
  # LOOKUP COLUMN FOR BMT
  claim_dt[, BMT_LVL:=ifelse(BMT_LVL %in% c('A','B'), 'AB', 'CDE')]
  # GET AGE TO ULT FACTOR
  claim_dt <- ldf_dt[claim_dt, on=.(CAP_LVL, LIMIT_LVL, BMT_LVL, ACC_YR_LVL, ST_LVL)]
  # GET TREND FACTOR
  claim_dt <- trend_dt[claim_dt, on=.(CAP_LVL, LIMIT_LVL, BMT_LVL, ST_LVL)]
  # CALCULATE DEVELOPED, TRENDED LOSS
  claim_dt[, LOSS_1000_DEV_TRND_INCR:=ifelse(LOSS_INCR>1000000, 1000000, LOSS_INCR) * AGE_TO_ULT * (TREND ^ TREND_PERIOD)]
  claim_dt[, LOSS_1000_DEV_TRND_INCR:=round(LOSS_1000_DEV_TRND_INCR,2)]
  # DROP COLUMN(S)
  drop_cols <- c('CAP_LVL', 'LIMIT_LVL', 'BMT_LVL', 'ACC_YR_LVL', 'ST_LVL',
                 'BI_LCL', 'PD_LCL', 'TREND_PERIOD', 'AGE_TO_ULT')
  claim_dt[, c(drop_cols):=NULL]

  # AGGREGATE DATA TO PHYS_VEH_DRVR_KEY LEVEL
  sum_cols <- c('LOSS_PAID', 'LOSS_INCR', 'DCC_PAID', 'DCC_INCR', 'AO_PAID', 'AO_CHRG', 'AO_INCR',
                'FEA_CNT_INCR', 'CLM_CNT_INCR', 'LOSS_CPD_PAID', 'LOSS_CPD_INCR', 'LOSS_CPD_DEV_INCR',
                'LOSS_CPD_DEV_TRND_INCR', 'LOSS_1000_DEV_TRND_INCR')
  claim_dt <- claim_dt[, lapply(.SD, sum, na.rm=F), by=PHYS_VEH_DRVR_KEY, .SDcols=c(sum_cols)]
  
  # JOIN TO PREMIUMS AND EXPOSURES
  cov_dt <- claim_dt[cov_dt, on=.(PHYS_VEH_DRVR_KEY)]
  
  # ZERO OUT NULLS
  for (col in sum_cols) cov_dt[, c(col):=ifelse(is.na(get(col)), 0, get(col))]
  
  # GET VEH_MOCK_IND
  veh_mock_qry <- "
    SELECT DISTINCT
      CAW.VEHICLE.ST_CD, 
      CAW.VEHICLE.PHYS_POL_KEY, 
      CAW.VEHICLE.PHYS_VEH_KEY, 
      CAW.VEHICLE.VEH_MOCK_IND 
    FROM 
      CAW.POL_DATES, 
      CAW.VEHICLE 
    WHERE 
          CAW.POL_DATES.POL_EFF_DT BETWEEN {d 'startdate'} AND {d 'enddate'}
      AND CAW.VEHICLE.POL_ID_CHAR = CAW.POL_DATES.POL_ID_CHAR 
      AND CAW.VEHICLE.RENW_SFX_NBR = CAW.POL_DATES.RENW_SFX_NBR 
      AND CAW.VEHICLE.POL_EXPR_YR = CAW.POL_DATES.POL_EXPR_YR 
  ;
  "
  # INSERT DATES INTO QUERY
  veh_mock_qry <- str_replace_all(veh_mock_qry, 'startdate', start_date)
  veh_mock_qry <- str_replace_all(veh_mock_qry, 'enddate', end_date)
  
  # RUN QUERY
  veh_mock_data <- as.data.table(dbGetQuery(caw_con, veh_mock_qry))
  
  # JOIN VEH MOCK INFO
  cov_dt <- veh_mock_data[cov_dt, on=.(ST_CD, PHYS_POL_KEY, PHYS_VEH_KEY)]
  
  # FORMATTING
  cov_dt[VEH_MOCK_IND!="Y", VEH_MOCK_IND:="N"]
  cov_dt[, VEH_MOCK_IND:=as.factor(VEH_MOCK_IND)]
  
  # DROP MOCK VEH RECORDS
  cov_dt <- cov_dt[VEH_MOCK_IND!="Y"]
  
  # DROP MOCK POLICIES
  cov_dt <- cov_dt[POL_MOCK_IND!='Y']
  
  # DROP POLICY ECY <= 1/365
  cov_dt <- cov_dt[BIPD_ECY>=1/365]
  
  # SET FLOOR TO LOSSES
  cov_dt[LOSS_CPD_PAID<0, LOSS_CPD_PAID:=0]
  cov_dt[LOSS_CPD_INCR<0, LOSS_CPD_INCR:=0]
  cov_dt[LOSS_CPD_DEV_INCR<0, LOSS_CPD_DEV_INCR:=0]
  cov_dt[LOSS_CPD_DEV_TRND_INCR<0, LOSS_CPD_DEV_TRND_INCR:=0]
  cov_dt[LOSS_1000_DEV_TRND_INCR<0, LOSS_1000_DEV_TRND_INCR:=0]
  
  # CALCULATE PURE PREMIUM
  cov_dt[, PP:=LOSS_CPD_DEV_TRND_INCR/BIPD_ECY]
  
  # DROP UNNECESSARY COLUMNS
  drop_cols <- c(
    'PHYS_VEH_DRVR_KEY',
    'POL_RENW_IND',
    'POL_TERM',
    'POL_MOCK_IND',
    'COH_ID_NBR',
    'BI_PHYS_COV_KEY',
    'PD_PHYS_COV_KEY',
    'DCC_PAID',
    'DCC_INCR',
    'AO_PAID',
    'AO_CHRG',
    'AO_INCR',
    'COV_STRT_DT',
    'COV_STOP_DT',
    'DRVR_STRT_DT',
    'DRVR_STOP_DT',
    'VEH_STRT_DT',
    'VEH_STOP_DT',
    'VEH_MOCK_IND',
    'COH_INCP_DT',
    'COH_STOP_DT',
    'RT_REV_DT',
    'SYS_MOD_DT',
    'PD_EE',
    'PD_EP',
    'BI_EE',
    'BI_EP',
    'PD_WE',
    'PD_WP',
    'BI_WE',
    'BI_WP'
  )
  cov_dt <- cov_dt[, -c(drop_cols), with=F]
  
  # CHECK DATA LENGTH
  print(length(cov_dt$ST_CD))
  
  # RETURN BASE DATA
  return(cov_dt)
}

# LOAD DEPENDENCIES
for (var in var_lib[[var_name]][['dependencies']]) {
  if (! var %in% names(var_lib)) {
    plog('Loading dependency ', var, ' definition...')
    source(here(base_lib_path, var, 'source.R'), local=TRUE)
  }
}

