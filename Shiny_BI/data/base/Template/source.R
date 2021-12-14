library(data.table)
library(odbc)

# DEFINE VARIABLE
var_name <- 'Template'

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
  lcbi <- c('1111')
  lcpd <- c('2222')
  lcbd <- c(lcbi, lcpd)
  lcbd_str <- paste0(paste0("'", lcbd, "'"), collapse=',')

  # CONNECT TO CAW
  caw_con <- dbConnect(odbc(), 'DB2P', uid=uid, pwd=pwd)
  
  # CONNECT TO PROSTAR
  prostar_con <- dbConnect(odbc(), 'ProStar', uid=uid, pwd=pwd)
  
  # POLICY INFO QUERY
  pol_dates_qry <- "
    SELECT
    	CAW.TMP1.VAR1,
      CAW.TMP2.VAR2
    FROM
      CAW.TMP1,
      CAW.TMP2
    WHERE
      CAW.TMP1.VAR1 <> 'TN'
      and CAW.TMP2.VAR2  BETWEEN {d 'startdate'} and {d 'enddate'}
      and CAW.TPM1.VAR3 = CAW.TPM1.VAR3
    GROUP BY
    	CAW.TMP1.VAR1,
      CAW.TMP2.VAR2
    ;
  "
  # INSERT DATES
  pol_dates_qry <- str_replace_all(pol_dates_qry, 'startdate', start_date)
  pol_dates_qry <- str_replace_all(pol_dates_qry, 'enddate', end_date)
  # QUERY DATA
  pol_dt <- as.data.table(dbGetQuery(caw_con, pol_dates_qry))
  # REMOVE DUPLICATES
  first_inds <- pol_dt[, .I[1], by=.(ST_CD, PHYS_POL_KEY)]$V1
  pol_dt <- pol_dt[first_inds] 
  
  # RETURN BASE DATA
  return(pol_dates_qry)
}

# LOAD DEPENDENCIES
for (var in var_lib[[var_name]][['dependencies']]) {
  if (! var %in% names(var_lib)) {
    plog('Loading dependency ', var, ' definition...')
    source(here(base_lib_path, var, 'source.R'), local=TRUE)
  }
}

