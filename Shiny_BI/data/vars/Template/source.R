library(data.table)
library(odbc)
library(stringr)

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
  base_data <- args[['base_data']]
  
  # CONNECT TO CAW
  caw_con <- dbConnect(odbc(), 'DB2P', uid=uid, pwd=pwd)
  
  # VIOLATIONS QUERY
  viol_qry <- "
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
  viol_qry <- str_replace_all(viol_qry, 'startdate', start_date)

  # GET VIOLATION DATA
  viol_data <- as.data.table(dbGetQuery(caw_con, viol_qry))
  
  # ATTACH NEW COLUMNS
  base_data <- viol_data[base_data, on=.(VAR1, VAR2)]
  
  # RETURN BASE DATA
  return(base_data)
}

# LOAD DEPENDENCIES
for (var in var_lib[[var_name]][['dependencies']]) {
  if (! var %in% names(var_lib)) {
    plog('Loading dependency ', var, ' definition...')
    source(here(var_lib_path, var, 'source.R'), local=TRUE)
  }
}
