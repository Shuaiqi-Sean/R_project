library(data.table)
library(odbc)

# DEFINE VARIABLE
var_name <- 'SAMPLEVAR'

var_lib[[var_name]] <- list()

var_lib[[var_name]][['dependencies']] <- c('VAR1', 'VAR2')

var_lib[[var_name]][['builder']] <- function(...) {
  
  # GET ARGUMENTS
  args <- list(...)
  start_date <- args[['start_date']]
  end_date <- args[['end_date']]
  uid <- args[['uid']]
  pwd <- args[['pwd']]
  base_data <- args[['base_data']]
  
  # BUILD VARIABLE
  
  # JOIN VARIABLE ONTO BASE DATASET
  
  # CHECK DATA LENGTH
  print(length(base_data$ST_CD))
  
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
