# REFERENCE CLASS FOR MODELS
model <- setRefClass(
  'model',
  fields = list(
    vars = 'character',
    coefs = 'list',
    types = 'list',
    ints = 'list'
  ),
  methods = list(
    # METHOD TO ADD FACTORS TO MODEL
    add = function(var, coef, type, int=NULL) {
      vars <<- unique(c(vars, var))
      coefs[[var]] <<- coef
      types[[var]] <<- type
      ints[[var]] <<- int
    },
    # METHOD TO SCORE DATASET
    predict = function(dt) {
      # SCORE DATA
      # LIST TO STORE INDIVIDUAL VARIABLE FACTOR FOR EACH RECORD
      preds <- list()
      
      # FOR EACH VARIABLE IN THE MODEL
      for (var in vars) {
        # GET THE COEFFICIENTS FROM THE MODEL
        var_coefs <- coefs[[var]]
        var_type <- types[[var]]
        var_ints <- ints[[var]]
        
        # GIVE THE FACTOR (COLUMN) A NAME
        fct_name <- paste0(var, '_FCT')
        
        # IF THE VARIABLE IS CATEGORICAL
        if (var_type=='categorical') {
          # GET FACTOR TABLE
          var_coefs <- var_coefs[, c(var, 'coef'), with=F]
          # GET THE CORRESPONDING COLUMN IN THE DATASET
          sub_dt <- dt[, c(var), with=F]
          # LOOKUP THE FACTOR FOR EACH RECORD
          sub_dt <- var_coefs[sub_dt, on=c(var)]
          # EXPONENTIATE THE COEFFICIENTS
          sub_dt[, fct:=exp(coef)]
          # STORE THE COLUMN IN THE LIST
          preds[[fct_name]] <- sub_dt[['fct']]
        # ELSE IF NUMERIC
        } else if (var_type=='numeric') {
          # GET FACTOR TABLE
          var_coefs <- var_coefs[, c(var, 'coef'), with=F]
          # GET THE CORRESPONDING COLUMN IN THE DATASET
          sub_dt <- dt[, c(var), with=F]
          # GET THE COEFFICIENT (SHOULD ONLY BE ONE FOR NUMERIC VARIABLES)
          var_coefs <- var_coefs[, coef][1]
          # EXPONENTIATE THE PRODUCT OF THE VARIABLE AND THE COEFFICIENT
          sub_dt[, fct:=exp(get(var)*var_coefs)]
          # STORE THE COLUMN IN THE LIST
          preds[[fct_name]] <- sub_dt[['fct']]
        # ELSE IF INTERACTION
        } else if (var_type=='interaction') {
          # GET PIECES OF THE INTERACTION
          enum_var <- var_ints[1]
          num_var <- var_ints[2]
          # GET FACTOR TABLE
          var_coefs <- var_coefs[, c(enum_var, 'coef'), with=F]
          # GET THE CORRESPONDING COLUMNS IN THE DATASET
          sub_dt <- dt[, c(enum_var, num_var), with=F]
          # LOOKUP THE COEFFICIENT FOR EACH RECORD
          sub_dt <- var_coefs[sub_dt, on=c(enum_var)]
          # EXPONENTIATE THE PRODUCT OF THE NUMERIC VARIABLE AND THE COEFFICIENT
          sub_dt[, fct:=exp(get(num_var)*coef)]
          # STORE THE COLUMN IN THE LIST
          preds[[fct_name]] <- sub_dt[['fct']]
        }
      }
      
      # CREATE DATA TABLE OF RECORD-LEVEL FACTORS
      preds <- do.call(data.table, preds)
      
      # CALCULATE TOTAL PREDICTION (PRODUCT OF FACTORS)
      cols <- data.table::copy(names(preds))
      preds[, PRED:=1]
      for (var in cols) {
        preds[, PRED:=PRED*get(var)]
        preds[, c(var):=round(get(var),4)]
      }
      
      # RETURN PREDICTION DATASET
      return(preds)
    },
    # METHOD TO ADD FACTOR TABLE TO MODEL
    add_design_to_model = function(design, tbl, fct_col, lower_col, upper_col) {
      by_vars <- design$by_vars
      type <- design$type
      enum_var <- design$enum_var
      num_var <- design$num_var
      tbl <- data.table::copy(tbl)
      # LOG VALUES
      tbl[, coef:=log(get(fct_col))]
      tbl[, lower_ci:=NA]
      tbl[, upper_ci:=NA]
      # SET LOWER AND UPPER CI, NA IF NOT FOUND
      if (!is.null(lower_col) & !is.null(upper_col)) {
        if (lower_col %in% names(tbl) & upper_col %in% names(tbl)) {
          tbl[, lower_ci:=log(get(lower_col))]
          tbl[, upper_ci:=log(get(upper_col))]
        } 
      }
      
      # IF CATEGORICAL, INTERACTION, OR MULTI-ROW NUMERIC TABLE
      if (type=='categorical' | type=='interaction' | (type=='numeric' & length(by_vars)>0)) {
        new_tbl <- tbl[, c(enum_var, 'coef', 'lower_ci', 'upper_ci'), with=F]
        add(enum_var, new_tbl, 'categorical')
        
        # IF SINGLE ROW NUMERIC VARIABLE TABLE
      } else if (type=='numeric' & length(by_vars)==0) {
        new_tbl <- tbl[1]
        new_tbl[, c(num_var):='']
        new_tbl <- new_tbl[, c(num_var, 'coef', 'lower_ci', 'upper_ci'), with=F]
        add(num_var, new_tbl, 'numeric')
        
        # ELSE IF MULTIPLE NUMERIC VARIABLE TABLE
      } else if (type=='multi-numeric') {
        # CREATE A MODEL TABLE FOR EACH ROW
        for (i in 1:nrow(tbl)) {
          by_var <- as.character(tbl[i, get(num_var)])
          new_tbl <- tbl[i]
          new_tbl[, c(by_var):='']
          new_tbl <- new_tbl[, c(by_var, 'coef', 'lower_ci', 'upper_ci'), with=F]
          add(by_var, new_tbl, 'numeric')
        }
        
        # ELSE IF MULTIPLE INTERACTION VARIABLE TABLE
      } else if (type=='multi-interaction') {
        # GET NAMES OF NUMERIC VARIABLES
        num_vars <- unique(tbl[[num_var]])
        # CREATE TABLE FOR EACH NUMERIC PIECE
        for (var in num_vars) {
          inds <- (tbl[[num_var]] == var)
          new_tbl <- tbl[inds, c(enum_var, 'coef', 'lower_ci', 'upper_ci'), with=F]
          int_name <- paste0(enum_var, '_', var)
          add(int_name, new_tbl, 'interaction', c(enum_var, var))
        }
      }
    },
    # METHOD TO CHECK UNIQUENESS OF COEFFICIENTS
    check_uniqueness = function() {
      uniqueness <- TRUE
      for (x in names(coefs)) {
        dt <- coefs[[x]]
        var_col <- setdiff(names(dt), c('coef', 'lower_ci', 'upper_ci'))
        nlvls <- dt[, uniqueN(get(var_col))]
        nrows <- nrow(dt)
        if (nlvls!=nrows) {
          plog('Coefficients are not 1 to 1 for levels of variable ', x)
          uniqueness <- FALSE
        }
      }
      return(uniqueness)
    }
  )
)

