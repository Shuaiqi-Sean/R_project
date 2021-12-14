# REFERENCE CLASS FOR TABLE DESIGNS
table_design <- setRefClass(
  'table_design',
  fields = c(
    'tbl_name',
    'data_name',
    'mdl_list',
    'wt_var',
    'by_vars',
    'type',
    'enum_var',
    'num_var',
    'val_funcs',
    'val_vars'
  ),
  methods = list(
    set = function(tbl_name, data_name, mdl_list, wt_var, by_vars, type, enum_var, num_var=NULL, val_funcs=NULL, val_vars=NULL) {
      tbl_name <<- tbl_name
      data_name <<- data_name
      mdl_list <<- mdl_list
      wt_var <<- wt_var
      by_vars <<- by_vars
      type <<- type
      enum_var <<- enum_var
      num_var <<- if (type=='categorical') NULL else num_var
      val_funcs <<- if (is.null(val_funcs)) list() else val_funcs
      val_vars <<- val_vars
    },
    summary = function() {
      data_dt <- data.table(Parameters='Dataset', Arguments=data_name)
      type_dt <- data.table(Parameters='Type', Arguments=type)
      mdl_list_dt <- lapply(1:length(mdl_list), function(x){
        # MODEL
        param_content <- paste0('Model ', x)
        arg_content <- names(mdl_list)[x]
        mdl_dt <- data.table(Parameters=param_content, Arguments=arg_content)
        # VARIABLES
        param_content <- paste0('Variables ', x)
        arg_content <- paste(mdl_list[[x]], collapse=', ')
        var_dt <- data.table(Parameters=param_content, Arguments=arg_content)
        # BIND
        rbindlist(list(mdl_dt, var_dt))
      })
      mdl_list_dt <- rbindlist(mdl_list_dt)
      wt_dt <- data.table(Parameters='Weight', Arguments=wt_var)
      grp_dt <- data.table(Parameters='Groupings', Arguments=paste(by_vars, collapse=', '))
      enum_dt <- data.table(Parameters='Enum. Var.', Arguments=enum_var)
      num_dt <- data.table(Parameters='Num. Var.', Arguments=ifelse(is.null(num_var), '', num_var))
      design_dt <- rbindlist(list(data_dt, type_dt, mdl_list_dt, wt_dt, grp_dt, enum_dt, num_dt))
      return(design_dt)
    },
    multi_fct_tbl = function() {
      print('Generating factor tables...')
      # READ IN DATA
      meta_data <- fread(here(data_lib_path, data_name, 'meta.csv'))
      sel_cols <- unique(c(wt_var, by_vars, num_var, val_vars, unlist(mdl_list)))
      meta_data <- meta_data[columns %in% sel_cols]
      col_types <- meta_data$types
      names(col_types) <- meta_data$columns
      dataset <- fread(here(data_lib_path, data_name, 'data.csv'), colClasses=col_types, select=names(col_types))

      # CREATE FACTOR TABLES FOR EACH MODEL, THEN JOIN TOGETHER INTO ONE
      for (i in 1:length(mdl_list)) {
        mdl_name <- names(mdl_list)[i]
        mdl_vars <- mdl_list[[i]]
        if (!exists('tmp_fct_tbl')) {
          tmp_fct_tbl <- create_fct_table(dataset, mdl_name, mdl_vars, val_funcs)
        } else {
          fct <- create_fct_table(dataset, mdl_name, mdl_vars, val_funcs=NULL, include_sel=F)
          if (length(by_vars)>0) {
            tmp_fct_tbl <- fct[tmp_fct_tbl, on=c(by_vars)]
          } else {
            tmp_fct_tbl <- cbind(tmp_fct_tbl, fct)
          }
        }
      }

      # PLACE ALL FACTORS AT RIGHT SIDE OF TABLE
      fct_cols <- c('_fct_raw', '_lower_ci', '_upper_ci', '_fct')
      fct_cols <- lapply(names(mdl_list), function(x) paste0(x, fct_cols))
      fct_cols <- unlist(fct_cols)
      non_fct_cols <- setdiff(names(tmp_fct_tbl), c(fct_cols, 'sel_fct_raw', 'sel_fct'))
      order_cols <- c(non_fct_cols, fct_cols, 'sel_fct_raw', 'sel_fct')
      setcolorder(tmp_fct_tbl, order_cols)

      # ORDER BY GROUPING VARIABLES
      setorderv(tmp_fct_tbl, by_vars)

      # RETURN LIST
      print('Generation complete.')
      return(tmp_fct_tbl)
    },
    # METHOD TO ADD NEW MODEL TO FACTOR TABLE
    add_to_fct_table = function(tbl, mdl_name, mdl_vars) {
      print('Adding data...')
      # ADD MODEL NAME AND VARIABLE TO DESIGN
      mdl_list[[mdl_name]] <<- mdl_vars
      
      # READ IN DATA
      meta_data <- fread(here(data_lib_path, data_name, 'meta.csv'))
      sel_cols <- unique(c(wt_var, by_vars, num_var, val_vars, mdl_vars))
      meta_data <- meta_data[columns %in% sel_cols]
      col_types <- meta_data$types
      names(col_types) <- meta_data$columns
      dataset <- fread(here(data_lib_path, data_name, 'data.csv'), colClasses=col_types, select=names(col_types))
      
      # CREATE NEW FACTOR TABLE DATA
      new_data <- create_fct_table(dataset, mdl_name, mdl_vars, val_funcs=NULL, include_sel=F)
      
      # JOIN TO TABLE
      tbl <- new_data[tbl, on=c(by_vars)]
      
      # PLACE ALL FACTORS AT RIGHT SIDE OF TABLE
      fct_cols <- c('_fct_raw', '_lower_ci', '_upper_ci', '_fct')
      fct_cols <- lapply(names(mdl_list), function(x) paste0(x, fct_cols))
      fct_cols <- unlist(fct_cols)
      non_fct_cols <- setdiff(names(tbl), c(fct_cols, 'sel_fct_raw', 'sel_fct'))
      order_cols <- c(non_fct_cols, fct_cols, 'sel_fct_raw', 'sel_fct')
      setcolorder(tbl, order_cols)
      
      # RETURN TABLE
      print('Additions complete.')
      return(tbl)
    },
    create_fct_table = function(dataset, mdl_name, mdl_vars, val_funcs, include_sel=TRUE) {
      # LIST TO STORE TABLE COMPONENTS
      tbls <- list()

      # AGGREGATE MODEL FACTORS
      # LOAD FACTOR LIST
      load(here(mdl_lib_path, 'rdata', mdl_name))
      coefs <- mdl[['coefs']][mdl_vars]
      ints <- mdl[['ints']][mdl_vars]

      # COLUMN NAMES
      fct_col <- paste0(mdl_name, '_fct_raw')
      upper_col <- paste0(mdl_name, '_upper_ci')
      lower_col <- paste0(mdl_name, '_lower_ci')

      # IF VARIABLE IS CATEGORICAL
      if (type=='categorical') {
        # UNLIST
        coefs <- coefs[[1]]

        # ENSURE CATEGORICAL FORMATTING
        coefs[, c(mdl_vars):=as.factor(get(mdl_vars))]

        # CALCULATE FACTOR AND UPPER AND LOWER CI
        coefs[, c(fct_col):=pmin(round(exp(coef),8),99)]
        coefs[, c(upper_col):=pmin(round(exp(upper_ci),8),99)]
        coefs[, c(lower_col):=pmin(round(exp(lower_ci),8),99)]

        # FILTER ON NECESSARY COLUMNS ONLY
        coefs <- coefs[, c(mdl_vars, fct_col, upper_col, lower_col), with=FALSE]

        # JOIN FACTORS TO DATASET
        dataset <- coefs[dataset, on=c(mdl_vars)]

        # ELSE IF VARIABLE IS NUMERIC
      } else if (type=='numeric') {
        # UNLIST
        coefs <- coefs[[1]]

        # GET COEFFICIENT AND ERROR FROM (WHAT SHOULD BE A) ONE-ROW FACTOR TABLE
        coef <- coefs$coef[1]
        upper_ci <- coefs$upper_ci[1]
        lower_ci <- coefs$lower_ci[1]

        # CALCULATE FACTOR AND CONFIDENCE INTERVALS
        if (length(by_vars)>0) {
          dataset[, c(fct_col):=pmin(exp(coef*get(num_var)),99)]
          dataset[, c(lower_col):=pmin(exp(lower_ci*get(num_var)),99)]
          dataset[, c(upper_col):=pmin(exp(upper_ci*get(num_var)),99)]
        } else {
          dataset[, c(fct_col):=pmin(exp(coef),99)]
          dataset[, c(lower_col):=pmin(exp(lower_ci),99)]
          dataset[, c(upper_col):=pmin(exp(upper_ci),99)]
        }

        # ELSE IF VARIABLE IS AN INTERACTION
      } else if (type=='interaction') {
        # UNLIST
        coefs <- coefs[[1]]
        ints <- ints[[1]]

        # GET PIECES OF INTERACTION
        int_enum_var <- ints[1]
        int_num_var <- ints[2]

        # ENSURE CATEGORICAL FORMATTING
        coefs[, c(int_enum_var):=as.factor(get(int_enum_var))]

        # FILTER ON NECESSARY COLUMNS ONLY
        coefs <- coefs[, c(int_enum_var, 'coef', 'lower_ci', 'upper_ci'), with=FALSE]

        # JOIN FACTORS TO DATASET
        dataset <- coefs[dataset, on=c(int_enum_var)]

        # CALCULATE EXPONENTIATED FACTORS
        dataset[, c(fct_col):=pmin(exp(coef*get(int_num_var)),99)]
        dataset[, c(lower_col):=pmin(exp(lower_ci*get(int_num_var)),99)]
        dataset[, c(upper_col):=pmin(exp(upper_ci*get(int_num_var)),99)]

        # ELSE IF VARIABLE IS MULTIPLE NUMERIC VARIABLES
      } else if (type=='multi-numeric') {
        # GET FACTOR AND CONFIDENCE INTERVALS FOR EACH LEVEL OF THE GROUPING VARIABLE
        coefs <- lapply(names(coefs), function(x){
          tbl <- coefs[[x]]
          tbl[, c(num_var):=x]
          tbl
        })
        coefs <- rbindlist(coefs)
        coefs[, c(fct_col):=pmin(exp(coef),99)]
        coefs[, c(lower_col):=pmin(exp(lower_ci),99)]
        coefs[, c(upper_col):=pmin(exp(upper_ci),99)]
        coefs <- coefs[, c(num_var, fct_col, lower_col, upper_col), with=F]

        # JOIN FACTORS TO DATASET
        dataset <- coefs[dataset, on=c(num_var)]

        # ELSE IF VARIABLE IS MULTIPLE INTERACTION VARIABLES
      } else if (type=='multi-interaction') {
        # GET FACTOR AND CONFIDENCE INTERVALS FOR EACH LEVEL OF THE GROUPING VARIABLES
        coefs <- lapply(names(coefs), function(x){
          tbl <- coefs[[x]]
          tbl[, c(num_var):=ints[[x]][2]]
          tbl[, c(fct_col):=pmin(exp(coef),99)]
          tbl[, c(lower_col):=pmin(exp(lower_ci),99)]
          tbl[, c(upper_col):=pmin(exp(upper_ci),99)]
          tbl <- tbl[, c(num_var, enum_var, fct_col, lower_col, upper_col), with=F]
          tbl
        })
        coefs <- rbindlist(coefs)

        # JOIN FACTORS TO DATASET
        dataset <- coefs[dataset, on=c(enum_var, num_var)]

        # ELSE RETURN NULL
      } else {
        print('Invalid inputs.  Valid inputs include:')
        print('Single categorical variables.')
        print('Single numeric variables.')
        print('Single interaction variables.')
        print('Multiple numeric variables.  A grouping variable must have levels which match the names of the numeric variables.')
        print('Multiple interaction variables.  A grouping variable must have levels which match the names of the numeric variables.')
        return(NULL)
      }

      # AGGREGATE DATA
      fct_tbl <- dataset[, .(fct=weighted.mean(get(fct_col), get(wt_var)),
                             lower=weighted.mean(get(lower_col), get(wt_var)),
                             upper=weighted.mean(get(upper_col), get(wt_var))),
                         by=c(as.character(by_vars))]
      fct_tbl[is.na(fct), fct:=1]
      fct_tbl[is.na(lower), lower:=1]
      fct_tbl[is.na(upper), upper:=1]
      setnames(fct_tbl, c('fct', 'upper', 'lower'), c(fct_col, upper_col, lower_col))

      # AGGREGATE VALUES AND MERGE ONTO FACTOR TABLE
      if (!is.null(val_funcs)) {
        val_tbl <- dataset[, lapply(names(val_funcs), function(x) eval(val_funcs[[x]])), by=c(as.character(by_vars))]
        val_cols <- setdiff(names(val_tbl), by_vars)
        setnames(val_tbl, val_cols, names(val_funcs))
        if (length(by_vars)>0) {
          fct_tbl <- val_tbl[fct_tbl, on=c(as.character(by_vars))]
        } else {
          fct_tbl <- cbind(val_tbl, fct_tbl)
        }
      }

      # CREATE COPY OF ORIGINAL FACTORS FOR REBASING PURPOSES
      fct_tbl[, paste0(mdl_name, '_fct'):=get(fct_col)]

      # SELECTION FIELD, DEFAULTED TO MODEL
      if (include_sel) {
        fct_tbl[, sel_fct_raw:=get(fct_col)]
        fct_tbl[, sel_fct_raw:=round(sel_fct_raw, 4)]
        fct_tbl[, sel_fct:=sel_fct_raw]
      }

      # ENSURE FORMATTING
      for (col in by_vars) fct_tbl[, c(col):=as.factor(get(col))]

      return(fct_tbl)
    }
  )
)
