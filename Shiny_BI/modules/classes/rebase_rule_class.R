# REFERENCE CLASS FOR REBASING RULES
rebase_rule <- setRefClass(
  'rebase_rule',
  fields = c(
    'name',
    'rebase_to_lvl',
    'rebase_to_base',
    'tgt_tbl_name',
    'fct_mdl',
    'src_mdl',
    'var_col',
    'var_lvl',
    'adj_tbl_name',
    'join_col',
    'wt_col',
    'map_joins',
    'map_vars',
    'map_adjs'
  ),
  methods = list(
    # METHOD TO SET REBASING RULE INPUTS
    set = function(
      name,
      rebase_to_lvl,
      rebase_to_base,
      tgt_tbl_name,
      fct_mdl,
      src_mdl,
      var_col,
      var_lvl,
      adj_tbl_name,
      join_col,
      wt_col,
      map_joins,
      map_vars,
      map_adjs
    ) {
      # SET FIELDS
      name <<- name
      rebase_to_lvl <<- rebase_to_lvl
      rebase_to_base <<- rebase_to_base
      tgt_tbl_name <<- tgt_tbl_name
      fct_mdl <<- fct_mdl
      src_mdl <<- src_mdl
      var_col <<- if (rebase_to_lvl) var_col else NULL
      var_lvl <<- if (rebase_to_lvl & rebase_to_base) var_lvl else NULL
      adj_tbl_name <<- if (!rebase_to_base) adj_tbl_name else NULL
      join_col <<- if (!rebase_to_base) join_col else NULL
      wt_col <<- if (!rebase_to_lvl) wt_col else NULL
      map_joins <<- if (!rebase_to_base) map_joins else NULL
      map_vars <<- if (rebase_to_lvl & !rebase_to_base) map_vars else NULL
      map_adjs <<- if (!rebase_to_lvl & !rebase_to_base) map_adjs else NULL
    },
    # METHOD TO SUMMARIZE RULE INTO A DATA TABLE
    summary = function() {
      add_item <- function(dt, param, arg) {
        if (is.null(arg)) arg <- ''
        item_dt <- data.table(Parameters=param, Arguments=arg)
        dt <- rbindlist(list(dt, item_dt))
        return(dt)
      }
      dt <- data.table()
      dt <- add_item(dt, 'Name', name)
      dt <- add_item(dt, 'Rebase-to-level method?', rebase_to_lvl)
      dt <- add_item(dt, 'Push adj. to base rate?', rebase_to_base)
      dt <- add_item(dt, 'Rebase Table', tgt_tbl_name)
      dt <- add_item(dt, 'Factor Model', fct_mdl)
      dt <- add_item(dt, 'Source Model', src_mdl)
      dt <- add_item(dt, 'Variable Column', var_col)
      dt <- add_item(dt, 'Variable Level', var_lvl)
      dt <- add_item(dt, 'Adjustment Table', adj_tbl_name)
      dt <- add_item(dt, 'Adjustment Join Col.', join_col)
      dt <- add_item(dt, 'Weight Column', wt_col)
      for (i in seq_len(length(map_joins))) {
        param <- paste('Map Join Lvl:', map_joins[i])
        if (!rebase_to_lvl & !rebase_to_base) {
          arg <- paste('to Wtd. Avg:', map_adjs[i])
        } else {
          arg <- paste('to Var. Lvl:', map_vars[i])
        }
        dt <- add_item(dt, param, arg)
      }
      return(dt)
    },
    # METHOD TO APPLY REBASING RULE TO A GIVEN FACTOR TABLE LIST
    apply = function(fct_tbl_list) {
      src_col <- paste0(src_mdl, '_fct')
      fct_col <- paste0(fct_mdl, '_fct')
      fct_tbl <- data.table::copy(fct_tbl_list[[tgt_tbl_name]])
      # IF USING REBASE-TO-LEVEL METHOD
      if (rebase_to_lvl) {
        if (rebase_to_base) {
          # GET REBASE ADJUSTMENT
          mask <- fct_tbl[, get(var_col)==var_lvl]
          rebase <- fct_tbl[mask, get(src_col)][1]
          # REBASE
          fct_tbl[, c(fct_col):=get(fct_col)/rebase]
          # CREATE COLUMN TO STORE REBASE ADJUSTMENT FOR AUDITING
          fct_tbl[, c(name):=1/rebase]
          # STORE REBASED FACTOR TABLE
          fct_tbl_list[[tgt_tbl_name]] <- fct_tbl
        } else {
          adj_tbl <- data.table::copy(fct_tbl_list[[adj_tbl_name]])
          # PUT JOIN LEVELS AND MAPPED ADJUSTMENT LEVELS INTO A TABLE AND RENAME
          map_tbl <- data.table(JOIN=map_joins, VAR=map_vars)
          setnames(map_tbl, c('JOIN', 'VAR'), c(join_col, var_col))
          # GET FACTOR FOR EACH MAPPING
          fct_map <- fct_tbl[, c(join_col, var_col, src_col), with=F]
          map_tbl <- fct_map[map_tbl, on=c(join_col, var_col)]
          # MAP FACTOR BACK TO JOIN LEVELS
          map_tbl[, c(var_col):=NULL]
          setnames(map_tbl, src_col, 'REBASE')
          fct_tbl <- map_tbl[fct_tbl, on=c(join_col)]
          adj_tbl <- map_tbl[adj_tbl, on=c(join_col)]
          # REBASE
          fct_tbl[, c(fct_col):=get(fct_col)/REBASE]
          fct_tbl[, c(name):=1/REBASE]
          adj_tbl[, c(fct_col):=get(fct_col)*REBASE]
          adj_tbl[, c(name):=REBASE]
          # OVERWRITE FACTOR TABLES
          fct_col_order <- names(fct_tbl_list[[tgt_tbl_name]])
          adj_col_order <- names(fct_tbl_list[[adj_tbl_name]])
          fct_tbl <- fct_tbl[, unique(c(fct_col_order, name)), with=F]
          adj_tbl <- adj_tbl[, unique(c(adj_col_order, name)), with=F]
          fct_tbl_list[[tgt_tbl_name]] <- fct_tbl
          fct_tbl_list[[adj_tbl_name]] <- adj_tbl
        }
        # ELSE, USE THE REBASE-TO-WEIGHTED-MEAN METHOD
      } else {
        if (rebase_to_base) {
          # CALCULATE ADJUSTMENT (WEIGHTED MEAN) OF FACTOR PER LEVEL OF SELECTED VARIABLE
          # IF SOURCE COLUMN IS THE SAME AS FACTOR COLUMN, REBASE USING THE WEIGHTED MEAN OF THE FACTOR COLUMN
          if (fct_col==src_col) {
            rebase <- fct_tbl[, weighted.mean(get(fct_col), get(wt_col), na.rm=T)]
            # ELSE, MAKE THE NEW REBASED FACTOR HAVE THE SAME WEIGHTED MEANS AS THE SOURCE COLUMN
          } else {
            rebase <- fct_tbl[, weighted.mean(get(fct_col), get(wt_col), na.rm=T) / 
                                weighted.mean(get(src_col), get(wt_col), na.rm=T)]
          }
          # REBASE
          fct_tbl[, c(fct_col):=get(fct_col)/rebase]
          fct_tbl[, c(name):=1/rebase]
          # STORE REBASED AND ADJUSTED TABLE
          fct_tbl_list[[tgt_tbl_name]] <- fct_tbl
        } else {
          adj_tbl <- data.table::copy(fct_tbl_list[[adj_tbl_name]])
          # CALCULATE ADJUSTMENT (WEIGHTED MEAN) OF FACTOR PER LEVEL OF SELECTED VARIABLE
          # IF SOURCE COLUMN IS THE SAME AS FACTOR COLUMN, REBASE USING THE WEIGHTED MEAN OF THE FACTOR COLUMN
          if (fct_col==src_col) {
            rebase_tbl <- fct_tbl[, .(REBASE=weighted.mean(get(fct_col), get(wt_col), na.rm=T)), by=c(join_col)]
            # ELSE, MAKE THE NEW REBASED FACTOR HAVE THE SAME WEIGHTED MEANS AS THE SOURCE COLUMN
          } else {
            rebase_tbl <- fct_tbl[, .(REBASE=weighted.mean(get(fct_col), get(wt_col), na.rm=T) / 
                                        weighted.mean(get(src_col), get(wt_col), na.rm=T)), by=c(join_col)]
          }
          # PUT JOIN LEVELS AND MAPPED ADJUSTMENT LEVELS INTO A TABLE AND RENAME
          map_tbl <- data.table(JOIN=map_joins, ADJ=map_adjs)
          setnames(map_tbl, 'JOIN', join_col)
          # JOIN TO REBASE FACTOR
          rebase_tbl <- map_tbl[rebase_tbl, on=c(join_col)]
          rebase_tbl[, REBASE:=REBASE/ADJ]
          # REBASE FACTOR TABLE
          fct_col_rebase <- rebase_tbl[fct_tbl, on=c(join_col)][, REBASE]
          fct_tbl[, c(fct_col):=get(fct_col)/fct_col_rebase]
          fct_tbl[, c(name):=1/fct_col_rebase] # COLUMN FOR AUDITING
          # OFFSET TO ADJUSTMENT TABLE
          adj_col_rebase <- rebase_tbl[adj_tbl, on=c(join_col)][, REBASE]
          adj_tbl[, c(fct_col):=get(fct_col)*adj_col_rebase]
          adj_tbl[, c(name):=adj_col_rebase] # COLUMN FOR AUDITING
          # STORE REBASED AND ADJUSTED TABLE
          fct_tbl_list[[tgt_tbl_name]] <- fct_tbl
          fct_tbl_list[[adj_tbl_name]] <- adj_tbl
        }
      }
      return(fct_tbl_list)
    }
  )
)
