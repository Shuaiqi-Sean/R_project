# CLASS TO DEFINE PRODUCT TABLES
product_table <- setRefClass(
  'product_table',
  field = list(
    name = 'character',
    table_names = 'character',
    join_cols = 'character',
    fct_cols = 'character',
    val_tbl = 'data.table',
    design = 'table_design'
  ),
  methods = list(
    initialize = function(name, table_names, join_cols, fct_cols, val_tbl, design) {
      name <<- name
      table_names <<- table_names
      join_cols <<- join_cols
      fct_cols <<- fct_cols
      val_tbl <<- val_tbl
      design <<- design
    }
  )
)

# CLASS TO DEFINE LIST OF FACTOR TABLE DATA AND THEIR RHANDSONTABLE DISPLAYS
factor_tables <- setRefClass(
  'factor_tables',
  # rhots: NAMED LIST TO STORE RHANDSONTABLE DISPLAYS (rhandsontable() OBJECTS)
  # data: NAMED LIST TO STORE CORRESPONDING UNDERLYING DATA (data.table() OBJECTS)
  fields = list(
    rhots = 'list',
    data = 'list',
    designs = 'list',
    prod_tbls = 'list'
  ),
  methods = list(
    # METHOD TO MAKE A TABLE FROM A DESIGN AND SAVE DESIGN
    make_data = function(design){
      tbl <- design$multi_fct_tbl()
      name <- design$tbl_name
      data[[name]] <<- tbl
      rhots[[name]] <<- make_rhot(tbl)
      designs[[name]] <<- design
    },
    # METHOD TO SYNCHRONIZE RHANDSONTABLE DISPLAYS WITH UNDERLYING DATA
    sync_rhots = function(){
      tbls <- lapply(data, function(x) make_rhot(x))
      names(tbls) <- get_names()
      rhots <<- tbls
    },
    # METHOD TO SYNCHRONIZE UNDERLYING DATA WITH RHANDSONTABLE DISPLAYS
    sync_data = function(input){
      tbl_names <- get_names()
      tbls <- lapply(tbl_names, function(x){
        rhot_output_name <- paste0(x, '_fct_tbl_output')
        if (is.null(input[[rhot_output_name]])) {
          tbl <- data[[x]]
        } else {
          rhot_data <- as.data.table(hot_to_r(input[[rhot_output_name]]))
          rhot_cols <- names(rhot_data)
          data_cols <- names(data[[x]])
          if (identical(rhot_cols, data_cols)) {
            tbl <- rhot_data
          } else {
            tbl <- data[[x]]
          }
        }
        tbl
      })
      names(tbls) <- tbl_names
      data <<- tbls
    },
    # METHOD TO GET NAMES OF TABLES
    get_names = function(){
      return(names(data))
    },
    # METHOD TO MAKE RHANDSONTABLE DISPLAY
    make_rhot = function(tbl){
      rhandsontable(tbl, height=450, overflow='hidden') %>% 
        hot_context_menu(allowRowEdit=FALSE, allowColEdit=FALSE)
    },
    # METHOD TO ADD PRODUCT TABLE
    add_prod_tbl = function(data_name, val_vars, val_funcs, name, table_names, join_cols, fct_cols) {
      # READ IN DATA
      meta_data <- fread(here(data_lib_path, data_name, 'meta.csv'))
      sel_cols <- unique(c(val_vars, join_cols))
      meta_data <- meta_data[columns %in% sel_cols]
      col_types <- meta_data$types
      names(col_types) <- meta_data$columns
      dataset <- fread(here(data_lib_path, data_name, 'data.csv'), colClasses=col_types, select=names(col_types))
      
      # AGGREGATE VALUES
      if (!is.null(val_funcs)) {
        val_tbl <- dataset[, lapply(names(val_funcs), function(x) eval(val_funcs[[x]])), by=c(as.character(join_cols))]
        for (j in join_cols) set(val_tbl, j=j, value=as.factor(val_tbl[[j]]))
        val_cols <- setdiff(names(val_tbl), join_cols)
        setnames(val_tbl, val_cols, names(val_funcs))
      }
      
      # ASSEMBLE DESIGN, ONLY USED FOR design$add_to_model METHOD
      design <- table_design()
      mdl_list <- lapply(table_names, function(x){
        designs[[x]]$mdl_list
      })
      mdl_list <- do.call(c, mdl_list)
      design$set(
        tbl_name = name,
        data_name = data_name,
        mdl_list = mdl_list,
        wt_var = designs[[table_names[1]]]$wt_var, # USE WEIGHT VAR FROM FIRST TABLE
        by_vars = join_cols,
        enum_var = meta_data[which.max(cardinality), columns], # USE MOST GRANULAR VARIABLE
        num_var = NULL,
        type = 'categorical',
        val_funcs = val_funcs,
        val_vars = val_vars
      )
      
      # STORE ARGUMENTS AS product_table CLASS, USED BY make_product_table METHOD
      prod_tbl <- product_table(
        name = name,
        table_names = table_names,
        join_cols = join_cols,
        fct_cols = fct_cols,
        val_tbl = val_tbl,
        design = design
      )
      prod_tbls[[name]] <<- prod_tbl
    },
    # METHOD TO CALCULATE PRODUCT TABLE, RESULT STORED IN data
    calc_product_table = function(prod_tbl){
      name <- prod_tbl$name
      table_names <- prod_tbl$table_names
      join_cols <- prod_tbl$join_cols
      fct_cols <- prod_tbl$fct_cols
      val_tbl <- prod_tbl$val_tbl
      tryCatch({
        # GET PRODUCT OF TABLES, ONE FOR EACH FACTOR SELECTED
        tbl_list <- lapply(fct_cols, function(fct){
          # FOR EACH TABLE SELECTED, GET ONLY THE COLUMNS SELECTED
          tables <- lapply(table_names, function(x){
            tbl <- data.table::copy(data[[x]])
            cols <- intersect(names(tbl), c(join_cols, fct))
            tbl <- tbl[, cols, with=F]
            tbl
          })
          # GET THE PRODUCT TABLE (SEE helpers.R)
          tbl <- merge_reduce(tables, join_cols)
          # SET COLUMN NAME
          setnames(tbl, 'product', fct)
        })
        # MERGE TABLES TOGETHER TO GET ONE TABLE WITH ALL FACTORS
        prod_tbl <- Reduce(merge, tbl_list)
        # GET VALUE COLUMNS
        val_cols <- setdiff(names(val_tbl), join_cols)
        # JOIN TO VALUE TABLE
        prod_tbl <- prod_tbl[val_tbl, on=c(join_cols)]
        # SET COLUMN ORDER
        setcolorder(prod_tbl, c(join_cols, val_cols, fct_cols))
        # STORE DATA TABLE AND RHANDSONTABLE
        data[[name]] <<- prod_tbl
      }, warning = function(w) {
        print(w)
        data[[name]] <<- data.table()
      }, error = function(e) {
        print(e)
        data[[name]] <<- data.table()
      })
    },
    # MAKE ALL PRODUCT TABLES
    calc_all_product_tables = function(){
      for (prod_tbl in prod_tbls) {
        calc_product_table(prod_tbl)
      }
    },
    # METHOD TO ADD HIGHLIGHTING RULE TO RHANDSONTABLE DISPLAYS
    highlight = function(fct_col, lower_col, upper_col){
      # GET NAMES
      tbl_names <- get_names()
      # FOR EACH TABLE
      tbls <- lapply(tbl_names, function(x){
        # CREATE A CUSTOM RENDERING CALLBACK (JAVASCRIPT)
        renderer <- "
          function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.NumericRenderer.apply(this, arguments);
            if (col == #fct_index) {
              var upper_bound = instance.getData()[row][#upper_index]
              var lower_bound = instance.getData()[row][#lower_index]
              if (value < lower_bound || value > upper_bound) {
                td.style.background = 'pink';
              } else {
                td.style.background = 'lightgreen';
              }
            }
          }
        "
        # GET THE RHANDSONTABLE OBJECT
        rhot <- rhots[[x]]
        # GET COLUMN NAMES
        cols <- rhot$x$rColHeaders
        # IF THE FACTOR COLUMN, UPPER BOUND, AND LOWER BOUND ARE ALL PRESENT
        found <- all(c(fct_col, lower_col, upper_col) %in% cols)
        if (found) {
          # GET THE INDICES OF THE FACTOR COLUMN, UPPER BOUND, AND LOWER BOUND COLUMN
          upper_index <- which(upper_col==cols) - 1
          lower_index <- which(lower_col==cols) - 1
          fct_index <- which(fct_col==cols) - 1
          # CUSTOMIZE THE RENDERER FUNCTION WITH THOSE INDICES
          renderer <- str_replace_all(renderer, '#upper_index', as.character(upper_index))
          renderer <- str_replace_all(renderer, '#lower_index', as.character(lower_index))
          renderer <- str_replace_all(renderer, '#fct_index', as.character(fct_index))
          # ADD RENDERER TO RHANDSONTABLE DISPLAY
          rhot <- hot_cols(rhot, renderer = renderer)
        }
        # RETURN NEW RHANDSONTABLE OBJECT
        return(rhot)
      })
      # STORE NEW NAMED LIST OF HIGHLIGHTED RHANDSONTABLE OBJECTS
      names(tbls) <- tbl_names
      rhots <<- tbls
    },
    # METHOD TO ADD A DESIGN
    add_design = function(design) {
      designs[[design$tbl_name]] <<- design
    },
    # METHOD TO REMOVE A DESIGN BY NAME
    delete_table = function(tbl_name) {
      data[[tbl_name]] <<- NULL
      rhots[[tbl_name]] <<- NULL
      designs[[tbl_name]] <<- NULL
      prod_tbls[[tbl_name]] <<- NULL
    },
    # METHOD TO APPLY DESIGNS
    apply_designs = function() {
      # CREATE EMPTY FACTOR TABLE LIST
      tmp_tbl_list <- list()
      # FOR EACH TABLE DESIGN
      for (design in designs) {
        # GENERATE AND STORE TABLE
        tmp_tbl_list[[design$tbl_name]] <- design$multi_fct_tbl()
      }
      # OVERWRITE FACTOR TABLE LIST
      data <<- tmp_tbl_list
      sync_rhots()
    },
    # METHOD TO ADD MODEL TO EXISTING FACTOR TABLE
    add_to_fct_table = function(tbl_name, mdl_name, mdl_var) {
      design <- designs[[tbl_name]]
      data[[tbl_name]] <<- design$add_to_fct_table(data[[tbl_name]], mdl_name, mdl_var)
    },
    # METHOD TO RESET FACTORS TO RAW
    reset_to_raw = function() {
      for (tbl_name in get_names()) {
        mdls <- designs[[tbl_name]]$mdl_list
        mdls <- names(mdls)
        mdls <- c(mdls, 'sel')
        for (mdl_name in mdls) {
          # TABLE COLUMNS
          tbl_cols <- names(data[[tbl_name]])
          # RAW COLUMNS
          fct_col_raw <- paste0(mdl_name, '_fct_raw')
          # FCT COLUMNS
          fct_col <- paste0(mdl_name, '_fct')
          # RESET
          if (fct_col %in% tbl_cols & fct_col_raw %in% tbl_cols) {
            data[[tbl_name]][, c(fct_col):=get(fct_col_raw)]
          }
        }
      }
    },
    # METHOD TO DELETE COLUMN FROM TABLES
    delete_column = function(col) {
      for (tbl_name in get_names()) {
        tbl_cols <- names(data[[tbl_name]])
        if (col %in% tbl_cols) {
          data[[tbl_name]][, c(col):=NULL]
        }
      }
    },
    # METHOD TO CREATE COMPARISON COLUMN
    compare_column = function(col_1, col_2, col_name) {
      for (tbl_name in get_names()) {
        tbl_cols <- names(data[[tbl_name]])
        if (col_1 %in% tbl_cols & col_2 %in% tbl_cols) {
          data[[tbl_name]][, c(col_name):=round(get(col_1)/get(col_2)-1,3)]
        }
      }
    }
  )
)
