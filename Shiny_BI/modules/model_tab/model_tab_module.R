
# MODEL TAB UI
model_tab_ui <- function(id) {
  
  # NAMESPACE TAG
  ns <- NS(id)

  # UI
  model_tab <- tabItem(
    shinyjs::useShinyjs(),
    tabName = 'model_tab',
    fluidRow(
      column(
        width = 6,
        ### LAUNCH H2O ###
        box(
          title = 'Launch',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          splitLayout(
            numericInput(ns('num_cores_input'), label='Number of cores (-1 = All)', value=-1, step=1),
            numericInput(ns('max_mem_input'), label='Memory size (GB)', value=60, step=1)
          ),
          actionButton(ns('h2o_launch_btn'), label='Launch H2O'),
          actionButton(ns('h2o_flow_btn'), label='H2O Flow', onclick="window.open('http://localhost:54321')")
        ),
        
        ### IMPORT DATA INTO H2O ###
        box(
          title = 'Data',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('data_file_ui')),
          actionButton(ns('h2o_import_data_btn'), label='Import Data'),
          actionButton(ns('refresh_data_lib_btn'), label='Refresh Library')
        ),
        
        ### LOAD MODEL DESIGN ###
        box(
          title = 'Load Model Design',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('load_design_ui')),
          actionButton(ns('load_design_btn'), label='Load Design')
        ),
        
        ### MODEL PARAMETERS ###
        box(
          title = 'Parameters',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          strong('Variables'),
          p(),
          uiOutput(ns('select_mdl_vars_ui')),
          actionButton(ns('increase_steps_btn'), label='Add Step'),
          actionButton(ns('decrease_steps_btn'), label='Remove Step'),
          p(),
          uiOutput(ns('interaction_labels_ui')),
          actionButton(ns('add_int_btn'), label='Add Interaction'),
          p(),
          uiOutput(ns('select_response_ui')),
          uiOutput(ns('select_weight_ui')),
          selectInput(ns('select_dist_selector'), 
                      label='Distribution', 
                      choices=c('tweedie', 'poisson', 'gamma', 'gaussian', 'binomial', 'multinomial', 'ordinal', 'quasibinomial')),
          uiOutput(ns('tweedie_params_ui')),
          uiOutput(ns('ref_lvl_ui')),
          actionButton(ns('add_ref_lvl_btn'), label='Add Reference Level'),
          actionButton(ns('rem_ref_lvl_btn'), label='Remove Reference Level'),
          p(),
          dataTableOutput(ns('ref_lvl_tbl_output')),
          p(),
          uiOutput(ns('model_name_ui')),
          p(),
          actionButton(ns('build_mdl_btn'), label='Build Model'),
          actionButton(ns('save_design_btn'), label='Save Design')
        ),
        
        ### SAVE MODEL ###
        box(
          title = 'Export',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('save_model_ui')),
          actionButton(ns('save_model_btn'), label='Save Model'),
          actionButton(ns('refresh_mdl_lib_btn'), label='Refresh Library')
        )
      ),
      
      column(
        width = 6,
        ### PREDICT ###
        box(
          title = 'Predict',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          uiOutput(ns('pred_mdl_ui')),
          actionButton(ns('refresh_pred_mdl_lib_btn'), label='Refresh Library'),
          p(),
          uiOutput(ns('pred_data_ui')),
          uiOutput(ns('pred_obs_ui')),
          uiOutput(ns('pred_wt_ui')),
          uiOutput(ns('pred_name_ui')),
          checkboxInput(ns('pred_res_checkbox'), label='Calculate residuals?'),
          uiOutput(ns('pred_res_ui')),
          uiOutput(ns('pred_file_ui')),
          actionButton(ns('score_data_btn'), label='Predict'),
          p(),
          infoBoxOutput(ns('nase_info_box'))
        ),
        
        ### COEFFICIENT PREVIEW ###
        box(
          title = 'Coefficients',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          uiOutput(ns('coef_preview_ui')),
          actionButton(ns('coef_preview_btn'), label='View'),
          actionButton(ns('coef_refresh_btn'), label='Refresh Library'),
          p(),
          DTOutput(ns('coef_preview_table_output'))
        ),
        
        ### MODEL EDITOR ###
        box(
          title = 'Editor',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          uiOutput(ns('edit_models_ui')),
          uiOutput(ns('edit_incl_vars_ui')),
          uiOutput(ns('edit_excl_vars_ui')),
          textInput(ns('edit_model_name'), label='New Model Name'),
          actionButton(ns('edit_save_btn'), label='Save Model')
        )
      )
    )
  )
}

# MODEL TAB SERVER
model_tab_server <- function(input, output, session) {
  
  # NAMESPACE TAG
  ns <- session$ns
  nsp <- function(...) {
    args <- list(...)
    ns(do.call(paste0, args))
  }
  
  # REACTIVE VALUE TO TRACK H2O CLUSTER STATUS (UP/DOWN)
  h2o_is_up <- reactiveVal(FALSE)
  
  # REACTIVE VALUE TO HAVE META DATA OF H2O FRAME ACCESSIBLE BETWEEN OUTPUT OBJECTS
  h2o_meta_data <- reactiveVal(FALSE)
  
  # REACTIVE VALUES FOR:
  # NUMBER OF STEPS IN MODEL (DEFAULT 1)
  # INTERACTIONS IDS
  # MOST RECENTLY CREATED INTERACTION ID (INCREMENTED TO CREATE NEW, UNIQUE IDS)
  n_steps <- reactiveVal(1)
  int_ids <- reactiveVal(c())
  newest_int_id <- reactiveVal(as.integer(Sys.time()))
  
  # LAUNCH H2O BUTTON
  observeEvent(input$h2o_launch_btn, {
    req(
      input$num_cores_input,
      input$max_mem_input
    )
    nthreads <- input$num_cores_input
    max_mem_size <- paste0(input$max_mem_input, 'g')
    tryCatch({
      h2o.init(nthreads = nthreads, max_mem_size = max_mem_size)
      h2o_is_up(TRUE)
    }, error = function(err) {
      print('Failed to launch H2O cluster.')
      print(err)
      h2o_is_up(FALSE)
    })
  }, ignoreInit = TRUE)
  
  # DATASET SELECTOR
  output$data_file_ui <- renderUI({
    input$refresh_data_lib_btn
    datasets <- list.dirs(data_lib_path, full=F)
    datasets <- setdiff(datasets, '')
    selectInput(ns('data_file_input'), choices=datasets, label='Dataset')
  })
  
  # IMPORT DATA BUTTON
  observeEvent(input$h2o_import_data_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('h2o_import_data_btn')
    on.exit(shinyjs::enable('h2o_import_data_btn'))
    
    # REQUIRED INPUTS
    req(h2o_is_up())
    print('Importing data...')
    # GET SELECTED DATASET NAME
    data_name <- input$data_file_input
    data_path <- here(data_lib_path, data_name, 'data.csv')
    
    # READ IN META DATA
    print('Importing meta data...')
    meta_data <- fread(here(data_lib_path, data_name, 'meta.csv'))
    meta_data[, h2o_types:='unknown']
    meta_data[types=='character', h2o_types:='string']
    meta_data[types=='integer', h2o_types:='numeric']
    meta_data[types=='numeric', h2o_types:='numeric']
    meta_data[types=='Date', h2o_types:='time']
    meta_data[types=='factor', h2o_types:='enum']
    h2o_meta_data(meta_data)
    
    # IMPORT DATA
    h2o_data <- h2o.importFile(data_path, destination_frame=data_name, col.types=meta_data$h2o_types)
    print('Import complete.')
  }, ignoreInit = TRUE)
  
  # VARIABLE SELECTOR
  output$select_mdl_vars_ui <- renderUI({
    req(h2o_meta_data())
    lapply(1:n_steps(), function(x) {
      choices <- h2o_meta_data()[h2o_types %in% c('numeric', 'enum'), columns]
      selectInput(nsp('select_mdl_vars_selector_', x), choices=choices, label=NULL, mult=T, selectize=T)
    })
  })
  
  # RESPONSE VARIABLE SELECTOR
  output$select_response_ui <- renderUI({
    req(h2o_meta_data())
    choices <- h2o_meta_data()[h2o_types %in% c('numeric', 'enum'), columns]
    selectInput(ns('select_response_selector'), choices=choices, label='Response', mult=F, selectize=T)
  })
  
  # WEIGHT SELECTOR
  output$select_weight_ui <- renderUI({
    req(h2o_meta_data())
    choices <- h2o_meta_data()[h2o_types %in% c('numeric'), columns]
    selectInput(ns('select_weight_selector'), choices=choices, label='Weight', mult=F, selectize=T)
  })
  
  # ADD STEP BUTTON
  observeEvent(input$increase_steps_btn, {
    # GET CURRENT SELECTED VARIABLES
    curr_vars <- lapply(1:n_steps(), function(x){
      sel_name <- paste0('select_mdl_vars_selector_', x)
      input[[sel_name]]
    })
    
    # INCREASE NUMBER OF STEPS
    n_steps(n_steps()+1)
    
    # UPDATE NEW SELECTORS WITH PREVIOUS SELECTIONS
    for (i in seq_len(n_steps()-1)) {
      sel_name <- paste0('select_mdl_vars_selector_', i)
      updateSelectInput(session, sel_name, selected=curr_vars[[i]])
    }
    
    # UPDATE INTERACTION SELECTORS
    for (int in int_ids()) {
      sel_name <- paste0('select_int_vars_step_selector_', int)
      curr_sel <- input[[sel_name]]
      updateSelectInput(session, sel_name, choices=1:n_steps(), selected=curr_sel)
    }
  })
  
  # REMOVE STEP BUTTON
  observeEvent(input$decrease_steps_btn, {
    # GET CURRENT SELECTED VARIABLES
    curr_vars <- lapply(1:n_steps(), function(x){
      sel_name <- paste0('select_mdl_vars_selector_', x)
      input[[sel_name]]
    })
    
    # DECREASE NUMBER OF STEPS
    n_steps(max(n_steps()-1, 1))
    
    # UPDATE NEW SELECTORS WITH PREVIOUS SELECTIONS
    for (i in 1:n_steps()) {
      sel_name <- paste0('select_mdl_vars_selector_', i)
      updateSelectInput(session, sel_name, selected=curr_vars[[i]])
    }
    
    # UPDATE INTERACTION SELECTORS
    for (int in int_ids()) {
      sel_name <- paste0('select_int_vars_step_selector_', int)
      curr_sel <- input[[sel_name]]
      updateSelectInput(session, sel_name, choices=1:n_steps(), selected=curr_sel)
    }
  })
  
  # INTERACTION SELECTOR LABELS
  output$interaction_labels_ui <- renderUI(
    if (length(int_ids()) > 0) {
      fluidRow(
        column(width=2, strong('Step'), p()),
        column(width=4, strong('Categorical Variable'), p()),
        column(width=4, strong('Numeric Variable'), p()),
        column(width=2)
      )
    }
  )
  
  # ADD INTERACTION BUTTON
  observeEvent(input$add_int_btn, {
    req(h2o_meta_data())
    new_id <- newest_int_id() + 1
    int_ids(c(int_ids(), new_id))
    newest_int_id(new_id)
    stp_choices <- 1:n_steps()
    cat_choices <- h2o_meta_data()[h2o_types=='enum', columns]
    num_choices <- h2o_meta_data()[h2o_types=='numeric', columns]
    stp_selector <- selectInput(nsp('select_int_vars_step_selector_', new_id), choices=stp_choices, label=NULL)
    cat_selector <- selectInput(nsp('select_int_vars_cat_selector_', new_id), choices=cat_choices, label=NULL)
    num_selector <- selectInput(nsp('select_int_vars_num_selector_', new_id), choices=num_choices, label=NULL)
    rem_btn <- actionButton(nsp('rem_int_btn_', new_id), label='Remove')
    int_ui <- fluidRow(
      id = nsp('int_row_', new_id),
      column(width=2, stp_selector),
      column(width=4, cat_selector),
      column(width=4, num_selector),
      column(width=2, rem_btn)
    )
    insertUI(paste0('#', ns('add_int_btn')), where='beforeBegin', ui=int_ui)
  })
  
  # REMOVE INTERACTION BUTTONS
  observe({
    lapply(int_ids(), function(x){
      rem_btn_name <- paste0('rem_int_btn_', x)
      if (!is.null(input[[rem_btn_name]])) {
        if (input[[rem_btn_name]] > 0) {
          ui_name <- paste0('#', nsp('int_row_', x))
          removeUI(ui_name)
          int_ids(setdiff(int_ids(), x))
        }
      }
    })
  })
  
  # CHANGE NUMBER OF STEPS TO 1 FOR ANY DISTRIBUTION EXCEPT TWEEDIE
  observe({
    dist <- input$select_dist_selector
    if (dist != 'tweedie') {
      n_steps(1)
    }
  })
  
  # TWEEDIE POWER SELECTOR
  output$tweedie_params_ui <- renderUI({
    req(input$select_dist_selector)
    if (input$select_dist_selector=='tweedie') {
      numericInput(ns('tweedie_params_input'), label = 'Tweedie power', min = 0, max = 3, value = 1.5)
    }
  })
  
  # REACTIVE VALUE TO STORE REFERENCE LEVEL DATA TABLE
  ref_lvl_tbl <- reactiveVal(data.table())
  
  # SELECTORS FOR ADDING REFERENCE LEVELS
  output$ref_lvl_ui <- renderUI({
    for (i in 1:n_steps()) {
      req(input[[paste0('select_mdl_vars_selector_',i)]])
    }
    
    var_selector <- renderUI({
      choices <- lapply(1:n_steps(), function(x){input[[paste0('select_mdl_vars_selector_',x)]]})
      choices <- unique(unlist(choices))
      enum_vars <- h2o_meta_data()[h2o_types=='enum', columns]
      choices <- intersect(enum_vars, choices)
      selectInput(ns('select_var_relvl_selector'), label='Relevel Variable', choices=choices)
    })
    
    lvl_selector <- renderUI({
      req(input$select_var_relvl_selector)
      var <- input$select_var_relvl_selector
      data_name <- isolate(input$data_file_input)
      choices <- h2o.levels(h2o.getFrame(data_name)[[var]])
      selectInput(ns('set_ref_lvl_selector'), label='Reference Level', choices=choices)
    })
    
    fluidRow(
      column(width=6, var_selector),
      column(width=6, lvl_selector)
    )
  })
  
  # ADD REFERENCE LEVEL BUTTON
  observeEvent(input$add_ref_lvl_btn, {
    req(
      input$select_var_relvl_selector,
      input$set_ref_lvl_selector
    )
    dt1 <- ref_lvl_tbl()
    dt2 <- data.table(vars=input$select_var_relvl_selector, 
                      lvls=input$set_ref_lvl_selector)
    dt3 <- rbindlist(list(dt1, dt2))
    ref_lvl_tbl(dt3)
  })
  
  # REMOVE REFERENCE LEVEL BUTTON
  observeEvent(input$rem_ref_lvl_btn, {
    req(
      input$select_var_relvl_selector, 
      input$set_ref_lvl_selector
    )
    dt1 <- ref_lvl_tbl()
    if (nrow(dt1)>0) {
      dt1 <- dt1[! (vars==input$select_var_relvl_selector & lvls==input$set_ref_lvl_selector)]
      ref_lvl_tbl(dt1)
    }
  })
  
  # DATA TABLE OUTPUT OF REFERENCE LEVELS
  output$ref_lvl_tbl_output <- renderDataTable({
    dt <- copy(ref_lvl_tbl())
    if (nrow(dt)>0) {
      setnames(dt, c('vars', 'lvls'), c('Variable', 'Reference Level'))
    }
    dt
  })
  
  # MODEL NAME INPUT
  output$model_name_ui <- renderUI({
    default <- paste0(input$data_file_input, '_mdl')
    textInput(ns('model_name_input'), label='Model Name', value=default)
  })
  
  # BUILD MODEL BUTTON
  observeEvent(input$build_mdl_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('build_mdl_btn')
    on.exit(shinyjs::enable('build_mdl_btn'))
    
    # REQUIRED INPUTS
    req(
      h2o_is_up(),
      input$data_file_input,
      input$select_weight_selector,
      input$select_dist_selector,
      input$select_response_selector,
      input$model_name_input
    )
    
    for (i in 1:n_steps()) req(input[[paste0('select_mdl_vars_selector_', i)]])
    
    print('Building model...')
    # GET REFERENCE TO DATASET
    dt <- h2o.getFrame(input$data_file_input)
    
    # FILTER ON WEIGHT COLUMN > 0
    dt <- dt[dt[[input$select_weight_selector]]>0,]
    
    # SET REFERENCE LEVELS
    n_refs <- nrow(ref_lvl_tbl())
    for (i in seq_len(n_refs)) {
      var_name <- ref_lvl_tbl()[i, vars]
      lvl_name <- ref_lvl_tbl()[i, lvls]
      # CHECK THAT VARIABLE AND REFERENCE LEVELS EXIST IN DATA
      if (! var_name %in% names(dt)) {
        plog('Variable not found in dataset: ', var_name)
        req(FALSE) # STOPS observeEvent
      }
      if (! lvl_name %in% h2o.levels(dt, var_name)) {
        plog('Reference level ', lvl_name, ' not found in variable ', var_name)
        req(FALSE) 
      }
      # SET REF LEVEL
      dt[[var_name]] <- h2o.relevel(x = dt[[var_name]], y = lvl_name)
      plog('Set reference level of column ', var_name, ' in dataset ', input$data_file_input, ' to level ', lvl_name)
    }
    
    # CHECK THAT INTERACTION PIECES ARE INCLUDED IN STEP VARIABLES
    ints_included <- TRUE
    for (j in int_ids()) {
      int_step <- input[[paste0('select_int_vars_step_selector_', j)]]
      cat_var <- input[[paste0('select_int_vars_cat_selector_', j)]]
      num_var <- input[[paste0('select_int_vars_num_selector_', j)]]
      step_vars <- input[[paste0('select_mdl_vars_selector_', int_step)]]
      if (! cat_var %in% step_vars) {
        plog(cat_var, ' not found in step ', int_step, ' variables.')
        ints_included <- FALSE
      }
      if (! num_var %in% step_vars) {
        plog(num_var, ' not found in step ', int_step, ' variables.')
        ints_included <- FALSE
      }
    }
    req(ints_included)
    
    # CHECK THAT VARIABLES ARE ONLY USED ONCE
    all_vars <- lapply(1:n_steps(), function(x){
      input[[paste0('select_mdl_vars_selector_', x)]]
    })
    all_vars <- unlist(all_vars)
    no_dupes <- TRUE
    for (var in unique(all_vars)) {
      if (sum(all_vars %in% var)>1) {
        plog(var, ' found in multiple steps. Please remove or create duplicate variables with different names.')
        no_dupes <- FALSE
      }
    }
    req(no_dupes)
    
    # IF TWEEDIE DISTRIBUTION IS SELECTED
    if (input$select_dist_selector=='tweedie') {
      
      # GET RESPONSE AND WEIGHT ARGUMENTS (FOR CODE BREVITY)
      input_y <- input$select_response_selector
      input_wt <- input$select_weight_selector
      
      # BUILD MODEL FOR EACH STEP
      for (i in 1:n_steps()) {
        
        # IF FIRST STEP
        if (i==1) {
          # USE THE USER SUPPLIED RESPONSE AND WEIGHT ARGUMENTS
          y <- input_y
          weights_column <- input_wt
          # ELSE USE THE ADJUSTED RESPONSE AND WEIGHT COLUMNS FROM LAST STEP
        } else {
          y <- paste0(input_y, '_adj_', i-1)
          weights_column <- paste0(input_wt, '_adj_', i-1)
        }
        
        # GET INTERACTIONS, IF ANY
        interaction_pairs <- list()
        for (j in int_ids()) {
          int_step <- input[[paste0('select_int_vars_step_selector_', j)]]
          if (int_step==i) {
            cat_var <- input[[paste0('select_int_vars_cat_selector_', j)]]
            num_var <- input[[paste0('select_int_vars_num_selector_', j)]]
            next_ind <- length(interaction_pairs) + 1
            interaction_pairs[[next_ind]] <- c(cat_var, num_var)
          }
        }
        
        # IF THERE ARE NO INTERACTIONS, SET TO NULL
        if (length(interaction_pairs)==0) {
          interaction_pairs <- NULL
        }
        
        # BUILD MODEL USING VARIABLES IN CORRESPONDING SELECTOR
        mdl <- h2o.glm(x = input[[paste0('select_mdl_vars_selector_', i)]],
                       y = y,
                       interaction_pairs = interaction_pairs,
                       training_frame = dt,
                       model_id = paste0(input$model_name_input, '_step_', i),
                       seed = 44124,
                       family = input$select_dist_selector,
                       weights_column = weights_column, 
                       tweedie_variance_power = input$tweedie_params_input,
                       tweedie_link_power = 0,
                       solver = 'IRLSM',
                       lambda = 0,
                       standardize = FALSE,
                       compute_p_values = TRUE,
                       beta_epsilon = 1e-9,
                       remove_collinear_columns = TRUE)
        
        # GET PREDICTIONS FROM THIS STEP
        dt[[paste0('pred_', i)]] <- h2o.predict(mdl, dt)[['predict']]
        
        # CALCULATE TOTAL PREDICTION SO FAR (PRODUCT OF PREDICTIONS)
        if (i==1) {
          dt[['total_pred']] <- dt[['pred_1']]
        } else {
          dt[['total_pred']] <- dt[['total_pred']] * dt[[paste0('pred_',i)]]
        }
        
        # ADJUST RESPONSE AND WEIGHT COLUMN
        dt[[paste0(input_y, '_adj_', i)]] <- dt[[input_y]] / dt[['total_pred']]
        dt[[paste0(input_wt, '_adj_', i)]] <- dt[[input_wt]] * dt[['total_pred']] ^ (2-input$tweedie_params_input)
      }
      # ELSE IF ANY OTHER DISTRIBUTION
    } else {
      # GET INTERACTIONS, IF ANY
      interaction_pairs <- list()
      for (j in int_ids()) {
        cat_var <- input[[paste0('select_int_vars_cat_selector_', j)]]
        num_var <- input[[paste0('select_int_vars_num_selector_', j)]]
        next_ind <- length(interaction_pairs) + 1
        interaction_pairs[[next_ind]] <- c(cat_var, num_var)
      }
      
      # IF THERE ARE NO INTERACTIONS, SET TO NULL
      if (length(interaction_pairs)==0) {
        interaction_pairs <- NULL
      }
      
      # BUILD MODEL USING STEP 1 VARIABLES (MULTI-STEP NOT SUPPORTED)
      mdl <- h2o.glm(x = input$select_mdl_vars_selector_1,
                     y = input$select_response_selector,
                     interaction_pairs = interaction_pairs,
                     training_frame = dt,
                     model_id = input$model_name_input,
                     seed = 44124,
                     family = input$select_dist_selector,
                     weights_column = input$select_weight_selector,
                     solver = 'IRLSM',
                     lambda = 0,
                     standardize = FALSE,
                     compute_p_values = TRUE,
                     remove_collinear_columns = TRUE)
    }
    print('Build complete.')
  })
  
  # SAVE MODEL DESIGN BUTTON
  model_design <- reactiveVal(list())
  observeEvent(input$save_design_btn, {
    req(
      input$select_response_selector,
      input$select_weight_selector,
      input$select_dist_selector,
      input$model_name_input
    )
    
    print('Saving model design...')
    
    # SAVE DESIGN AS LIST OF CURRENT INPUTS
    design <- list()
    
    # GET NUMBER OF STEPS AND INTERACTIONS
    design[['n_steps']] <- n_steps()
    design[['int_ids']] <- int_ids()
    
    # FOR EACH STEP, SAVE VARIABLES INPUTS
    for (i in 1:n_steps()) {
      input_name <- paste0('select_mdl_vars_selector_',i)
      req(input[[input_name]])
      design[[input_name]] <- input[[input_name]] 
    }
    
    # SAVE EACH INTERACTION (STEP, CATEGORICAL PIECE, NUMERICAL PIECE)
    for (id in int_ids()) {
      input_name <- paste0('select_int_vars_step_selector_', id)
      design[[input_name]] <- input[[input_name]]
      input_name <- paste0('select_int_vars_cat_selector_', id)
      design[[input_name]] <- input[[input_name]]
      input_name <- paste0('select_int_vars_num_selector_', id)
      design[[input_name]] <- input[[input_name]]
    }
    
    # SAVE RESPONSE, WEIGHT, AND DISTRIBUTION
    design[['select_response_selector']] <- input$select_response_selector
    design[['select_weight_selector']] <- input$select_weight_selector
    design[['select_dist_selector']] <- input$select_dist_selector
    if (input$select_dist_selector=='tweedie') {
      design[['tweedie_params_input']] <- input$tweedie_params_input
    }
    
    # SAVE REFERENCE LEVELS
    design[['ref_lvl_tbl']] <- ref_lvl_tbl()
    
    # SAVE DESIGN LIST
    save(design, file=here(mdl_lib_path, 'designs', input$model_name_input))
    print('Model design saved.')
  })
  
  # LOAD MODEL DESIGN SELECTOR
  output$load_design_ui <- renderUI({
    designs <- list.files(here(mdl_lib_path, 'designs'))
    selectInput(ns('load_design_selector'), choices=designs, label='Design')
  })
  
  # LOAD MODEL DESIGN
  observeEvent(input$load_design_btn, {
    req(input$load_design_selector, h2o_meta_data())
    print('Loading model design...')
    
    # LOAD DESIGN
    load(here(mdl_lib_path, 'designs', input$load_design_selector))
    
    # LOAD NUMBER OF STEPS
    n_steps(design[['n_steps']])
    
    # LOAD MODEL VARIABLES FOR EACH STEP
    for (i in 1:n_steps()) {
      input_name <- paste0('select_mdl_vars_selector_',i)
      updateSelectInput(session, input_name, selected=design[[input_name]])
    }
    
    # REMOVE CURRENT INTERACTIONS
    for (id in int_ids()) {
      int_name <- paste0('#', nsp('int_row_', id))
      removeUI(int_name)
    }
    int_ids(c())
    
    # INITIALIZE NEW IDS
    newest_int_id(as.integer(Sys.time()))
    
    # LOAD EACH INTERACTION (STEP, CATEGORICAL PIECE, NUMERICAL PIECE)
    stp_choices <- 1:n_steps()
    cat_choices <- h2o_meta_data()[h2o_types=='enum', columns]
    num_choices <- h2o_meta_data()[h2o_types=='numeric', columns]
    
    for (id in design[['int_ids']]) {
      new_id <- newest_int_id() + 1
      newest_int_id(new_id)
      int_ids(c(int_ids(), new_id))
      
      stp_selected <- design[[paste0('select_int_vars_step_selector_', id)]]
      cat_selected <- design[[paste0('select_int_vars_cat_selector_', id)]]
      num_selected <- design[[paste0('select_int_vars_num_selector_', id)]]
      
      stp_selector <- selectInput(nsp('select_int_vars_step_selector_', new_id), choices=stp_choices, selected=stp_selected, label=NULL)
      cat_selector <- selectInput(nsp('select_int_vars_cat_selector_', new_id), choices=cat_choices, selected=cat_selected, label=NULL)
      num_selector <- selectInput(nsp('select_int_vars_num_selector_', new_id), choices=num_choices, selected=num_selected, label=NULL)
      rem_btn <- actionButton(nsp('rem_int_btn_', new_id), label='Remove')
      
      int_ui <- fluidRow(
        id = nsp('int_row_', new_id),
        column(width=2, stp_selector),
        column(width=4, cat_selector),
        column(width=4, num_selector),
        column(width=2, rem_btn)
      )
      insertUI(paste0('#', ns('add_int_btn')), where='beforeBegin', ui=int_ui)
    }
    
    # LOAD RESPONSE, WEIGHT, AND DISTRIBUTION
    updateSelectInput(session, 'select_response_selector', selected=design[['select_response_selector']])
    updateSelectInput(session, 'select_weight_selector', selected=design[['select_weight_selector']])
    updateSelectInput(session, 'select_dist_selector', selected=design[['select_dist_selector']])
    if (design[['select_dist_selector']]=='tweedie') {
      updateNumericInput(session, 'tweedie_params_input', value=design[['tweedie_params_input']])
    }
    
    # LOAD REFERENCE LEVELS
    ref_lvl_tbl(design[['ref_lvl_tbl']])
    
    print('Model design loaded.')
  }, ignoreInit = TRUE)
  
  # SAVE MODEL SELECTOR
  output$save_model_ui <- renderUI({
    req(h2o_is_up())
    input$refresh_mdl_lib_btn # REACTIVE DEPENDENCY ON REFRESH BUTTON
    choices <- h2o.ls()[,1]
    selectInput(ns('select_mdl_save_selector'), label='Save Model', choices=choices, mult=T)
  })
  
  # SAVE MODEL BUTTON
  observeEvent(input$save_model_btn, {
    req(
      h2o_is_up(),
      input$model_name_input,
      input$select_mdl_save_selector,
      input$data_file_input
    )
    
    for (i in 1:n_steps()) req(input[[paste0('select_mdl_vars_selector_', i)]])
    
    print('Saving model...')
    # GET MODEL NAME
    mdl_name <- input$model_name_input
    unlink(here(mdl_lib_path, 'h2o', mdl_name), recursive=TRUE)
    dir.create(here(mdl_lib_path, 'h2o', mdl_name))
    
    # GET AND SAVE EACH SELECTED MODEL
    for (mdl in input$select_mdl_save_selector) {
      h2o.saveModel(h2o.getModel(mdl), path = here(mdl_lib_path, 'h2o', mdl_name), force=TRUE)
    }
    
    # GET ALL VARIABLES
    vars <- c()
    for (i in 1:n_steps()) {
      vars <- c(vars, input[[paste0('select_mdl_vars_selector_', i)]])
    }
    
    # GET ALL INTERACTIONS
    ints <- lapply(int_ids(), function(x){
      cat_var <- input[[paste0('select_int_vars_cat_selector_', x)]]
      num_var <- input[[paste0('select_int_vars_num_selector_', x)]]
      c(cat_var, num_var)
    })
    
    # GET REFERENCE TO H2O DATA
    h2o_data <- h2o.getFrame(input$data_file_input)
    
    # GET TYPES AND NAMES OF FIELDS
    types <- h2o.getTypes(h2o_data)
    names(types) <- h2o.colnames(h2o_data)
    
    # GET MODEL COEFFICIENTS
    coefs <- lapply(input$select_mdl_save_selector, function(x){
      as.data.table(h2o.getModel(x)@model$coefficients_table)
    })
    coefs <- rbindlist(coefs)
    setnames(coefs, c('coefficients', 'names'), c('coef', 'lookup'))
    
    # ZERO OUT NA STD ERROR
    coefs[is.na(std_error), std_error:=0]
    
    # COMPUTER UPPER AND LOWER CI
    coefs[, upper_ci:=coef + 1.96 * std_error]
    coefs[, lower_ci:=coef - 1.96 * std_error]
    
    # LIST TO STORE TABLES, VARIABLE TYPES (MAIN EFFECT VS INT.), SCHEMA (H2O VS SEL), 
    # AND CATEGORICAL PART OF INTERACTIONS
    mdl <- model()
    
    # FOR EACH VARIABLE
    for (var in vars) {
      # IF VARIABLE IS CATEGORICAL
      if (types[[var]]=='enum'){
        # MAKE A TABLE LISTING EACH LEVEL OF THE VARIABLE
        fct <- data.table(levels=h2o.levels(h2o_data, var))
        # MAKE A LOOKUP COLUMN THAT MATCHES THE H2O COEFFICIENT NAMING CONVENTION
        fct[, lookup:=paste0(var, '.', levels)]
        # STORE VARIABLE TYPE
        type <- 'categorical'
        # ELSE IF NUMERICAL
      } else {
        # MAKE A ONE-ROW TABLE CONTAINING THE VARIABLE
        fct <- data.table(levels='')
        fct[, lookup:=var]
        type <- 'numeric'
      }
      
      # RENAME LEVELS COLUMN TO VARIABLE NAME FOR EASIER JOINING LATER
      setnames(fct, 'levels', var)
      
      # GET COEFFICIENTS FOR EACH LEVEL
      fct <- coefs[fct, on=.(lookup)]
      
      # NAs REPRESENT REFERENCE LEVELS
      fct[is.na(coef), coef:=0]
      fct[is.na(upper_ci), upper_ci:=0]
      fct[is.na(lower_ci), lower_ci:=0]
      
      # REMOVE UNNECESSARY COLUMNS
      fct <- fct[, c(var, 'coef', 'lower_ci', 'upper_ci'), with=F]
      
      # STORE DATA IN MODEL OBJECT
      mdl$add(var, fct, type)
    }
    
    # FOR EACH CATEGORICAL-NUMERIC INTERACTION
    for (int in ints) {
      # GET PIECES OF INTERACTION
      enum_var <- int[1]
      num_var <- int[2]
      # CREATE NAME
      int_name <- paste0(enum_var, '_', num_var)
      # CREATE LOOKUP TABLE
      fct <- data.table(levels=h2o.levels(h2o_data, enum_var))
      fct[, lookup:=paste0(int_name, '.', levels)]
      setnames(fct, 'levels', enum_var)
      # LOOKUP FACTORS FOR EACH LEVEL
      fct <- coefs[fct, on=.(lookup)]
      # NAs REPRESENT REFERENCE LEVELS
      fct[is.na(coef), coef:=0]
      fct[is.na(upper_ci), upper_ci:=0]
      fct[is.na(lower_ci), lower_ci:=0]
      # REMOVE UNNECESSARY COLUMNS
      fct <- fct[, c(enum_var, 'coef', 'lower_ci', 'upper_ci'), with=F]
      # STORE DATA IN MODEL OBJECT
      mdl$add(int_name, fct, 'interaction', c(enum_var, num_var))
    }
    
    # SAVE FACTOR LIST
    save(mdl, file=here(mdl_lib_path, 'rdata', mdl_name))
    
    print('Save complete.')
  }, ignoreInit = TRUE)
  
  # MODEL SELECTOR FOR PREDICTIONS
  output$pred_mdl_ui <- renderUI({
    input$refresh_pred_mdl_lib_btn
    mdls <- list.files(here(mdl_lib_path, 'rdata'), full=F)
    selectInput(ns('pred_mdl_selector'), label='Model', choices=mdls)
  })
  
  # DATASET SELECTOR FOR PREDICTIONS
  output$pred_data_ui <- renderUI({
    datasets <- list.dirs(data_lib_path, full=F)
    datasets <- setdiff(datasets, '')
    selectInput(ns('pred_data_selector'), label='Dataset', choices=datasets)
  })
  
  # OBSERVED VALUE SELECTOR FOR PREDICTIONS (FOR NORM ASE CALCULATION)
  output$pred_obs_ui <- renderUI({
    req(input$pred_data_selector)
    meta <- fread(here(data_lib_path, input$pred_data_selector, 'meta.csv'))
    choices <- meta[, columns]
    selectInput(ns('pred_obs_selector'), label='Observed Values', choices=choices)
  })
  
  # WEIGHT SELECTOR FOR PREDICTIONS (FOR NORM ASE CALCULATION)
  output$pred_wt_ui <- renderUI({
    req(input$pred_data_selector)
    meta <- fread(here(data_lib_path, input$pred_data_selector, 'meta.csv'))
    choices <- meta[! types %in% c('factor', 'character'), columns]
    selectInput(ns('pred_wt_selector'), label='Weight', choices=choices)
  })
  
  # NAME INPUT FOR PREDICTION COLUMN
  output$pred_name_ui <- renderUI({
    req(input$pred_mdl_selector)
    default <- paste0(input$pred_mdl_selector, '_pred')
    textInput(ns('pred_name_input'), label='Prediction Column Name', value=default)
  })
  
  # INPUTS FOR RESIDUALS
  output$pred_res_ui <- renderUI({
    req(input$pred_res_checkbox)
    
    # GET DATA COLUMN NAMES
    meta <- fread(here(data_lib_path, input$pred_data_selector, 'meta.csv'))
    choices <- meta[! types %in% c('factor', 'character'), columns]
    
    # NAME INPUT FOR RESIDUAL FIELD
    res_name_default <- paste0(input$pred_mdl_selector, '_res')
    res_name <- textInput(ns('pred_res_name_input'), label='Residual Column Name', value=res_name_default)
    
    # NAME INPUTS FOR ADJUSTED NUMERATOR FIELD
    num_sel <- selectInput(ns('pred_res_num_selector'), label='Numerator', choices=choices)
    num_name <- textInput(ns('pred_res_num_name_input'), label='Adjusted Numerator Name')
    
    # NAME INPUTS FOR ADJUSTED DENOMINATOR FIELD
    den_sel <- selectInput(ns('pred_res_den_selector'), label='Denominator', choices=choices)
    den_name <- textInput(ns('pred_res_den_name_input'), label='Adjusted Denominator Name')
    
    # NUMERIC INPUT FOR TWEEDIE POWER
    tweedie_p <- numericInput(ns('pred_res_tweedie_input'), label='Tweedie Power', value=1.5)
    
    # ASSEMBLE UI
    fluidRow(
      column(width=4, res_name, tweedie_p),
      column(width=4, num_sel, num_name),
      column(width=4, den_sel, den_name)
    )
  })
  
  # NAME INPUT FOR PREDICTION FILE
  output$pred_file_ui <- renderUI({
    req(input$pred_mdl_selector)
    default <- paste0(input$pred_mdl_selector, '_fcts')
    textInput(ns('pred_file_input'), label='Prediction File Name', value=default)
  })
  
  # REACTIVE VALUE TO STORE NORMALIZED ASE
  normalized_ase <- reactiveVal(NULL)
  
  # PREDICT BUTTON
  observeEvent(input$score_data_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('score_data_btn')
    on.exit(shinyjs::enable('score_data_btn'))
    
    # REQUIRED INPUTS
    req(
      input$pred_mdl_selector,
      input$pred_obs_selector,
      input$pred_wt_selector,
      input$pred_data_selector,
      input$pred_file_input,
      input$pred_name_input
    )
    
    print('Scoring data...')
    
    # LOAD MODEL
    load(here(mdl_lib_path, 'rdata', input$pred_mdl_selector))
    
    # LOAD DATA
    meta <- fread(here(data_lib_path, input$pred_data_selector, 'meta.csv'))
    dt_types <- meta$types
    names(dt_types) <- meta$columns
    dt <- fread(here(data_lib_path, input$pred_data_selector, 'data.csv'), colClasses=dt_types)
    
    # PREDICT
    preds <- mdl$predict(dt)
    
    # CALCULATE NORMALIZED ASE
    actuals <- dt[[input$pred_obs_selector]]
    predicted <- preds[['PRED']]
    weight <- dt[[input$pred_wt_selector]]
    ase <- as.integer(nase(actuals, predicted, weight))
    normalized_ase(ase)
    
    # APPEND PREDICTIONS TO DATA
    dt[, c(input$pred_name_input):=preds$PRED]
    new_meta_row <- data.table(columns=input$pred_name_input, types='numeric', cardinality=0)
    meta <- rbindlist(list(meta, new_meta_row))
    
    # APPEND RESIDUALS TO DATA IF OPTED
    if (input$pred_res_checkbox) {
      # GET COLUMNS TO ADJUST AND NAMES TO GIVE ADJUSTED COLUMNS
      res_name <- input$pred_res_name_input
      num_sel <- input$pred_res_num_selector
      num_name <- input$pred_res_num_name_input
      den_sel <- input$pred_res_den_selector
      den_name <- input$pred_res_den_name_input
      
      # GET TWEEDIE P PARAMETER
      tweedie_p <- input$pred_res_tweedie_input
      
      # CALCULATE RESIDUAL
      dt[, c(res_name):=get(input$pred_obs_selector)/get(input$pred_name_input)]
      
      # CREATE ADJUSTED NUMERATOR AND DENOMINATOR COLUMNS
      dt[, c(num_name):=get(num_sel) * get(input$pred_name_input) ^ (1 - tweedie_p)]
      dt[, c(den_name):=get(den_sel) * get(input$pred_name_input) ^ (2 - tweedie_p)]
      
      # APPEND NEW COLUMNS TO META DATA FILE
      new_cols <- c(res_name, num_name, den_name)
      new_meta_row <- data.table(columns=new_cols, types='numeric', cardinality=0)
      meta <- rbindlist(list(meta, new_meta_row))
    }
    
    # REMOVE DUPLICATES IN META DATA IN CASE WE'VE APPENDED THIS PREDICITON BEFORE
    last_inds <- meta[, .I[.N], by=columns]$V1
    meta <- meta[last_inds]
    
    # SAVE DATA
    fwrite(dt, here(data_lib_path, input$pred_data_selector, 'data.csv'))
    fwrite(meta, here(data_lib_path, input$pred_data_selector, 'meta.csv'))
    
    # SAVE PREDICTIONS
    fwrite(preds, here(data_lib_path, input$pred_data_selector, paste0(input$pred_file_input, '.csv')))
    print('Score complete.')
  })
  
  # INFO BOX FOR NORMALIZED ASE
  output$nase_info_box <- renderInfoBox({
    infoBox(
      title = strong('Normalized ASE'),
      value = formatC(normalized_ase(), format='d', big.mark=','),
      icon = icon('crosshairs'),
      color = 'blue', 
      fill = TRUE
    )
  })
  
  # MODEL SELECTOR FOR COEFFICIENT PREVIEW
  output$coef_preview_ui <- renderUI({
    mdls <- list.files(here(mdl_lib_path, 'rdata'))
    selectInput(ns('coef_preview_selector'), choices=mdls, label='Model')
  })
  
  # REACTIVE VALUE TO STORE COEFFICIENT TABLE
  coef_tbl <- reactiveVal(data.table())
  
  # COEFFICIENT PREVIEW BUTTON
  observeEvent(input$coef_preview_btn, {
    req(input$coef_preview_selector)
    load(here(mdl_lib_path, 'rdata', input$coef_preview_selector))
    coef_cols <- c('coef', 'lower_ci', 'upper_ci')
    tbls <- lapply(mdl$vars, function(x){
      tbl <- mdl$coefs[[x]]
      lvl.col <- setdiff(names(tbl), coef_cols)
      tbl[, parameter:=x]
      tbl <- tbl[, c('parameter', lvl.col, coef_cols), with=F]
      setnames(tbl, lvl.col, 'levels')
      tbl
    })
    tbls <- rbindlist(tbls)
    tbls <- tbls[, c(coef_cols):=lapply(.SD, round, 5), .SDcols=c(coef_cols)]
    coef_tbl(tbls)
  })
  
  # COEFFICIENT PREVIEW
  output$coef_preview_table_output <- renderDT({
    coef_tbl()
  })
  
  # MODEL EDITOR: MODEL SELECTOR
  output$edit_models_ui <- renderUI({
    mdls <- list.files(here(mdl_lib_path, 'rdata'))
    selectInput(ns('edit_models_selector'), choices=mdls, label='Model(s)', mult=T)
  })
  
  # MODEL EDITOR: INCLUDE VARIABLES SELECTOR
  output$edit_incl_vars_ui <- renderUI({
    req(input$edit_models_selector)
    vars <- sapply(input$edit_models_selector, function(x){
      load(here(mdl_lib_path, 'rdata', x))
      return(mdl$vars)
    })
    selectInput(ns('edit_incl_vars_selector'), choices=vars, label='Include Variables', mult=T)
  })
  
  # MODEL EDITOR: EXCLUDE VARIABLES SELECTOR
  output$edit_excl_vars_ui <- renderUI({
    req(input$edit_models_selector)
    vars <- sapply(input$edit_models_selector, function(x){
      load(here(mdl_lib_path, 'rdata', x))
      return(mdl$vars)
    })
    selectInput(ns('edit_excl_vars_selector'), choices=vars, label='Exclude Variables', mult=T)
  })
  
  # MODEL EDITOR: SAVE MODEL
  observeEvent(input$edit_save_btn, {
    req(input$edit_models_selector)
    
    # CREATE EMPTY MODEL
    new_mdl <- model()
    
    # FOR EACH MODEL
    for (mdl_name in input$edit_models_selector) {
      # LOAD MODEL (object name: mdl)
      load(here(mdl_lib_path, 'rdata', mdl_name))
      # GET CHOSEN VARIABLE NAMES
      vars <- intersect(input$edit_incl_vars_selector, mdl$vars)
      # DO NOT INCLUDE SPECIFIED VARIABLES
      vars <- setdiff(vars, input$edit_excl_vars_selector)
      # DO NOT INCLUDE DUPLICATES
      vars <- setdiff(vars, new_mdl$vars)
      # GET SELECTED COMPONENTS
      sel_coefs <- vars[vars %in% names(mdl$coefs)]
      sel_types <- vars[vars %in% names(mdl$types)]
      sel_ints <- vars[vars %in% names(mdl$ints)]
      # APPEND TO NEW MODEL: VARIABLE NAMES, COEFFICIENTS, VARIABLE TYPES, INTERACTIONS
      new_mdl$vars <- c(new_mdl$vars, vars)
      new_mdl$coefs <- c(new_mdl$coefs, mdl$coefs[sel_coefs])
      new_mdl$types <- c(new_mdl$types, mdl$types[sel_types])
      new_mdl$ints <- c(new_mdl$ints, mdl$ints[sel_ints])
    }
    
    # SAVE MODEL
    mdl <- new_mdl
    file_path <- here(mdl_lib_path, 'rdata', input$edit_model_name)
    save(mdl, file=file_path)
    plog(input$edit_model_name, ' saved to ', file_path)
  })
}
