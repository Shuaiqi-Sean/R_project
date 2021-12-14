
# ANALYSIS TAB UI
analysis_tab_ui <- function(id) {
  
  # NAMESPACE TAG
  ns <- NS(id)
  
  # UI
  analysis_tab <- tabItem(
    shinyjs::useShinyjs(),
    tabName = 'analysis_tab',
    fluidRow(
      column(
        width = 4,
        ### GENERATE NEW TABLE ###
        box(
          title = 'Factor Table Design',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          tabsetPanel(
            tabPanel(
              title = 'New',
              p(),
              uiOutput(ns('select_fct_data_ui')),
              uiOutput(ns('select_agg_ui')),
              actionButton(ns('refresh_fct_data_btn'), label='Refresh Library'),
              p(),
              strong('Models and Factor Variables'),
              p(),
              uiOutput(ns('select_fct_mdl_ui')),
              actionButton(ns('refresh_fct_mdl_btn'), label='Refresh Library'),
              actionButton(ns('add_mdl_sel_btn'), label='+'),
              actionButton(ns('rem_mdl_sel_btn'), label='-'),
              p(),
              uiOutput(ns('select_tbl_type_ui')),
              p(),
              uiOutput(ns('select_weight_var_ui')),
              uiOutput(ns('select_group_var_ui')),
              uiOutput(ns('select_enum_var_ui')),
              uiOutput(ns('select_num_var_ui')),
              uiOutput(ns('fct_tbl_name_ui')),
              actionButton(ns('generate_table_btn'), label='Generate Table'),
              actionButton(ns('save_to_design_btn'), label='Save to Design')
            ),
            tabPanel(
              title = 'Add',
              p(),
              uiOutput(ns('new_mdl_add_ui')),
              uiOutput(ns('new_mdl_var_ui')),
              uiOutput(ns('add_mdl_tbl_ui')),
              actionButton(ns('add_mdl_btn'), label='Add to Table')
            ),
            tabPanel(
              title = 'Apply',
              p(),
              uiOutput(ns('fct_tbl_design_ui')),
              tableOutput(ns('fct_tbl_design_output')),
              actionButton(ns('fct_tbl_design_apply_btn'), label='Apply All Designs'),
              actionButton(ns('fct_tbl_design_rem_btn'), label='Remove Table')
            ),
            tabPanel(
              title = 'Product',
              p(),
              uiOutput(ns('product_table_load_ui')),
              actionButton(ns('product_table_load_btn'), label='Load'),
              actionButton(ns('product_table_delete_btn'), label='Delete'),
              p(),
              uiOutput(ns('product_table_name_ui')),
              uiOutput(ns('product_table_ui')),
              uiOutput(ns('product_table_join_ui')),
              uiOutput(ns('product_table_fct_ui')),
              p('This tool will use the dataset and aggregation columns selected on New tab.'),
              actionButton(ns('product_table_add_btn'), label='Add Product Table'),
              actionButton(ns('product_table_clear_btn'), label='Clear')
            ),
            tabPanel(
              title = 'Load',
              p(),
              uiOutput(ns('load_fct_tbls_ui')),
              actionButton(ns('load_fct_tbls_btn'), label='Load Factor Tables')
            ),
            tabPanel(
              title = 'Save',
              p(),
              textInput(ns('fct_tbls_name_input'), label='Factor Tables Name', placeholder='new_factor_tables'),
              actionButton(ns('save_fct_tbls_btn'), label='Save Factor Tables')
            )
          )
        )
      ),
      
      column(
        width = 4,
        ### REBASE DESIGN ###
        box(
          title = 'Rebase Design',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          tabsetPanel(
            tabPanel(
              title = 'New',
              p(),
              checkboxInput(ns('rebase_to_lvl_checkbox'), label='Use rebase-to-level method', value=FALSE),
              checkboxInput(ns('rebase_base_rate_checkbox'), label='Push adjustment to base rate', value=FALSE),
              uiOutput(ns('rebase_tgt_table_ui')),
              uiOutput(ns('rebase_adj_table_ui')),
              uiOutput(ns('rebase_factor_ui')),
              uiOutput(ns('rebase_source_ui')),
              uiOutput(ns('rebase_join_ui')),
              uiOutput(ns('rebase_weight_ui')),
              uiOutput(ns('rebase_wtd_avg_adj_ui')),
              uiOutput(ns('rebase_lvl_var_ui')),
              uiOutput(ns('rebase_lvl_base_rate_ui')),
              uiOutput(ns('rebase_lvl_map_ui')),
              textInput(ns('rebase_rule_name_input'), label='Rule Name'),
              actionButton(ns('rebase_save_btn'), label='Save to Design')
            ),
            tabPanel(
              title = 'Copy',
              p(),
              'This tool will duplicate the current rebasing rules with the following overrides.',
              p(),
              uiOutput(ns('rebase_design_override_factor_ui')),
              uiOutput(ns('rebase_design_override_source_ui')),
              uiOutput(ns('rebase_design_override_suffix_ui')),
              actionButton(ns('rebase_design_override_btn'), label='Copy Design')
            ),
            tabPanel(
              title = 'View',
              p(),
              uiOutput(ns('rebase_design_rule_ui')),
              tableOutput(ns('rebase_design_table_output')),
              actionButton(ns('rebase_design_del_rule_btn'), label='Delete Rule'),
              actionButton(ns('rebase_increase_priority_btn'), label='Increase Priority'),
              actionButton(ns('rebase_decrease_priority_btn'), label='Decrease Priority')
            ),
            tabPanel(
              title = 'Load',
              p(),
              uiOutput(ns('rebase_design_load_ui')),
              actionButton(ns('rebase_design_load_btn'), label='Load Design')
            ),
            tabPanel(
              title = 'Save',
              p(),
              textInput(ns('rebase_design_name_input'), label='Design Name'),
              actionButton(ns('rebase_design_save_btn'), label='Save Design')
            )
          )
        )
      ),
      
      column(
        width = 4,
        # ANALYSIS FIELDS
        box(
          title = 'Analysis Fields',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          tabsetPanel(
            tabPanel(
              title = '% Difference',
              p(),
              uiOutput(ns('compare_field_1_ui')),
              uiOutput(ns('compare_field_2_ui')),
              textInput(ns('compare_field_name_input'), value='pct_diff', label='Field Name'),
              actionButton(ns('compare_field_create_btn'), label='Create Field')
            ),
            tabPanel(
              title = 'Highlighting',
              p(),
              uiOutput(ns('highlight_lower_bound_ui')),
              uiOutput(ns('highlight_upper_bound_ui')),
              uiOutput(ns('highlight_value_ui')),
              actionButton(ns('highlight_apply_btn'), label='Apply Highlighting')
            ),
            tabPanel(
              title = 'Hide Fields',
              p(),
              uiOutput(ns('hide_fields_ui'))
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        ### FACTOR TABLES ###
        box(
          title = actionLink(ns('display_refresh'), 'Factor Tables', icon=icon('refresh')),
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          uiOutput(ns('factor_table_output'))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        ### FACTOR CHART ###
        box(
          title = 'Chart',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          fluidRow(
            column(width=3, uiOutput(ns('sel_chart_tbl_ui'))),
            column(width=3, uiOutput(ns('sel_chart_var_ui'))),
            column(width=3, uiOutput(ns('sel_chart_fct_ui'))),
            column(width=3, uiOutput(ns('sel_chart_bar_ui')))
          ),
          plotlyOutput(ns('analysis_chart'))
        )
      ),
      
      column(
        width = 3,
        ### SAVE FACTOR SELECTS AS MODEL ###
        box(
          title = 'Save Model',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          'Note: the selectors allow multiple columns to be specified.  
          The tool will search for each column specified in order and use the first one found, so order matters.',
          p(),
          textInput(ns('save_sel_mdl_name_input'), label='Model Name', placeholder='new_sel_mdl'),
          uiOutput(ns('save_sel_fct_ui')),
          uiOutput(ns('save_sel_lower_ci_ui')),
          uiOutput(ns('save_sel_upper_ci_ui')),
          uiOutput(ns('save_sel_tbls_ui')),
          actionButton(ns('save_sel_mdl_btn'), label='Save Model')
        )
      ),
      
      column(
        width = 3,
        ### EXPORT FACTOR TABLES TO EXCEL ###
        box(
          title = 'Export to Excel',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          textInput(ns('export_xlsx_name_input'), label='File Name'),
          actionButton(ns('export_xlsx_btn'), label='Export Tables')
        )
      )
    )
  )
}

# ANALYSIS TAB SERVER
analysis_tab_server <- function(input, output, session) {
  
  # NAMESPACE TAG
  ns <- session$ns
  nsp <- function(...) {
    args <- list(...)
    ns(do.call(paste0, args))
  }
  
  # REACTIVE VALUE TO STORE CURRENT FACTOR TABLES
  fct_tbls <- factor_tables()
  
  # REACTIVE VALUE TO STORE AGGREGATION AND FORMATTING FUNCTIONS
  val_funcs <- reactiveVal()
  val_fmts <- reactiveVal()
  val_vars <- reactiveVal()
  
  # REACTIVE VALUES TO OBSERVE DATA CHANGES
  fct_update <- reactiveVal(0)
  name_update <- reactiveVal(0)
  trigger_fct_update <- function() fct_update(isolate(fct_update())+1)
  trigger_name_update <- function() name_update(isolate(name_update())+1)
  
  # REACTIVE VALUE TO STORE NUMBER OF MODELS
  num_mdls <- reactiveVal(1)
  
  # REACTIVE VALUE TO STORE REBASING RULES
  rebase_design_list <- reactiveVal(list())
  
  # REACTIVE VALUE TO STORE SELECTED META DATA
  analysis_meta_data <- reactiveVal(FALSE)
  
  # OBSERVER TO UPDATE META DATA
  observe({
    req(input$select_fct_data_selector)
    data_name <- input$select_fct_data_selector
    meta_data <- fread(here(data_lib_path, data_name, 'meta.csv'))
    analysis_meta_data(meta_data)
  })
  
  # DATASET SELECTOR
  output$select_fct_data_ui <- renderUI({
    input$refresh_fct_data_btn
    datasets <- list.dirs(data_lib_path, full=F)
    datasets <- setdiff(datasets, '')
    selectInput(ns('select_fct_data_selector'), choices=datasets, label='Dataset')
  })
  
  # AGGREGATION SELECTOR
  output$select_agg_ui <- renderUI({
    aggregators <- list.files(here(analysis_lib_path, 'aggregators'))
    selectInput(ns('select_agg_selector'), choices=aggregators, label='Aggregation Columns')
  })
  
  # MODEL AND FACTOR VARIABLE SELECTORS
  output$select_fct_mdl_ui <- renderUI({
    input$refresh_fct_mdl_btn
    mdls <- list.files(here(mdl_lib_path, 'rdata'), full=F)
    lapply(1:num_mdls(), function(x) {
      sel_1 <- selectInput(nsp('select_fct_mdl_selector_', x), choices=mdls, label=NULL)
      sel_2 <- selectInput(nsp('select_fct_var_selector_', x), choices=list(), label=NULL, multiple=T)
      fluidRow(
        column(width=6, sel_1),
        column(width=6, sel_2)
      )
    })
  })
  
  # UPDATE FACTOR VARIABLE SELECTORS BASED ON MODEL
  observe({
    name_update()
    lapply(1:num_mdls(), function(x){
      req(input[[paste0('select_fct_mdl_selector_', x)]])
      
      # GET SELECTED MODEL
      mdl_name <- input[[paste0('select_fct_mdl_selector_', x)]]
      load(here(mdl_lib_path, 'rdata', mdl_name))
      
      # GET VARIABLES CURRENTLY IN DESIGNS
      designed_vars <- lapply(fct_tbls$designs, function(x){
        x$mdl_list[[mdl_name]]
      })
      designed_vars <- unlist(designed_vars)
      names(designed_vars) <- NULL
      designed_vars <- if (!is.null(designed_vars)) sort(designed_vars) else NULL
      
      # GET VARIABLES NOT DESIGNED
      undesigned_vars <- setdiff(mdl$vars, designed_vars)
      undesigned_vars <- if (!is.null(undesigned_vars)) sort(undesigned_vars) else NULL
      
      # FACTOR CHOICES: CREATE GROUPS FOR BUILT AND NON-BUILT
      choices <- list(
        'Undesigned' = as.list(undesigned_vars),
        'Designed' = as.list(designed_vars)
      )
      
      # MAINTAIN PREVIOUS SELECTION
      selected <- isolate(input[[paste0('select_fct_var_selector_',x)]])
      
      # UPDATE CHOICES
      updateSelectInput(session, paste0('select_fct_var_selector_',x), choices=choices, selected=selected)
    })
  })
  
  # INCREASE NUMBER OF MODELS BUTTON
  observeEvent(input$add_mdl_sel_btn, {
    num_mdls(num_mdls()+1)
  }, ignoreInit = TRUE)
  
  # DECREASE NUMBER OF MODELS BUTTON
  observeEvent(input$rem_mdl_sel_btn, {
    num_mdls(max(num_mdls()-1, 1))
  }, ignoreInit = TRUE)
  
  # FACTOR TABLE TYPE SELECTOR
  output$select_tbl_type_ui <- renderUI({
    choices <- c('categorical', 'numeric', 'interaction', 'multi-numeric', 'multi-interaction')
    selectInput(ns('select_tbl_type_selector'), label='Table Type', choices=choices)
  })
  
  # WEIGHT SELECTOR
  output$select_weight_var_ui <- renderUI({
    req(analysis_meta_data())
    meta_data <- analysis_meta_data()
    choices <- meta_data[order(columns)][! types %in% c('character', 'factor'), columns]
    selectInput(ns('select_weight_var_selector'), choices=choices, label='Weight')
  })
  
  # GROUP BY SELECTOR
  output$select_group_var_ui <- renderUI({
    req(analysis_meta_data())
    meta_data <- analysis_meta_data()
    choices <- meta_data[order(columns), columns]
    selectInput(ns('select_group_var_selector'), choices=choices, label='Groupings', mult=T)
  })
  
  # CATEGORICAL VARIABLE SELECTOR
  output$select_enum_var_ui <- renderUI({
    req(
      analysis_meta_data(), 
      input$select_group_var_selector, 
      input$select_tbl_type_selector
    )
    meta_data <- analysis_meta_data()
    choices <- meta_data[order(columns), columns]
    by_vars <- input$select_group_var_selector
    selected <- meta_data[columns %in% by_vars][which.max(cardinality), columns]
    if (input$select_tbl_type_selector=='multi-interaction') {
      label <- 'Categorical piece of interaction'
    } else {
      label <- 'Selection Level'
    }
    selectInput(ns('select_enum_var_selector'), choices=choices, selected=selected, label=label)
  })
  
  # NUMERIC VARIABLE SELECTOR
  output$select_num_var_ui <- renderUI({
    req(
      input$select_tbl_type_selector != 'categorical'
    )
    type <- input$select_tbl_type_selector
    if (type %in% c('multi-numeric', 'multi-interaction')) {
      choices <- input$select_group_var_selector
      label <- 'Field containing names of numeric variables'
    } else {
      meta_data <- analysis_meta_data()
      choices <- meta_data[order(columns)][! types %in% c('character', 'factor'), columns]
      label <- 'Numeric Variable'
    }
    selectInput(ns('select_num_var_selector'), choices=choices, label=label)
  })
  
  # FACTOR TABLE NAME INPUT
  output$fct_tbl_name_ui <- renderUI({
    textInput(ns('fct_tbl_name_input'), label='Table Name', value=input$select_enum_var_selector)
  })
  
  # FACTOR TABLE DESIGN ELEMENT SELECTOR
  output$fct_tbl_design_ui <- renderUI({
    name_update()
    choices <- names(fct_tbls$designs)
    selectInput(ns('fct_tbl_design_selector'), label='View Table Design', choices=choices)
  })
  
  # FACTOR TABLE DESIGN OUTPUT
  output$fct_tbl_design_output <- renderTable({
    req(input$fct_tbl_design_selector)
    design <- fct_tbls$designs[[input$fct_tbl_design_selector]]
    design$summary()
  })
  
  # FACTOR TABLE DESIGN REMOVE BUTTON
  observeEvent(input$fct_tbl_design_rem_btn, {
    req(input$fct_tbl_design_selector)
    fct_tbls$delete_table(input$fct_tbl_design_selector)
    trigger_fct_update()
    trigger_name_update()
    plog('Removed table for ', input$fct_tbl_design_selector, '.')
  })
  
  # FACTOR TABLE DESIGN APPLY BUTTON
  observeEvent(input$fct_tbl_design_apply_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('fct_tbl_design_apply_btn')
    on.exit(shinyjs::enable('fct_tbl_design_apply_btn'))
    print('Applying design...')
    fct_tbls$apply_designs()
    trigger_fct_update()
    trigger_name_update()
    print('Design applied.')
  })
  
  # GENERATE TABLE BUTTON
  observeEvent(input$generate_table_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('generate_table_btn')
    on.exit(shinyjs::enable('generate_table_btn'))
    
    # REQUIRED INPUTS
    req(
      input$select_fct_data_selector,
      input$select_weight_var_selector,
      input$fct_tbl_name_input,
      input$select_tbl_type_selector
    )
    for (i in 1:num_mdls()) {
      req(input[[paste0('select_fct_mdl_selector_',i)]])
      req(input[[paste0('select_fct_var_selector_',i)]])
    }
    
    # CREATE EMPTY FACTOR TABLE DESIGN
    design <- table_design()
    
    # CREATE MODEL LIST
    mdl_names <- sapply(1:num_mdls(), function(x) input[[paste0('select_fct_mdl_selector_', x)]])
    mdl_vars <- lapply(1:num_mdls(), function(x) input[[paste0('select_fct_var_selector_', x)]])
    mdl_list <- mdl_vars
    names(mdl_list) <- mdl_names
    
    # LOAD AGGREGATION COLUMNS
    source(here(analysis_lib_path, 'aggregators', input$select_agg_selector), local=T)
    
    # ASSEMBLE FACTOR TABLE DESIGN
    design$set(
      tbl_name = input$fct_tbl_name_input,
      data_name = input$select_fct_data_selector,
      mdl_list = mdl_list,
      wt_var = input$select_weight_var_selector,
      by_vars = input$select_group_var_selector,
      enum_var = input$select_enum_var_selector,
      num_var = input$select_num_var_selector,
      type = input$select_tbl_type_selector,
      val_funcs = val_funcs(),
      val_vars = val_vars()
    )
    
    # GENERATE TABLE
    fct_tbls$make_data(design)
    trigger_fct_update()
    trigger_name_update()
  })
  
  # SAVE TO DESIGN BUTTON
  observeEvent(input$save_to_design_btn, {
    req(
      input$select_fct_data_selector,
      input$select_weight_var_selector,
      input$fct_tbl_name_input,
      input$select_tbl_type_selector
    )
    for (i in 1:num_mdls()) {
      req(input[[paste0('select_fct_mdl_selector_',i)]])
      req(input[[paste0('select_fct_var_selector_',i)]])
    }
    
    # CREATE EMPTY FACTOR TABLE DESIGN
    design <- table_design()
    
    # CREATE MODEL LIST
    mdl_names <- sapply(1:num_mdls(), function(x) input[[paste0('select_fct_mdl_selector_', x)]])
    mdl_vars <- lapply(1:num_mdls(), function(x) input[[paste0('select_fct_var_selector_', x)]])
    mdl_list <- mdl_vars
    names(mdl_list) <- mdl_names
    
    # LOAD AGGREGATION COLUMNS
    source(here(analysis_lib_path, 'aggregators', input$select_agg_selector), local=T)
    
    # ASSEMBLE FACTOR TABLE DESIGN
    design$set(
      tbl_name = input$fct_tbl_name_input,
      data_name = input$select_fct_data_selector,
      mdl_list = mdl_list,
      wt_var = input$select_weight_var_selector,
      by_vars = input$select_group_var_selector,
      enum_var = input$select_enum_var_selector,
      num_var = input$select_num_var_selector,
      type = input$select_tbl_type_selector,
      val_funcs = val_funcs(),
      val_vars = val_vars()
    )
    
    # APPEND TO DESIGN LIST
    fct_tbls$add_design(design)
    trigger_name_update()
  })
  
  # ADD: NEW MODEL SELECTOR
  output$new_mdl_add_ui <- renderUI({
    input$refresh_fct_mdl_btn
    mdls <- list.files(here(mdl_lib_path, 'rdata'), full=F)
    selectInput(ns('new_mdl_add_selector'), choices=mdls, label='New Model')
  })
  
  # ADD: NEW MODEL VARIABLE SELECTOR
  output$new_mdl_var_ui <- renderUI({
    req(input$new_mdl_add_selector)
    load(here(mdl_lib_path, 'rdata', input$new_mdl_add_selector))
    choices <- mdl$vars
    choices <- sort(choices)
    selectInput(ns('new_mdl_var_selector'), choices=choices, label='Variable', mult=T)
  })
  
  # ADD: TABLE SELECTOR
  output$add_mdl_tbl_ui <- renderUI({
    name_update()
    choices <- fct_tbls$get_names()
    selectInput(ns('add_mdl_tbl_selector'), choices=choices, label='Table')
  })
  
  # ADD: ADD TO TABLE BUTTON
  observeEvent(input$add_mdl_btn, {
    req(
      input$new_mdl_add_selector,
      input$new_mdl_var_selector,
      input$add_mdl_tbl_selector
    )
    fct_tbls$sync_data(input)
    fct_tbls$add_to_fct_table(
      input$add_mdl_tbl_selector,
      input$new_mdl_add_selector,
      input$new_mdl_var_selector
    )
    fct_tbls$sync_rhots()
    trigger_fct_update()
    trigger_name_update()
  })
  
  # PRODUCT TABLES: LOADED TABLES SELECTOR
  output$product_table_load_ui <- renderUI({
    name_update()
    choices <- names(fct_tbls$prod_tbls)
    selectInput(ns('product_table_load_selector'), choices=choices, label='Load Table')
  })
  
  # PRODUCT TABLES: LOAD BUTTON
  loaded_prod_tbl <- reactiveValues()
  observeEvent(input$product_table_load_btn, {
    req(input$product_table_load_selector)
    tryCatch({
      prod_tbl <- fct_tbls$prod_tbls[[input$product_table_load_selector]]
      loaded_prod_tbl$name <- prod_tbl$name
      loaded_prod_tbl$table_names <- prod_tbl$table_names
      loaded_prod_tbl$join_cols <- prod_tbl$join_cols
      loaded_prod_tbl$fct_cols <- prod_tbl$fct_cols
    }, warning = function(w){
      print('Failed to load product table.')
      print(w)
    }, error = function(e){
      print('Failed to load product table.')
      print(e)
    })
  })
  
  # PRODUCT TABLES: DELETE BUTTON
  observeEvent(input$product_table_delete_btn, {
    req(input$product_table_load_selector)
    fct_tbls$delete_table(input$product_table_load_selector)
    trigger_name_update()
  })
  
  # PRODUCT TABLES: NAME INPUT
  output$product_table_name_ui <- renderUI({
    textInput(ns('product_table_name_input'), label='Name', value=loaded_prod_tbl$name)
  })
  
  # PRODUCT TABLES: TABLE SELECTOR
  output$product_table_ui <- renderUI({
    name_update()
    choices <- fct_tbls$get_names()
    selectInput(ns('product_table_selector'), choices=choices, selected=loaded_prod_tbl$table_names, label='Tables', mult=T)
  })
  
  # PRODUCT TABLES: JOIN SELECTOR
  output$product_table_join_ui <- renderUI({
    fct_update()
    choices <- lapply(input$product_table_selector, function(x) names(fct_tbls$data[[x]]))
    choices <- unique(unlist(choices))
    selectInput(ns('product_table_join_selector'), choices=choices, selected=loaded_prod_tbl$join_cols, label='Join', mult=T)
  })
  
  # PRODUCT TABLES: FACTOR SELECTOR
  output$product_table_fct_ui <- renderUI({
    fct_update()
    choices <- lapply(input$product_table_selector, function(x) names(fct_tbls$data[[x]]))
    choices <- Reduce(intersect, choices)
    selectInput(ns('product_table_fct_selector'), choices=choices, selected=loaded_prod_tbl$fct_cols, label='Factors', mult=T)
  })
  
  # PRODUCT TABLES: ADD PRODUCT TABLE BUTTON
  observeEvent(input$product_table_add_btn, {
    req(
      input$product_table_name_input,
      input$product_table_selector,
      input$product_table_join_selector,
      input$product_table_fct_selector
    )
    
    # LOAD AGGREGATION COLUMNS
    source(here(analysis_lib_path, 'aggregators', input$select_agg_selector), local=T)
    
    tryCatch({
      fct_tbls$add_prod_tbl(
        data_name = input$select_fct_data_selector,
        val_funcs = val_funcs(),
        val_vars = val_vars(),
        name = input$product_table_name_input,
        table_names = input$product_table_selector,
        join_cols = input$product_table_join_selector,
        fct_cols = input$product_table_fct_selector
      )
    }, warning = function(w){
      print(w)
    }, error = function(e){
      print(e)
    })
    trigger_name_update()
  })
  
  # PRODUCT TABLES: CLEAR BUTTON
  observeEvent(input$product_table_clear_btn, {
    loaded_prod_tbl$name <- NULL
    loaded_prod_tbl$table_names <- NULL
    loaded_prod_tbl$join_cols <- NULL
    loaded_prod_tbl$fct_cols <- NULL
  })
  
  # REBASING: TABLE SELECTOR
  output$rebase_tgt_table_ui <- renderUI({
    name_update()
    selectInput(ns('rebase_tgt_table_selector'), label='Rebase Table', choices=fct_tbls$get_names())
  })
  
  # REBASING: ADJUST TABLE SELECTOR
  output$rebase_adj_table_ui <- renderUI({
    req(!input$rebase_base_rate_checkbox)
    name_update()
    selectInput(ns('rebase_adj_table_selector'), label='Adjustment Table', choices=fct_tbls$get_names())
  })
  
  # REBASING: FACTOR SELECTOR
  output$rebase_factor_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_adj_table_selector
    )
    choices <- list.files(here(mdl_lib_path, 'rdata'), full=F)
    choices <- c(choices, 'sel')
    selectInput(ns('rebase_factor_selector'), label='Factor Model', choices=choices)
  })
  
  # REBASING: SOURCE FACTOR SELECTOR
  output$rebase_source_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_adj_table_selector
    )
    # GET NUMERIC COLUMNS FROM TARGET TABLE
    choices <- list.files(here(mdl_lib_path, 'rdata'), full=F)
    choices <- c(choices, 'sel')
    selectInput(ns('rebase_source_selector'), label='Source Model', choices=choices)
  })
  
  # REBASING: JOIN VARIABLE SELECTOR
  output$rebase_join_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_adj_table_selector,
      !input$rebase_base_rate_checkbox
    )
    fct_update()
    # GET CATEGORICAL COLUMNS FROM TARGET TABLE
    fct_tbl <- fct_tbls$data[[input$rebase_tgt_table_selector]]
    fct_cols <- sapply(fct_tbl, uclass)
    fct_cols <- names(fct_cols)[fct_cols=='factor']
    # GET CATEGORICAL COLUMNS FROM ADJUSTED TABLE
    adj_tbl <- fct_tbls$data[[input$rebase_adj_table_selector]]
    adj_cols <- sapply(adj_tbl, uclass)
    adj_cols <- names(adj_cols)[adj_cols=='factor']
    # GET THE COMMON COLUMNS
    choices <- intersect(fct_cols, adj_cols)
    selectInput(ns('rebase_join_selector'), label='Joining Column', choices=choices)
  })
  
  # REBASING: WEIGHT SELECTOR
  output$rebase_weight_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_adj_table_selector,
      !input$rebase_to_lvl_checkbox
    )
    fct_update()
    # GET NUMERIC COLUMNS FROM TARGET TABLE
    fct_tbl <- fct_tbls$data[[input$rebase_tgt_table_selector]]
    fct_cols <- sapply(fct_tbl, uclass)
    fct_cols <- names(fct_cols)[fct_cols=='numeric']
    # GET NUMERIC COLUMNS FROM ADJUSTED TABLE
    adj_tbl <- fct_tbls$data[[input$rebase_adj_table_selector]]
    adj_cols <- sapply(adj_tbl, uclass)
    adj_cols <- names(adj_cols)[adj_cols=='numeric']
    # GET THE COMMON COLUMNS
    choices <- intersect(fct_cols, adj_cols)
    selectInput(ns('rebase_weight_selector'), label='Weight', choices=choices)
  })
  
  # REBASING: WEIGHTED AVERAGE ADJUSTMENT
  output$rebase_wtd_avg_adj_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_join_selector,
      !input$rebase_to_lvl_checkbox,
      !input$rebase_base_rate_checkbox
    )
    
    # GET TARGET TABLE
    fct_tbl <- fct_tbls$data[[input$rebase_tgt_table_selector]]
    
    # GET JOINING VARIABLE
    join_col <- input$rebase_join_selector
    
    # GET UNIQUE JOIN LEVELS
    req(join_col %in% names(fct_tbl))
    join_lvls <- fct_tbl[, unique(get(join_col))]
    
    # SET NUMBER OF JOIN LEVELS
    rebase_num_join_lvls(length(join_lvls))
    
    # CREATE JOIN SELECTORS
    join_sels <- lapply(1:length(join_lvls), function(x){
      selectInput(nsp('rebase_wtd_avg_adj_join_',x), label=NULL, choices=join_lvls[x])
    })
    
    # CREATE WEIGHTED AVERAGE ADJUSTMENT INPUTS
    adj_sels <- lapply(1:length(join_lvls), function(x){
      numericInput(nsp('rebase_wtd_avg_adj_num_',x), value=1, label=NULL)
    })
    
    # BIND TOGETHER, CREATE A SEPERATE ROW FOR EACH MAPPING FOR UI ALIGNMENT
    lapply(0:length(join_lvls), function(x){
      if (x==0) {
        fluidRow(
          column(width=6, strong('Join Levels'), p()),
          column(width=6, strong('Wtd Avg Adj'), p())
        )
      } else {
        fluidRow(
          column(width=6, join_sels[[x]]),
          column(width=6, adj_sels[[x]])
        )
      }
    })
  })
  
  # LEVEL REBASING: VARIABLE SELECTOR
  output$rebase_lvl_var_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_to_lvl_checkbox
    )
    fct_update()
    # GET CATEGORICAL COLUMNS FROM TARGET TABLE
    fct_tbl <- fct_tbls$data[[input$rebase_tgt_table_selector]]
    choices <- sapply(fct_tbl, uclass)
    choices <- names(choices)[choices=='factor']
    selectInput(ns('rebase_lvl_var_selector'), label='Level Variable', choices=choices)
  })
  
  # LEVEL REBASING: MAPPING SELECTORS
  output$rebase_lvl_map_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_join_selector,
      input$rebase_lvl_var_selector,
      input$rebase_to_lvl_checkbox,
      !input$rebase_base_rate_checkbox
    )
    fct_update()
    
    # GET TARGET TABLE
    fct_tbl <- fct_tbls$data[[input$rebase_tgt_table_selector]]
    
    # GET JOINING VARIABLE AND LEVEL VARIABLE
    join_col <- input$rebase_join_selector
    var_col <- input$rebase_lvl_var_selector
    
    # GET EXISTING MAPPING BETWEEN JOIN AND LEVEL VARIABLE
    req(join_col %in% names(fct_tbl), var_col %in% names(fct_tbl))
    map_lvls <- unique(fct_tbl[, c(join_col, var_col), with=F])
    
    # GET UNIQUE JOIN LEVELS
    join_lvls <- map_lvls[, unique(get(join_col))]
    join_lvls <- sort(join_lvls)
    
    # SET NUMBER OF JOIN LEVELS
    rebase_num_join_lvls(length(join_lvls))
    
    # CREATE JOIN SELECTORS
    join_sels <- lapply(1:length(join_lvls), function(x){
      selectInput(nsp('rebase_lvl_join_map_',x), label=NULL, choices=join_lvls[x])
    })
    
    # CREATE VARIABLE LEVEL SELECTORS
    var_sels <- lapply(1:length(join_lvls), function(x){
      mask <- map_lvls[[join_col]]==join_lvls[x]
      choices <- map_lvls[mask, get(var_col)]
      choices <- sort(choices)
      selectInput(nsp('rebase_lvl_var_map_',x), label=NULL, choices=choices)
    })
    
    fluidRow(
      column(width=6, strong('Join Levels'), p(), join_sels),
      column(width=6, strong('Variable Levels'), p(), var_sels)
    )
  })
  
  # REACTIVE VALUE FOR NUMBER OF JOIN LEVELS
  rebase_num_join_lvls <- reactiveVal(0)
  
  # LEVEL REBASING: PUSH TO BASE RATE
  output$rebase_lvl_base_rate_ui <- renderUI({
    req(
      input$rebase_tgt_table_selector,
      input$rebase_lvl_var_selector,
      input$rebase_to_lvl_checkbox,
      input$rebase_base_rate_checkbox
    )
    fct_update()
    
    # GET TARGET TABLE
    fct_tbl <- fct_tbls$data[[input$rebase_tgt_table_selector]]
    # GET LEVEL VARIABLE
    var_col <- input$rebase_lvl_var_selector
    # GET UNIQUE LEVELS
    req(var_col %in% names(fct_tbl))
    choices <- fct_tbl[, unique(get(var_col))]
    choices <- sort(choices)
    selectInput(ns('rebase_lvl_base_rate_selector'), label='Level', choices=choices)
  })
  
  # REBASING: SAVE TO DESIGN BUTTON
  observeEvent(input$rebase_save_btn, {
    req(
      input$rebase_tgt_table_selector,
      input$rebase_factor_selector,
      input$rebase_source_selector
    )
    
    # GET MAPPED ADJUSTMENT VARIABLE LEVEL FOR EACH JOIN LEVEL (IF ANY)
    if (!input$rebase_to_lvl_checkbox & !input$rebase_base_rate_checkbox) {
      map_joins <- sapply(seq_len(rebase_num_join_lvls()), function(x){
        input[[paste0('rebase_wtd_avg_adj_join_',x)]]
      })
    } else {
      map_joins <- sapply(seq_len(rebase_num_join_lvls()), function(x){
        input[[paste0('rebase_lvl_join_map_',x)]]
      })
    }
    map_vars <- sapply(seq_len(rebase_num_join_lvls()), function(x){
      input[[paste0('rebase_lvl_var_map_',x)]]
    })
    map_adjs <- sapply(seq_len(rebase_num_join_lvls()), function(x){
      input[[paste0('rebase_wtd_avg_adj_num_',x)]]
    })
    
    # CREATE REBASING RULE
    rebase <- rebase_rule()
    rebase$set(
      name = input$rebase_rule_name_input,
      rebase_to_lvl = input$rebase_to_lvl_checkbox,
      rebase_to_base = input$rebase_base_rate_checkbox,
      tgt_tbl_name = input$rebase_tgt_table_selector,
      fct_mdl = input$rebase_factor_selector,
      src_mdl = input$rebase_source_selector,
      var_col = input$rebase_lvl_var_selector,
      var_lvl = input$rebase_lvl_base_rate_selector,
      adj_tbl_name = input$rebase_adj_table_selector,
      join_col = input$rebase_join_selector,
      wt_col = input$rebase_weight_selector,
      map_joins = map_joins,
      map_vars = map_vars,
      map_adjs = map_adjs
    )
    
    # SAVE REBASING RULE
    tmp_design <- rebase_design_list()
    tmp_design[[rebase$name]] <- rebase
    rebase_design_list(tmp_design)
  })
  
  # REBASE DESIGN: RULE SELECTOR
  output$rebase_design_rule_ui <- renderUI({
    choices <- names(rebase_design_list())
    selectInput(ns('rebase_design_rule_selector'), label='View Rule', choices=choices)
  })
  
  # REBASE DESIGN: TABLE OUTPUT
  output$rebase_design_table_output <- renderTable({
    req(input$rebase_design_rule_selector)
    rebase <- rebase_design_list()[[input$rebase_design_rule_selector]]
    req(rebase)
    rebase$summary()
  })
  
  # REBASE DESIGN: SAVE DESIGN BUTTON
  observeEvent(input$rebase_design_save_btn, {
    req(input$rebase_design_name_input)
    design <- rebase_design_list()
    save(design, file=here(analysis_lib_path, 'rebase', input$rebase_design_name_input))
    plog('Design saved: ', input$rebase_design_name_input)
  })
  
  # REBASE DESIGN: DELETE RULE BUTTON
  observeEvent(input$rebase_design_del_rule_btn, {
    req(input$rebase_design_rule_selector)
    design <- rebase_design_list()
    design[[input$rebase_design_rule_selector]] <- NULL
    rebase_design_list(design)
    fct_tbls$delete_column(input$rebase_design_rule_selector)
    plog('Rebase rule deleted: ', input$rebase_design_rule_selector)
  })
  
  # REBASE DESIGN: INCREASE PRIORITY BUTTON
  observeEvent(input$rebase_increase_priority_btn, {
    req(input$rebase_design_rule_selector)
    designs <- rebase_design_list()
    design_names <- sapply(designs, function(x) x$name)
    ind <- which(input$rebase_design_rule_selector==design_names)
    new_order <- c(1:length(designs))[-ind]
    new_order <- c(ind, new_order)
    designs <- designs[new_order]
    rebase_design_list(designs)
  })
  
  # REBASE DESIGN: DECREASE PRIORITY BUTTON
  observeEvent(input$rebase_decrease_priority_btn, {
    req(input$rebase_design_rule_selector)
    designs <- rebase_design_list()
    design_names <- sapply(designs, function(x) x$name)
    ind <- which(input$rebase_design_rule_selector==design_names)
    new_order <- c(1:length(designs))[-ind]
    new_order <- c(new_order, ind)
    designs <- designs[new_order]
    rebase_design_list(designs)
  })
  
  # REBASE DESIGN: LOAD DESIGN SELECTOR
  output$rebase_design_load_ui <- renderUI({
    choices <- list.files(here(analysis_lib_path, 'rebase'), full=F)
    selectInput(ns('rebase_design_load_selector'), label='Load Design', choices=choices)
  })
  
  # REBASE DESIGN: LOAD DESIGN BUTTON
  observeEvent(input$rebase_design_load_btn, {
    req(input$rebase_design_load_selector)
    load(here(analysis_lib_path, 'rebase', input$rebase_design_load_selector))
    rebase_design_list(design)
    plog('Design loaded: ', input$rebase_design_load_selector)
  })
  
  # REBASE DESIGN: OVERRIDE FACTOR MODEL SELECTOR
  output$rebase_design_override_factor_ui <- renderUI({
    choices <- list.files(here(mdl_lib_path, 'rdata'), full=F)
    choices <- c(choices, 'sel')
    selectInput(ns('rebase_design_override_factor_selector'), label='Factor Model Override', choices=choices)
  })
  
  # REBASE DESIGN: OVERRIDE SOURCE MODEL SELECTOR
  output$rebase_design_override_source_ui <- renderUI({
    choices <- list.files(here(mdl_lib_path, 'rdata'), full=F)
    choices <- c(choices, 'sel')
    selectInput(ns('rebase_design_override_source_selector'), label='Source Model Override', choices=choices)
  })
  
  # REBASE DESIGN: OVERRIDE NAME
  output$rebase_design_override_suffix_ui <- renderUI({
    textInput(ns('rebase_design_override_suffix_input'), label='Add Suffix to Rule Names', value='new')
  })
  
  # REBASE DESIGN: OVERRIDE DESIGN BUTTON
  observeEvent(input$rebase_design_override_btn, {
    req(
      input$rebase_design_override_factor_selector,
      input$rebase_design_override_source_selector
    )
    # GET CURRENT REBASE DESIGNS (USE COPY METHOD, ELSE '<-' WILL COPY BY REFERENCE)
    tmp_design_list <- lapply(rebase_design_list(), function(x) x$copy())
    # OVERRIDE EACH REBASING RULE
    for (rebase in tmp_design_list) {
      if (rebase$fct_mdl != rebase$src_mdl) {
        rebase$src_mdl <- input$rebase_design_override_source_selector
      } else {
        rebase$src_mdl <- input$rebase_design_override_factor_selector
      }
      rebase$fct_mdl <- input$rebase_design_override_factor_selector
      rebase$name <- paste0(rebase$name, '_', input$rebase_design_override_suffix_input)
    }
    # RENAME LIST
    names(tmp_design_list) <- paste0(names(tmp_design_list), '_', input$rebase_design_override_suffix_input)
    # COMBINE LISTS
    tmp_design_list <- c(rebase_design_list(), tmp_design_list)
    # OVERWRITE REBASING DESIGN LIST
    rebase_design_list(tmp_design_list)
  })
  
  # ALL TABLE COLUMNS
  all_columns <- reactive({
    req(length(fct_tbls$get_names()>=1))
    fct_update()
    name_update()
    choices <- lapply(fct_tbls$get_names(), function(x) names(fct_tbls$data[[x]]))
    choices <- unique(unlist(choices))
    choices <- tryCatch(sort(choices), warning = function(w) choices)
    choices
  })
  
  # COMPARE FIELD 1 SELECTOR
  output$compare_field_1_ui <- renderUI({
    selectInput(ns('compare_field_1_selector'), choices=all_columns(), label='Compare field:')
  })
  
  # COMPARE FIELD 2 SELECTOR
  output$compare_field_2_ui <- renderUI({
    selectInput(ns('compare_field_2_selector'), choices=all_columns(), label='to:')
  })
  
  # COMPARE FIELD CREATE BUTTON
  compare_fields <- reactiveVal(list())
  observeEvent(input$compare_field_create_btn, {
    req(
      input$compare_field_1_selector, 
      input$compare_field_2_selector,
      input$compare_field_name_input
    )
    # GET ARGUMENTS
    col_1 <- input$compare_field_1_selector
    col_2 <- input$compare_field_2_selector
    col_name <- input$compare_field_name_input
    # CREATE COMPARISON FIELDS LIST
    fields <- list(col_1=col_1, col_2=col_2, col_name=col_name)
    compare_fields(fields)
  }, ignoreInit=TRUE)
  
  # HIGHLIGHTING: UPPER BOUND SELECTOR
  output$highlight_upper_bound_ui <- renderUI({
    selectInput(ns('highlight_upper_bound_selector'), choices=all_columns(), label='Upper Bound')
  })
  
  # HIGHLIGHTING: LOWER BOUND SELECTOR
  output$highlight_lower_bound_ui <- renderUI({
    selectInput(ns('highlight_lower_bound_selector'), choices=all_columns(), label='Lower Bound')
  })
  
  # HIGHLIGHTING: VALUE SELECTOR
  output$highlight_value_ui <- renderUI({
    selectInput(ns('highlight_value_selector'), choices=all_columns(), label='Value')
  })
  
  # HIGHLIGHTING: APPLY BUTTON
  highlight_fields <- reactiveVal()
  observeEvent(input$highlight_apply_btn, {
    # GET ARGUMENTS
    upper_col <- input$highlight_upper_bound_selector
    lower_col <- input$highlight_lower_bound_selector
    val_col <- input$highlight_value_selector
    # APPLY NEW HIGHLIGHT RULE
    highlight_fields(list(
      val_col=val_col,
      lower_col=lower_col,
      upper_col=upper_col
    ))
    trigger_fct_update()
  })
  
  # HIDE FIELDS: FIELD SELECTOR
  output$hide_fields_ui <- renderUI({
    selected <- isolate(input$hide_fields_selector)
    selectInput(ns('hide_fields_selector'), choices=all_columns(), label='Fields', mult=T, selected=selected)
  })
  
  # CHART: FACTOR TABLE SELECTOR
  output$sel_chart_tbl_ui <- renderUI({
    name_update()
    selectInput(ns('sel_chart_tbl_selector'), label='Table', choices=fct_tbls$get_names())
  })
  
  # CHART: VARIABLE SELECTOR
  output$sel_chart_var_ui <- renderUI({
    req(input$sel_chart_tbl_selector)
    fct_update()
    name_update()
    tbl <- fct_tbls$data[[input$sel_chart_tbl_selector]]
    choices <- names(tbl)
    selectInput(ns('sel_chart_var_selector'), label='Variable', choices=choices)
  })
  
  # CHART: FACTOR(S) SELECTOR
  output$sel_chart_fct_ui <- renderUI({
    req(input$sel_chart_tbl_selector)
    fct_update()
    name_update()
    tbl <- fct_tbls$data[[input$sel_chart_tbl_selector]]
    choices <- names(tbl)[grepl('_fct|_lower_ci|_upper_ci', names(tbl))]
    selectInput(ns('sel_chart_fct_selector'), label='Factors', choices=choices, mult=T)
  })
  
  # CHART: BAR SELECTOR
  output$sel_chart_bar_ui <- renderUI({
    req(input$sel_chart_tbl_selector)
    fct_update()
    name_update()
    tbl <- fct_tbls$data[[input$sel_chart_tbl_selector]]
    classes <- sapply(tbl, uclass)
    choices <- names(classes)[classes %in% c('numeric', 'integer')]
    selectInput(ns('sel_chart_bar_selector'), label='Bars', choices=choices)
  })
  
  # FACTOR TABLE CHART
  output$analysis_chart <- renderPlotly({
    req(
      input$sel_chart_tbl_selector,
      input$sel_chart_var_selector,
      input$sel_chart_fct_selector,
      input$sel_chart_bar_selector
    )
    fct_update()
    var <- input$sel_chart_var_selector
    fcts <- input$sel_chart_fct_selector
    bar <- input$sel_chart_bar_selector
    
    # GET FACTOR TABLE
    fct_tbls$sync_data(input)
    tbl <- copy(fct_tbls$data[[input$sel_chart_tbl_selector]])
    tbl <- tbl[, c(var, bar, fcts), with=F]
    setnames(tbl, c(var, bar), c('levels', 'wts'))
    
    # AGGREGATE FACTORS
    agg_fcts <- tbl[, lapply(.SD, weighted.mean, wts), by=levels, .SDcols=fcts]
    agg_wts <- tbl[, .(wts=sum(wts)), by=levels]
    tbl <- agg_wts[agg_fcts, on=.(levels)]
    tbl <- tbl[order(levels)]
    
    # CREATE PLOTLY OBJECT BASED ON SELECTED FACTOR TABLE
    plt <- plot_ly(tbl) 
    
    # ADD A LINE FOR EACH FACTOR COLUMN SPECIFIED
    for (f in fcts) {
      tbl[is.na(tbl[[f]]), c(f):=1]
      plt %>% add_lines(x = ~levels, 
                        y = tbl[[f]], 
                        mode = 'markers',
                        marker = list(size=6, line=list(width=2)),
                        name = f) -> plt
    }
    # ADD BARS
    plt %>% add_trace(x = ~levels, 
                      y = tbl[['wts']], 
                      name = bar, 
                      type = 'bar', 
                      yaxis = 'y2', 
                      marker = list(color = rgb(194, 207, 219, m=255))) -> plt
    # FORMAT PLOT LAYOUT
    plt %>% layout(yaxis = list(side = 'left', overlaying = 'y2'),
                   yaxis2 = list(side = 'right', showline = FALSE, showgrid = FALSE),
                   legend = list(orientation = 'h', y = -0.2)) -> plt
    # RETURN PLOT
    plt
  })
  
  # FACTOR TABLE DATA TABLE OUTPUTS
  observe({
    fct_update()
    name_update()
    
    # APPLY REBASING RULES
    # SYNC FACTOR TABLES TO RHANDSONTABLE DISPLAYS
    fct_tbls$sync_data(input)
    # UNAPPLY REBASING
    fct_tbls$reset_to_raw()
    # CALCULATE PRODUCT TABLES
    fct_tbls$calc_all_product_tables()
    # APPLY EACH REBASING RULE
    for (rebase in rebase_design_list()) {
      tryCatch({
        fct_tbls$data <- rebase$apply(fct_tbls$data)
      }, warning = function(w) {
        plog('Warning apply rebase rule: ', rebase$name)
        print(w)
      }, error = function(e) {
        plog('Error applying rebase rule: ', rebase$name)
        print(e)
      })
    }
    print('Applied rebasing rules.')
    # APPLY COMPARISON COLUMNS
    compare_field <- compare_fields()[['col_name']]
    if (!is.null(compare_field)) {
      fct_tbls$compare_column(
        compare_fields()[['col_1']],
        compare_fields()[['col_2']],
        compare_field
      )
    }
    # UPDATE RHANDSONTABLES
    fct_tbls$sync_rhots()
    # APPLY HIGHLIGHTING
    if (!is.null(highlight_fields())) {
      fct_tbls$highlight(
        highlight_fields()[['val_col']], 
        highlight_fields()[['lower_col']], 
        highlight_fields()[['upper_col']]
      )
    }
    
    # UPDATE / CREATE DATA TABLE OUTPUTS
    lapply(fct_tbls$get_names(), function(x) {
      # CREATE UNIQUE IDENTIFIER
      fct_tbl_name <- paste0(x, '_fct_tbl_output')
      # CREATE OUTPUT OBJECT
      output[[fct_tbl_name]] <- renderRHandsontable({
        # GET RHANDSONTABLE AND COLUMN NAMES
        tbl <- fct_tbls$rhots[[x]]
        # SORT DATA
        sort_cols <- input[[paste0(x, '_sort_selector')]]
        if (!is.null(sort_cols)) {
          dt <- jsonlite::fromJSON(tbl$x$data)
          dt <- as.data.table(dt)
          setorderv(dt, sort_cols)
          dt <- jsonlite::toJSON(dt)
          tbl$x$data <- dt
        }
        # COLUMN NAMES
        cols <- tbl$x$colHeaders
        # FORMAT VALUES USING LIST SPECIFIED IN FACTOR TABLE CLASS
        val_fmt_cols <- names(val_fmts())
        for (fmt in val_fmt_cols) {
          if (fmt %in% cols) {
            tbl <- val_fmts()[[fmt]](tbl)
          }
        }
        # HIDE COLUMNS SELECTED IN ANALYSIS FIELDS TOOL
        for (hide in input$hide_fields_selector) {
          if (hide %in% cols) {
            tbl <- hot_col(tbl, col=hide, colWidths=0.1)
          }
        }
        # FORMAT COMPARISON COLUMNS
        if (!is.null(compare_field)) {
          if (compare_field %in% cols) {
            tbl <- hot_col(tbl, col=compare_field, format='0.00%')
          }
        }
        # ONLY sel_fct_raw SHOULD BE EDITABLE
        non_sel_cols <- setdiff(cols, 'sel_fct_raw')
        tbl <- hot_col(tbl, col=non_sel_cols, readOnly=TRUE)
        # COLUMN NAME WRAPPING
        col_headers <- sapply(cols, function(x){
          x <- str_replace_all(x, '_', ' ')
          x <- strwrap(x, width=10)
          paste(x, collapse='<br>')
        }, USE.NAMES=FALSE)
        tbl$x$colHeaders <- col_headers
        # RETURN TABLE
        tbl
      })
    })
  })
  
  # DYNAMIC TABSET PANEL
  output$factor_table_output <- renderUI({
    fct_update()
    name_update()
    input$display_refresh
    tabs <- lapply(fct_tbls$get_names(), function(x) {
      fct_tbl_name <- nsp(x, '_fct_tbl_output')
      tabPanel(
        x,
        p(),
        selectInput(nsp(x, '_sort_selector'), choices=names(fct_tbls$data[[x]]), label='Sort', mult=T),
        rHandsontableOutput(fct_tbl_name)
      )
    })
    do.call(tabsetPanel, tabs)
  })
  
  # SAVE FACTOR TABLES BUTTON
  observeEvent(input$save_fct_tbls_btn, {
    req(input$fct_tbls_name_input)
    print('Saving factor tables...')
    # SAVE FACTOR TABLES
    fct_tbls$sync_data(input)
    fct_tbl_path <- here(analysis_lib_path, 'tables', input$fct_tbls_name_input)
    save_fct_tbls <- fct_tbls
    save(save_fct_tbls, file=fct_tbl_path)
    print('Save complete.')
  }, ignoreInit = TRUE)
  
  # SAVE MODEL: FACTOR COLUMN SELECTOR
  output$save_sel_fct_ui <- renderUI({
    name_update()
    columns <- sapply(fct_tbls$data, names)
    columns <- unlist(columns)
    columns <- unique(columns)
    columns <- columns[grepl('fct', columns)]
    selectInput(ns('save_sel_fct_selector'), choices=columns, label='Factor Column(s)', mult=T)
  })
  
  # SAVE MODEL: LOWER CI SELECTOR
  output$save_sel_lower_ci_ui <- renderUI({
    name_update()
    columns <- sapply(fct_tbls$data, names)
    columns <- unlist(columns)
    columns <- unique(columns)
    columns <- columns[grepl('lower_ci', columns)]
    selectInput(ns('save_sel_lower_ci_selector'), choices=columns, label='Lower CI', mult=T)
  })
  
  # SAVE MODEL: UPPER CI SELECTOR
  output$save_sel_upper_ci_ui <- renderUI({
    name_update()
    columns <- sapply(fct_tbls$data, names)
    columns <- unlist(columns)
    columns <- unique(columns)
    columns <- columns[grepl('upper_ci', columns)]
    selectInput(ns('save_sel_upper_ci_selector'), choices=columns, label='Upper CI', mult=T)
  })
  
  # SAVE MODEL: TABLE SELECTOR
  output$save_sel_tbls_ui <- renderUI({
    name_update()
    selectInput(ns('save_sel_tbls_selector'), choices=fct_tbls$get_names(), label='Tables', selected=fct_tbls$get_names(), mult=T)
  })
  
  # SAVE MODEL: SAVE BUTTON
  observeEvent(input$save_sel_mdl_btn, {
    req(
      input$save_sel_mdl_name_input,
      input$save_sel_fct_selector,
      input$save_sel_tbls_selector
    )
    print('Saving selected model...')
    
    # GET DESIRED NAME OF NEW SELECTED MODEL AND CORRESPONDING COLUMNS
    mdl_name <- input$save_sel_mdl_name_input
    
    # GET TABLE NAMES
    tbl_names <- input$save_sel_tbls_selector
    
    # GET FACTOR COLUMNS
    fct_cols <- input$save_sel_fct_selector
    
    # GET CONFIDENCE INTERVALS FROM CHOSEN BASIS MODEL
    lower_cis <- input$save_sel_lower_ci_selector
    upper_cis <- input$save_sel_upper_ci_selector
    
    # SYNC DATA TO CURRENT RHANDSONTABLE DISPLAYS
    fct_tbls$sync_data(input)
    
    # CREATE EMPTY MODEL
    mdl <- model()
    
    # LOOP THROUGH FACTOR TABLES AND PULL DESIRED DATA
    for (tbl_name in tbl_names) {
      # GET FACTOR TABLE
      tbl <- fct_tbls$data[[tbl_name]]
      # GET DESIGN (PRODUCT TABLE DESIGNS ARE STORED ELSEWHERE)
      if (tbl_name %in% names(fct_tbls$prod_tbls)) {
        design <- fct_tbls$prod_tbls[[tbl_name]]$design
      } else {
        design <- fct_tbls$designs[[tbl_name]]
      }
      # GET FIRST FACTOR COLUMN FOUND IN TABLE
      basis_fct_col <- fct_cols[fct_cols %in% names(tbl)][1]
      # GET FIRST LOWER AND UPPER CI FOUND IN TABLE
      basis_lower_ci <- lower_cis[lower_cis %in% names(tbl)][1]
      basis_upper_ci <- upper_cis[upper_cis %in% names(tbl)][1]
      # ADD TABLE TO MODEL USING DESIGN INFO
      plog('Table: ', tbl_name, ' Fct: ', basis_fct_col, ' LowerCI: ', basis_lower_ci, ' UpperCI: ', basis_upper_ci)
      mdl$add_design_to_model(design, tbl, basis_fct_col, basis_lower_ci, basis_upper_ci)
    }
    
    # SAVE IF THE MODEL IS GOOD (1 COEFFICIENT FOR EACH LEVEL)
    # ELSE RETURN ERROR
    if (mdl$check_uniqueness()) {
      save(mdl, file=here(mdl_lib_path, 'rdata', mdl_name))
      print('Save complete.')
    } else {
      print('Model not saved.')
      print('Please check factor tables to make sure each level is assigned only 1 factor.')
    }
  })
  
  # EXPORT FACTOR TABLES TO EXCEL BUTTON
  observeEvent(input$export_xlsx_btn, {
    req(input$export_xlsx_name_input)
    print('Exporting tables to excel...')
    # GET FACTOR TABLES
    fct_tbls$sync_data(input)
    tmp_tbl_list <- copy(fct_tbls$data)
    # GET DESIRED NAME
    file_name <- paste0(input$export_xlsx_name_input, '.xlsx')
    # EXPORT DATA
    write.xlsx(tmp_tbl_list, file=here('bin', file_name))
    plog('Tables exported to ', here('bin', file_name))
  })
  
  # LOAD FACTOR TABLES SELECTOR
  output$load_fct_tbls_ui <- renderUI({
    choices <- list.files(here(analysis_lib_path, 'tables'), full=F)
    selectInput(ns('load_fct_tbls_selector'), label = 'Factor Tables', choices = choices)
  })
  
  # LOAD FACTOR TABLES BUTTON
  observeEvent(input$load_fct_tbls_btn, {
    req(input$load_fct_tbls_selector)
    print('Loading factor tables...')
    # LOAD AGGREGATION COLUMNS
    source(here(analysis_lib_path, 'aggregators', input$select_agg_selector), local=T)
    # GET SELECTED FACTOR TABLE SET
    fct_tbl_path <- here(analysis_lib_path, 'tables', input$load_fct_tbls_selector)
    # LOAD FACTOR TABLE DATA
    load_fct_tbls <- get(load(fct_tbl_path))
    # SET FACTOR TABLE LIST
    tryCatch(fct_tbls$data <<- load_fct_tbls$data, 
             error = function(e) fct_tbls$data <<- list())
    tryCatch(fct_tbls$rhots <<- load_fct_tbls$rhots, 
             error = function(e) fct_tbls$rhots <<- list())
    tryCatch(fct_tbls$designs <<- load_fct_tbls$designs, 
             error = function(e) fct_tbls$designs <<- list())
    tryCatch(fct_tbls$prod_tbls <<- load_fct_tbls$prod_tbls,
             error = function(e) fct_tbls$prod_tbls <<- list())
    fct_tbls$sync_rhots()
    # TRIGGER UPDATES
    trigger_fct_update()
    trigger_name_update()
    print('Load complete.')
  }, ignoreInit = TRUE)
}
