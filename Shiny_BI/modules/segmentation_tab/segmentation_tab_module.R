
# SEGMENTATION TAB UI
segmentation_tab_ui <- function(id){
  
  # NAMESPACE TAG
  ns <- NS(id)

  # UI
  tabItem(
    shinyjs::useShinyjs(),
    tabName = 'segmentation_tab',
    fluidRow(
      column(
        width = 12, 
        box(
          title = 'Load Data',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          uiOutput(ns('data_ui')),
          actionButton(ns('load_btn'), label='Load')
        ),
        box(
          title = 'Segmentation Parameters',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          uiOutput(ns('mdl_ui')),
          actionButton(ns('import_mdl_btn'), label='Import Model Variables'),
          p(),
          uiOutput(ns('vars1_ui')),
          uiOutput(ns('vars2_ui')),
          uiOutput(ns('field_labels_ui')),
          p(),
          actionButton(ns('add_field_btn'), label='Add Field'),
          p(),
          uiOutput(ns('compare_checkbox_ui')),
          p(),
          fluidRow(
            column(width=3, uiOutput(ns('compare_name_ui'))),
            column(width=3, uiOutput(ns('compare_col_1_ui'))),
            column(width=3, uiOutput(ns('compare_col_2_ui'))),
            column(width=3, uiOutput(ns('threshold_ui')))
          ),
          p(),
          actionButton(ns('search_btn'), label='Search Segments')
        ),
        box(
          title = 'Tables',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('table_outputs')),
          p(),
          textInput(ns('file_name_input'), label='Save to Excel File'),
          actionButton(ns('save_btn'), label='Save')
        )
      )
    )
  )
}

# SEGMENTATION TAB SERVER
segmentation_tab_server <- function(input, output, session){
  
  # NAMESPACE TAG
  ns <- session$ns
  nsp <- function(...) {
    args <- list(...)
    ns(do.call(paste0, args))
  }
  
  # DATASET TO SEARCH
  search_data <- data.table()
  
  # REACTIVE VALUE TO STORED LIST OF SEGMENT SEARCH RESULTS
  table_results <- reactiveVal(list())
  
  # REACTIVE VALUES TO OBSERVE DATA CHANGES
  data_update <- reactiveVal(0)
  trigger_data_update <- function() data_update(isolate(data_update())+1)
  
  # DATA SELECTOR
  output$data_ui <- renderUI({
    datasets <- list.dirs(data_lib_path, full=F)
    datasets <- setdiff(datasets, '')
    selectInput(ns('data_selector'), choices=datasets, label='Dataset')
  })
  
  # LOAD DATA BUTTON
  observeEvent(input$load_btn, {
    req(input$data_selector)
    print('Loading data...')
    
    # GET DATA NAME AND PATH
    data_name <- input$data_selector
    data_path <- here(data_lib_path, data_name, 'data.csv')
    
    # GET META DATA
    meta <- fread(here(data_lib_path, data_name, 'meta.csv'))
    
    # CREATE NAMED VECTOR OF COLUMN TYPES AND CORRESPONDING COLUMNS
    col_types <- meta$types
    names(col_types) <- meta$columns
    
    # READ IN DATA
    search_data <<- fread(data_path, colClasses=col_types)
    
    # TRIGGER DATA UPDATE
    trigger_data_update()
    
    print('Load complete.')
  })
  
  # COLUMN NAMES
  col_names <- reactiveVal(c())
  observe({
    data_update()
    col_names(names(search_data))
  })
  
  # MODEL SELECTOR
  output$mdl_ui <- renderUI({
    mdls <- list.files(here(mdl_lib_path, 'designs'))
    label <- '(Optional) Select a model to get categorical variables'
    selectInput(ns('mdl_selector'), choices=mdls, label=label)
  })
  
  # IMPORT CATEGORICAL VARIABLES FROM MODEL
  observeEvent(input$import_mdl_btn, {
    req(input$mdl_selector)
    load(here(mdl_lib_path, 'designs', input$mdl_selector))
    vars <- design$ref_lvl_tbl$vars
    updateSelectInput(session, 'vars1_selector', selected=vars)
  })
  
  # VARS (GROUP 1) SELECTOR
  output$vars1_ui <- renderUI({
    selectInput(ns('vars1_selector'), choices=col_names(), label='Variable Group 1', mult=T)
  })
  
  # VARS (GROUP 2) SELECTOR
  output$vars2_ui <- renderUI({
    selectInput(ns('vars2_selector'), choices=col_names(), label='Variable Group 2', mult=T)
  })

  # FIELD LABELS
  output$field_labels_ui <- renderUI({
    req(length(field_ids())>0)
    fluidRow(
      column(width=1, strong('')),
      column(width=1, strong('Format')),
      column(width=2, strong('Name')),
      column(width=1, strong('Calculation')),
      column(width=2, strong('Value A')),
      column(width=2, strong('Value B')),
      column(width=1, strong('Criteria')),
      column(width=1, strong('Criteria Value'))
    )
  })
  
  # CALCULATED FIELDS SELECTOR
  # REACTIVE VALUE TO STORE NUMBER OF CALCULATED FIELDS
  field_ids <- reactiveVal(c())
  newest_field_id <- reactiveVal(as.integer(Sys.time()))
  
  # ADD CALCULATED FIELD BUTTON
  observeEvent(input$add_field_btn, {
    new_id <- newest_field_id() + 1
    newest_field_id(new_id)
    field_ids(c(field_ids(), new_id))
    
    # NAME FIELD
    name_ui_id <- paste0('name_ui_', new_id)
    output[[name_ui_id]] <- renderUI({
      textInput(nsp('name_input_', new_id), label=NULL)
    })
    
    # CREATE FORMAT SELECTOR
    fmt_ui_id <- paste('fmt_ui_', new_id)
    output[[fmt_ui_id]] <- renderUI({
      choices <- c('#', '#.##', '$', '%')
      selectInput(nsp('fmt_selector_', new_id), label=NULL, choices=choices)
    })
    
    # CREATE A CALCULATION SELECTOR
    calc_ui_id <- paste0('calc_ui_', new_id)
    output[[calc_ui_id]] <- renderUI({
      choices <- c('SUM', 'SUM(A)/SUM(B)', '% OF TOTAL', 'ADJ PP')
      selectInput(nsp('calc_selector_', new_id), label=NULL, choices=choices)
    })
    
    # CREATE VALUE SELECTOR(S)
    val_a_ui_id <- paste0('val_a_ui_', new_id)
    output[[val_a_ui_id]] <- renderUI({
      selectInput(nsp('val_a_selector_', new_id), choices=col_names(), label=NULL)
    })
    
    val_b_ui_id <- paste0('val_b_ui_', new_id)
    output[[val_b_ui_id]] <- renderUI({
      req(input[[paste0('calc_selector_', new_id)]]%in%c('SUM(A)/SUM(B)', 'ADJ PP'))
      selectInput(nsp('val_b_selector_', new_id), choices=col_names(), label=NULL)
    })
    
    # CREATE CRITERIA SELECTOR
    crit_ui_id <- paste0('crit_ui_', new_id)
    output[[crit_ui_id]] <- renderUI({
      choices <- c('NONE', '>', '>=', '<', '<=')
      selectInput(nsp('crit_selector_', new_id), choices=choices, label=NULL)
    })
    
    # CREATE CRITERIA THRESHOLD SELECTOR
    crit_val_ui_id <- paste0('crit_val_ui_', new_id)
    output[[crit_val_ui_id]] <- renderUI({
      req(input[[paste0('crit_selector_', new_id)]]!='NONE')
      numericInput(nsp('crit_val_input_', new_id), value=0, label=NULL)
    })
    
    # CREATE REMOVE BUTTON
    rem_btn_ui_id <- paste0('rem_btn_ui_', new_id)
    output[[rem_btn_ui_id]] <- renderUI({
      actionButton(ns(paste0('rem_btn_', new_id)), label='Remove')
    })
    
    # ASSEMBLE UI
    ui <- fluidRow(
      id = ns(paste0('field_row_', new_id)),
      column(width=1, uiOutput(ns(rem_btn_ui_id))),
      column(width=1, uiOutput(ns(fmt_ui_id))),
      column(width=2, uiOutput(ns(name_ui_id))),
      column(width=1, uiOutput(ns(calc_ui_id))),
      column(width=2, uiOutput(ns(val_a_ui_id))),
      column(width=2, uiOutput(ns(val_b_ui_id))),
      column(width=1, uiOutput(ns(crit_ui_id))),
      column(width=1, uiOutput(ns(crit_val_ui_id)))
    )
    
    # INSERT UI
    insertUI(paste0('#', ns('add_field_btn')), where='beforeBegin', ui=ui)
  })
  
  # REMOVE FIELD BUTTON ACTION
  observe({
    lapply(field_ids(), function(x){
      rem_btn_name <- paste0('rem_btn_', x)
      if (!is.null(input[[rem_btn_name]])) {
        if (input[[rem_btn_name]] > 0) {
          ui_name <- paste0('#', ns(paste0('field_row_', x)))
          removeUI(ui_name)
          field_ids(setdiff(field_ids(), x))
        }
      }
    })
  })
  
  # COMPARISION CHECKBOX
  output$compare_checkbox_ui <- renderUI({
    req(input$vars1_selector)
    checkboxInput(ns('compare_checkbox'), label='Filter on column differences?')
  })
  
  # COMPARISON COLUMN NAME
  output$compare_name_ui <- renderUI({
    req(input$compare_checkbox)
    textInput(ns('compare_name_input'), label='New Field Name')
  })
  
  # COMPARE COLUMN 1 SELECTOR
  output$compare_col_1_ui <- renderUI({
    req(input$compare_checkbox)
    choices <- sapply(field_ids(), function(x) input[[paste0('name_input_', x)]])
    if (class(choices)=='character') {
      selectInput(ns('compare_col_1_selector'), choices=choices, label='Field 1')
    }
  })
  
  # COMPARE COLUMN 1 SELECTOR
  output$compare_col_2_ui <- renderUI({
    req(input$compare_checkbox)
    choices <- sapply(field_ids(), function(x) input[[paste0('name_input_', x)]])
    if (class(choices)=='character') {
      selectInput(ns('compare_col_2_selector'), choices=choices, label='Field 2')
    }
  })
  
  # THRESHOLD INPUT
  output$threshold_ui <- renderUI({
    req(input$compare_checkbox)
    numericInput(ns('threshold_input'), label='Abs. % Difference Threshold', value=0)
  })
  
  # SEARCH BUTTON
  observeEvent(input$search_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('search_btn')
    on.exit(shinyjs::enable('search_btn'))
    
    # REQUIRED INPUTS
    req(
      input$vars1_selector,
      nrow(search_data)>0,
      length(field_ids())>0
    )
    
    # GET SEGMENTATION VARIABLES
    vars1 <- input$vars1_selector
    vars2 <- input$vars2_selector
    
    # GET PAIRS
    if (length(vars2)==0) {
      all_pairs <- as.data.table(t(combn(vars1, 2)))
    } else {
      all_pairs <- CJ(vars1, vars2, unique=TRUE)
    }
    n_pairs <- nrow(all_pairs)
    
    # LIST TO STORE RESULTS
    rhots <- list()
    
    # GET TABULATION OF DATA FOR EACH PAIR
    for (i in 1:n_pairs) {
      # GET PAIR
      pair <- all_pairs[i, 1:2]
      pair <- c(pair[[1]], pair[[2]])
      
      # FOR EACH SPECIFIED CALCULATED FIELD
      results <- lapply(field_ids(), function(x){
        # GET THE DESIRED NAME OF THE FIELD, 
        # THE DESIRED CALCULATION, 
        # AND THE VALUES TO CALCULATE ON
        field <- input[[paste0('name_input_', x)]]
        calc <- input[[paste0('calc_selector_', x)]]
        val_a <- input[[paste0('val_a_selector_', x)]]
        val_b <- input[[paste0('val_b_selector_', x)]]
        
        # CREATE CALCULATED FIELD WITH COLUMN NAME 'field'
        if (calc=='SUM') {
          agg <- search_data[, .(field=sum(get(val_a))), by=c(pair)]
        } else if (calc=='SUM(A)/SUM(B)') {
          agg <- search_data[, .(field=sum(get(val_a))/sum(get(val_b))), by=c(pair)]
        } else if (calc=='ADJ PP') {
          total_adj_pp <- search_data[, sum(get(val_a))/sum(get(val_b))]
          agg <- search_data[, .(field=sum(get(val_a))/sum(get(val_b))/total_adj_pp), by=c(pair)]
        } else if (calc=='% OF TOTAL') {
          total <- search_data[, sum(get(val_a))]
          agg <- search_data[, .(field=sum(get(val_a))/total), by=c(pair)]
        }
        
        # RENAME 'field' TO DESIRED FIELD NAME
        setnames(agg, 'field', field)
        
        # RETURN AGGREGATION
        return(agg)
      })
      
      # MERGE RESULTS INTO ONE TABLE
      tbl <- Reduce(merge, results)
      
      # CALCULATE DIFFERENCE COLUMNS
      if (input$compare_checkbox) {
        req(
          input$compare_name_input,
          input$compare_col_1_selector,
          input$compare_col_2_selector,
          input$threshold_input
        )
        
        # GET NAMES OF COLUMNS TO COMPARE
        compare_name <- input$compare_name_input
        col_1 <- input$compare_col_1_selector
        col_2 <- input$compare_col_2_selector
        
        # GET THRESHOLD VALUE
        threshold <- abs(input$threshold_input)
        
        # CREATE % DIFFERENCE COLUMN
        tbl[, c(compare_name):=get(col_1)/get(col_2)-1]
        tbl[, c(compare_name):=round(get(compare_name), 4)]
        
        # FILTER
        inds <- (tbl[[compare_name]] >= threshold | tbl[[compare_name]] <= -threshold)
        tbl <- tbl[inds]
      }
      
      # CREATE HIGHLIGHTING RENDERER USING USER CRITERIA
      # JAVASCRIPT FUNCTION WHICH WE WILL INSERT IF-STATEMENTS IN FOR HIGHLIGHTING
      renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          #renderer
        }
      "
      
      # LOOP THROUGH USER-CREATED CRITERIA FIELDS, CREATING A HIGHLIGHTER FOR EACH
      for (id in field_ids()) {
        # GET THE NAME OF THE FIELD
        name <- input[[paste0('name_input_', id)]]
        # GET THE CRITERIA OPERATOR
        crit <- input[[paste0('crit_selector_', id)]]
        # GET THE CRITERIA THRESHOLD
        val <- input[[paste0('crit_val_input_', id)]]
        # CREATE HIGHLIGHTER IF CRITERIA IS MADE
        if (crit != 'NONE') {
          # HIGHLIGHTING (JS) LOGIC
          highlighter <- "
            if (col == #diff_index) {
              if (value #comparison #threshold) {
                td.style.background = 'pink';
              }
            }
            #renderer
          "
          # GET THE INDEX OF THE CRITERIA COLUMN
          diff_index <- which(name==names(tbl)) - 1
          # MODIFY HIGHLIGHTER ACCORDING TO USER INPUTS
          highlighter <- str_replace_all(highlighter, '#diff_index', as.character(diff_index))
          highlighter <- str_replace_all(highlighter, '#comparison', crit)
          highlighter <- str_replace_all(highlighter, '#threshold', as.character(val))
          # INSERT HIGHLIGHTER INTO TEMPLATE
          renderer <- str_replace_all(renderer, '#renderer', highlighter)
        }
      }
      
      # DELETE LAST EMPTY {{renderer}}
      renderer <- str_replace_all(renderer, '#renderer', '')
      
      if (nrow(tbl)>0) {
        # CONVERT TO RHANDSONTABLE
        rhot <- rhandsontable(tbl)
        
        # FORMAT FOR EACH SPECIFIED FIELD
        for (id in field_ids()) {
          # GET THE NAME OF THE FIELD
          name <- input[[paste0('name_input_', id)]]
          # GET THE DESIRED FORMAT
          fmt <- input[[paste0('fmt_selector_', id)]]
          # CONVERT SELECTED OPTION TO RHANDSONTABLE FORMAT SPECIFICATION
          fmt <- switch(fmt, '#' = '2a', '#.##' = '0.00', '$' = '$2a', '%' = '0.00%')
          # FORMAT WITH THE SPECIFIED FORMAT
          rhot <- hot_col(rhot, col=name, format=fmt)
        }
        
        # FORMAT DIFFERENCE COLUMN
        if (input$compare_checkbox) {
          rhot <- hot_col(rhot, col=compare_name, format='0.00%')
        }
        
        # ADD RENDERER TO RHANDSONTABLE DISPLAY
        rhot <- hot_cols(rhot, renderer = renderer)
        
        # STORE RESULT
        tbl_name <- paste0(pair[1], '_x_', pair[2])
        rhots[[tbl_name]] <- rhot
      }
    }
    
    # STORE LIST OF TABLES
    table_results(rhots)
  })
  
  # PIVOT TABLE DISPLAY
  output$table_output <- renderRHandsontable({
    table_results()
  })
  
  # GENERATE TABLE OUTPUTS
  observe({
    tables <- table_results()
    tbl_names <- names(tables)
    lapply(tbl_names, function(x){
      tbl_name <- paste0(x, '_tbl_output')
      output[[tbl_name]] <- renderRHandsontable({
        tables[[x]]
      })
    })
  })
  
  # DYNAMIC TABSET PANEL
  output$table_outputs <- renderUI({
    tbl_names <- names(table_results())
    tabs <- lapply(tbl_names, function(x) {
      tbl_name <- nsp(x, '_tbl_output')
      tabPanel(
        x,
        p(),
        rHandsontableOutput(tbl_name)
      )
    })
    do.call(tabsetPanel, tabs)
  })
  
  # SAVE TABLES BUTTON
  observeEvent(input$save_btn, {
    req(input$file_name_input)
    file_path <- here('bin', paste0(input$file_name_input, '.xlsx'))
    tables <- lapply(table_results(), function(x){
      dt <- jsonlite::fromJSON(x$x$data)
      dt <- as.data.table(dt)
      dt
    })
    names(tables) <- names(table_results())
    write.xlsx(tables, file=file_path)
    plog('Tables saved to: ', file_path)
  })
}
