
# PIVOT TAB UI
pivot_tab_ui <- function(id){
  
  # NAMESPACE TAG
  ns <- NS(id)

  # UI
  tabItem(
    shinyjs::useShinyjs(),
    tabName = 'pivot_tab',
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
          title = 'Pivot Parameters',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          fluidRow(
            column(width=6, uiOutput(ns('rows_ui'))),
            column(width=6, uiOutput(ns('cols_ui')))
          ),
          uiOutput(ns('field_labels_ui')),
          p(),
          actionButton(ns('add_field_btn'), label='Add Field'),
          p(),
          uiOutput(ns('compare_checkbox_ui')),
          fluidRow(
            column(width=3, uiOutput(ns('compare_col_1_ui'))),
            column(width=3, uiOutput(ns('compare_col_2_ui'))),
            column(width=3, uiOutput(ns('threshold_ui')))
          ),
          p(),
          actionButton(ns('pivot_btn'), label='Pivot')
        ),
        box(
          title = 'Pivot Table',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          rHandsontableOutput(ns('pivot_table_output'))
        )
      )
    )
  )
}

# PIVOT TAB SERVER
pivot_tab_server <- function(input, output, session){
  
  # NAMESPACE TAG
  ns <- session$ns
  nsp <- function(...) {
    args <- list(...)
    ns(do.call(paste0, args))
  }
  
  # PIVOT DATA
  pivot_data <- data.table()
  
  # REACTIVE VALUE TO STORED PIVOT TABLE
  pivot_table <- reactiveVal(data.table())
  
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
    pivot_data <<- fread(data_path, colClasses=col_types)
    
    # TRIGGER DATA UPDATE
    trigger_data_update()
    
    print('Load complete.')
  })
  
  # COLUMN NAMES
  col_names <- reactiveVal(c())
  observe({
    data_update()
    col_names(names(pivot_data))
  })
  
  # ROWS SELECTOR
  output$rows_ui <- renderUI({
    selectInput(ns('rows_selector'), choices=col_names(), label='Rows', mult=T)
  })
  
  # COLUMNS SELECTOR
  output$cols_ui <- renderUI({
    selectInput(ns('cols_selector'), choices=col_names(), label='Columns', mult=T)
  })
  
  # FIELD LABELS
  output$field_labels_ui <- renderUI({
    req(length(field_ids())>0)
    fluidRow(
      column(width=1, strong('')),
      column(width=2, strong('Format')),
      column(width=2, strong('Name')),
      column(width=2, strong('Calculation')),
      column(width=2, strong('Value A')),
      column(width=2, strong('Value B'))
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
      req(input[[paste0('calc_selector_', new_id)]] %in% c('SUM(A)/SUM(B)', 'ADJ PP'))
      selectInput(nsp('val_b_selector_', new_id), choices=col_names(), label=NULL)
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
      column(width=2, uiOutput(ns(fmt_ui_id))),
      column(width=2, uiOutput(ns(name_ui_id))),
      column(width=2, uiOutput(ns(calc_ui_id))),
      column(width=2, uiOutput(ns(val_a_ui_id))),
      column(width=2, uiOutput(ns(val_b_ui_id)))
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
    req(
      input$rows_selector,
      length(input$col_selector)==0
    )
    checkboxInput(ns('compare_checkbox'), label='Compare and highlight differences?')
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
    numericInput(ns('threshold_input'), label='% Difference Threshold', value=0)
  })
  
  # PIVOT BUTTON
  observeEvent(input$pivot_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('pivot_btn')
    on.exit(shinyjs::enable('pivot_btn'))
    
    # REQUIRED INPUTS
    req(
      input$rows_selector,
      nrow(pivot_data)>0,
      length(field_ids())>0
    )
    
    # GET (GROUP-BY) ROWS AND COLUMNS
    rows <- input$rows_selector
    cols <- input$cols_selector
    
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
        agg <- pivot_data[, .(field=sum(get(val_a))), by=c(rows, cols)]
      } else if (calc=='SUM(A)/SUM(B)') {
        agg <- pivot_data[, .(field=sum(get(val_a))/sum(get(val_b))), by=c(rows, cols)]
      } else if (calc=='ADJ PP') {
        total_adj_pp <- pivot_data[, sum(get(val_a))/sum(get(val_b))]
        agg <- pivot_data[, .(field=sum(get(val_a))/sum(get(val_b))/total_adj_pp), by=c(rows, cols)]
      } else if (calc=='% OF TOTAL') {
        total <- pivot_data[, sum(get(val_a))]
        agg <- pivot_data[, .(field=sum(get(val_a))/total), by=c(rows, cols)]
      }
      
      # RENAME 'field' TO DESIRED FIELD NAME
      setnames(agg, 'field', field)
      
      # RETURN AGGREGATION
      return(agg)
    })
    
    # MERGE RESULTS INTO ONE TABLE
    tbl <- Reduce(merge, results)
    
    # PIVOT
    if (length(cols)>0) {
      lhs <- paste(rows, collapse='+')
      rhs <- paste(cols, collapse='+')
      formula <- paste(lhs, '~', rhs)
      vals <- setdiff(names(tbl), c(rows, cols))
      tbl <- dcast(tbl, formula, value.var=vals)
    }
    
    # CALCULATE DIFFERENCE COLUMN IF CHECKED
    if (input$compare_checkbox) {
      req(
        input$compare_col_1_selector,
        input$compare_col_2_selector,
        input$threshold_input
      )
      # GET NAMES OF COLUMNS TO COMPARE
      col_1 <- input$compare_col_1_selector
      col_2 <- input$compare_col_2_selector
      
      # GET THRESHOLD VALUE
      threshold <- input$threshold_input
      
      # CREATE % DIFFERENCE COLUMN
      tbl[, DIFF:=get(col_1)/get(col_2)-1]
      tbl[, DIFF:=round(DIFF, 4)]
    }
   
    # CONVERT TO RHANDSONTABLE
    rhot <- rhandsontable(tbl)
    
    # FORMAT
    # IF ONLY ONE CALCULATED FIELD SPECIFIED
    if (length(field_ids())==1) {
      # GET THE DESIRED FORMAT
      fmt <- input[[paste0('fmt_selector_', field_ids())]]
      # CONVERT SELECTED OPTION TO RHANDSONTABLE FORMAT SPECIFICATION
      fmt <- switch(fmt, '#' = '2a', '#.##' = '0.00', '$' = '$2a', '%' = '0.00%')
      # COLUMNS TO BE FORMATTED ARE ANY THAT ARE NOT SPECIFIED IN ROWS SELECTOR
      vals <- setdiff(names(tbl), c('DIFF', rows))
      # FOR EACH VALUE COLUMN, FORMAT WITH THE SPECIFIED FORMAT
      for (val in vals) {
        rhot <- hot_col(rhot, col=val, format=fmt)
      }
    # IF THERE ARE MULTIPLE CALCULATED FIELDS SPECIFIED
    } else {
      # FOR EACH SPECIFIED FIELD
      for (id in field_ids()) {
        # GET THE NAME OF THE FIELD
        name <- input[[paste0('name_input_', id)]]
        # GET THE DESIRED FORMAT
        fmt <- input[[paste0('fmt_selector_', id)]]
        # CONVERT SELECTED OPTION TO RHANDSONTABLE FORMAT SPECIFICATION
        fmt <- switch(fmt, '#' = '2a', '#.##' = '0.00', '$' = '$2a', '%' = '0.00%')
        # FIND VALUE COLUMNS WHICH INCLUDE THE FIELD NAME
        vals <- names(tbl)[grepl(name, names(tbl))]
        # FOR EACH OF THOSE VALUE COLUMNS, FORMAT WITH THE SPECIFIED FORMAT
        for (val in vals) {
          rhot <- hot_col(rhot, col=val, format=fmt)
        }
      }
    }
    
    # FORMAT AND HIGHLIGHT DIFFERENCE COLUMN (IF OPTED)
    if (input$compare_checkbox) {
      # CREATE A CUSTOM RENDERING CALLBACK FOR RHANDSONTABLE (JAVASCRIPT)
      renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          if (col == #diff_index) {
            var upper_bound = #threshold
            var lower_bound = -#threshold
            if (value < lower_bound || value > upper_bound) {
              td.style.background = 'pink';
            } else {
              td.style.background = 'lightgreen';
            }
          }
        }
      "
      # GET COLUMN NAMES
      cols <- rhot$x$rColHeaders
      # GET THE INDEX OF THE DIFFERENCE COLUMN
      diff_index <- which('DIFF'==cols) - 1
      # CUSTOMIZE THE RENDERER FUNCTION WITH GIVEN THRESHOLD AND DIFF COL INDEX
      renderer <- str_replace_all(renderer, '#threshold', as.character(threshold))
      renderer <- str_replace_all(renderer, '#diff_index', as.character(diff_index))
      # ADD RENDERER TO RHANDSONTABLE DISPLAY
      rhot <- hot_cols(rhot, renderer = renderer)
      rhot <- hot_col(rhot, col='DIFF', format='0.00%')
    }
    
    # STORE RESULT
    pivot_table(rhot)
  })
  
  # PIVOT TABLE DISPLAY
  output$pivot_table_output <- renderRHandsontable({
    pivot_table()
  })
  
}
