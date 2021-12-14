
# DATA TAB UI
data_tab_ui <- function(id) {
  
  # NAMESPACE TAG
  ns <- NS(id)
  
  # UI
  tabItem(
    shinyjs::useShinyjs(),
    tabName = 'data_tab',
    fluidRow(
      column(
        width = 6,
        ### IMPORT DATA ###
        box(
          title = 'Import Data',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          shinyFilesButton(ns('import_ext_data_btn'), label='Import External Data', title='Choose a dataset:', multiple=F),
          p(),
          p(em('--- or ---')),
          uiOutput(ns('import_data_ui')),
          actionButton(ns('import_data_btn'), 'Import from Data Library'),
          actionButton(ns('peek_data_btn'), 'Peek')
        ),
        
        ### BUILD DATA ###
        box(
          title = 'Build Data',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('base_ui')),
          dateRangeInput(ns('dates_input'), label='Policy Effective Dates'),
          uiOutput(ns('build_vars_ui')),
          # REQUIRED USER ACTION
          HTML(paste("<b>", as.character(tags$span(style="color:black", "Required user actions:")), "</b>")),
          p(),
          uiOutput(ns('varMessages')),
          p(),
          # USER& PSWD
          splitLayout(
            textInput(ns('username_input'), label='Username'),
            passwordInput(ns('password_input'), label='Password')
          ),
          checkboxInput(ns('build_deps_checkbox'), label='Build dependencies', value=TRUE),
          actionButton(ns('build_btn'), label='Build New Dataset'),
          actionButton(ns('add_data_btn'), label='Add To Dataset'),
          uiOutput(ns('recommendation_ui'))
        ),
        
        ### FILTER DATA ###
        box(
          title = 'Filter Data',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('filter_ui')),
          actionButton(ns('add_filter_btn'), label='Add Filter'),
          actionButton(ns('apply_filters_btn'), label='Apply Filters')
        ),
        
        ### SAVE DATA ###
        box(
          title = 'Save Data',
          status = color, 
          solidHeader = TRUE,
          width = NULL,
          textInput(ns('data_name_input'), label='Dataset Name', placeholder='dataset_name'),
          uiOutput(ns('save_vars_ui')),
          actionButton(ns('save_btn'), label='Save Dataset'),
          actionButton(ns('clear_btn'), label='Clear Data')
        )
      ),
      
      column(
        width = 6,
        ### INSPECT DATA ###
        box(
          title = 'Inspect Data',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('inspect_var_ui')),
          actionButton(ns('inspect_var_table_btn'), label='Create Table'),
          actionButton(ns('inspect_var_chart_btn'), label='Create Chart'),
          p(),
          tabsetPanel(
            id = ns('inspect_var_tabset_panel'),
            tabPanel('Table', p(), dataTableOutput(ns('inspect_var_table'))),
            tabPanel('Chart', plotlyOutput(ns('inspect_var_chart')))
          )
        ),
        
        ### CONVERT DATA ###
        box(
          title = 'Convert Data Types',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('convert_vars_ui')),
          uiOutput(ns('convert_type_ui')),
          actionButton(ns('convert_btn'), label='Convert')
        ),
        
        ### META DATA ###
        box(
          title = 'Meta Data',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          DTOutput(ns('meta_table_preview_output'))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        ### DATA TABLE PREVIEW ###
        box(
          title = 'Data Preview',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          dataTableOutput(ns('table_preview_output'))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        ### VARIABLE CODE VIEWER ###
        box(
          title = 'Code Viewer',
          status = color,
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns('code_var_ui')),
          uiOutput(ns('code_view_ui'))
        )
      )
    )
  )
}

# DATA TAB SERVER
data_tab_server <- function(input, output, session) {
  
  # NAMESPACE TAG
  ns <- session$ns
  
  # REACTIVE VALUES TO STORE DATASET AND META DATA
  base_data <- data.table()
  meta_data <- data.table()
  
  # REACTIVE VALUES TO OBSERVE DATA CHANGES
  base_update <- reactiveVal(0)
  meta_update <- reactiveVal(0)
  trigger_base_update <- function() base_update(isolate(base_update())+1)
  trigger_meta_update <- function() meta_update(isolate(meta_update())+1)
  
  # BASE SELECTOR
  output$base_ui <- renderUI({
    base_covs <- list.dirs(base_lib_path, full=F, recursive=F)
    base_covs <- setdiff(base_covs, '')
    selectInput(ns('build_base_selector'), choices=base_covs, label='Coverage')
  })
  
  # VARIABLE SELECTOR
  output$build_vars_ui <- renderUI({
    vars <- list.dirs(var_lib_path, full=F, recursive=F)
    vars <- setdiff(vars, '')
    selectInput(ns('build_vars_input'), choices=vars, label='Variable Selection', mult=T, selectize=T)
  })
  
  # REQUIRED USER ACTION
  output$varMessages <- renderUI({
    # order the variable list so that list of messages is 'grouped' by variable
    # x = unlist(input$build_vars_input, recursive = TRUE)
    # if (length(x) > 0) {x = x[order(x)]}
    # x = as.list(x)
    
    varMessageList = lapply(input$build_vars_input, createVarMessage)
    if(length(varMessageList)==0) {varMessageList = list("Given the list of variables currently selected, there are NO actions required from the user.", "")}
    varMessageList = list(varMessageList, "")
    
    # flatten
    varMessageList = unlist(varMessageList)

    colorlessMsg = HTML(paste(varMessageList,
               sep = '<br/>',
               collapse = '<br/>'))
    HTML(as.character(tags$span(style="color:red", colorlessMsg)))
  })   
  
  # BUILD DATA BUTTON
  observeEvent(input$build_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('build_btn')
    on.exit(shinyjs::enable('build_btn'))
    
    # REQUIRED INPUTS
    req(
      input$build_base_selector,
      input$dates_input,
      input$username_input,
      input$password_input,
      input$build_vars_input
    )
    
    # CHECK CREDENTIALS BY TESTING DATA SOURCE CONNECTION
    req(check_db2p(input$username_input, input$password_input))
    
    # COLLECT INPUTS
    args <- list(start_date=as.character(input$dates_input[1]),
                 end_date=as.character(input$dates_input[2]),
                 uid=input$username_input,
                 pwd=input$password_input)
    sel_vars <- input$build_vars_input
    
    # SELECTED COVERAGE
    base_cov <- input$build_base_selector
    
    # INITIATE DATA BUILDER
    builder <- data_builder()
    
    # LOAD VARIABLE LIBRARY
    base_def <- builder$load_var_lib(base_lib_path, base_cov)
    var_defs <- builder$load_var_lib(var_lib_path, sel_vars)
    builder$var_src <- c(base_def, var_defs)
    
    # BUILD BASE DATASET
    base_data <<- builder$build_vars(base_cov, args, input$build_deps_checkbox)
    args[['base_data']] <- base_data
    
    # BUILD SELECTED VARIABLES
    base_data <<- builder$build_vars(sel_vars, args, input$build_deps_checkbox)
    
    # TRIGGER BASE UPDATE
    trigger_base_update()
    print('Build complete.')
  })
  
  # ADD TO DATASET BUTTON
  observeEvent(input$add_data_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('add_data_btn')
    on.exit(shinyjs::enable('add_data_btn'))
    
    # REQUIRED INPUTS
    req(
      nrow(base_data) > 0,
      input$dates_input,
      input$username_input,
      input$password_input,
      input$build_vars_input
    )
    
    # CHECK CREDENTIALS BY TESTING DATA SOURCE CONNECTION
    req(check_db2p(input$username_input, input$password_input))
    
    # COLLECT INPUTS
    args <- list(start_date=isolate(as.character(input$dates_input[1])),
                 end_date=isolate(as.character(input$dates_input[2])),
                 uid=isolate(input$username_input),
                 pwd=isolate(input$password_input),
                 base_data=base_data)
    sel_vars <- isolate(input$build_vars_input)
    
    # INITIATE DATA BUILDER
    builder <- data_builder()
    
    # LOAD VARIABLE LIBRARY
    builder$var_src <- builder$load_var_lib(var_lib_path, sel_vars)
    
    # BUILD SELECTED VARIABLES
    base_data <<- builder$build_vars(sel_vars, args, input$build_deps_checkbox)
    
    # GIVE RECOMMENDATIONS FOR UPDATES
    recommendations <- builder$recommend(var_lib_path, base_data)
    recommended_updates(recommendations)
    
    # TRIGGER BASE UPDATE
    trigger_base_update()
    print('Additions complete.')
  })
  
  # RECOMMENDATION UI
  recommended_updates <- reactiveVal()
  output$recommendation_ui <- renderUI({
    req(length(recommended_updates())>0)
    selector <- selectInput(
      ns('recommendation_selector'), 
      label = 'Recommended Updates', 
      choices = recommended_updates(), 
      selected = recommended_updates(),
      mult = T
    )
    fluidRow(
      column(
        width=12,
        p(),
        selector,
        actionButton(ns('move_recommendations_btn'), label='Move to Variable Selection')
      )
    )
  })
  
  # MOVE RECOMMENDATIONS TO VARIABLE SELECTION BUTTON
  observeEvent(input$move_recommendations_btn, {
    recommendations <- input$recommendation_selector
    updateSelectInput(session, 'build_vars_input', selected=recommendations)
    recommended_updates(c())
  })
  
  # REACTIVE VALUE TO STORE NUMBER OF FILTERS
  filter_ids <- reactiveVal(c())
  newest_filter_id <- reactiveVal(as.integer(Sys.time()))
  
  # ADD FILTER BUTTON
  observeEvent(input$add_filter_btn, {
    new_id <- newest_filter_id() + 1
    newest_filter_id(new_id)
    filter_ids(c(filter_ids(), new_id))
    
    # CREATE A VARIABLE SELECTOR
    selector_ui_id <- paste0('filter_var_ui_', new_id)
    output[[selector_ui_id]] <- renderUI({
      vars <- meta_data[types %in% c('factor', 'numeric', 'integer', 'integer64'), columns]
      selectInput(ns(paste0('filter_var_selector_', new_id)), label=NULL, choices=vars)
    })
    
    # CREATE A COMPARISON SELECTOR
    condition_ui_id <- paste0('filter_condition_ui_', new_id)
    output[[condition_ui_id]] <- renderUI({
      req(input[[paste0('filter_var_selector_', new_id)]])
      var <- input[[paste0('filter_var_selector_', new_id)]]
      type <- meta_data[columns==var, types][1]
      if (type=='factor') {
        choices <- c('include', 'exclude')
      } else {
        choices <- c('=', '!=', '<', '<=', '>', '>=')
      }
      selectInput(ns(paste0('filter_condition_selector_', new_id)), label=NULL, choices=choices)
    })
    
    # CREATE A VALUE SELECTOR
    value_ui_id <- paste0('filter_value_ui_', new_id)
    output[[value_ui_id]] <- renderUI({
      req(input[[paste0('filter_var_selector_', new_id)]])
      var <- input[[paste0('filter_var_selector_', new_id)]]
      type <- meta_data[columns==var, types][1]
      if (type=='factor') {
        choices <- base_data[, unique(get(var))]
        selectInput(ns(paste0('filter_value_input_', new_id)), label=NULL, choices=choices)
      } else {
        numericInput(ns(paste0('filter_value_input_', new_id)), label=NULL, value=0)
      }
    })
    
    # CREATE REMOVE BUTTON
    btn_ui_id <- paste0('filter_remove_ui_', new_id)
    output[[btn_ui_id]] <- renderUI({
      actionButton(ns(paste0('filter_remove_btn_', new_id)), label='Remove')
    })
    
    # ASSEMBLE UI
    ui <- fluidRow(
      id = ns(paste0('filter_row_', new_id)),
      column(width=3, uiOutput(ns(selector_ui_id))),
      column(width=3, uiOutput(ns(condition_ui_id))),
      column(width=3, uiOutput(ns(value_ui_id))),
      column(width=3, uiOutput(ns(btn_ui_id)))
    )
    
    # INSERT UI
    insertUI(paste0('#', ns('add_filter_btn')), where='beforeBegin', ui=ui)
  })
  
  # REMOVE FILTER BUTTON
  observe({
    lapply(filter_ids(), function(x){
      rem_btn_name <- paste0('filter_remove_btn_', x)
      if (!is.null(input[[rem_btn_name]])) {
        if (input[[rem_btn_name]] > 0) {
          ui_name <- paste0('#', ns(paste0('filter_row_', x)))
          removeUI(ui_name)
          filter_ids(setdiff(filter_ids(), x))
        }
      }
    })
  })
  
  # APPLY FILTERS BUTTON
  observeEvent(input$apply_filters_btn, {
    for (id in filter_ids()) {
      req(
        input[[paste0('filter_var_selector_', id)]],
        input[[paste0('filter_condition_selector_', id)]],
        input[[paste0('filter_value_input_', id)]]
      )
    }
    print('Applying filter...')
    for (id in filter_ids()) {
      variable <- input[[paste0('filter_var_selector_', id)]]
      conditional <- input[[paste0('filter_condition_selector_', id)]]
      value <- input[[paste0('filter_value_input_', id)]]
      base_data <<- filter_data(base_data, variable, conditional, value)
    }
    trigger_base_update()
    print('Filters applied.')
  })
  
  # IMPORT EXTERNAL DATA BUTTON
  volumes <- getVolumes()
  observe({
    shinyFileChoose(input, 'import_ext_data_btn', roots = volumes, session = session)
    file_path <- parseFilePaths(volumes, input$import_ext_data_btn)
    file_path <- as.character(file_path$datapath)
    
    if (length(file_path)!=0) {
      # READ IN DATA
      print('Importing data...')
      base_data <<- fread(file_path)
      trigger_base_update()
	  print(length(base_data$ST_CD)) 
      print('Import complete.')
    }
  })
  
  # IMPORT FROM DATA LIBRARY SELECTOR
  output$import_data_ui <- renderUI({
    datasets <- list.dirs(data_lib_path, full=F)
    datasets <- setdiff(datasets, '')
    selectInput(ns('import_data_selector'), choices=datasets, label='Dataset')
  })
  
  # IMPORT FROM DATA LIBRARY BUTTON
  observeEvent(input$import_data_btn, {
    req(input$import_data_selector)
    print('Importing data...')
    # GET DATA NAME AND PATH
    data_name <- input$import_data_selector
    data_path <- here(data_lib_path, data_name, 'data.csv')
    
    # GET META DATA
    meta <- fread(here(data_lib_path, data_name, 'meta.csv'))
    
    # CREATE LIST OF COLUMN TYPES AND CORRESPONDING COLUMNS
    col_types <- lapply(unique(meta$types), function(x){
      meta[types==x, columns]
    })
    names(col_types) <- unique(meta$types)
    
    # READ IN DATA
    new_data <- fread(data_path, colClasses=col_types)
    
    # UPDATE DATE RANGE SELECTOR
    if ('POL_EFF_DT' %in% names(new_data)) {
      updateDateRangeInput(
        session, 
        'dates_input', 
        start = min(new_data$POL_EFF_DT),
        end = max(new_data$POL_EFF_DT)
      )
    }
    
    # READ IN VARIABLE SELECTIONS
    tryCatch({
      load(here(data_lib_path, data_name, 'variable_selections'))
      updateSelectInput(session, 'build_vars_input', selected=var_sels)
    }, warning = function(w) {}, error = function(e) {})
    
    # UPDATE BASE DATA
    base_data <<- new_data
    trigger_base_update()
    print('Import complete.')
  }, ignoreInit = TRUE)
  
  # PEEK BUTTON
  observeEvent(input$peek_data_btn, {
    req(input$import_data_selector)
    print('Importing data...')
    # GET DATA NAME AND PATH
    data_name <- input$import_data_selector
    data_path <- here(data_lib_path, data_name, 'data.csv')
    
    # GET META DATA
    meta <- fread(here(data_lib_path, data_name, 'meta.csv'))
    
    # CREATE LIST OF COLUMN TYPES AND CORRESPONDING COLUMNS
    col_types <- lapply(unique(meta$types), function(x){
      meta[types==x, columns]
    })
    names(col_types) <- unique(meta$types)
    
    # READ IN DATA
    new_data <- fread(data_path, colClasses=col_types, nrows=1000)
    
    # UPDATE DATE RANGE SELECTOR
    if ('POL_EFF_DT' %in% names(new_data)) {
      updateDateRangeInput(
        session, 
        'dates_input', 
        start = min(new_data$POL_EFF_DT),
        end = max(new_data$POL_EFF_DT)
      )
    }
    
    # READ IN VARIABLE SELECTIONS
    tryCatch({
      load(here(data_lib_path, data_name, 'variable_selections'))
      updateSelectInput(session, 'build_vars_input', selected=var_sels)
    }, warning = function(w) {}, error = function(e) {})
    
    # UPDATE BASE DATA
    base_data <<- new_data
    trigger_base_update()
    print('Import complete.')
  })
  
  # INSPECT VARIABLE SELECTOR
  output$inspect_var_ui <- renderUI({
    meta_update()
    selectInput(ns('inspect_var_selector'), choices=meta_data$columns, label='Inspect Variable')
  })
  
  # INSPECT VARIABLE CHART
  observeEvent(input$inspect_var_chart_btn, {
    updateTabsetPanel(session, 'inspect_var_tabset_panel', selected = 'Chart')
    output$inspect_var_chart <- renderPlotly({
      var <- isolate(input$inspect_var_selector)
      plt <- NULL
      if (nrow(meta_data)>0) {
        var_type <- meta_data[columns==var, types]
        if (var_type=='factor') {
          print('Building chart...')
          chart_data <- base_data[, .(N=.N), by=c(var)]
          plt <- ggplot(chart_data, aes_string(x = var, y = 'N')) + geom_bar(stat='identity', fill=box_color)
        } else if (var_type %in% c('numeric', 'integer', 'integer64')) {
          print('Building chart...')
          chart_data <- base_data[, c(var), with=F]
          plt <- ggplot(chart_data, aes_string(x = var)) + geom_histogram(fill=box_color)
        } 
      }
      plt
    })
  })
  
  # INSPECT VARIABLE TABLE
  observeEvent(input$inspect_var_table_btn, {
    updateTabsetPanel(session, 'inspect_var_tabset_panel', selected = 'Table')
    output$inspect_var_table <- renderDT({
      var <- isolate(input$inspect_var_selector)
      chart_data <- data.table()
      if (nrow(meta_data)>0) {
        var_type <- meta_data[columns==var, types]
        if (var_type=='factor') {
          print('Building table...')
          chart_data <- base_data[, .(N=.N), by=c(var)]
          chart_data[, PCT:=N/sum(N)]
          setorderv(chart_data, var)
          chart_data <- datatable(chart_data) %>% formatPercentage('PCT',2)
        } else if (var_type %in% c('numeric', 'integer', 'integer64')) {
          print('Building table...')
          chart_data <- base_data[, summary(get(var))]
          cols <- names(chart_data)
          chart_data <- data.table(t(as.vector(chart_data)))
          setnames(chart_data, names(chart_data), cols)
        } 
      } 
      chart_data
    })
  })
  
  # CONVERT VARIABLES SELECTOR
  output$convert_vars_ui <- renderUI({
    meta_update()
    selectInput(ns('convert_vars_selector'), choices=meta_data$columns, label='Convert Variables', mult=T)
  })
  
  # CONVERT DATA TYPE SELECTOR
  output$convert_type_ui <- renderUI({
    selectInput(ns('convert_type_selector'), choices=c('factor', 'numeric', 'integer', 'Date', 'character'), label='Type')
  })
  
  # CONVERT DATA TYPE BUTTON
  observeEvent(input$convert_btn, {
    meta_data[columns %in% input$convert_vars_selector, types:=input$convert_type_selector]
    trigger_meta_update()
  })
  
  # DATA PREVIEW TABLE
  output$table_preview_output <- renderDT({
    base_update()
    base_data[1:1000]
  }, options = list(scrollX = TRUE))
  
  # OBSERVE CHANGES IN BASE DATA AND UPDATE META DATA
  observe({
    base_update()
    get_cardinality <- function(col, dt) {
      type <- dt[, class(get(col))]
      if (type=='factor') dt[, uniqueN(get(col))] else 0
    }
    meta_data <<- data.table(
      columns = names(base_data),
      types = sapply(base_data, class), 
      cardinality = sapply(names(base_data), get_cardinality, dt=base_data)
    )
    trigger_meta_update()
  })
  
  # META DATA PREVIEW TABLE
  output$meta_table_preview_output <- renderDT({
    meta_update()
    meta_data
  }, editable = TRUE, server = TRUE)
  
  # OBSERVE EDITS TO META DATA AND WRITE BACK TO SOURCE DATA TABLE
  observeEvent(input$meta_table_preview_output_cell_edit, {
    info <- input$meta_table_preview_output_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    meta_data[[i, j]] <<- DT::coerceValue(v, meta_data[[i, j]])
    replaceData(dataTableProxy(ns('meta_table_preview_output')), meta_data, resetPaging=F)
  })
  
  # SAVE VARIABLE SELECTIONS UI
  output$save_vars_ui <- renderUI({
    vars <- input$build_vars_input
    vars <- unique(c(input$save_vars_selector, vars))
    selectInput(ns('save_vars_selector'), choices=vars, selected=vars, label='Variable Selections', mult=T)
  })
  
  # SAVE DATA BUTTON
  observeEvent(input$save_btn, {
    # DISABLE BUTTON WHILE PROCESSING TO INDICATE BUSY STATE
    shinyjs::disable('save_btn')
    on.exit(shinyjs::enable('save_btn'))
    
    req(input$data_name_input)
    print('Saving data...')
    dir.create(here(data_lib_path, input$data_name_input), showWarnings=FALSE)
    fwrite(meta_data, here(data_lib_path, input$data_name_input, 'meta.csv'))
    fwrite(base_data, here(data_lib_path, input$data_name_input, 'data.csv'))
    var_sels <- input$save_vars_selector
    save(var_sels, file=here(data_lib_path, input$data_name_input, 'variable_selections'))
    
    # GENERATE SAS READIN CODE
    sink(paste0(here(data_lib_path, input$data_name_input) ,"/load_", input$data_name_input,".sas"))
      # FUNCTION DEFINED IN HELPERS
      wr_sas_readin(1:length(meta_data$columns), meta_data, input$data_name_input)
    sink()
    
    print('Save complete.')
    
  }, ignoreInit = TRUE)
  
  # CLEAR DATA BUTTON
  observeEvent(input$clear_btn, {
    base_data <<- data.table()
    meta_data <<- data.table()
    gc()
    trigger_base_update()
    trigger_meta_update()
  }, ignoreInit = TRUE)
  
  # VARIABLE CODE SELECTOR
  output$code_var_ui <- renderUI({
    vars <- list.dirs(var_lib_path, full=F, recursive=F)
    vars <- setdiff(vars, '')
    selectInput(ns('code_var_selector'), choices=vars, label='Variable Selection')
  })
  
  # VARIABLE CODE VIEWER
  # CREATE TEMPORARY MARKDOWN FILE PATH
  md_file <- paste0(tempfile(), '.md')
  output$code_view_ui <- renderUI({
    req(input$code_var_selector)
    # GET PATH TO VARIABLE SOURCE FILE
    file_path <- here(var_lib_path, input$code_var_selector, 'source.R')
    # READ CODE IN AS ONE LONG STRING
    raw_text <- readChar(file_path, file.info(file_path)$size)
    # PLACE R MARKDOWN TAGS AROUND TEXT
    md_text <- paste0('```{r}\n', raw_text, '\n```')
    # WRITE R MARKDOWN TO TEMPORARY FILE
    writeLines(md_text, md_file)
    # CREATE UI DISPLAY OF MARKDOWN FILE
    includeMarkdown(md_file)
  })
}