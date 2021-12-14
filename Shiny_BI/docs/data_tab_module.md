#### **Overview**  
`data_tab_module.R` contains the definition for the UI and server component of the Data tab. `data_tab_ui()` is a function which returns the HTML code required to create the Data tab UI.  It receives an arbitrary `id` as an argument, which is used to make the UI components unique against other Shiny modules.  See the [Shiny modules](https://shiny.rstudio.com/articles/modules.html) documentation for more on this feature.  The `data_tab_server()` function contains the bulk of the code and forms the server component of the Data tab.  `data_tab_server()` and `data_tab_ui()` are called in `app.R` as part of the main UI and main server code.

`data_tab_server()` revolves mainly around manipulation of the internal `base_data` and `meta_data` variables.  These are first instantiated as empty data tables from the `data.table` package within `data_tab_server()`.  These variables are then changed when the user imports data, builds new data, adds to the dataset, filters the data, or converts field types.  `base_data` contains the dataset of interest to the user, while `meta_data` contains the column names, the column types, and the cardinality of each categorical column.

To monitor changes in the `base_data` and `meta_data` variables, two more `reactiveVal` components are created: `base_update()` and `meta_update()`.  These are integers which are incremented through `trigger_base_update()` and `trigger_meta_update()`.  `trigger_base_update()` is called whenever the `base_data` is changed, and `trigger_meta_update()` is called whenever the `meta_data` is changed.  `base_update()` and `meta_update()` are placed in components which should update whenever the base data or meta data updates.

#### **Build Data**
`data_tab_server()` includes the data building procedures within `observeEvent(input$build_btn, {...})`.  The code for this procedure is fully commented below.

```R
  # Run this code whenever the Build Data button is pressed.
  observeEvent(input$build_btn, {
    # Require that the following inputs have values:
    # Coverage selection
    # Policy effective date range
    # Username (for data warehouse connections)
    # Password (for data warehouse connections)
    # Variable selections
    req(
      input$build_base_selector,
      input$dates_input,
      input$username_input,
      input$password_input,
      input$build_vars_input
    )
    
    # Also require that the username and password are valid (able to connect to DB2P)
    req(check_db2p(input$username_input, input$password_input))
    
    # Assemble the user inputs in a named list called args.
    args <- list(start_date=as.character(input$dates_input[1]),
                 end_date=as.character(input$dates_input[2]),
                 uid=input$username_input,
                 pwd=input$password_input)
    
    # Also store the selected variables.
    sel_vars <- input$build_vars_input
    
    # As well as the selected coverage.
    base_cov <- input$build_base_selector
    
    # Create a new data_builder() object, defined in data_builder_class.R
    # This object contains methods which allow us to build a new dataset.
    builder <- data_builder()
    
    # load_var_lib() runs source.R files found in var_lib_path, under the folders
    # named by c(base_cov, sel_vars).  These source.R files contains definitions
    # (functions) which take the dataset and add fields to it.  We store these functions
    # in var_src, an attribute of the data_builder() object.
    builder$var_src <- builder$load_var_lib(var_lib_path, c(base_cov, sel_vars))
    
    # We print the current time to the console, to track the duration of this step.
    print(Sys.time())
    
    # The build_vars() function runs the variable functions for the given variables
    # (base_cov, in this case) passing along the given arguments (args) to those
    # functions.  The 'base_cov' coverage functions are unique in that they do not
    # add to an existing dataset, but rather create the "base" of the dataset to which
    # other variables are appended to.  This base includes the premiums, losses, 
    # exposures, policy keys, and etc.  Note the use of <<- so that base_data is
    # accessible within other components as well.
    base_data <<- builder$build_vars(base_cov, args, input$build_deps_checkbox)
    
    # We add base_data to the arguments list, since the other variables will be
    # adding to it.
    args[['base_data']] <- base_data
    
    # The build_vars() function runs the variable functions for the given variables
    # (sel_vars, in this case) passing along the given arguments (args) to those
    # functions.
    base_data <<- builder$build_vars(sel_vars, args, input$build_deps_checkbox)
    
    # Print the current time again to the console so that the duration may be noted.
    print(Sys.time())
    
    # This step updates the base_data variable.  Any components which are dependent
    # on base_data will have base_update() included.  trigger_base_update() increments
    # base_update(), causing those components to detect the change and update.
    trigger_base_update()
    
    # Print to the console that the build has completed.
    print('Build complete.')
  }, ignoreInit = TRUE)
```
