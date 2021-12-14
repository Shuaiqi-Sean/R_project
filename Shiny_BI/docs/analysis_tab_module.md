#### **Overview**
`analysis_tab_module.R` contains the definition for the UI and server component of the Analysis tab. `analysis_tab_ui()` is a function which returns the HTML code required to create the Analysis tab UI.  It receives an arbitrary `id` as an argument, which is used to make the UI components unique against other Shiny modules.  See the [Shiny modules](https://shiny.rstudio.com/articles/modules.html) documentation for more on this feature.  The `analysis_tab_server()` function contains the bulk of the code and forms the server component of the Analysis tab.  `analysis_tab_server()` and `analysis_tab_ui()` are called in `app.R` as part of the main UI and main server code.

#### **Custom Classes**
`analysis_tab_server()` revolves mainly around manipulation of several objects created from custom classes:
* The `fct_tbls` object of class `factor_tables`, defined in `factor_tables_class.R`.
* The `rebase_design_list`, a list of `rebase_rule` class objects, defined in `rebase_rule_class.R`.
* Objects of class `table_design`, defined in `table_design_class.R`.
* Objects of class `product_table`, defined in `factor_tables_class.R`.

`fct_tbls` is a `factor_tables` class object which contains several attributes for storing data and several methods for manipulating that data.  It contains the data tables used to store our factors, the `rhandsontable`s used to display that data, the `table_design` containing the parameters used to create that data, and `product_table`s for multiplication of those tables.  More on this object is explained in the `factor_tables_class.R` documentation.

#### **Update Triggers**
There are 4 elements which we used to control update triggers:
```R
  # REACTIVE VALUES TO OBSERVE DATA CHANGES
  fct_update <- reactiveVal(0)
  name_update <- reactiveVal(0)
  trigger_fct_update <- function() fct_update(isolate(fct_update())+1)
  trigger_name_update <- function() name_update(isolate(name_update())+1)
```
When an element should update when the factor tables have updated, we place `fct_update()` inside that element so that it becomes dependent on it.  If an element should update when the names of the factor tables have updated, we place `name_update()` inside that element.  If we would like an element to trigger an update for any elements with `fct_update()`, then we place `trigger_fct_update()` inside that element.  Likewise for `trigger_name_update()`.  `fct_update()` and `name_update()` are simply integers which are incremented by `trigger_fct_update()` and `trigger_name_update()`.  The increment causes Shiny to detect a change, and thus rerun any elements dependent on `fct_update()` or `name_update()`.  Note that `fct_update()` and `name_update()` are not intrinsically linked to the `factor_tables` object, it is up to the developer to put `trigger_fct_update()` and `trigger_name_update()` in the correct places.

#### **Factor Table Outputs**
The most important component of the `analysis_tab_module` is the `# FACTOR TABLE DATA TABLE OUTPUTS` `observe()` section.  Due to the line `fct_tbls$sync_data(input)`, which includes `input` as an argument, this observer runs when there is nearly any change in inputs (Usually we use input$some_element to react to only a specific input).  This observer takes the underlying data of the `fct_tbls` object and creates corresponding `rhandsontable` representations of that data.  Below is the code, fully commented.

```R
# FACTOR TABLE DATA TABLE OUTPUTS
  observe({
    # Run this code whenever the factor tables or their names have updated.
    fct_update()
    name_update()
    
    # If the user has made any changes to the rhandsontable displays, 
    # particularly making selects, then the sync_data(input) call will
    # detect those changes and carry them to the underlying data.
    fct_tbls$sync_data(input)
    
    # The factor tables include columns for the original indicates/selects,
    # as well as the rebased indicates/selects.  Before we apply the rebasing rules,
    # we will set the rebased columns equal to the original columns.
    fct_tbls$reset_to_raw()
    
    # The fct_tbls object includes parameters for any desired multiplication
    # of tables, which is useful for when the user would like to take several
    # related tables and multiply their factors together.  To generate these
    # tables, we call the calc_all_product_tables() method.
    fct_tbls$calc_all_product_tables()
    
    # The rebase_design_list() is a reactive object containing a list of 
    # rebase_design objects.  For each rebase_design in this list, we will apply
    # the rule to the factor tables object.  We also use tryCatch() here, so that
    # if the rule errors out, it will not apply and instead return an error/warning
    # without crashing the app.
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
    
    # The user may specify "comparison" columns, which is just a column stating the
    # percent difference between two column.  We store the arguments of this user
    # selection in the compare_field() reactive object, which is a list of those 
    # arguments.  To create this field, we use the compare_column method of the 
    # fct_tbls object.
    compare_field <- compare_fields()[['col_name']]
    if (!is.null(compare_field)) {
      fct_tbls$compare_column(
        compare_fields()[['col_1']],
        compare_fields()[['col_2']],
        compare_field
      )
    }
    
    # After we have synced the data tables, set the factors back to original,
    # calculated product tables, applied rebasing rules, and created the comparison
    # columns, we will update the rhandsontable representations from the underlying
    # data.  We use the sync_rhots() method to do this which is short for
    # sync_rhandsontables.
    fct_tbls$sync_rhots()
    
    # After the rhandsontables are synced to the underlying data, we will apply 
    # any highlighting rules selected by the user.  These apply to the rhandsontable
    # objects, and not the data.table objects, so this step comes after sync_rhots().
    # We store the highlight arguments from the user in highlight_fields(), 
    # a reactive object.  We then use the highlight() method of the fct_tbls object
    # to apply this highlighting.
    if (!is.null(highlight_fields())) {
      fct_tbls$highlight(
        highlight_fields()[['val_col']], 
        highlight_fields()[['lower_col']], 
        highlight_fields()[['upper_col']]
      )
    }
    
    # Here we use lapply() to loop through the rhandsontable representations 
    # and use renderRHandsontable() to create reactive Shiny displays of these tables.
    # Inside renderRHandsontable(), we apply several cosmetic changes to the tables.
    # After this point, there are technically 3 representations of our data:
    # The data.table objects, representing the raw data.
    # The rhandsontable (server) objects, contained in the fct_tbls object.
    # The rhandsontable (client) objects, created through renderRHandsontable().
    # Shiny does its best to keep the server and client tables in sync, but results
    # may vary.
    lapply(fct_tbls$get_names(), function(x) {
    
      # We use paste0() to create an id for out rhandsontable display.
      # This is simply the concatenation of the data table name and 'fct_tbl_output'.
      fct_tbl_name <- paste0(x, '_fct_tbl_output')
      
      # renderRHandsontable() is outputted to output[[fct_tbl_name]] to create 
      # the output display.
      output[[fct_tbl_name]] <- renderRHandsontable({
      
        # Inside renderRHandsontable(), first we get the rhandsontable from
        # the fct_tbls object.
        tbl <- fct_tbls$rhots[[x]]
        
        # If the user has put any columns in the sort selector, we will sort
        # by those columns.  This is fairly easy with data.table but since we
        # are dealing with an rhandsontable object, we must first extract the (JSON)
        # data, convert it to data table, sort, then convert it back to JSON.
        sort_cols <- input[[paste0(x, '_sort_selector')]]
        if (!is.null(sort_cols)) {
          dt <- jsonlite::fromJSON(tbl$x$data)
          dt <- as.data.table(dt)
          setorderv(dt, sort_cols)
          dt <- jsonlite::toJSON(dt)
          tbl$x$data <- dt
        }
        
        # We get the names of the table columns here, so that we can reference them 
        # in the next few steps.
        cols <- tbl$x$colHeaders
        
        # Several columns will have customized formats to them such as currency
        # formatting, percentage formatting, etc.  These formats are stored in
        # /analysis/aggregators/ and are selected by the user using the 
        # select_agg_selector element.
        val_fmt_cols <- names(val_fmts())
        for (fmt in val_fmt_cols) {
          if (fmt %in% cols) {
            tbl <- val_fmts()[[fmt]](tbl)
          }
        }
        
        # The user may use the hide_fields_selector element to hide specific columns.
        # To accomplish this, the column widths are set to 0.1, which renders them
        # as if they were hidden.
        for (hide in input$hide_fields_selector) {
          if (hide %in% cols) {
            tbl <- hot_col(tbl, col=hide, colWidths=0.1)
          }
        }
        
        # Any comparison columns created by the user are formatted as percents.
        if (!is.null(compare_field)) {
          if (compare_field %in% cols) {
            tbl <- hot_col(tbl, col=compare_field, format='0.00%')
          }
        }
        
        # We set the entire table to read only, except for the sel_fct_raw column.
        non_sel_cols <- setdiff(cols, 'sel_fct_raw')
        tbl <- hot_col(tbl, col=non_sel_cols, readOnly=TRUE)
        
        # The modified rhandsontable is returned and given to the 
        # renderRHandsontable call.
        tbl
      })
    })
  })
```
