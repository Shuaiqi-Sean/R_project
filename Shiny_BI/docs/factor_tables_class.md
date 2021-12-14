#### **Overview**  
The `factor_tables` class is defined in `factor_tables_class.R`. The class of objects is created using `setRefClass()`.  For more information about reference classes, [see here](http://adv-r.had.co.nz/R5.html).

#### **Fields**
`factor_tables` has the following fields:
* `rhots`: This is a named list of `rhandsontable` objects, created through the `rhandsontable` package.  `rhandsontable` objects allow representing datasets as Excel-like tables.
* `data`: This is a named list of `data.table` objects, created through the `data.table` package.  `data.table` allows fast, succinct manipulation of datasets.  For each dataset in `data`, there should be a corresponding `rhandsontable` object in `rhots`. 
* `designs`: This is a named list of `table_design` objects, a class defined in `table_design_class.R`.  These objects contain arguments and methods used to create the datasets in `data`.  For each dataset in `data`, there should be a corresponding `table_design` object in `designs`.
* `prod_tbls`: This is a named list of `product_table` objects, defined also in `factor_tables_class.R`.  These `product_table` objects contain no methods besides `initialize`, and are used solely for organizing information.  The information contains arguments which are used to find tables which the user wants multiplied together.

#### **Methods**
`factor_tables` has the following methods:  
* `make_data(design)`: This method creates a factor table as well as the `rhandsontable` representation of the factor table.  This method requires a `table_design` object as an argument.  The `table_design` object contains inputs from the user which define what the table should contain (cuts of the data, model variable, etc.).  The `table_design` object contains a method called `multi_fct_tbl()` which creates this table from the information stored in the `table_design` object.  `make_data()` then adds this table to the `data` list as well as an `rhandsontable` version of this table to the `rhots` list.  The `table_design` object is added to the `designs` list.
* `sync_rhots()`: This method syncs the `rhandsontable` objects within `rhots` to the `data.table` objects within `data`.  It iterates through the `data` list, and for each dataset, creates an `rhandsontable` version of it.
* `sync_data(input)`: This method syncs the `data.table` objects within `data` to the client representations of the `rhandsontable` objects within `rhots`.  The 'client' piece here is important.  While `rhots` does contain `rhandsontable` versions of `data`, Shiny also renders these `rhandsontable` objects using `renderRhandsontable()` and `rHandsontableOutput()`.  These renders are not guaranteed to be in sync with `rhots`.  So, instead of syncing `data` to `rhots`, we sync `data` to Shiny's rendering, accessible from the Shiny variable `input`.  This method is used when the user makes factor selects, thus editing the `rhandsontable` object.
* `get_names()`: A simple method for acquiring the names of the factor tables (names of `data` list).
* `make_rhot(tbl)`: This method takes a `data.table` object and returns a `rhandsontable` version of the object.
* `add_prod_tbl(data_name, val_vars, val_funcs, name, table_names, join_cols, fct_cols)`: This method creates a `product_table` object and adds it to `prod_tbls`.  A `table_design` object is also created, but since product tables differ from normal factor tables, it is only used for its `add_to_model` method and is not stored in the `designs` list.  Instead, it is stored within the `product_table` object.  No calculation (multiplications, joining, etc) takes place in `add_prod_tbl`.  It is used only to store the arguments in a new `product_table` object.  The actual calculation of the product table takes place in `calc_product_table()`.
  * `data_name` refers to the name of a dataset which is read in.  
  * `val_vars` are field names of the dataset which include values such as losses, ECY, etc.  `val_vars` and `join_cols` together specify which columns are read in from the dataset.
  * `val_funcs` is a list of functions which transform the fields of `val_vars`.  These both are specified in `analysis/aggregators`.  These aggregations are only calculated once and the result is stored in the `product_table` object.  This way, the dataset does not have to be read each time the product table is recalculated.
  * `name` is the name given to the product table.
  * `table_names` are the tables which should be joined and multiplied together.
  * `join_cols` specifies fields to slice the data by and simultaneously, fields which are used to join factor tables together for product calculation.  
  * `fct_cols` specifies columns from the factor tables which contain the values which should be multiplied.
* `calc_product_table(prod_tbl)`: This method takes a `product_table` object and performs the multiplication of the tables specified by the object.  
  1. For each factor column:
      1. Get the tables specified by `table_names`.  Include only the `join_cols` fields and the specified factor column.
      2. Use the helper function `merge_reduce()` to join these tables together, creating a product table.
  2. Step 1 produces a product table for each factor column specified.  Join these tables together using `Reduce(merge, ...)`.
  3. Join `val_tbl` to the product table.  This contains information fields such as losses, ECY, premiums, etc.
  4. Store the final product table in `data`.
* `calc_all_product_tables()`: A loop, which iterates through `prod_tbls` and calls `calc_product_table` for each `product_table` object.
* `highlight(fct_col, lower_col, upper_col)`: This method accepts a factor column, a lower bound column, and an upper bound column as arguments.  It iterates through the elements within `rhots`, and adds a custom JavaScript renderer to each one.  This renderer will highlight red the `fct_col` column if it is outside the bounds defined by the `lower_col` column and the `upper_col` column.  This is useful for checking if a select is within a certain confidence interval.
* `add_design(design)`: This method adds the given `table_design` object to the `designs` list.
* `delete_table(tbl_name)`: This method take a given table (by name) and removes the corresponding item from each of the field lists: `data`, `rhots`, `designs`, and `prod_tbls`.
* `apply_designs()`: This method takes the `designs` list and build a table from each design using the `table_design` method, `multi_fct_tbl()`.  This method overwrites the existing `data` and `rhots` fields.
* `add_to_fct_table(tbl_name, mdl_name, mdl_var)`: This method is used for adding additional models to existing factor tables.  It gets the design corresponding to `tbl_name` and then uses the `add_to_fct_table()` method of the design object.  This method returns a data table which overwrites the corresponding `data` element. 
* `reset_to_raw()`: This method is used to reset rebased factors to their original state.  This method is called before rebasing so that we do not accidentally apply the same rebasing rule more than once.  The method gets the models used in each factor table, and for each factor table finds the original and rebased columns.  Then, it sets the rebased columns equal to the original columns.
* `delete_column(col)`: This method deletes from each table the `col` specified, if found.
* `compare_column(col_1, col_2, col_name)`: This method calculates the percent difference between `col_1` and `col_2`, if found, and puts the calculated value in a new field, `col_name`.
