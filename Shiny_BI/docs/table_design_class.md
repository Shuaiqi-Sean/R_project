#### **Overview**  
The `table_design` class is defined in `table_design_class.R`. The class of objects is created using `setRefClass()`.  For more information about reference classes, [see here](http://adv-r.had.co.nz/R5.html).

#### **Fields**
`table_design` has the following fields:
* `tbl_name`: Name of the table.  User given.
* `data_name`: Name of the dataset (located in `data/datasets/`) used to generate the table.
* `mdl_list`: A named list of character vectors. The names correspond to the name of models stored in `model/rdata`. The elements of the list correspond to names of variable(s) of the model.
* `wt_var`: The name of the field in the dataset (`data_name`) that should be used to perform weighted averaging of factors.
* `by_vars`: A character vector of fields to aggregate the model factors by.
* `type`: A character specifying the type of factor table.  This can be `categorical`, `numeric`, `interaction`, `multi-numeric`, or `multi-interaction`.  `multi-numeric` tables are used for representing multiple related numeric variables at once, whereas `multi-interaction` is used for representing multiple interactions that share the same categorical component.
* `enum_var`: Name of a field in the dataset. Corresponds to the selection level of the table or the categorical piece of an interaction, depending on `type`.
* `num_var`: Name of a field in the dataset. For numeric factor tables, determines the field which is multiplied by the coefficient to get a factor.  For multi-numeric and multi-interaction tables, determines the column which contains the names of the individual numeric variables (the coefficients are exponentiated without being multipled by some numeric value).
* `val_funcs`: A named list. The names become field names in the resulting table, and the elements are functions which aggregate dataset fields. These are specified in `analysis/aggregators`.
* `val_vars`: A character vector containing the names of fields in the dataset which are needed by `val_funcs`. These are specified in '`analysis/aggregators`.

#### **Methods**
`table_design` has the following methods:  
* `set(...)`: This method is used to set the values of each of the fields. 
* `summary()`: This method returns a `data.table` containing a formatted summary of the table design..
* `multi_fct_tbl()`: This method is used to generate the factor table. The method reads in data, creates a seperate factor table for each model specified, and then joins those tables together. Afterwards, it does some formatting to ensure certain columns are the right and left sides of the table, for consistency and readability. No arguments are passed in since the necessary information is stored in the object fields.
  1. The first step in creating the factor table is reading in the dataset. The method uses `wt_var`, `by_vars`, `num_var`, `val_vars`, and `mdl_list` to read in only the fields that are needed.
  2. The second step is to create a factor table for each model. It does this by iterating through `mdl_list`. The first table is stored in `tmp_fct_tbl`, while subsequent ones are appended to it, using the `by_vars` as the joining column(s). Some factor tables might not have these joining columns (one-row numeric tables) so when `by_vars` is not populated, we simply `cbind()` them together.
  3. The third step is formatting. Factor columns are pushed to the right of the table. Additionally, the table is sorted by `by_vars` for readability.
* `add_to_fct_table(tbl, mdl_name, mdl_vars)`: This method is similar to `multi_fct_tbl()`, but is used in instances where we would like to add models to existing factor tables instead of creating a new one.
* `create_fct_table(dataset, mdl_name, mdl_vars, val_funcs, include_sel=TRUE)`: This method is used by `multi_fct_tbl()` and `add_to_fct_table()` to generate (single-model) factor tables.
  1. The element of `mdl_list` is broken down into the name (`mdl_name`) and the variable(s) (`mdl_vars`).
  2. Using `mdl_name`, the model is loaded from the model library (`model/rdata`).
  3. The coefficients and interaction pieces (if any) are pulled from the model object.
  4. A model factor is generated for each record of the `dataset`. The way this is done depends on `type`.
      1. `categorical`: The coefficient table is left joined onto the dataset.
      2. `numeric`: The coefficient is multiplied by the numeric value column of the dataset and exponentiated. In cases where no grouping is being performed, the coefficient is simply exponentiated and attached to every record. When it is aggregated, it will then still match it's non-aggregated value.
      3. `interaction`: 
          1. The names of the interacted fields are extracted from `ints` of the model object. 
          2. The coefficient table is left joined onto the dataset. 
          3. The factors for each record are then multiplied by the numeric value column and exponentiated.
      4. `multi-numeric`: 
          1. The (one-row) coefficient table for each specified numeric variable is extracted from the model object `coefs` list. 
          2. A new column is added to each table, containing the name of the variable. 
          3. These tables are then stacked to form one larger table. 
          4. The factors are exponentiated without multiplying by a numeric value. When they are aggregated at the `num_var` level, they will match their non-aggregated values.
          5. This table is left joined to the dataset using the `num_var` field, which contains a name of a numeric variable for each record. This per-record assignment is decided arbitrarily within the associated variable script (`data/vars`).
      5. `multi-interaction`: Follows the same logic as `multi-numeric` types but includes `enum_var` as well as `num_var` in the join to the dataset.
  5. The dataset, with factors now joined to it, is aggregated by `by_vars`. The factors are aggregated by computing the weighted average of the factors for each level of `by_vars`. Since we are using weighted averages, when the dataset is aggregated at levels which are equal to the levels that the model was solved, no change in factors occurs when the factors are aggregated.  This is also true when the factor table levels are subsets of the levels which the model was solved at (such as SIC_CD within BCG_ME).
  6. NA values are mapped to factor of 1.
  7. If any additional aggregation values are desired (populated in `val_funcs`), these are calculated into `val_tbl`, which is then joined to the factor table. `val_funcs` contains quoted (`quote()`) functions, which are then evaluated using `eval()`.
  8. A copy of the factor column is made. This copy is used to perform rebasing on.
  9. If argument `include_sel` is TRUE, then two additional copies of the factor column are made: one for selection, and another for rebasing the selection.
  10. The `by_vars` columns are formatted as factors.
  11. The factor table is returned by the method.
