#### **Overview**  
The `rebase_rule` class is defined in `rebase_rule_class.R`. The class of objects is created using `setRefClass()`.  For more information about reference classes, [see here](http://adv-r.had.co.nz/R5.html).

#### **Fields**
`rebase_rule` has the following fields:
* `name`: User provided name of the rebasing rule. Also forms the column names of the adjustment columns added to factor tables.
* `rebase_to_lvl`: Boolean value. TRUE if using the rebase-to-level method, FALSE if not.
* `rebase_to_base`: Boolean value. TRUE if using the push-adjustment-to-base-rate method, FALSE if not.
* `tgt_tbl_name`: Table name of the table being rebased.
* `fct_mdl`: Name of factor model, used to find which columns to rebase/adjust.
* `src_mdl`: Name of another factor model, used to calculate the rebase/adjust factor.
* `var_col`: If using the `rebase_to_lvl` method, this field stores which variable contains the level the factors are being rebased to.
* `var_lvl`: If using the `rebase_to_lvl` method, this field stores the level of the variable to which the factors are being rebased to. Only used when both `rebase_to_lvl` and `rebase_to_base` are TRUE. If `rebase_to_base` is FALSE, `map_vars` is used instead.
* `adj_tbl_name`: Table name of the table being adjusted. Only used if `rebase_to_base` is FALSE.
* `join_col`: If not using the `rebase_to_lvl` method, this field specifies the column used to join the target table (`tgt_tbl_name`) to the adjustment table (`adj_tbl_name`).
* `wt_col`: Name of the field containing the weights used for weighted average calculations. Only used if not using the `rebase_to_lvl` method.
* `map_joins`: A vector containing levels of the column specified by `join_col`.
* `map_vars`: A vector of the same length as `map_joins` containing the levels of `var_lvl` to which the levels of `map_joins` will be rebased to.
* `map_adjs`: If neither `rebase_to_lvl` nor `rebase_to_base` is used, then rebasing is calculated using the weighted average within each level of `map_joins`. The default is rebasing to 1. `map_adjs` allows setting the new weighted average to a number other than 1.

#### **Methods**
`rebase_rule` has the following methods:  
* `set(...)`: This method is used to set the values of each of the fields. Depending on the values of `rebase_to_lvl` and `rebase_to_base`, some values are set to `NULL`.
* `summary()`: This method returns a `data.table` containing a formatted summary of the rebase rule.
* `apply(fct_tbl_list)`: This method accepts a named list of `data.table` objects and applies the rebase rule to the list.  `rebase_to_lvl` and `rebase_to_base` can be TRUE or FALSE meaning there are 4 different ways of rebasing.
  * `rebase_to_lvl` = TRUE, `rebase_to_base` = TRUE:  
      1. Get the factor (`src_mdl`) corresponding to the specified level (`var_lvl`) of the specified variable (`var_col`).  
      2. Divide the factor column (`fct_col`) by that factor.
  * `rebase_to_lvl` = TRUE, `rebase_to_base` = FALSE: 
      1. Create a mapping table which maps specified levels of `join_col` (`map_joins`) to specified levels of `var_col` (`map_vars`).
      2. Get the factor (`src_col`) corresponding to each mapping.
      3. Within each level of `join_col`, divide the factors (`fct_col`) of the target table (`tgt_tbl_name`) by the mapped factor.
      4. To offset the rebasing, multiply the factors of the adjustment table (`adj_tbl_name`) by the mapped factor.
  * `rebase_to_lvl` = FALSE, `rebase_to_base` = TRUE:
      1. Compute the weighted average of the factor column (`fct_col`).
      2. If a different `src_col` is specified (`src_col` != `fct_col`), divide that weighted average by the weighted average of the source column.
      3. Divide the factor column by the rebase calculated in step 2.
  * `rebase_to_lvl` = FALSE, `rebase_to_base` = FALSE:
      1. Calculate the weighted average of the factor column (`fct_col`), within each level of `join_col`.
      2. If a different `src_col` is specified (`src_col` != `fct_col`), divide the weighted averages from step 1 by the weighted averages of the source column within each level of `join_col`.
      3. Divide each rebase from step 2 by the specified manual adjustment (`map_adjs`) within each level of `join_col`.
      4. Divide the factors (`fct_col`) of the target table (`tgt_tbl_name`) by the rebase from step 3.
      5. To offset the rebasing, multiply the factors of the adjustment table by the rebase from step 3.
