Eventually you may need to add a new variable to the variable library so that you can use them on the **Data** tab.  The app uses a modular approach to variables; each variable is contained in a single folder containing everything needed to build that variable.  Adding a variable to the variable library is fairly straightforward.
  
1. Navigate to `data/vars`.  
2. Create a new folder with the name of your variable.  
3. Copy in the `source_template.R` from `data/vars`, or a `source.R` file from a similar variable.  
4. Rename `source_template.R` to `source.R`.  
5. Open `source.R`.  
6. Assign to `var_name` the name of your variable.  This should be the same name as the folder.  
7. Assign to `var_lib[[var_name]][['dependencies']]` the name(s) of variables in the variable library that your new variable depends on.  
8. Modify the procedures within the `var_lib[[var_name]][['builder']]` function.  This function takes any arguments and the source template includes the arguments provided from the app.  One input, `base_data`, will be always be a dataset, and the `return` should always be `base_data` with the new variable(s) appended.  The arguments are explained below.  You should include any files you need for this variable within the variable folder.  You can then reference those variables using `here(var_lib_path, 'SAMPLE_VAR', 'sample_file.csv')` which will return the full file path of the file.  The `base_data` dataset originates from `data/vars/base/source.R`.  There you can see which fields are included in the initial dataset before variables are appended.
  * `start_date` is the beginning policy effective date.  This can be used when filtering data from data warehouses.
  * `end_date` is the ending policy effective date.  This can be used when filtering data from data warehouses.
  * `uid` is a username.  This can be used to connect to data warehouses.
  * `pwd` is a password.  This can be used to connect to data warehouses.
  * `base_data` is the dataset which we are adding the new variable to.
9. Save the `source.R` file.
