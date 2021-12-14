**Library Paths**  
Defined here are `var_lib_path`, `data_lib_path`, `mdl_lib_path`, and `analysis_lib_path`.  These variables refer to folder paths which contain "libraries" of accessible data: variable definitions in `var_lib_path`, datasets in `data_lib_path`, models in `mdl_lib_path`, and factor tables in `analysis_lib_path`.  The modules refer to these variables to get the location of this data.  These variables can be changed here to refer to the new locations, allowing easy movement of the libraries.  Note the use of the `<<-` operator, so that they are globally accessible.

**Theme**  
Defined here are `box_color`, `nav_color`, and `color`.  `box_color` controls the color of the box headers within the UI.  `nav_color` controls the app header color.  These variables are used by the `css_style()` helper function to create the necessary CSS to change those elements.

**Modules**  
The app is broken up into modules, seperate chunks of code which when pieced together form the app.  The `source()` function runs the code from a given file path.  The `here()` function is used to piece together the file path, using the location of the `app.R` file as the root folder.  There are several modules:  
* `data_tab_module.R` contains UI and server components for the Data tab.
* `model_tab_module.R` contains UI and server components for the Model tab.
* `analysis_tab_module.R` contains UI and server components for the Analysis tab.
* `docs_tab_module.R` contains UI components for the Docs tab.
* `model_class.R` defines the model class object, used for storing model coefficients and prediction methods.  Does not contain the building algorithm, which is in the `h2o` package.
* `rebase_rule_class.R` defines the rebase rule object which contains user-given inputs for rebasing factors and methods to implement them.
* `table_design_class.R` defines the table design object which contains user-given inputs for creating factor tables and methods to implement them.
* `factor_tables_class.R` defines the factor tables object.  The factor tables object contains the designs and corresponding data for all factor tables currently loaded.
* `data_builder_class.R` defines the data builder object.  Used for creating datasets and interacting with the variable library.
* `helpers.R` contains helpful miscellaneous functions.

**Header UI**  
This small `shinydashboard` UI component defines the header of the app.

**Sidebar UI**  
This `shinydashboard` UI component defines the sidebar navigation menu.

**Body UI**  
This `shinydashboard` UI contains all of the UI components for each tab.  To organize the UI code, the functions `data_tab_ui`, `model_tab_ui`, `analysis_tab_ui`, and `docs_tab_ui` are used to return the UI components for each tab seperately.  They are defined in their respective tab module files.

**Assemble UI**  
This code takes the header, sidebar, and body HTML and unifies them into one UI component, the `dashboardPage`.

**Define Server Code**  
This function is the server component of the application.  To organize the code, the server components are split into their respective modules for each tab. `app.R` then calls each of these modules within the server function.

**Run App**  
The `shinyApp(ui, server)` runs the Shiny app, using the ui component and server function we've specified.
