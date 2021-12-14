#### **Purpose**
The purpose of the app is to get, model, and analyze Commercial Lines CAPP data.

#### **Structure**
The app is seperated into three tabs.  Each tab represents a function in the CAPP process:
* **DATA**: Generate the CAPP dataset.  Pick and choose customized variables, individually defined with a `source.R` file in the variable library.  Save data in the dataset library as csv.
* **MODEL**: Use H2O to generate a model.  Pick and choose datasets and model parameters.  Score data.
* **ANALYSIS**: Analyze factors from the model.  Pick and choose cuts of data.  Make selects and save as a seperate model.
* **PIVOT**: Select fields to cut the data by.  Create calculated fields to aggregate stats.
* **SEGMENTATION**: Choose variables of the data by which to cut the data.  Retrieve segments containing those variables matching certain criteria.

#### **System requirements**
You should have R and RStudio installed before working with the app.

#### **Setup**
1. Run `setup.R` to install the required packages.
2. Run `app.R` to launch the application.

#### **Use**
Typical use of the app will flow through each of the tabs.  Start with the data tab, then go on to the model tab, and finally the analysis tab.  The tabs are explained in more detail below.

<br>
#### **Data**
**Import Data**  
There are two options for importing data into the data tab:  
1. `Import External Data`.  This button allows the user to choose a csv file to import.  
2. `Import from Data Library`.  If the user has already imported or built a dataset and saved them, then that dataset becomes available in the data library.  Each dataset in the data library has its own folder which includes the dataset itself as well as meta data such as column types and possibly prediction data if it has been generated through the model tab.

**Build Data**  
In lieu of importing existing datasets, users can also generate new data through the `Build Data` tool.  
1. Select the data range using the policy effective date ranges.  
2. Select variables to include with the dataset.  Note that some variables are simply appended from other static datasets and will not be available for all policy effective date ranges.  
3. Provide a LAN ID and password in order to connect to Progressive databases.  
4. Click `Build New Dataset` to create a dataset from scratch, or click `Add to Dataset` if you wish to add the selected variables to a dataset you have imported.  

**Filter Data**  
Here you can click `Add Filter` to create a new filter rule.  This allows you to select categorical and numeric fields and choose simple filter rules (exclude, include, less than, greater than, etc.).  Click `Apply Filters` to filter the dataset.

**Save Data**
Here you can give the in-memory dataset a name and save it as a csv file.  The variable selections used to build the dataset will also be saved in the dataset folder as `variable_selections`.  The `Clear Data` button can be used to clear the data from memory.

**Data Preview**  
When data is populated, a table appears in this box with a preview of the dataset.  

**Code Viewer**
This selector allows you to view the `source.R` file for any selected variable.

**Inspect Data**  
Here you can select a field and get a bar or density chart of the field.

**Convert Data Types**
This tool allows you to select any variables in the dataset and convert them to a specific R data type.  This can also be done by modifying the meta data view below.

**Meta Data**  
This table provides column types and cardinality for each field.  The column types are `R` data types.  The `types` field can be edited by clicking on a cell.  It is important that any categorical variables used in modeling are classified as `factor` while any numeric variables are classified as `numeric`, `integer`, or `integer64`.

<br>
#### **Model**
**Launch**  
The app uses [H2O](http://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html) for modeling purposes.  At this time, only GLMs are supported in the app.  This section of the app allows you to specify the number of cores (threads) to use as well as the maximum memory allocation for H2O.  Click `Launch H2O` and H2O will launch from wherever the app is running.  Click `H2O Flow` and you will be redirected to the H2O GUI.

**Data**  
Select a dataset from the data library to model on.  After you have launched H2O, click `Import Data` to get the data into the H2O cluster.

**Load Model Design**  
If you have already created a model and saved off the design, you can click `Load Design` to load that configuration again from the model design library.  Otherwise, go to the Parameters section to construct a new model design.

**Parameters**  
* **Variables**: These main effect selectors are populated with names of columns in the imported dataset.  For multi-step models (tweedie only), click `Add Step`.  The app will automatically adjust the response variable in between steps when solving.  Categorical by numeric interactions can be specified by clicking `Add Interaction`.  Categorical by categorical interactions should be created explicitly in the dataset beforehand (such as by concatenating two fields) and then added as a main effect. The tool does not support using the same field in multiple steps.  If you would like to do this, duplicate the field with a different name.
* **Response**: Select the response (dependent variable).
* **Weight**: Select the per-record weight.
* **Distribution**: Specify the response distribution.  Only tweedie distributions support multi-step models at this time.
* **Tweedie power**: If you have selected tweedie for the distribution, you can specify the tweedie power here.
* **Relevel variable**/**Reference Level**: You can select categorical variables and set their corresponding reference levels by clicking `Add Reference Level`.  Select a variable and the reference level and click `Remove Reference Level` to remove the reference level setting from the table.
* **Model Name**: This text input field serves to give the model and its design a name.  This field should be populated for the Export section to work.
* `Build Model`: This will build the model in H2O.  The model will not be saved until you do so in the Export section.
* `Save Design`: This button will save the Parameter inputs so that you can load them with ease later.

**Export**  
The selector here is populated with H2O objects.  If it is not, click `Refresh Library`.  Select the model object(s) in H2O and click `Save Model` to save the model factors.  This will create a model object, as defined in `modules/classes/model_class.R`.

**Predict**  
This tool allows the user to score a given dataset with a given model.  It is assumed that the dataset has the necessary variables and levels for the given model, else an error will be produced.  The tool will also calculated normalized ASE (observed and predicted values rebased to 100), which is why the user must specify **Observed Values** and **Weight**.  In addition to the prediction, the tool will also output the factors whose product give the prediction.  The predictions are assigned the name given in the **Prediction File Name** field and is saved as a csv in the data library along with the specified dataset.

The tool can also be used to calculate residuals of the prediction by checking on the **Calculate residuals?** box.  This feature will add columns to the dataset representing the residual (commonly known as adjusted pure premium for CL R&D), as well as the adjusted numerator (loss) and adjusted denominator (ECY).

The **Normalized ASE** box will display NA if something went wrong during scoring.  This is often because there are levels in the data which have not been seen by the model, or if there are entire variables which are not present in the data but are present in the model.

**Coefficients**  
Here the user can select a model to inspect the GLM coefficients.

**Editor**
The Editor tool allows you to quickly modify and merge models.  Multiple models can be selected, and their components will be merged into a new, singular model whose name is specified in **New Model Name**.  The **Include Variables** and **Exclude Variables** selectors allow the user to select which components will go into the new model.

<br>
#### **Analysis**
**Factor Table Design**  
There are 6 tabs related to factor table design: **New**, **Add**, **Apply**, **Product**, **Load**, and **Save**.
* **New**.  Here you can create new factor tables and save the design of tables you make.
   * **Dataset**.  Select a dataset for the underlying data.
   * **Aggregation Columns**.  Select an R file from `analysis/aggregators/` which defines the summary columns specific to your coverage/dataset.
   * **Models and Factor Variables**.  Select a model and a variable in that model.  You can add or subtract as many models as desired using the `+` and `-` buttons.  Multiple variables can be selected, however, this is only for tables of the multi-numeric or multi-interaction archetype.
   * **Table Type**.  Each table conforms to an archetype corresponding to the chosen model variables.  A typical variable is categorical (e.g. Body Type).  Some are numeric (e.g. Vehicle Age).  Others are categorical-numeric interactions (e.g. Body Type x Vehicle Age).  The CAPP process also has several "trickier" variables, which are archetyped here as "multi-numeric" and "multi-interaction".  Examples of these are age groups and BC x CDL%, respectively.
   * **Weight**.  Factor table creation involves joining model factors to a dataset and then summarizing the dataset by the levels provided in the **Groupings** selector.  In order to summarize factors, a weighted average must be computed.  This selector allows the user to choose the weight used in that computation.
   * **Groupings**.  The dataset and the model factors joined to the dataset will be summarized by the fields specified in this selector.  If you do not specify this field for numeric variables, the tool will generate a one row table containing the exponentiated coefficient of the model variable.
   * **Selection Level**.  This selector is not available for multi-interaction table types.  This selection specifies which field will align with our selects.  For instance, a business class variable may be solved in groups but selects may be made at the individual business class level.
   * **Numeric Variable**.  Specify a field in this selector for numeric and interaction table types.  This field represents the numeric field which is multiplied by the model coefficient to get a factor for each record.
   * **Categorical piece of interaction**.  Specify a field in this selector for multi-interaction table types.
   * **Field containing names of numeric variables**.  Specify a field in this selector for multi-numeric and multi-interaction table types.  This field should contain for each record the name of a numeric variable to which the record is assigned.  For instance, each record has multiple age group fields (AGE_GRP01, AGE_GRP02, etc.).  The dataset should include an additional column (perhaps called AGE_GRP_NAMES) which includes values such as "AGE_GRP01", "AGE_GRP02", etc.  The value of AGE_GRP_NAMES would be determined arbitrarily - for instance, you could map a record to "AGE_GRP03" because the average age falls within that age group.  Or perhaps, you could assign it "AGE_GRP06", because the max driver age falls within that category.
   * **Table Name**.  The table name given here will name the tab in the **Factor Tables** section as well as name the corresponding design.  Choose this name so that it does not match another table, else it will overwrite that table.
   * `Generate Table`.  This button will take the given inputs, create a design object, and then call a function to build a table from that design.  The design object is viewable in the **Apply** tab.
   * `Save to Design`.  This button will take the given inputs and create a design object without generating a table.  The design object is viewable in the **Apply** tab.  This useful for when you would like to specify several tables to build without waiting for them to build in between.
* **Add**.  Use this tool to add new model columns to existing factor tables.
* **Apply**.
   * **View Table Design**.  This selector allows you to view a particular table design currently loaded in the app.
   * `Apply All Designs`.  This button will build out the currently loaded table design set.  This generates a table for each design in the set so it may take a while depending on how many designs are in the set.
   * `Remove Table`.  This button allows you to remove the table and corresponding design selected using the **View Table Design** selector.
* **Product**.  Use this tool to multiply the factors from two or more factor tables.
  * **Load Table**.  Any product table designs currently loaded can be selected here.
  * `Load`.  This button will load the selectors with the arguments used to create the selected product table.
  * `Delete`.  This button will delete the product table.
  * **Name**.  This name is used to identify the product table.  The name becomes a new tab in the Factor Tables box.
  * **Tables**.  Use this selector to select the tables you would like to join together.
  * **Join**.  Use this selector to select the columns which join the tables.  The tool automatically detects which tables have which columns and how to join them.
  * **Factors**.  Use this selector to choose the columns which you would like to see the product of.
  * `Add Product Table`.  This button will build the product table and add it to the tabs in the Factor Tables box.
  * `Clear`.  This button will clear all selectors.
* **Load**  
   * **Factor Tables**.  This selector is populated with factor tables in the factor tables library.
   * `Load Factor Tables`.  This button allows you to load the factor tables into the app.  This will overwrite any currently loaded factor tables and factor table designs.
* **Save**  
   * **Factor Tables Name**.  You may specify a name for the set of factor tables currently loaded.
   * `Save Design`.  This button will save the factor tables and their designs into the factor table library with the name provided in the **Factor Tables Name** field.

**Rebase Design**  
Rebasing is used to change how factors are presented, often so we can easily compare them to prior results.  To rebase factors, we rebase the factors on one table and offset that adjustment in another table so that the overall prediction doesn't change.  Mathematically: if `y = u * v` then so too does `y = (a * u) * (1/a * v)`.  There are 5 tabs related to rebase design: **New**, **Copy**, **View**, **Load**, and **Save**.
* **New**.  Here you can create new rebasing rules, apply them, and save them to a set of rules (designs).
   * **Use rebase-to-level method**.  By default, the rebasing tool will rebase factors by dividing out the weighted average of the factors (the effect of this being that the weighted average factor then becomes 1).  The user may instead choose to rebase relative to a certain level.  For instance, if a discount variable shows 1.10 for level "No" and 1.00 for level "Yes", we might rebase to level "No" so that the "Yes" level is presented as a discount factor.  This would give us 1.00 for level "No" and 0.91 for level "Yes".  This is actually equivalent to setting "No" as a reference level on the Model tab, so when possible it is recommended to do that instead.
   * **Push adjustment to base rate**.  When we rebase, we typically offset that rebasing with an adjustment in another table.  This way, the prediction remains the same.  If the offset adjustment is the same for every policy, then we can apply the offset to the base rate instead (since every policy receives the same base rate).
   * **Apply to confidence intervals**.  Model factors come with a pair of upper and lower confidence intervals.  If you wish to rebase these along with the model factors, check this box.
   * **Rebase Table**.  This is the table whose factors will be rebased.
   * **Adjustment Table**.  If **Push adjustment to base rate** is unchecked, the user must specify where the offset will be going.
   * **Factor Model**.  The rebasing tool will look for and rebase the factor column corresponding to this model in the selected **Rebase Table** and **Adjustment Table**.  It will also find the corresponding confidence intervals, if **Apply to confidence intervals** is checked.
   * **Source Model**.  Typically, the column that is being rebased and the column from which the rebasing adjustment is calculated are the same.  Sometimes, the user may wish to rebase using an adjustment calculated from a different column (such as rebasing to a prior model).  Use this selector to specify a different source for the rebasing adjustment.
   * **Joining Column**.  If **Push adjustment to base rate** is unchecked, the rebasing tool must know how to join the offsets from the rebased table to the table being adjusted.  This requires a common field between the two tables which can be specified here.
   * **Level Variable**.  If **Use rebase-to-level method** is checked, **Level Variable** must be specified so the rebasing tool knows which column contains the level to which the factors are being rebased.
   * **Level**/**Variable Level**.  If **Use rebase-to-level method** is checked, the user must specify which level the factors are being rebased.  If an **Adjustment Table** is also specified, then the user must specify a **Variable Level** for each level of the **Joining Column**.  Typically, each **Variable Level** will be the same, but the option to differ is there if necessary.
   * **Weight**.  If **Use rebase-to-level method** is unchecked, the user must specify which column contains the weights with which to compute weighted average of the factors.
   * **Rule Name**.  This text input field gives a name to the rebasing rule (design).  When the rebasing is applied to the tables, new columns using this name will also appear which contain the rebase adjustments applied to the original factors.  As such, it is recommended that the rule name be as short as possible while maintaing uniqueness from other rebase rule names.
   * `Save to Design`.  This button will save the rebase design.  The rebasing rules are automatically applied whenever the factor tables are updated.
* **Copy**.  Here you can take the current rebase design set (viewable on the **View** tab) and override a few of the arguments in each design en masse.
   * **Factor Model Override**.  For each rule in the rebase design set, the rebasing tool will override the **Factor Model** argument with the model supplied in this selector.  This is useful for applying prior rebasing rules to new models.
   * **Source Model Override**.  Typically, the source model will match the factor model.  In these cases, **Source Model Override** is ignored.  When the **Source Model** argument differs from the **Factor Model** argument for a given rebase rule, the rebasing tool will instead use the model selected here.
   * **Add Suffix to Rule Names**.  This text input allows you to rename each rebase rule by applying a suffix to the name.  Since this makes the name longer, it is recommended to keep the suffix as short as possible.
   * `Copy Design`.  This button will modify each rebasing rule in the rebasing design set.
* **View**.  Here you can apply the current rebasing designs to the factor tables.
   * **View Rule**.  This selector allows you to view specific rebasing rules currently in the rebasing design set.
   * `Delete Rule`.  This button will delete the currently selected **View Rule** from the rebase design set.
   * `Increase Priority`.  This button will increase the priority of the selected rebase rule.  This is useful when you want the selected rebase rule to run before other rules.
   * `Decrease Priority`.  This button will decrease the priority of the selected rebase rule.  This is useful when you want the selected rebase rule to run after other rules.
* **Load**.
   * **Load Design**.  This selector allows you to select a rebasing design set from the rebase design library.
   * `Load Design`.  This button will load the rebasing design into the app.  It will not apply the rebasing rules to the tables until you click `Apply Design` on the **Apply** tab.
* **Save**.  Here you can name and save a set of rebasing rules for later use.
   * **Design Name**.  This text input field allows you to name the set of rebasing rules.
   * `Save Design`.  This button will save the set of rebasing rules in the rebase design library.  You can later load these designs in different sessions if desired.

**Analysis Fields**  
This tool allows you to add percent comparison columns or cosmetically modify factor tables.  There are 3 tabs: **% Difference**, **Highlighting**, and **Hide Fields**.
* **% Difference**.  This tool allows you to choose 2 columns to compare and calculate the percent difference between those 2 column for each factor table.  Note that at this time, a field created in this manner does not change dynamically with its component fields.  That is to say, if the table is modified in any way, you must click `Create Field` again to keep the field accurate.
* **Highlighting**.  This tool will perform a simple highlighting rule.  If **Value** is outside the **Lower Bound** or **Upper Bound**, then the **Value** column will be highlighted red.  Else, it will be highlighted green.  Note that at this time, a field created in this manner does not change dynamically with its component fields.  That is to say, if the table is modified in any way, you must click `Apply Highlighting` again to keep the field accurate.
* **Hide Fields**.  This tool allows you to hide and unhide particular fields in each table.  Useful for when the tables become too wide.  Tip: Zooming in or out of your browser will allow more of the table to fit inside the Factor Table box!

**Factor Tables**  
Any factor tables generated will appear here in tabs.  The user may sort on columns by selecting them in the **Sort** box.  Each table contains a **sel_fct_raw** column, which can be modified to edit selects.

**Chart**  
The chart tool allows you to get a line and bar graph visualization of factors for a given table.  Select a **Table**, a **Variable** to cut the data by, **Factors** to weight average for the line graph, and **Bars** to sum for the bar graph.

**Save Model**  
This tool will create a new model from the given **Factor Column(s)**.  A **Tables** selector is available in case there are any tables the user may wish to exclude from the model.  The **Lower CI** and **Upper CI** selector chooses the model from which confidence intervals were used to inform the selects.  For each table, the tool will check if the first factor column specified exists.  If so, it will use export that factor column into the model.  Otherwise, it will use the second specified factor column, and so on.  The same logic applies to the given CI columns.  Clicking `Save Model` will create a new model object and store it in the model library.  It can then be referenced and used just like any other model.

**Export to Excel**  
In case the user wants to do any ad-hoc analyses, this tool allows you to export the current factor tables into Excel with the given **File Name**.  This `R` console will print where the document has been stored.

<br>
#### **Pivot**
**Load Data**  
Select a dataset to analyze and load it into memory.

**Pivot Parameters**  
Slice the data by **Rows** and/or **Columns**.  The value cells will be populated by calculated fields made using the **Add Field** button.

**Pivot Table**  
View the output of your pivot table aggregation.

<br>
#### **Segmentation**
**Load Data**  
Select a dataset to analyze and load it into memory.

**Segmentation Parameters**  
The segmentation tool is used to create many slices of data and highlight segments of data which meet your chosen criteria.  All pair-wise combinations between **Variable Group 1** and **Variable Group 2** will be used to slice the data.  Then, criteria can be made on calculated fields (made using the **Add Field** button).  Segments which meet the criteria will be highlighted red in the generated tables.

The **(Optional) Select a model to get categorical variables** feature is used to quickly pull in all categorical variables which exist in the data and a selected model.

If you just want all pairwise combinations in one group of variables, just put them all in **Variable Group 1** and leave **Variable Group 2** empty.

**Tables**  
The generated tables will be presented here after clicking **Search Segments**.  The tables can then be saved to an Excel sheet using the **Save to Excel File** feature.
