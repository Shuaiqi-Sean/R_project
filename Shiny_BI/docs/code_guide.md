This guide is to assist in understanding and building upon the app code base.  The app is built using Shiny, so it is recommended to read or watch the [Shiny tutorials](https://shiny.rstudio.com/tutorial/) first. Additionally, it is recommended to become familiar with [Shiny modules](https://shiny.rstudio.com/articles/modules.html) and the [Shiny function reference documentation](https://shiny.rstudio.com/reference/shiny/).

#### **Packages**
The app is built using R and uses several packages:
```R
library(rhandsontable) # Render interactive displays of data tables.
library(shiny) # The framework for the app: both client (UI) and server components.
library(shinydashboard) # Components for the UI layout.
library(shinyFiles) # Tool for importing local data through the shiny UI.
library(DT) # Alternative tool for rendering data table displays.
library(data.table) # Reading, writing, and manipulation of datasets.
library(lubridate) # Used for date handling.
library(h2o) # Algorithms for creating models.
library(here) # Use here() to free the code from static folder locations.
library(plotly) # For creating interactive charts.
library(stringr) # For manipulation of strings. 
library(openxlsx) # For writing datasets into Excel sheets.
```

#### **Common Code**
The app makes regular use of several functions and techniques which are outlined below.

#### **Shiny UI**
```R
  column(
    width = 6,
    ...
  )
```
`column()` is used to define a UI space - a "column".  The width is an integer from 1 to 12, and is the total width of 12 is shared by other adjacent columns. Together with `fluidRow()`, we can form flexible grids of spaces on which to place UI elements.

```R
  fluidRow(...)
```
`fluidRow()` is used to define a UI space - a "row".  `column()` is typically placed inside `fluidRow()` in order to form grids in which to place UI elements.

```R
  box(
    title = 'A Box',
    status = color,
    solidHeader = TRUE,
    width = NULL,
    ...
  )
```
`box()` is used to create a UI box that allows us to group UI elements which are related.  The `title` parameter allows putting a header onto the box.  The `status` parameter is set to `color`, which is defined in `app.R`.  The `solidHeader` parameter allows the header to have a solid color.  `width` is set to NULL so that the box stretches to fit the space it is placed in.

```R
  tabsetPanel(
    tabPanel('Name', ...),
    tabPanel('Name', ...)
  )
```
`tabsetPanel` will create a UI element containing tabs defined by `tabPanel`.  The `tabsetPanel` is placed inside a `box()` element and will fill the space.  The `tabPanel` elements allow grouping related elements into tabs within that space.

```R
  ns <- NS(id)
  ns()
```
`ns()` is short for namespace, and it is used for Shiny code modularization. `NS(id)` returns the `ns()` function, and `ns()` is used to append `id` as a prefix onto a given input, similar to `paste()`.  The use of this is to make sure an HTML element in a module is unique amongst all modules.  Read about [Shiny modules](https://shiny.rstudio.com/articles/modules.html) for more information.

```R
  actionButton('id', label='Button Label')
```
`actionButton()` creates a button in the UI with the given id and label.  The `'id'` argument is used in the server function to observe the value of the button (whether it has been pressed or not).

```R
  uiOutput('some_ui')
```
More often than not, a UI element needs to be dynamic.  For instance, a selection input may give names of the columns of a given dataset.  If the dataset changes, the selection input needs to change too.  To meet the need for dynamic UIs, we have the server create the UI element instead and retrieve that element through `uiOutput()`.

```R
  textInput('id', label='Text Input')
```
`textInput()` creates a text box which the user can then enter a string into.  This is used if we want the user to control the name of some object, such as a dataset, model, etc.  The value given is retrieved by the server through the given `id`.

```R
  selectInput('id', label='Selection Input', choices=c('a','b','c''), mult=F)
```
`selectInput()` is used when we would like the user to choose from a list of things.  Typically, this list of things is dynamic, so `selectInput()` is seen often in the server code where the `choices` are updated.  `mult` can be set to `TRUE` or `FALSE` if we want the user to be able to select multiple things or not.  The server can retrieve the vector of selections with the given `id`.

```R
  DTOutput('some_data_table_output')
  dataTableOutput('some_data_table_output')
```
`DTOutput()` and `dataTableOutput()` are two names for the same function, which renders an interactive data table display on the UI.  The id given refers to a corresponding `renderDT()` or `renderDataTable()` server function.

#### **Shiny Server**
```R
  input$some_input
```
`input` is an argument given to the server function.  It tracks any inputs or changes to the UI such as button presses or selection changes.  Specfic inputs can be accessed with `input$id` where `id` is the id of the element.  When referenced inside a reactive component, such as `renderUI({...})` or `observe({...})`, the input will trigger the reactive component to run its given expression `{...}` if the input changes.

```R
  some_var <- reactiveVal(...)
```
`reactiveVal()` is used to create a (reactive) value that is accessible from any component within the Shiny server.  Additionally and importantly, when this value changes, it will trigger an update for any components that depend on it.  The value is accessed with the syntax `some_var()` and changed with the syntax `some_var(new_val)`.  We can prevent a component from updating due to its dependency on the reactive value by using `isolate(some_var())`.  The reactive value can be any R object.

```R
  req(...)
```
`req()` is used to check if a value does not exist or if it evaluates to FALSE.  When it sees either, the remaining code in the expression in which it is called does not run.  This is useful if you have a button that should only run when certain inputs are given, or if there is a selector that should only appear if certain inputs are given.

```R
  output$some_ui <- renderUI({...})
```
`renderUI()` is used to created a UI object, with the benefit that we can make the UI object more dynamic when it is created server-side.  The expression inside `{}` will run any time one of the dependencies, such as an `input$` or a reactive value changes.  The last statement in the `renderUI({})` expression is typically a Shiny UI call, such as `selectInput()` or perhaps a combination of UI elements inside of a `fluidRow()` call.  See `uiOutput()` for how this output is accessed.

```R
  observeEvent(input$some_btn, {...})
```
`observeEvent()` will monitor the status of an input or inputs and will run the given expression when the status changes.  The input is often a button.  Importantly, the expression inside `{}` is only run when that specified input changes and not whenever a dependency does.

```R
  observe({...})
```
`observe()` is like `observeEvent()` except that it will run whenever any of its dependencies, such as reactive values or some input, changes.

```R
  updateSelectInput(session, 'id', ...)
```
Sometimes it is useful to change (update) a UI input component from outside the `renderUI({...})` it is made.  When this functionality is needed, `updateSelectInput()` can be used to reference the input component by `id` and can change its list of choices, selections, labels, options, etc.

#### **data.table Techniques**
```R
  fread(path, colClasses = named_character_vector)
  fwrite(obj, path)
```
`fread()` and `fwrite()` are fast file reading and writing functions provided by `library(data.table)`.  In order to get the correct column types upon import, we use the `colClasses` parameter, which accepts a named character vector as an argument (names are the column names, values are R data types).

```R
  dt[i, j, ...]
```
This is the general syntax for using a `data.table`.  `i` represents row subsetting.  `j` represents column subsetting/manipulation.  `...` are other options, such as `by`, `on`, and `.SDcols`.

```R
  dt_c <- dt_b[dt_a, on=.(col1, col2)]
```
This syntax is used to left join `dt_b` to `dt_a` using the columns `col1` and `col2` as common keys.  The resulting dataset will be stored in `dt_c`.

```R
  dt[col1 == some_val]
  dt[col2 > some_val]
  dt[col3 %in% some_vector]
```
Row subsetting a `data.table` is simple.  Above are a few examples which subset using TRUE/FALSE evaluation by comparing a column to a value or set of values.

```R
  dt[1:1000]
```
Row subsetting can also be done through indices.

```R
  dt[, .(col1, col2)]
  dt[, c('col1', 'col2'), with=F]
```
To retrieve only certain columns from a `data.table`, we use both techniques shown above.  The `.()` syntax is short for `list()`.

```R
  dt[, .I]
  dt[, .N]
```
`dt[, .I]` includes the special `data.table` variable `.I`, which will return indices of the rows.  `.N` will return the number of rows.

```R
  dt[, col1:=new_vals]
```
Assigning new values to a `data.table` column is done using the `:=` operator.  Using `:=` is typically better than `dt$col1 <- new_vals`.

```R
  dt[, sum(col1), by=.(col2)]
  dt[, sum(col1), by=c('col2')]
```
`data.table` can perform group-by operations using the `by` parameter.  This is SQL equivalent to `SELECT SUM(col1) FROM dt BY col2`.

```R
  dt <- unique(dt)
```
An easy way to de-dupe a `data.table` is to use `unique()`.  This is SQL equivalent to `SELECT DISTINCT ... FROM dt` or `SELECT ... FROM dt GROUP BY ...`.

```R
  first_inds <- dt[, .I[1], by=.(col1, col2)]$V1
  dt <- dt[first_inds]
  
  last_inds <- dt[, .I[.N], by=.(col1, col2)]$V1
  dt <- dt[last_inds]
```
Another way to de-dupe is to select the first or last occurence of duplicate records by some key.  Above are two examples of this.

```R
  setnames(dt, c('old'), c('new'))
```
Renaming columns is possible through `setnames()`.

```R
  dt <- dt[order(col1)]
  setorderv(dt, c('col1'))
```
Ordering data is possible through `order()` or by name through `setorderv()`.

```R
  dt[, .(alias_col = original_col)]
```
Aliasing columns can be done using `=` within the `.()` syntax.

```R
  names(dt)
```
The column names of a `data.table` can be retrieved using `names()`.

```R
  sum_cols <- c('col1', 'col2')
  dt[, lapply(.SD, sum), by=.(col3), .SDcols=c(sum_cols)]
```
We can apply the same aggregation to multiple columns simultaneously using `lapply`, `.SD`, and `.SDcols`.  Here, `.SD` is a special `data.table` variable which stands for subdata.  Technically, it is a list of all columns in the dataset.  We can specify which columns are include in `.SD` by specifying the `.SDcols` option.

```R
  dt[col1==col2, .(col3=sum(col4)), by=.(col5)][col3>0][order(col6)]
```
`data.table` operations can be combined and chained together.

#### **Other R Techniques**
```R
  list.dirs(path, full=F, recursive=F)
  list.files(path, full=F)
```
`list.dirs()` is useful for getting the names of folders within a given path.  With `full=F` and `recursive=F`, it gives just the folder name back and not the full path, and it does not give folder names within the folder.  `list.files()` will give the names of the files within a path.

```R
  <<-
```
```R
  a <- 1
  incr <- function() a <- 2
  incr()
  print(a)
  # 1
  a <- 1
  incr <- function() a <<- 2
  incr()
  print(a)
  # 2
```
The regular assignment operator `<-` is used for local assignment of a variable.  Using `<<-` however, will also modify the variable in any parent environments.  An example is shown above.

```R
  lapply(some_list, function(x){...})
  sapply(some_list, function(x){...})
```
`lapply()` is a useful function for applying a given function to each item in a list.  The return of `lapply()` is a list of the same length, with the given `function(x){...}` applied to each item in the original list.  `sapply()` is similar to `lapply()` except it will return a vector instad of a list.

```R
  here(...)
```
Normally to reference a file the R programmer would need to first use `setwd('path')` to set their working directory and then use `'some/folder/file.x'` to refer to a file.  `setwd()` is not ideal for code that is shared, since not everyone will have the same local folder structure.  As an alternative, we use `library(here)`, which automatically sets the working directory to the folder in which the script is launched.  Using `here(...)` will give a full path.  For instance, if we launch the script from `C:/projects/`, then `here('app', 'foo', 'bar')` will return `C:/projects/app/foo/bar/`.

```R
  setdiff(x, y)
```
`setdiff()` will find the difference of the given `x` (a vector) from another given `y`. The placement of the arguments matters.  If `x = c(1, 2, 3)` and `y = c(1, 2)` then `setdiff(x, y)` will return `3`.  If `x = c(1, 2)` and `y = c(1, 2, 3)` then `setdiff(x, y)` will return nothing (`integer(0)`).
