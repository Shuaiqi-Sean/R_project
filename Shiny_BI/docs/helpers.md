#### **Overview**
`helpers.R` contains several function definitions which are used throughout the app.

#### **Functions**
`helpers.R` contains the following functions:
* `plog(...)`: This function paste the arguments provided together and then prints the result to the console.
* `newMenuItem(..., tabName)`: This function is a wrapper for `menuItem(..., tabName)`. It is required in order for the tabs (Data, Model, Analysis, ...) to switch correctly when clicked.
* `filter_data(dt, variable, conditional, value)`: This function is used to filter a dataset based on a field (`variable`), a `value`, and a comaprison operator (`conditional`).  It will look at the `conditional` argument provided, and then perform the corresponding comparison using the `switch()` statement.  `switch()` will return a vector of TRUE/FALSE corresponding to the number of rows in the dataset.  We use this vector to subset the dataset (`dt`), and return the result.
* `nase(actuals, predicted, weight)`: This function returns the normalized ASE given the a vector of observed values (`actuals`), a corresponding vector of predictions (`predicted`), and a corresponding vector of weight values (`weight`).
* `css_style(box_color, nav_color)`: This function accepts 2 colors (created with the `rgb()` function) and uses those to return CSS which modifies the color of the header bar of the page and the color of box elements.  The arguments for `box_color` and `nav_color` are defined in `app.R`.
* `check_db2p(uid, pwd)`: This function attempts to connect to DB2P using the provided username (`uid`) and password (`pwd`).  If the connection fails, it returns FALSE, otherwise, TRUE.
* `merge_reduce(tables, join_cols)`: This function is a recursive function which will merge together a list of tables (`tables`) using the names of the fields by which to join those tables (`join_cols`).  It will take the first table in the list, look for another table in the list which shares at least one of the key columns (`join_cols`), and then perform an inner join of the two tables.  It will repeat this process until all tables have been joined together.
* `uclass(x)`: This function evaluates the class of `x` and returns only the first class if the object has multiple.  Additionally, it returns `factor` if the object is of type `ordered`.
