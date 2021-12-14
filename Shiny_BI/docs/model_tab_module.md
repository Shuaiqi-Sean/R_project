#### **Overview**
`model_tab_module.R` contains the definition for the UI and server component of the Model tab. `model_tab_ui()` is a function which returns the HTML code required to create the Model tab UI.  It receives an arbitrary `id` as an argument, which is used to make the UI components unique against other Shiny modules.  See the [Shiny modules](https://shiny.rstudio.com/articles/modules.html) documentation for more on this feature.  The `model_tab_server()` function contains the bulk of the code and forms the server component of the Model tab.  `model_tab_server()` and `model_tab_ui()` are called in `app.R` as part of the main UI and main server code.

`model_tab_server()` launches H2O locally on demand from the user.  From there, the user can load data into H2O from the data library.  `model_tab_server()` handles saving and loading of the `design` object, which is a named list of various model parameters (selected variables, number of steps, tweedie parameter, etc).  

`observeEvent(input$save_model_btn, {...}` handles combining and transforming `h2o` models into a single custom R class, `model()`, which is defined in `modules/classes/model_class.R`.  This object is important, as it is needed to interact with models without launching h2o.

Aside from building models, `model_tab_server()` and `observeEvent(input$score_data_btn, {...}` handle scoring data on a model.  This model can either be one created and saved from h2o or one generated from the Analysis tab. 

#### **Model Builder**  
One of the most important components of the model tab is the model builder, which takes inputs from the user and builds an h2o model using the data currently loaded in h2o.  Below is the fully commented code for `observeEvent(input$build_mdl_btn, {...})`, which completes this important step.

```R
# BUILD MODEL BUTTON
  observeEvent(input$build_mdl_btn, {
    # Before running anything, require the following conditions:
    # H2O is running (the Launch H2O button has been pressed)
    # A dataset has been selected by the user
    # A weight column has been selected by the user (such as ECY)
    # A distribution has been selected by the user (such as tweedie)
    # A response variable has been selected by the user
    # A name has been given by the user for the model
    req(
      h2o_is_up(),
      input$data_file_input,
      input$select_weight_selector,
      input$select_dist_selector,
      input$select_response_selector,
      input$model_name_input
    )
    
    # Also require that, for each step specified, that there are variables 
    # selected for each step (they are not empty)
    for (i in 1:n_steps()) req(input[[paste0('select_mdl_vars_selector_', i)]])
    
    # Print in the console that model building has started.
    print('Building model...')
    
    # To refer to the h2o dataset specified by the user, we use h2o.getFrame(), using
    # the user specified dataset name as an argument.  The result is stored in the
    # dt variable, which points to the h2o environment dataset.
    dt <- h2o.getFrame(input$data_file_input)
    
    # Weights must be non-zero, so we filter them out before moving forward.
    dt <- dt[dt[[input$select_weight_selector]]>0,]
    
    # The user may specify reference levels, one for each categorical variable.
    # These variables and their corresponding reference levels are stored in the
    # reactive value, ref_lvl_tbl(), which is a data.table object.  We loop through
    # the rows of ref_lvl_tbl(), and at each row, get the specified variable and 
    # attempt to set the reference level to the specified level.
    
    # Get the number of reference level specifications.  Each row represents a 
    # reference level specified by the user.
    n_refs <- nrow(ref_lvl_tbl())
    
    # For each row in the reference level table,
    for (i in seq_len(n_refs)) {
    
      # Get the variable (field) containing the level
      var_name <- ref_lvl_tbl()[i, vars]
      
      # Get the reference level to set
      lvl_name <- ref_lvl_tbl()[i, lvls]
      
      # Check that the variable exists within the dataset
      if (! var_name %in% names(dt)) {
        plog('Variable not found in dataset: ', var_name)
        req(FALSE) # STOPS observeEvent
      }
      
      # Check that the reference level exists amongst the levels of that variable
      if (! lvl_name %in% h2o.levels(dt, var_name)) {
        plog('Reference level ', lvl_name, ' not found in variable ', var_name)
        req(FALSE) 
      }
      
      # Set the reference level of the variable using h2o.relevel()
      dt[[var_name]] <- h2o.relevel(x = dt[[var_name]], y = lvl_name)
      plog('Set reference level of column ', var_name, ' in dataset ', input$data_file_input, ' to level ', lvl_name)
    }
    
    # We must also check that for each interaction specified, the corresponding 
    # main effect pieces are included, else h2o will produce an error.  We first assume
    # that all interactions main effects are included by setting ints_included = TRUE.
    # If we encounter any exceptions to the case, we will set ints_included to FALSE.
    ints_included <- TRUE
    
    # The HTML element ids corresponding to each interaction are stored in int_ids(), 
    # a reactive value.  We loop through each interaction -
    for (j in int_ids()) {
      # Get the step for which the interaction is included
      int_step <- input[[paste0('select_int_vars_step_selector_', j)]]
      
      # Get the categorical field name
      cat_var <- input[[paste0('select_int_vars_cat_selector_', j)]]
      
      # Get the numeric field name
      num_var <- input[[paste0('select_int_vars_num_selector_', j)]]
      
      # Get the variables included in that step
      step_vars <- input[[paste0('select_mdl_vars_selector_', int_step)]]
      
      # If the categorical variable is not in the step variables, produce a 
      # warning in the console and set ints_included to FALSE
      if (! cat_var %in% step_vars) {
        plog(cat_var, ' not found in step ', int_step, ' variables.')
        ints_included <- FALSE
      }
      
      # If the numeric variable is not in the step variables, produce a 
      # warning in the console and set ints_included to FALSE
      if (! num_var %in% step_vars) {
        plog(num_var, ' not found in step ', int_step, ' variables.')
        ints_included <- FALSE
      }
    }
    
    # If ints_included is FALSE, stop here and do not run the rest of the code.
    req(ints_included)
    
    # We must check that each variable is only used once in the model and not 
    # duplicated amongst different steps.  To do so, first we get all variables
    # selected.
    all_vars <- lapply(1:n_steps(), function(x){
      input[[paste0('select_mdl_vars_selector_', x)]]
    })
    all_vars <- unlist(all_vars)
    
    # We assume that there are no duplicates by setting no_dupes to TRUE.  If
    # we encounter any exceptions to the case, we will set no_dupes to FALSE.
    no_dupes <- TRUE
    
    # For each variable selected,
    for (var in unique(all_vars)) {
      # Count the number of instances that this variable is found amongst the
      # selected variables.  If it is found more than once, then we set 
      # no_dupes to FALSE.
      if (sum(all_vars %in% var)>1) {
        plog(var, ' found in multiple steps. Please remove or create duplicate variables with different names.')
        no_dupes <- FALSE
      }
    }
    
    # If no_dupes is false, stop here and do not run the rest of the code.
    req(no_dupes)
    
    # The tweedie distribution is treated differently than other distributions,
    # namely that we currently have a method for creating multi-step tweedie
    # models but not for other distributions. A tweedie model is built for each step,
    # and in between each step an adjustment is performed to the weights and response
    # variable such that succeeding steps are solved on the residuals of the prior.
    
    # If the selected distribution is a tweedie distribution...
    if (input$select_dist_selector=='tweedie') {
      
      # Get the selected response and weight field names
      input_y <- input$select_response_selector
      input_wt <- input$select_weight_selector
      
      # Build a tweedie model for each step
      for (i in 1:n_steps()) {
        
        # For the first step, use the user supplied response and weight fields.
        # For other steps, use the adjusted columns created from the prior step.
        if (i==1) {
          y <- input_y
          weights_column <- input_wt
        } else {
          y <- paste0(input_y, '_adj_', i-1)
          weights_column <- paste0(input_wt, '_adj_', i-1)
        }
        
        # h2o uses an interaction_pairs parameter which accepts a list of vectors.
        # Each vector contains 2 strings, which are the names of the variables
        # that should interact.
        interaction_pairs <- list()
        
        # For each interaction specified,
        for (j in int_ids()) {
        
          # Get the corresponding step # of that interaction
          int_step <- input[[paste0('select_int_vars_step_selector_', j)]]
          
          # If the corresponding step # matches the current step #,
          # Store the interaction arguments in interaction_pairs
          if (int_step==i) {
            cat_var <- input[[paste0('select_int_vars_cat_selector_', j)]]
            num_var <- input[[paste0('select_int_vars_num_selector_', j)]]
            next_ind <- length(interaction_pairs) + 1
            interaction_pairs[[next_ind]] <- c(cat_var, num_var)
          }
        }
        
        # If there are no interactions, set interaction_pairs to NULL
        if (length(interaction_pairs)==0) {
          interaction_pairs <- NULL
        }
        
        # Use h2o.glm() to build the h2o model object.  See the h2o documentation
        # for details about this function.
        mdl <- h2o.glm(x = input[[paste0('select_mdl_vars_selector_', i)]],
                       y = y,
                       interaction_pairs = interaction_pairs,
                       training_frame = dt,
                       model_id = paste0(input$model_name_input, '_step_', i),
                       seed = 44124,
                       family = input$select_dist_selector,
                       weights_column = weights_column, 
                       tweedie_variance_power = input$tweedie_params_input,
                       tweedie_link_power = 0,
                       solver = 'IRLSM',
                       lambda = 0,
                       standardize = FALSE,
                       compute_p_values = TRUE,
                       beta_epsilon = 1e-9,
                       remove_collinear_columns = TRUE)
        
        # Use h2o.predict() to get the prediction output column.
        dt[[paste0('pred_', i)]] <- h2o.predict(mdl, dt)[['predict']]
        
        # Calculate the 'total' prediction thus far.  For the first step,
        # the total prediction is simply the output of the model.  For subsequent
        # steps, we multiply preceding total prediction by the current model prediction
        # since the model is multiplicative (as opposed to additive).
        if (i==1) {
          dt[['total_pred']] <- dt[['pred_1']]
        } else {
          dt[['total_pred']] <- dt[['total_pred']] * dt[[paste0('pred_',i)]]
        }
        
        # The adjusted response variable (the residual) is calculated by dividing
        # the observed values by the total prediction.  The adjusted weights are
        # calculated by wts * pred^(2-p) where p is the tweedie power parameter.
        dt[[paste0(input_y, '_adj_', i)]] <- dt[[input_y]] / dt[['total_pred']]
        dt[[paste0(input_wt, '_adj_', i)]] <- dt[[input_wt]] * dt[['total_pred']] ^ (2-input$tweedie_params_input)
      }
    
    # For other distributions, we build only one step (one model).
    } else {
      interaction_pairs <- list()
      for (j in int_ids()) {
        cat_var <- input[[paste0('select_int_vars_cat_selector_', j)]]
        num_var <- input[[paste0('select_int_vars_num_selector_', j)]]
        next_ind <- length(interaction_pairs) + 1
        interaction_pairs[[next_ind]] <- c(cat_var, num_var)
      }
      
      if (length(interaction_pairs)==0) {
        interaction_pairs <- NULL
      }

      mdl <- h2o.glm(x = input$select_mdl_vars_selector_1,
                     y = input$select_response_selector,
                     interaction_pairs = interaction_pairs,
                     training_frame = dt,
                     model_id = input$model_name_input,
                     seed = 44124,
                     family = input$select_dist_selector,
                     weights_column = input$select_weight_selector,
                     solver = 'IRLSM',
                     lambda = 0,
                     standardize = FALSE,
                     compute_p_values = TRUE,
                     remove_collinear_columns = TRUE)
    }
    
    # Finally, print to the console that the build is complete.
    print('Build complete.')
  })
  
```
