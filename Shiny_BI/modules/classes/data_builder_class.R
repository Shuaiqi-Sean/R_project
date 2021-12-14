data_builder <- setRefClass(
  'data_builder',
  fields = list(
    var_src = 'list',
    built_vars = 'character'
  ),
  methods = list(
    # FUNCTION TO LOAD LIBRARY
    load_var_lib = function(path, build_vars) {
      var_lib <- list()
      for (var in build_vars) {
        if (! var %in% names(var_lib)) {
          plog('Loading ', var, ' definition...')
          source(here(path, var, 'source.R'), local=TRUE)
        }
      }
      return(var_lib)
    },
    # FUNCTION TO BUILD VARIABLE FROM VARIABLE LIBRARY BY NAME
    build_vars = function(vars, args, dependencies=TRUE) {
      print(Sys.time())
      var_tree <- get_parent_tree(vars, var_src)
      build_order <- get_build_order(var_src[var_tree])
      if (!dependencies) build_order <- intersect(build_order, vars)
      for (var in build_order) {
        plog('Building ', var, '...')
        builder <- var_src[[var]][['builder']]
        tryCatch({
          new_data <- do.call(builder, args)
          args[['base_data']] <- new_data
          built_vars <<- c(built_vars, var)
        }, error=function(e){
          print(e)
          plog('Failed build for ', var)
        })
      }
      print(Sys.time())
      return(new_data)
    },
    # RECURSIVE FUNCTION TO DETERMINE ORDER IN WHICH TO BUILD VARIABLES
    # BASED ON DEPENDENCY CHAIN OF VARIABLES
    get_build_order = function(var_lib, build_order=c()) {
      for (var in names(var_lib)) {
        if (length(var_lib[[var]][['dependencies']])==0) {
          build_order <- c(build_order, var)
          var_lib[[var]] <- NULL
          for (j in names(var_lib)) {
            var_lib[[j]][['dependencies']] <- setdiff(var_lib[[j]][['dependencies']], var)
          }
        }
      }
      if (length(var_lib)==0) {
        return(build_order)
      } else {
        get_build_order(var_lib, build_order)
      }
    },
    # RECURSIVE FUNCTION TO GET PARENT DEPENDENCIES FOR GIVEN VARIABLES
    # WILL INCLUDE THE GIVEN VARIABLES
    get_parent_tree = function(vars, var_lib, tree=c()) {
      for (var in vars) {
        deps <- var_lib[[var]][['dependencies']]
        if (!is.null(deps)) deps <- get_parent_tree(deps, var_lib, tree)
        tree <- c(tree, deps)
      }
      tree <- c(tree, vars)
      tree <- unique(tree)
      return(tree)
    },
    # RECURSIVE FUNCTION TO GET CHILD DEPENDENCIES FOR GIVEN VARIABLES
    # WILL EXCLUDE THE GIVEN VARIABLES
    get_child_tree = function(vars, var_lib, tree=c()) {
      for (var in vars) {
        for (src in names(var_lib)) {
          deps <- var_lib[[src]][['dependencies']]
          if (var %in% deps) {
            tree <- c(tree, src)
            tree <- get_child_tree(src, var_lib, tree)
          }
        }
      }
      tree <- unique(tree)
      return(tree)
    },
    # FUNCTION TO RETURN UPDATE RECOMMENDATIONS BASED ON DATASET AND CURRENT BUILD
    recommend = function(path, dt) {
      all_vars <- list.dirs(path, full=F, recursive=F)
      sink('NUL') # SUPPRESS CONSOLE OUTPUT
      var_lib <- load_var_lib(path, all_vars)
      sink()
      dt_vars <- intersect(all_vars, names(dt))
      recommendations <- get_child_tree(built_vars, var_lib)
      recommendations <- intersect(recommendations, dt_vars)
      recommendations <- setdiff(recommendations, built_vars)
      return(recommendations)
    }
  )
)