# FUNCTION TO CONCATENATE AND PRINT
plog <- function(...) {
  print(paste0(...))
}

# FUNCTION TO FIX SIDEBAR TAB FUNCTIONALITY
newMenuItem <- function(..., tabName) {
  mi <- menuItem(..., tabName = tabName)
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  return(mi)
}

# FUNCTION TO FILTER DATASET
filter_data <- function(dt, variable, conditional, value) {
  filter <- switch(
    conditional,
    '>'  = (dt[[variable]] > value),
    '>=' = (dt[[variable]] >= value),
    '<'  = (dt[[variable]] < value),
    '<=' = (dt[[variable]] <= value),
    '='  = (dt[[variable]] == value),
    '!=' = (dt[[variable]] != value),
    'include' = (dt[[variable]] %in% value),
    'exclude' = (! dt[[variable]] %in% value)
  )
  return(dt[filter])
}

# FUNCTION TO CALCULATE NORMALIZED ASE
nase <- function(actuals, predicted, weight) {
  actuals <- actuals / weighted.mean(actuals, weight) * 100
  predicted <- predicted / weighted.mean(predicted, weight) * 100
  err <- (predicted - actuals) ^ 2
  weighted.mean(err, weight)
}

# FUNCTION TO RETURN CUSTOM STYLE
css_style <- function(box_color, nav_color) {
  tag <- "
    .box.box-solid.box-primary>.box-header {
      color:#fff;
      background:#box_color
    }
  
    .box.box-solid.box-primary{
      border-bottom-color:#box_color;
      border-left-color:#box_color;
      border-right-color:#box_color;
      border-top-color:#box_color;
    }
  
    .logo {
      background-color: #nav_color !important;
    }

    .navbar {
      background-color: #nav_color !important;
    }
  "
  tag <- str_replace_all(tag, '#box_color', box_color)
  tag <- str_replace_all(tag, '#nav_color', nav_color)
  tag <- tags$style(HTML(tag))
  return(tag)
}

# FUNCTION CHECK DB2P CONNECTION
check_db2p <- function(uid, pwd) {
  library(odbc)
  status <- tryCatch({
    dbConnect(odbc(), 'DB2P', uid=uid, pwd=pwd)
    return(TRUE)
  }, error = function(e) {
    print(e)
    return(FALSE)
  })
  return(status)
}

# RECURSIVE FUNCTION TO CALCULATE PRODUCT OF MULTIPLE FACTOR TABLES
merge_reduce <- function(tables, join_cols) {
  # IF ONLY ONE TABLE, RETURN
  if (length(tables)==1) {
    # GET FINAL TABLE
    final_table <- tables[[1]]
    # GET FACTOR COLUMNS
    fct_cols <- setdiff(names(final_table), join_cols)
    # CALCULATE PRODUCT
    final_table[, product:=1]
    for (fct in fct_cols) {
      final_table[, product:=product*get(fct)]
      final_table[, c(fct):=NULL]
    }
    # RETURN FINAL TABLE
    setkeyv(final_table, join_cols)
    return(final_table)
  # ELSE IF MULTIPLE TABLES
  } else {
    # GET THE FIRST TABLE
    tbl1 <- tables[[1]]
    # GET THE JOIN COLUMNS PRESENT IN THE TABLE
    tbl1_cols <- intersect(names(tbl1), join_cols)
    # LOOK FOR ANOTHER TABLE THAT HAS AT LEAST ONE OF THOSE COLUMNS
    for (i in 2:length(tables)) {
      tbl2 <- tables[[i]]
      tbl2_cols <- intersect(names(tbl2), join_cols)
      match_flag <- any(tbl1_cols %in% tbl2_cols)
      if (match_flag) {
        match_ind <- i
        break
      }
    }
    # JOIN TABLES
    match_cols <- intersect(tbl1_cols, tbl2_cols)
    merge_tbl <- merge(tbl1, tbl2, by=match_cols, allow.cartesian=T)
    # RENAME FACTOR COLUMNS
    fct_cols <- setdiff(names(merge_tbl), join_cols)
    new_cols <- paste0('fct', 1:length(fct_cols))
    setnames(merge_tbl, fct_cols, new_cols)
    # STORE NEW TABLE
    tables[[match_ind]] <- merge_tbl
    # DELETE FIRST TABLE
    tables[1] <- NULL
    # PASS NEW LIST INTO ANOTHER ITERATION
    merge_reduce(tables, join_cols)
  }
}

# FUNCTION TO RETURN CLASS WITH CASE FOR ORDERED FACTORS
uclass <- function(x) {
  if ('ordered' %in% class(x)) {
    return('factor')
  } else {
    return(class(x)[1])
  }
}

# FUNCTIONS FOR ASSEMBLING VARIABLE MESSAGE CONTENT
createVarHeader <- function(varName, varDesc) {
  paste("<u><b><em>[", varName, "] ", varDesc, ":</em></b></u>", sep="")
}

createVarMessage <- function(varName) {
  # "AGE_AND_PTS", "NON_PSRC_VIO_CNTS", "PSRC_VIO_CNTS"
  if (grepl("AGE_AND_PTS", varName, perl=TRUE) |
      grepl("PSRC_VIO_CNTS+", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is an 'Age and Points' ('Violation Points') variable"),
      "1. Verify Violation Code to Point-assignment is current.",
      "2. Verify Violation Code to Tiering Violation Group-assignment is current.",
      ""
    )  
  }
  # "BCG_BT", "BCG_ME", "BCG_RAD", "BIPD_BCG_ME", "COLL_BCG_ME", "BMT", "BMT_NO_LIV"
  else if (grepl("BCG+", varName, perl=TRUE) |
           grepl("BMT+", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'Business Class' variable"),
      "1. Pull list of unique SIC codes from CAPP dataset.",
      "2. Verify each SIC code has a BCG in the lookup table.",
      ""
    )  
  }
  # "BIPD_BTG_VEH_AGE_GRP", "BIPD_BTG_ME", "COLL_BTG_ME", "BTG_LIMIT", "BTG_ME", "BTG_STDAMT", "COLL_BTG_VEH_AGE_GRP", "COMP_BTG_ME_2"
  else if (grepl("BTG+", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'Body Type' variable"),
      "1. Pull list of unique vehicle body types from CAPP dataset.",
      "2. Verify each vehicle body type has a BTG in the lookup table.",
      ""
    )  
  }
  # "BIPD_SYM_GRP_0518", "COLL_SYM_GRP_0518", "COMP_SYM_GRP_0518", "PIP_SYM_GRP_0518"
  else if (grepl("SYM_GRP+", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'Symbol Group' variable"),
      "- Verify most recent lookup table is current.",
      ""
    )  
  }
  # "V41_USDOT_SCR_GRPS", "V41_USDOT_SCR_GRPS_2018"
  else if (grepl("USDOT+", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'USDOT Group' variable"),
      "1. Send list of CAPP policies to USDOT (Tom Mendenhall's) team.",
      "2. Merge USDOT scores back onto CAPP dataset.",
      ""
    )  
  }      
  # "CR_GRP", "CR_SCR", "CR_SCR_2018", "CR_TIER"
  else if (grepl("^(CR)", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'Credit' variable"),
      "1. Verify latest credit version to use (D41, D51, or other models).",
      "2. Pull credit scores from EXD for all available policies.",
      "3. Historically, 66% of policies have credit scores from EXD, but some policy terms will not.  For such policies:",
      "... a. If prior policy terms have credit scores from EXD, use them.",
      "... b. Otherwise, leave as blank.",
      "4. Verify latest credit score breaks (ex. 100 maps to D0).",
      "5. For policies with no credit scores, use credit groups CAW.",
      ""
    )  
  }        
  # LIMIT_GRP
  else if (grepl("LIMIT_GRP", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'Coverage Limit' variable"),
      "- Verify most recent lookup tables are current.",
      ""
    )  
  }   
  # "OOS_DRVR_POL_LVL"
  else if (grepl("OOS_DRVR_POL_LVL", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is an 'out-of-state driver' variable"),
      "- If SIC code = 1799, then set OOS_DRVR_POL_LVL = 'N'.",
      ""
    )  
  }         
  # TERR_MDL
  else if (grepl("TERR_MDL", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'Territory' variable"),
      "- Get latest Zip code to territory assignment (in CSV form) from Marketing.",
      ""
    )  
  }             
  # USE_GRP
  else if (grepl("USE_GRP", varName, perl=TRUE)) {
    list(
      createVarHeader(varName, "is a 'Use Group' variable"),
      "- Vehicle Use Group is determined by BT and BC combinations along with clarifying questions asked during quote flow.",
      ""
    )  
  }   
  else {NULL}
}

# FUNCTION TO WRITE SAS READIN FILE IN DATA TAB, SAVE DATA STEP
  wr_sas_readin <- function(meta_length,meta,dt_name) {
    # DEFINE SAS FILE PATH
    cat("%let path=%substr(%sysget(SAS_EXECFILEPATH),1,
      %eval(%length(%sysget(SAS_EXECFILEPATH))-
      %length(%sysget(SAS_EXECFILENAME))));", "\n")
    cat("\n")
    
    # LOAD DATA TO WORK LIBRARY
    cat(paste0("DATA WORK.",dt_name,";"),"\n")
    cat("%let _EFIERR_ = 0;","\n")
    cat('INFILE "' , "&path.",'data.csv"', "\n", sep = "")
    cat("DELIMITER = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;","\n")
    
    # INFORMAT
    wr_informat <- for(i in meta_length){  
      if(meta$columns[i] %in% c(meta$columns[grepl("_DT",meta$columns)])){
        cat("\t","INFORMAT",meta$columns[i],"yymmdd10. ;", "\n")
      }
      
      else if (meta$types[i] %in% c("character","factor")){
        cat("\t","INFORMAT",meta$columns[i],"$35. ;", "\n")
      }
      
      else {
        cat("\t","INFORMAT",meta$columns[i],"best32. ;", "\n")
      }
    }
    
    # FORMAT
    wr_format<- for(i in meta_length) {
      if(meta$columns[i] %in% c(meta$columns[grepl("_DT",meta$columns)])){
        cat("\t","FORMAT",meta$columns[i],"DATE9. ;", "\n")
      }
      
      else if (meta$types[i] %in% c("character","factor")){
        cat("\t","FORMAT",meta$columns[i],"$35. ;", "\n")
      }
      
      else {
        cat("\t","FORMAT",meta$columns[i],"best12. ;", "\n")
      }
    }
    
    # INPUT
    cat("\t","input","\n")
    wr_input<- for(i in meta_length) {
      cat("\t", meta$columns[i], "\n")
    }
    
    # END SAS FILE
    cat("\t",";","\n")
    cat("\t","if _ERROR_ then call symputx('_EFIERR_',1);", "\n")
    cat("run;")
  }
  
  
  
  