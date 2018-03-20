
options(datatable.alloccol = 900)
library(data.table)
library(microbenchmark)
library(profvis)
library(magrittr)
library(testthat)
library(readxl)
temporary_variables <- c("tw_src_env_var", "Start_Space", "fn_file", "Params_text", "i", "fn_text",
                         "li", "fn", "utility_fns", "Merged_Input", "Merged_Output", "v",
                         "Database_Values", "Data_and_Params", "Input_Data", "Functions",
                         "initial_functions", "Parameters_and_Inflation_Factors",
                         "Essential_Values", "Output_Values")
if (exists("v")) {
    cat("Warning: variable \"v\" found in scope will be overwritten and destroyed.\n")
}
for (v in temporary_variables) {
  if ((v != "v") & exists(v)) {
    cat("Warning: variable", paste0("\"",v,"\""),
        "found in scope will be overwritten and destroyed.\n")
  }
}
cat("\n")
initial_functions <- lsf.str()
if (!exists("Period_Mode")) {
  Period_Mode <- F
}
if (Period_Mode) {
  cat("Period mode.\n\n")
  tw_src_env_var <- "TW_SRC_PERIOD"
} else {
  cat("Annual mode.\n\n")
  tw_src_env_var <- "TW_SRC_ANNUAL"
}
if (Period_Mode) {
  Essential_Values <- c("P_ID","F_ID","H_ID","Period","P_Weight")
} else {
  Essential_Values <- c("P_ID","F_ID","H_ID","P_Weight")
}
if (Period_Mode) {
  if (!exists("Aggregate_Period_Data")) {
    cat("Aggregation: Aggregation of period data not indicated.  Aggregating by default.\n")
    Aggregate_Period_Data <- T
  }
}
if (!exists("src_dir")) {
  if (!is.na(Sys.getenv(tw_src_env_var, unset=NA))) {
    src_dir <- Sys.getenv(tw_src_env_var)
    cat("Source directory: Using environment variable", tw_src_env_var, "=\n", src_dir, "\n\n")
  } else {
    stop("Source directory not defined in script or environment variable.")
  }
} else {
  cat("Source directory: Using supplied src_dir=\n", src_dir, "\n\n")
}
Inflate_Database <- function(Data, Params, Inflators, Inflator_Mapping) {
  base_Year <- Params$baseyear
  model_Year <- Params$modelyear
  inflator_types <- Inflator_Mapping[, unique(Inflator)]
  Inflation_Factors <- Inflators[year==model_Year, ..inflator_types] /
                         Inflators[year==base_Year, ..inflator_types]
  Inflation_Factors <- Inflation_Factors[, data.table(t(.SD), keep.rownames=TRUE)]
  setnames(Inflation_Factors, 1:2, c("Inflator", "Factor"))
  cat("\nApplying inflators: ")
  for (inf in Inflation_Factors[, Inflator]) {
    cat(inf, "")
    vars <- intersect(Inflator_Mapping[Inflator==inf, Variable], names(Data))
    if (length(vars) > 0) {
      Data[, (vars) := Inflation_Factors[Inflator==inf, Factor] * .SD, .SDcols = vars]
    }
  }
  cat("\n\n")
  Params['inflator_InflationMultiplier_earnings'] <- Inflation_Factors[Inflator=='earnings', Factor]
  Params['inflator_InflationMultiplier_selfemp'] <- Inflation_Factors[Inflator=='selfemp', Factor]
  list(Data, Params)
}
TAWA_Build <- function(Input_Data, Functions, Output_Values, Params, Robust = F) {
  for (n in names(Params)) {
    assign(n, Params[[n]])
  }
  Output <- identity
  create_function_list <- function(Functions, Output_Values) {
    attr(Output, "input") <<- Output_Values
    functions <- c()
    visit <- function(f) {
      if (!any(functions==f)) {
        functions <<- c(functions, f)
        for (g in setdiff(Functions, f)) {
          if (length(intersect(attributes(get(g))$output,
                               attributes(get(f))$input))>0) {
            visit(g)
          }
        }
      }
    }
    visit("Output")
    functions
  }
  create_function_graph <- function(Functions) {
    neighbours <- vector(mode="list", length=length(Functions))
    names(neighbours) <- Functions
    for (f in Functions) {
      f_neighbours <- c()
      f_outputs <- attributes(get(f))$output
      for (g in setdiff(Functions, f)) {
        g_inputs <- attributes(get(g))$input
        if (length(intersect(attributes(get(f))$output,
                             attributes(get(g))$input))>0) {
          f_neighbours <- c(f_neighbours, g)
        }
      }
      neighbours[[f]] <- f_neighbours
    }
    neighbours
  }
  topological_sort <- function(Functions, Neighbours) {
    sorted_functions <- c()
    markings <- vector(mode="list", length=length(Functions))
    names(markings) <- c(Functions)
    for (i in 1:length(markings)) {
      markings[i] <- ''
    }
    visit <- function(f) {
      if (markings[[f]] == 'T') {
        stop("Cycle detected?")
      }
      if (markings[[f]] == '') {
        markings[[f]] <<- 'T'
        for (g in Neighbours[[f]]) {
          visit(g)
        }
        markings[[f]] <<- 'P'
        sorted_functions <<- c(f, sorted_functions)
      }
    }
    while (sum(markings=='')>0) {
      f <- names(markings)[markings==''][1]
      visit(f)
    }
    sorted_functions
  }
  Fast_Join <- 1 - Robust
  if (Period_Mode) {
    Periods <- 24
  } else {
    Periods <- 1
  }
  Database_Values <- names(Input_Data)
  if (length(setdiff(Output_Values, Database_Values)) > 0) {
    fn_list <- create_function_list(Functions, Output_Values)
    fn_neighbours <- create_function_graph(fn_list)
    fn_list_sorted <- topological_sort(fn_list, fn_neighbours)
    fn_list_sorted <- fn_list_sorted[-length(fn_list_sorted)] 
    GLOBAL_unused_fns <<- setdiff(Functions, fn_list)
    Merged_Input <- lapply(fn_list_sorted, function(x) attributes(get(x))$input) %>%
                      unlist() %>% sort()
    Merged_Output <- lapply(fn_list_sorted, function(x) attributes(get(x))$output) %>%
                       unlist() %>% sort()
    Int_Values <- intersect(Merged_Input, Merged_Output)
    Input_Values <- setdiff(Merged_Input, Int_Values)
    if (length(setdiff(Output_Values, union(Merged_Output, Database_Values))) > 0) {
        stop(paste("Output variable(s):",
                   paste(setdiff(Output_Values, Merged_Output), collapse=", "),
                   "cannot be found in input database or produced with available procedures."))
    }
    if (length(setdiff(Input_Values, Database_Values)) > 0) {
      stop(paste("Required variable(s):",
                 paste(setdiff(Input_Values, Database_Values), collapse=", "),
                 "not found in input database."))
    }
    for (fn in fn_list_sorted) {
      fn_args <- names(as.list(args(get(fn))))
      fn_args <- fn_args[1:length(fn_args)-1]
      for (arg in fn_args) {
        if ((arg!='Data') & (!exists(arg))) {
          stop(cat("Parameter", arg, "not supplied."))
        }
      }
      args_list <- vector(mode="list", length=length(fn_args))
      names(args_list) <- fn_args
      for (n in names(args_list)) {
        if (n=='Data') {
          args_list[[n]] <- as.name('Input_Data')
        } else {
          args_list[[n]] <- as.name(n)
        }
      }
      do.call(fn, args=args_list)
    }
  } else {
    cat("\nNB: All requested output variables found in input database.  No procedures to apply.\n")
  }
  Input_Data
}
Functions <- list.files(path = src_dir, pattern = "[.][rR]$", full.names = T)
for (fn_file in Functions) {
  Start_Space <- lsf.str()
  tryCatch(source(fn_file), error = function(x)
           print(paste("File", fn_file, "attempts to overwrite an existing function.")))
  if (identical(setdiff(lsf.str(), Start_Space), character(0))) {
    print(paste("File", fn_file, "doesn't define any new functions."))
    next
  }
  lockBinding(setdiff(lsf.str(), Start_Space), .GlobalEnv)
}
split_path <- function(x) {
  if (dirname(x)==x)
    x
  else
    c(basename(x), split_path(dirname(x)))
}
Functions <- unlist(lapply(Functions, function (x) {gsub("\\..*", "", split_path(x)[[1]])}))
utility_fns <- Functions[lapply(Functions,
                                function (x) identical(attributes(get(x))$utility, T)) == T]
Functions <- Functions[!(Functions %in% utility_fns)]
Merged_Input <- lapply(Functions, function(x) attributes(get(x))$input) %>%
                unlist() %>% sort()
Merged_Output <- lapply(Functions, function(x) attributes(get(x))$output) %>%
                 unlist() %>% sort()
if (exists("Parameters_File")) {
  cat("Parameters: Reading supplied Parameters_File = \n", Parameters_File, "\n")
  if (exists("Parameters_Sheet")) {
    cat("Parameters: Reading parameters from sheet Parameters_Sheet =",
        Parameters_Sheet, "\n")
  } else {
    cat("Parameters: Parameters_Sheet not specified. Defaulting to first sheet.\n")
    Parameters_Sheet <- 1
  }
  if (exists("Parameters_Column")) {
    cat("Parameters: Reading parameters from column Parameters_Column =",
        Parameters_Column, "\n\n")
  } else {
    cat("Parameters: Parameters_Column not specified. Defaulting to second column.\n\n")
    Parameters_Column <- 2
  }
  Params_text <- read_excel(Parameters_File, sheet = Parameters_Sheet, col_names = T, skip = 1) %>%
    as.data.frame()
  Params_text <- Params_text[complete.cases(Params_text), c(1, Parameters_Column)]
  Params_text$Parameter <- gsub("\\+", "_", gsub("/", "_", Params_text$Parameter))
  Params_text[, 2] <- gsub("\\\\", "/", Params_text[, 2])
  Params_text[, 2] <- gsub("\\[\\[", "rbind(c(", Params_text[, 2])
  Params_text[, 2] <- gsub("\\[", "c(", Params_text[, 2])
  Params_text[, 2] <- gsub("]", ")", Params_text[, 2])
  Params_text[, 2] <- gsub(";", ",", Params_text[, 2])
  Params_text[, 2] <- gsub("'", "", Params_text[, 2])
  Parameters <- vector(mode="list", length=dim(Params_text)[1])
  names(Parameters) <- Params_text$Parameter
  for (i in 1:nrow(Params_text)){
    if (!is.na(suppressWarnings(as.numeric(Params_text[i,2])))){
      Parameters[[Params_text[i,1]]] <- suppressWarnings(as.numeric(Params_text[i,2]))
    } else if (grepl("rbind", Params_text[i,2])) {
      Parameters[[Params_text[i,1]]] <- eval(parse(text = Params_text[i,2])) 
    } else {
      Parameters[[Params_text[i,1]]] <- Params_text[i,2]
    }
  }
} else if (exists("Parameters")) {
  cat("Database: Using Parameters found in containing scope.\n")
} else {
  stop("No Parameters_File supplied and no Parameters list found in containing scope.")
}
if (exists("Terminal_Values_File")) {
  cat("Terminal Values: Using supplied Terminal_Values_File =\n\n",
      Terminal_Values_File, "\n")
  Terminal_Values <- as.vector(fread(Terminal_Values_File))
} else if (exists("Terminal_Values")) {
  cat("Terminal Values: Using Terminal_Values found in containing scope.\n\n")
} else {
  cat("Terminal Values: No Terminal_Values_File and no TAWA_Database data table found in\n")
  cat("containing scope.  Outputting all terminal values.\n\n")
  Terminal_Values <- setdiff(Merged_Output, Merged_Input)
}
Database_Values <- setdiff(union(Merged_Input, Terminal_Values), Merged_Output)
Essential_Values <- c(setdiff(Essential_Values, "P_Weight"),
                      paste0("P_Weight_",Parameters$modelyear,"TaxYearIntegrated"))
Database_Values <- union(Essential_Values, Database_Values)
if ("Database_File" %in% names(Parameters)) {
  cat("Database: Database_File found in supplied parameters.\n")
  Database_File <- Parameters$Database_File
}
if (exists("Database_File")) {
  cat("Database: Reading supplied Database_File = \n", Database_File, "\n\n")
  TAWA_Database <- fread(Database_File, select = Database_Values,
                            colClasses = setNames(rep("character",9),
                                                  paste0("P_Jobs_", seq(1,9),
                                                         "_WageOrSelfEmployed")))
} else if (exists("TAWA_Database")) {
  cat("Database: Using TAWA_Database found in containing scope.\n\n")
} else {
  stop("No Database_File supplied and no TAWA_Database data table found in containing scope.")
}
Input_Data <- copy(TAWA_Database)
if (exists("Inflators_File") & exists("Inflator_Mapping_File")) {
  cat("Inflators: Reading supplied Inflators_File = \n", Inflators_File, "\n")
  if (Inflators_File == Parameters_File) {
    Inflators <- read_excel(Parameters_File, sheet = "Inflators", col_names = T) %>%
                   as.data.table()
    cat("Reading inflators from sheet \"Inflators\"\n")
  } else {
    Inflators <- fread(Inflators_File)
  }
  cat("Inflators: Reading supplied Inflator_Mapping_File = \n", Inflator_Mapping_File, "\n\n")
  if (Inflator_Mapping_File == Parameters_File) {
    Inflator_Mapping <- read_excel(Parameters_File, sheet = "Mapping", col_names = T) %>%
                          as.data.table()
    cat("Reading inflator mapping from sheet \"Mapping\"\n")
  } else {
    Inflator_Mapping <- fread(Inflator_Mapping_File)
  }
} else if (exists("Inflators") & exists("Inflator_Mapping")) {
  cat("Inflators: Using Inflators found in containing scope.\n")
  cat("Inflators: Using Inflator_Mapping found in containing scope.\n\n")
} else {
  stop(paste("No Inflators_File (Inflator_Mapping_File) supplied and no Inflators",
            "(Inflator_Mapping) found in containing scope."))
}
setnames(Inflators, names(Inflators), unlist(lapply(names(Inflators),tolower)))
Inflator_Mapping[, Inflator:=tolower(Inflator)]
Data_and_Params <- Inflate_Database(Input_Data, Parameters, Inflators, Inflator_Mapping)
Input_Data <- Data_and_Params[[1]]
Parameters_and_Inflation_Factors <- Data_and_Params[[2]]
cat("Executing procedures... ")
Final <- TAWA_Build(Input_Data, Functions, Terminal_Values, Parameters_and_Inflation_Factors)
cat("done.\n\n")
if (!exists("Output_File")) {
  cat("Output: Output_File not specified.  Defaulting to NUL.\n\n")
  Output_File <- "NUL"
} else {
  cat("Output: Writing output to file Output_File =\n", Output_File, "\n\n")
}
Output_Values <- union(Essential_Values, Terminal_Values)
if (length(setdiff(Output_Values, names(Final)))>0) {
  cat("Warning: Variable(s)",
      paste(setdiff(Output_Values, names(Final)), collapse=", "),
      "not found in final data table.  Ignoring.\n\n")
  Output_Values <- intersect(Output_Values, names(Final))
}
Final <- Final[, ..Output_Values]
if (Period_Mode) {
  if (Aggregate_Period_Data) {
    Aggregate <- function(x) {
      if (typeof(x) == "double") return(sum(x))
      if (typeof(x) == "logical") return(any(x))
      return(first(x)) 
    }
    Final <- Final[, lapply(.SD, Aggregate), by = P_ID]
    Final[, Period := NULL]
  }
}
fwrite(Final, Output_File)
rm(list=setdiff(lsf.str(), initial_functions))
for (v in temporary_variables) {
  if ((v != "v") & exists(v)) {
    rm(list = v)
  }
}
rm(v, temporary_variables)
invisible(gc())
