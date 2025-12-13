
#> ChatGPT wrote this for me to just help keep the environment clean after creating
#> different model selection lists
clean_model_lists <- function(..., envir = .GlobalEnv) {
  lists <- as.character(match.call())[-1]
  lists <- lists[lists != "envir"]
  
  for (lst in lists) {
    if (exists(lst, envir = envir)) {
      # get element names if possible
      els <- tryCatch(names(get(lst, envir = envir)), error = function(e) NULL)
      
      # combine list name + element names
      to_remove <- c(lst, els)
      
      # keep only those that exist in environment
      to_remove <- to_remove[to_remove %in% ls(envir = envir)]
      
      # remove quietly
      if (length(to_remove) > 0) {
        suppressWarnings(rm(list = to_remove, envir = envir))
        message("Removed: ", paste(to_remove, collapse = ", "))
      } else {
        message("No matching objects found for ", lst)
      }
    } else {
      message("List not found: ", lst)
    }
  }
}

# Define a function to apply effects coding to specified categorical variables.
#> I need to expand this function widely so I can very easily apply effects coding
#> to different dataframes and what not. 
apply_effects_coding <- function(df, vars) {
  for (var in vars) {
    if (var %in% names(df)) {
      df[[var]] <- as.factor(df[[var]])  # Ensure it's a factor
      contrasts(df[[var]]) <- contr.sum(length(levels(df[[var]]))) / 2  # Effects coding
    }
  }
  return(df)
}


#grab all of the tables from the models and bind them together and rename columns
bind_tidy_tbls <- function(fmdls) {
  
  tidy_tbls <- lapply(fmdls, `[[`, "tidy_tbl")
  
  #tidy_tbls = a list object of the output from rename_lmers
  #filter it down to just the tbls:
  #tidy_tbls <- lapply(mdls, `[[`, "tidy_tbl")
  
  for (i in seq_along(tidy_tbls)) {
    
    name_i <- names(tidy_tbls[i])
    tbl_i <- tidy_tbls[[i]]
    
    #add in model name row
    tbl_i$model <- name_i
    
    #put df back into list
    tidy_tbls[[name_i]] <- tbl_i
  }
  tidy_tbl <- do.call(rbind, tidy_tbls)
  
  tidy_tbl <- tidy_tbl |>
    rename(
      estimate  = Estimate,
      se        = `Std. Error`,
      pval      = `Pr(>|t|)`,
      conf.low  = `2.5 %`,
      conf.high = `97.5 %`
    )
  
  tidy_tbl$model_term_number <- rownames(tidy_tbl)
  rownames(tidy_tbl) <- NULL
  
  return(tidy_tbl)
  
}


# A function to turn a named vector of models into formulas that can be plugged
#into lm/lmer functions 
make_formulas <- function(models) {
  
  #> models = a named vector that contains exactly how the full model should
  #> be specified. 
  #> e.g., : 
  #> models <- c(
  #> model1_name = "dv ~ ivs + (random_effects)",
  #> model2_name = "df ~ ivs + (random_effects)"
  #> )
  
  #create a list to save the formulas to 
  model_formulas <- vector("list", length(models))
  
  #rename the model formula list with the names provided in the "models" input
  names(model_formulas) <- names(models)
  
  #loop through the models and turn them into a formula 
  for(i in seq_along(models)) {
    
    #create the formula for the model and assign it back into model_formulas 
    model_formulas[[i]] <- formula(models[[i]])
    
  }
  
  return(model_formulas)
}


qmd_image_files <- function(directory,
                            go_up = 2,
                            pattern = "\\.png$") {
  
  file_vector <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  output <- vector("list", length(file_vector))
  
  
  going_up <- paste0(rep("../", go_up), collapse = "")
  
  for (i in seq_along(file_vector)) {
    
    
    file_i <- paste0("![](", going_up, file_vector[i], ")", sep = "")
    
    output[[i]] <- file_i
  }
  
  output <- do.call(c, output)
  return(output)
}


run_qmd_image_files <- function(directories, 
                                go_up = 2,
                                pattern = "\\.png$") {
  
  output <- vector("list", length(directories))
  
  for (i in seq_along(directories)) {
    
    directory_files_i <- qmd_image_files(directories[i], 
                                         go_up = go_up,
                                         pattern = pattern)
    
    directory_name <- directories[i]
    
    if(!is.null(directory_files_i)) output[[i]] <- directory_files_i
    
  }
  
  #output <- do.call(c, output) #Its actually better that the images are in 
  #separate lists in the output 
  return(output)
  
}

#> Function to neatly grab all of the relevant graphs for each model 
get_model_imgs <- function(model_name, img_list, render = FALSE) {
  # Flatten the nested list into one character vector
  all_imgs <- unlist(img_list, use.names = FALSE)
  
  # Keep only those image strings that contain the model shorthand
  matches <- grep(paste0("_", model_name, "(_|\\.)"), all_imgs, value = TRUE)
  
  # Order pattern
  order_pattern <- c("apa_table", "apa_lmer_random", "summary", "comparison", "apa_fed_report")
  ordered <- unlist(lapply(order_pattern, function(p) grep(p, matches, value = TRUE)))
  
  if (render) {
    cat(ordered, sep = "\n\n")
    return(invisible(NULL))
  } else {
    return(ordered)
  }
}

format_p <- function(p, digits = 3) {
  ifelse(p < .001, "< .001",
         sub("^0", "", sprintf(paste0("%.", digits, "f"), p)))
}

tidy_t.test <- function(formulas, data, nice_names = NULL) {
  
  output <- vector("list", length(formulas))
  
  for (i in seq_along(formulas)) {
    f <- as.formula(formulas[i])
    res <- t.test(f, data = data)
    
    # Variable and group names
    var_name   <- all.vars(f)[1]
    group_name <- all.vars(f)[2]
    
    # Extract group means and SDs
    means <- as.numeric(res$estimate)
    sds   <- tapply(data[[var_name]], data[[group_name]], sd, na.rm = TRUE)
    
    # Format test(df)
    test_label <- sprintf("t(%.0f)", res$parameter)
    
    # Build row
    output[[i]] <- data.frame(
      Variable  = var_name,
      Study1    = sprintf("%.2f (%.2f)", means[1], sds[1]),
      Study2    = sprintf("%.2f (%.2f)", means[2], sds[2]),
      test_df   = test_label,
      Statistic = sprintf("%.2f", res$statistic),
      p         = format_p(res$p.value),
      stringsAsFactors = FALSE
    )
  }
  
  output <- do.call(rbind, output)
  
  if(!is.null(nice_names)) output <- rename_rows(output, rename_vector = nice_names)
  
  rownames(output) <- NULL
  return(output)
}

tidy_prop.test <- function(formulas, data, nice_names = NULL) {
  
  output <- vector("list", length(formulas))
  
  for (i in seq_along(formulas)) {
    f <- as.formula(formulas[i])
    
    # Extract variable names
    var_name   <- all.vars(f)[1] #grabs the first variable 
    group_name <- all.vars(f)[2] #grabs the second variable 
    
    # Build 2x2 table
    tab <- table(data[[group_name]], data[[var_name]])
    
    # Run proportion test (assumes 1 = "success")
    res <- prop.test(x = tab[, "1"], n = rowSums(tab), correct = FALSE)
    
    # Extract proportions per group (study)
    props <- as.numeric(res$estimate)
    
    # Compute z from chi-square
    z_val <- sqrt(res$statistic)
    
    # Format test(df)
    test_label <- "z"
    
    # Build row
    output[[i]] <- data.frame(
      Variable  = var_name,
      Study1    = sprintf("%.1f%%", props[1] * 100),
      Study2    = sprintf("%.1f%%", props[2] * 100),
      test_df   = test_label,
      Statistic = sprintf("%.2f", z_val),
      p         = format_p(res$p.value),
      stringsAsFactors = FALSE
    )
  }
  
  output <- do.call(rbind, output)
  
  if(!is.null(nice_names)) output <- rename_rows(output, rename_vector = nice_names)
  
  rownames(output) <- NULL
  return(output)
}



combine_tidy_tests <- function(data, 
                               t_formulas = NULL, 
                               p_formulas = NULL, 
                               nice_names = NULL) {
  
  # Run t-tests if any
  tbl_t <- if (!is.null(t_formulas)) {
    tidy_t.test(t_formulas, data = data, nice_names = nice_names)
  } else NULL
  
  # Run prop-tests if any
  tbl_p <- if (!is.null(p_formulas)) {
    tidy_prop.test(p_formulas, data = data, nice_names = nice_names)
  } else NULL
  
  # Combine
  combined <- rbind(tbl_t, tbl_p)
  rownames(combined) <- NULL
  combined
}

my_mlr <- function(data, grp, items, time, 
                   lme = TRUE, 
                   lmer = TRUE, 
                   aov = FALSE) {
  
  
  if(length(items) == 1) {data[["items_long"]] <- items
  data[["values_long"]] <- data[[items]]
  #filter data so there are at least two observations for
  #everyone 
  data <- data |> group_by(.data[[grp]]) |> 
    mutate(
      n = n(), #number of observations for each participant
      na_sum = sum(is.na({{items}})), #number of missing responses for a participant
      remove = n - na_sum
    ) |> 
    filter(remove > 2) |> 
    ungroup()
  #UPDATE LMER OPTION FOR SINGLE ITEM
  lmer <- FALSE
  
  }
  
  if(length(items) > 1) {
    #need this format 
    data <- data |> pivot_longer(cols = all_of(items),
                                 names_to = "items_long",
                                 values_to = "values_long")
  }
  
  output <- multilevel.reliability(
    data,
    grp = grp,
    Time = time,  #I need to provide this. It is study_day 
    items = "items_long", #corresponds to the column name in the longer df
    values = "values_long", #corresponds to the column name in the longer df 
    long = TRUE,
    lme = lme,
    lmer = lmer,
    aov = aov
  )
  
  return(output)
  
}


run_my_mlr <- function(data, grp, measures, time) {
  
  mlr_results <- vector("list", length(measures))
  names(mlr_results) <- names(measures)
  
  error_log <- list()
  
  for (i in seq_along(measures)) {
    
    scale_i <- names(measures[i]) #name of the scale
    items_i <- measures[[i]]
    mlr_i <- tryCatch(
      {
        my_mlr(data = data, grp = grp, items = items_i, time = time)
      },
      error = function(e) {
        message("Error in scale '", scale_i, "': ", e$message)
        e$message
      },
      warning = function(w) {
        message("Warning in scale '", scale_i, "': ", w$message)
        invokeRestart("muffleWarning")
      }
    )
    
    mlr_results[[scale_i]] <-  mlr_i
    
  }
  
  return(mlr_results)
  
}


tidy_mlr <- function(mlr, scale = "scale", rnd = 2) {
  
  
  if(inherits(mlr, "multilevel") && mlr$n.items > 1) {
    
    out <- data.frame(scale = scale,
                      RkF = mlr$RkF,
                      R1R = mlr$R1R,
                      RkR = mlr$RkR,
                      Rc = mlr$Rc,
                      RkRn = mlr$RkRn,
                      Rcn = mlr$Rcn
    )
  }   
  
  if(inherits(mlr, "multilevel") && mlr$n.items == 1) {
    
    out <- data.frame(scale = scale,
                      RkF = NA_integer_,
                      R1R = NA_integer_,
                      RkR = NA_integer_,
                      Rc = NA_integer_,
                      RkRn = mlr$RkRn,
                      Rcn = mlr$Rcn
    )
  }  
  
  out[] <- lapply(out, function(x) if(is.numeric(x)) round(x, digits=rnd) else(x))
  
  return(out)
  
}

run_tidy_mlr <- function(mlr_results) {
  
  
  tidy_mlr_out <- vector("list")  
  
  for(i in seq_along(mlr_results)) {
    
    
    if(inherits(mlr_results[[i]], "multilevel")) {
      
      tidy_mlr_i <- tidy_mlr(mlr_results[[i]], scale = names(mlr_results[i]))
      
      tidy_mlr_out[[names(mlr_results[i])]] <- tidy_mlr_i
    } 
    
    
  }
  
  tidy_mlr_out <- do.call(rbind, tidy_mlr_out)
  return(tidy_mlr_out)
  
}

reverse_code_items <- function(df, 
                               items, 
                               min_val = 1, 
                               max_val = 5, 
                               rename = c("overwrite", "prefix", "suffix"),
                               rename_str = "rev_",
                               verbose = TRUE) {
  # Match rename argument
  rename <- match.arg(rename)
  
  # Check that all items exist
  missing_cols <- setdiff(items, names(df))
  if (length(missing_cols)) {
    stop("These items are not in the dataset: ", paste(missing_cols, collapse = ", "))
  }
  
  # Compute reversed values
  reversed_values <- lapply(items, function(col) (max_val + min_val) - df[[col]])
  names(reversed_values) <- items
  
  # Handle naming behavior
  if (rename == "overwrite") {
    for (col in items) df[[col]] <- reversed_values[[col]]
  } else if (rename == "prefix") {
    for (col in items) df[[paste0(rename_str, col)]] <- reversed_values[[col]]
  } else if (rename == "suffix") {
    for (col in items) df[[paste0(col, rename_str)]] <- reversed_values[[col]]
  }
  
  # Verbose output
  if (verbose) {
    new_names <- switch(rename,
                        overwrite = items,
                        prefix = paste0(rename_str, items),
                        suffix = paste0(items, rename_str)
    )
    message(sprintf(
      "Reversed %d items (%s) into %s using a %d–%d scale.",
      length(items),
      paste(items, collapse = ", "),
      paste(new_names, collapse = ", "),
      min_val, max_val
    ))
  }
  
  return(df)
}

calc_sum_scale <- function(df, items, scale_name, 
                           rev_items = NULL,     
                           min_val = 1, max_val = 5, 
                           na_thresh = .2,       
                           make_miss = TRUE,
                           verbose = TRUE) {
  
  # --- (1) Make a temporary copy of the relevant columns ---
  tmp <- df[, items, drop = FALSE]
  
  # --- (2) Reverse-code within the temp data only ---
  if (!is.null(rev_items)) {
    tmp <- reverse_code_items(
      tmp,
      items = rev_items,
      min_val = min_val,
      max_val = max_val,
      rename = "overwrite",
      verbose = verbose
    )
  }
  
  # --- (3) Check columns still exist (after reverse coding) ---
  missing_cols <- setdiff(items, names(tmp))
  if (length(missing_cols)) {
    stop("These items are not in the dataset: ", paste(missing_cols, collapse = ", "))
  }
  
  # --- (4) Compute missingness and scale using temp data ---
  n_items <- length(items)
  df$n_miss <- rowSums(is.na(tmp))
  df$prop_miss <- df$n_miss / n_items
  df[[scale_name]] <- rowSums(tmp, na.rm = TRUE)
  
  # --- (5) Apply missing threshold ---
  if (make_miss) {
    df[df$prop_miss > na_thresh, scale_name] <- NA
  }
  
  # --- (6) Verbose report ---
  if (verbose) {
    n_na <- sum(is.na(df[[scale_name]]))
    message(sprintf(
      "%s: %d rows (%.1f%%) set to NA due to > %.0f%% missingness.",
      scale_name, n_na, 100 * n_na / nrow(df), 100 * na_thresh
    ))
  }
  
  # --- (7) Cleanup helper cols and return original df ---
  df$n_miss <- NULL
  df$prop_miss <- NULL
  return(df)
}

# df <- calc_sum_scale(df, s_lovattach_scale_items, "s_lovattach_scale",
#                      na_thresh = 0.2,
#                      rev_items = "spsi27")
# 
# s_lovattach_scale_items <- colnames(df[, grepl("(^sbond[1-7]$|^srq4$|^spsi(1$|22$|27$))", names(df))]) 



# p_lovattach_scale_items <- gsub("^s", "p", s_lovattach_scale_items)  
# p_lovattach_scale_items
# 
# df <- calc_sum_scale(df, p_lovattach_scale_items, "p_lovattach_scale",
#                      na_thresh = 0.2,
#                      rev_items = "ppsi27")

# # Self-report passion scale (reverse spsi19, spsi24)
# df <- calc_sum_scale(
#   df,
#   items = s_passion_scale_items,
#   scale_name = "s_passion_scale",
#   rev_items = c("spsi19", "spsi24"),
#   min_val = 1,
#   max_val = 5,
#   na_thresh = 0.2
# )
# 
# # Partner-report passion scale (reverse ppsi19, ppsi24)
# df <- calc_sum_scale(
#   df,
#   items = p_passion_scale_items,
#   scale_name = "p_passion_scale",
#   rev_items = c("ppsi19", "ppsi24"),
#   min_val = 1,
#   max_val = 5,
#   na_thresh = 0.2
# )
# 
# scale_specs <- list(
#   
#   # --- Love & Attachment ---
#   s_lovattach = list(
#     items = colnames(df[, grepl("(^sbond[1-7]$|^srq4$|^spsi(1$|22$|27$))", names(df))]),
#     rev_items = "spsi27",
#     scale_name = "s_lovattach_scale"
#   ),
#   p_lovattach = list(
#     items = gsub("^s", "p", colnames(df[, grepl("(^sbond[1-7]$|^srq4$|^spsi(1$|22$|27$))", names(df))])),
#     rev_items = "ppsi27",
#     scale_name = "p_lovattach_scale"
#   ),
#   
#   # --- Passion ---
#   s_passion = list(
#     items = c("spsi3", "spsi13", "spsi16", "spsi19", "spsi23", "spsi24",
#               "srq1", "srq2", "srq3"),
#     rev_items = c("spsi19", "spsi24"),
#     scale_name = "s_passion_scale"
#   ),
#   p_passion = list(
#     items = c("ppsi3", "ppsi13", "ppsi16", "ppsi19", "ppsi23", "ppsi24",
#               "prq1", "prq2", "prq3"),
#     rev_items = c("ppsi19", "ppsi24"),
#     scale_name = "p_passion_scale"
#   ),
#   
#   # --- Antagonism/Trust ---
#   s_antagtrust = list(
#     items = c("spsi2", "spsi5", "spsi6", "spsi11", "spsi17", "spsi20",
#               "spsi21", "spsi25", "spsi26", "spsi28", "sresp1", "sresp2"),
#     rev_items = c("spsi2", "spsi5", "spsi17", "spsi21", "spsi25", "spsi28"),
#     scale_name = "s_antagtrust_scale"
#   ),
#   p_antagtrust = list(
#     items = c("ppsi2", "ppsi5", "ppsi6", "ppsi11", "ppsi17", "ppsi20",
#               "ppsi21", "ppsi25", "ppsi26", "ppsi28", "presp1", "presp2"),
#     rev_items = c("ppsi2", "ppsi5", "ppsi17", "ppsi21", "ppsi25", "ppsi28"),
#     scale_name = "p_antagtrust_scale"
#   ),
#   
#   # --- Sexual Exclusivity ---
#   s_sexexcl = list(
#     items = c("spsi4", "spsi7", "spsi10", "spsi12", "spsi14", "spsi15", "spsi18"),
#     rev_items = c("spsi4", "spsi7", "spsi10", "spsi12", "spsi14", "spsi15", "spsi18"),
#     scale_name = "s_sexexcl_scale"
#   ),
#   p_sexexcl = list(
#     items = c("ppsi4", "ppsi7", "ppsi10", "ppsi12", "ppsi14", "ppsi15", "ppsi18"),
#     rev_items = c("ppsi4", "ppsi7", "ppsi10", "ppsi12", "ppsi14", "ppsi15", "ppsi18"),
#     scale_name = "p_sexexcl_scale"
#   )
# )
# 
# for (spec in scale_specs) {
#   df <- calc_sum_scale(
#     df,
#     items = spec$items,
#     rev_items = spec$rev_items,
#     scale_name = spec$scale_name,
#     min_val = 1,
#     max_val = 7,
#     na_thresh = 0.2,
#     make_miss = TRUE,
#     verbose = TRUE
#   )
# }

refit_model <- function(mod) {
  form <- formula(mod)
  dat  <- mod@frame
  
  model <- lmerTest::lmer(
    form,
    data = dat,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 200000)
    )
  )
  return(model)
}


apa_lmer_random <- function(model,
                            nice_names = NULL,
                            bold_title = "Table",
                            italics_title = "Random Effects",
                            table_note = "",
                            font_size = 10,
                            font = "Times New Roman",
                            digits = 3) {
  stopifnot(inherits(model, "lmerMod") || inherits(model, "lmerModLmerTest"))
  
  clean_text <- function(x) {
    x <- gsub("(?<!\n)\n(?!\n)", " ", x, perl = TRUE)
    x <- gsub(" {2,}", " ", x)
    trimws(x)
  }
  
  vc_list <- VarCorr(model)
  groups  <- names(vc_list)
  
  # Check if ANY grouping factor has correlations
  any_correlated <- any(vapply(vc_list, function(vg) {
    cor_mat <- attr(vg, "correlation")
    !is.null(cor_mat) && any(!is.na(cor_mat[lower.tri(cor_mat)]))
  }, logical(1)))
  
  # --- If all are uncorrelated: collapse into one simple SD table ---
  if (!any_correlated) {
    df_all <- data.frame()
    for (grp in groups) {
      vg <- vc_list[[grp]]
      sds <- as.numeric(attr(vg, "stddev"))
      terms_raw <- names(attr(vg, "stddev"))
      terms <- terms_raw
      
      if (!is.null(nice_names) && exists("rename_rows", mode = "function")) {
        tmp <- data.frame(Term = terms, stringsAsFactors = FALSE)
        tmp <- rename_rows(tmp, rename_vec = nice_names)
        terms <- tmp$Term
      }
      
      tmp_df <- data.frame(
        Term = terms,
        SD   = sprintf(paste0("%.", digits, "f"), sds),
        check.names = FALSE
      )
      df_all <- rbind(df_all, tmp_df)
    }
    
    # Add residual SD
    res_sd <- sigma(model)
    df_all <- rbind(df_all,
                    data.frame(Term = "Residual",
                               SD = sprintf(paste0("%.", digits, "f"), res_sd),
                               check.names = FALSE))
    
    # --- Single flextable ---
    ft <- flextable::flextable(df_all)
    italics_title_clean <- clean_text(italics_title)
    bold_title_clean    <- clean_text(bold_title)
    
    ft <- flextable::add_header_row(ft, values = italics_title_clean,
                                    colwidths = rep(ncol(df_all), 1))
    ft <- flextable::add_header_row(ft, values = bold_title_clean,
                                    colwidths = rep(ncol(df_all), 1))
    ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df_all), part = "header")
    ft <- flextable::merge_at(ft, i = 2, j = 1:ncol(df_all), part = "header")
    ft <- flextable::align(ft, i = 1:2, j = 1, part = "header", align = "left")
    ft <- flextable::bold(ft, i = 1, part = "header", bold = TRUE)
    ft <- flextable::italic(ft, i = 2, part = "header", italic = TRUE)
    ft <- flextable::align(ft, j = 1, align = "left", part = "body")
    ft <- flextable::align(ft, j = 2, align = "right", part = "body")
    ft <- flextable::border_remove(ft)
    std_border <- officer::fp_border(color = "black", width = 1)
    ft <- flextable::hline(ft, i = 2, border = std_border, part = "header")
    ft <- flextable::hline(ft, i = 3, border = std_border, part = "header")
    ft <- flextable::hline_bottom(ft, border = std_border, part = "body")
    ft <- flextable::font(ft, fontname = font, part = "all")
    ft <- flextable::fontsize(ft, size = font_size, part = "all")
    ft <- flextable::width(ft, width = rep(1.4, ncol(df_all)))  # widen all cols
    ft <- flextable::set_table_properties(ft, layout = "autofit", width = 0.9)
    ft <- flextable::italic(ft, j = "SD", part = "header", italic = TRUE)
    
    auto_note <- "Values represent standard deviations of the random effects and residual.
    Random effects were specified as uncorrelated."
    full_note <- paste0(auto_note, if (nzchar(table_note)) paste0(" ", table_note) else "")
    table_note_clean <- clean_text(full_note)
    
    ft <- flextable::add_footer_lines(ft, values = rep("", ncol(df_all)))
    ft <- flextable::compose(
      ft, i = 1, j = 1, part = "footer",
      value = flextable::as_paragraph(flextable::as_i("Note. "), table_note_clean)
    )
    ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df_all), part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
    ft <- flextable::fontsize(ft, part = "footer", size = font_size)
    return(list(ft))
  }
  
  # --- Otherwise: correlated case, use per-group tables ---
  make_group_table <- function(grp, vc) {
    vg        <- vc[[grp]]
    sds       <- as.numeric(attr(vg, "stddev"))
    terms_raw <- names(attr(vg, "stddev"))
    n         <- length(sds)
    
    terms <- terms_raw
    if (!is.null(nice_names) && exists("rename_rows", mode = "function")) {
      tmp <- data.frame(Term = terms, stringsAsFactors = FALSE)
      tmp <- rename_rows(tmp, rename_vec = nice_names)
      terms <- tmp$Term
    }
    
    cor_mat <- attr(vg, "correlation")
    disp <- matrix("", nrow = n, ncol = n, dimnames = list(terms, terms))
    storage.mode(disp) <- "character"
    diag(disp) <- sprintf(paste0("%.", digits, "f"), sds)
    
    corsm <- as.matrix(cor_mat)
    for (i in 2:n) for (j in 1:(i - 1))
      disp[i, j] <- sprintf(paste0("%.", digits, "f"), corsm[i, j])
    
    df <- data.frame(
      Term = rownames(disp),
      as.data.frame(disp, check.names = FALSE, stringsAsFactors = FALSE),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    rownames(df) <- NULL
    
    # add residual row
    res_sd <- sigma(model)
    res_row <- as.list(rep("", ncol(df)))
    names(res_row) <- names(df)
    res_row$Term <- "Residual"
    res_row[[2]] <- sprintf(paste0("%.", digits, "f"), res_sd)
    df <- rbind(df, res_row)
    
    # format flextable
    ft <- flextable::flextable(df)
    italics_title_clean <- clean_text(italics_title)
    bold_title_clean    <- clean_text(bold_title)
    ft <- flextable::add_header_row(ft, values = italics_title_clean,
                                    colwidths = rep(ncol(df), 1))
    ft <- flextable::add_header_row(ft,
                                    values = paste0(bold_title_clean),
                                    colwidths = rep(ncol(df), 1))
    ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df), part = "header")
    ft <- flextable::merge_at(ft, i = 2, j = 1:ncol(df), part = "header")
    ft <- flextable::align(ft, i = 1:2, j = 1, part = "header", align = "left")
    ft <- flextable::bold(ft, i = 1, part = "header", bold = TRUE)
    ft <- flextable::italic(ft, i = 2, part = "header", italic = TRUE)
    ft <- flextable::align(ft, j = 1, align = "left", part = "body")
    ft <- flextable::align(ft, j = 2:ncol(df), align = "right", part = "body")
    ft <- flextable::border_remove(ft)
    std_border <- officer::fp_border(color = "black", width = 1)
    ft <- flextable::hline(ft, i = 2, border = std_border, part = "header")
    ft <- flextable::hline(ft, i = 3, border = std_border, part = "header")
    ft <- flextable::hline_bottom(ft, border = std_border, part = "body")
    ft <- flextable::fontsize(ft, size = font_size, part = "all")
    ft <- flextable::width(ft, width = rep(1.6, ncol(df)))  # widen all cols
    ft <- flextable::set_table_properties(ft, layout = "autofit", width = 0.9)
    #ft <- flextable::set_table_properties(ft, layout = "autofit", width = 0.76)
    
    auto_note <- "Values on the diagonal are standard deviations of the random effects; 
    values below the diagonal are correlations among random effects."
    full_note <- paste0(auto_note, if (nzchar(table_note)) paste0(" ", table_note) else "")
    table_note_clean <- clean_text(full_note)
    
    ft <- flextable::add_footer_lines(ft, values = rep("", ncol(df)))
    ft <- flextable::compose(
      ft, i = 1, j = 1, part = "footer",
      value = flextable::as_paragraph(flextable::as_i("Note. "), table_note_clean)
    )
    ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df), part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
    ft <- flextable::fontsize(ft, part = "footer", size = font_size)
    ft <- flextable::font(ft, fontname = font, part = "all")
    ft
  }
  
  lapply(groups, make_group_table, vc = vc_list)
}


# Run random-effects tables for each model in a list
run_apa_lmer_random <- function(model_list,
                                model_path = NULL,     # e.g., "results$model" or NULL if the element IS the model
                                nice_names = NULL, 
                                title_prefix = "Random Effects: ", #fills in the model shorthand name 
                                bold_title = "",
                                font = "Times New Roman",
                                font_size = 11,
                                report_name = "apa_lmer_random",
                                ...) {                 # pass-through to apa_lmer_random()
  # safe accessor (matches your fed helper)
  safe_pluck <- function(x, path) {
    if (is.null(path) || !nzchar(path)) return(x)
    parts <- strsplit(path, "\\$")[[1]]
    Reduce(function(acc, nm) acc[[nm]], parts, init = x)
  }
  
  for (i in seq_along(model_list)) {
    model_name <- names(model_list)[i]
    model_obj  <- safe_pluck(model_list[[i]], model_path)
    
    if (!inherits(model_obj, c("lmerMod", "lmerModLmerTest"))) {
      message(sprintf("Skipping %s: object at '%s' is not an lmer model.",
                      model_name, ifelse(is.null(model_path), "<root>", model_path)))
      next
    }
    
    # Build titles once
    italics_i <- paste0(title_prefix, model_name)
    
    # Call your table-maker, forwarding extras
    tbl <- apa_lmer_random(
      model = model_obj,
      nice_names = nice_names,
      bold_title   = "",           # you’ve been using empty bold + italics as title line
      italics_title = italics_i,
      font = font,
      font_size = font_size, 
      ...
    )
    
    # Attach back onto the model entry
    model_list[[i]][[report_name]] <- tbl
  }
  
  model_list
}


# Save the random-effects tables to disk
save_apa_lmer_random <- function(model_list,
                                 directory,
                                 report_path = "apa_lmer_random",
                                 prefix = "apa_lmer_random",
                                 create_subfolder = TRUE) {
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  
  if (isTRUE(create_subfolder)) {
    directory <- file.path(directory, prefix)
  } else if (is.character(create_subfolder)) {
    directory <- file.path(directory, create_subfolder)
  }
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  
  for (i in seq_along(model_list)) {
    model_name <- names(model_list)[i]
    report <- model_list[[i]][[report_path]]
    
    if (is.null(report)) {
      message(sprintf("Skipping %s: no '%s' report found.", model_name, report_path))
      next
    }
    
    # Single flextable
    if (inherits(report, "flextable")) {
      out <- file.path(directory, paste0(prefix, "_", model_name, ".png"))
      flextable::save_as_image(report, path = out)
      next
    }
    
    # List of flextables (e.g., per-group correlated RE)
    if (is.list(report)) {
      idx <- 1L
      for (ft in report) {
        if (!inherits(ft, "flextable")) next
        out <- file.path(directory, paste0(prefix, "_", model_name, "_", idx, ".png"))
        flextable::save_as_image(ft, path = out)
        idx <- idx + 1L
      }
    }
  }
  
  message(sprintf("Saved APA random-effect tables to: %s", normalizePath(directory)))
}



apa_lmer_model1 <- function(model, data,
                           nice_names = NULL,
                           bold_title = "Table",
                           italics_title = "",
                           table_note = "",
                           font_size = 10,
                           font = "Times New Roman",
                           effects_to_bold = NULL,
                           sig_level = .05,
                           extra_note_info = TRUE,
                           reorder_predictors = TRUE) {
  # ---- 1. Extract and rename columns ----
  df_out <- rename_lmer(model, data, rename_vec = nice_names)
  df_out <- df_out[, c("var_star", "Estimate", "Std. Error", "df",
                       "t value", "Pr(>|t|)", "2.5 %", "97.5 %")]
  names(df_out) <- c("Predictor", "Estimate", "SE", "df", "t", "p", "conf.low", "conf.high")
  
  # Keep numeric p for later significance check
  p_numeric <- df_out$p
  
  # ---- 2. Format before table ----
  df_out$Estimate <- sprintf("%.2f", df_out$Estimate)
  df_out$t <- sprintf("%.2f", df_out$t)
  df_out$df <- sprintf("%.2f", df_out$df)
  df_out$CI <- sprintf("[%.2f, %.2f]", df_out$conf.low, df_out$conf.high)
  
  df_out$p <- ifelse(p_numeric < .001, "< .001", sprintf("%.3f", p_numeric))
  df_out$p <- sub("^0\\.", ".", df_out$p)  # remove leading zeros
  
  # Keep only desired columns
  df_out <- df_out[, c("Predictor", "Estimate", "CI", "t", "df", "p")]
  
  # ---- 2b. Reorder predictors: main -> 2-way -> 3-way ----
  if (reorder_predictors) {
    # count "*" per predictor (0 = main effect, 1 = two-way, 2+ = higher)
    m <- gregexpr("\\*", df_out$Predictor, perl = TRUE)
    star_counts <- lengths(regmatches(df_out$Predictor, m))  # 0 for no matches
    
    ord <- order(star_counts)
    df_out <- df_out[ord, ]
    p_numeric <- p_numeric[ord]  # keep alignment for bolding significant rows
  }
  
  # ---- 3. Build flextable ----
  ft <- flextable::flextable(df_out)
  
  #> Ensuring nice printing of names:
  # Step 1: replace single newlines (not double) with a space
  italics_title_clean <- gsub("(?<!\n)\n(?!\n)", " ", italics_title, perl = TRUE)
  # Step 2: collapse multiple spaces into one
  italics_title_clean <- gsub(" {2,}", " ", italics_title_clean)
  # Step 3: trim leading/trailing whitespace
  italics_title_clean <- trimws(italics_title_clean)
  
  # Add table title rows
  ft <- flextable::add_header_row(
    ft,
    values = italics_title_clean,
    colwidths =  rep(ncol(df_out), 1)
  )
  
  # Step 1: replace single newlines (not double) with a space
  bold_title_clean <- gsub("(?<!\n)\n(?!\n)", " ", bold_title, perl = TRUE)
  # Step 2: collapse multiple spaces into one
  bold_title_clean <- gsub(" {2,}", " ", bold_title_clean)
  # Step 3: trim leading/trailing whitespace
  bold_title_clean <- trimws(bold_title_clean)
  
  ft <- flextable::add_header_row(
    ft,
    values = bold_title_clean,
    colwidths =  rep(ncol(df_out), 1)
  )
  # Merge each header row across all columns
  ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df_out), part = "header")
  ft <- flextable::merge_at(ft, i = 2, j = 1:ncol(df_out), part = "header")
  
  # APA style: left-align both
  ft <- flextable::align(ft, i = 1:2, j = 1, part = "header", align = "left")
  
  #Bold Table Header
  ft <- flextable::bold(ft, i = 1, part = "header", bold = TRUE)
  #Make Title italics
  ft <- flextable::italic(ft, i = 2, part = "header", italic = TRUE)
  
  
  # ---- 4. Styling header columns of what values are ----
  ft <- flextable::align(ft, j = "Predictor", align = "left", part = "body")
  ft <- flextable::align(ft, j = c("Estimate", "CI", "t", "df", "p"), align = "right", part = "body")
  ft <- flextable::align(ft, i = 3, align = "center", part = "header")
  ft <- flextable::italic(ft, i = 3, part = "header", italic = TRUE)
  
  # Horizontal rules
  ft <- flextable::border_remove(ft)
  std_border <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::hline(ft, i = 2, border = std_border, part = "header")
  ft <- flextable::hline(ft, i = 3, border = std_border, part = "header")
  ft <- flextable::hline_bottom(ft, border = std_border, part = "body")
  
  # Bold significant rows
  sig_rows <- which(p_numeric < sig_level)
  if (length(sig_rows) > 0) {
    ft <- flextable::bold(ft, i = sig_rows, part = "body", bold = TRUE)
  }
  
  
  # ---- 4b. Bold specific effects manually ----
  if (!is.null(effects_to_bold) && length(effects_to_bold) > 0) {
    
    effects_to_bold <- as.character(effects_to_bold)
    
    # Helper function to split a term into clean component names
    split_terms <- function(x) {
      strsplit(x, "\\s*[:\\*xX]\\s*")[[1]] |> trimws()
    }
    
    # Prepare a normalized version of Predictors in the table
    pred_components <- lapply(df_out$Predictor, split_terms)
    
    custom_rows <- integer(0)
    
    for (term in effects_to_bold) {
      term_parts <- split_terms(term)
      
      # Match predictors with the same set of components (order-insensitive)
      matches <- sapply(pred_components, function(p) setequal(p, term_parts))
      custom_rows <- c(custom_rows, which(matches))
    }
    
    custom_rows <- unique(custom_rows)
    
    if (length(custom_rows) == 0) {
      message("No matches found for 'effects_to_bold' in Predictor column.")
    } else {
      ft <- flextable::bold(ft, i = custom_rows, part = "body", bold = TRUE)
    }
  }
  
  
  ft <- flextable::autofit(ft)
  
  # ---- 5. Add table note ----
  
  
  clean_text <- function(x) {
    x <- gsub("(?<!\n)\n(?!\n)", " ", x, perl = TRUE)
    x <- gsub(" {2,}", " ", x)
    trimws(x)
  }
  table_note <- clean_text(table_note)
  
  if (extra_note_info == TRUE) {
    
    # --- ICCs ---
    model_icc <- performance::icc(model)
    
    # --- R2 values ---
    model_r2 <- performance::r2(model)
    r2_cond <- round(model_r2$R2_conditional, 2)
    r2_marg <- round(model_r2$R2_marginal, 2)
    
    # --- Observation and group info ---
    n <- lme4::getME(model, "N")
    ng <- lme4::ngrps(model)
    
    obs_note <- paste0("Number of Observations: ", n)
    gr_note  <- paste0("Number of Groups: ", ng)
    obs_gr_note <- paste0(obs_note, "; ", gr_note)
    
    # --- ICC section ---
    icc_adj_note   <- paste0("Adjusted ICC: ", round(model_icc$ICC_adjusted, 2))
    icc_unadj_note <- paste0("Unadjusted ICC: ", round(model_icc$ICC_unadjusted, 2))
    icc_notes <- paste0(icc_adj_note, "; ", icc_unadj_note)
    
    # --- R² section ---
    r2_cond_note <- paste0("Conditional R2: ", r2_cond)
    r2_marg_note <- paste0("Marginal R2: ", r2_marg)
    r2_notes <- paste0(r2_cond_note, "; ", r2_marg_note)
    
    # --- Reference note ---
    icc_calc_note <- paste0(
      "ICC and R2 calculated using icc() and r2() from the performance package; ",
      "see package documentation and Nakagawa et al. (2017) for calculation details."
    )
    
    # --- Combine all notes ---
    table_note <- paste0(
      table_note, "\n",
      obs_gr_note, "\n",
      icc_notes, "\n",
      r2_notes, "\n",
      icc_calc_note
    )
  }
  
  
  
  ft <- flextable::add_footer_lines(ft, values = rep("", ncol(df_out)))
  ft <- flextable::compose(
    ft, i = 1, j = 1, part = "footer",
    value = flextable::as_paragraph(
      flextable::as_i("Note. "),
      table_note
    )
  )
  
  
  ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df_out), part = "footer")
  ft <- flextable::align(ft, align = "left", part = "footer")
  ft <- flextable::fontsize(ft, part = "footer", size = font_size)
  
  # ---- 6. Font & size ----
  ft <- flextable::set_table_properties(ft, layout = "autofit")
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  
  
  # (optional) Limit the total table width if needed (e.g., 6.5 inches for APA margins)
  ft <- flextable::set_table_properties(ft,
                                        layout = "autofit",
                                        width = .76) #proportion of page width.
  #.76 x 8.5 = 6.46
  
  return(ft)
}



run_apa_lmer_model1 <- function(model_list,
                               data = NULL,
                               data_path = "data",
                               nice_names = NULL,
                               model_path = NULL,
                               bold_title = "Table",
                               italics_title = "",
                               table_note = "",
                               font_size = 10,
                               font = "Times New Roman",
                               effects_to_bold = NULL,
                               sig_level = .05,
                               extra_note_info = TRUE,
                               reorder_predictors = TRUE
) {
  
  safe_pluck <- function(x, path) {
    Reduce(function(acc, nm) acc[[nm]], path, init = x)
  }
  
  if(is.null(model_path)) {
    stop("The path where the model is stored in the model list must be
                  supplied")
  }
  
  
  for (i in seq_along(model_list)) {
    
    
    italics_title_i <- model_list[[i]]$name
    
    #> If the bold title contains a period (e.g., "Table 3.")
    #> then paste the current number without a space, otherwise, paste the current
    #> number with a space
    if(grepl("\\.", bold_title)) {
      bold_title_i <- paste0(bold_title, i)} else {
        bold_title_i <- paste(bold_title, i)
      }
    
    
    # grab mlm_comparison history object dynamically
    model_i <- safe_pluck(model_list[[i]], model_path)
    
    if (is.null(data)) { #if I don't supply a df that is used across models,
      #then use the path that must be specified
      data <- safe_pluck(model_list[[i]], data_path)
      data <- get(data, envir = parent.frame())
    }
    
    table_i <- apa_lmer_model(
      model = model_i,
      data = data,
      nice_names = nice_names,
      bold_title = bold_title_i,
      italics_title = italics_title_i,
      table_note = table_note,
      font = font,
      font_size = font_size,
      effects_to_bold = effects_to_bold,
      sig_level = sig_level,
      extra_note_info = extra_note_info,
      reorder_predictors = reorder_predictors
    )
    
    model_list[[i]]$apa_table <- table_i
    
  }
  
  return(model_list)
  
}

save_apa_lmer_tables1 <- function(model_list,
                                 directory,
                                 apa_table_path = "apa_table",
                                 prefix = "apa_table",
                                 create_subfolder = TRUE) {
  
  safe_pluck <- function(x, path) {
    parts <- strsplit(path, "\\$")[[1]]
    for (nm in parts) {
      if (is.null(x)) break
      x <- x[[nm]]
    }
    x
  }
  
  # ---- Handle folder creation ----
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  
  # Optional subfolder behavior
  if (isTRUE(create_subfolder)) {
    directory <- file.path(directory, prefix)
  } else if (is.character(create_subfolder)) {
    # user provided a custom folder name
    directory <- file.path(directory, create_subfolder)
  }
  
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  
  # ---- Loop through model list ----
  for (i in seq_along(model_list)) {
    model_name <- names(model_list)[i]
    table_i <- safe_pluck(model_list[[i]], apa_table_path)
    
    # Skip if no table found
    if (is.null(table_i)) {
      message(sprintf("Skipping %s: no APA table found at '%s'.",
                      model_name, apa_table_path))
      next
    }
    
    # Construct filename
    file_name <- file.path(directory,
                           paste0(prefix, "_", model_name, ".png"))
    
    # Save table image
    flextable::save_as_image(table_i, path = file_name)
  }
  
  message(sprintf("Saved APA tables to: %s", normalizePath(directory)))
  invisible(model_list)
}