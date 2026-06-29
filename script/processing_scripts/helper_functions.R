


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