
#> Clear up environment 
rm(list = ls())

#> Helpful if I am working with many .R files and want to keep my environment 
#> clean and avoid overwriting specific functions when running new documents. 
# Detach all packages and load in only packages I need
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods",
                      "package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,
                                  TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
rm(detachAllPackages)

library(tidyverse)
# library(labelled) #for spss labels 
library(codechest) #convenience functions 
# library(haven)
library(psych)
library(lme4)
library(lmerTest)
# library(broom)
# library(broom.mixed)
library(performance) # computes icc 
library(varTestnlme) #used for LRTs involving variances of random effects 
library(questionr)
library(ggtext) #for some nicer text on ggplots 
library(emmeans) #interaction simple effects analyses 
# library(kableExtra)
# library(gt)
library(flextable)
library(ggeffects) #for simple effects graphs
library(patchwork) #for pulling together forest plots as desired 

#load in the functions
source("../../collab_chest/helper_functions.R")

#load in model list that specifies all of the models
source("script/analysis_scripts/part_sex_attract_model_list.R")


#> Selecting variables for the present analysis for open data sharing ---------

#> NOTE: The below code was commented out after it was run. 

# # #> Read in original dataframe before filtering to variables to share 
df <- readRDS("data/analysis_data/prolif_1_and_2_filtered.rds")

cor(df$IPinterest, df$p_lovattach_scale, use = "complete")
cor(df$IPinterest, df$s_lovattach_scale, use = "complete")



# # 
# keep_variables <- c(
# 
#   # Identifiers / grouping
#   "PROLIFIC_PID", #> NOTE: this variable was not anonymized until just prior to
#   #sharing the data. Because of this, the variable name was used throughout the
#   #code widely and it would have been a pain to update all of the code to make
#   #that change. Thus, the solution was to overwrite this original variable name
#   #with the anonymized version. The creation of the anonymized variable is documented
#   #in the individual prolific 1 and prolific 2 files.
#   "study",
#   "Zstudy",
#   "studyday",
# 
#   # Menstrual / cycle variables
#   "menses",
# 
#   # Sexual behavior outcomes
#   "sex1",
#   "sex2init",
#   "sex3Pinit",
#   "sex4reject",
#   "Zsex1",
#   "Zsex2init",
#   "Zsex3Pinit",
#   "Zsex4reject",
# 
#   # General sexual desire
#   "IP1",
#   "ZIP1",
# 
#   #> Extra-pair items
#   "EP1", "EP2",
# 
#   # Extra-pair outcomes (modeled + components)
#   "EPinterest",
#   "EPattract",
#   "ZEPinterest",
#   "ZEPattract",
#   "Zsextoday", #scenario item
# 
#   #> In-pair items
#   "IP2", "IP3",
# 
#   # In-pair outcomes (modeled + components)
#   "IPinterest",
#   "IPattract",
#   "ZIPinterest",
#   "ZIPattract",
# 
#   # Probability of conception
#   "prc_stirn_bc", #backward count
#   "prc_stirn_bctypical", #backward count typical day
#   "prc_stirn_fc", #forward count
#   "prc_stirn", #coalesced backward and forward count
#   "prc_stirn_mean", #between-woman variance
#   "prc_stirn_ww", #within-woman variance
#   "Zprc_stirn",  #Z-scored prc_stirn
#   "Zprc_stirn_ww", #Z-scored prc_stirn_ww
#   "Zprc_stirn_mean", #Z-scored prc_stirn_mean
# 
#   # Estradiol  (log-transformed)
#   "estr_b",
#   "estr_bt",
#   "estr_f",
#   "estr",
#   "estr_mean",
#   "estr_ww",
#   "Zestr_ww",
#   "Zestr_mean",
# 
#   # Progesterone (log-transformed)
#   "prog_b",
#   "prog_bt",
#   "prog_f",
#   "prog",
#   "prog_ww",
#   "prog_mean",
#   "Zprog_ww",
#   "Zprog_mean",
# 
#   # Hormones (raw)
#   "rawestr",
#   "rawprog",
#   "rawestr_ww",
#   "rawprog_ww",
#   "rawestr_mean",
#   "rawprog_mean",
#   "Zrawestr_ww",
#   "Zrawprog_ww",
#   "Zrawestr_mean",
#   "Zrawprog_mean",
# 
#   # Sexual attractiveness (composites + simple effect probes)
#   "p_sexattract", #partner sexual attractiveness
#   "s_sexattract", #self sexual attractiveness
#   "Zp_sexattract",
#   "Zs_sexattract",
#   "Zp_sexattract_p1", #centered at "plus 1" standard deviation from mean
#   "Zp_sexattract_m1", #centered at "minus 1" standard deviation below mean
# 
#   #> 3 items for partner sexual attractiveness composite
#   "pattract1", "pattract2", "pattract3",
#   #> 3 items for self sexual attractiveness composite
#   "sattract1", "sattract2", "sattract3"
# 
# 
#   # Covariates (not included to ensure anonymity)
# 
#   # "age_1",
#   # "rellength",
#   # "lnrellength",
#   # "livepart",
#   # "children",
#   # "children_current",
#   # "children1"
# 
#   )
# 
# save_df <- df[, keep_variables]
# 
# # Save as CSV file
# write.csv(save_df, file = "data/analysis_data/prolif_1_and_2_psa.csv", row.names = FALSE)
# 
# library(haven)
# # Save as SAV file (requires haven package)
# write_sav(save_df, path = "data/analysis_data/prolif_1_and_2_psa.sav")
# 
# # Save as RDS file
# saveRDS(save_df, file = "data/analysis_data/prolif_1_and_2_psa.rds")
# 
# rm(save_df)

#> Load open-sharing data file ----------

#df <- readRDS("data/analysis_data/prolif_1_and_2_psa.rds")


df_study1 <- df[df$study == "Study 1", ]  
df_study2 <- df[df$study == "Study 2", ] 
#creating a df where there is 1 row for each participant for some analyses 
dfp <- df[!duplicated(df$PROLIFIC_PID), ]

df_bmi <- df |> filter(BMI_trimmed >= 18 & BMI_trimmed <= 35)




#> Renaming Vector and Scales ----------

#> a renaming vector to ensure the resulting output has clean names 
nice_names <- c(
  ##New Name = Old Name 
  "Age" = "age_1",
  "Relationship Length" = "rellength", 
  "Cohabiting" = "livepart",
  "Children" = "children",
  "Children with Current Partner" = "children_current",
  "Menses" = "menses",
  "Study" = "study",
  "Study" = "Zstudy",
  "Partner Sexual Attract." = "Zp_sexattract",
  "Self Sexual Attract." = "Zs_sexattract",
  "Progesterone (WW)" = "Zprog_ww",
  "Estradiol (WW)" = "Zestr_ww",
  "Probability of Conception (WW)" = "Zprc_stirn_ww",
  "Probability of Conception (mean)" = "Zprc_stirn_mean",
  "Progesterone (mean)" = "Zprog_mean",
  "Estradiol (mean)" = "Zestr_mean",
  "Raw Estradiol (mean)" = "Zrawestr_mean",
  "Raw Progesterone (mean)" = "Zrawprog_mean",
  "Raw Estradiol (WW)" = "Zrawestr_ww",
  "Raw Progesterone (WW)" = "Zrawprog_ww",
  "Partner Sexual Attract. (m1)" = "Zp_sexattract_m1",
  "Partner Sexual Attract. (p1)" = "Zp_sexattract_p1",
  "Partner Sex Attract 1" = "pattract1",
  "Partner Sex Attract 2" = "pattract2",
  "Partner Sex Attract 3" = "pattract3",
  "Self Sex Attract 1" = "sattract1",
  "Self Sex Attract 2" = "sattract2",
  "Self Sex Attract 3" = "sattract3",
  
  #scale renaming 
  "Partner Sexual Attract." = "pattract",
  "Self Sexual Attract." = "sattract"
)

pattract_vars <- colnames(df[, grepl("pattract[1-3]", names(df))])
pattract_vars
sattract_vars <- colnames(df[, grepl("sattract[1-3]", names(df))])
sattract_vars

scales <- list(
  pattract = pattract_vars,
  sattract = sattract_vars
)


daily_scales <- list(
  EPsex = "sextoday",
  EPattract = c("EP1", "EP2"),
  EPinterest = c("ZEPattract", "Zsextoday"),
  EPinterest1 = "EPinterest",
  EPinterest2 = c("Zsextoday", "EP1", "EP2"),
  IPinit = "sex2init",
  IPattract = c("IP2", "IP3"),
  IPinterest = c("ZIPattract", "Zsex2init"),
  sex_freq = "sex1",
  part_init = "sex3Pinit",
  self_reject = "sex4reject"
)


#> Saving Directories -----

data_appendix_directory <- "output/psa_data_appendix_output/"
results_directory <- "output/psa_results/"

directories <- ls()[grepl("_directory", ls())]

for (i in directories) { #i = each element of the directories 
   
  directory_i <- get(i)
      
  if(!dir.exists(directory_i)) {
    
    dir.create(directory_i, recursive = TRUE)
    print(paste("Directory", directory_i, "created"))
  } else {print("Directory already exists")}
  
}


#> Exploring Sample Size/Nesting -----

#> Number of observations from each study 
study_obs <- df |> group_by(study) |> count()
study_obs
#> Number of surveys completed by each participant 
survey_completion <- df |> group_by(study, PROLIFIC_PID) |> count()
#> Number of participants in each study 
study_n <- survey_completion |> group_by(study) |> count()

# Build caption string dynamically
survey_completion_caption <- survey_completion |>
  group_by(study) |>
  summarize(
    mean = mean(n, na.rm = TRUE),
    sd   = sd(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(label = sprintf("%s: *M* = %.1f, *SD* = %.2f", study, mean, sd)) |>
  pull(label) |>
  paste(collapse = "; ")
survey_completion_caption

survey_completion_fig <- survey_completion |> 
  ggplot(aes(x = n, fill = study)) +
  geom_histogram(bins = 23) +
  labs(
    x = "Number of Daily Surveys Completed",
    y = "Number of Participants",
    title = "Most participants completed most surveys",
    fill = "",
    caption = survey_completion_caption,
  ) +
  jermeys_theme() +
  theme(
    plot.caption = element_markdown() #adds italics for caption
  )
survey_completion_fig

# filename <- paste0(data_appendix_directory, "survey_complete_fig", ".png")
# ggsave(filename, survey_completion_fig, width = 10, height = 6)

#> Descriptives Table by Sample ----------



#> I want the following structure:
#> Column 1:  
#> Age M (SD)
#> Relationship Length (years) M (SD) 
#> % cohabiting 
#> % with children
#>  % with a child with the current partner 
#>  
#>  Column 2 and 3: 
#>  Study 1          Study 2
#>  Columns 4-6
#>  test(df) statistic p 


#> Check variables


#one partipant accidentaly marked their age as "2". Given they passed prolific
#check, I'm sure that was not purposeful and it can just be removed. 
dfp[dfp$age_1 < 18, "age_1"] 
dfp[dfp$age_1 < 18, "age_1"] <- NA
#also removing that in the full sample for tests of age 
df[df$age_1 < 18, "age_1"] <- NA

# #Relationship length 
# describe(dfp$rellength)
# 
# #> Yes/no children 
# table(dfp$children)
# 
# #> Yes/no cohabiting 
# dfp$livepart
# table(dfp$livepart)
# 
# 
# #> Yes/no children with current partner
# dfp$children_current
# table(dfp$children_current)




# tidy_prop.test("livepart ~ study", data = df, nice_names)
# 
# tidy_t.test("age_1 ~ study", data = dfp, nice_names)

#> NOTE: 6/16/26 -- Getting an error on rerunning this. Need to debug the 
#> combine_tidy_tests function. 
# des_table <- combine_tidy_tests(data = dfp,
#                    t_formulas = c("age_1 ~ study", "rellength ~ study"), 
#                    p_formulas = c("livepart ~ study", "children ~ study", "children_current ~ study"),
#                    nice_names = nice_names)
# 
# des_table


# write.csv(des_table, paste0(results_directory, "psa_descriptive_table1.csv"))


#> Measurement Information ----------


rel_info <- scale_reliabilities(dfp, scales, "study", c("Study 1", "Study 2"))
rel_info
tidy_rel_info <- tidy_scale_reliabilities(rel_info)
tidy_rel_info 
rename_rows(tidy_rel_info, nice_names)

pub_cors_by(dfp, pattract_vars, study, levels(df$study))
pub_cors_by(dfp, sattract_vars, study, levels(df$study))


#> Correlations between attraction variables that were combined: 
cor(dfp$ZEPattract, dfp$Zsextoday)
cor(dfp$ZIPattract, dfp$Zsex2init, use = "pairwise")


#> Looking at what % of the sample has attractive versus unattractive partners 
dfp |> 
  summarise(
    m_ps = mean(p_sexattract, na.rm = TRUE),
    sd_ps = sd(p_sexattract, na.rm = TRUE),
    below_1sd = mean(p_sexattract < (m_ps - sd_ps), na.rm = TRUE) * 100,
    above_1sd = mean(p_sexattract > (m_ps + sd_ps), na.rm = TRUE) * 100
  )
psych::describe(dfp$p_sexattract)


dfs <- df |> group_by(PROLIFIC_PID) |> 
  mutate(
    sd_EPinterest = sd(EPinterest, na.rm = TRUE),
    sd_EPattract  = sd(EPattract,  na.rm = TRUE),
    sd_sextoday   = sd(sextoday,   na.rm = TRUE)
  ) |> 
  ungroup()


dfs[!duplicated(dfs$PROLIFIC_PID) & dfs$sd_sextoday != 0, ]
187/484 #only 39% of the sample has variance on sextoday 

dfs[!duplicated(dfs$PROLIFIC_PID) & dfs$sd_EPattract != 0, ]
286/484 #59% of the sample has variance on extra-pair attraction 

dfs[!duplicated(dfs$PROLIFIC_PID) & dfs$sd_EPinterest != 0, ]
316/484 #65% of the sample has variance on the composite of extra-pair interest 


#> Just load in the csv file after saving results to not waste time rerunning this. 
# mlr_results <- run_my_mlr(df, grp = "PROLIFIC_PID", daily_scales, "studyday")
# 
# mlr_results
# mlr_summary <- run_tidy_mlr(mlr_results)
# mlr_summary

#write.csv(mlr_summary, paste0(data_appendix_directory, "mlr_summary.csv"))

mlr_summary <- read.csv(paste0(data_appendix_directory, "mlr_summary.csv"))

#> Running and Saving Models ---------

#> Create model environment: 

model_environment <- new.env(parent = globalenv())

# names(mdls[ep])
# names(mdls[ea])
# names(mdls[sc])
# names(mdls[ip])


df$children1 <- as.factor(df$children)
contrasts(df$children1) <- contr.sum(2) / 2
contrasts(df$children1)


model_sets <- c("ep", "ea", "sc", "ip", "ex")


psa_analysis <- function(mdls, model_sets) {
#> Will need all of the necessary dataframes available in global environment 
  

#> Looping through the different model sets and running and saving them. 
#> Removing the full model lists from the environment after each set to not bog
#> down the environment too much 
for(i in seq_along(model_sets)) {
  
  #grab the specific model set from the environment 
  mdls_set_i_name <- model_sets[i]
  mdls_set_i <- get(mdls_set_i_name) 
  
  
  message("\n--- Running model set: ", mdls_set_i_name, " ---")
  
  
  tryCatch({
  
  #grab specific models   
  mdls_output <- mdls[mdls_set_i]
    
  #save that restricted set of models to a new list 
  mdls_output <- run_mlm_comparisons(mdls_output, 
                              model_env = model_environment)
  
  #update that new list with the fixed effect drops 
  mdls_output <- run_fixed_effect_drops(
    model_list = mdls_output, #make sure the model list referenced here is updated as needed
    remove = "menses",
    new_random_effect = "menses",
    alpha = .10,
    model_env = model_environment,
    model_path = "mc",
    fes_path = "fixed_effects",
    data_path = "data",
    name_path = "name")
  
  #save the full model output  
  rds_file_name <- paste0(mdls_set_i_name, "_FULL")
  saveRDS(mdls_output, paste0(data_appendix_directory, rds_file_name, ".rds"))
  
  #Grab the final models from the list and save those 
  fmdls_output <- vector("list", length(mdls_output))
  names(fmdls_output) <- names(mdls_output)
  for (m_i in seq_along(mdls_output)) {

    model_name_short <- names(mdls_output)[m_i] #e.g., EP_HP
    model_name_long <- mdls_output[[m_i]]$name
    use_model <- final_model_fed(mdls_output[[m_i]]$fed_menses)

    output <- list(
      name = model_name_long,
      post_mlm_comp_and_fed_model = use_model
    )
    fmdls_output[[model_name_short]] <- output

  }
  
  #clear environment of full model list to not bog down memory 
  rm(mdls_output)
  
  #Update file name and save 
  rds_file_name <- paste0(mdls_set_i_name, "_FINAL")
  saveRDS(fmdls_output, paste0(data_appendix_directory, rds_file_name, ".rds"))
  
  #remove the final model list to clear up environment further 
  rm(fmdls_output)
  gc() #garbage collector 
  
  },#end of tryCatch error handling
  error = function(e) {
    message("Error in model set ", mdls_set_i_name, ": ", e$message)
  })
  
} #end of main for loop  

}


#psa_analysis(mdls, model_sets[4])

psa_analysis(mdls, model_sets[1])

#> Load in final saved models 


#NOTE: If you don't have the models saved, you will have to un-comment the above
#code "psa_analysis(mdls, model_sets)" and run it. That will probably take at least
#> 10 minutes to run. 

# mep <- readRDS("output/data_appendix_output/ep_FULL.rds")
# fmep <- readRDS("output/data_appendix_output/ep_FINAL.rds")

# mea <- readRDS("output/data_appendix_output/ea_FULL.rds")
# fmea <- readRDS("output/data_appendix_output/ea_FINAL.rds")
# 
# msc <- readRDS("output/data_appendix_output/sc_FULL.rds")
# fmsc <- readRDS("output/data_appendix_output/sc_FINAL.rds")
# 
# mip <- readRDS("output/psa_data_appendix_output/ip_FULL.rds")
# fmip <- readRDS("output/psa_data_appendix_output/ip_FINAL.rds")
# 
# mex <- readRDS("output/data_appendix_output/ex_FULL.rds")
# fmex <- readRDS("output/data_appendix_output/ex_FINAL.rds")



#> SINGLE MODEL SET EXAMPLE -- If you want to test out a single model, the below
#> process can be done using a singular model from the mdls list. 

# cm <- run_mlm_comparisons(mdls[1])
# 
# cm <- run_fixed_effect_drops(
#   model_list = cm, #make sure the model list referenced here is updated as needed
#   remove = "menses",
#   new_random_effect = "menses",
#   alpha = .10,
#   model_env = model_environment,
#   model_path = "mc",
#   fes_path = "fixed_effects",
#   data_path = "data",
#   name_path = "name")
# 
# #cm$EP_PRCPSMSy$fed_menses
# saveRDS(cm, "output/data_appendix_output/filtered_df_models_core_ALL.rds")
# 
# cfm <- vector("list", 16)
# names(cfm) <- names(cm)
# for (i in seq_along(cfm)) {
# 
#   model_name_short <- names(cm[i]) #e.g., EP_HP
#   model_name_long <- cm[[i]]$name
#   use_model <- final_model_fed(cm[[i]]$fed_menses)
# 
#   output <- list(
#     name = model_name_long,
#     post_mlm_comp_and_fed_model = use_model
#   )
#   cfm[[model_name_short]] <- output
# 
# }
# 
# saveRDS(cfm, "output/data_appendix_output/filtered_df_models_core_FINAL.rds")

#> Create Nice Tables of Random Effect Removal and Menses Fixed Effect Drops ----------

full_model_files <- c("output/psa_data_appendix_output/ep_FULL.rds",
                      "output/psa_data_appendix_output/ea_FULL.rds",
                      "output/psa_data_appendix_output/sc_FULL.rds",
                      "output/psa_data_appendix_output/ip_FULL.rds"
                      #"output/psa_data_appendix_output/ep_bmi_FULL.rds"
                      )


psa_reports <- function(full_model_files) {

for(i in seq_along(full_model_files)) {
  
  # Define and display which file is being processed
  file_i <- full_model_files[i]
  

  message("\n--- Processing file ", i, " of ", length(full_model_files), ": ", file_i, " ---")

  
  tryCatch({
    
    #load in the current file 
    fm_i <- readRDS(file_i)  
    
  #update the currrent full model list with the mlm reports
  fm_i <- run_apa_mlm_report(fm_i,
                           mc_path = "mc",
                           title_prefix = "Random Effects Decision Process: ",
                           font = "Garamond",
                           font_size = 12,
                           sig_level = .20,
                           verbosity = "long")
  
  #> Save those file reports
  save_apa_mlm_reports(fm_i, data_appendix_directory,
                       report_path = "apa_mlm_report",
                       prefix = "apa_mlm_report",
                       create_subfolder = TRUE)
  
  #> Create and save the fixed effect removal process report 
  fm_i <- run_apa_fed_report(fm_i,
                             fed_path = "fed_menses",
                             font = "Garamond",
                             font_size = 12)

  save_apa_fed_reports(fm_i, data_appendix_directory)
  
  #remove the full model list to not bog down memory 
  rm(fm_i) #removes the model list from the environment 
  gc() #tells R: "Now clean up that memory right away" Sometimes R doesn't immediately
  #free up the memory space after removing an object, this ensures it will. 
  
}, error = function(e) {
  message("Error processing file ", file_i, ": ", e$message)
})
  
  
}

  
  message("\n All report generation attempts complete.")
  
}

#psa_reports(full_model_files[4])

psa_reports(full_model_files[1])

#> SINGLE MODEL REPORT ON A SET EXAMPLE

# cm <- run_apa_mlm_report(cm,
#                          mc_path = "mc",
#                          title_prefix = "Random Effects Decision Process: ",
#                          font = "Garamond",
#                          font_size = 12,
#                          sig_level = .20,
#                          verbosity = "long")
# 
# save_apa_mlm_reports(cm, data_appendix_directory,
#                      report_path = "apa_mlm_report",
#                      prefix = "apa_mlm_report",
#                      create_subfolder = TRUE)
# 
# cm <- run_apa_fed_report(cm,
#                                 fed_path = "fed_menses",
#                                 font = "Garamond",
#                                 font_size = 12)
# 
# save_apa_fed_reports(cm, data_appendix_directory)
# 


#> Create APA Style Tables of Each Model ----------

#> apa_lmer_model function renames variables to the "nice_names" version. 
#> It should handle cases where the interaction terms are flipped in order. 
bold_effects <- c("Partner Sexual Attract. * Probability of Conception (WW)",
                  "Partner Sexual Attract. * Estradiol (WW)",
                  "Partner Sexual Attract. * Progesterone (WW)",
                  "Partner Sexual Attract. * Raw Progesterone (WW)",
                  "Partner Sexual Attract. * Raw Estradiol (WW)",
                  "Probability of Conception (WW) * Partner Sexual Attract. (m1)",
                  "Probability of Conception (WW) * Partner Sexual Attract. (p1)"
                  # "Partner Sexual Attract. (m1) * Estradiol (WW)",
                  # "Partner Sexual Attract. (m1) * Progesterone (WW)",
                  # "Partner Sexual Attract. (p1) * Estradiol (WW)",
                  # "Partner Sexual Attract. (p1) * Progesterone (WW)"
                  )



final_model_files1 <- c("output/psa_data_appendix_output/ep_FINAL.rds",
                       "output/psa_data_appendix_output/ea_FINAL.rds",
                       "output/psa_data_appendix_output/sc_FINAL.rds"
                       #"output/psa_data_appendix_output/ip_FINAL.rds"
                       #"output/psa_data_appendix_output/ep_bmi_FINAL.rds"
                       )


supplemental_sections1 <- list(
  section_1 = c("EP_PRCPSMSy", "EP_HPSMSy"), #full models 
  section_2 = c("EP_PRCPSMSyBMI", "EP_HPSMSyBMI"), #full models w/extreme BMI's removed 
  section_3 = c("EP_PRCPSM", "EP_HPSM"), #remove study 
  section_4 = c("EP_PRCPSSy", "EP_HPSSy"), #remove between-woman 
  section_5 = c("EP_PRCPS", "EP_HPS"), #remove study and between-woman 
  section_6 = c("EP_PRCPSMs1", "EP_HPSMs1", #separate study 1 and study 2 models 
                 "EP_PRCPSMs2", "EP_HPSMs2"),
  section_7 = c("EP_PRCPMSy", "EP_HPMSy"), #remove self
  section_8 = c("EP_PRCP", "EP_HP"), #basic model
  section_9 = c("EP_RHPSMSy") #raw hormone analysis with full models 
)
supplemental_sections1

som_numbers1 <- c(3, 5, 6)

psa_lmer_tbls <- function(final_model_files, bold_effects, supplemental_sections, som_numbers,
                          extra_note_info = TRUE) {
  
  error_log <- list()  # store errors to inspect later
  
  
  for (i in seq_along(final_model_files)) {
    
    

    #file name 
    file_i <- final_model_files[i]
  message("\n--- Processing file ", i, " of ", length(final_model_files), ": ", file_i, " ---")
    
    
    # Determine replacement prefix based on filename
    if (grepl("ea_FINAL", file_i)) {
      new_prefix <- "EA_"
    } else if (grepl("sc_FINAL", file_i)) {
      new_prefix <- "SC_"
    } else if (grepl("ep_FINAL", file_i)) {
      new_prefix <- "EP_"  # default
    } else {
      new_prefix <- NULL #just grab the original name 
    }
    
    # Replace prefixes only if we have a new prefix
    if (!is.null(new_prefix)) {
      supplemental_sections <- lapply(supplemental_sections, function(section) {
        gsub("^[A-Z]{2}_", new_prefix, section)
      })
    }
    
  
    tryCatch({
      
    #load final model 
    fm_i <- readRDS(file_i)
    
    
    for(s_i in seq_along(supplemental_sections)) {
    
    #grab shorter name for ease of use within the loop
    sec_i <- supplemental_sections[[s_i]]  
    message(" Running section: ", paste(sec_i, collapse = ", "))
      
    #grab specific models in this supplemental section
    #fm_section <- fm_i[sec_i]
    
    #give the tables a specific title based upon the specific supplemental section
    #number
    table_i_title <- paste0("Supplemental Table ", som_numbers[i], ".", s_i, ".")  
    #som_numbers[i] = the first number of the supplemental 
    #so, the first table title will be: Supplemental Table 3.1 
    # my function automatically will add in another .1, .2, etc. for table in the
    # section. 
    #> So, the final table will be numbered as: 
    #> "Supplemental Table 3.1.1" 
 
    # Run APA lmer table creation
    tryCatch({  
         
    fm_i[sec_i] <- run_apa_lmer_model(model_list = fm_i[sec_i],
                                data = df, #need to supply df with all of the variable names
                                #as long as variable has all of the correct names, it
                                #won't matter if its filtered differently.
                                nice_names = nice_names,
                                model_path = "post_mlm_comp_and_fed_model",
                                bold_title = table_i_title,
                                table_note = "All continuous variables were standardized prior to
                        analysis. Menses was coded 0 = not menstruating, 1 = menstruating.
                                ",
                                font = "Garamond",
                                sig_level = FALSE,
                                effects_to_bold = bold_effects,
                                extra_note_info = extra_note_info,
                                font_size = 12
    )
    
    
    }, error = function(e_inner) {
      msg <- paste0("Error in file '", file_i,
                    "', section '", sec_i,
                    "': ", e_inner$message)
      message(msg)
      error_log[[paste(file_i, sec_i, sep = "_")]] <<- msg
    })
  } #end of inner for loop
    
   #save the fully updated model list  
   save_apa_lmer_tables(fm_i, data_appendix_directory, apa_table_path = "apa_table")
    
   #remove the final model list from environment to clear memory 
   rm(fm_i)
   gc() #ensure memory is freed up 
    
   
    }, error = function(e_outer) {
      msg <- paste0("Failed to process file '", file_i, "': ", e_outer$message)
      message(msg)
      error_log[[file_i]] <<- msg
    })
   
    message("\n All table generation attempts complete.")  
    
   
  } #end of outer for loop
  
  if (length(error_log)) {
    message("\n Some errors occurred. Run `View(error_log)` to inspect.")
    return(error_log)
  } else {
    message("\n All models ran successfully.")
  }
  
} #End of function

#psa_lmer_tbls(final_model_files1[1], bold_effects, supplemental_sections1, som_numbers1)

#> I can reuse this function, but I am just going to create a single set of models 
#> so I'll just set the supplemental sections to 1. 
supplemental_sections2 <- list(
  section_1 = c(ip[c(1:5, 7)]), 
  section_2 = c(ip[c(8:10, 12:16)])

)
supplemental_sections2




# psa_lmer_tbls("output/psa_data_appendix_output/ip_FINAL.rds", bold_effects,
#               supplemental_sections = supplemental_sections2,
#               extra_note_info = FALSE,
#               som_numbers = 7)



# cfm <- run_apa_lmer_model(model_list = cfm,
#                             data = df, #need to supply df with all of the variable names
#                             #as long as variable has all of the correct names, it
#                             #won't matter if its filtered differently.
#                             nice_names = nice_names,
#                             model_path = "post_mlm_comp_and_fed_model",
#                             bold_title = "Supplemental Table 3.",
#                             table_note = "All continuous variables were standardized prior to
#                     analysis. Menses was coded 0 = not menstruating, 1 = menstruating.
#                             ",
#                             font = "Garamond",
#                             sig_level = FALSE,
#                             effects_to_bold = bold_effects,
#                             extra_note_info = TRUE,
#                             font_size = 12
# )
# #cfm$EP_PRCPSMSy$apa_table
# save_apa_lmer_tables(cfm, data_appendix_directory, apa_table_path = "apa_table")

# print(make_mlm_report(ncm$Sex2_HPS$mc, verbosity = "long"))
# print(ncm$Sex2_HPS$apa_mlm_report)
# ncm$Sex2_HPS$mc$step02_final_model$model1
# 


#> Create APA Style Tables of Each Models Random Effects ------


# final_model_files <- c("output/psa_data_appendix_output/ep_FINAL.rds",
#                        "output/psa_data_appendix_output/ea_FINAL.rds",
#                        "output/psa_data_appendix_output/sc_FINAL.rds",
#                        "output/psa_data_appendix_output/ip_FINAL.rds"
# ) 

psa_re_tbls <- function(final_model_files, nice_names) {
  
  
  error_log <- list()  # store any errors encountered
  
  for(i in seq_along(final_model_files)) {
    
    #file name 
    file_i <- final_model_files[i]
  message("\n--- Processing file ", i, " of ", length(final_model_files), ": ", file_i, " ---")
    
  
  tryCatch({  
      
    #load final model 
    fm_i <- readRDS(file_i)
    
    # Try to generate random-effects tables
    tryCatch({
    
    #update final model list with random effects table
    fm_i <- run_apa_lmer_random(fm_i, model_path = "post_mlm_comp_and_fed_model",
                                nice_names = nice_names, font = "Garamond", font_size = 12)
    
    }, error = function(e_inner) {
      msg <- paste0("Error in run_apa_lmer_random() for file ", file_i, ": ", e_inner$message)
      message(msg)
      error_log[[file_i]] <<- msg
      return(NULL)  # skip saving if this part fails
    })
    
    #save random effects tables for specific model list 
    save_apa_lmer_random(fm_i, data_appendix_directory)
    
  }, error = function(e) {
    message("Error processing file ", file_i, ": ", e$message)
  }) 
    
    #clear up memory 
    rm(fm_i)
    gc()
    
    
  } #end of for loop  
  
  message("\n Random-effects table generation attempts complete.")
  
  
  if (length(error_log)) {
    message("\n Some errors occurred. Run `View(error_log)` to inspect.")
    return(error_log)
  } else {
    message("\n All random-effects tables generated successfully.")
  }
  
} #end of function

#psa_re_tbls(final_model_files[4], nice_names)

#psa_re_tbls(final_model_files1[1], nice_names)



# cfm <- run_apa_lmer_random(cfm, model_path = "post_mlm_comp_and_fed_model",
#                            nice_names, font = "Garamond", font_size = 12)
# 
# save_apa_lmer_random(cfm, data_appendix_directory)


#> Example using a single model: 

# t1 <- apa_lmer_model(m1,
#                     df,
#                     nice_names = nice_names,
#                     bold_title = "Supplemental Table 3.1",
#                     italics_title = "Extra-pair interest full model: Predicting
#                     extra-pair interest from partner and self sexual attractiveness.",
#                     table_note = "All continuous variables were standardized prior to
#                     analysis. Menses was coded 0 = not menstruating, 1 = menstruating.
#                             Random effects are recorded in the table below.",
#                     font = "Garamond",
#                     font_size = 12,
#                     effects_to_bold = c("Partner Sexual Attract. * WW Probability of Conception",
#                                         "Partner Sexual Attract. * WW Estradiol",
#                                         "Partner Sexual Attract. * WW Progesterone"),
#                     sig_level = FALSE)
# t1
# 
# #> This works nice if I want to use the table within a manuscript
# # Create a new Word doc and add the table
# doc <- read_docx() |>
#   body_add_flextable(t1) |>
#   print(target = "output/data_appendix_output/extra_pair_interest_table.docx")

#> This works great for displaying in pdf or html document. 
#save_as_image(t1, path = "output/data_appendix_output/extra_pair_interest_table.png")



#> Simple Effects Analyses for Figure --------

fmep <- readRDS("output/psa_data_appendix_output/ep_FINAL.rds")
fmip <- readRDS("output/psa_data_appendix_output/ip_FINAL.rds")


m1 <- fmep$EP_PRCPSMSy$post_mlm_comp_and_fed_model

m2 <- fmep$EP_HPSMSy$post_mlm_comp_and_fed_model

m3 <- fmip$S2_PRCPS$post_mlm_comp_and_fed_model

options(scipen = 999)
et1 <- emtrends(
  m1,
  specs = "Zp_sexattract",
  var = "Zprc_stirn_ww",
  at = list(Zp_sexattract = c(-1, 1)),
  infer = TRUE,
  pmbkrtest.limit = 13000,
  lmerTest.limit = 13000
)
as.data.frame(et1)$p.value

et2 <- emtrends(
  m2,
  specs = "Zp_sexattract",
  var = "Zprog_ww",
  at = list(Zp_sexattract = c(-1, 1)),
  infer = TRUE,
  pmbkrtest.limit = 13000,
  lmerTest.limit = 13000
)
as.data.frame(et2)
as.data.frame(et2)$p.value

et3 <- emtrends(
  m2,
  specs = "Zp_sexattract",
  var = "Zestr_ww",
  at = list(Zp_sexattract = c(-1, 1)),
  infer = TRUE,
  pmbkrtest.limit = 13000,
  lmerTest.limit = 13000
)
as.data.frame(et3)
as.data.frame(et3)$p.value


et4 <- emtrends(
  m3,
  specs = "Zp_sexattract",
  var = "Zprc_stirn_ww",
  at = list(Zp_sexattract = c(-1, 1)),
  infer = TRUE,
  pmbkrtest.limit = 13000,
  lmerTest.limit = 13000
)
as.data.frame(et4)
as.data.frame(et4)$p.value

ss1 <- ggpredict(m1, terms = c("Zprc_stirn_ww", "Zp_sexattract [-1,1]"))
ss2 <- ggpredict(m2, terms = c("Zprog_ww", "Zp_sexattract [-1,1]"))
ss3 <- ggpredict(m2, terms = c("Zestr_ww", "Zp_sexattract [-1,1]"))

ss1$variable <- "Probability of Conception"
ss2$variable <- "Progesterone"
ss3$variable <- "Estradiol"

ss <- rbind(ss1, ss2, ss3)

ss$variable <- factor(ss$variable, levels = c("Probability of Conception", "Progesterone", "Estradiol"))


psa_prc_hormones <- ss |> ggplot(aes(x = x, y = predicted, color = group, fill = group, linetype = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_color_manual(
    values = c("red", "lightblue"),
    labels = c("1 SD below mean", "1 SD above mean"),
    name = "Partner Attractiveness"
  ) +
  scale_fill_manual(
    values = c("red", "lightblue"),
    labels = c("1 SD below mean", "1 SD above mean"),
    name = "Partner Attractiveness"
  ) +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("1 SD below mean", "1 SD above mean"),
    name = "Partner Attractiveness"
  ) +
  labs(
    x = "",
    y = "Extra-Pair Sexual Interests"
  ) +
 coord_cartesian(xlim = c(-2, 2)) +
  facet_wrap(~variable, strip.position = "bottom") +
  jermeys_theme() +
  theme(
    legend.key.width = unit(2.4, "cm"),  # wider legend boxes
    legend.key.height = unit(1, "cm"), # optional: taller boxes
    legend.position = "top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    strip.background = element_blank(),
    strip.placement = "outside"
  )
psa_prc_hormones


# filename <- paste0(results_directory, "psa_prc_hormones_simpleeff_fig", ".png")
# ggsave(filename, psa_prc_hormones, width = 12, height = 6)



#> Create APA Style Table for the Manuscript -----------

#> Grabbing effects from relevant models here. Just 6 effects from first model 
#> and 10 from second model 
#> Main effects, key interactions, and study interactions. 
m1
m2

tblm1 <- rename_lmer(m1, df, nice_names)
tblm1$model <- "PRC"
tblm2 <- rename_lmer(m2, df, nice_names)
tblm2$model <- "H"

tbl2 <- rbind(tblm1, tblm2)

tbl2 <- tbl2 |> 
  filter(str_detect(
    var_star,
    paste(
      # main effects
      "^Partner Sexual Attract\\.$",
      "^Study$",
      "^Probability of Conception \\(WW\\)$",
      "^Progesterone \\(WW\\)$",
      "^Estradiol \\(WW\\)$",
      
      # 2-way interactions with Partner Sexual Attract
      "^(Probability of Conception \\(WW\\)|Progesterone \\(WW\\)|Estradiol \\(WW\\)) \\* Partner Sexual Attract\\.$",
      
      # 2-way interactions with Study
      "^Study \\* (Probability of Conception \\(WW\\)|Progesterone \\(WW\\)|Estradiol \\(WW\\)|Partner Sexual Attract\\.)$",
      
      # 3-way interactions with Study × Partner Sexual Attract
      "^Study \\* (Probability of Conception \\(WW\\)|Progesterone \\(WW\\)|Estradiol \\(WW\\)) \\* Partner Sexual Attract\\.$",
      
      sep = "|"
    )
  ))
tbl2

tbl2_ordered <- tbl2 %>%
  mutate(
    order_rank = case_when(
      str_detect(var_star, "^Study$") ~ 1,
      str_detect(var_star, "^(Probability of Conception \\(WW\\)|Progesterone \\(WW\\)|Estradiol \\(WW\\))$") ~ 2,
      str_detect(var_star, "^Partner Sexual Attract\\.$") ~ 3,
      str_detect(var_star, "^Study \\* Probability of Conception \\(WW\\)$") ~ 4.1,
      str_detect(var_star, "^Study \\* Progesterone \\(WW\\)$") ~ 4.2,
      str_detect(var_star, "^Study \\* Estradiol \\(WW\\)$") ~ 4.3,         # now before Partner
      str_detect(var_star, "^Study \\* Partner Sexual Attract\\.$") ~ 4.4,  # moved below
      str_detect(var_star, "^(Probability of Conception \\(WW\\)|Progesterone \\(WW\\)|Estradiol \\(WW\\)) \\* Partner Sexual Attract\\.$") ~ 5,
      str_detect(var_star, "^Study \\* (Probability of Conception \\(WW\\)|Progesterone \\(WW\\)|Estradiol \\(WW\\)) \\* Partner Sexual Attract\\.$") ~ 6,
      TRUE ~ 999
    ),
    model = factor(model, levels = c("PRC", "H"))
  ) %>%
  arrange(model, order_rank)

tbl2_final <- tbl2_ordered %>%
  transmute(
    Variable = var_star,
    Estimate = sprintf("%.2f", Estimate),
    CI = sprintf("[%.2f, %.2f]", `2.5 %`, `97.5 %`),
    t = sprintf("%.2f", `t value`),
    df = sprintf("%.1f", df),
    p = sprintf("%.3f", `Pr(>|t|)`)
  )

tbl2_final


#write.csv(tbl2_final, paste0(results_directory, "psa_full_model_results_table2.csv"))

#> Building a Full Table of Model Results --------

fmep <- readRDS("output/psa_data_appendix_output/ep_FINAL.rds")
fmea <- readRDS("output/psa_data_appendix_output/ea_FINAL.rds")
fmsc <- readRDS("output/psa_data_appendix_output/sc_FINAL.rds")
fmip <- readRDS("output/psa_data_appendix_output/ip_FINAL.rds")
 
fmdls <- c(fmep, fmea, fmsc, fmip) 
rm(list = ls()[grepl("fm[a-z]{2}$", ls())])
# fmdls <- rename_lmers(fmdls, nice_names, df, model_path = "post_mlm_comp_and_fed_model")
# 
# 
# tidy_tbl <- bind_tidy_tbls(fmdls)
# as_tibble(tidy_tbl)
# #save the model output to a csv file:
#write.csv(tidy_tbl, paste0(results_directory, "df_lmer_model_coefficients.csv"), row.names = FALSE)

tidy_tbl <- read_csv(paste0(results_directory, "df_lmer_model_coefficients.csv"))

#> Interaction Forest Plots ----------


var1 <- "Probability of Conception (WW) * Partner Sexual Attract."
var2 <- "Progesterone (WW) * Partner Sexual Attract."
var3 <- "Estradiol (WW) * Partner Sexual Attract."
var4 <- "Raw Estradiol (WW) * Partner Sexual Attract."
var5 <- "Raw Progesterone (WW) * Partner Sexual Attract."

order_vec <- c("EP_PRCPSMSy", #full model  
               "EP_PRCPSMSyBMI", #full model filtering on BMI
               "EP_PRCPSM", "EP_PRCPSSy", #dropping study/mean
               "EP_PRCPS", #dropping study and mean 
               "EP_PRCPSMs1", "EP_PRCPSMs2", #by study 
               "EP_PRCPMSy", "EP_PRCP", #dropping self sexual attractiveness
               
               "EP_BLANK", #placeholder to get even spacing 
               
               "EP_HPSMSy", #full model 
               "EP_HPSMSyBMI", #full model with BMI filtering
               "EP_HPSM", "EP_HPSSy", #dropping study/mean 
               "EP_HPS", #droping study and mean 
               "EP_HPSMs1", "EP_HPSMs2", #by study 
               "EP_HPMSy",  "EP_HP", #dropping self sexual attractiveness 
               "EP_RHPSMSy", #raw hormones 
               
               #In-pair and General sexual desire model order 
               "IP_PRCPSMSy", 
               "IA_PRCPSMSy",
               "S2_PRCPS",
               
               "GN_PRCPSMSy",
               "S1_PRCPSMSy",
               "S3_PRCPSMSy",
               "S4_PRCPSMSy",
               
               "IP_HPSMSy", 
               "IA_HPSMSy",
               "S2_HPS_e",
               
               "GN_HPSMSy",
               "S1_HPSMSy_p",
               "S3_HPSMSy",
               "S4_HPSMSy"
               )
order_vec1 <- gsub("^EP_", "EA_", order_vec)
order_vec2 <- gsub("^EP_", "SC_", order_vec)

order_vec <- c(order_vec, order_vec1, order_vec2)
order_vec <- order_vec[!duplicated(order_vec)] #by updating the EA and SC names
#I end up with duplicates of all of the unchanged variables that get carried over
#from the original order_vec. Removing them here. 

psa_ints <- tidy_tbl |> 
  filter(var_star == var1 | var_star == var2 | var_star == var3 |
           var_star == var4 | var_star == var5) |> 
  mutate(
    sig = pval < .05,
    sig = ifelse(sig == TRUE, "Significant", "Non-significant"),
    sig = factor(sig, levels = c("Significant", "Non-significant")),  # reorder legend
    # classify hormone from the label text
    hormone = ifelse(grepl("Prog", var_star, ignore.case = TRUE),
                     "Progesterone", 
                     ifelse(grepl("Estr", var_star, ignore.case = TRUE), "Estradiol",
                            "Probability of Conception")),
    hormone = factor(hormone, levels = c("Probability of Conception", "Progesterone", "Estradiol")),
   model = factor(model, levels = rev(order_vec))
  )
psa_ints


# --- Add blank rows directly to psa_ints ---
blank_rows <- psa_ints %>%
  filter(grepl("RHPSMSy$", model)) %>%
  distinct(model, .keep_all = TRUE) |> 
  mutate(
    hormone = factor("Probability of Conception",
                     levels = c("Probability of Conception","Progesterone","Estradiol")),
    # Assign appropriate model names
    model = case_when(
      grepl("^EP_", model) ~ "EP_BLANK",
      grepl("^EA_", model) ~ "EA_BLANK",
      grepl("^SC_", model) ~ "SC_BLANK"
    ),
    # Keep these as real factor levels
    model = factor(model, levels = rev(order_vec)),
    # Make them invisible in the plot (all NA estimates)
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    se = NA_real_,
    pval = NA_real_,
    t.value = NA_real_,
    var_star = "",
    sig = factor("Non-significant", levels = levels(psa_ints$sig))
  )
blank_rows
# Bind them into psa_ints
psa_ints <- bind_rows(psa_ints, blank_rows)
psa_ints %>% filter(grepl("BLANK", model))
#rm(list = ls(pattern = "fm"))

psa_plot_df <- psa_ints[psa_ints$model %in% c(ep, ea, sc) |
                          grepl("BLANK", psa_ints$model), ] |> 
  mutate(
    dv = ifelse(grepl("^EP_", model), "Extra-Pair Interest",
                ifelse(grepl("^EA_", model), "Extra-Pair Attraction",
                       ifelse(grepl("^SC_", model), "Extra-Pair Sex", NA))),
    dv = factor(dv,
                levels = c("Extra-Pair Interest", 
                           "Extra-Pair Attraction", 
                           "Extra-Pair Sex")),
    dv_title_main = case_when(
      dv == "Extra-Pair Interest"   ~ "Extra-Pair Interest",
      dv == "Extra-Pair Attraction" ~ "Extra-Pair Attraction",
      dv == "Extra-Pair Sex"        ~ "Extra-Pair Sex"
    ),
    dv_subtitle = case_when(
      dv == "Extra-Pair Interest"   ~ "(Main preregistered outcome calculated as the mean of two standardized components: extra-pair attraction and extra-pair sex)",
      dv == "Extra-Pair Attraction" ~ "(Sub-component of Extra-pair Interest: Two questions on extra-pair physical/sexual attraction)",
      dv == "Extra-Pair Sex"        ~ "(Sub-component of Extra-pair Interest: Single question extra-pair scenario)"
    ),
    y_title = case_when(
      dv == "Extra-Pair Interest"   ~ "Models",
      dv == "Extra-Pair Attraction" ~ "Models",
      dv == "Extra-Pair Sex"        ~ "Models"
    )
  )

# #> Creating blank rows for spacing in the graph 
# blank_rows <- psa_plot_df %>%
#   filter(grepl("RHPSMSy$", model)) %>%      # get one row per DV
#   distinct(dv, dv_title_main, dv_subtitle, y_title) %>%
#   mutate(
#     hormone  = factor("Probability of Conception",
#                       levels = c("Probability of Conception","Progesterone","Estradiol")),
#     model    = case_when(
#       dv == "Extra-Pair Interest"   ~ "EP_BLANK",
#       dv == "Extra-Pair Attraction" ~ "EA_BLANK",
#       dv == "Extra-Pair Sex"        ~ "SC_BLANK"
#     ),
#     model    = factor(model, levels = levels(psa_plot_df$model)),
#     estimate = NA_real_,
#     conf.low = NA_real_,
#     conf.high = NA_real_,
#     pval = NA_real_,
#     var_star = "",
#     sig = factor("Non-significant", levels = levels(psa_plot_df$sig))
#   )
# # Bind the blank rows
# psa_plot_df <- bind_rows(psa_plot_df, blank_rows)
# #psa_plot_df[grepl("BLANK", psa_plot_df$model), ]



psa_plot_split <- psa_plot_df |> split(~ dv)


make_plot <- function(df) {

   ggplot(df, aes(x = estimate, y = model, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("Non-significant" = "gray50", "Significant" = "#009E73"),
      name = NULL,
      drop = FALSE #forces Significant and Non-significant to both display whether
      #or not they are both represented in a figure
    ) +
    labs(
     # x = "Estimate (β) with 95% CI",
      x = NULL,
      y = df$y_title,
      title = df$dv_title_main,  # main title 
      subtitle = df$dv_subtitle
     # caption = "Bars are Wald 95% CIs computed via Satterthwaite's method"
    ) +
    facet_wrap(~hormone, scales = "free_y") +
    scale_y_discrete(labels = function(x) ifelse(grepl("_BLANK$", x), "", x)) +
    jermeys_theme(base_size = 18) +
    theme(
      legend.position = "none", #suppress legends 
      #legend.position = "top",
      #legend.direction = "horizontal",
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 12))
    )
}

# build each subplot
fps <- lapply(psa_plot_split, make_plot)
# plots$`Extra-Pair Sex` <- plots$`Extra-Pair Sex` +
#   theme(legend.position = c(.88, -0.85),
#         legend.direction = "horizontal")
# 
# plots$`Extra-Pair Sex`

fps$`Extra-Pair Interest`
fps$`Extra-Pair Attraction`
fps$`Extra-Pair Sex`


# combine vertically
fp_ep_ea_sc <- wrap_plots(fps, ncol = 1) +
  plot_annotation(
    title = "Partner Sexual Attractiveness Interactions with Each Predictor and Across Outcomes and Different Model Controls",
    caption = "Estimate (\u03B3) with 95% CIs",
    #subtitle = "",  # 
    theme = theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                  plot.caption = element_text(size = 20, hjust = .55),
                  )
  ) 
fp_ep_ea_sc #this figure gets to be a lot 

# filename <- paste0(results_directory, "fp_core16_fig", ".png")
#> NOTE: I ended up just expanding my plot screen to a desirable size and saving
#> the figure via that since using the below code caused the plot to be scrunched. 
#> 
# ggsave(filename, fp_core16, width = 10, height = 6)


make_plot2 <- function(models) {

fp <- psa_ints[psa_ints$model %in% models, ] |>
  ggplot(aes(x = estimate, y = model,
             color = sig, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0) +
  geom_point(size = 2) +
  scale_alpha_manual(values = c("Significant" = 1, "Non-significant" = 0.5), guide = "none") +
  scale_color_manual(
    values = c("Non-significant" = "gray50", "Significant" = "#009E73"),  # customize here
    name = NULL,
    guide = "none"
  ) +
  scale_shape_manual(                              # shape palette
    values = c("Non-significant" = 17, "Significant" = 16),  # open vs filled circle
    name = NULL,
    guide = "none"
  ) +
  labs(
    x = "Estimate (\u03B3) with 95% CI",
    y = "Model",
    color = "",
    #title = "Partner Sexual Attractiveness Interactions Across Models",
    #caption = "Bars are Wald 95% CIs computed via Satterthwaite's method"
  ) +
  facet_wrap(~hormone, scales = "free_y", strip.position = "bottom") +
  scale_y_discrete(labels = function(x) ifelse(grepl("_BLANK$", x), "", x)) +
  jermeys_theme(base_size = 18) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    strip.placement = "outside",
#   strip.background = element_rect(fill = "white"),
    strip.background.x = element_blank(),
    strip.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 20),
    plot.margin = margin(t = 20, b = 20, l = 20, r = 30),
  )
  
  return(fp)

}

fps2 <- lapply(list(c(ep, "EP_BLANK"), c(ea, "EA_BLANK"), c(sc, "SC_BLANK"),
                    ip), make_plot2)
fps2[[4]]

# #Save the three separate images: 
# filename <- paste0(results_directory, "psa_epinterest_fp1", ".png")
# ggsave(filename, fps2[[1]], width = 13, height = 7)
# 
# 
# filename <- paste0(results_directory, "psa_epattract_fp1", ".png")
# ggsave(filename, fps2[[2]], width = 13, height = 7)
# 
# 
# filename <- paste0(results_directory, "psa_epscenario_fp1", ".png")
# ggsave(filename, fps2[[3]], width = 13, height = 7)
# 
# filename <- paste0(results_directory, "psa_ip_fp1", ".png")
# ggsave(filename, fps2[[4]], width = 13, height = 7)


#> Collecting Simple Effects -------


#> example on a single model 
#tidy_emtrend(m1, "Zp_sexattract", "Zprog_ww")


# simple_effects <- tidy_emtrends(fmdls, "Zp_sexattract",
#                   vars = c("Zprc_stirn_ww", "Zprog_ww", "Zestr_ww"),
#                   "post_mlm_comp_and_fed_model")


#write_csv(simple_effects, paste0(results_directory, "df_simple_effects.csv"))

simple_effects <- read.csv(paste0(results_directory, "df_simple_effects.csv"))

# nrow(simple_effects[simple_effects$variable == "Zprc_stirn_ww", ])
# nrow(simple_effects[simple_effects$variable == "Zprog_ww", ])
# nrow(simple_effects[simple_effects$variable == "Zestr_ww", ])

simple_effects[simple_effects$model %in% c("EP_PRCPSMSy", "EP_HPSMSy") &
                 simple_effects$moderator_value %in% c(-1, 1), ] 


#> Printing file names to paste into .qmd file -------

lmer_tbls_directory <- paste0(data_appendix_directory, "apa_table/")

re_tbls_directory <- paste0(data_appendix_directory, "apa_lmer_random/")

fed_reports_directory <- paste0(data_appendix_directory, "apa_fed_report/")

mlm_reports_directory <- paste0(data_appendix_directory, "apa_mlm_report/")


directories <- mget(ls()[grepl("_directory", ls())])
directories <- do.call(c, directories)


qmd_printing <- qmd_image_files(lmer_tbls_directory, go_up = 2)

cat(paste(qmd_printing, collapse = "\n\n"))


qmd_png_file_links <- run_qmd_image_files(directories)

#saveRDS(qmd_png_file_links, "output/psa_data_appendix_output/qmd_png_file_links.rds")

qmd_png_file_links










#> Looking at specific model summaries ------


# for (i in seq_along(fmdls)) {
# #  print(i)
# # print(isSingular(fmip[[i]]$post_mlm_comp_and_fed_model))
#   print(names(fmdls[i]))
#   print(fmdls[[i]]$post_mlm_comp_and_fed_model@optinfo$conv$lme4)
# }

#> The hormone primary model didn't quite converge. Refitting the model with a 
#> different optimizer and max number of iterations solves the issue. Results are
#> the same up to the second/third decimal place either way. 
# summary(fmdls$EP_HPSMSy$post_mlm_comp_and_fed_model)
# summary(refit_model(fmdls$EP_HPSMSy$post_mlm_comp_and_fed_model))


# #extra-pair attraction 
# summary(fmdls$EA_PRCPSMSy$post_mlm_comp_and_fed_model)
# summary(fmdls$EA_HPSMSy$post_mlm_comp_and_fed_model)
# 
# #scenario 
# summary(fmdls$SC_PRCPSMSy$post_mlm_comp_and_fed_model)
# summary(fmdls$SC_HPSMSy$post_mlm_comp_and_fed_model)
# 
# summary(fmdls$IP_PRCPSMSy$post_mlm_comp_and_fed_model) #in-pair interest 
# summary(fmdls$IA_PRCPSMSy$post_mlm_comp_and_fed_model) #in-pair attraction 
# summary(fmdls$S2_PRCPS$post_mlm_comp_and_fed_model) 
# summary(fmdls$S2_HPS_e$post_mlm_comp_and_fed_model) #sexual initiation by self 
# summary(fmdls$GN_PRCPSMSy$post_mlm_comp_and_fed_model) #general sexual desire 
# summary(fmdls$S3_PRCPSMSy$post_mlm_comp_and_fed_model) #partner initiation 
# summary(fmdls$S1_PRCPSMSy$post_mlm_comp_and_fed_model) #total frequency 
# summary(fmdls$S4_PRCPSMSy$post_mlm_comp_and_fed_model) #rejection of partner advances 


