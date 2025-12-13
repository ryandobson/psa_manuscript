
#> Model List(s) ------------
mdls <- list(
  
  #naming conventions: 
  #> DependentVariable_KeyPredictorAdditionalPredictors
  #> e.g., 
  #> EP_PRC_PSMSy =
  #> EP_ = extra pair interest is the dependent variable
  #> PRC = prc_stirn (i.e., probability of conception) is key predictor
  #> partner sexual attractiveness, self sexual attractiveness, mean predictors,
  #> and Study are included 
  #> study = Sy
  #> partner = P
  #> self = S
  #> mean = M
  #> hormones = H (i.e., progesterone and estradial)
  #> raw hormones = RH 
  #> only use study 1 observations = s1
  #> only use study 2 observations = s2
  
#> Version 1: Primary preregistered complete models   
  
  #> Model 
  EP_PRCPSMSy = list(
    name = "Primary preregistered Probability of Conception model reported in 
    manuscript",
    data = "df",
    fixed_effects =
      c("menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprc_stirn_ww",
        "Zs_sexattract : Zprc_stirn_mean",
        "Zp_sexattract : Zprc_stirn_ww",
        "Zp_sexattract : Zprc_stirn_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zprc_stirn_ww", 
        "Zstudy : Zprc_stirn_mean",
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprc_stirn_ww",
        "Zstudy : Zs_sexattract : Zprc_stirn_mean",
        "Zstudy : Zp_sexattract : Zprc_stirn_ww",
        "Zstudy : Zp_sexattract : Zprc_stirn_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  #Model  
  EP_HPSMSy = list(
    name = "Primary preregistered estradiol and progesterone model reported in 
    manuscript",
    data = "df",
    fixed_effects = 
      c("menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
        "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
        "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
        "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
        "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
        "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract :
   Zestr_mean",
        "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
        "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract :
   Zestr_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  

#> Version 2: Removing only study terms from the primary preregistered models 

  #> Model 
  EP_PRCPSM = list(
    name = "Removing study terms from the primary preregistered PRC model",
    data = "df",
    fixed_effects =
      c("menses", "Zprc_stirn_ww", "Zprc_stirn_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprc_stirn_ww",
        "Zs_sexattract : Zprc_stirn_mean",
        "Zp_sexattract : Zprc_stirn_ww",
        "Zp_sexattract : Zprc_stirn_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract"
       ),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  #Model  
  EP_HPSM = list(
    name = "Removing study terms from the primary preregistered hormones model",
    data = "df",
    fixed_effects = 
      c("menses", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
        "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
        "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
        "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract"
       ),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
#> Version 3: Removing only between-woman terms from the primary preregistered 
#> models 

  #> Model 
  EP_PRCPSSy = list(
    name = "Removing between-woman terms from the primary preregistered PRC model",
    data = "df",
    fixed_effects =
      c("menses", "Zstudy", "Zprc_stirn_ww", 
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprc_stirn_ww",
        "Zp_sexattract : Zprc_stirn_ww",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zprc_stirn_ww", 
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprc_stirn_ww",
        "Zstudy : Zp_sexattract : Zprc_stirn_ww",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  #Model  
  EP_HPSSy = list(
    name = "Removing between-woman terms from the primary preregistered hormone
    model",
    data = "df",
    fixed_effects = 
      c("menses", "Zstudy", "Zestr_ww", "Zprog_ww", 
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
        "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
        "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
   
  
#> Version 4: Removing both between-woman and study terms from the primary 
#> preregistered models 

  #> Model  (same as model 9 swapping EPinterest for EPattract)
  EP_PRCPS = list(
    name = "Removing between-woman and study terms from the primary preregistered
    PRC model",
    data = "df",
    fixed_effects =
      c("menses", "Zprc_stirn_ww", 
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprc_stirn_ww",
        "Zp_sexattract : Zprc_stirn_ww",
        "menses : Zs_sexattract", "menses : Zp_sexattract"),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  #Model 
  EP_HPS = list(
    name = "Removing between-woman and study terms from the primary preregistered
    E and P model",
    data = "df",
    fixed_effects = 
      c("menses", "Zestr_ww", "Zprog_ww",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
        "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
        "menses : Zs_sexattract", "menses : Zp_sexattract"),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ),#end of model 
  


#> Version 5: Removing self sexual attractiveness terms from the primary preregistered
#> models 

  #> Model 
  EP_PRCPMSy = list( 
    name = "Removing self sexual attractiveness terms from the primary preregistered 
    PRC model",
    data = "df",
    fixed_effects =
      c("menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
        "Zp_sexattract",
        "Zp_sexattract : Zprc_stirn_ww",
        "Zp_sexattract : Zprc_stirn_mean",
        "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zprc_stirn_ww", 
        "Zstudy : Zprc_stirn_mean", 
        "Zstudy : Zp_sexattract", 
        "Zstudy : Zp_sexattract : Zprc_stirn_ww",
        "Zstudy : Zp_sexattract : Zprc_stirn_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  

  # Model 
  EP_HPMSy = list(
    name = "Removing self sexual attractiveness from the primary preregistered E and
      P model",
    data = "df",
    fixed_effects = 
      c("menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
        "Zp_sexattract",
        "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
        "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
        "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
        "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
        "Zstudy : Zp_sexattract", 
        "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
        "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract :
     Zestr_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 

  

#> Version 6: Only including partner sexual attractiveness and its interactions
#> with (1) PRC and (2) E & P 

  #> Model  (same as model 10 swapping EPinterest for EPattract)
  EP_PRCP = list(
    name = "Extra pair interest predicted by probability of conception, partner sexual 
    attractiveness, and the interaction between probability of conception and 
    partner sexual attractiveness",
    data = "df",
    fixed_effects =
      c("menses", "Zprc_stirn_ww", "Zp_sexattract",
        "Zp_sexattract : Zprc_stirn_ww",
        "menses : Zp_sexattract"),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  
  #> Model 
  EP_HP = list(
    name = "Extra pair interest predicted by hormones (E and P), partner sexual
    attractiveness, and the interactions between hormones and partner sexual 
    attractiveness",
    data = "df",
    fixed_effects = c("menses", "Zestr_ww", "Zprog_ww",
                      "Zp_sexattract",
                      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
                      "menses : Zp_sexattract"),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model


#> Version 7: Raw hormone analyses (no Probability of Conception models)

  EP_RHPSMSy = list(
  name = "Primary preregistered hormone (E and P) model swapping out log-transformed
  hormone values for raw hormone values",
  data = "df",
  fixed_effects = c(
    "menses", "Zstudy", "Zrawestr_ww", "Zrawprog_ww", "Zrawestr_mean", "Zrawprog_ww",
    "Zs_sexattract", "Zp_sexattract",
    "Zs_sexattract : Zrawprog_ww", "Zs_sexattract : Zrawestr_ww",
    "Zs_sexattract : Zrawprog_mean", "Zs_sexattract : Zrawestr_mean",
    "Zp_sexattract : Zrawprog_ww", "Zp_sexattract : Zrawestr_ww",
    "Zp_sexattract : Zrawprog_mean", "Zp_sexattract : Zrawestr_mean",
    "menses : Zs_sexattract", "menses : Zp_sexattract",
    "Zstudy : menses", "Zstudy : Zrawestr_ww", "Zstudy : Zrawprog_ww",
    "Zstudy : Zrawestr_mean", "Zstudy : Zrawprog_mean",
    "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
    "Zstudy : Zs_sexattract : Zrawprog_ww", "Zstudy : Zs_sexattract : Zrawestr_ww",
    "Zstudy : Zs_sexattract : Zrawprog_mean", "Zstudy : Zs_sexattract : Zrawestr_mean",
    "Zstudy : Zp_sexattract : Zrawprog_ww", "Zstudy : Zp_sexattract : Zrawestr_ww",
    "Zstudy : Zp_sexattract : Zrawprog_mean", "Zstudy : Zp_sexattract : Zrawestr_mean",
    "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
  ),
  random_effects = c("1", "Zrawestr_ww", "Zrawprog_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
)



  
) #end of core model list 


#> Version 8: Both preregistered models conducted in each study separately. 


#> Two separate Study 1 Models 

# Model 1
mdls$EP_PRCPSMs1 <- list(
  name = "Primary preregistered PRC model with study 1 participants",
  data = "df_study1",
  fixed_effects = c(
    "menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
    "Zs_sexattract", "Zp_sexattract",
    "Zs_sexattract : Zprc_stirn_ww",
    "Zs_sexattract : Zprc_stirn_mean",
    "Zp_sexattract : Zprc_stirn_ww",
    "Zp_sexattract : Zprc_stirn_mean",
    "menses : Zs_sexattract", "menses : Zp_sexattract"
  ),
  random_effects = c("1", "Zprc_stirn_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
)

# Model 2
mdls$EP_HPSMs1 <- list(
  name = "Primary preregistered hormone model with study 1 participants",
  data = "df_study1",
  fixed_effects = c(
    "menses","Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
    "Zs_sexattract", "Zp_sexattract",
    "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
    "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
    "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
    "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
    "menses : Zs_sexattract", "menses : Zp_sexattract"
  ),
  random_effects = c("1", "Zestr_ww", "Zprog_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
)


#> Two separate Study 2 Models 

# Model 1
mdls$EP_PRCPSMs2 <- list(
  name = "Primary preregistered PRC model with study 2 participants",
  data = "df_study2",
  fixed_effects = c(
    "menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
    "Zs_sexattract", "Zp_sexattract",
    "Zs_sexattract : Zprc_stirn_ww",
    "Zs_sexattract : Zprc_stirn_mean",
    "Zp_sexattract : Zprc_stirn_ww",
    "Zp_sexattract : Zprc_stirn_mean",
    "menses : Zs_sexattract", "menses : Zp_sexattract"
  ),
  random_effects = c("1", "Zprc_stirn_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
)

# Model 2
mdls$EP_HPSMs2 <- list(
  name = "Primary preregistered hormone model with study 2 participants",
  data = "df_study2",
  fixed_effects = c(
    "menses","Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
    "Zs_sexattract", "Zp_sexattract",
    "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
    "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
    "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
    "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
    "menses : Zs_sexattract", "menses : Zp_sexattract"
  ),
  random_effects = c("1", "Zestr_ww", "Zprog_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
)



# study1_mdls <-  grep("^(?!df)[A-Za-z]{2}_.*(?<![mp])1$", ls(), perl = TRUE, value = TRUE)
# #> (?!df) excludes things that start with "df" 
# #> [A-Za-z]{2} matches any upper/lower case characters for the first two characters
# #> .* matches anything
# #> "?<!" Here, the "<" means look behind. So it checks what comes before the "1" here.
# #> If I were to just use "?!" it would be looking ahead of its current position.
# #> It means "match a 1 at the end of the string only if it is not immediately preceded by 
# #> an m or p here 
# study1_mdls
# study1_mdls <- mget(study1_mdls)


#> Creating a list for the Extra-Pair Interest components: 

EAmdls <- vector("list")
#extra-pair attraction models 
for(i in seq_along(mdls)) {
  m <- mdls[[i]]
  model_name <- names(mdls)[i]
  m$name <- paste(model_name, "model swapping out extra-pair interests for its component
                   of extra-pair attraction")
  m$dependent_variable <- "ZEPattract"
  new_model_name <- gsub("^EP_", "EA_", model_name)
  EAmdls[[new_model_name]] <- m 
}

SCmdls <- vector("list")
#extra-pair attraction models 
for(i in seq_along(mdls)) {
  m <- mdls[[i]]
  model_name <- names(mdls)[i]
  m$name <- paste(model_name, "model swapping out extra-pair interests for its component
                   of extra-pair sex (scenario question)")
  m$dependent_variable <- "Zsextoday"
  new_model_name <- gsub("^EP_", "SC_", model_name)
  SCmdls[[new_model_name]] <- m 
}
rm(m, i, model_name, new_model_name)


#> Primary preregistered models explicitly defining plus 1 and minus 1 variables 
#> to probe simple slopes. 

#> Model 
EP_PRCPSMSyp1 <- list(
  name = "Extra pair interest, Probability of conception, Zp_sexattract_p1,
   Self sexual attractiveness, Women Mean predictors, Study",
  data = "df",
  fixed_effects =
    c("menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract_p1",
      "Zs_sexattract : Zprc_stirn_ww",
      "Zs_sexattract : Zprc_stirn_mean",
      "Zp_sexattract_p1 : Zprc_stirn_ww",
      "Zp_sexattract_p1 : Zprc_stirn_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract_p1",
      "Zstudy : menses", "Zstudy : Zprc_stirn_ww", 
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract_p1", 
      "Zstudy : Zs_sexattract : Zprc_stirn_ww",
      "Zstudy : Zs_sexattract : Zprc_stirn_mean",
      "Zstudy : Zp_sexattract_p1 : Zprc_stirn_ww",
      "Zstudy : Zp_sexattract_p1 : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract_p1"),
  random_effects = c(1, "Zprc_stirn_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
) 

#> Model 
EP_PRCPSMSym1 <- list(
  name = "Extra pair interest, Probability of conception, Zp_sexattract_m1,
   Self sexual attractiveness, Women Mean predictors, Study",
  data = "df",
  fixed_effects =
    c("menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract_m1",
      "Zs_sexattract : Zprc_stirn_ww",
      "Zs_sexattract : Zprc_stirn_mean",
      "Zp_sexattract_m1 : Zprc_stirn_ww",
      "Zp_sexattract_m1 : Zprc_stirn_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract_m1",
      "Zstudy : menses", "Zstudy : Zprc_stirn_ww", 
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract_m1", 
      "Zstudy : Zs_sexattract : Zprc_stirn_ww",
      "Zstudy : Zs_sexattract : Zprc_stirn_mean",
      "Zstudy : Zp_sexattract_m1 : Zprc_stirn_ww",
      "Zstudy : Zp_sexattract_m1 : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract_m1"),
  random_effects = c(1, "Zprc_stirn_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
) 

#Model 
EP_HPSMSyp1 <- list(
  name = "Extra pair interest, Progesterone and Estradial hormones, Zp_sexattract_p1, Self sexual attractiveness, Women mean predictors, Study",
  data = "df",
  fixed_effects = 
    c("menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract_p1",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract_p1 : Zprog_ww", "Zp_sexattract_p1 : Zestr_ww",
      "Zp_sexattract_p1 : Zprog_mean", "Zp_sexattract_p1 : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract_p1",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract_p1", 
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract :
   Zestr_mean",
      "Zstudy : Zp_sexattract_p1 : Zprog_ww", "Zstudy : Zp_sexattract_p1 : Zestr_ww",
      "Zstudy : Zp_sexattract_p1 : Zprog_mean", "Zstudy : Zp_sexattract_p1 :
   Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract_p1"),
  random_effects = c(1, "Zestr_ww", "Zprog_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
) 

#Model 
EP_HPSMSym1 <- list(
  name = "Extra pair interest, Progesterone and Estradial hormones, Zp_sexattract_m1, Self sexual attractiveness, Women mean predictors, Study",
  data = "df",
  fixed_effects = 
    c("menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract_m1",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract_m1 : Zprog_ww", "Zp_sexattract_m1 : Zestr_ww",
      "Zp_sexattract_m1 : Zprog_mean", "Zp_sexattract_m1 : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract_m1",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract_m1", 
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract :
   Zestr_mean",
      "Zstudy : Zp_sexattract_m1 : Zprog_ww", "Zstudy : Zp_sexattract_m1 : Zestr_ww",
      "Zstudy : Zp_sexattract_m1 : Zprog_mean", "Zstudy : Zp_sexattract_m1 :
   Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract_m1"),
  random_effects = c(1, "Zestr_ww", "Zprog_ww"),
  grouping_variable = "PROLIFIC_PID",
  dependent_variable = "ZEPinterest"
) 


moderation_mdls <- ls(pattern = "^EP_.*(p1$|m1$)")
moderation_mdls
moderation_mdls <- mget(moderation_mdls)

IPmdls <- list(
  
  #> In-Pair Models 
  
  
  
  # Model 1 (probability of conception)
  IP_PRCPSMSy = list(
    name = "In pair interest as the outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy",  "Zprc_stirn_ww",  "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprc_stirn_ww", 
      "Zs_sexattract : Zprc_stirn_mean", 
      "Zp_sexattract : Zprc_stirn_ww", 
      "Zp_sexattract : Zprc_stirn_mean", 
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses",  "Zstudy : Zprc_stirn_ww",
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zs_sexattract : Zprc_stirn_mean", 
      "Zstudy : Zp_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zp_sexattract : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZIPinterest"
  ),
  
  
  # Model 1 (hormones)
  IP_HPSMSy = list(
    name = "In pair interest as the outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZIPinterest"
  ),
  
  #Model 2 (Probability of conception)
  IA_PRCPSMSy = list(
    name = "In pair attraction as the outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy",  "Zprc_stirn_ww",  "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprc_stirn_ww", 
      "Zs_sexattract : Zprc_stirn_mean", 
      "Zp_sexattract : Zprc_stirn_ww", 
      "Zp_sexattract : Zprc_stirn_mean", 
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses",  "Zstudy : Zprc_stirn_ww",
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zs_sexattract : Zprc_stirn_mean", 
      "Zstudy : Zp_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zp_sexattract : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZIPattract"
  ),
  
  # Model 2 (hormones)
  IA_HPSMSy = list(
    name = "In pair attraction as the outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZIPattract"
  ),
  
  # IA_HPS = list(
  #   name = "In pair attraction as the outcome",
  #   data = "df",
  #   fixed_effects = c(
  #     "menses", "Zestr_ww", "Zprog_ww",
  #     "Zs_sexattract", "Zp_sexattract",
  #     "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
  #     "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
  #     "menses : Zs_sexattract", "menses : Zp_sexattract",
  #     "menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
  #   ),
  #   random_effects = c("1", "Zestr_ww", "Zprog_ww"),
  #   grouping_variable = "PROLIFIC_PID",
  #   dependent_variable = "ZIPattract"
  # ),
  
  
  # Model 3 (Probability of conception)
  S2_PRCPSMSy = list(
    name = "Self sexual initiation with partner as the outcome.",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy",  "Zprc_stirn_ww",  "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprc_stirn_ww", 
      "Zs_sexattract : Zprc_stirn_mean", 
      "Zp_sexattract : Zprc_stirn_ww", 
      "Zp_sexattract : Zprc_stirn_mean", 
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses",  "Zstudy : Zprc_stirn_ww",
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zs_sexattract : Zprc_stirn_mean", 
      "Zstudy : Zp_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zp_sexattract : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex2init"
  ),

  
  # Model 3 (hormones)
  S2_HPSMSy = list(
    name = "Self sexual initiation with partner as the outcome.",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex2init"
  ),
  
  # Model 3 (hormones estradiol random slope dropped)
  S2_HPSMSy_e = list(
    name = "Self sexual initiation with partner as the outcome.
    Dropping estradiol random slope because of model convergence issues",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex2init"
  ),
  
  
  
  # Model 4 (probability of conception)
  GN_PRCPSMSy = list(
    name = "General sexual desire as the outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy",  "Zprc_stirn_ww",  "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprc_stirn_ww", 
      "Zs_sexattract : Zprc_stirn_mean", 
      "Zp_sexattract : Zprc_stirn_ww", 
      "Zp_sexattract : Zprc_stirn_mean", 
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses",  "Zstudy : Zprc_stirn_ww",
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zs_sexattract : Zprc_stirn_mean", 
      "Zstudy : Zp_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zp_sexattract : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZIP1"
  ),
  
  # Model 4 (hormones)
  GN_HPSMSy = list(
    name = "General sexual desire as the outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZIP1"
  ),
  
  
  # Model 5 (probability of conception)
  S1_PRCPSMSy = list(
    name = "Total number of times engaged in sexual activity with partner as outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy",  "Zprc_stirn_ww",  "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprc_stirn_ww", 
      "Zs_sexattract : Zprc_stirn_mean", 
      "Zp_sexattract : Zprc_stirn_ww", 
      "Zp_sexattract : Zprc_stirn_mean", 
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses",  "Zstudy : Zprc_stirn_ww",
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zs_sexattract : Zprc_stirn_mean", 
      "Zstudy : Zp_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zp_sexattract : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex1"
  ),
  
  # Model 5 (hormones)
  S1_HPSMSy = list(
    name = "Total number of times engaged in sexual activity with partner as outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex1"
  ),
  
  # Model 5 (progesterone random slope dropped)
  S1_HPSMSy_p = list(
    name = "Total number of times engaged in sexual activity with partner as outcome. 
    Dropping progesterone random slope because of model convergence issues",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract"
    ),
    random_effects = c("1", "Zestr_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex1"
  ),
  
  
  # Model 6 (probability of conception)
  S3_PRCPSMSy = list(
    name = "Partner sexual initiation with self as outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy",  "Zprc_stirn_ww",  "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprc_stirn_ww", 
      "Zs_sexattract : Zprc_stirn_mean", 
      "Zp_sexattract : Zprc_stirn_ww", 
      "Zp_sexattract : Zprc_stirn_mean", 
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses",  "Zstudy : Zprc_stirn_ww",
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zs_sexattract : Zprc_stirn_mean", 
      "Zstudy : Zp_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zp_sexattract : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract", "Zsex4reject"
    ),
    random_effects = c("1", "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex3Pinit"
  ),
  
  # Model 6 (hormones)
  S3_HPSMSy = list(
    name = "Partner sexual initiation with self as outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract", "Zsex4reject"
    ),
    random_effects = c("1", "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex3Pinit"
  ),
  
  # Model 6 (probability of conception)
  S4_PRCPSMSy = list(
    name = "Number of sexual advances by partner that were rejected by self as outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy",  "Zprc_stirn_ww",  "Zprc_stirn_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprc_stirn_ww", 
      "Zs_sexattract : Zprc_stirn_mean", 
      "Zp_sexattract : Zprc_stirn_ww", 
      "Zp_sexattract : Zprc_stirn_mean", 
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses",  "Zstudy : Zprc_stirn_ww",
      "Zstudy : Zprc_stirn_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zs_sexattract : Zprc_stirn_mean", 
      "Zstudy : Zp_sexattract : Zprc_stirn_ww", 
      "Zstudy : Zp_sexattract : Zprc_stirn_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract", "Zsex3Pinit"
    ),
    random_effects = c("1", "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex4reject"
  ),
  
  # Model 7 (hormones)
  S4_HPSMSy = list(
    name = "Number of sexual advances by partner that were rejected by self as outcome",
    data = "df",
    fixed_effects = c(
      "menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
      "Zs_sexattract", "Zp_sexattract",
      "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
      "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
      "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
      "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
      "menses : Zs_sexattract", "menses : Zp_sexattract",
      "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
      "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
      "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract",
      "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
      "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract : Zestr_mean",
      "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
      "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract : Zestr_mean",
      "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract", "Zsex3Pinit"
    ),
    random_effects = c("1", "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "Zsex4reject"
  )
  
)

#> In-pair simple slopes (I have code for just using emmeans to do this later
#> now)

# # Model 1
# Sex2_PRCPSp1 <- list(
#   name = "Sex init, Probability of conception, Partner sexual attractiveness, Self sexual attractiveness",
#   data = "df",
#   fixed_effects = c(
#     "menses", "Zprc_stirn_ww",
#     "Zs_sexattract", "Zp_sexattract_p1",
#     "Zs_sexattract : Zprc_stirn_ww",
#     "Zp_sexattract_p1 : Zprc_stirn_ww",
#     "menses : Zs_sexattract", "menses : Zp_sexattract_p1"
#   ),
#   random_effects = c("1", "Zprc_stirn_ww"),
#   grouping_variable = "PROLIFIC_PID",
#   dependent_variable = "Zsex2init"
# )
# 
# # Model 2
# Sex2_PRCPSm1 <- list(
#   name = "Sex init, Probability of conception, Partner sexual attractiveness, Self sexual attractiveness",
#   data = "df",
#   fixed_effects = c(
#     "menses", "Zstudy", "Zprc_stirn_ww", 
#     "Zs_sexattract", "Zp_sexattract_m1",
#     "Zs_sexattract : Zprc_stirn_ww",
#     "Zp_sexattract_m1 : Zprc_stirn_ww",
#     "menses : Zs_sexattract", "menses : Zp_sexattract_m1"
#   ),
#   random_effects = c("1", "Zprc_stirn_ww"),
#   grouping_variable = "PROLIFIC_PID",
#   dependent_variable = "Zsex2init"
# )
# 
# IP_moderation <- ls(pattern = "^Sex2_.*[mp]1$")
# IP_moderation <- mget(IP_moderation)
extras <- list(
  
  #> Model 
  EP_PRCPSMSyAR = list(
    name = "Primary preregistered Probability of Conception model reported in 
    manuscript",
    data = "df",
    fixed_effects =
      c("menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprc_stirn_ww",
        "Zs_sexattract : Zprc_stirn_mean",
        "Zp_sexattract : Zprc_stirn_ww",
        "Zp_sexattract : Zprc_stirn_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zprc_stirn_ww", 
        "Zstudy : Zprc_stirn_mean",
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprc_stirn_ww",
        "Zstudy : Zs_sexattract : Zprc_stirn_mean",
        "Zstudy : Zp_sexattract : Zprc_stirn_ww",
        "Zstudy : Zp_sexattract : Zprc_stirn_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract",
        "age_1", "ln_rellength"),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  #Model  
  EP_HPSMSyAR = list(
    name = "Primary preregistered estradiol and progesterone model reported in 
    manuscript",
    data = "df",
    fixed_effects = 
      c("menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
        "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
        "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
        "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
        "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
        "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract :
   Zestr_mean",
        "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
        "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract :
   Zestr_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract",
        "age_1", "ln_rellength"),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  #> Model 
  EP_PRCPSMSyARC = list(
    name = "Primary preregistered Probability of Conception model reported in 
    manuscript",
    data = "df",
    fixed_effects =
      c("menses", "Zstudy", "Zprc_stirn_ww", "Zprc_stirn_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprc_stirn_ww",
        "Zs_sexattract : Zprc_stirn_mean",
        "Zp_sexattract : Zprc_stirn_ww",
        "Zp_sexattract : Zprc_stirn_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zprc_stirn_ww", 
        "Zstudy : Zprc_stirn_mean",
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprc_stirn_ww",
        "Zstudy : Zs_sexattract : Zprc_stirn_mean",
        "Zstudy : Zp_sexattract : Zprc_stirn_ww",
        "Zstudy : Zp_sexattract : Zprc_stirn_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract",
        "age_1", "ln_rellength", "children1"),
    random_effects = c(1, "Zprc_stirn_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ), #end of model 
  
  #Model  
  EP_HPSMSyARC = list(
    name = "Primary preregistered estradiol and progesterone model reported in 
    manuscript",
    data = "df",
    fixed_effects = 
      c("menses", "Zstudy", "Zestr_ww", "Zprog_ww", "Zestr_mean", "Zprog_mean",
        "Zs_sexattract", "Zp_sexattract",
        "Zs_sexattract : Zprog_ww", "Zs_sexattract : Zestr_ww",
        "Zs_sexattract : Zprog_mean", "Zs_sexattract : Zestr_mean",
        "Zp_sexattract : Zprog_ww", "Zp_sexattract : Zestr_ww",
        "Zp_sexattract : Zprog_mean", "Zp_sexattract : Zestr_mean",
        "menses : Zs_sexattract", "menses : Zp_sexattract",
        "Zstudy : menses", "Zstudy : Zestr_ww", "Zstudy : Zprog_ww",
        "Zstudy : Zestr_mean", "Zstudy : Zprog_mean",
        "Zstudy : Zs_sexattract", "Zstudy : Zp_sexattract", 
        "Zstudy : Zs_sexattract : Zprog_ww", "Zstudy : Zs_sexattract : Zestr_ww",
        "Zstudy : Zs_sexattract : Zprog_mean", "Zstudy : Zs_sexattract :
   Zestr_mean",
        "Zstudy : Zp_sexattract : Zprog_ww", "Zstudy : Zp_sexattract : Zestr_ww",
        "Zstudy : Zp_sexattract : Zprog_mean", "Zstudy : Zp_sexattract :
   Zestr_mean",
        "Zstudy : menses : Zs_sexattract", "Zstudy : menses : Zp_sexattract",
        "age_1", "ln_rellength", "children1"),
    random_effects = c(1, "Zestr_ww", "Zprog_ww"),
    grouping_variable = "PROLIFIC_PID",
    dependent_variable = "ZEPinterest"
  ) #end of model 

  
)


#> Model Selection Vectors ----

# core models 
ep <- names(mdls)

# extra-pair attraction models 
ea <- names(EAmdls)

# extra-pair scenario models 
sc <- names(SCmdls)

# 7 In-pair models 
ip <- names(IPmdls)

ex <- names(extras)

# 4 simple slope probing models 
ss <- names(moderation_mdls)

#> combining models into a single list 
mdls <- c(mdls, EAmdls, SCmdls, IPmdls, extras, moderation_mdls)


#> I had ChatGPT write this little helper function to remove the extra list objects
#> from the environment. Just to help keep the environment clean. 
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
clean_model_lists(IPmdls, EAmdls, SCmdls, extras, moderation_mdls)
