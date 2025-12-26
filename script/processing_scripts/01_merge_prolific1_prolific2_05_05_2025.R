
library(tidyverse)
library(labelled) #for spss labels 
library(codechest) ##nice GitHub package with many utility functions  
library(haven)

#> RecipientFirstName#> Clear up environment 
rm(list = ls())

#> Read in dataframes 
p1 <- readRDS("data/analysis_data/prolific1_full.rds")
p2 <- readRDS("data/analysis_data/prolific2_full.rds")

#> Creating dataset identifier 
p1$study <- 1
p2$study <- 2

#> Read in variable matching file 
match <- read_csv("data/input_data/prolif1_prolif2_dataset_connections.csv")

#the "seepart" variable isn't in first dataframe but to get it to transfer in I'm just 
#going to create it as missing so the values come in for the second df. 
match[match$`PROLIFIC 2` == "seepart" & !is.na(match$`PROLIFIC 2`), "PROLIFIC 1"] <- "seepart" 
match[match$`PROLIFIC 2` == "seepart" & !is.na(match$`PROLIFIC 2`), "PROLIFIC 1"]
#adding it to the dataframe as NA too 
p1$seepart <- NA


match[grepl("state", match$`PROLIFIC 1`), 1]
match[grepl("state", match$`PROLIFIC 1`), 2]

#adding in the regular secure attachment variable from each dataframe. 
#this function adds in new mapping variables 
# match_df = "match" (the matching df to add to)
# match_vector = named vector where 
#> p1 = p2, 
#> p1 = p2 
#> See below for vector to add variables to. 
add_mapping <- function(match_df, match_vector) {
 
   for (i in seq_along((match_vector))) {
     p1 <- names(match_vector)[i]
     p2 <- unname(match_vector[i])

     new_row <- setNames(
       data.frame(p1, p2, stringsAsFactors = FALSE),
       names(match_df)  # this guarantees the names match
     )
     match_df <- rbind(match_df, new_row)
   }
  
  return(match_df)
  
#> The below code works and doesn't need a for loop 
  # new_rows <- data.frame(
  #   `PROLIFIC 1` = names(match_vector),
  #   `PROLIFIC 2` = unname(match_vector),
  #   stringsAsFactors = FALSE
  # )
  # 
  # # Ensure column names line up exactly with match_df
  # names(new_rows) <- names(match_df)
  # 
  # rbind(match_df, new_rows)
   
}

#> This is useful because as I create variables as needed, I can just add them
#> in here. 
add_variables <- c(
  #p1 (first study) variable = p2 (second study) variable 
  "state_anxious" = "state_anx"
)

match <- add_mapping(match, add_variables)


# Ensure both p1 and p2 contain all variables listed in the match table
#> This is done because there are instances where there is a variable that IS in
#> one study dataframe, but it is blank for the other study. This duplicates the 
#> study name across both spots in the matching list to ensure that for those few
#> variables that don't have matches across studies can still be brought into the
#> full dataset. 
for (i in seq_len(nrow(match))) { 
  #this loops through the numbers of the length of match, 
  # i = 1 to 235 (or more if match increases in length)
#this selects the variable name in the first row for each column
  var1 <- match$`PROLIFIC 1`[i]  
  var2 <- match$`PROLIFIC 2`[i]

  
  # If var1 is NA but var2 is not, copy var2 to var1 and create NA column in p1
  if (is.na(var1) && !is.na(var2)) {
    match[i, 'PROLIFIC 1'] <- var2
    p1[[var2]] <- NA
  }
  
  # If var2 is NA but var1 is not, copy var1 to var2 and create NA column in p2
  if (is.na(var2) && !is.na(var1)) {
    match[i, 'PROLIFIC 2'] <- var1
    p2[[var1]] <- NA
  }
  
}


#renaming this so it matches 
p2$Durationinseconds <- p2$Duration_in_seconds_
p2$Duration_in_seconds_ <- NULL

#I duplicated this variable for the purpose of bringing it into SPSS and double
#checking a few things. Want to use the original variable name now. 
p2$studyday <- NULL

p1_cols <- match$`PROLIFIC 1`
p2_cols <- match$`PROLIFIC 2`

#grabbing the p1 column names from the matching list that are not in my dataset
p1_cols[!p1_cols %in% colnames(p1)]

#grabbing the p2 column names from the matching list that are not in my dataset 
p2_cols[!p2_cols %in% colnames(p2)]

#> In other words, above I am grabbing all of the variable names in the matching
#> dataset which I don't have direct matches for in the dataset

p2$StartDate <- as.POSIXct(p2$StartDate, format = "%Y-%m-%d %H:%M:%S")
p2$EndDate <- as.POSIXct(p2$EndDate, format = "%Y-%m-%d %H:%M:%S")
p2$RecordedDate <- as.POSIXct(p2$RecordedDate, format = "%Y-%m-%d %H:%M:%S")


#> 11/8/25: trying to identify if the matching list has any duplications (i.e.,
#> incorrect values). Just one instance where two different variables from p1
#> are set to equal "childhealth" from p2. The childhealth composites don't appear
#> to even be created in my version of the p1 dataset, so its not an issue for 
#> the time being. 
# #> Check to see if there are any duplicated variables: 
# unique(p1_cols[duplicated(p1_cols)]) #none 
# unique(p2_cols[duplicated(p2_cols)])
# match[grepl("childhealth", p2_cols), ]

#> Before I bind the dataframes together, I am going to create a named vector from
#> the matching list and then I rename all of the p2 variables to have the names
#> of the p1 variables. So, the final dataset variable names will match names from
#> p1. After the renaming, I can easily rbind all of the common columns. 

#> Create a named vector for renaming. 
#> The "names" attribute is the current prolific 2 names, the actual values are
#> desired column names from prolific 1
rename_vector <- setNames(match$`PROLIFIC 1`, match$`PROLIFIC 2`)
rename_vector
#> Find the matching columns 
matched_cols <- names(p2) %in% names(rename_vector) #true/false vector 

#> Apply renaming only to matched columns 
names(p2)[matched_cols] <- rename_vector[names(p2)[matched_cols]]

#> Find common column names 
common_cols <- intersect(names(p1), names(p2))

#> Bind together all of the common columns 
df <- rbind(p1[, common_cols], p2[, common_cols])


#> Add in non-common columns that are needed 

p2_add <- c("energy1", "energy2", "energy",
            "negaffect1", "negaffect2", "neg_affect")

df <- df |> left_join(p2[, c("PROLIFIC_PID", "studyday", p2_add)])


#> Creating a few extra variables before filtering -------

#> raw hormone values 
df$rawestr <- 2.71828^df$estr
df$rawprog <- 2.71828^df$prog
#does the same thing as exp(df$estr)

#> no/yes coding for whether someone has a child with their current partner
df$children_current <- ifelse(df$child_number_current_1 == 0 & !is.na(df$child_number_1), 0, df$children)


df$bc_day #same as RCD (reverse count day) in Schleifenbaum 

curcycle <- read.csv("data/input_data/PROLIFIC_PID_with_CURR_CYCLENGTH_MERGE_dec9_2025.csv")
curcycle$PROLIFIC_PID1 <- curcycle$PROLIFIC_PID
curcycle$PROLIFIC_PID <- NULL
#> Still some duplicates here. I can just take a single instance for right now. 
curcycle <- curcycle[!duplicated(curcycle[, c("PROLIFIC_PID1", "studyday")]), ]
#> adding in the current cycle length to the dataset

df <- df |> left_join(curcycle, by = join_by(PROLIFIC_PID1, studyday))

df <- df |> 
  mutate(
    RCD_STAND =
      case_when(
        bc_day >= -14 & bc_day <= -1 &
          CURR_CYCLENGTH >= 18 & CURR_CYCLENGTH <= 40 ~ bc_day,
        
        bc_day < -14 &
          CURR_CYCLENGTH >= 18 & CURR_CYCLENGTH <= 40 ~
          ((CURR_CYCLENGTH + bc_day + 1) / (CURR_CYCLENGTH - 14) * 15) - 30
      ),
    
    RCD_STAND = round(RCD_STAND, 1),
    RCD_STAND_RND = round(RCD_STAND, 0)
  )


#> Dataframe filtering STUDY 1 -------


excluded_ids1 <- c("p1.104", "p1.185", "p1.247", "p1.298", "p1.328", "p1.381",
                  "p1.278", "p1.186", "p1.204", "p1.221", "p1.302", "p1.321", "p1.132",
                  "p1.324", "p1.253", "p1.370", "p1.243", "p1.155", "p1.197", "p1.389",
                  "p1.198", "p1.225", "p1.236", "p1.251", "p1.274", "p1.252")


long_cycles1 <- c("p1.101", "p1.329", "p1.105", "p1.385", "p1.124", "p1.176",
                 "p1.392")


short_cycles1 <- c("p1.252")


# Creating the variable to remove people who have too few backward count days 
condition <- df |> group_by(PROLIFIC_PID) |> 
  filter(!is.na(estr_b)) |> 
  count(PROLIFIC_PID, name = "bc_days_n") 
condition
df <- df |> left_join(condition) 


filtered_df <- df |>
  filter(
    ifelse(
      study == 1,
      bc_days_n >= 10 &
        !(PROLIFIC_PID %in% excluded_ids1) &
        !(PROLIFIC_PID == 'p1.144' & studyday > 15) &
        !(PROLIFIC_PID %in% long_cycles1) &
        !(PROLIFIC_PID %in% short_cycles1),
      TRUE  # keep all rows where study != 1
    )
  )

#> Dataframe filtering STUDY 2 ------

#> Creating a variable to filter on number of valid backward count days 
#> pre-registration indicates a person must have at least 10 to be included
# condition <- df |> group_by(PROLIFIC_PID) |> 
#   filter(!is.na(bc_day)) |> 
#   count(PROLIFIC_PID, name = "bc_days_n") 
# condition
# df <- df |> left_join(condition)

exclude_ids2 <- c("p2.178", "p2.109", "p2.326", "p2.359", "p2.375", "p2.308", 
                 "p2.325", "p2.132", "p2.224", "p2.185", "p2.115", "p2.243", "p2.150", 
                 "p2.364", "p2.350")


#> These two cases had weird cycles, but we still had 10+days before their weird
#> cycle that we could include. So we don't lose these cases totally. 
special_case_1 <- "p2.264" # Include if study_day <= 12
special_case_2 <- "p2.161"  # Include if study_day <= 14


filtered_df <- filtered_df |> 
  filter(
    ifelse(
    study == 2,
    bc_days_n >= 10 &
      !PROLIFIC_PID %in% exclude_ids2 &
      (PROLIFIC_PID != special_case_1 | studyday <= 12) &
      (PROLIFIC_PID != special_case_2 | studyday <= 14),
    TRUE  # keep all other studies (e.g., study 1)
  )
  )


filtered_df |> group_by(study) |> 
  summarize(
    N = n(),
    NA_count = sum(!is.na(estr))
  ) 


#> Variable Creation function -------

# Variables to create Z-scores for across both studies 
Z_vars_full <- c("p_sexattract", "s_sexattract", "sextoday", "study",
                 "IP1", "sex1", "sex3Pinit", 
                 "sex4reject",  #added 10/28/25
                 "eat", "prog", "estr", "prc_stirn", #added 12/4/25
                 "eat1", "eat2" #added 12/11/2
                 ) 

# Variables to create Z-scores for each study (these will mostly be constructs
# created by averaging/summing other variables)
# The construct will need the same name to get included here, but it will be 
# calculated differently in each study.
# I might be able to create those variables by using a for loop over each element
# of this list and then use an ifelse() statement to scale each variables by study
Z_vars_by_study <- list(
  # "first_study_variable" = "second_study_short"
  "s_lovattach_scale" = "s_lovattachSHORT",
  "p_lovattach_scale" = "p_lovattachSHORT"
#> It appears that the state_anx variable short version may need to be created 
#> in the prolific1 file, to have both the short/long version. 
  #"state_anxious" = "state_anx"
)

#> In this case, the s (self) love attache scale has two versions: 
#> In the second study, we don't have the full version of the scale -- just the SHORT 
df[df$study == 1, "s_lovattach_scale"]
df[df$study == 2, "s_lovattach_scale"]

#> There is a short version of the s_lovattache_scale for both study 1 and study 2
#> However, the full version of the scale is only available for study 1. We want
#> variables that use the most information, so we will create one variable that 
#> creates the Z-scored variable by taking the full version from study 1 and the
#> short version from study 2 (as that is the only version)
df[df$study == 1, "s_lovattachSHORT"]
df[df$study == 2, "s_lovattachSHORT"]


# Variables to create mlm person-mean and within-person variables for  
mlm_variables <- c("prog", "estr", "prc_stirn", "Echg", "Pchg",
                   "rawestr", "rawprog" #added 10/28/25
                   )

#This function allows me to create all of the variables for each dataframe. I can 
#plug in the full dataframe and the filtered dataframe into this function to get 
#all of the results. 
calculate_variables <- function(df, Zvariables1, Zvariables2, mlm_variables) {
# Zvariables1 = Z-scores for variables that can happen across studies 
# Zvariables2 = Z-scores for variables that need to be done BY study (i.e., 
  #could have been created before the merge). This is supplied as a list. 
  #where study1 = study2 
# mlm_variables = all of the variables to disaggregate within and between-person
  #variance 
  
# Variables to Z-score after merge (all use the same items) ------

for (var in Zvariables1) {
  new_name <- paste0("Z", var)
  df[[new_name]] <- scale(df[[var]])[, 1]
}


# Variables to Z-score by study (i.e., could be Z-scored before merge) -----
# These are primarily variables for which different items went into the creation
# and we want to Z-score them so people have a similar standing on them irrespective
# to their raw values. 


combine_long_short <- function(df, Z_vars_by_study) {
  
  # Prefix ZLS = long-short composite variable 
  for (i in seq_along(Z_vars_by_study)) {
    
    # print(names(Z_vars_by_study)[i]) #extracts the list name part 
    # print(Z_vars_by_study[[i]]) #extracts the content of the list 
    
    #grab variables 
    study1_var <- names(Z_vars_by_study)[i]
    study2_var <- Z_vars_by_study[[i]]
    #create a final variable name by adding "ZLS" to study 1 variable name 
    combined_var <- paste0("ZLS", study1_var)
    #vector to select the final variables to grab for appending variable into
    #df. 
    final_vars <- c(combined_var, "PROLIFIC_PID")
    
    
    #create a temporary dataframe for study 1 variable.
    #need to filter down to unique prolific PID first because these are between
    #group variables 
    
    tdf1 <- df[df$study == 1 & !is.na(df[[study1_var]]), c(study1_var, "PROLIFIC_PID", "study")] #filtering down to just study 1 cases 
    tdf1 <- tdf1[!duplicated(tdf1$PROLIFIC_PID), c("PROLIFIC_PID", study1_var)] #filter down to unique participant IDS
    tdf1[combined_var] <- scale(tdf1[study1_var])[, 1] #scale the variable 
    tdf1 <- tdf1[, final_vars] #select only columns to include in final df 
    
    
    tdf2 <- df[df$study == 2, c(study2_var, "PROLIFIC_PID", "study")] #filtering down to just study 2 cases 
    tdf2 <- tdf2[!duplicated(tdf2$PROLIFIC_PID), c("PROLIFIC_PID", study2_var)] #filter down to unique participant IDS
    # NOTE: The variables are renamed here to be based on the study 1 variable names 
    tdf2[combined_var] <- scale(tdf2[study2_var])[, 1] #scale the variable 
    tdf2 <- tdf2[, final_vars] #select only columns to include in final df
    
    
    tdf <- rbind(tdf1[, final_vars], tdf2[, final_vars]) #bind those rows back together to bring back into df 
    
    #add the variable back into the dataframe
    df <- df |> left_join(tdf, by = join_by(PROLIFIC_PID))
    
  }
  
  return(df)  
  
} #end of combine_long_short function 

df <- combine_long_short(df, Z_vars_by_study)


# mlm person-mean and within-person variable creation -------


df <- mlm_groupmean(df, mlm_variables, "PROLIFIC_PID", within_affix = "_ww",
                    between_affix = "_mean", affix_type = "suffix", include_z = TRUE)

#> mlm_groupmean function was updated and the below code simplifies to the above
#> code
# library(rlang)
# 
# for (var in mlm_variables) {
#   
#   var_sym <- ensym(var) 
#   
#   df <- mlm_groupmean(df, PROLIFIC_PID, !!var_sym)
#   
# }
# 
# #> Renaming within and mean variables 
# bg_cols <- grepl("BG_", names(df))
# bg_cols
# names(df)[bg_cols] <- sub("BG_", "", names(df)[bg_cols])
# names(df)[bg_cols] <- paste0(names(df)[bg_cols], "_mean")
# 
# wg_cols <- grepl("WG_", names(df))
# wg_cols
# names(df)[wg_cols] <- sub("WG_", "", names(df)[wg_cols])
# names(df)[wg_cols] <- paste0(names(df)[wg_cols], "_ww")


return(df)





} #end of calculate variables function 


### Using "calculate_variables()" to make datasets -------

df_full <- calculate_variables(df, Z_vars_full, Z_vars_by_study, mlm_variables)

df_filt <- calculate_variables(filtered_df, Z_vars_full, Z_vars_by_study, mlm_variables)

# Quick check of combining short and long variables looks good. 
df_full  |> 
  filter(!duplicated(PROLIFIC_PID)) |> 
  group_by(study) |> summarize(
    #  mean = across(matches("ZLS"), ~ mean(., na.rm = TRUE)),
    #  sd = across(matches("ZLS"), \(x) sd(x, na.rm = TRUE)),
    across(matches("ZLS"), list(mean = ~mean(., na.rm =TRUE),
                                sd = ~sd(., na.rm = TRUE),
                                N = ~n(),
                                NA_count = ~sum(is.na(.))
    )
    )
  ) |>   pivot_longer(
    cols = -study,
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_?(mean|sd|N|NA_count)$",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from = stat,
    values_from = value
  )


#> #> height/weight recode -----------

#> Using the filtered dataframe at the participant level to look at BMI 
fdfp <- filtered_df[!duplicated(filtered_df$PROLIFIC_PID), ]

psych::describe(fdfp[, grepl("height|weight", names(fdfp))])

#> Seems like some particpants entered height/weight in a different metric. 

#> Prolific code: 
#> height_1 == 4, 5, 6
#> height_2 < 12
#> weight >= 90 and <= 322
#> 
#> IF  ((height_1 = 4 or height_1 =5 or height_1 = 6) and height_2 le 11) height_comp=height_1*12 + height_2.
# EXECUTE.
# 
# IF  ((height_1 = 4 or height_1 =5 or height_1 = 6) and height_2 le 11 and weight_1 ge 90 and 
#      weight_1 le 322) BMI=weight_1*703/(height_comp*height_comp).
# EXECUTE.
# 
# IF  (BMI ge 17 and BMI le 48) BMI_trimmed=BMI.
# EXECUTE.

# Compute height_comp only when height_1 is 4, 5, or 6 *and* height_2 ≤ 11
fdfp$height_in <- with(fdfp, ifelse(height_1 %in% c(4, 5, 6) & height_2 <= 11,
                                    height_1 * 12 + height_2,
                                    NA))
summary(fdfp$height_in)

# Compute BMI only when height_comp condition AND weight_1 between 90 and 322
fdfp$BMI <- with(fdfp, ifelse(height_1 %in% c(4, 5, 6) &
                                height_2 <= 11 &
                                weight_1 >= 90 & weight_1 <= 322,
                              weight_1 * 703 / (height_in^2),
                              NA))
summary(fdfp$BMI)

#> Here I get 7 participants who have weird BMI scores. 
fdfp[(fdfp$BMI < 17 | fdfp$BMI > 48) & !is.na(fdfp$BMI), ]


# Trim BMI to values between 17 and 48
fdfp$BMI_trimmed <- with(fdfp, ifelse(BMI >= 17 & BMI <= 48, BMI, NA))
summary(fdfp$BMI_trimmed)

#> Retains 90% of people 
50 / 484

#> merge the height, weight, and BMI variables back into the DF 

fdfp <- fdfp[, c("PROLIFIC_PID", "height_in", "BMI", "BMI_trimmed")]

df_filt <- df_filt |> left_join(fdfp, by = join_by(PROLIFIC_PID))


#### Updating some variables across datasets -------


#> As I go and make updates, there are certain specific variables that I need
#> updated and I need them updated for both datasets. 

dfs <- list(
  df_full = df_full,
  df_filt = df_filt
)

dfs <- lapply(dfs, function(x)
{
  #make the study variable a factor with specific levels 
  x$study <- factor(x$study,
                    levels = c(1, 2),
                    labels = c("Study 1", "Study 2"))
  #create simple effects analysis variables 
  x$Zp_sexattract_p1 <- x$Zp_sexattract + 1 #adding 1 makes the 0 point 1 SD below the mean 
  x$Zp_sexattract_m1 <- x$Zp_sexattract - 1 #subtracting 1 makes the 0 point 1 SD above the mean
  
  #ADD MORE CALLS HERE THAT NEED TO BE DONE TO BOTH DFS
  
  
  
  x #return the object
} ) 

#reassign dfs 
df_full <- dfs$df_full
df_filt <- dfs$df_filt

#> NOTE: There are issues with the reporting of weight, which will need to be
#> fixed if that variable is used. 
# df_full |> ggplot(aes(x = weight_1)) +
#   geom_histogram()
# summary(df$weight_1)
# df_full |> filter(weight_1 > 240)

#### Labelling Variables ---------


anchor_info <- list(
  likely7    = c("Not at all likely"           = 1,
                 "Niether likely nor unlikely" = 4,
                 "Very Likely"                 = 7),
  
  usual5     = c("Much less than usual"        = 1,
                 "About the same as usual"     = 3,
                 "Much more than usual"        = 5),
  
  agree7     = c("Very strongly disagree"      = 1,
                 "Neither agree nor disagree"  = 4,
                 "Strongly agree"              = 7),
  
  pastday7   = c("Not at all in the past day"  = 1,
                 "Very much in the past day"   = 7),
  
  noyes      = c("No"  = 0,
                 "Yes" = 1),
  
  count4     = c("0"         = 0,
                 "1"         = 1,
                 "2"         = 2,
                 "3"         = 3,
                 "4 or more" = 4),
  
  freq5      = c("Not at all"   = 0,
                 "A great deal" = 4),
  
  ethnic     = c("White"                                = "1",
                 "Black or African American"            = "2",
                 "American Indian or Alaska Native"     = "3",
                 "Asian"                                = "4",
                 "Native American or Parcific Islander" = "5",
                 "Other"                                = "6"),
  
  rel7       = c("Much less" = 1,
                 "Average"   = 4,
                 "Much more" = 7),
  
  agerel7    = c("At a much younger age" = 1,
                 "Around the same time"  = 4,
                 "At a much older age"   = 7),
  
  relstat    = c("We are married"                               = 1,
                 "We are engaged to be married"                 = 2,
                 "We are exclusively dating but not currently engaged or to be
                 married"                                       = 3,
                 "We are dating and date other people as well"  = 4)
)


variable_info <- list(
 
#### INITIAL SURVEY VARIABLES #####

 
  age_1 = list(
    label = "Participant age: What is your age?"
  ),
  height_1 = list(
    label = "Partipant height: feet"
  ),
  height_2 = list(
   label = "Participant height: inches"
  ),
  weight_1 = list(
   label = "Participant weight: what is your weight? (lbs)"
  ),
  height_in = list(
   label = "Participant height in Inches: height_1 * 12 + height_2"
 ),
  BMI = list(
    label = "Participant BMI (not trimming 7 extreme values"
  ),
  BMI_trimmed = list(
    label = "Participant BMI trimming values < 17 or > 48"
  ),
  ethnic = list(
    label = "Which ethnic group(s) best fits your own identity?",
    anchors = "ethnic"
  ),
  childhealth1 = list(
    label = "Relative to other students, how many days of elementary school,
    junor high, and high school did you miss because you were sick?",
    anchors = "rel7"
  ),
  childhealth2 = list(
    label = "Relative to most children, how healthy were you as a child",
    anchors = "rel7"
  ),
  menarche_age_1 = list(
    label = "At what age did you have your first menstrual period? (years of age)"
  ),
  pubtim1 = list(
    label = "Compared to my same sex peers, I started having periods _____",
    anchors = "agerel7"
  ),
  pubtime2 = list(
    label = "Compared to my same sex peers, I started development of breasts _____",
    anchors = "agerel7"
  ),
  pubtim3 = list(
    label = "Compared to my same sex peers, I started development of a 'womanly
    shape' _____",
    anchors = "agerel7"
  ),
#> NOTE: The relationship length variables need to be combined. These are not
#> linear transformations of each other. One is the years of the relationship and
#> then months is the additional number of months past how many years they have
#> been in a relationship
  rellength_1 = list(
    label = "How long have you been in a relationship with your partner (years)"
  ),
  rellength_2 = list(
    label = "How long have you been in a relationship with your partner (months)"
  ),
  rellength = list(
    label = "Recoded relationship length: years + (months / 12)"
  ),
  lnrellength = list(
    label = "Natural log of rellength - recoded relationship length"
  ),
  partner_age_1 = list(
    label = "What is your partners age? (years)"
  ),
  relstat = list(
    label = "What is the current status of your relationship?",
    anchors = "relstat"
  ),
  livepart = list(
    label = "Do you currently live with your partner?",
    anchors = "noyes"
  ),
  children = list(
    label = "Do you currently have any children?",
    anchors = "noyes"
  ),
  child_number_1 = list(
    label = "How many children do you have?"
  ),
  child_number_current_1 = list(
    label = "How many children do you have with your current partner?"
  ),
  children_current = list(
    label = "Do you have any children with your current partner?",
    anchors = "noyes"
  ),
  younchild_age_1 = list(
    label = "What is the age of your youngest child?"
  ),
  pattract1 = list(
    label = "Relative to most men of your partners age, how sexually attractive
    is your partner?",
    anchors = "rel7"
  ),
  pattract2 = list(
    label = "Relative to most men of your partners age, how attractive is your
    partner as a sexual partner?",
    anchors = "rel7"
  ),
  pattract3 = list(
    label = "Relative to most men of your partners age, how interested are others 
    in your partner as a sexual partner?",
    anchors = "rel7"
  ),
  sattract1 = list(
    label = "Relative to most women of your age, how sexually attractive are you?",
    anchors = "rel7"
  ),
  sattract2 = list(
    label = "Relative to most women of your age, how attractive are you as a 
    sexual partner?",
    anchors = "rel7"
  ),
  sattract3 = list(
    label = "Relative to most women of your age, how interested are others in 
    you as a sexual partner?",
    anchors = "rel7"
  ),

#### DAILY VARIABLES ####  
  
 menses = list(
   label = "Are you currently menstruating? That is, are you currently on your 
   period?",
   anchors = "noyes"
 ),
 menses_lastentry = list(
   label = "Did you have your period since your last daily entry",
   anchors = "noyes"
 ),
 menses_lastentrydate_1 = list(
   label = "On what date did your period begin? (On what date did you first 
   experience bleeding?) NOTE: DAY FIRST, THEN MONTH,THEN YEAR"
 ),


 IP1 = list( #NOTE: Was "GSD" in study 2
   label = "In the past 24 hours: I had strong feelings of sexual desire",
   anchors = "freq5"
 ),
 IP2 = list( #NOTE: Was "IP1" in study 2
   label = "I felt a strong sexual attraction toward my primary current partner",
   anchors = "freq5"
 ),
 IP3 = list( #NOTE: Was "IP2" in study 2 
   label = "I was strongly physically attracted to my primary partner",
   anchors = "freq5"
 ),
#> 9/25/2025: These variables are not yet in full dataset as they are only present
#> for study 2. Need to be careful with them when combinging data 
 # IP3 = list( #NOTE: Was absent in study 1 
 #   label = "I found my current partner kind of a sexual turn off",
 #   anchors = "freq5"
 # ),
 # IP4 = list( #NOTE: Was absent in study 1 
 #   label = "I felt 'grossed out' by the thought of being sexual with my partner",
 #   anchors = "freq5"
 # ),
 EP1 = list(
   label = "I felt strong sexual attraction toward someone other than my current
   partner",
   anchors = "freq5"
 ),
 EP2 = list(
   label = "I was strongly physically attracted to someone other than my current
   partner",
   anchors = "freq5"
 ),
 sex1 = list(
   label = "In the past 24 hours: On how many occasions did you engage in 
   sexual activity with your partner?",
   anchors = "count4"
 ),
 sex2init = list(
   label = "In the past 24 hours: On how many occasions did you initiate sexual
   activity with your partner?",
   anchors = "count4"
 ),
 sex3Pinit = list(
   label = "In the past 24 hours: On how many occasions did your partner initiate
   sexual activity with you (whether you accepted it or not)?",
   anchors = "count4"
 ),
 sex4reject = list(
   label = "In the past 24 hours: On how many occasions did you reject a sexual
   advance by your partner?",
   anchors = "count4"
 ),
#> NOTE: I believe this question was only asked in study 2 and is not yet in 
#> full data set (as of 9/25/2025)
 # sex5partreject = list(
 #   label = "In the past 24 hours: On how many occasions did your partner reject
 #   a sexual advance by you?",
 #   anchors = "count4"
 # ),
 sextoday = list(
    label = "Imagine that you met a very interesting and attractive 
 person today, and you were very sexually attracted to this person and this 
 person was very sexually attracted to you. Imagine that you have the evening 
 free and you are able to spend the evening with this person. How likely do you 
 think you would be to have sex with this person today, if you could be sure 
 that no one would ever find out about it?",
    anchors = "likely7"
   ),
 eat1 = list(
   label = "In the past 24 hours: How much did you eat?",
   anchors = "usual5"
   ),
 eat2 = list(
   label = "In the past 24 hours: How hungry were you?",
   anchors = "usual5"
   ),
 vissex1 = list(
   label = "I find the thought of a very attractive body of the opposite sex
   very exciting",
   anchors = "agree7"
 ),
 vissex2 = list(
   label = "Seeing attractive people (of my preferred sex) in skimpy clothing 
   such as lingerie or tight briefs would be very sexually exciting to me",
   anchors = "agree7"
 ),
 vissex3 = list(
   label = "The thought of touching a very attractive body of the opposite sex
   gives me tingles",
   anchors = "agree7"
 ),
 vissex4 = list(
   label = "If I were to fantasize about having sex with someone right now, I 
   would try to picture very vividly in my mind what their body would look like",
   anchors = "agree7"
 ),
 attach1 = list(
   label = "I felt relaxed knowing that my partner is there for me",
   anchors = "pastday7"
 ),
 attach2 = list(
   label = "I felt needy for my partner's affection and attention",
   anchors = "pastday7"
 ),
 attach3 = list(
   label = "I've wanted some emotion distance from my partner",
   anchors = "pastday7"
 ),
 attach4 = list(
   label = "I felt my partner really cared about me",
   anchors = "pastday7"
 ),
 attach5 = list(
   label = "I really needed my partner's emotional support",
   anchors = "pastday7"
 ),
 attach6 = list(
   label = "I was uncomfortable having my partner close to me",
   anchors = "pastday7"
 ),
 energy1 = list(
   label = "I felt energetic",
   anchors = "pastday7"
 ),
 energy2 = list(
   label = "I felt sluggish",
   anchors = "pastday7"
 ),
 negaffect1 = list(
   label = "I felt upset",
   anchors = "pastday7"
 ),
 negaffect2 = list(
   label = "I felt sad",
   anchors = "pastday7"
),

#### COMPOSITE VARIABLES #####
  
 eat = list(
   label = "Hunger and eating: Mean of eat1 and eat2",
   anchors = "usual5"
 ),
 energy = list(
   label = "Energetic and sluggish (reversed): Mean of energy1 and reverse-coded energy2",
   anchors = "pastday7"
 ),
 neg_affect = list(
   label = "Upset and sad: Mean of negaffect1 and negaffect2",
   anchors = "pastday7"
 ),
 IPattract = list(
   label = "In pair attraction: mean of IP2 and IP3",
   anchors = "freq5"
 ),
 EPattract = list(
   label = "Extra pair attraction: mean of EP1 and EP2",
   anchors = "freq5"
 ),

 IPinterst = list(
   label = "In pair interest: mean of ZIPattract and Zsex2init"
 ),
 EPinterest = list(
   label = "Extra pair interest: mean of ZEPattract and Zsextoday" 
 ),
 IPvEPinterest = list(
   label = "In pair versus extra pair interest: IPinterest - EPinterest"
 ),
 ZIPvZEPinterest = list(
   label = "Z scored in pair versus extra pair interest: ZIPinterest - ZEPinterest"
 ),
 vissex = list(
   label = "Visual imagination of attractive men sexual excitation: mean of 
   vissex1, vissex2, vissex3, and vissex4",
   anchors = "agree7"
 ),
 
 Zp_sexattract_p1 = list(
    label = "Z-scored partner sexual attractiveness plus 1. Adding 1 to
the variable changes the 0 point to 1 SD below the mean"
 ),

 Zp_sexattract_m1 = list(
   label = "Z-scored partner sexual attractiveness minus 1. Subtracting 1 from
the variable changes the 0 point to 1 SD above the mean"
 )
 
 

 
) #end of variable info 


apply_variable_info <- function(df, variable_info, anchor_catalog) {
  for (v in names(variable_info)) {
    if (!v %in% names(df)) next
    
    info <- variable_info[[v]]
    lab  <- info$label
    
    # only apply anchors if the field exists and is not NULL
    if ("anchors" %in% names(info) && !is.null(info$anchors)) {
      anc <- anchor_catalog[[info$anchors]]
      df[[v]] <- haven::labelled(df[[v]], labels = anc, label = lab)
    } else {
      df[[v]] <- haven::labelled(df[[v]], label = lab)
    }
  }
  df
}

df_full <- apply_variable_info(df_full, variable_info, anchor_info)
df_filt <- apply_variable_info(df_filt, variable_info, anchor_info)

#### FULL DATAFRAME SAVE --------
# Save as CSV file
write.csv(df_full, file = "data/analysis_data/prolif_1_and_2_full.csv", row.names = FALSE)

# Save as SAV file (requires haven package)
write_sav(df_full, path = "data/analysis_data/prolif_1_and_2_full.sav")

# Save as RDS file
saveRDS(df_full, file = "data/analysis_data/prolif_1_and_2_full.rds")


#### FILTERED DATAFRAME SAVE -------
# Save as CSV file
write.csv(df_filt, file = "data/analysis_data/prolif_1_and_2_filtered.csv", row.names = FALSE)

# Save as SAV file (requires haven package)
write_sav(df_filt, path = "data/analysis_data/prolif_1_and_2_filtered.sav")

# Save as RDS file
saveRDS(df_filt, file = "data/analysis_data/prolif_1_and_2_filtered.rds")





