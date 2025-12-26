
library(tidyverse)
library(labelled) #for spss labels 
library(codechest) ##nice GitHub package with many utility functions 
library(haven)

#> Clear up environment 
rm(list = ls())

df <- read.csv("data/input_data/daily_initial_full_dataset_april22_2025.csv")


#> Creating an anonymized PROLIFIC_PID variable to allow for sharing of the data
#> and script files. 

# Because I am doing this anonymization after already having used "PROLIFIC_PID"
# in many areas, I am going to re-write the original variable name with the anonymous
#> variables so that I don't have to change every piece of code that references it. 
#> I will create a duplicate of it so I can be sure that the variables function
#> the same way. 

#save a duplicate of the original variable. 
df$PROLIFIC_PID1 <- df$PROLIFIC_PID

# create lookup table
pid_map <- data.frame(
  PROLIFIC_PID = unique(df$PROLIFIC_PID),
  anon_pid = paste0("p2.", 100 + seq_along(unique(df$PROLIFIC_PID)) - 1),
  stringsAsFactors = FALSE
)

# merge back into long data
df <- as_tibble(merge(df, pid_map, by = "PROLIFIC_PID", all.x = TRUE))
rm(pid_map)
#> Overwrite original PROLIFIC_PID with anonmous PID: 
df$PROLIFIC_PID <- df$anon_pid


#> This is the dataset which I cleaned in Python from the direct downloads
#> of each daily survey. 
#> The detailed cleaning log is available for the creation of that dataset. 
menses <- read.csv("data/input_data/menses_onset_days_MERGE_may13_2025.csv")
menses$PROLIFIC_PID1 <- menses$PROLIFIC_PID
menses$PROLIFIC_PID <- NULL
#> No duplicate rows identified. 
print(df[duplicated(df[, c("PROLIFIC_PID", "study_day")]) | 
           duplicated(df[, c("PROLIFIC_PID", "study_day")], fromLast = TRUE), 
         c("PROLIFIC_PID", "study_day", "Progress")], n = Inf)

#> Merge Menses Dates for Count Calculations -------


df <- df |> left_join(menses, by = join_by(PROLIFIC_PID1))

#> Renaming a few variables so they match Prolific 1 variables 
#> Since I copied the code from the Prolific1 data cleaning, its easier
#> for me to just match the variable names here to that. 


df <- df |> mutate(
  studyday = study_day,
  mensesfollowday = MENSESFOLLOWDAY
)

#Identify number of participants to be removed 
df |> group_by(PROLIFIC_PID) |> summarize(
  n = n()
) |> 
  filter(n < 10)


#> Updating some menses values from participant notes ---------
df |> filter(is.na(menses)) |> select(comments) 

temp <- with(df, case_when(
    #These three participants left a note on some day that they were on their 
    #period these days 
    PROLIFIC_PID == "p2.349" & study_day == 1  ~ 1,
    PROLIFIC_PID == "p2.104" & study_day == 5  ~ 1,
    PROLIFIC_PID == "p2.267" & study_day == 21 ~ 1,
    #These participants either noted they were not on their period or they missed
    #the question but it was clear that they were not on their period. 
    is.na(menses) & PROLIFIC_PID %in% c("p2.279", "p2.330", "p2.353", "p2.149", "p2.309", "p2.316", 
"p2.345", "p2.179", "p2.270", "p2.174", "p2.188", "p2.219", "p2.217", 
"p2.107", "p2.178", "p2.182", "p2.258", "p2.135", "p2.301", "p2.267", 
"p2.370") & !(PROLIFIC_PID == "p2.267" & study_day == 21) ~ 0,
    TRUE ~ menses
    ))

#> Find updated rows: 
which(df$menses != temp) #none of the original values were updated. Just missing
#values, which returns nothing here. 
#> The below code returns the line matching, which is basically what co-author had.
#> I identified 1 extra missing menses value  
which(is.na(df$menses) & !is.na(temp))

rm(temp)

#> update the values in place now 
df <- df |>
  mutate(menses = case_when(
    #These three participants left a note on some day that they were on their 
    #period these days 
    PROLIFIC_PID == "p2.349"  & study_day == 1  ~ 1,
    PROLIFIC_PID == "p2.104" & study_day == 5  ~ 1,
    PROLIFIC_PID == "p2.267"  & study_day == 21 ~ 1,
    #These participants either noted they were not on their period or they missed
    #the question but it was clear that they were not on their period. 
    is.na(menses) & PROLIFIC_PID %in% c("p2.279", "p2.330", "p2.353", "p2.149", "p2.309", "p2.316", 
                                        "p2.345", "p2.179", "p2.270", "p2.174", "p2.188", "p2.219", "p2.217", 
                                        "p2.107", "p2.178", "p2.182", "p2.258", "p2.135", "p2.301", "p2.267", 
                                        "p2.370") & !(PROLIFIC_PID == "p2.267" & study_day == 21) ~ 0,
    TRUE ~ menses
  ))

#> Syntax rel qual computes 4-29-25 ---------

# Love/attachment short scales
df$s_lovattachSHORT <- rowSums(df[, paste0("sbond", 1:7)], na.rm = TRUE) + df$srq3 + df$spsi9
df$p_lovattachSHORT <- rowSums(df[, paste0("pbond", 1:7)], na.rm = TRUE) + df$prq3 + df$ppsi9

# Passion short scales
df$s_passionSHORT <- df$spsi1 + df$spsi6 + df$spsi10 - df$spsi11 + df$srq1 + df$srq2
df$p_passionSHORT <- df$ppsi1 + df$ppsi6 + df$ppsi10 - df$ppsi11 + df$prq1 + df$prq2

# Honesty/trust short (note the negatives)
df$s_hontrustSHORT <- -(df$spsi3 + df$spsi5 + df$spsi7)
df$p_hontrustSHORT <- -(df$ppsi3 + df$ppsi5 + df$ppsi7)

# Social responsibility short
df$s_socrespSHORT <- df$spsi2 + df$spsi4 + df$spsi8 + df$spsi12 + df$sresp1 + df$sresp2
df$p_socrespSHORT <- df$ppsi2 + df$ppsi4 + df$ppsi8 + df$ppsi12 + df$presp1 + df$presp2

# Combined support vs confidence short
df$s_supportvconfSHORT <- df$s_hontrustSHORT + df$s_socrespSHORT
df$p_supportvconfSHORT <- df$p_hontrustSHORT + df$p_socrespSHORT


#> These scale variables are not in the dataset yet. 
# Relationship involvement scales (longer composites)
df$s_relinvolvSHORT <- df$s_lovattachSHORT + df$s_passionSHORT + df$s_socrespSHORT + df$s_hontrustSHORT  
df$p_relinvolvSHORT <- df$p_lovattachSHORT + df$p_passionSHORT + df$p_socrespSHORT + df$p_hontrustSHORT  

# Combined relationship involvement
df$tot_reinvolvSHORT  <- df$s_relinvolvSHORT + df$p_relinvolvSHORT
df$diff_reinvolvSHORT <- df$s_relinvolvSHORT - df$p_relinvolvSHORT


# Sum and difference scores
df$tot_lovattach <- df$s_lovattachSHORT + df$p_lovattachSHORT
df$diff_lovattach <- df$s_lovattachSHORT - df$p_lovattachSHORT

df$tot_passion <- df$s_passionSHORT + df$p_passionSHORT
df$diff_passion <- df$s_passionSHORT - df$p_passionSHORT

df$tot_supportvconf <- df$s_supportvconfSHORT + df$p_supportvconfSHORT
df$diff_supportvconf <- df$s_supportvconfSHORT - df$p_supportvconfSHORT

# Kid variables based on logical conditions
#> 1 = they have at least one child with their current partner, 0 means they don't
#> have a child with their current partner 
df$kid_partner <- ifelse(df$children == 1 & df$child_number_current_1 >= 1, 1, 0)
#> 1 = they have a child with a partner that's not their current partner, 0 means
#> they don't 
df$kid_nonpartner <- ifelse(df$children == 1 & df$child_number_current_1 == 0, 1, 0)

# Averages for attraction and love variables
df$s_sexattract <- rowMeans(df[, c("sattract1", "sattract2", "sattract3")], na.rm = TRUE)
df$p_sexattract <- rowMeans(df[, c("pattract1", "pattract2", "pattract3")], na.rm = TRUE)

df$s_plovecurr <- rowMeans(df[, c("splovecurr1", "splovecurr2", "splovecurr3", "splovecurr4")], na.rm = TRUE)
df$p_plovecurr <- rowMeans(df[, c("pplovecurr1", "pplovecurr2", "pplovecurr3", "pplovecurr4")], na.rm = TRUE)

df$s_plovepeak <- rowMeans(df[, c("splovepeak1", "splovepeak2", "splovepeak3", "splovepeak4")], na.rm = TRUE)
df$p_plovepeak <- rowMeans(df[, c("pplovepeak1", "pplovepeak2", "pplovepeak3", "pplovepeak4")], na.rm = TRUE)


#> Standardized all of the above variables:
btw_vars <- df |> select(matches("(^s_|^p_|^tot_|^diff_).*") & !matches("_2$")) |> colnames()

#> Because these are variables that are not repeated everyday I'm going to filter
#> down to 1 observation for participants to avoid bias from differing number
#> of days for participatns. 
tdf <- df[!duplicated(df$PROLIFIC_PID), c("PROLIFIC_PID", btw_vars)]

for (var in btw_vars) {
  new_name <- paste0("Z", var)
  tdf[[new_name]] <- scale(tdf[[var]])[, 1]
}

df <- df |> left_join(tdf[, c("PROLIFIC_PID", paste0("Z", btw_vars))], by = join_by(PROLIFIC_PID))


#> daily computes 4-29-25 ------ 

# Calculate means for grouped variables
df$vissex_pos   <- rowMeans(df[, c("vissex1", "vissex2", "vissex3", "vissex4")], na.rm = TRUE)
df$vissex_neg   <- rowMeans(df[, c("vissex5", "vissex6")], na.rm = TRUE)
df$vissex_pref  <- df$vissex_pos + df$vissex_neg

df$IP_attract   <- rowMeans(df[, c("IP1", "IP2")], na.rm = TRUE)
df$IP_repuls    <- rowMeans(df[, c("IP3", "IP4")], na.rm = TRUE)
df$EP_attract   <- rowMeans(df[, c("EP1", "EP2")], na.rm = TRUE)

df$mate_reten   <- rowMeans(df[, c("partbehav1", "partbehav2", "partbehav3")], na.rm = TRUE)

df$state_secure <- rowMeans(df[, c("attach1", "attach4")], na.rm = TRUE)
df$state_anx    <- rowMeans(df[, c("attach2", "attach5")], na.rm = TRUE)
df$state_avoid  <- rowMeans(df[, c("attach3", "attach6")], na.rm = TRUE)

df$neg_affect   <- rowMeans(df[, c("negaffect1", "negaffect2")], na.rm = TRUE)

# Energy score: energy1 and reverse-scored energy2
df$energy <- rowMeans(cbind(
  df$energy1,
  -(df$energy2 - 8)
), na.rm = TRUE)

df$eat <- rowMeans(df[, c("eat1", "eat2")], na.rm = TRUE)


#> 4/25/2025 -- Highlighting below out to make sure I go back and
#> confirm this is correct. 
# # Conditional replacements based on 'seepart'
df$sex1_ALL        <- ifelse(df$seepart == 0, 0, df$sex1)
df$sex2init_ALL    <- ifelse(df$seepart == 0, 0, df$sex2init)
df$sex3Pinit_ALL   <- ifelse(df$seepart == 0, 0, df$sex3Pinit)
df$sex4reject_ALL  <- ifelse(df$seepart == 0, 0, df$sex4reject)
df$sex5partreject_ALL <- ifelse(df$seepart == 0, 0, df$sex5partreject)

#> Base R selecting the most recent variables created by just determining the
#> range of variables. 
ww_vars <- names(df[, which(names(df) == "vissex_pos") : which(names(df) == "sex5partreject_ALL")])

#> Z score variables 
#> #> These variables are within-woman so I can just Z-score the full dataframe and
#> it should be fine  
for (var in ww_vars) {
new_name <- paste0("Z", var)
 df[[new_name]] <- scale(df[[var]])[, 1]
}
df |> select(all_of(ww_vars)) |> cor(use = "pairwise")

#> also need sextoday Z scored
df[paste0("Z", "sextoday")] <- scale(df[,"sextoday"])[, 1]

df$Zsextoday

df$IPinterest <- rowMeans(df[, c("ZIP_attract", "Zsex2init_ALL")], na.rm = TRUE)

df$EPinterest <- rowMeans(df[, c("ZEP_attract", "Zsextoday")], na.rm = TRUE)
  
int_vars <- colnames(df[, grepl(".*Pinterest$", colnames(df))])
int_vars
df$ZIPinterest <- scale(df[, "IPinterest"])[, 1]
cor(df$ZIPinterest, df$IPinterest)
df$ZEPinterest <- scale(df[, "EPinterest"])[, 1]
cor(df$ZIPinterest, df$EPinterest)

df$IPvEPinterest <- df$IPinterest - df$EPinterest

df$ZIPvZEPinterest <- df$ZIPinterest - df$ZEPinterest


#Ensure all of these exist and then pass into group_mean_center function
group_mean_center_vars <- c(
  "GSD",
  "sex1",
  "sex2init",
  "sex3Pinit",
  "sex4reject",
  "sex5partreject",
  "sextoday",
  "vissex_pos",
  "vissex_neg",
  "vissex_pref",
  "IP_attract",
  "IP_repuls",
  "EP_attract",
  "mate_reten",
  "state_secure",
  "state_anx",
  "state_avoid",
  "eat",
  "neg_affect",
  "energy",
  #"sex1ALL_ALL", 
  "sex2init_ALL",
  "sex3Pinit_ALL",
  "sex4reject_ALL",
  "sex5partreject_ALL",
  "IPinterest",
  "EPinterest",
  "IPvEPinterest"
)
#4/29/25 - highlighted out variables are not in my dataset. 
group_mean_center_vars %in% names(df)

# compute attach soi fam etc 4-29-25 -------

# Attachment style composites
df$avoid_att <- df$attstyl3 + df$attstyl7 + df$attstyl11 - df$attstyl1 - df$attstyl5 - df$attstyl9
df$anx_att   <- df$attstyl2 + df$attstyl4 + df$attstyl6 - df$attstyl8 + df$attstyl10 + df$attstyl12

# SOI composite with reverse-coded item
df$soi <- df$soi1 + df$soi2 + df$soi3 + df$soi5 + df$soi6 + (8 - df$soi4)

# SOI subscales
df$soi_beh <- df$soi1 + df$soi2
df$soi_att <- df$soi3 + (8 - df$soi4)
df$soi_des <- df$soi5 + df$soi6

# Family wealth average
df$famwealth <- rowMeans(df[, c("famwealth1", "famwealth2")], na.rm = TRUE)

# Childhood adversity composite
df$childadv <- rowMeans(df[, paste0("childunpred", 1:5)], na.rm = TRUE)



btw_vars2 <- colnames(df[, which(names(df) == "avoid_att") : which(names(df) == "childadv")])

#> Because these variables are not collected over time I'll shorten the dataframe
#> before standardizing too 
tdf <- df[!duplicated(df$PROLIFIC_PID), c("PROLIFIC_PID", btw_vars2)]

for (var in btw_vars2) {
  new_name <- paste0("Z", var)
  tdf[[new_name]] <- scale(tdf[[var]])[, 1]
}

#tdf[paste0("Z", btw_vars2)] <- scale(tdf[btw_vars2])

df <- df |> left_join(tdf[, c("PROLIFIC_PID", paste0("Z", btw_vars2))], by = join_by(PROLIFIC_PID))


# pms pubertal timing 4-29-25 --------

# PMS composite
df$pms <- rowMeans(df[, paste0("pms", 1:12)], na.rm = TRUE)

# Copy menarche age from original variable
df$menarche_age <- df$menarche_age_1

# Replace reported year with reasonable age
df$menarche_age[df$menarche_age_1 == 2012] <- 13

# Truncate extreme menarche age values
df$menarche_age_trunc <- df$menarche_age
df$menarche_age_trunc[df$menarche_age > 20] <- 20

# Variables to Z score. 
vars_to_zscore <- names(df[grepl("pubtim[1-3]$", names(df))])

for (var in vars_to_zscore) {
  
  new_var <- paste0("Z", var) #update variable name with Z 
  df[[new_var]] <- scale(df[[var]])[, 1] #Z score variable and add to dataset
  
}
df[, grepl("Zpubtim[1-3]", names(df))] #check variables 

# Standardize for pubertal timing composites
df$Zmenarche_age_trunc <- scale(df$menarche_age_trunc)[, 1]


df$menarche_tim <- rowMeans(df[, c("Zmenarche_age_trunc", "Zpubtim1")], na.rm = TRUE)


df$physdev_tim <- rowMeans(df[, c("Zpubtim2", "Zpubtim3")], na.rm = TRUE)

# No explicit SPSS code for these but it must have been done 
df$Zphysdev_tim <- scale(df$physdev_tim)[, 1]
df$Zmenarche_tim <- scale(df$menarche_tim)[ ,1]

df$pubertal_tim <- rowMeans(df[, c("Zmenarche_tim", "Zphysdev_tim")], na.rm = TRUE)

# Difference score for child health
df$childhealth <- df$childhealth2 - df$childhealth1


# rellength recode 4-29-25 -------

# Replace missing values (NA in R) with 0 for rellength_1 and cap values at 20
df$rellength_1_recode <- ifelse(is.na(df$rellength_1), 0, pmin(df$rellength_1, 20))

# Replace missing values with 0 for rellength_2,
# values 0–11 are kept, values 18–84 are set to 0
df$rellength_2_recode <- ifelse(
  is.na(df$rellength_2), 0,
  ifelse(df$rellength_2 >= 0 & df$rellength_2 <= 11, df$rellength_2,
         ifelse(df$rellength_2 >= 18 & df$rellength_2 <= 84, 0, df$rellength_2))
)

# Compute combined relationship length in years
df$rellength_recode <- df$rellength_1_recode + df$rellength_2_recode / 12

# Natural log of relationship length
df$ln_rellength_recode <- log(df$rellength_recode)




#> PROLIFIC_PID info ------

#> Use the 'PROLIFIC_PID' as the grouping variable in this dataset and not the
#>prolifID variable.
#> Quick check here looks like there are only two instances where the columns
#> differ though.
#> The prolifID column should have automatically generated the participants 
#> prolific ID but in the rare case that it failed to do that, participants could
#> enter there prolific ID themselves. Per the Python cleaning code, the few cases
#> that failed to enter properly were already fixed and entered into the PROLIFIC_PID
#> column, which is the meta-data column that participants couldn't edit and was
#> less likely to contain any error. 
setdiff(df$prolifID, df$PROLIFIC_PID1)
df |> filter(is.na(prolifID)) |> select(prolifID) #no missing values either


#> Syntax bc_day fc_day -----


#> Forward Count: 
#> As long as the study day is less than the menses two day. 
#> The mensesfollow day is never going to come up on a forward count because
#> they are all after the daily sessions. 
#> study_day - menses1 day IF study day is less than menses2day. 
#> IF study_day => menses2 day, THEN study_day minus menses2day. 

#> bctypical: 
#> menses1 = day 5, their cycle length is 25, you add those and you get 30. 
#> 

# Initialize columns in the dataframe
df$bc_day <- NA
df$fc_day <- NA
df$bctypical_day <- NA

# 1. If (mensesfollowday ge 20) bc_day = studyday - mensesfollowday.
df$bc_day <- ifelse(
  !is.na(df$mensesfollowday) & df$mensesfollowday >= 20,
  df$studyday - df$mensesfollowday,
  df$bc_day
)

# 2. If (studyday lt MENSES1DAY) bc_day = studyday - MENSES1DAY.
df$bc_day <- ifelse(
  !is.na(df$studyday) & !is.na(df$MENSES1DAY) & df$studyday < df$MENSES1DAY,
  df$studyday - df$MENSES1DAY,
  df$bc_day
)

# 3. If (studyday ge MENSES1DAY or studyday ge MENSES2DAY) bc_day = studyday - mensesfollowday.
df$bc_day <- ifelse(
  !is.na(df$studyday) & !is.na(df$mensesfollowday) &
    ( (!is.na(df$MENSES1DAY) & df$studyday >= df$MENSES1DAY) |
        (!is.na(df$MENSES2DAY) & df$studyday >= df$MENSES2DAY) ),
  df$studyday - df$mensesfollowday,
  df$bc_day
)  # need some very careful handling of NA values for the "or" statement to match
#the SPSS output. This statement doesn't change any values anyway. 

# 4. If (studyday ge MENSES1DAY and studyday lt MENSES2DAY) bc_day = studyday - MENSES2DAY.
df$bc_day <- ifelse(
  !is.na(df$studyday) & !is.na(df$MENSES1DAY) & !is.na(df$MENSES2DAY) &
    df$studyday >= df$MENSES1DAY & df$studyday < df$MENSES2DAY,
  df$studyday - df$MENSES2DAY,
  df$bc_day
)

#var_check(df, dat, bc_day, bc_day)
# 5. If (studyday ge MENSES1DAY) fc_day = studyday - MENSES1DAY.
df$fc_day <- ifelse(
  !is.na(df$studyday) & !is.na(df$MENSES1DAY) & df$studyday >= df$MENSES1DAY,
  df$studyday - df$MENSES1DAY,
  df$fc_day
)


# 6. If (studyday ge MENSES2DAY) fc_day = studyday - MENSES2DAY.
df$fc_day <- ifelse(
  !is.na(df$studyday) & !is.na(df$MENSES2DAY) & df$studyday >= df$MENSES2DAY,
  df$studyday - df$MENSES2DAY,
  df$fc_day
)


# 
# 7. If (studyday ge MENSES1DAY) bctypical_day = studyday - (MENSES1DAY + cycle_length).
df$bctypical_day <- ifelse(
  !is.na(df$studyday) & !is.na(df$MENSES1DAY) & !is.na(df$cycle_length) & df$studyday >= df$MENSES1DAY,
  df$studyday - (df$MENSES1DAY + df$cycle_length),
  df$bctypical_day
)

# 8. If (studyday ge MENSES2DAY) bctypical_day = studyday - (MENSES2DAY + cycle_length).
df$bctypical_day <- ifelse(
  !is.na(df$studyday) & !is.na(df$MENSES2DAY) & !is.na(df$cycle_length) & df$studyday >= df$MENSES2DAY,
  df$studyday - (df$MENSES2DAY + df$cycle_length),
  df$bctypical_day
)

# 9. If (bc_day le -1) fc_day = -1.
df$fc_day <- ifelse(
  !is.na(df$bc_day) & df$bc_day <= -1,
  -1,
  df$fc_day
)
# 
# 10. If (bc_day le -1) bctypical_day = 1.
df$bctypical_day <- ifelse(
  !is.na(df$bc_day) & df$bc_day <= -1,
  1,
  df$bctypical_day
)
# 
# 11. If (bc_day = 0) fc_day = 0.
df$fc_day <- ifelse(
  !is.na(df$bc_day) & df$bc_day == 0,
  0,
  df$fc_day
)

# 12. If (bc_day = 1) fc_day = 1.
df$fc_day <- ifelse(
  !is.na(df$bc_day) & df$bc_day == 1,
  1,
  df$fc_day
)

# 
# 13. If (bc_day = 0) bctypical_day = -cycle_length.
df$bctypical_day <- ifelse(
  !is.na(df$bc_day) & !is.na(df$cycle_length) & df$bc_day == 0,
  -df$cycle_length,
  df$bctypical_day
)

# 14. If (bc_day = 1) bctypical_day = 1 - cycle_length.
df$bctypical_day <- ifelse(
  !is.na(df$bc_day) & !is.na(df$cycle_length) & df$bc_day == 1,
  1 - df$cycle_length,
  df$bctypical_day
)


#> estr_b and prog_b Calculation (backward count) ------

# Assign estr_b and prog_b based on bc_day
df <- df |> 
  mutate(
    estr_b = case_when(
      bc_day == -40 ~ 4.70,
      bc_day == -39 ~ 4.54,
      bc_day == -38 ~ 4.38,
      bc_day == -37 ~ 4.22,
      bc_day == -36 ~ 4.04,
      bc_day == -35 ~ 3.87,
      bc_day == -34 ~ 3.70,
      bc_day == -33 ~ 3.53,
      bc_day == -32 ~ 3.39,
      bc_day == -31 ~ 3.32,
      bc_day == -30 ~ 3.31,
      bc_day == -29 ~ 3.35,
      bc_day == -28 ~ 3.43,
      bc_day == -27 ~ 3.51,
      bc_day == -26 ~ 3.56,
      bc_day == -25 ~ 3.60,
      bc_day == -24 ~ 3.61,
      bc_day == -23 ~ 3.64,
      bc_day == -22 ~ 3.75,
      bc_day == -21 ~ 3.94,
      bc_day == -20 ~ 4.18,
      bc_day == -19 ~ 4.47,
      bc_day == -18 ~ 4.74,
      bc_day == -17 ~ 4.94,
      bc_day == -16 ~ 5.01,
      bc_day == -15 ~ 4.97,
      bc_day == -14 ~ 4.86,
      bc_day == -13 ~ 4.72,
      bc_day == -12 ~ 4.61,
      bc_day == -11 ~ 4.57,
      bc_day == -10 ~ 4.61,
      bc_day == -9  ~ 4.73,
      bc_day == -8  ~ 4.87,
      bc_day == -7  ~ 4.97,
      bc_day == -6  ~ 4.99,
      bc_day == -5  ~ 4.93,
      bc_day == -4  ~ 4.73,
      bc_day == -3  ~ 4.45,
      bc_day == -2  ~ 4.12,
      bc_day == -1  ~ 3.77,
      TRUE           ~ NA_real_
    ),
    prog_b = case_when(
      bc_day == -40 ~ 7.41,
      bc_day == -39 ~ 7.27,
      bc_day == -38 ~ 7.12,
      bc_day == -37 ~ 6.96,
      bc_day == -36 ~ 6.81,
      bc_day == -35 ~ 6.64,
      bc_day == -34 ~ 6.53,
      bc_day == -33 ~ 6.39,
      bc_day == -32 ~ 6.26,
      bc_day == -31 ~ 6.19,
      bc_day == -30 ~ 6.19,
      bc_day == -29 ~ 6.22,
      bc_day == -28 ~ 6.23,
      bc_day == -27 ~ 6.22,
      bc_day == -26 ~ 6.20,
      bc_day == -25 ~ 6.12,
      bc_day == -24 ~ 6.04,
      bc_day == -23 ~ 5.97,
      bc_day == -22 ~ 5.93,
      bc_day == -21 ~ 5.90,
      bc_day == -20 ~ 5.90,
      bc_day == -19 ~ 5.91,
      bc_day == -18 ~ 5.97,
      bc_day == -17 ~ 6.11,
      bc_day == -16 ~ 6.34,
      bc_day == -15 ~ 6.67,
      bc_day == -14 ~ 7.06,
      bc_day == -13 ~ 7.50,
      bc_day == -12 ~ 7.93,
      bc_day == -11 ~ 8.26,
      bc_day == -10 ~ 8.60,
      bc_day == -9  ~ 8.81,
      bc_day == -8  ~ 8.99,
      bc_day == -7  ~ 9.13,
      bc_day == -6  ~ 9.20,
      bc_day == -5  ~ 9.12,
      bc_day == -4  ~ 8.93,
      bc_day == -3  ~ 8.49,
      bc_day == -2  ~ 7.94,
      bc_day == -1  ~ 7.35,
      TRUE           ~ NA_real_
    )
  )


#> estr_f and prog_f Calculation (forward count) -------
df <- df |> 
  mutate(
    estr_f = case_when(
      fc_day == 0  ~ 3.47,
      fc_day == 1  ~ 3.54,
      fc_day == 2  ~ 3.64,
      fc_day == 3  ~ 3.71,
      fc_day == 4  ~ 3.77,
      fc_day == 5  ~ 3.90,
      fc_day == 6  ~ 4.04,
      fc_day == 7  ~ 4.24,
      fc_day == 8  ~ 4.41,
      fc_day == 9  ~ 4.58,
      fc_day == 10 ~ 4.70,
      fc_day == 11 ~ 4.79,
      fc_day == 12 ~ 4.80,
      fc_day == 13 ~ 4.82,
      fc_day == 14 ~ 4.77,
      fc_day == 15 ~ 4.77,
      fc_day == 16 ~ 4.77,
      fc_day == 17 ~ 4.76,
      fc_day == 18 ~ 4.80,
      fc_day == 19 ~ 4.79,
      fc_day == 20 ~ 4.76,
      fc_day == 21 ~ 4.73,
      fc_day == 22 ~ 4.66,
      fc_day == 23 ~ 4.60,
      fc_day == 24 ~ 4.54,
      fc_day == 25 ~ 4.46,
      fc_day == 26 ~ 4.37,
      fc_day == 27 ~ 4.27,
      fc_day == 28 ~ 4.16,
      fc_day == 29 ~ 4.03,
      fc_day == 30 ~ 3.89,
      fc_day == 31 ~ 3.78,
      fc_day == 32 ~ 3.63,
      fc_day == 33 ~ 3.51,
      fc_day == 34 ~ 3.38,
      fc_day == 35 ~ 3.23,
      fc_day == 36 ~ 3.12,
      fc_day == 37 ~ 2.99,
      fc_day == 38 ~ 2.87,
      fc_day == 39 ~ 2.74,
      TRUE          ~ NA_real_
    ),
    prog_f = case_when(
      fc_day == 0  ~ 6.49,
      fc_day == 1  ~ 6.30,
      fc_day == 2  ~ 6.14,
      fc_day == 3  ~ 5.96,
      fc_day == 4  ~ 5.86,
      fc_day == 5  ~ 5.80,
      fc_day == 6  ~ 5.76,
      fc_day == 7  ~ 5.82,
      fc_day == 8  ~ 5.89,
      fc_day == 9  ~ 6.01,
      fc_day == 10 ~ 6.16,
      fc_day == 11 ~ 6.36,
      fc_day == 12 ~ 6.61,
      fc_day == 13 ~ 6.85,
      fc_day == 14 ~ 7.11,
      fc_day == 15 ~ 7.37,
      fc_day == 16 ~ 7.65,
      fc_day == 17 ~ 7.95,
      fc_day == 18 ~ 8.24,
      fc_day == 19 ~ 8.50,
      fc_day == 20 ~ 8.72,
      fc_day == 21 ~ 8.86,
      fc_day == 22 ~ 8.88,
      fc_day == 23 ~ 8.80,
      fc_day == 24 ~ 8.67,
      fc_day == 25 ~ 8.51,
      fc_day == 26 ~ 8.39,
      fc_day == 27 ~ 8.30,
      fc_day == 28 ~ 8.17,
      fc_day == 29 ~ 8.08,
      fc_day == 30 ~ 7.94,
      fc_day == 31 ~ 7.77,
      fc_day == 32 ~ 7.60,
      fc_day == 33 ~ 7.42,
      fc_day == 34 ~ 7.26,
      fc_day == 35 ~ 7.04,
      fc_day == 36 ~ 6.88,
      fc_day == 37 ~ 6.71,
      fc_day == 38 ~ 6.52,
      fc_day == 39 ~ 6.38,
      TRUE          ~ NA_real_
    )
  )


#> extr_bt and prog_bt Calculation (backward typical) ------
# Assign estr_bt and prog_bt based on bctypical_day
df <- df |> 
  mutate(
    estr_bt = case_when(
      bctypical_day == -40 ~ 4.70,
      bctypical_day == -39 ~ 4.54,
      bctypical_day == -38 ~ 4.38,
      bctypical_day == -37 ~ 4.22,
      bctypical_day == -36 ~ 4.04,
      bctypical_day == -35 ~ 3.87,
      bctypical_day == -34 ~ 3.70,
      bctypical_day == -33 ~ 3.53,
      bctypical_day == -32 ~ 3.39,
      bctypical_day == -31 ~ 3.32,
      bctypical_day == -30 ~ 3.31,
      bctypical_day == -29 ~ 3.35,
      bctypical_day == -28 ~ 3.43,
      bctypical_day == -27 ~ 3.51,
      bctypical_day == -26 ~ 3.56,
      bctypical_day == -25 ~ 3.60,
      bctypical_day == -24 ~ 3.61,
      bctypical_day == -23 ~ 3.64,
      bctypical_day == -22 ~ 3.75,
      bctypical_day == -21 ~ 3.94,
      bctypical_day == -20 ~ 4.18,
      bctypical_day == -19 ~ 4.47,
      bctypical_day == -18 ~ 4.74,
      bctypical_day == -17 ~ 4.94,
      bctypical_day == -16 ~ 5.01,
      bctypical_day == -15 ~ 4.97,
      bctypical_day == -14 ~ 4.86,
      bctypical_day == -13 ~ 4.72,
      bctypical_day == -12 ~ 4.61,
      bctypical_day == -11 ~ 4.57,
      bctypical_day == -10 ~ 4.61,
      bctypical_day == -9  ~ 4.73,
      bctypical_day == -8  ~ 4.87,
      bctypical_day == -7  ~ 4.97,
      bctypical_day == -6  ~ 4.99,
      bctypical_day == -5  ~ 4.93,
      bctypical_day == -4  ~ 4.73,
      bctypical_day == -3  ~ 4.45,
      bctypical_day == -2  ~ 4.12,
      bctypical_day == -1  ~ 3.77,
      TRUE                 ~ NA_real_
    ),
    prog_bt = case_when(
      bctypical_day == -40 ~ 7.41,
      bctypical_day == -39 ~ 7.27,
      bctypical_day == -38 ~ 7.12,
      bctypical_day == -37 ~ 6.96,
      bctypical_day == -36 ~ 6.81,
      bctypical_day == -35 ~ 6.64,
      bctypical_day == -34 ~ 6.53,
      bctypical_day == -33 ~ 6.39,
      bctypical_day == -32 ~ 6.26,
      bctypical_day == -31 ~ 6.19,
      bctypical_day == -30 ~ 6.19,
      bctypical_day == -29 ~ 6.22,
      bctypical_day == -28 ~ 6.23,
      bctypical_day == -27 ~ 6.22,
      bctypical_day == -26 ~ 6.20,
      bctypical_day == -25 ~ 6.12,
      bctypical_day == -24 ~ 6.04,
      bctypical_day == -23 ~ 5.97,
      bctypical_day == -22 ~ 5.93,
      bctypical_day == -21 ~ 5.90,
      bctypical_day == -20 ~ 5.90,
      bctypical_day == -19 ~ 5.91,
      bctypical_day == -18 ~ 5.97,
      bctypical_day == -17 ~ 6.11,
      bctypical_day == -16 ~ 6.34,
      bctypical_day == -15 ~ 6.67,
      bctypical_day == -14 ~ 7.06,
      bctypical_day == -13 ~ 7.50,
      bctypical_day == -12 ~ 7.93,
      bctypical_day == -11 ~ 8.26,
      bctypical_day == -10 ~ 8.60,
      bctypical_day == -9  ~ 8.81,
      bctypical_day == -8  ~ 8.99,
      bctypical_day == -7  ~ 9.13,
      bctypical_day == -6  ~ 9.20,
      bctypical_day == -5  ~ 9.12,
      bctypical_day == -4  ~ 8.93,
      bctypical_day == -3  ~ 8.49,
      bctypical_day == -2  ~ 7.94,
      bctypical_day == -1  ~ 7.35,
      TRUE                 ~ NA_real_
    )
  )


#> prc_stirn_fc Calculation ------

df <- df |> 
  mutate(prc_stirn_fc = case_when(
    fc_day == 0  ~ 0.01,
    fc_day == 1  ~ 0.01,
    fc_day == 2  ~ 0.02,
    fc_day == 3  ~ 0.03,
    fc_day == 4  ~ 0.05,
    fc_day == 5  ~ 0.09,
    fc_day == 6  ~ 0.16,
    fc_day == 7  ~ 0.27,
    fc_day == 8  ~ 0.38,
    fc_day == 9  ~ 0.48,
    fc_day == 10 ~ 0.56,
    fc_day == 11 ~ 0.58,
    fc_day == 12 ~ 0.55,
    fc_day == 13 ~ 0.48,
    fc_day == 14 ~ 0.38,
    fc_day == 15 ~ 0.28,
    fc_day == 16 ~ 0.20,
    fc_day == 17 ~ 0.14,
    fc_day == 18 ~ 0.10,
    fc_day == 19 ~ 0.07,
    fc_day == 20 ~ 0.06,
    fc_day == 21 ~ 0.04,
    fc_day == 22 ~ 0.03,
    fc_day == 23 ~ 0.02,
    fc_day == 24 ~ 0.01,
    fc_day %in% 25:40 ~ 0.01,  # Single condition for fc_day between 25 and 40
    TRUE ~ NA_real_  # Default value for unmatched rows 
  ))


#> prc_stirn_bc Calculation -------

df <- df |> 
  mutate(prc_stirn_bc = case_when(
    bc_day %in% -40:-27 ~ 0.01,
    bc_day == -26       ~ 0.02,
    bc_day == -25       ~ 0.03,
    bc_day == -24       ~ 0.05,
    bc_day == -23       ~ 0.09,
    bc_day == -22       ~ 0.16,
    bc_day == -21       ~ 0.27,
    bc_day == -20       ~ 0.38,
    bc_day == -19       ~ 0.48,
    bc_day == -18       ~ 0.56,
    bc_day == -17       ~ 0.58,
    bc_day == -16       ~ 0.55,
    bc_day == -15       ~ 0.48,
    bc_day == -14       ~ 0.38,
    bc_day == -13       ~ 0.28,
    bc_day == -12       ~ 0.20,
    bc_day == -11       ~ 0.14,
    bc_day == -10       ~ 0.10,
    bc_day == -9        ~ 0.07,
    bc_day == -8        ~ 0.06,
    bc_day == -7        ~ 0.04,
    bc_day == -6        ~ 0.03,
    bc_day == -5        ~ 0.02,
    bc_day %in% -4:-1   ~ 0.01,
    TRUE                ~ NA_real_  # Default for unmatched cases 
  ))



#> prc_stirn_bctypical Calculation --------
df <- df |> 
  mutate(prc_stirn_bctypical = case_when(
    bctypical_day %in% -40:-27 ~ 0.01,
    bctypical_day == -26      ~ 0.02,
    bctypical_day == -25      ~ 0.03,
    bctypical_day == -24      ~ 0.05,
    bctypical_day == -23      ~ 0.09,
    bctypical_day == -22      ~ 0.16,
    bctypical_day == -21      ~ 0.27,
    bctypical_day == -20      ~ 0.38,
    bctypical_day == -19      ~ 0.48,
    bctypical_day == -18      ~ 0.56,
    bctypical_day == -17      ~ 0.58,
    bctypical_day == -16      ~ 0.55,
    bctypical_day == -15      ~ 0.48,
    bctypical_day == -14      ~ 0.38,
    bctypical_day == -13      ~ 0.28,
    bctypical_day == -12      ~ 0.20,
    bctypical_day == -11      ~ 0.14,
    bctypical_day == -10      ~ 0.10,
    bctypical_day == -9       ~ 0.07,
    bctypical_day == -8       ~ 0.06,
    bctypical_day == -7       ~ 0.04,
    bctypical_day == -6       ~ 0.03,
    bctypical_day == -5       ~ 0.02,
    bctypical_day %in% -4:-1  ~ 0.01,
    TRUE                     ~ NA_real_  # Default for unmatched cases 
  ))


#> estr, prog, and prc_stirn Calculation --------


df <- df |> mutate(
  estr = rowMeans(across(c("estr_f", "estr_bt")), na.rm = TRUE),
  prog = rowMeans(across(c("prog_f", "prog_bt")), na.rm = TRUE),
  prc_stirn = rowMeans(across(c(prc_stirn_fc, prc_stirn_bctypical)), na.rm = TRUE)
)

df <- df |> 
  mutate(
    estr = ifelse(!is.na(bc_day) & bc_day <= -1, estr_b, estr),
    prog = ifelse(!is.na(bc_day) & bc_day <= -1, prog_b, prog),
    prc_stirn = ifelse(!is.na(bc_day) & bc_day <= -1, prc_stirn_bc, prc_stirn)
  )


#> Echg_f and Pchg_f Calculation -------
df <- df |> 
  mutate(
    Echg_f = case_when(
      fc_day == 0  ~ -11.2,
      fc_day == 1  ~ 2.3,
      fc_day == 2  ~ 3.6,
      fc_day == 3  ~ 2.8,
      fc_day == 4  ~ 2.5,
      fc_day == 5  ~ 6.0,
      fc_day == 6  ~ 7.4,
      fc_day == 7  ~ 12.6,
      fc_day == 8  ~ 12.9,
      fc_day == 9  ~ 15.2,
      fc_day == 10 ~ 12.4,
      fc_day == 11 ~ 10.3,
      fc_day == 12 ~ 1.2,
      fc_day == 13 ~ 2.5,
      fc_day == 14 ~ -6.0,
      fc_day == 15 ~ 0.0,
      fc_day == 16 ~ 0.0,
      fc_day == 17 ~ -1.2,
      fc_day == 18 ~ 4.8,
      fc_day == 19 ~ -1.2,
      fc_day == 20 ~ -3.6,
      fc_day == 21 ~ -3.4,
      fc_day == 22 ~ -7.7,
      fc_day == 23 ~ -6.1,
      fc_day == 24 ~ -5.8,
      fc_day == 25 ~ -7.2,
      fc_day == 26 ~ -7.4,
      fc_day == 27 ~ -7.5,
      fc_day == 28 ~ -7.4,
      fc_day == 29 ~ -7.8,
      fc_day == 30 ~ -7.3,
      fc_day == 31 ~ -5.1,
      fc_day == 32 ~ -6.1,
      fc_day == 33 ~ -4.3,
      fc_day == 34 ~ -4.1,
      fc_day == 35 ~ -4.1,
      fc_day == 36 ~ -2.6,
      fc_day == 37 ~ -2.8,
      fc_day == 38 ~ -2.2,
      fc_day == 39 ~ -2.1,
      TRUE         ~ NA_real_
    ),
    Pchg_f = case_when(
      fc_day == 0  ~ -897,
      fc_day == 1  ~ -114,
      fc_day == 2  ~ -80,
      fc_day == 3  ~ -76,
      fc_day == 4  ~ -37,
      fc_day == 5  ~ -20,
      fc_day == 6  ~ -13,
      fc_day == 7  ~ 20,
      fc_day == 8  ~ 24,
      fc_day == 9  ~ 46,
      fc_day == 10 ~ 66,
      fc_day == 11 ~ 105,
      fc_day == 12 ~ 164,
      fc_day == 13 ~ 201,
      fc_day == 14 ~ 280,
      fc_day == 15 ~ 363,
      fc_day == 16 ~ 513,
      fc_day == 17 ~ 734,
      fc_day == 18 ~ 953,
      fc_day == 19 ~ 1124,
      fc_day == 20 ~ 1208,
      fc_day == 21 ~ 919,
      fc_day == 22 ~ 142,
      fc_day == 23 ~ -552,
      fc_day == 24 ~ -808,
      fc_day == 25 ~ -860,
      fc_day == 26 ~ -561,
      fc_day == 27 ~ -379,
      fc_day == 28 ~ -490,
      fc_day == 29 ~ -304,
      fc_day == 30 ~ -421,
      fc_day == 31 ~ -438,
      fc_day == 32 ~ -370,
      fc_day == 33 ~ -329,
      fc_day == 34 ~ -247,
      fc_day == 35 ~ -281,
      fc_day == 36 ~ -169,
      fc_day == 37 ~ -152,
      fc_day == 38 ~ -142,
      fc_day == 39 ~ -89,
      TRUE         ~ NA_real_
    )
  )


#> Echg_b and Pchg_b Calculation --------
df <- df |> 
  mutate(
    Echg_b = case_when(
      bc_day == -39 ~ -16.2,
      bc_day == -38 ~ -13.8,
      bc_day == -37 ~ -11.8,
      bc_day == -36 ~ -11.2,
      bc_day == -35 ~ -8.9,
      bc_day == -34 ~ -7.5,
      bc_day == -33 ~ -6.3,
      bc_day == -32 ~ -4.5,
      bc_day == -31 ~ -2.0,
      bc_day == -30 ~ -0.3,
      bc_day == -29 ~ 1.1,
      bc_day == -28 ~ 2.4,
      bc_day == -27 ~ 2.6,
      bc_day == -26 ~ 1.7,
      bc_day == -25 ~ 1.4,
      bc_day == -24 ~ 0.4,
      bc_day == -23 ~ 1.1,
      bc_day == -22 ~ 4.4,
      bc_day == -21 ~ 8.9,
      bc_day == -20 ~ 13.9,
      bc_day == -19 ~ 22.0,
      bc_day == -18 ~ 27.1,
      bc_day == -17 ~ 25.3,
      bc_day == -16 ~ 10.1,
      bc_day == -15 ~ -5.9,
      bc_day == -14 ~ -15.0,
      bc_day == -13 ~ -16.8,
      bc_day == -12 ~ -11.7,
      bc_day == -11 ~ -3.9,
      bc_day == -10 ~ 3.9,
      bc_day == -9  ~ 12.8,
      bc_day == -8  ~ 17.0,
      bc_day == -7  ~ 13.7,
      bc_day == -6  ~ 2.9,
      bc_day == -5  ~ -8.6,
      bc_day == -4  ~ -25.1,
      bc_day == -3  ~ -27.7,
      bc_day == -2  ~ -24.1,
      bc_day == -1  ~ -18.2,
      TRUE          ~ NA_real_
    ),
    Pchg_b = case_when(
      bc_day == -39 ~ -216,
      bc_day == -38 ~ -200,
      bc_day == -37 ~ -183,
      bc_day == -36 ~ -147,
      bc_day == -35 ~ -142,
      bc_day == -34 ~ -80,
      bc_day == -33 ~ -89,
      bc_day == -32 ~ -73,
      bc_day == -31 ~ -35,
      bc_day == -30 ~ 0,
      bc_day == -29 ~ 15,
      bc_day == -28 ~ 5,
      bc_day == -27 ~ -5,
      bc_day == -26 ~ -10,
      bc_day == -25 ~ -38,
      bc_day == -24 ~ -35,
      bc_day == -23 ~ -28,
      bc_day == -22 ~ -15,
      bc_day == -21 ~ -11,
      bc_day == -20 ~ 0,
      bc_day == -19 ~ 4,
      bc_day == -18 ~ 23,
      bc_day == -17 ~ 59,
      bc_day == -16 ~ 116,
      bc_day == -15 ~ 221,
      bc_day == -14 ~ 376,
      bc_day == -13 ~ 643,
      bc_day == -12 ~ 971,
      bc_day == -11 ~ 1086,
      bc_day == -10 ~ 1564,
      bc_day == -9  ~ 1268,
      bc_day == -8  ~ 1320,
      bc_day == -7  ~ 1204,
      bc_day == -6  ~ 668,
      bc_day == -5  ~ -760,
      bc_day == -4  ~ -1579,
      bc_day == -3  ~ -2687,
      bc_day == -2  ~ -2057,
      bc_day == -1  ~ -1250,
      TRUE          ~ NA_real_
    )
  )


#> Echg_bt and Pchg_bt Calculation ------
df <- df |> 
  mutate(
    Echg_bt = case_when(
      bctypical_day == -39 ~ -16.2,
      bctypical_day == -38 ~ -13.8,
      bctypical_day == -37 ~ -11.8,
      bctypical_day == -36 ~ -11.2,
      bctypical_day == -35 ~ -8.9,
      bctypical_day == -34 ~ -7.5,
      bctypical_day == -33 ~ -6.3,
      bctypical_day == -32 ~ -4.5,
      bctypical_day == -31 ~ -2.0,
      bctypical_day == -30 ~ -0.3,
      bctypical_day == -29 ~ 1.1,
      bctypical_day == -28 ~ 2.4,
      bctypical_day == -27 ~ 2.6,
      bctypical_day == -26 ~ 1.7,
      bctypical_day == -25 ~ 1.4,
      bctypical_day == -24 ~ 0.4,
      bctypical_day == -23 ~ 1.1,
      bctypical_day == -22 ~ 4.4,
      bctypical_day == -21 ~ 8.9,
      bctypical_day == -20 ~ 13.9,
      bctypical_day == -19 ~ 22.0,
      bctypical_day == -18 ~ 27.1,
      bctypical_day == -17 ~ 25.3,
      bctypical_day == -16 ~ 10.1,
      bctypical_day == -15 ~ -5.9,
      bctypical_day == -14 ~ -15.0,
      bctypical_day == -13 ~ -16.8,
      bctypical_day == -12 ~ -11.7,
      bctypical_day == -11 ~ -3.9,
      bctypical_day == -10 ~ 3.9,
      bctypical_day == -9  ~ 12.8,
      bctypical_day == -8  ~ 17.0,
      bctypical_day == -7  ~ 13.7,
      bctypical_day == -6  ~ 2.9,
      bctypical_day == -5  ~ -8.6,
      bctypical_day == -4  ~ -25.1,
      bctypical_day == -3  ~ -27.7,
      bctypical_day == -2  ~ -24.1,
      bctypical_day == -1  ~ -18.2,
      TRUE                ~ NA_real_
    ),
    Pchg_bt = case_when(
      bctypical_day == -39 ~ -216,
      bctypical_day == -38 ~ -200,
      bctypical_day == -37 ~ -183,
      bctypical_day == -36 ~ -147,
      bctypical_day == -35 ~ -142,
      bctypical_day == -34 ~ -80,
      bctypical_day == -33 ~ -89,
      bctypical_day == -32 ~ -73,
      bctypical_day == -31 ~ -35,
      bctypical_day == -30 ~ 0,
      bctypical_day == -29 ~ 15,
      bctypical_day == -28 ~ 5,
      bctypical_day == -27 ~ -5,
      bctypical_day == -26 ~ -10,
      bctypical_day == -25 ~ -38,
      bctypical_day == -24 ~ -35,
      bctypical_day == -23 ~ -28,
      bctypical_day == -22 ~ -15,
      bctypical_day == -21 ~ -11,
      bctypical_day == -20 ~ 0,
      bctypical_day == -19 ~ 4,
      bctypical_day == -18 ~ 23,
      bctypical_day == -17 ~ 59,
      bctypical_day == -16 ~ 116,
      bctypical_day == -15 ~ 221,
      bctypical_day == -14 ~ 376,
      bctypical_day == -13 ~ 643,
      bctypical_day == -12 ~ 971,
      bctypical_day == -11 ~ 1086,
      bctypical_day == -10 ~ 1564,
      bctypical_day == -9  ~ 1268,
      bctypical_day == -8  ~ 1320,
      bctypical_day == -7  ~ 1204,
      bctypical_day == -6  ~ 668,
      bctypical_day == -5  ~ -760,
      bctypical_day == -4  ~ -1579,
      bctypical_day == -3  ~ -2687,
      bctypical_day == -2  ~ -2057,
      bctypical_day == -1  ~ -1250,
      TRUE                ~ NA_real_
    )
  )

#> Echg and Pchg Calculation -------

df <- df |> 
  mutate(
    Echg = ifelse(!is.na(bc_day) & bc_day <= -1, Echg_b, rowMeans(across(c(Echg_f, Echg_bt)), na.rm = TRUE)),
    Pchg = ifelse(!is.na(bc_day) & bc_day <= -1, Pchg_b, rowMeans(across(c(Pchg_f, Pchg_bt)), na.rm = TRUE))
  )

#> Raw hormone value Calculations ------
# Compute the raw exponential values for estr and prog
df <- df |> 
  mutate(
    estr_raw = exp(estr),
    prog_raw = exp(prog)
  )

# Compute the raw exponential values for estr_f and prog_f
df <- df |> 
  mutate(
    estr_raw_f = exp(estr_f),
    prog_raw_f = exp(prog_f)
  )

# Compute the raw exponential values for estr_b and prog_b
df <- df |> 
  mutate(
    estr_raw_b = exp(estr_b),
    prog_raw_b = exp(prog_b)
  )

#> Between- and Within-Woman Variable Calculations ------

mlm_variables <- c("prog", "estr", "prc_stirn", "Echg", "Pchg")

gc_vars <- c(group_mean_center_vars, mlm_variables)

df <- mlm_groupmean(df, gc_vars, "PROLIFIC_PID", within_affix = "_ww", 
                    between_affix = "_mean", affix_type = "suffix", include_z = TRUE)

#> Selection of Cases --------

#> Creating a variable to filter on number of valid backward count days 
#> pre-registration indicates a person must have at least 10 to be included
condition <- df |> group_by(PROLIFIC_PID) |> 
  filter(!is.na(bc_day)) |> 
  count(PROLIFIC_PID, name = "bc_days_n") 
condition
df <- df |> left_join(condition)

# Define IDs that should always be excluded

exclude_ids <- c("p2.178", "p2.109", "p2.326", "p2.359", "p2.375", "p2.308", 
                 "p2.325", "p2.132", "p2.224", "p2.185", "p2.115", "p2.243", "p2.150", 
                 "p2.364", "p2.350")


# Define special cases where exclusion depends on study_day
special_case_1 <- "p2.264" # Include if study_day <= 12
special_case_2 <- "p2.161"  # Include if study_day <= 14


# #> Fixing some variable names for SPSS save: 
names(df) <- gsub("\\.+", "_", names(df))
df$mensesfollowday <- NULL


filtered_df <- df |> 
  filter(
    bc_days_n >= 10,
    !PROLIFIC_PID %in% exclude_ids, 
    (PROLIFIC_PID != special_case_1 | study_day <= 12),
    (PROLIFIC_PID != special_case_2 | study_day <= 14)
  )


#> 233 participants 
filtered_df[!duplicated(filtered_df$PROLIFIC_PID), "PROLIFIC_PID"]


#> SAVING DATAFRAMES --------


#### FULL DATAFRAME 
# Save as CSV file
write.csv(df, file = "data/analysis_data/prolific2_full.csv", row.names = FALSE)

# Save as SAV file (requires haven package)
write_sav(df, path = "data/analysis_data/prolific2_full.sav")

# Save as RDS file
saveRDS(df, file = "data/analysis_data/prolific2_full.rds")


#### FILTERED DATAFRAME 
# Save as CSV file
write.csv(filtered_df, file = "data/analysis_data/prolific2_filtered.csv", row.names = FALSE)

# Save as SAV file (requires haven package)
write_sav(filtered_df, path = "data/analysis_data/prolific2_filtered.sav")

# Save as RDS file
saveRDS(filtered_df, file = "data/analysis_data/prolific2_filtered.rds")








