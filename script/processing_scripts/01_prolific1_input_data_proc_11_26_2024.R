

library(tidyverse)
library(haven) #for reading spss file
library(labelled) #for spss labels 
library(psych)
library(codechest) #nice GitHub package with many utility functions 

#> Just rerun as updates are made: 

#> Clear up environment 
rm(list = ls())

df1 <- haven::read_sav("data/input_data/prolific_study1_data.sav")


#> Creating an anonymized PROLIFIC_PID variable to allow for sharing of the data
#> and script files. 

# Because I am doing this anonymization after already having used "PROLIFIC_PID"
# in many areas, I am going to re-write the original variable name with the anonymous
#> variables so that I don't have to change every piece of code that references it. 
#> I will create a duplicate of it so I can be sure that the variables function
#> the same way. 

#save a duplicate of the original variable. 
df1$PROLIFIC_PID1 <- df1$PROLIFIC_PID

# create lookup table
pid_map <- data.frame(
  PROLIFIC_PID = unique(df1$PROLIFIC_PID),
  anon_pid = paste0("p1.", 100 + seq_along(unique(df1$PROLIFIC_PID)) - 1),
  stringsAsFactors = FALSE
)

# merge back into long data
df1 <- as_tibble(merge(df1, pid_map, by = "PROLIFIC_PID", all.x = TRUE))
rm(pid_map)
#> Overwrite original PROLIFIC_PID with anonmous PID: 
df1$PROLIFIC_PID <- df1$anon_pid


#> The input data file that I have loaded here is identical to the following file:
#df <- read_sav("data/input_data/daily surveys COMPLETE 10_31_24 NO LONG CYCLES 7.sav")
#> I've just renamed that file for ease of loading the data frame in. But know that
#> it is the exact same as the file I loaded above. 
dat <- df1 #making a copy of the df for ease 
#> Saving a copy of the df with only original variables (i.e., no variables created in df sent to me)
df <- df1[, c(names(df1[, 1:237]), "anon_pid", "PROLIFIC_PID1")] #per co-author, "pms12" was the last variable in original df 
#> Technically the data frame I've loaded in isn't untouched because he has 
#> already gone in and made a lot of variables and done some different edits to things.
#> The ID variables were also edited to further protect participants identities.  
#> All of that work was done in SPSS, though. 
#> I'll be working from raw variables to re-create everything here for clarity. 


var_check <- function(df, dat, var1, var2) {
  # Convert input variables to strings (column names)
  var1 <- deparse(substitute(var1))
  var2 <- deparse(substitute(var2))
  
  # Check if columns exist in the data frames
  if (!(var1 %in% names(df))) stop(paste("Column", var1, "not found in df"))
  if (!(var2 %in% names(dat))) stop(paste("Column", var2, "not found in dat"))
  
  # Ensure columns are numeric
  if (!is.numeric(df[[var1]]) || !is.numeric(dat[[var2]])) {
    stop("Both variables must be numeric")
  }
  
  # Calculate correlation
  cor1 <- cor(df[[var1]], dat[[var2]], use = "complete.obs")
  
  # Check if variables are identical after rounding
  idcl <- identical(as.double(round(df[[var1]], 7)), as.double(round(dat[[var2]], 7)))
  
  # Compute summaries
  sum1 <- summary(df[[var1]])
  sum2 <- summary(dat[[var2]])
  
  # Return results as a list
  ls <- list(
    correlation = cor1,
    identical = idcl,
    df_summary = sum1,
    dat_summary = sum2
  )
  
  return(ls)
}


#> Checking for duplicate lines (11/8 update) --------

print(df[duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")]) | 
           duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")], fromLast = TRUE), 
         c("PROLIFIC_PID", "STUDY_ID", "Progress")], n = Inf)
#> Based on this check, there are 14 instances where there are two reports on the
#> same day from the same person. 
#> Unsure how some of these happened, but there are several with little progress,
#> its possible that something glitched and they got signed out and then when they
#> went back in it generated a new survey. 
#> Going to remove the instances where one report has less than 20% progress. 
df[(duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")]) | 
   duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")], fromLast = TRUE)) & 
   df$Progress < 20, 
            c("PROLIFIC_PID", "STUDY_ID", "Progress")]

df[(duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")]) | 
      duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")], fromLast = TRUE)) & 
     df$Progress < 20, 
   c("PROLIFIC_PID", "STUDY_ID", "Progress")]

bad_rows <- (duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")]) | 
      duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")], fromLast = TRUE)) & 
     df$Progress < 20

df <- df[!bad_rows, ]

#> Now there are only 8 cases where someone had two responses in a single day
#> (16 total lines) but they completed the full survey (or 98%) both times. 
print(df[duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")]) | 
           duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")], fromLast = TRUE), 
         c("PROLIFIC_PID", "STUDY_ID", "Progress")], n = Inf)


#> Again, its uncertain what happened here, but taking a sample of some of the 
#> daily variables shows that for most variables, they had nearly identical 
#> response. Most instances are identical while some are +/- 1. 
print(df[duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")]) | 
           duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")], fromLast = TRUE), 
         grepl("StartDate|eat|IP|EP|vissex", colnames(df))], n = Inf)
#> Don't have preregistration on how to handle this, but the most reasonable way
#> seems to be to just take the first response of the day. 
#str(df$StartDate)
dup_rows <- duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")]) | 
  duplicated(df[, c("PROLIFIC_PID", "STUDY_ID")], fromLast = TRUE)

df[dup_rows, "StartDate"]
df[(dup_rows & ave(as.numeric(df$StartDate),
                   df$PROLIFIC_PID, df$STUDY_ID,
                   FUN = function(x) x == max(x))), c("PROLIFIC_PID", "STUDY_ID", "StartDate")]

df <- df[!(dup_rows & ave(as.numeric(df$StartDate),
                   df$PROLIFIC_PID, df$STUDY_ID,
                   FUN = function(x) x == max(x))), ]

#copying the filtered df to dat for checking things 
dat <- dat[!bad_rows, ]
dat <- dat[!(dup_rows & ave(as.numeric(dat$StartDate),
                           dat$PROLIFIC_PID, dat$STUDY_ID,
                           FUN = function(x) x == max(x))), ]

#> Relationship Quality Variables ------

#> Selecting sbond items. This works well if using the corret df object. 
grep("^sbond[1-7]$", names(df)) #selects items based on name/column location


#Compute s_lovattach_scale and p_lovattach_scale
df$s_lovattach_scale <- rowSums(df[, grep("^sbond[1-7]$", names(df))], na.rm = TRUE) +
  df$srq4 + df$spsi1 + df$spsi22 - df$spsi27
#var_check(df, dat, s_lovattach_scale, s_lovattach_scale) #identical


df$p_lovattach_scale <- rowSums(df[, grep("^pbond[1-7]$", names(df))], na.rm = TRUE) +
  df$prq4 + df$ppsi1 + df$ppsi22 - df$ppsi27
#var_check(df, dat, p_lovattach_scale, p_lovattach_scale) # identical


# Compute s_passion_scale and p_passion_scale
df$s_passion_scale <- df$spsi3 + df$spsi13 + df$spsi16 - df$spsi19 + df$spsi23 - df$spsi24 +
  df$srq1 + df$srq2 + df$srq3
#var_check(df, dat, s_passion_scale, s_passion_scale) #identical

df$p_passion_scale <- df$ppsi3 + df$ppsi13 + df$ppsi16 - df$ppsi19 + df$ppsi23 - df$ppsi24 +
  df$prq1 + df$prq2 + df$prq3
#var_check(df, dat, p_passion_scale, p_passion_scale) #identical


# Self-report passion items
s_passion_scale_items <- c("spsi3", "spsi13", "spsi16", "spsi19", "spsi23", "spsi24", 
                           "srq1", "srq2", "srq3")

# Partner-report passion items (replace s → p)
p_passion_scale_items <- gsub("^s", "p", s_passion_scale_items)


# Compute s_antagtrust_scale and p_antagtrust_scale
df$s_antagtrust_scale <- -df$spsi2 - df$spsi5 + df$spsi6 + df$spsi11 - df$spsi17 +
  df$spsi20 - df$spsi21 - df$spsi25 + df$spsi26 - df$spsi28 + df$sresp1 + df$sresp2
#var_check(df, dat, s_antagtrust_scale, s_antagtrust_scale) #identical
df$p_antagtrust_scale <- -df$ppsi2 - df$ppsi5 + df$ppsi6 + df$ppsi11 - df$ppsi17 +
  df$ppsi20 - df$ppsi21 - df$ppsi25 + df$ppsi26 - df$ppsi28 + df$presp1 + df$presp2
#var_check(df, dat, p_antagtrust_scale, p_antagtrust_scale) #identical


# Compute s_sexexcl_scale and p_sexexcl_scale
df$s_sexexcl_scale <- -df$spsi4 - df$spsi7 - df$spsi10 - df$spsi12 - df$spsi14 - df$spsi15 - df$spsi18
#var_check(df, dat, s_sexexcl_scale, s_sexexcl_scale) #identical
df$p_sexexcl_scale <- -df$ppsi4 - df$ppsi7 - df$ppsi10 - df$ppsi12 - df$ppsi14 - df$ppsi15 - df$ppsi18
#var_check(df, dat, p_sexexcl_scale, p_sexexcl_scale) #identical

# Compute s_relinvolv_scale and p_relinvolv_scale
df$s_relinvolv_scale <- df$s_lovattach_scale + df$s_passion_scale + df$s_antagtrust_scale +
  df$s_sexexcl_scale + df$spsi8 + df$spsi9
# var_check(df, dat, s_relinvolv_scale, s_relinvolve)
#"s_relinvolv_scale does not exist in loaded-in dataset. 
#"s_relinvolve" does exist. They are correlated at r = .98. 
#There is some extra missing data from the s_relinvolv_scale variable created here.
#> Some edits must have been made down the road. Will leave the original variable in now. 

df$p_relinvolv_scale <- df$p_lovattach_scale + df$p_passion_scale + df$p_antagtrust_scale +
  df$p_sexexcl_scale + df$ppsi8 + df$ppsi9
# var_check(df, dat, p_relinvolv_scale, p_relinvolve)
#Same exact case here as the above variable. 

# Compute tot_reinvolv and diff_reinvolv
df$tot_reinvolv <- df$s_relinvolv_scale + df$p_relinvolv_scale
df$diff_reinvolv <- df$s_relinvolv_scale - df$p_relinvolv_scale
# var_check(df, dat, tot_reinvolv, tot_relinvolve)
# var_check(df, dat, diff_reinvolv, diff_relinvolve)
#Same thing going on here as above. diff variable notably lower correlation, r = .91

# Totals and differences for lovattach scale
df$tot_lovattach <- df$s_lovattach_scale + df$p_lovattach_scale
df$diff_lovattach <- df$s_lovattach_scale - df$p_lovattach_scale
#var_check(df, dat, tot_lovattach, tot_lovattach) #identical 
#var_check(df, dat, diff_lovattach, diff_lovattach) #identical 


# Totals and differences for passion scale
df$tot_passion <- df$s_passion_scale + df$p_passion_scale
df$diff_passion <- df$s_passion_scale - df$p_passion_scale
#var_check(df, dat, tot_passion, tot_passion) #identical 
#var_check(df, dat, diff_passion, diff_passion) #identical 


# Totals and differences for antagtrust scale
df$tot_antagtrust <- df$s_antagtrust_scale + df$p_antagtrust_scale
df$diff_antagtrust <- df$s_antagtrust_scale - df$p_antagtrust_scale
# Totals and differences for antagtrust scale
#var_check(df, dat, tot_antagtrust, tot_antagtrust) #identical
#var_check(df, dat, diff_antagtrust, diff_antagtrust) #identical


# Totals and differences for sexexcl scale
df$tot_sexexcl <- df$s_sexexcl_scale + df$p_sexexcl_scale
df$diff_sexexcl <- df$s_sexexcl_scale - df$p_sexexcl_scale
# Totals and differences for sexexcl scale
#var_check(df, dat, tot_sexexcl, tot_sexexcl) #identical
#var_check(df, dat, diff_sexexcl, diff_sexexcl) #identical

#> Self and Partner sexual attraction
df$s_sexattract <- rowMeans(df[, c("sattract1", "sattract2", "sattract3")], na.rm = TRUE)
df$p_sexattract <- rowMeans(df[, c("pattract1", "pattract2", "pattract3")], na.rm = TRUE)

#> Attachment and PMS Scales  -------
# Avoidant and Anxious Attachment
df$avoid_att <- df$attstyl3 + df$attstyl7 + df$attstyl11 - df$attstyl1 - df$attstyl5 - df$attstyl9
df$anx_att <- df$attstyl2 + df$attstyl4 + df$attstyl6 - df$attstyl8 + df$attstyl10 + df$attstyl12

# State Attachment Scales
df$state_secure <- df$attach1 + df$attach4 + df$attach7
df$state_anxious <- df$attach2 + df$attach5 + df$attach8
df$state_avoid <- df$attach3 + df$attach6 + df$attach9

# Social Exclusion and Group Social Scales
df$socexcl <- df$belong3 + df$belong5 + df$belong6
df$groupsoc <- df$belong1 + df$belong2 + df$belong4

# PMS Average (Assumes pms1 to pms12 are consecutive columns)
df$pms_2 <- rowMeans(df[, grep("^pms[1-9]$|^pms1[0-2]$", names(df))], na.rm = TRUE)


df <- df  |> 
  # Love and attachment (full)
  mutate(
    s_lovattachFAC = rowSums(across(sbond1:sbond7)) + srq4 + spsi22,
    p_lovattachFAC = rowSums(across(pbond1:pbond7)) + prq4 + ppsi22
  ) |>
  
  # Passion (full)
  mutate(
    s_passionFAC = spsi3 + spsi13 + spsi16 - spsi19 + spsi23 - spsi24 + srq1 + srq2 + srq3,
    p_passionFAC = ppsi3 + ppsi13 + ppsi16 - ppsi19 + ppsi23 - ppsi24 + prq1 + prq2 + prq3
  ) |>
  
  # Honesty and trust (full, all items reverse-coded)
  mutate(
    s_hontrustFAC = -(spsi2 + spsi4 + spsi7 + spsi5 + spsi10 + spsi12 + spsi15 + spsi17 + spsi18 + spsi25 + spsi28),
    p_hontrustFAC = -(ppsi2 + ppsi4 + ppsi7 + ppsi5 + ppsi10 + ppsi12 + ppsi15 + ppsi17 + ppsi18 + ppsi25 + ppsi28)
  ) |>
  
  # Social responsibility (full)
  mutate(
    s_socrespFAC = spsi6 + spsi11 + spsi20 - spsi21 + spsi26 + sresp1 + sresp2 + spsi8 + spsi9,
    p_socrespFAC = ppsi6 + ppsi11 + ppsi20 - ppsi21 + ppsi26 + presp1 + presp2 + ppsi8 + ppsi9
  ) |>
  
  # Support vs. conflict (full)
  mutate(
    s_supportvconflFAC = s_hontrustFAC + s_socrespFAC + spsi4 - spsi14 - spsi27,
    p_supportvconflFAC = p_hontrustFAC + p_socrespFAC + ppsi4 - ppsi14 - ppsi27
  ) |>
  
  # Love and attachment (short)
  mutate(
    s_lovattachSHORT = rowSums(across(sbond1:sbond7)) + srq4 + spsi22,
    p_lovattachSHORT = rowSums(across(pbond1:pbond7)) + prq4 + ppsi22
  ) |>
  
  # Passion (short)
  mutate(
    s_passionSHORT = spsi3 + spsi13 + spsi23 - spsi24 + srq1 + srq3,
    p_passionSHORT = ppsi3 + ppsi13 + ppsi23 - ppsi24 + prq1 + prq3
  ) |>
  
  # Honesty and trust (short, reverse-coded)
  mutate(
    s_hontrustSHORT = -(spsi10 + spsi12 + spsi15),
    p_hontrustSHORT = -(ppsi10 + ppsi12 + ppsi15)
  ) |>
  
  # Social responsibility (short)
  mutate(
    s_socrespSHORT = spsi6 + spsi11 + spsi20 + spsi26 + sresp1 + sresp2,
    p_socrespSHORT = ppsi6 + ppsi11 + ppsi20 + ppsi26 + presp1 + presp2
  ) |>
  
  # Support vs. conflict (short)
  mutate(
    s_supportvconfSHORT = s_hontrustSHORT + s_socrespSHORT,
    p_supportvconfSHORT = p_hontrustSHORT + p_socrespSHORT
  ) |>
  
  # Relationship involvement (short)
  mutate(
    s_relinvolvSHORT = s_lovattachSHORT + s_passionSHORT + s_supportvconfSHORT,
    p_relinvolvSHORT = p_lovattachSHORT + p_passionSHORT + p_supportvconfSHORT,
    tot_relinvolvSHORT = s_relinvolvSHORT + p_relinvolvSHORT,
    diff_relinvolvSHORT = s_relinvolvSHORT - p_relinvolvSHORT
  )

#> Study Day Grouping Variable -------

# Map STUDY_ID to studyday
df$studyday <- as.numeric(sub("DAY_", "", df$STUDY_ID))

#> Syntax daily computes ------

# Compute means for eat, IPattract, EPattract, and vissex
df$eat <- rowMeans(df[, c("eat1", "eat2")], na.rm = TRUE)
#var_check(df, dat, eat, eat) # r = 1.00 and same # of NA's (not identical for whatever reason)
df$IPattract <- rowMeans(df[, c("IP2", "IP3")], na.rm = TRUE)
#var_check(df, dat, IPattract, IPattract) # r = 1.00 and same # of NA's (not identical for whatever reason)
df$EPattract <- rowMeans(df[, c("EP1", "EP2")], na.rm = TRUE)
#var_check(df, dat, EPattract, EPattract) # r = 1.00 and same # of NA's (not identical for whatever reason)
df$vissex <- rowMeans(df[, grep("^vissex[1-4]$", names(df))], na.rm = TRUE)
#var_check(df, dat, vissex, vissex) # r = 1.00 and same # of NA's (not identical for whatever reason)

# Compute menses_day
df$menses_day <- df$menses * df$studyday_1
#> The "menses" variable is coded 0 = not menstruating and 1 = menstruating. 
#> The studyday_1 variable is simply day 1-30 of the study. So, multiplying the
#> menses variable by the study day variable simply inputs the specific study day
#> the participant was menstruating and then inputs 0 for every other day. 


zvars1 <- c("sextoday", "IPattract", "EPattract", "sex2init")

for (var in zvars1) {
  new_name <- paste0("Z", var)
  df[[new_name]] <- scale(df[[var]])[, 1]
}

# df |> select(matches("Z(IP|EP|sex)")) |> 
#                cor(use = "pairwise")

df$IPinterest <- rowMeans(df[, c("ZIPattract", "Zsex2init")], na.rm = TRUE)

df$EPinterest <- rowMeans(df[, c("ZEPattract", "Zsextoday")], na.rm = TRUE)

int_vars <- colnames(df[, grepl(".*Pinterest$", colnames(df))])
int_vars
df$ZIPinterest <- scale(df$IPinterest)[, 1]
df$ZEPinterest <- scale(df$EPinterest)[, 1]

# df |> select(int_vars, paste0("Z", int_vars)) |> 
#   cor(use = "pairwise")

df$IPvEPinterest <- df$IPinterest - df$EPinterest

df$ZIPvZEPinterest <- df$ZIPinterest - df$ZEPinterest


#> Variables to bring in from Dataframe -----

df <- df |> left_join(df1[, c("ResponseId", "mensesfollowday", "MENSES1DAY", "MENSES2DAY")], by = join_by("ResponseId"))

#> Syntax bc_day fc_day -----

#> SPSS syntax originally used was re-created in R here. 

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

var_check(df, dat, bc_day, bc_day)

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
    TRUE ~ NA_real_  # Default value for unmatched rows (optional)
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
    TRUE                ~ NA_real_  # Default for unmatched cases (optional)
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
    TRUE                     ~ NA_real_  # Default for unmatched cases (optional)
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

#identical 
var_check(df, dat, Echg_bt, Echg_bt)

#identical 
var_check(df, dat, Pchg_bt, Pchg_bt)

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

#> None of these are in the original dat dataframe sent to me. 


#> # rellength recode -------

#> Appears that the variables themselves are ready to combine. No unusual values. 
describe(df[c("rellength_1", "rellength_2")])
summary(df[c("rellength_1", "rellength_2")])

# Compute combined relationship length in years
df$rellength <- df$rellength_1 + (df$rellength_2 / 12)

describe(df$rellength) # looks good 

# Natural log of relationship length
df$ln_rellength <- log(df$rellength)




#> Between- and Within-Woman Variable Calculations ------

mlm_variables <- c("prog", "estr", "prc_stirn", "Echg", "Pchg")

df <- mlm_groupmean(df, mlm_variables, group = "PROLIFIC_PID", within_affix = "_ww",
                    between_affix = "_mean", affix_type = "suffix", include_z = TRUE)


# var_check(df, dat, BG_prog, prog_mean)
# var_check(df, dat, BG_estr, estr_mean)
# var_check(df, dat, BG_prc_stirn, prc_stirn_mean)
# var_check(df, dat, BG_Echg, Echg_mean)
# var_check(df, dat, BG_Pchg, Pchg_mean)
# 
# var_check(df, dat, WG_prog, prog_ww)
# var_check(df, dat, WG_estr, estr_ww)
# var_check(df, dat, WG_prc_stirn, prc_stirn_ww)
# var_check(df, dat, WG_Echg, Echg_ww)
# var_check(df, dat, WG_Pchg, Pchg_ww)



#> Syntax selection of cases  ------
# Define the list of PROLIFIC_PID values to exclude


excluded_ids <- c("p1.104", "p1.185", "p1.247", "p1.298", "p1.328", "p1.381",
  "p1.278", "p1.186", "p1.204", "p1.221", "p1.302", "p1.321", "p1.132",
  "p1.324", "p1.253", "p1.370", "p1.243", "p1.155", "p1.197", "p1.389",
  "p1.198", "p1.225", "p1.236", "p1.251", "p1.274", "p1.252")



long_cycles <- c("p1.101", "p1.329", "p1.105", "p1.385", "p1.124", "p1.176",
  "p1.392")


short_cycles <- c("p1.252")


# Creating the variable to remove people who have too few backward count days 
condition <- df |> group_by(PROLIFIC_PID) |> 
  filter(!is.na(estr_b)) |> 
  count(PROLIFIC_PID, name = "bc_days_n") 
condition
df <- df |> left_join(condition) 

filtered_df <- df |> 
  filter(
    bc_days_n >= 10,
    !(PROLIFIC_PID %in% excluded_ids),
    !(PROLIFIC_PID == 'p1.144' & studyday > 15) &
    !(df$PROLIFIC_PID %in% long_cycles) &
    !(df$PROLIFIC_PID %in% short_cycles)
  )
#> Okay:  I do get the same exact df length filtering as co-author did. I also get 
#> the same exact valid n, and mean, and sd. 
 
filtered_df |> summarize(
  across(matches("^(estr|prog)$"), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    N = ~n(),
    valid_n =  ~sum(!is.na(.))
  ))
)  |>  pivot_longer(
  cols = everything(),
  names_to = c("variable", "stat"),
  names_pattern = "^(.*)_(mean|sd|N|valid_n)$",
  values_to = "value"
) |> 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

length(unique(filtered_df$PROLIFIC_PID)) #251 unique participants now 

#> Variable Notes (READ ME) ------

#> Several variables have been brought in from the dataframe sent to me because
#> I do not know how to create them or I couldn't convert the SPSS syntax into
#> R code properly. 

#> Three Variables: 
#> (1) mensesfollowday
#> (2) MENSES1DAY
#> (3) MENSES2DAY

###### COMMENTS from Co-author 
# These variables are derived from participants' reports of when they started menses. 
# They can't readily be calculated from the responses given to the diaries. I set 
# up a separate excel file and confirmed, for each individual, when they started 
# menses. MENSES1DAY and MENSES2 DAY refer to the first menses they reported and 
# the second menses they reported, if either. 

#> On 12/27/24 I just copied the above 3 variables into the new df I am creating
#> to make additional variables. 
#> Of greatest concern, these variables are used to create: 
#> (1) bc_day
#> (2) fc_day 
#> (3) bctypical_day

#> I was able to properly make the above variables and then worked on hormone
#> counts. The backward, forward, and backward typical hormone counts were all
#> created. 
#> 


#> Looking at this again as of 4/22/2025 (preparing second dataset now)
#> Per my note on 12/30/24, I don't have one variable needed to make the condition
#> for the filtering down work "estr_b_cgt" #> I highlighted that code out for now. 





#> Variable Labels (leave at end of doc and rerun as needed) -------

var_label(df) <- list(
  #variable name in df = "description of the variable"
  prolifID = "Unique participant ID from Prolific",
  rellength = "Relationship length recoded from years + months"
)


#> Saving Dataframes --------
#I'm going to save a filtered df and a full df. 

#### FULL DATAFRAME 
# Save as CSV file
write.csv(df, file = "data/analysis_data/prolific1_full.csv", row.names = FALSE)

# Save as SAV file (requires haven package)
write_sav(df, path = "data/analysis_data/prolific1_full.sav")

# Save as RDS file
saveRDS(df, file = "data/analysis_data/prolific1_full.rds")


#### FILTERED DATAFRAME 
# Save as CSV file
write.csv(filtered_df, file = "data/analysis_data/prolific1_filtered.csv", row.names = FALSE)

# Save as SAV file (requires haven package)
write_sav(filtered_df, path = "data/analysis_data/prolific1_filtered.sav")

# Save as RDS file
saveRDS(filtered_df, file = "data/analysis_data/prolific1_filtered.rds")




