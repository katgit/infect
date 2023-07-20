

library(meta)
library(effectsize)
library(compute.es)
#library(data.table)


library(tidyverse)
#library(dplyr)
#library(ggplot2)

library(metafor)
library(metaforest)
#require(devtools)
library(clubSandwich)
library(esc)

rm(list = ls())

#KatiaL explore where NS values are and what they mean
alldata <- data.table::fread("CSVdata.csv", data.table=FALSE, 
                             na.strings=c(getOption("datatable.na.strings","NA"), "NS"))
#df <- fread("C:\\Users\\Brandon\\Desktop\\Research\\DyslipidAMD_Vavvas\\CSVdata.csv")
#df -> alldata

str(alldata)
class(alldata$TG_SD_allAMD)

#Katia: explore why this (and some other columns contains NS value)
alldata$TC_N_allAMD

# convert columns to be numeric
# This needs to be done because some of the values have "\n" character (line feed)
# and as a result are read as a character value.
alldata_fixed <- alldata %>% 
  mutate(across( c(matches("TG_N"), matches("TG_SD"), matches("TG_mean"),matches("TG_ci"),
                   matches("TC_N"), matches("TC_SD"), matches("TC_mean"),matches("TC_ci"),
                   matches("HDL_N"), matches("HDL_SD"), matches("HDL_mean"),matches("HDL_ci"),
                   matches("LDL_N"), matches("LDL_SD"), matches("LDL_mean"), matches("LDL_ci")),
                 as.numeric))


#mutate: convert all means-based results to mg/dL **completely unnecessary and can take out at end if not presenting study characteristics or something
to_mg_dL1 <- function(x){ x * 88.56}
to_mg_dL2 <- function(x){ x * 38.67}

alldata1 <- alldata_fixed %>% 
  mutate( across( c(matches("TG_SD"), matches("TG_mean")), 
                  ~ifelse(Unitofserumlipid == "mmol/L", to_mg_dL1(.x), .x),
                  .names = "{.col}_conv")) %>%
  mutate( across( c(matches("TC_SD"), matches("TC_mean"),
                    matches("HDL_SD"), matches("HDL_mean"),
                    matches("LDL_SD"), matches("LDL_mean")), 
                  ~ifelse(Unitofserumlipid == "mmol/L", to_mg_dL2(.x), .x),
                  .names = "{.col}_conv"))


## -------------------------------------------- ##
##   calculate SMD's for TG ctrl vs. all AMD    ##
## -------------------------------------------- ##
calc_SMD <- function(df, prefix, suffix){
  m2i <- rlang::sym( paste(prefix, "mean_ctrl_conv", sep="_") )
  m1i <- rlang::sym( paste(prefix, "mean", suffix, "conv", sep="_") )
  sd2i <- rlang::sym( paste(prefix, "SD_ctrl_conv", sep="_") )
  sd1i <- rlang::sym( paste(prefix, "SD", suffix, "conv", sep="_") )
  n2i <- rlang::sym( paste(prefix, "N_ctrl", sep="_") )
  n1i <- rlang::sym( paste(prefix, "N", suffix, sep="_") )
  
  
  df1 <- escalc(measure = "SMD",                      
               m2i = df%>% pull(!!m2i),
               m1i = df%>% pull(!!m1i),
               sd2i = df%>% pull(!!sd2i),
               sd1i = df%>% pull(!!sd1i),
               n2i = df%>% pull(!!n2i),
               n1i = df%>% pull(!!n1i))
  
  names(df1)[names(df1) == "yi"] <- paste("yi", prefix, "ctrl", suffix, sep="_")
  names(df1)[names(df1) == "vi"] <- paste("vi", prefix, "ctrl", suffix, sep="_")
  df1
}
## --------------------------------------------------
# An example of running this function:
#result <- calc_SMD(df=alldata1, prefix="TG", suffix="allAMD") 

# Apply this function for all cases
prefix_list <- rep(c("TG", "TC", "HDL", "LDL"), each=4)
suffix_list <- rep(c("allAMD", "eidry", "advdry", "advwet"), times=4)
mlist <- mapply(calc_SMD, prefix=prefix_list, suffix=suffix_list, MoreArgs=list(df = alldata1),
                SIMPLIFY = FALSE)

# mlist contains 32 small dataframes each with 2 columns, e.g.
# yi_TG_ctrl_allAMD and vi_TG_ctrl_allAMD
# Let's put them all together and then add them to the original dataframe
alldata2 <- cbind(alldata1 , bind_cols(mlist) )


#adjust for CI's that are 0.
# Katia: actually there are no 0 here. Most values are missing (NA)
# These columns are already numeric, so these lines are redundant
class(alldata2$TG_ciU_advdry)
alldata2$TG_ciU_advdry <- as.numeric(alldata2$TG_ciU_advdry) 
alldata2$TG_ciL_advdry <- as.numeric(alldata2$TG_ciL_advdry) 
alldata2$TG_ciU_advwet <- as.numeric(alldata2$TG_ciU_advwet) 
alldata2$TG_ciL_advwet <- as.numeric(alldata2$TG_ciL_advwet) 

#------------------------------------------#
# Adjust ciU and ciL
#
# This function returns a dataframe with 
# two new columns
#------------------------------------------#
calc_adj <- function(df, prefix, suffix){
  U <- rlang::sym( paste(prefix, "ciU", suffix, sep="_") )
  L <- rlang::sym( paste(prefix, "ciL", suffix, sep="_") )
  Uadj <-  (paste(prefix, "ciU", suffix, "adj", sep="_") )
  Ladj <-  (paste(prefix, "ciL", suffix, "adj", sep="_") )
  
  UL <- df %>% 
    #select((!!U), (!!L)) %>%
    mutate((!!Uadj) := if_else((!!U) - (!!L) == 0, (!!U) + 0.004, (!!U)), 
           (!!Ladj) := if_else((!!U) - (!!L) == 0, (!!L) - 0.004, (!!L))) %>%
    select((!!Uadj), (!!Ladj))
  UL
}
# End of function -------------------------

# Example of usage of the function
#df <- calc_adj(df = alldata2, prefix = "TG", suffix = "allAMD")
#head(df)

prefix_list <- rep(c("TG", "TC", "HDL", "LDL"), each=4)
suffix_list <- rep(c("allAMD", "eidry", "advdry", "advwet"), times=4)
mlist <- mapply(calc_adj, prefix=prefix_list, suffix=suffix_list, MoreArgs=list(df = alldata2),
                SIMPLIFY = FALSE)
# Note: alldata3 is what previously was alldata1.5
alldata3 <- cbind(alldata2 , bind_cols(mlist) )

# Check the names in the output
tail(names(alldata3),16)

#------------------------------------------#
# convert all odds ratios into SMD's
#
# This function returns a dataframe with 
# two new columns
#------------------------------------------#
calc_covert_odds_ratio <- function(df, prefix, suffix){
  U <- rlang::sym( paste(prefix, "ciU", suffix, "adj", sep="_") )
  L <- rlang::sym( paste(prefix, "ciL", suffix, "adj", sep="_") )
  vi <-  (paste("vi",prefix, "ctrl", suffix, "R", sep="_") )

  UL <- df %>% 
    mutate((!!vi) := (((log(!!U) - log(!!L))/3.92) ^2) *3 /(pi^2)  ) %>%
    select((!!vi))
  UL
}
#---------------------------------------------

# test function
df <- calc_covert_odds_ratio(df = alldata3, prefix="TG", suffix="allAMD")
head(df)


prefix_list <- rep(c("TG", "TC", "HDL", "LDL"), each=4)
suffix_list <- rep(c("allAMD", "eidry", "advdry", "advwet"), times=4)
mlist <- mapply(calc_covert_odds_ratio, 
                prefix=prefix_list, 
                suffix=suffix_list, 
                MoreArgs=list(df = alldata3),
                SIMPLIFY = FALSE)
# Note: alldata4 is what previously was alldata2
alldata4 <- cbind(alldata3 , bind_cols(mlist) )

# Check the names in the output
tail(names(alldata4),16)


##CONVERT ODDS RATIO TO ODDS BEFORE PLUGGING INTO THIS FORMULA
alldata_wlogs <- alldata4 %>%
  mutate(across(c(matches("_Ratio_")), 
                log, .names = "{.col}_logOR"))
tail(names(alldata_wlogs),16)


##-------------------------------------
## Apply lores() function
##-------------------------------------

#trying straight to log to hedge's g
calc_lores <- function(df, prefix, suffix){
  
  lor     <- paste(prefix, "Ratio", suffix, "logOR", sep="_") 
  var.lor <- paste("vi", prefix, "ctrl", suffix, "R", sep="_") 
  n.1 <-  (paste(prefix, "n_ctrl_R",  sep="_") )
  n.2 <-  (paste(prefix, "n", suffix, "R", sep="_") )
  out <-  (paste(prefix, suffix, sep="") )
  
  result <- lores(lor = df %>% pull(lor),
                  var.lor = df %>% pull(var.lor), 
                  n.1 = df %>% pull(n.1),
                  n.2 = df %>% pull(n.2), 
                  level = 95, cer = 0.2, dig = 7, verbose = TRUE, id=NULL, data=NULL)
  result
}
# End of function -------------------------
prefix_list <- rep(c("TG", "TC", "HDL", "LDL"), each=4)
suffix_list <- rep(c("allAMD", "eidry", "advdry", "advwet"), times=4)
mlist <- mapply(calc_lores, 
                prefix=prefix_list, 
                suffix=suffix_list, 
                MoreArgs=list(df = alldata_wlogs),
                SIMPLIFY = FALSE)
names(mlist) <- paste0(prefix_list, suffix_list)



#create nested models

#------------------------------------------#
# coalescing (first time)
#------------------------------------------#
calc_coalesce1 <- function(df, mlist,  prefix, suffix){
  first  <- paste("yi",prefix, "ctrl", suffix, sep="_") 
  second <- paste(prefix, suffix,  sep="") 
  out <-  (paste("yi",prefix, "ctrl", suffix, "comb", sep="_") )
  
  result <- coalesce(df %>% pull(first), 
                     mlist[[second]]$g)
  result
}
#---------------------------------------------
# test function
# result <- calc_coalesce1(df = alldata_wlogs, mlist = mlist, 
#               prefix = "TG", suffix = "allAMD")
# head(result)
coal_list <- mapply(calc_coalesce1, 
                    prefix=prefix_list, 
                    suffix=suffix_list, 
                    MoreArgs=list(df = alldata_wlogs, mlist = mlist),
                    SIMPLIFY = FALSE)
names(coal_list) <- paste("yi", prefix_list, "ctrl", suffix_list, "comb", sep="_")
# alldata4.new is what before was alldata4
alldata4.new <- cbind(alldata_wlogs, bind_cols(coal_list))



#------------------------------------------#
# coalescing (second time)
#------------------------------------------#
calc_coalesce2 <- function(df, prefix, suffix){
  first  <- paste("vi",prefix, "ctrl", suffix, sep="_") 
  second <- paste("vi",prefix, "ctrl", suffix, "R", sep="_") 
  
  result <- coalesce(df %>% pull(first), 
                     df %>% pull(second))
  result
}
#---------------------------------------------
coal_list2 <- mapply(calc_coalesce2, 
                    prefix=prefix_list, 
                    suffix=suffix_list, 
                    MoreArgs=list(df = alldata4.new),
                    SIMPLIFY = FALSE)
names(coal_list2) <- paste("vi", prefix_list, "ctrl", suffix_list, "comb", sep="_")
# alldata4.new is what before was alldata4
alldata5.new <- cbind(alldata4.new, bind_cols(coal_list2))



#-----------------------------------------------------
#creating new table that only include relevant tables
#-----------------------------------------------------
calc_model_list <- function(df, suffix, prefix) {
  yi <- df %>% pull( paste("yi", prefix, "ctrl", suffix, "comb", sep="_") )
  V <- df %>% pull( paste("vi", prefix, "ctrl", suffix, "comb", sep="_") )
  #multi-level nested model 
  rma.mv(yi = yi, V = V, random = ~1 | StudyID/EffectID, data=df,tdist = TRUE)
}
# End of function -------------------------

model_list <- mapply(calc_model_list, 
                     prefix=prefix_list, 
                     suffix=suffix_list, 
                     MoreArgs=list(df = alldata5.new),
                     SIMPLIFY = FALSE)
names(model_list) <- paste("m", prefix_list, suffix_list, sep="_")





#-----------------------------------------------------
# Calculating I^2
#-----------------------------------------------------
calc_I_2 <- function(ilist, icol){

  modl <- ilist[[icol]]
  W <- diag(1/modl$vi)
  X <- model.matrix(modl)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  val1 <- 100 * sum(modl$sigma2) / (sum(modl$sigma2) + (modl$k-modl$p)/sum(diag(P)))
  val2 <- 100 * modl$sigma2 / (sum(modl$sigma2) + (modl$k-modl$p)/sum(diag(P)))
  list(W=W, X=X, P=P, val1=val1, val2=val2)
}
# End of function -------------------------

I_2_list <- lapply(names(model_list), calc_I_2, ilist = model_list)
names(I_2_list) <- names(model_list)


#------------------------------------------#
# coalescing (third time)
#------------------------------------------#
calc_coalesce3 <- function(df, prefix, suffix){
  first  <- paste(prefix, "N", suffix, sep="_") 
  second <- paste(prefix, "n", suffix, "R", sep="_") 
  
  result <- coalesce(df %>% pull(first), 
                     df %>% pull(second))
  result
}
#---------------------------------------------
prefix_list <- rep(c("TG", "TC", "HDL", "LDL"), each=5)
suffix_list <- rep(c("ctrl", "allAMD", "eidry", "advdry", "advwet"), times=4)
coal_list3 <- mapply(calc_coalesce3, 
                     prefix=prefix_list, 
                     suffix=suffix_list, 
                     MoreArgs=list(df = alldata5.new),
                     SIMPLIFY = FALSE)
names(coal_list3) <- paste("N_comb", prefix_list, suffix_list,  sep="_")
alldata5 <- cbind(alldata5.new, bind_cols(coal_list3))



