rm(list=ls())
path <- getwd()
library(clubSandwich)
library(andersonTools)
library(car)

path <- strsplit(path, "/PAP/analysis")[[1]]
MCC <- read.csv(paste(path,"baseline/data/public/MCCs.csv", sep="/"))

MCC$is_coop <- MCC$secC_group.q8==2
MCC$full_timers <-  as.numeric(as.character(MCC$q14m)) 
MCC$clients_rainy <-  as.numeric(as.character(MCC$Q23))  
MCC$clients_rainy[MCC$clients_rainy>1000] <- NA 
MCC$clients_rainy[MCC$clients_rainy<5] <- NA 
MCC$capacity <-  as.numeric(as.character(MCC$qual.q26))  
MCC$amount_dry <-  as.numeric(as.character(MCC$qual.q27))  
MCC$capacity_use_dry <- MCC$amount_dry/MCC$capacity *100
MCC$capacity_use_dry[MCC$capacity_use_dry>100] <- NA
MCC$years_in_operation <-  as.numeric(as.character(MCC$secC_group.q6a))  
MCC$q29[MCC$q29 == "98"] <- NA
MCC$quality_premium <-  as.numeric(as.character(MCC$q29)) == 1 
MCC$years_in_operation[MCC$years_in_operation >50] <- NA 
MCC$nr_milk_cans <-  as.numeric(as.character(MCC$q14i)) 

MCC$supply_credit <-  as.numeric(as.character(MCC$q17)) %in% c(1,2)   
MCC$supply_accaracides <-  as.numeric(as.character(MCC$q18))   %in% c(1,2)



outcomes <- c("is_coop","full_timers","clients_rainy","capacity","capacity_use_dry","quality_premium","years_in_operation", "nr_milk_cans", "supply_credit","supply_accaracides")

##balance is matrix that collects outcomes - ctrl mean, ctrl sd, TE, se(TE), p-val,-lim,+lim,nobs
balance <-   array(NA,dim=c(length(outcomes),8))
for (i in 1:length(outcomes)) {
ols <- lm(as.formula(paste(outcomes[i],"lactoscan",sep="~")), data=MCC) 
vcov_cluster <- vcovCR(ols, cluster = MCC$catchment_ID, type = "CR2")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)
balance[i,1] <- mean(as.matrix(MCC[MCC$lactoscan == "C",outcomes[i]]), na.rm=T)
balance[i,2] <- sd(as.matrix(MCC[MCC$lactoscan == "C",outcomes[i]]), na.rm=T)
balance[i,3:8] <- c(res[2,2],res[2,3],res[2,7], conf[2,5],conf[2,6], nobs(ols))
}

##f-test for main table
F_test <- array(NA,dim=c(2,8))
mod <- lm((lactoscan=="T") ~ is_coop+capacity+quality_premium+years_in_operation+supply_accaracides, data=MCC)

W <- Wald_test(mod, constraints = constrain_zero(c("is_coopTRUE","capacity","quality_premiumTRUE","years_in_operation","supply_accaracidesTRUE")),
               vcov = "CR2", cluster = MCC$catchment_ID, test = "HTZ")  # joint test

F_test[1,1] <- W$Fstat
F_test[2,1] <- W$p_val

##f-test for appendix table
mod <- lm((lactoscan=="T") ~ full_timers+clients_rainy+capacity_use_dry+nr_milk_cans+supply_credit, data=MCC)

W <- Wald_test(mod, constraints = constrain_zero(c("full_timers","clients_rainy","capacity_use_dry","nr_milk_cans","supply_creditTRUE")),
               vcov = "CR2", cluster = MCC$catchment_ID, test = "HTZ")  # joint test

F_test[1,2] <- W$Fstat
F_test[2,2] <- W$p_val

balance[,1:7] <- round(balance[,1:7],digits=3)

saveRDS(balance, file= paste(path,"PAP/results/balance.RData", sep="/"))


###this is at farmer level

farmers <- read.csv(paste(path,"baseline/data/public/farmers.csv", sep="/"))

farmers$hh_size <- as.numeric(as.character(farmers$q21))

farmers$age_head <- as.numeric(as.character(farmers$q18))
farmers$age_head[is.na(farmers$age_head)] <- as.numeric(as.character(farmers$q14[is.na(farmers$age_head)] ))

## herd

set <- c("q24", "q26","q28","q30","q32","q34")

farmers$q24[farmers$q24 >500 ] <- NA
farmers$q24[farmers$q26 >200 ] <- NA
farmers$q24[farmers$q28 >200 ] <- NA


farmers$q24[farmers$q30 >500 ] <- NA
farmers$q24[farmers$q32 >200 ] <- NA
farmers$q24[farmers$q34 >200 ] <- NA

farmers[set] <- lapply(farmers[set],  function(x) replace(x,x==999,NA))
farmers[set] <- lapply(farmers[set],  function(x) as.numeric(as.character(x)))
farmers$herd_size <- rowSums(farmers[set])

farmers$improved_share <- rowSums(farmers[c("q30","q32","q34")]) /farmers$herd_size
farmers$liter_day_wet <- farmers$q44
farmers$liter_sold_day_wet <- farmers$q50
farmers$sell_MCC_wet <- (farmers$q51 == 2)

farmers$use_steel <- (farmers$q60 %in% c("4","6") | farmers$qx7 %in% c("4","6") | farmers$qx19 %in% c("4","6") | farmers$qx31 %in% c("4","6") | farmers$qx43 %in% c("4","6") | farmers$qx55 %in% c("4","6"))
farmers$coop_member <- farmers$q22 == "Yes"
farmers$acaracide_exp <- as.numeric(as.character(farmers$Tick3.q74))/3700

farmers$trader <- ifelse(sub("^[^0-9]*[0-9]+", "", farmers$farmer_ID) %in% c("_T", "_T_R"), 1, 0)


outcomes <- c("hh_size", "age_head","herd_size","improved_share","liter_day_wet","liter_sold_day_wet","sell_MCC_wet", "use_steel", "coop_member", "acaracide_exp")


###dataset for comparing treatment to interaction directly
farmers_int <-  farmers[(farmers$video_shown==TRUE & farmers$lactoscan=="T") | (farmers$video_shown==FALSE & farmers$lactoscan=="C"),]



##balance is matrix that collects outcomes - ctrl mean, ctrl sd, TE, se(TE), p-val,-lim,+lim,nobs
balance_farmer <-   array(NA,dim=c(length(outcomes),32))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i],"lactoscan*video_shown*trader",sep="~")), data=farmers) 
  vcov_cluster <- vcovCR(ols, cluster = farmers$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  balance_farmer[i,1] <- mean(as.matrix(farmers[(farmers$lactoscan == "C" & farmers$video_shown == FALSE), outcomes[i]]), na.rm=T)
  balance_farmer[i,2] <- sd(as.matrix(farmers[(farmers$lactoscan == "C" & farmers$video_shown == FALSE),outcomes[i]]), na.rm=T)

  
  
  
  balance_farmer[i,3:5] <- c(res[2,2],res[2,3],res[2,7])
  balance_farmer[i,6:8] <- c(res[3,2],res[3,3],res[3,7])
  balance_farmer[i,9:12] <- c(res[5,2],res[5,3],res[5,7], nobs(ols))
  
  balance_farmer[i,13] <- linearHypothesis(ols, c("lactoscanT = lactoscanT:trader") , vcov. = vcov_cluster)[[4]][2]
  balance_farmer[i,14] <- linearHypothesis(ols, c("video_shownTRUE = video_shownTRUE:trader") , vcov. = vcov_cluster)[[4]][2]
  balance_farmer[i,15] <- linearHypothesis(ols, c("lactoscanT:video_shownTRUE = lactoscanT:video_shownTRUE:trader") , vcov. = vcov_cluster)[[4]][2]
  
  ###directly compare control to interaction - we can use treat as indicator as this automatically also has vid
  ols <- lm(as.formula(paste(outcomes[i],"lactoscan*trader",sep="~")), data=farmers_int) 
  vcov_cluster <- vcovCR(ols,cluster=farmers_int$catchment_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  balance_farmer[i,27:29] <- c(res[2,2],res[2,3],res[2,7])
  balance_farmer[i,30] <- res[5,7] ###interaction with trader
 
  
}

balance_farmer[1:length(outcomes),16] <- anderson_sharp_q(balance_farmer[1:length(outcomes),5])
balance_farmer[1:length(outcomes),17] <- anderson_sharp_q(balance_farmer[1:length(outcomes),8])
balance_farmer[1:length(outcomes),18] <- anderson_sharp_q(balance_farmer[1:length(outcomes),11])
balance_farmer[1:length(outcomes),31] <- anderson_sharp_q(balance_farmer[1:length(outcomes),29])

###model with demeaned orthogonal treatment - milk analyzer
farmers$trader_demeaned  <- farmers$trader - mean(farmers$trader, na.rm=T)
farmers$vid_demeaned <- farmers$video_shown - mean(farmers$video_shown, na.rm=T)

for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"lactoscan*vid_demeaned",sep="~")), data=farmers)
  vcov_cluster <- vcovCR(ols,cluster=farmers$catchment_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  balance_farmer[i,19:21] <- c(res[2,2],res[2,3],res[2,7])
}
###model with demeaned orthogonal treatment - video treatment



farmers$lactoscan_demeaned <- (farmers$lactoscan == "T") - mean(farmers$lactoscan =="T", na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"video_shown*lactoscan_demeaned",sep="~")), data=farmers)
  vcov_cluster <- vcovCR(ols,cluster=farmers$catchment_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  balance_farmer[i,22:24] <- c(res[2,2],res[2,3],res[2,7])
}

balance_farmer[1:length(outcomes),25] <- anderson_sharp_q(balance_farmer[1:length(outcomes),21])
balance_farmer[1:length(outcomes),26] <- anderson_sharp_q(balance_farmer[1:length(outcomes),24])


balance_farmer <- round(balance_farmer,digits=3)




balance_farmer[,1:17] <- round(balance_farmer[,1:17],digits=3)

mod <- lm((lactoscan=="T") ~age_head+herd_size+improved_share+liter_sold_day_wet+acaracide_exp, data=farmers)
vcov_cluster <- vcovCR(mod,cluster=farmers$catchment_ID,type="CR2")


W <- Wald_test(mod, constraints = constrain_zero(c("age_head","herd_size","improved_share","liter_sold_day_wet","acaracide_exp")),
               vcov = "CR2", cluster = farmers$catchment_ID, test = "HTZ")  # joint test

F_test[1,3] <- W$Fstat
F_test[2,3] <- W$p_val


mod <- lm(video_shown ~age_head+herd_size+improved_share+liter_sold_day_wet+acaracide_exp, data=farmers)
W <- Wald_test(mod, constraints = constrain_zero(c("age_head","herd_size","improved_share","liter_sold_day_wet","acaracide_exp")),
               vcov = "CR2", cluster = farmers$catchment_ID, test = "HTZ")  # joint test

F_test[1,4] <- W$Fstat
F_test[2,4] <- W$p_val

mod <- lm((video_shown*(lactoscan=="T")) ~age_head+herd_size+improved_share+liter_sold_day_wet+acaracide_exp, data=farmers)
W <- Wald_test(mod, constraints = constrain_zero(c("age_head","herd_size","improved_share","liter_sold_day_wet","acaracide_exp")),
               vcov = "CR2", cluster = farmers$catchment_ID, test = "HTZ")  # joint test

F_test[1,5] <- W$Fstat
F_test[2,5] <- W$p_val

###now for appendix table
mod <- lm((lactoscan=="T") ~hh_size+liter_day_wet+sell_MCC_wet+use_steel+coop_member, data=farmers)
W <- Wald_test(mod, constraints = constrain_zero(c("hh_size","liter_day_wet","sell_MCC_wetTRUE", "use_steelTRUE","coop_memberTRUE")),
               vcov = "CR2", cluster = farmers$catchment_ID, test = "HTZ")  # joint test

F_test[1,6] <- W$Fstat
F_test[2,6] <- W$p_val


mod <- lm(video_shown ~hh_size+liter_day_wet+sell_MCC_wet+use_steel+coop_member, data=farmers)
W <- Wald_test(mod, constraints = constrain_zero(c("hh_size","liter_day_wet","sell_MCC_wetTRUE", "use_steelTRUE","coop_memberTRUE")),
               vcov = "CR2", cluster = farmers$catchment_ID, test = "HTZ")  # joint test

F_test[1,7] <- W$Fstat
F_test[2,7] <- W$p_val


mod <-lm((video_shown*(lactoscan=="T")) ~hh_size+liter_day_wet+sell_MCC_wet+use_steel+coop_member, data=farmers)
W <- Wald_test(mod, constraints = constrain_zero(c("hh_size","liter_day_wet","sell_MCC_wetTRUE", "use_steelTRUE","coop_memberTRUE")),
               vcov = "CR2", cluster = farmers$catchment_ID, test = "HTZ")  # joint test

F_test[1,8] <- W$Fstat
F_test[2,8] <- W$p_val


F_test <- round(F_test,digits=3)


saveRDS(balance_farmer, file= paste(path,"PAP/results/balance_farmer.RData", sep="/"))
saveRDS(F_test, file= paste(path,"PAP/results/F_test.RData", sep="/"))

