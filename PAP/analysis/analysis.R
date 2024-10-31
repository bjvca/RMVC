rm(list=ls())

library(clubSandwich)

path <- getwd()
path <- strsplit(path,"/PAP/analysis")[[1]]

### read in clean farmer data
farmers_end <- read.csv(paste(path, "endline/data/public/farmers.csv", sep = "/"))

farmers_base <- read.csv(paste(path, "baseline/data/public/farmers.csv", sep = "/"))

###catch 
farmers_end$catch_ID <- as.factor(farmers_end$catchment)

##treat is MCC level treatment; vid is farmer level treatment
farmers_end$treat <- farmers_end$treat == "T" 

##merge in relevant data from baseline
# farmer type (direct of via trader)
farmers_end$farmer_type

names(farmers_end)[names(farmers_end) == 'farmer_type'] <- 'farmer_type_end'

farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","farmer_type")], by="farmer_ID", all.x=T)
farmers_end$trader <- farmers_end$farmer_type == 2
#six primary outcomes at the farmer level


###merge in corresponding baseline variable
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","q53.2")],by="farmer_ID",all.x=T)
farmers_end$MMC_delivery <- farmers_end$q53.2.x == "True"
farmers_end$b_MMC_delivery <- farmers_end$q53.2.y == "True"

outcomes <- c("MMC_delivery")
b_outcomes <- c("b_MMC_delivery")

##Sold to milk to collection center in the week preceding the survey
###fully interacted model
res_farmer <-   array(NA,dim=c(length(outcomes),18))
for (i in 1:length(outcomes)) {
ols <- lm(as.formula(paste(paste(outcomes[i],"treat*vid*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)
res_farmer[i,1] <- mean(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
res_farmer[i,2] <- sd(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
res_farmer[i,3:7] <- c(res[2,2],res[2,3],res[2,6], conf[2,5],conf[2,6])
res_farmer[i,8:12] <- c(res[3,2],res[3,3],res[3,6], conf[3,5],conf[3,6])
res_farmer[i,13:18] <- c(res[5,2],res[5,3],res[5,6], conf[5,5],conf[5,6], nobs(ols))
}
###model with demeaned orthogonal treatment
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)
 
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmer[i,3:7] <- c(res[2,2],res[2,3],res[2,6], conf[2,5],conf[2,6])

}
###model with demeaned orthogonal treatment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmer[i,3:7] <- c(res[2,2],res[2,3],res[2,6], conf[2,5],conf[2,6])
  
}
