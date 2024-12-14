rm(list=ls())

library(clubSandwich)
library(andersonTools)
library(car)
library(lubridate)
library(ggplot2)


### function definition for inverse hyperbolic sine transform
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)}

path <- getwd()
path <- strsplit(path,"/PAP/analysis")[[1]]

### read in endline farmer data
farmers_end <- read.csv(paste(path, "endline/data/public/farmers.csv", sep = "/"))

### read in baseline farmer data to control for baseline outcomes
farmers_base <- read.csv(paste(path, "baseline/data/public/farmers.csv", sep = "/"))

###catchment area for clustering
farmers_end$catch_ID <- as.factor(farmers_end$catchment)

##treat is MCC level treatment; vid is farmer level treatment
farmers_end$treat <- farmers_end$treat == "T"

table(farmers_end$treat,farmers_end$vid)

##merge in relevant data from baseline
# farmer type (direct of via trader) for treatment heterogeiety
names(farmers_end)[names(farmers_end) == 'farmer_type'] <- 'farmer_type_end'

farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","farmer_type")], by="farmer_ID", all.x=T)
farmers_end$trader <- farmers_end$farmer_type == 2

table(farmers_end$trader) # this did not really work that well - only 600 are linked via a trader and 1610 are direct
#primary outcomes at the farmer level

#q39 == "Yes" ##oversowing
#q39c == "Yes" ##legume pastures
#q39d == "Yes" ##trees
#q40 in c(1,3) #controlled/zero grazing in dry seaon
#q41 in c(1,3) #controlled/zero grazing in wet seaon
#q42 == "Yes" #pasture conservation
#q43 == "Yes" ##supplements
farmers_end$q39[farmers_end$q39 == "n/a"] <- NA
farmers_end$q39c[farmers_end$q39c == "n/a" | farmers_end$q39c == "98"] <- NA
farmers_end$q39d[farmers_end$q39d == "n/a" | farmers_end$q39d == "98"] <- NA
farmers_end$q42[farmers_end$q42 == "n/a" | farmers_end$q42 == "98"] <- NA
farmers_end$q40[farmers_end$q40 == "n/a" | farmers_end$q40 == "96"] <- NA
farmers_end$q41[farmers_end$q41 == "n/a" | farmers_end$q41 == "96"] <- NA
farmers_end$q43[farmers_end$q43 == "n/a"] <- NA

farmers_base$b_improve_index <- anderson_index(cbind(farmers_base$q39 == "Yes",farmers_base$q39c  == "Yes", farmers_base$q40 %in% c(1,3), farmers_base$q41 %in% c(1,3), farmers_base$q42  == "Yes", farmers_base$q43  == "Yes"))$index
farmers_end$improve_index <- anderson_index(cbind(farmers_end$q39 == "Yes",farmers_end$q39c  == "Yes", farmers_end$q40 %in% c(1,3), farmers_end$q41 %in% c(1,3), farmers_end$q42  == "Yes", farmers_end$q43  == "Yes"))$index
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_improve_index")], by="farmer_ID", all.x=T)

##buyer checks using milk analyzer - needs more work - only for those who actually sold
farmers_end$check_MA <- farmers_end$q58.4 == "True" | farmers_end$qx5.4 == "True" | farmers_end$qx17.4 == "True" | farmers_end$qx29.4 == "True" | farmers_end$qx41.4 == "True" | farmers_end$qx53.4 == "True"
farmers_end$check_MA[farmers_end$q52 == "n/a" | farmers_end$q52 == "No"] <- NA
farmers_base$b_check_MA <- farmers_base$q58== "4" | farmers_base$qx5.4 == "True" | farmers_base$qx17.4 == "True" | farmers_base$qx29.4 == "True" | farmers_base$qx41.4 == "True" | farmers_base$qx53.4 == "True"
farmers_base$b_check_MA[farmers_base$q52 == "No"] <- NA
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_check_MA")], by="farmer_ID", all.x=T)

##Price received for milk sold (inclusive of any quality premium that may have been obtained) (price per liter, average of price during last transaction with in last 7 days with buyer) - q55/qx2/qx14qx26/qx38/qx50

columns <- c("q55","qx2","qx14","qx26","qx38","qx50")

# Replace "n/a" and "999" with NA in each specified column
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

farmers_end$avg_sales_p <- rowMeans(farmers_end[columns], na.rm=T)
farmers_end$avg_sales_p[is.nan(farmers_end$avg_sales_p)] <- NA

farmers_base$b_avg_sales_p <- rowMeans(farmers_base[columns], na.rm=T)
farmers_base$b_avg_sales_p[is.nan(farmers_base$b_avg_sales_p)] <- NA
##remove outliers
farmers_base$b_avg_sales_p[farmers_base$b_avg_sales_p >= 1200] <- NA

farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_avg_sales_p")], by="farmer_ID", all.x=T)

## Does the buyer pay for higher quality milk - q61/qx8/qx20/qx32/qx44/qx56
farmers_end$gets_q_bonus <- farmers_end$q61 == "Yes" | farmers_end$qx8 == "Yes" | farmers_end$qx20 == "Yes" | farmers_end$qx32 == "Yes" | farmers_end$qx44 == "Yes" | farmers_end$qx56 == "Yes"
farmers_end$gets_q_bonus[farmers_end$q52 == "n/a" | farmers_end$q52 == "No"] <- NA

farmers_base$b_gets_q_bonus <- farmers_base$q61 == "Yes" | farmers_base$qx8 == "Yes" | farmers_base$qx20 == "Yes" | farmers_base$qx32 == "Yes" | farmers_base$qx44 == "Yes" | farmers_base$qx56 == "Yes"
farmers_base$b_gets_q_bonus[farmers_base$q52 == "No"] <- NA

farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_gets_q_bonus")], by="farmer_ID", all.x=T)


### farmers improve bargaining power
farmers_end$bargain_power <- farmers_end$q65 %in% c(1,3) | farmers_end$qx12 %in% c(1,3) | farmers_end$qx24 %in% c(1,3)  | farmers_end$qx36 %in% c(1,3) | farmers_end$qx48 %in% c(1,3) | farmers_end$qx60 %in% c(1,3)
farmers_end$bargain_power[farmers_end$q52 == "n/a" | farmers_end$q52 == "No"] <- NA

farmers_base$b_bargain_power <- farmers_base$q65 %in% c(1,3) | farmers_base$qx12 %in% c(1,3) | farmers_base$qx24 %in% c(1,3)  | farmers_base$qx36 %in% c(1,3) | farmers_base$qx48 %in% c(1,3) | farmers_base$qx60 %in% c(1,3)
farmers_base$b_bargain_power[farmers_base$q52 == "No"] <- NA

farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_bargain_power")], by="farmer_ID", all.x=T)

##### put outcomes in an array to loop over (and make anderson index)
outcomes <- c("improve_index","check_MA","avg_sales_p","gets_q_bonus","bargain_power")
b_outcomes <- c("b_improve_index","b_check_MA","b_avg_sales_p","b_gets_q_bonus","b_bargain_power")

###Make anderson index
farmers_end$primary_farmer_index <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_primary_farmer_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes <- c(outcomes, "primary_farmer_index")
b_outcomes <- c(b_outcomes, "b_primary_farmer_index")
###dataset for comparing treatment to interaction directly
farmers_end_int <-  farmers_end[(farmers_end$vid==TRUE & farmers_end$treat==TRUE) | (farmers_end$vid==FALSE & farmers_end$treat==FALSE),]
###fully interacted model
res_farmers <-   array(NA,dim=c(length(outcomes),32))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*vid*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,1] <- mean(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,2] <- sd(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,3:5] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,6:8] <- c(res[3,2],res[3,3],res[3,6])
  res_farmers[i,9:12] <- c(res[6,2],res[6,3],res[6,6], nobs(ols))
  
  res_farmers[i,13] <- linearHypothesis(ols, c("treatTRUE = treatTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,14] <- linearHypothesis(ols, c("vidTRUE = vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,15] <- linearHypothesis(ols, c("treatTRUE:vidTRUE = treatTRUE:vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  ###directly compare control to interaction - we can use treat as indicator as this automatically also has vid
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end_int)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end_int$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,27:29] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,30] <- res[5,6] ###interaction with trader
}

res_farmers[1:length(outcomes)-1,16] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,5])
res_farmers[1:length(outcomes)-1,17] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,8])
res_farmers[1:length(outcomes)-1,18] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,11])


###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)
 
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~"), b_outcomes[i], sep = "+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,19:21] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,31] <- res[6,6] ###interaction with trader demeaned
}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~"), b_outcomes[i], sep = "+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,22:24] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,32] <- res[6,6] ###interaction with trader demeaned  
}

res_farmers[1:length(outcomes)-1,25] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,18])
res_farmers[1:length(outcomes)-1,26] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,21])


res_farmers <- round(res_farmers,digits=3)
res_farmers_prim <- round(res_farmers,digits=3) ## save for graphs later

saveRDS(res_farmers, file= paste(path,"PAP/results/res_farmers.RData", sep="/"))
 
##secondary outcomes - sales
#q52 sold in last week
farmers_end$q52[farmers_end$q52 == "n/a"] <- NA
farmers_end$sold_last_week <- farmers_end$q52 == "Yes"
farmers_base$b_sold_last_week <- farmers_base$q52 == "Yes"
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_sold_last_week")], by="farmer_ID", all.x=T)


columns <- c("q50","q50x") ## vols sold dry and rainy season

# Replace "n/a" and "999" with NA in each specified column
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

farmers_end$q_sold_dry  <- farmers_end$q50 
farmers_base$b_q_sold_dry  <- farmers_base$q50
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_q_sold_dry")], by="farmer_ID", all.x=T)

farmers_end$q_sold_wet  <- farmers_end$q50x 
farmers_base$b_q_sold_wet  <- farmers_base$q50x
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_q_sold_wet")], by="farmer_ID", all.x=T)


columns <- c("q54","qx1","qx13","qx25","qx37","qx49")
# Replace "n/a" and "999" with NA in each specified column
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

farmers_end$avg_sales_q <- rowMeans(farmers_end[columns], na.rm=T)
farmers_end$avg_sales_q[is.nan(farmers_end$avg_sales_q)] <- NA
farmers_end$avg_sales_q[is.na(farmers_end$avg_sales_q)] <- 0
farmers_end$avg_sales_q[is.na(farmers_end$q52)] <- NA

farmers_base$b_avg_sales_q <- rowMeans(farmers_base[columns], na.rm=T)
farmers_base$b_avg_sales_q[is.nan(farmers_base$b_avg_sales_q)] <- NA
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_avg_sales_q")], by="farmer_ID", all.x=T)

farmers_end$q_sold_dry[farmers_end$q_sold_dry > 500] <- NA
farmers_end$q_sold_wet[farmers_end$q_sold_wet > 500] <- NA
farmers_end$avg_sales_q[farmers_end$avg_sales_q > 500] <- NA

farmers_end$b_q_sold_dry[farmers_end$b_q_sold_dry > 500] <- NA
farmers_end$b_q_sold_wet[farmers_end$b_q_sold_wet > 500] <- NA
farmers_end$b_avg_sales_q[farmers_end$b_avg_sales_q > 500] <- NA

farmers_end$ihs_q_sold_dry <- ihs(farmers_end$q_sold_dry)
farmers_end$ihs_q_sold_wet <- ihs(farmers_end$q_sold_wet)
farmers_end$ihs_avg_sales_q <- ihs(farmers_end$avg_sales_q)


farmers_end$ihs_b_q_sold_dry <- ihs(farmers_end$b_q_sold_dry)
farmers_end$ihs_b_q_sold_wet <- ihs(farmers_end$b_q_sold_wet)
farmers_end$ihs_b_avg_sales_q <- ihs(farmers_end$b_avg_sales_q)

outcomes <- c("ihs_q_sold_dry", "ihs_q_sold_wet", "sold_last_week", "ihs_avg_sales_q")
b_outcomes <- c("ihs_b_q_sold_dry", "ihs_b_q_sold_wet", "b_sold_last_week", "ihs_b_avg_sales_q")

###Make anderson index
farmers_end$secondary_farmer_quant_index  <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_secondary_farmer_quant_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes <- c(outcomes, "secondary_farmer_quant_index")
b_outcomes <- c(b_outcomes, "b_secondary_farmer_quant_index")

##Sold to milk to collection center in the week preceding the survey
farmers_end_int <-  farmers_end[(farmers_end$vid==TRUE & farmers_end$treat==TRUE) | (farmers_end$vid==FALSE & farmers_end$treat==FALSE),]

###fully interacted model
res_farmers <-   array(NA,dim=c(length(outcomes),32))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*vid*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,1] <- mean(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,2] <- sd(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,3:5] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,6:8] <- c(res[3,2],res[3,3],res[3,6])
  res_farmers[i,9:12] <- c(res[6,2],res[6,3],res[6,6], nobs(ols))
  
  res_farmers[i,13] <- linearHypothesis(ols, c("treatTRUE = treatTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,14] <- linearHypothesis(ols, c("vidTRUE = vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,15] <- linearHypothesis(ols, c("treatTRUE:vidTRUE = treatTRUE:vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  ###directly compare control to interaction - we can use treat as indicator as this automatically also has vid
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end_int)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end_int$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,27:29] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,30] <- res[5,6] ###interaction with trader
}

res_farmers[1:length(outcomes)-1,16] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,5])
res_farmers[1:length(outcomes)-1,17] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,8])
res_farmers[1:length(outcomes)-1,18] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,11])


###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)

for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,19:21] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,31] <- res[6,6] ###interaction with trader demeaned
}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,22:24] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,32] <- res[6,6] ###interaction with trader demeaned  
}
res_farmers[1:length(outcomes)-1,25] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,18])
res_farmers[1:length(outcomes)-1,26] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,21])


res_farmers_sec_quant <- round(res_farmers,digits=3)

saveRDS(res_farmers_sec_quant, file= paste(path,"PAP/results/res_farmers_sec_quant.RData", sep="/"))

##secondary outcomes - treatment uptake
### there is no baseline data for these

###recalls video
###recalls grass
###usedgrass
### knows compositional quality is important: test_know_1, test_know_2, test_know_3


farmers_end$recalls_video <- farmers_end$recalls_video == "Yes"
farmers_end$recalls_grass <- farmers_end$recalls_grass == "Yes"
farmers_end$used_grass <- farmers_end$used_grass == "Yes"
farmers_end$used_video <- farmers_end$used_video == "Yes"

farmers_end$knows_comp <- farmers_end$test_know_1 == 1 & farmers_end$test_know_2 == 2  & farmers_end$test_know_3 == 1


outcomes <- c("recalls_video", "recalls_grass", "used_grass", "knows_comp")

###Make anderson index
farmers_end$secondary_farmer_quant_index  <- anderson_index(farmers_end[outcomes])$index

outcomes <- c(outcomes, "secondary_farmer_quant_index")
farmers_end_int <-  farmers_end[(farmers_end$vid==TRUE & farmers_end$treat==TRUE) | (farmers_end$vid==FALSE & farmers_end$treat==FALSE),]

##Sold to milk to collection center in the week preceding the survey
###fully interacted model
res_farmers <-   array(NA,dim=c(length(outcomes),32))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i],"treat*vid*trader",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,1] <- mean(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,2] <- sd(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,3:5] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,6:8] <- c(res[3,2],res[3,3],res[3,6])
  res_farmers[i,9:12] <- c(res[6,2],res[6,3],res[6,6], nobs(ols))
  
  res_farmers[i,13] <- linearHypothesis(ols, c("treatTRUE = treatTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,14] <- linearHypothesis(ols, c("vidTRUE = vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,15] <- linearHypothesis(ols, c("treatTRUE:vidTRUE = treatTRUE:vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  ###directly compare control to interaction - we can use treat as indicator as this automatically also has vid
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end_int)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end_int$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,27:29] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,30] <- res[5,6] ###interaction with trader
}

res_farmers[1:length(outcomes)-1,16] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,5])
res_farmers[1:length(outcomes)-1,17] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,8])
res_farmers[1:length(outcomes)-1,18] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,11])


###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)

for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,19:21] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,31] <- res[6,6] ###interaction with trader demeaned
}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,22:24] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,32] <- res[6,6] ###interaction with trader demeaned  
}
res_farmers[1:length(outcomes)-1,25] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,18])
res_farmers[1:length(outcomes)-1,26] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,21])


res_farmers_sec_uptake <- round(res_farmers,digits=3)

saveRDS(res_farmers_sec_uptake, file= paste(path,"PAP/results/res_farmers_sec_uptake.RData", sep="/"))

###secondary sales
farmers_end$mcc_wet <- farmers_end$q51=="2"
farmers_base$b_mcc_wet <- farmers_base$q51=="2"
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_mcc_wet")], by="farmer_ID", all.x=T)


farmers_end$mcc_dry <- farmers_end$q51x=="2"
farmers_base$b_mcc_dry <- farmers_base$q51x=="2"
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_mcc_dry")], by="farmer_ID", all.x=T)

farmers_end$mcc_last_seek <- farmers_end$q53.2 == "True"
farmers_base$b_mcc_last_seek <-farmers_base$q53.2 == "True"
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_mcc_last_seek")], by="farmer_ID", all.x=T)

farmers_end$price_wet <- as.numeric(as.character(farmers_end$q51a))
farmers_end$price_wet[farmers_end$price_wet == 999] <- NA
farmers_base$b_price_wet <- as.numeric(as.character(farmers_base$q51a))
farmers_base$b_price_wet[farmers_base$b_price_wet == 999 ] <- NA
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_price_wet")], by="farmer_ID", all.x=T)

farmers_end$price_dry <- as.numeric(as.character(farmers_end$q51ax))
farmers_end$price_dry[farmers_end$price_dry == 999] <- NA
farmers_base$b_price_dry <- as.numeric(as.character(farmers_base$q51ax))
farmers_base$b_price_dry[farmers_base$b_price_dry == 999] <- NA
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_price_dry")], by="farmer_ID", all.x=T)

outcomes <- c("mcc_wet", "mcc_dry", "mcc_last_seek", "price_wet", "price_dry")
b_outcomes <- c("b_mcc_wet", "b_mcc_dry", "b_mcc_last_seek", "b_price_wet", "b_price_dry")

###Make anderson index
farmers_end$secondary_farmer_quant_index  <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_secondary_farmer_quant_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes <- c(outcomes, "secondary_farmer_quant_index")
b_outcomes <- c(b_outcomes, "b_secondary_farmer_quant_index")
farmers_end_int <-  farmers_end[(farmers_end$vid==TRUE & farmers_end$treat==TRUE) | (farmers_end$vid==FALSE & farmers_end$treat==FALSE),]

##Sold to milk to collection center in the week preceding the survey
###fully interacted model
res_farmers <-   array(NA,dim=c(length(outcomes),32))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*vid*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,1] <- mean(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,2] <- sd(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,3:5] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,6:8] <- c(res[3,2],res[3,3],res[3,6])
  res_farmers[i,9:12] <- c(res[6,2],res[6,3],res[6,6], nobs(ols))
  
  res_farmers[i,13] <- linearHypothesis(ols, c("treatTRUE = treatTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,14] <- linearHypothesis(ols, c("vidTRUE = vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,15] <- linearHypothesis(ols, c("treatTRUE:vidTRUE = treatTRUE:vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  ###directly compare control to interaction - we can use treat as indicator as this automatically also has vid
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end_int)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end_int$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,27:29] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,30] <- res[5,6] ###interaction with trader
}

res_farmers[1:length(outcomes)-1,16] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,5])
res_farmers[1:length(outcomes)-1,17] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,8])
res_farmers[1:length(outcomes)-1,18] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,11])


###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)

for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,19:21] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,31] <- res[6,6] ###interaction with trader demeaned
}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,22:24] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,32] <- res[6,6] ###interaction with trader demeaned  
}

res_farmers[1:length(outcomes)-1,25] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,18])
res_farmers[1:length(outcomes)-1,26] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,21])


res_farmers_sec_sold <- round(res_farmers,digits=3)
saveRDS(res_farmers_sec_sold, file= paste(path,"PAP/results/res_farmers_sec_sold.RData", sep="/"))

###switching - no control for baseline outcomes

### still supplying to the MCC farmer was connected to during baseline
farmers_end$still_connected_yes <- farmers_end$still_connected==1
farmers_end$still_connected_yes[farmers_end$still_connected=="n/a"] <- NA

###still supplying wet season
farmers_end$q51_name[is.na(farmers_end$q51_name)] <- "NA"
farmers_end$still_supplying_wet <- as.character(farmers_end$q51_name_prev) ==  as.character(farmers_end$q51_name)

farmers_end$q51_namex[is.na(farmers_end$q51_namex)] <- "NA"
farmers_end$still_supplying_dry <- as.character(farmers_end$q51_name_prevx) ==  as.character(farmers_end$q51_namex)
                                                                                             
outcomes <- c("still_connected_yes","still_supplying_wet","still_supplying_dry")

###Make anderson index
farmers_end$secondary_farmer_switch_index  <- anderson_index(farmers_end[outcomes])$index

outcomes <- c(outcomes, "secondary_farmer_switch_index")
farmers_end_int <-  farmers_end[(farmers_end$vid==TRUE & farmers_end$treat==TRUE) | (farmers_end$vid==FALSE & farmers_end$treat==FALSE),]

##Sold to milk to collection center in the week preceding the survey
###fully interacted model
res_farmers <-   array(NA,dim=c(length(outcomes),32))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i],"treat*vid*trader",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,1] <- mean(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,2] <- sd(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
  res_farmers[i,3:5] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,6:8] <- c(res[3,2],res[3,3],res[3,6])
  res_farmers[i,9:12] <- c(res[5,2],res[5,3],res[5,6], nobs(ols))
  
  res_farmers[i,13] <- linearHypothesis(ols, c("treatTRUE = treatTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,14] <- linearHypothesis(ols, c("vidTRUE = vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  res_farmers[i,15] <- linearHypothesis(ols, c("treatTRUE:vidTRUE = treatTRUE:vidTRUE:traderTRUE") , vcov. = vcov_cluster)[[4]][2]
  ###directly compare control to interaction - we can use treat as indicator as this automatically also has vid
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end_int)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end_int$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,27:29] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,30] <- res[5,6] ###interaction with trader
}

res_farmers[1:length(outcomes)-1,16] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,5])
res_farmers[1:length(outcomes)-1,17] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,8])
res_farmers[1:length(outcomes)-1,18] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,11])


###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)

for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,19:21] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,31] <- res[6,6] ###interaction with trader demeaned
}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,22:24] <- c(res[2,2],res[2,3],res[2,6])
  res_farmers[i,32] <- res[6,6] ###interaction with trader demeaned  
}
res_farmers[1:length(outcomes)-1,25] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,18])
res_farmers[1:length(outcomes)-1,26] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,21])


res_farmers_sec_switching <- round(res_farmers,digits=3)
saveRDS(res_farmers_sec_switching, file= paste(path,"PAP/results/res_farmers_sec_switching.RData", sep="/"))

####### some graphs for presentattions
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert the res_farmers matrix to a data frame
res_farmers_df <- as.data.frame(res_farmers_sec_switching[1:3,])
colnames(res_farmers_df) <- c(
  "Mean", "SD", 
  "Coef1", "SE1", "Pval1", 
  "Coef2", "SE2", "Pval2", 
  "Coef3", "SE3", "Pval3", "N", 
  "LinearHyp1", "LinearHyp2", "LinearHyp3",
  "ASQ5", "ASQ8", "ASQ11", 
  "Coef_Demeaned1", "SE_Demeaned1", "Pval_Demeaned1", 
  "Coef_Demeaned2", "SE_Demeaned2", "Pval_Demeaned2", 
  "ASQ18", "ASQ21","Coef4", "SE4", "Pval4", "LinearHyp4", "LinearHyp1_demeaned", "LinearHyp2_demeaned"
)

# Add outcome labels (assuming outcomes vector is available)
res_farmers_df$Outcome <- outcomes[1:3]

# Reshape data to include all coefficients and linear hypothesis p-values
forest_data_long <- res_farmers_df %>%
  select(
    Outcome,
    Coef1, SE1, LinearHyp1,
    Coef2, SE2, LinearHyp2,
    Coef4, SE4, LinearHyp4
  ) %>%
  pivot_longer(
    cols = starts_with("Coef"),
    names_to = "Coefficient",
    values_to = "Estimate"
  ) %>%
  mutate(
    SE = case_when(
      Coefficient == "Coef1" ~ SE1,
      Coefficient == "Coef2" ~ SE2,
      Coefficient == "Coef4" ~ SE4
    ),
    LinearHypothesis = case_when(
      Coefficient == "Coef1" ~ LinearHyp1,
      Coefficient == "Coef2" ~ LinearHyp2,
      Coefficient == "Coef4" ~ LinearHyp4,
      TRUE ~ NA_real_ # No LinearHypothesis values for demeaned models
    ),
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE,
    Coefficient = case_when(
      Coefficient == "Coef1" ~ "milk analyzer",
      Coefficient == "Coef2" ~ "video",
      Coefficient == "Coef4" ~ "bundle"
    )
  )


# Remove rows with NA in Coefficient column
forest_data_long <- forest_data_long %>%
  filter(!is.na(Coefficient))

# Set the order of the coefficients for the legend (placing "interaction" last)
forest_data_long$Coefficient <- factor(forest_data_long$Coefficient, levels = c("milk analyzer", "video", "bundle"))

# Define custom colors for grouping (without pooled models)
custom_colors <- c(
  "milk analyzer" = "#1f77b4",  # Blue
  "video" = "#ff7f0e",          # Orange
  "bundle" = "#2ca02c"     # Green
)

# Create the forest plot
forest_plot <- ggplot(forest_data_long, aes(x = Estimate, y = reorder(Outcome, Estimate), color = Coefficient)) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) + # Points for coefficients
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, position = position_dodge(width = 0.7)) + # CI lines
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + # No-effect line
  geom_text(
    aes(label = ifelse(!is.na(LinearHypothesis), paste0("p = ", round(LinearHypothesis, 3)), "")),
    position = position_dodge(width = 0.7),
    vjust = -1, size = 3.5,
    show.legend = FALSE # Suppress legend for text layer
  ) + 
  scale_color_manual(values = custom_colors) + # Apply custom colors
  labs(
    title = "Farmer Switching MCCs",
    x = "Coefficient (95% CI)",
    y = "Outcomes",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),    # Increase legend title size if needed
    legend.text = element_text(size = 10)      # Increase legend item text size if needed
  ) +
  guides(color = guide_legend(override.aes = list(shape = 16)))  # Ensure color legend uses correct shape

# Print the plot
print(forest_plot)

ggsave( file= paste(path,"PAP/results/forest_plot.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)



#### forest plot for prices - assemble from 
#### forest plot for prices - assemble from 

# Convert the res_farmers matrix to a data frame

res_farmers_df <- as.data.frame(rbind(res_farmers_prim[3,],res_farmers_sec_sold[4:5,]))

# Add outcome labels (assuming outcomes vector is available)

colnames(res_farmers_df) <- c(
  "Mean", "SD", 
  "Coef1", "SE1", "Pval1", 
  "Coef2", "SE2", "Pval2", 
  "Coef3", "SE3", "Pval3", "N", 
  "LinearHyp1", "LinearHyp2", "LinearHyp3",
  "ASQ5", "ASQ8", "ASQ11", 
  "Coef_Demeaned1", "SE_Demeaned1", "Pval_Demeaned1", 
  "Coef_Demeaned2", "SE_Demeaned2", "Pval_Demeaned2", 
  "ASQ18", "ASQ21","Coef4", "SE4", "Pval4", "LinearHyp4", "LinearHyp1_demeaned", "LinearHyp2_demeaned"
)

res_farmers_df$Outcome <- c("price last week","price in wet season","price in dry season")

# Reshape data to include all coefficients and linear hypothesis p-values
forest_data_long <- res_farmers_df %>%
  select(
    Outcome,
    Coef_Demeaned1, SE_Demeaned1, LinearHyp1_demeaned,
    Coef_Demeaned2, SE_Demeaned2, LinearHyp2_demeaned,
    Coef4, SE4, LinearHyp4
  ) %>%
  pivot_longer(
    cols = starts_with("Coef"),
    names_to = "Coefficient",
    values_to = "Estimate"
  ) %>%
  mutate(
    SE = case_when(
      Coefficient == "Coef_Demeaned1" ~ SE_Demeaned1,
      Coefficient == "Coef_Demeaned2" ~ SE_Demeaned2,
      Coefficient == "Coef4" ~ SE4
    ),
    LinearHypothesis = case_when(
      Coefficient == "Coef_Demeaned1" ~ LinearHyp1_demeaned,
      Coefficient == "Coef_Demeaned2" ~ LinearHyp2_demeaned,
      Coefficient == "Coef4" ~ LinearHyp4,
      TRUE ~ NA_real_ # No LinearHypothesis values for demeaned models
    ),
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE,
    Coefficient = case_when(
      Coefficient == "Coef_Demeaned1" ~ "milk analyzer",
      Coefficient == "Coef_Demeaned2" ~ "video",
      Coefficient == "Coef4" ~ "bundle"
    )
  )


# Remove rows with NA in Coefficient column
forest_data_long <- forest_data_long %>%
  filter(!is.na(Coefficient))

# Set the order of the coefficients for the legend (placing "bundle" last)
forest_data_long$Coefficient <- factor(forest_data_long$Coefficient, levels = c("milk analyzer", "video", "bundle"))

# Define custom colors for grouping (without pooled models)
custom_colors <- c(
  "milk analyzer" = "#1f77b4",  # Blue
  "video" = "#ff7f0e",          # Orange
  "bundle" = "#2ca02c"     # Green
)

# Create the forest plot
forest_plot <- ggplot(forest_data_long, aes(x = Estimate, y = reorder(Outcome, Estimate), color = Coefficient)) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) + # Points for coefficients
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, position = position_dodge(width = 0.7)) + # CI lines
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + # No-effect line
  geom_text(
    aes(label = ifelse(!is.na(LinearHypothesis), paste0("p = ", round(LinearHypothesis, 3)), "")),
    position = position_dodge(width = 0.7),
    vjust = -1, size = 3.5,
    show.legend = FALSE # Suppress legend for text layer
  ) + 
  scale_color_manual(values = custom_colors) + # Apply custom colors
  labs(
    title = "Milk Prices received by farmers",
    x = "Coefficient (95% CI)",
    y = "Outcomes",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),    # Increase legend title size if needed
    legend.text = element_text(size = 10)      # Increase legend item text size if needed
  ) +
  guides(color = guide_legend(override.aes = list(shape = 16)))  # Ensure color legend uses correct shape

# Print the plot
print(forest_plot)

ggsave( file= paste(path,"PAP/results/forest_plot_price.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)

### simplest models
########################## bundle use ##############################################
# Outcomes to analyze and their corresponding y-axis labels
farmers_end$any_true <- with(farmers_end, 
                 q39 == "Yes" | 
                   q39c == "Yes" | 
                   q40 %in% c(1, 3) | 
                   q41 %in% c(1, 3) | 
                   q42 == "Yes" | 
                   q43 == "Yes")
outcomes <- c("check_MA",  "knows_comp","any_true")
outcome_labels <- c("milk was checked at MCC", "knows imporance of quality","invested in quality")

# Initialize an empty data frame to store results
results <- data.frame(
  Outcome = character(),
  Treatment = character(),
  Estimate = numeric(),
  StdError = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop through outcomes to run regressions and extract estimates
for (i in 1:length(outcomes)) {
  # Run regressions for each intervention
  ols_MA <- lm(as.formula(paste(outcomes[i], "treat", sep = "~")), data = farmers_end)
  ols_vid <- lm(as.formula(paste(outcomes[i], "vid", sep = "~")), data = farmers_end)
  ols_bundle <- lm(as.formula(paste(outcomes[i], "vid", sep = "~")),
                   data = farmers_end[
                     (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
                       (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ])
  
  # Extract coefficients, standard errors, and confidence intervals
  interventions <- list(MA = ols_MA, Video = ols_vid, Bundle = ols_bundle)
  for (name in names(interventions)) {
    model <- interventions[[name]]
    coef_est <- summary(model)$coefficients[2, 1] # Treatment effect estimate
    std_err <- summary(model)$coefficients[2, 2] # Standard error
    ci <- confint(model)[2, ]                    # Confidence interval
    
    # Append results to the data frame
    results <- rbind(results, data.frame(
      Outcome = outcomes[i],
      Treatment = name,
      Estimate = coef_est,
      StdError = std_err,
      CI_Lower = ci[1],
      CI_Upper = ci[2]
    ))
  }
}


# Reorganize results for a single forest plot with reversed y-axis order
results$Outcome <- factor(
  results$Outcome,
  levels = rev(outcomes), # Reverse the order of outcomes
  labels = rev(outcome_labels) # Reverse the corresponding labels
)
results$Treatment <- factor(results$Treatment, levels = c("MA", "Video", "Bundle")) # Maintain order

# Create the forest plot with reversed y-axis
forest_plot <- ggplot(results, aes(x = Estimate, y = Outcome, color = Treatment)) +
  geom_point(position = position_dodge(width = 0.6), size = 4) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), position = position_dodge(width = 0.6), height = 0.3, size = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.2) + # Bold vertical line at 0
  theme_minimal(base_size = 16) + # Base font size for better readability
  labs(
    title = "Farmer level use",
    x = "Treatment Effect Estimate",
    y = "",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("MA" = "#1b9e77", "Video" = "#d95f02", "Bundle" = "#7570b3")) + # Distinct color palette
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# Print the forest plot
print(forest_plot)
ggsave( file= paste(path,"PAP/results/forest_plot_farmer_use_simple.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)


########################## prices ##############################################
# Outcomes to analyze and their corresponding y-axis labels
outcomes <- c("avg_sales_p", "price_wet", "price_dry")
outcome_labels <- c("price in last week", "price in wet season", "price in dry season")

# Initialize an empty data frame to store results
results <- data.frame(
  Outcome = character(),
  Treatment = character(),
  Estimate = numeric(),
  StdError = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop through outcomes to run regressions and extract estimates
for (i in 1:length(outcomes)) {
  # Run regressions for each intervention
  ols_MA <- lm(as.formula(paste(outcomes[i], "treat", sep = "~")), data = farmers_end)
  ols_vid <- lm(as.formula(paste(outcomes[i], "vid", sep = "~")), data = farmers_end)
  ols_bundle <- lm(as.formula(paste(outcomes[i], "vid", sep = "~")),
                   data = farmers_end[
                     (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
                       (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ])
  
  # Extract coefficients, standard errors, and confidence intervals
  interventions <- list(MA = ols_MA, Video = ols_vid, Bundle = ols_bundle)
  for (name in names(interventions)) {
    model <- interventions[[name]]
    coef_est <- summary(model)$coefficients[2, 1] # Treatment effect estimate
    std_err <- summary(model)$coefficients[2, 2] # Standard error
    ci <- confint(model)[2, ]                    # Confidence interval
    
    # Append results to the data frame
    results <- rbind(results, data.frame(
      Outcome = outcomes[i],
      Treatment = name,
      Estimate = coef_est,
      StdError = std_err,
      CI_Lower = ci[1],
      CI_Upper = ci[2]
    ))
  }
}

# Reorganize results for a single forest plot
results$Outcome <- factor(results$Outcome, levels = outcomes, labels = outcome_labels) # Apply new labels
results$Treatment <- factor(results$Treatment, levels = c("MA", "Video", "Bundle")) # Maintain order

# Create the forest plot
forest_plot <- ggplot(results, aes(x = Estimate, y = Outcome, color = Treatment)) +
  geom_point(position = position_dodge(width = 0.6), size = 4) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), position = position_dodge(width = 0.6), height = 0.3, size = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.2) + # Bold vertical line at 0
  theme_minimal(base_size = 16) + # Base font size for better readability
  labs(
    title = "Farmer level impact on prices",
    x = "Treatment Effect Estimate",
    y = "",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("MA" = "#1b9e77", "Video" = "#d95f02", "Bundle" = "#7570b3")) + # Distinct color palette
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# Print the forest plot
print(forest_plot)
ggsave( file= paste(path,"PAP/results/forest_plot_price_simple.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)
#####switching

outcomes <- c("still_connected_yes","still_supplying_wet","still_supplying_dry")
outcome_labels <- c("still connected to the same MCC", "still supplying to same in wet season", "still supplying to same in dry season")

# Initialize an empty data frame to store results
results <- data.frame(
  Outcome = character(),
  Treatment = character(),
  Estimate = numeric(),
  StdError = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop through outcomes to run regressions and extract estimates
for (i in 1:length(outcomes)) {
  # Run regressions for each intervention
  ols_MA <- lm(as.formula(paste(outcomes[i], "treat", sep = "~")), data = farmers_end)
  ols_vid <- lm(as.formula(paste(outcomes[i], "vid", sep = "~")), data = farmers_end)
  ols_bundle <- lm(as.formula(paste(outcomes[i], "vid", sep = "~")),
                   data = farmers_end[
                     (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
                       (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ])
  
  # Extract coefficients, standard errors, and confidence intervals
  interventions <- list(MA = ols_MA, Video = ols_vid, Bundle = ols_bundle)
  for (name in names(interventions)) {
    model <- interventions[[name]]
    coef_est <- summary(model)$coefficients[2, 1] # Treatment effect estimate
    std_err <- summary(model)$coefficients[2, 2] # Standard error
    ci <- confint(model)[2, ]                    # Confidence interval
    
    # Append results to the data frame
    results <- rbind(results, data.frame(
      Outcome = outcomes[i],
      Treatment = name,
      Estimate = coef_est,
      StdError = std_err,
      CI_Lower = ci[1],
      CI_Upper = ci[2]
    ))
  }
}

# Reorganize results for a single forest plot
results$Outcome <- factor(results$Outcome, levels = outcomes, labels = outcome_labels) # Apply new labels
results$Treatment <- factor(results$Treatment, levels = c("MA", "Video", "Bundle")) # Maintain order

# Create the forest plot
forest_plot <- ggplot(results, aes(x = Estimate, y = Outcome, color = Treatment)) +
  geom_point(position = position_dodge(width = 0.6), size = 4) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), position = position_dodge(width = 0.6), height = 0.3, size = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.2) + # Bold vertical line at 0
  theme_minimal(base_size = 16) + # Base font size for better readability
  labs(
    title = "Impact on switching",
    x = "Treatment Effect Estimate",
    y = "",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("MA" = "#1b9e77", "Video" = "#d95f02", "Bundle" = "#7570b3")) + # Distinct color palette
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# Print the forest plot
print(forest_plot)
ggsave( file= paste(path,"PAP/results/forest_plot_swithing_simple.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)



######################## analysis at the MCC level ############################################
### read in clean endline MCC data
MCCs_end <- read.csv(paste(path, "endline/data/public/MCCs.csv", sep = "/"))
### drop duplicates - this was for dummy data but this will not do anything with eventual clean endline data
library(dplyr)
MCCs_end <- MCCs_end %>%
  distinct(MCC_ID, .keep_all = TRUE)
### drop empty (non-consent)
MCCs_end <- subset(MCCs_end,consent == 1)

### read in clean baseline farmer data 
MCCs_base <- read.csv(paste(path, "baseline/data/public/MCCs.csv", sep = "/"))

### testing of quality on incoming samples
MCCs_end$test_MA_in <-  MCCs_end$q25x3 == 1 | MCCs_end$q25x3 == 2
MCCs_end$test_MA_in[MCCs_end$q25x3 == "n/a"] <- NA

MCCs_base$b_test_MA_in <-  MCCs_base$q25x3 == 1 | MCCs_base$q25x3 == 2
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_test_MA_in")], by="MCC_ID", all.x=T)

### testing of quality on outgoing milk
MCCs_end$test_MA_out <- (MCCs_end$q39a == 2 | MCCs_end$q39c == 2) |
  (MCCs_end$q52a == 2 | MCCs_end$q52c == 2) |
  (MCCs_end$q62a == 2 | MCCs_end$q62c == 2) |
  (MCCs_end$q72a == 2 | MCCs_end$q72c == 2) |
  (MCCs_end$q82a == 2 | MCCs_end$q82c == 2)

 
MCCs_base$b_test_MA_out <- (MCCs_base$q39a %in% c(1, 3) | MCCs_base$q39c %in% c(1, 3)) |
  (MCCs_base$q52a %in% c(1, 3) | MCCs_base$q52c %in% c(1, 3)) |
  (MCCs_base$q62a %in% c(1, 3) | MCCs_base$q62c %in% c(1, 3)) |
  (MCCs_base$q72a %in% c(1, 3) | MCCs_base$q72c %in% c(1, 3)) |
  (MCCs_base$q82a %in% c(1, 3) | MCCs_base$q82c %in% c(1, 3))
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_test_MA_out")], by="MCC_ID", all.x=T)

#average prices at which milk was bought from farmers (during last 7 days)

MCCs_end$q25b[MCCs_end$q25b %in% c("n/a","999") ] <- NA
MCCs_end$q25b <- as.numeric(as.character(MCCs_end$q25b))

names(MCCs_end)[names(MCCs_end) == 'q25b'] <- 'price_bought'

#merge in baseline outcome
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","q25b")], by="MCC_ID", all.x=T)

names(MCCs_end)[names(MCCs_end) == 'q25b'] <- 'b_price_bought'


#Price at which milk was sold (in last 7 days) - q36/q49/q59/q69/q79
columns <- c("q36", "q49", "q59", "q69", "q79")

# Replace "n/a" and "999" with NA in each specified column
MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  x[x>10000] <- NA
  return(x)
})

MCCs_end$avg_sales_p <- rowMeans(MCCs_end[columns], na.rm=T)
MCCs_end$avg_sales_p[is.nan(MCCs_end$avg_sales_p)] <- NA


MCCs_base$b_avg_sales_p <- rowMeans(MCCs_base[columns], na.rm=T)
MCCs_base$b_avg_sales_p[is.nan(MCCs_base$b_avg_sales_p)] <- NA
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_avg_sales_p")], by="MCC_ID", all.x=T)

### quality paid to bonus to farmers
MCCs_end$q29[MCCs_end$q29 == "n/a"] <- NA
MCCs_end$gives_q_bonus <- MCCs_end$q29 == 1

MCCs_base$b_gives_q_bonus <- MCCs_base$q29 == 1
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_gives_q_bonus")], by="MCC_ID", all.x=T)

### if all are "n/a" this is missing
MCCs_end$gets_q_bonus <- ifelse(
  rowSums(MCCs_end[, c("q44", "q54", "q64", "q74", "q84")] == "n/a") == 5, 
  NA, 
  MCCs_end$q44 == 1 | MCCs_end$q54 == 1 | MCCs_end$q64 == 1 | MCCs_end$q74 == 1 | MCCs_end$q84 == 1
)

MCCs_base$b_gets_q_bonus <- ifelse(
  rowSums(MCCs_base[, c("q44", "q54", "q64", "q74", "q84")] == "n/a") == ncol(MCCs_base[, c("q44", "q54", "q64", "q74", "q84")]), 
  NA, 
  MCCs_base$q44 == 1 | MCCs_base$q54 == 1 | MCCs_base$q64 == 1 | MCCs_base$q74 == 1 | MCCs_base$q84 == 1
)

MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_gets_q_bonus")], by="MCC_ID", all.x=T)

###iterate over outcomes in this family
outcomes <- c("test_MA_in","test_MA_out","price_bought","avg_sales_p","gives_q_bonus","gets_q_bonus")
b_outcomes <- c("b_test_MA_in","b_test_MA_out","b_price_bought","b_avg_sales_p","b_gives_q_bonus","b_gets_q_bonus")

###Make anderson index
MCCs_end$primary_MCC_index <- anderson_index(MCCs_end[outcomes])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes])$index

outcomes <- c(outcomes, "primary_MCC_index")
b_outcomes <- c(b_outcomes, "b_primary_MCC_index")


res_MCCs <-   array(NA,dim=c(length(outcomes),7))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat",sep="~"),b_outcomes[i],sep="+")), data=MCCs_end)
  res_MCCs[i,1] <- mean(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]], na.rm=T)
  res_MCCs[i,2] <- sd(as.matrix(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]]), na.rm=T)
  res_MCCs[i,3:5] <- summary(ols)$coefficients[2,c(1:2,4)]
  res_MCCs[i,7] <- nobs(ols)
}

res_MCCs <- round(res_MCCs,digits=3)
res_MCCs[1:length(outcomes)-1,6] <- anderson_sharp_q(res_MCCs[1:length(outcomes)-1,5])

## collects results: ctrl mean, ctrl sd, effect, sd effect, p-val, q-val, nobs
## last line is index

saveRDS(res_MCCs, file= paste(path,"PAP/results/res_MCCs.RData", sep="/"))

###secondary outcomes at MCC level

### volumes
#4. farmers may drop out: Number of farmers that supply on an average day (rainy vs dry season) q23-q24 and in the last week - q25a

MCCs_end$nr_farmers_wet <- as.numeric(MCCs_end$Q23)
MCCs_base$b_nr_farmers_wet <- as.numeric(MCCs_base$Q23)
MCCs_base$b_nr_farmers_wet[MCCs_base$b_nr_farmers_wet>=1000] <- NA
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_nr_farmers_wet")], by="MCC_ID", all.x=T)

MCCs_end$nr_farmers_dry <- as.numeric(MCCs_end$Q24)
MCCs_base$b_nr_farmers_dry <- as.numeric(MCCs_base$Q24)
MCCs_base$b_nr_farmers_dry[MCCs_base$b_nr_farmers_dry>=1000] <- NA
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_nr_farmers_dry")], by="MCC_ID", all.x=T)

MCCs_end$nr_farmers_last_week <- as.numeric(MCCs_end$q25a)
MCCs_base$b_nr_farmers_last_week <- as.numeric(MCCs_base$q25a)
MCCs_base$b_nr_farmers_last_week[MCCs_base$b_nr_farmers_last_week >= 1000 ] <- NA
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_nr_farmers_last_week")], by="MCC_ID", all.x=T)


## volumes collected during dry and rainy season q27-q28
#Volume of milk collected on an average day in last 7 days - note that midline qualitative survey suggests this effect may be negative - q25c
MCCs_end$vol_dry <- as.numeric(MCCs_end$q27)
MCCs_base$b_vol_dry <- as.numeric(MCCs_base$qual.q27)
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_vol_dry")], by="MCC_ID", all.x=T)

MCCs_end$vol_wet <- as.numeric(MCCs_end$q28)
MCCs_base$b_vol_wet <- as.numeric(MCCs_base$qual.q28)
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_vol_wet")], by="MCC_ID", all.x=T)

MCCs_end$vol_last_week <- as.numeric(MCCs_end$q25c)
MCCs_base$b_vol_last_week <- as.numeric(MCCs_base$q25c)
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_vol_last_week")], by="MCC_ID", all.x=T)


#2. local sales - previous research found that milk collection centers are also important for local milk supply, often doubling as milk shops. Does the intervention crowd out the local market? - q32 == 4
###iterate over outcomes in this family
outcomes <- c("nr_farmers_wet","nr_farmers_dry","nr_farmers_last_week","vol_dry","vol_wet","vol_last_week")
b_outcomes <- c("b_nr_farmers_wet","b_nr_farmers_dry","b_nr_farmers_last_week","b_vol_dry","b_vol_wet","b_vol_last_week")

###Make anderson index
MCCs_end$primary_MCC_index <- anderson_index(MCCs_end[outcomes])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes])$index

outcomes <- c(outcomes, "primary_MCC_index")
b_outcomes <- c(b_outcomes, "b_primary_MCC_index")


res_MCCs <-   array(NA,dim=c(length(outcomes),7))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat",sep="~"),b_outcomes[i],sep="+")), data=MCCs_end)
  res_MCCs[i,1] <- mean(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]], na.rm=T)
  res_MCCs[i,2] <- sd(as.matrix(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]]), na.rm=T)
  res_MCCs[i,3:5] <- summary(ols)$coefficients[2,c(1:2,4)]
  res_MCCs[i,7] <- nobs(ols)
}

res_MCCs <- round(res_MCCs,digits=3)
res_MCCs[1:length(outcomes)-1,6] <- anderson_sharp_q(res_MCCs[1:length(outcomes)-1,5])

res_MCCs_sec_quant <- round(res_MCCs,digits=3)

saveRDS(res_MCCs_sec_quant, file= paste(path,"PAP/results/res_MCCs_sec_quant.RData", sep="/"))

#secondary outcomes - uptake at MCC level

MCCs_end$poster <- MCCs_end$poster == 1
MCCs_end$machine <- MCCs_end$machine ==1
MCCs_end$machine_project <- MCCs_end$machine_project == 1
MCCs_end$machine_in_use <- MCCs_end$machine_in_use == 1 | MCCs_end$machine_in_use == 2
MCCs_end$test_samples <- MCCs_end$q16c == "1"
MCCs_end$uses_app <- MCCs_end$record_keeping.4 == "True" | MCCs_end$record_keeping.5 == "True"


# Enumerator: Do you see the poster advertizing the milk analyzer?
# Enumerator: Do you see a milk analyzer?
# Enumerator: Is this the machine that was provided through the project? Make ESSAE.
# Enumerator: ask the manager to demonstrate the use of the milk analyzer on the fly and indicate what best maches what transpired
# How do you keep track of the milk delivered by farmers?
  
outcomes <- c("poster","machine","machine_project","machine_in_use","test_samples","uses_app")

###Make anderson index
MCCs_end$secondary_MCC_index <- anderson_index(MCCs_end[outcomes])$index

outcomes <- c(outcomes, "secondary_MCC_index")



res_MCCs <-   array(NA,dim=c(length(outcomes),7))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i],"treat",sep="~")), data=MCCs_end)
  res_MCCs[i,1] <- mean(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]], na.rm=T)
  res_MCCs[i,2] <- sd(as.matrix(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]]), na.rm=T)
  res_MCCs[i,3:5] <- summary(ols)$coefficients[2,c(1:2,4)]
  res_MCCs[i,7] <- nobs(ols)
}

res_MCCs <- round(res_MCCs,digits=3)
res_MCCs[1:length(outcomes)-1,6] <- anderson_sharp_q(res_MCCs[1:length(outcomes)-1,5])

res_MCCs_sec_uptake <- round(res_MCCs,digits=3)

saveRDS(res_MCCs_sec_uptake, file= paste(path,"PAP/results/res_MCCs_sec_uptake.RData", sep="/"))
##secondary: MCC sales

#2. volumes sold - q35, q48, q58, q68, q78

#Price at which milk was sold (in last 7 days) - q36/q49/q59/q69/q79
columns <- c("q35", "q48", "q58", "q68", "q78")

# Replace "n/a" and "999" with NA in each specified column
MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

MCCs_end$tot_sales_q <- rowSums(MCCs_end[columns], na.rm=T)

MCCs_base$b_tot_sales_q <- rowSums(MCCs_base[columns], na.rm=T)
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_tot_sales_q")], by="MCC_ID", all.x=T)


#measured SNF/butter fat using MA

MCCs_end$test_MA <- (MCCs_end$q39a == 2 | MCCs_end$q39c == 2) |
(MCCs_end$q52a == 2 | MCCs_end$q52c == 2) |
(MCCs_end$q62a == 2 | MCCs_end$q62c == 2) |
(MCCs_end$q72a == 2 | MCCs_end$q72c == 2) |
(MCCs_end$q82a == 2 | MCCs_end$q82c == 2)

MCCs_base$b_test_MA <- (MCCs_base$q39a == 2 | MCCs_base$q39c == 2) |
  (MCCs_base$q52a == 2 | MCCs_base$q52c == 2) |
  (MCCs_base$q62a == 2 | MCCs_base$q62c == 2) |
  (MCCs_base$q72a == 2 | MCCs_base$q72c == 2) |
  (MCCs_base$q82a == 2 | MCCs_base$q82c == 2)

MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_test_MA")], by="MCC_ID", all.x=T)


#4. Who decided on the price? 1. buyer made offer and MCC accepted, 
#2. MCC made offer and buyer accepted, 3. negotiation - q40/q53q63/q73/q83 == 2

MCCs_end$MCC_decides <- MCCs_end$q40 == "2" | MCCs_end$q53 == "2" | MCCs_end$q63 == "2" | MCCs_end$q73 == "2" | MCCs_end$q83 == "2"
MCCs_base$b_MCC_decides <- MCCs_base$q40 == "2" | MCCs_base$q53 == "2" | MCCs_base$q63 == "2" | MCCs_base$q73 == "2" | MCCs_base$q83 == "2"
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_MCC_decides")], by="MCC_ID", all.x=T)


#Did the buyer pay a quality premium? q44/q54/q64/q74/q84 == 1

MCCs_end$MCC_got_premium <- MCCs_end$q44 == "1" | MCCs_end$q54 == "1" | MCCs_end$q64 == "1" | MCCs_end$q74 == "1" | MCCs_end$q84 == "1"
MCCs_base$b_MCC_got_premium <- MCCs_base$q44 == "1" | MCCs_base$q54 == "1" | MCCs_base$q64 == "1" | MCCs_base$q74 == "1" | MCCs_base$q84 == "1"
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_MCC_got_premium")], by="MCC_ID", all.x=T)

#(b) How much was the quality premium (UGX per liter - average)? - q46/q56/q66/q76/q86

columns <- c("q46", "q56", "q66", "q76", "q86")

# Replace "n/a" and "999" with NA in each specified column
MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  x <- as.numeric(as.character(x))
  return(x)
})

MCCs_end$avg_prem_received <- rowMeans(MCCs_end[columns], na.rm=T)
MCCs_end$avg_prem_received[is.nan(MCCs_end$avg_prem_received)] <- NA

MCCs_base$b_avg_prem_received <- rowMeans(MCCs_base[columns], na.rm=T)
MCCs_base$b_avg_prem_received[is.nan(MCCs_base$b_avg_prem_received)] <- NA
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_avg_prem_received")], by="MCC_ID", all.x=T)


###iterate over outcomes in this family
outcomes <- c("tot_sales_q","test_MA","MCC_decides","MCC_got_premium","avg_prem_received")
b_outcomes <- c("b_tot_sales_q","b_test_MA","b_MCC_decides","b_MCC_got_premium","b_avg_prem_received")

###Make anderson index
MCCs_end$primary_MCC_index <- anderson_index(MCCs_end[outcomes])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes])$index

outcomes <- c(outcomes, "primary_MCC_index")
b_outcomes <- c(b_outcomes, "b_primary_MCC_index")


res_MCCs <-   array(NA,dim=c(length(outcomes),7))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat",sep="~"),b_outcomes[i],sep="+")), data=MCCs_end)
  res_MCCs[i,1] <- mean(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]], na.rm=T)
  res_MCCs[i,2] <- sd(as.matrix(MCCs_end[MCCs_end[c(outcomes[i],"treat")]$treat =="C", outcomes[i]]), na.rm=T)
  res_MCCs[i,3:5] <- summary(ols)$coefficients[2,c(1:2,4)]
  res_MCCs[i,7] <- nobs(ols)
}

res_MCCs <- round(res_MCCs,digits=3)
res_MCCs[1:length(outcomes)-1,6] <- anderson_sharp_q(res_MCCs[1:length(outcomes)-1,5])

res_MCCs_sec_sales <- round(res_MCCs,digits=3)

saveRDS(res_MCCs_sec_sales, file= paste(path,"PAP/results/res_MCCs_sec_sales.RData", sep="/"))


##sold to top 5 processors
MCCs_end$top_proc <- MCCs_end$q32.2=="True" & MCCs_end$q33.6!="True"
#merge in baseline outcome
MCCs_base$b_top_proc <- MCCs_base$q32.2=="True" & MCCs_base$q33.6!="True"
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_top_proc")], by="MCC_ID", all.x=T)

#### generate simple forest plots for presentations
####use
# Define outcomes and their corresponding labels
outcomes <- c("test_MA_in", "test_MA_out", "uses_app")
outcome_labels <- c("used MA to test incoming samples", 
                    "used MA to test outgoing samples", 
                    "digital quality tracking")

# Initialize an empty data frame to store results
results <- data.frame(
  Outcome = character(),
  Estimate = numeric(),
  StdError = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop through outcomes to run regressions and extract estimates
for (i in 1:length(outcomes)) {
  # Run regression for the single treatment
  ols_MA <- lm(as.formula(paste(outcomes[i], "treat", sep = "~")), data = MCCs_end)
  
  # Extract coefficients, standard errors, and confidence intervals
  coef_est <- summary(ols_MA)$coefficients[2, 1] # Treatment effect estimate
  std_err <- summary(ols_MA)$coefficients[2, 2] # Standard error
  ci <- confint(ols_MA)[2, ]                    # Confidence interval
  
  # Append results to the data frame
  results <- rbind(results, data.frame(
    Outcome = outcomes[i],
    Estimate = coef_est,
    StdError = std_err,
    CI_Lower = ci[1],
    CI_Upper = ci[2]
  ))
}

# Reorganize results for a forest plot
results$Outcome <- factor(
  results$Outcome, 
  levels = outcomes, 
  labels = outcome_labels # Use descriptive labels
)

# Add a treatment label for the legend
results$Treatment <- "MA" # Label the treatment as MA

# Create the forest plot
forest_plot <- ggplot(results, aes(x = Estimate, y = Outcome, color = Treatment)) +
  geom_point(size = 4) + # Points for estimates
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, size = 1) + # Error bars
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.2) + # Bold vertical line at 0
  theme_minimal(base_size = 16) + # Base font size for better readability
  labs(
    title = "Innovation bundle use",
    x = "Treatment Effect Estimate",
    y = "",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("MA" = "#1b9e77")) + # Same color for MA as in previous plots
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.position = "right" # Move legend to the right
  )

# Print the forest plot
print(forest_plot)
ggsave( file= paste(path,"PAP/results/forest_plot_MCC_use_simple.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)

##### prices

# Define outcomes and their corresponding labels
outcomes <- c("price_bought", "avg_sales_p")
outcome_labels <- c("price at which milk was bought", 
                    "price at which milk was sold")

# Initialize an empty data frame to store results
results <- data.frame(
  Outcome = character(),
  Estimate = numeric(),
  StdError = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop through outcomes to run regressions and extract estimates
for (i in 1:length(outcomes)) {
  # Run regression for the single treatment
  ols_MA <- lm(as.formula(paste(outcomes[i], "treat", sep = "~")), data = MCCs_end)
  
  # Extract coefficients, standard errors, and confidence intervals
  coef_est <- summary(ols_MA)$coefficients[2, 1] # Treatment effect estimate
  std_err <- summary(ols_MA)$coefficients[2, 2] # Standard error
  ci <- confint(ols_MA)[2, ]                    # Confidence interval
  
  # Append results to the data frame
  results <- rbind(results, data.frame(
    Outcome = outcomes[i],
    Estimate = coef_est,
    StdError = std_err,
    CI_Lower = ci[1],
    CI_Upper = ci[2]
  ))
}

# Reorganize results for a forest plot
results$Outcome <- factor(
  results$Outcome, 
  levels = outcomes, 
  labels = outcome_labels # Use descriptive labels
)

# Add a treatment label for the legend
results$Treatment <- "MA" # Label the treatment as MA

# Create the forest plot
forest_plot <- ggplot(results, aes(x = Estimate, y = Outcome, color = Treatment)) +
  geom_point(size = 4) + # Points for estimates
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, size = 1) + # Error bars
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.2) + # Bold vertical line at 0
  theme_minimal(base_size = 16) + # Base font size for better readability
  labs(
    title = "Impact on prices",
    x = "Treatment Effect Estimate",
    y = "",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("MA" = "#1b9e77")) + # Same color for MA as in previous plots
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.position = "right" # Move legend to the right
  )

# Print the forest plot
print(forest_plot)
ggsave( file= paste(path,"PAP/results/forest_plot_MCC_prices_simple.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)



#### analysis for samples
samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples <- samples[(samples$today >= as.Date("2024-12-04") & (samples$district== "Kazo" | samples$district== "Mbarara")) | samples$today >= as.Date("2024-12-05") , ] 

###iterate over outcomes in this family
outcomes <- c("Fat","SNF", "Added.Water","Protein", "Corrected.Lactometer.Reading")

###Make anderson index - revers outcome 3 (added water as less is better)
samples$samples_index <- anderson_index(samples[outcomes], revcols = 3)$index

outcomes <- c(outcomes, "samples_index")



res_samples <-   array(NA,dim=c(length(outcomes),7))
for (i in 1:length(outcomes)) {
  ###weight by quantity supplied
  ols <- lm(as.formula(paste(outcomes[i],"treat",sep="~")), data=samples, weights= samples$Qty)
  res_samples[i,1] <- mean(samples[samples[c(outcomes[i],"treat")]$treat =="C", outcomes[i]], na.rm=T)
  res_samples[i,2] <- sd(as.matrix(samples[samples[c(outcomes[i],"treat")]$treat =="C", outcomes[i]]), na.rm=T)
  vcov_cluster <- vcovCR(ols,cluster=samples$catch_ID,type="CR2")
  res <- coef_test(ols, vcov_cluster)
  res_samples[i,3:5] <- c(res[2,2],res[2,3],res[2,6])
  res_samples[i,7] <- nobs(ols)
}

res_samples <- round(res_samples,digits=3)
res_samples[1:length(outcomes)-1,6] <- anderson_sharp_q(res_samples[1:length(outcomes)-1,5])

## collects results: ctrl mean, ctrl sd, effect, sd effect, p-val, q-val, nobs
## last line is index

res_samples <- round(res_samples,digits=3)


saveRDS(res_samples, file= paste(path,"PAP/results/res_samples.RData", sep="/"))

#### simple graph for presentations
samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples <- samples[(samples$today >= as.Date("2024-12-04") & (samples$district== "Kazo" | samples$district== "Mbarara")) | samples$today >= as.Date("2024-12-05") , ] 

## also load samples from the android app
android_dta <- read.csv(paste(path, "sample_submissions/dta_reports.csv", sep="/"))
android_dta <- android_dta[android_dta$date >= as.Date("2024-12-01"),]
## stack this data to sample and add a treatment TA
android_dta$treat <- "TA"
## rename treatment in sample data to TM
common_cols <- intersect(names(samples), names(android_dta))

samples <- rbind(samples[common_cols],android_dta[common_cols])

samples$Rejected <- samples$Rejected == 1
samples$Price <- as.numeric(samples$Price)
samples$Qty <- as.numeric(samples$Qty)
samples$Analogue.Lactometer.Test <- as.numeric(samples$Analogue.Lactometer.Test)
#samples$Added.Water <- samples$Added.Water > 5

# Remove rows where 'Fat' is NA in the 'TA' treatment group
samples_clean <- samples %>%
  filter(!(treat == "TA" & is.na(Fat)))


# Define outcomes and their corresponding labels
outcomes <- c("Fat", "Added.Water", "Rejected", "SNF", "Corrected.Lactometer.Reading")
outcome_labels <- c("Fat", "Added Water", "Rejected", "SNF", "Corrected.Lactometer.Reading")

# Initialize an empty data frame to store results
results <- data.frame(
  Outcome = character(),
  Estimate_T = numeric(),
  Estimate_TA = numeric(),
  StdError_T = numeric(),
  StdError_TA = numeric(),
  CI_Lower_T = numeric(),
  CI_Upper_T = numeric(),
  CI_Lower_TA = numeric(),
  CI_Upper_TA = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Standardize the outcomes (Z-scores)
for (outcome in outcomes) {
  samples[[paste(outcome, "_standardized", sep = "")]] <- scale(samples[[outcome]])
}

# Loop through outcomes to run regressions and extract estimates
for (i in 1:length(outcomes)) {
  # Use the standardized outcome for the regression
  standardized_outcome <- paste(outcomes[i], "_standardized", sep = "")
  
  # Run regression for the standardized outcome with two dummies (T and TA)
  ols <- lm(as.formula(paste(standardized_outcome, "~ treat")), data = samples, weights = samples$Qty)
  
  # Extract coefficients, standard errors, and confidence intervals for T and TA
  coef_T <- summary(ols)$coefficients["treatT", 1]  # Estimate for T
  coef_TA <- summary(ols)$coefficients["treatTA", 1]  # Estimate for TA
  std_err_T <- summary(ols)$coefficients["treatT", 2]  # Standard error for T
  std_err_TA <- summary(ols)$coefficients["treatTA", 2]  # Standard error for TA
  ci_T <- confint(ols)["treatT", ]  # Confidence interval for T
  ci_TA <- confint(ols)["treatTA", ]  # Confidence interval for TA
  
  # Perform linearHypothesis to test if the coefficients of T and TA are equal
  hyp_test <- linearHypothesis(ols, c("treatT = treatTA"))
  p_value <- hyp_test$`Pr(>F)`[2]  # Extract p-value for equality test
  
  # Append results to the data frame
  results <- rbind(results, data.frame(
    Outcome = outcomes[i],
    Estimate_T = coef_T,
    Estimate_TA = coef_TA,
    StdError_T = std_err_T,
    StdError_TA = std_err_TA,
    CI_Lower_T = ci_T[1],
    CI_Upper_T = ci_T[2],
    CI_Lower_TA = ci_TA[1],
    CI_Upper_TA = ci_TA[2],
    p_value = p_value
  ))
}

# Reorganize results for a forest plot with reversed Y-axis order
results$Outcome <- factor(
  results$Outcome, 
  levels = rev(outcomes), # Reverse the order of outcomes
  labels = rev(outcome_labels) # Reverse the order of outcome labels
)
pd <- position_dodge(width = 0.4)

# Create the forest plot
forest_plot <- ggplot(results, aes(x = Estimate_T, y = Outcome)) +
  geom_point(aes(color = "T"), size = 4, position =position_nudge(y=0.2)) + # Points for estimates of T
  geom_point(aes(x = Estimate_TA, color = "TA"), size = 4, , position =position_nudge(y=-0.2)) + # Points for estimates of TA
  geom_errorbarh(aes(xmin = CI_Lower_T, xmax = CI_Upper_T), height = 0.3, size = 1, color = "#1b9e77", position =position_nudge(y=0.2)) + # Error bars for T
  geom_errorbarh(aes(xmin = CI_Lower_TA, xmax = CI_Upper_TA), height = 0.3, size = 1, color = "#85d1b0", position =position_nudge(y=-0.2)) + # Error bars for TA
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.2) + # Bold vertical line at 0
  theme_minimal(base_size = 16) + # Base font size for better readability
  labs(
    title = "Impact on Quality by Treatment",
    x = "Treatment Effect Estimate (Standardized)",
    y = "",
    color = "Treatment"
  ) +
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  ) +
  scale_color_manual(values = c("T" = "#1b9e77", "TA" = "#85d1b0")) + # Colors for T and TA
  # Add p-values for each outcome to the right of the bars
  geom_text(aes(x=Inf,label = sprintf("p = %.3f", p_value), hjust=1), 
             size = 4, hjust="right")

# Print the forest plot
print(forest_plot)
ggsave( file= paste(path,"PAP/results/forest_plot_samples_simple.png", sep="/"), plot = forest_plot, width = 10, height = 6, dpi = 300)



#####################cum dist#################################33
# Sample data
samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples <- samples[(samples$today >= as.Date("2024-12-04") & (samples$district== "Kazo" | samples$district== "Mbarara")) | samples$today >= as.Date("2024-12-05") , ] 

# Convert to POSIXct format with timezone "Africa/Kampala"
samples$timestamps <- ymd_hms(samples$start, tz = "Africa/Kampala")

# Remove the date part by setting all times to a common reference date
# e.g., "2024-01-01" so we can still handle it as a datetime object
samples$timestamps <- as.POSIXct(format(samples$timestamps, format = "2024-01-01 %H:%M:%S"), tz = "Africa/Kampala")

# Filter data to keep only timestamps between 7:00 and 14:00
samples <- samples %>%
  filter(hour(timestamps) >= 7 & hour(timestamps) < 14)

# Calculate cumulative percentage
samples <- samples %>%
  arrange(timestamps) %>%
  group_by(treat) %>%
  mutate(cumulative_count = row_number(),
         cumulative_percentage = (cumulative_count / n()) * 100) %>%
  ungroup()

cum_dist <- ggplot(samples, aes(x = timestamps, y = cumulative_percentage, color = treat)) +
  geom_line(size = 1.5) +
  labs(
    title = "Cumulative Distribution of Milk Deliveries",
    x = "Time of Delivery",
    y = "Cumulative Percentage of Events"
  ) +
  scale_color_manual(values = c("T" = "#1b9e77", "C" = "red")) +
  scale_x_datetime(
    labels = scales::date_format("%I:%M %p"),
    breaks = scales::date_breaks("2 hours")  # Show every 2nd hour
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85, 0.2),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    panel.grid.major = element_line(size = 0.5, color = "grey80"),
    panel.grid.minor = element_blank()
  )

# Print the plot
plot(cum_dist)
ggsave(paste(path,"PAP/results/distribution_of_delivery.png",sep="/"))

##KS test for difference in distributions
assorted_tests <-   array(NA,dim=c(4,2))

group_T <- samples$cumulative_percentage[samples$treat == "T"]
group_C <- samples$cumulative_percentage[samples$treat == "C"]

ks_test_result <- ks.test(group_T, group_C)
assorted_tests[1,1:2] <- as.numeric(print(ks_test_result)[1:2])

# Perform a two-sample t-test to compare means of cumulative times for T and C
group_T_times <- as.numeric(samples$timestamps[samples$treat == "T"])
group_C_times <- as.numeric(samples$timestamps[samples$treat == "C"])

t_test_result <- t.test(group_T_times, group_C_times)

# Print the result of the t-test
assorted_tests[2,1:2] <- as.numeric(t_test_result[c(1,3)])



 wilcox_test_result <- wilcox.test(group_T_times, group_C_times)

# Print the result of the Mann-Whitney U test
assorted_tests[3,1:2] <- as.numeric(wilcox_test_result[c(1,3)])

##first order stochastic dominance

# Sort the times for each group
sorted_T <- sort(samples$timestamps[samples$treat == "T"])
sorted_C <- sort(samples$timestamps[samples$treat == "C"])

# Compute CDFs by calculating the cumulative sum
cdf_T <- cumsum(rep(1, length(sorted_T))) / length(sorted_T)
cdf_C <- cumsum(rep(1, length(sorted_C))) / length(sorted_C)

# Check if the CDF of T is always less than or equal to the CDF of C  --- !!!this does not work with vectors of different length, for endline report this needs to be brought to same length in some way
cdf_diff <- cdf_T - cdf_C

# Test for first-order stochastic dominance
if (all(cdf_diff <= 0) && any(cdf_diff < 0)) {
  cat("Group T first-order stochastically dominates Group C.\n")
} else if (all(cdf_diff >= 0) && any(cdf_diff > 0)) {
  cat("Group C first-order stochastically dominates Group T.\n")
} else {
  cat("No first-order stochastic dominance between the two groups.\n")
}


#second order stochstic dominance
# Check if the cumulative area under the CDF of T is always less than or equal to the cumulative area under C
cdf_diff_area <- cumsum(cdf_T) - cumsum(cdf_C)

# Test for second-order stochastic dominance
if (all(cdf_diff_area <= 0) && any(cdf_diff_area < 0)) {
  cat("Group T second-order stochastically dominates Group C.\n")
} else if (all(cdf_diff_area >= 0) && any(cdf_diff_area > 0)) {
  cat("Group C second-order stochastically dominates Group T.\n")
} else {
  cat("No second-order stochastic dominance between the two groups.\n")
}

saveRDS(assorted_tests, file= paste(path,"PAP/results/assorted_tests.RData", sep="/"))





