rm(list=ls())

library(clubSandwich)
library(andersonTools)
library(car)
library(lubridate)
library(ggplot2)

path <- getwd()
path <- strsplit(path,"/PAP/analysis")[[1]]

### read in endline farmer data
farmers_end <- read.csv(paste(path, "endline/data/public/farmers.csv", sep = "/"))
### read in baseline farmer data 
farmers_base <- read.csv(paste(path, "baseline/data/public/farmers.csv", sep = "/"))

###catchment area for clustering
farmers_end$catch_ID <- as.factor(farmers_end$catchment)

##treat is MCC level treatment; vid is farmer level treatment
farmers_end$treat <- farmers_end$treat == "T" 

##merge in relevant data from baseline
# farmer type (direct of via trader)
names(farmers_end)[names(farmers_end) == 'farmer_type'] <- 'farmer_type_end'

farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","farmer_type")], by="farmer_ID", all.x=T)
farmers_end$trader <- farmers_end$farmer_type == 2

#primary outcomes at the farmer level

#q39 == "Yes" ##oversowing
#q39c == "Yes" ##legume pastures
#q39d == "Yes" ##trees
#q40 in c(1,3) #controlled/zero grazing in dry seaon
#q41 in c(1,3) #controlled/zero grazing in wet seaon
#q42 == "Yes" #pasture conservation
#q43 == "Yes" ##supplements

farmers_base$b_improve_index <- anderson_index(cbind(farmers_base$q39 == "Yes",farmers_base$q39c  == "Yes", farmers_base$q40 %in% c(1,3), farmers_base$q41 %in% c(1,3), farmers_base$q42  == "Yes", farmers_base$q43  == "Yes"))$index
farmers_end$improve_index <- anderson_index(cbind(farmers_end$q39 == "Yes",farmers_end$q39c  == "Yes", farmers_end$q40 %in% c(1,3), farmers_end$q41 %in% c(1,3), farmers_end$q42  == "Yes", farmers_end$q43  == "Yes"))$index
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_improve_index")], by="farmer_ID", all.x=T)

##buyer checks using milk analyzer
farmers_end$check_MA <- farmers_end$q58.4 == "True" | farmers_end$qx5.4 == "True" | farmers_end$qx17.4 == "True" | farmers_end$qx29.4 == "True" | farmers_end$qx41.4 == "True" | farmers_end$qx53.4 == "True"
farmers_base$b_check_MA <-farmers_base$q58== "4" | farmers_base$qx5.4 == "True" | farmers_base$qx17.4 == "True" | farmers_base$qx29.4 == "True" | farmers_base$qx41.4 == "True" | farmers_base$qx53.4 == "True"
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
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_avg_sales_p")], by="farmer_ID", all.x=T)

## Does the buyer pay for higher quality milk - q61/qx8/qx20/qx32/qx44/qx56

farmers_base$b_gets_q_bonus <- farmers_base$q61 == "Yes" | farmers_base$qx8 == "Yes" | farmers_base$qx20 == "Yes" | farmers_base$qx32 == "Yes" | farmers_base$qx44 == "Yes" | farmers_base$qx56 == "Yes"
farmers_end$gets_q_bonus <- farmers_end$q61 == "Yes" | farmers_end$qx8 == "Yes" | farmers_end$qx20 == "Yes" | farmers_end$qx32 == "Yes" | farmers_end$qx44 == "Yes" | farmers_end$qx56 == "Yes"
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_gets_q_bonus")], by="farmer_ID", all.x=T)


### farmers improve bargaining power

farmers_end$bargain_power <- farmers_end$q65 %in% c(1,3) | farmers_end$qx12 %in% c(1,3) | farmers_end$qx24 %in% c(1,3)  | farmers_end$qx36 %in% c(1,3) | farmers_end$qx48 %in% c(1,3) | farmers_end$qx60 %in% c(1,3)
farmers_base$b_bargain_power <- farmers_base$q65 %in% c(1,3) | farmers_base$qx12 %in% c(1,3) | farmers_base$qx24 %in% c(1,3)  | farmers_base$qx36 %in% c(1,3) | farmers_base$qx48 %in% c(1,3) | farmers_base$qx60 %in% c(1,3)
farmers_end <- merge(farmers_end,farmers_base[c("farmer_ID","b_bargain_power")], by="farmer_ID", all.x=T)

outcomes <- c("improve_index","check_MA","avg_sales_p","gets_q_bonus","bargain_power")
b_outcomes <- c("b_improve_index","b_check_MA","b_avg_sales_p","b_gets_q_bonus","b_bargain_power")

###Make anderson index
farmers_end$primary_farmer_index <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_primary_farmer_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes <- c(outcomes, "primary_farmer_index")
b_outcomes <- c(b_outcomes, "b_primary_farmer_index")

##Sold to milk to collection center in the week preceding the survey
###fully interacted model
res_farmers <-   array(NA,dim=c(length(outcomes),26))
for (i in 1:length(outcomes)) {
ols <- lm(as.formula(paste(paste(outcomes[i],"treat*vid*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
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
}

res_farmers[1:length(outcomes)-1,16] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,5])
res_farmers[1:length(outcomes)-1,17] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,8])
res_farmers[1:length(outcomes)-1,18] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,11])


###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)
 
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,19:21] <- c(res[2,2],res[2,3],res[2,6])

}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,22:24] <- c(res[2,2],res[2,3],res[2,6])
  
}
res_farmers[1:length(outcomes)-1,25] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,18])
res_farmers[1:length(outcomes)-1,26] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,21])


res_farmers <- round(res_farmers,digits=3)

saveRDS(res_farmers, file= paste(path,"PAP/results/res_farmers.RData", sep="/"))
 
##secondary outcomes - sales
#q52 sold in last week
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


farmers_base$b_avg_sales_q <- rowMeans(farmers_base[columns], na.rm=T)
farmers_base$b_avg_sales_q[is.nan(farmers_base$b_avg_sales_q)] <- NA
farmers_end <- merge( farmers_end, farmers_base[c("farmer_ID","b_avg_sales_q")], by="farmer_ID", all.x=T)

outcomes <- c("q_sold_dry", "q_sold_wet", "sold_last_week", "avg_sales_q")
b_outcomes <- c("b_q_sold_dry", "b_q_sold_wet", "b_sold_last_week", "b_avg_sales_q")

###Make anderson index
farmers_end$secondary_farmer_quant_index  <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_secondary_farmer_quant_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes <- c(outcomes, "secondary_farmer_quant_index")
b_outcomes <- c(b_outcomes, "b_secondary_farmer_quant_index")

##Sold to milk to collection center in the week preceding the survey
###fully interacted model
res_farmers <-   array(NA,dim=c(length(outcomes),26))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(outcomes[i],"treat*vid*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
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
}

res_farmers[1:length(outcomes)-1,16] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,5])
res_farmers[1:length(outcomes)-1,17] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,8])
res_farmers[1:length(outcomes)-1,18] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,11])


###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)

for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,19:21] <- c(res[2,2],res[2,3],res[2,6])
  
}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,22:24] <- c(res[2,2],res[2,3],res[2,6])
  
}
res_farmers[1:length(outcomes)-1,25] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,18])
res_farmers[1:length(outcomes)-1,26] <- anderson_sharp_q(res_farmers[1:length(outcomes)-1,21])


res_farmers_sec_quant <- round(res_farmers,digits=3)

saveRDS(res_farmers_sec_quant, file= paste(path,"PAP/results/res_farmers_sec_quant.RData", sep="/"))

##secondary outcomes - production


######################## analysis at the MCC level ############################################
### read in clean endline MCC data
MCCs_end <- read.csv(paste(path, "endline/data/public/MCCs.csv", sep = "/"))
### drop duplicates - this was for dummy data but this will not do anything with eventual clean endline data
library(dplyr)
MCCs_end <- MCCs_end %>%
  distinct(MCC_ID, .keep_all = TRUE)
### read in clean baseline farmer data 
MCCs_base <- read.csv(paste(path, "baseline/data/public/MCCs.csv", sep = "/"))

### testing of quality on incoming samples
MCCs_end$test_MA_in <-  MCCs_end$q25x3 == 1 | MCCs_end$q25x3 == 2
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
  return(x)
})

MCCs_end$avg_sales_p <- rowMeans(MCCs_end[columns], na.rm=T)
MCCs_end$avg_sales_p[is.nan(MCCs_end$avg_sales_p)] <- NA

MCCs_base$b_avg_sales_p <- rowMeans(MCCs_base[columns], na.rm=T)
MCCs_base$b_avg_sales_p[is.nan(MCCs_base$b_avg_sales_p)] <- NA
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_avg_sales_p")], by="MCC_ID", all.x=T)

### quality paid to bonus to farmers
MCCs_end$gives_q_bonus <- MCCs_end$q29 == 1
MCCs_base$b_gives_q_bonus <- MCCs_base$q29 == 1
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_gives_q_bonus")], by="MCC_ID", all.x=T)


 
MCCs_end$gets_q_bonus <- MCCs_end$q44==1 | MCCs_end$q54 ==1 | MCCs_end$q64 ==1 |  MCCs_end$q74 ==1 |  MCCs_end$q84 ==1
MCCs_base$b_gets_q_bonus <- MCCs_base$q44==1 | MCCs_base$q54 ==1 | MCCs_base$q64 ==1 |  MCCs_base$q74 ==1 |  MCCs_base$q84 ==1
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


##sold to top 5 processors
MCCs_end$top_proc <- MCCs_end$q32.2=="True" & MCCs_end$q33.6!="True"
#merge in baseline outcome
MCCs_base$b_top_proc <- MCCs_base$q32.2=="True" & MCCs_base$q33.6!="True"
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_top_proc")], by="MCC_ID", all.x=T)

#### analysis for samples
samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))

###iterate over outcomes in this family
outcomes <- c("Fat","SNF", "Added.Water","Protein", "Corrected.Lactometer.Reading")

###Make anderson index - revers outcome 3 (added water as less is better)
samples$samples_index <- anderson_index(samples[outcomes], revcols = 3)$index

outcomes <- c(outcomes, "samples_index")



res_samples <-   array(NA,dim=c(length(outcomes),7))
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i],"treat",sep="~")), data=samples)
  res_samples[i,1] <- mean(samples[samples[c(outcomes[i],"treat")]$treat =="C", outcomes[i]], na.rm=T)
  res_samples[i,2] <- sd(as.matrix(samples[samples[c(outcomes[i],"treat")]$treat =="C", outcomes[i]]), na.rm=T)
  vcov_cluster <- vcovCR(ols,cluster=samples$catch_ID,type="CR3")
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


# Sample data

# Convert to POSIXct format with timezone "Africa/Kampala"
samples$timestamps <- ymd_hms(samples$start, tz = "Africa/Kampala")

# Remove the date part by setting all times to a common reference date
# e.g., "2024-01-01" so we can still handle it as a datetime object
samples$timestamps <- as.POSIXct(format(samples$timestamps, format = "2024-01-01 %H:%M:%S"), tz = "Africa/Kampala")

# Filter data to keep only timestamps between 7:00 and 14:00
samples <- samples %>%
  filter(hour(timestamps) >= 7 & hour(timestamps) < 14)

##redefine outcome as time before closure (14:00)
samples$reference_time <-  floor_date(samples$timestamps, unit = "day") + hours(14)
samples$time_elapsed <- as.numeric(difftime(samples$reference_time,samples$timestamps, units = "mins"))

# Calculate cumulative percentage
samples <- samples %>%
  arrange(time_elapsed) %>%
  group_by(treat) %>%
  mutate(cumulative_count = row_number(),
         cumulative_percentage = (cumulative_count / n()) * 100) %>%
  ungroup()

# Plot the cumulative distributions for each group
cum_dist <- ggplot(samples, aes(x = time_elapsed, y = cumulative_percentage, color = treat)) +
  geom_line() +
  labs(title = "Cumulative Distribution of Milk Deliveries for Groups T and C Between 7:00 and 14:00",
       x = "Time",
       y = "Cumulative Percentage of Events") +
  scale_color_manual(values = c("T" = "blue", "C" = "red")) +
  theme_minimal()

ggsave(paste(path,"PAP/results/test.png",sep="/"))

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





