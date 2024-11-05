rm(list=ls())

library(clubSandwich)
library(andersonTools)

path <- getwd()
path <- strsplit(path,"/PAP/analysis")[[1]]

### read in endline farmer data
farmers_end <- read.csv(paste(path, "endline/data/public/farmers.csv", sep = "/"))
### read in baseline farmer data 
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
res_farmers <-   array(NA,dim=c(length(outcomes),18))
for (i in 1:length(outcomes)) {
ols <- lm(as.formula(paste(paste(outcomes[i],"treat*vid*trader",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)
res_farmers[i,1] <- mean(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
res_farmers[i,2] <- sd(as.matrix(farmers_end[outcomes[i]]), na.rm=T)
res_farmers[i,3:7] <- c(res[2,2],res[2,3],res[2,6], conf[2,5],conf[2,6])
res_farmers[i,8:12] <- c(res[3,2],res[3,3],res[3,6], conf[3,5],conf[3,6])
res_farmers[i,13:18] <- c(res[5,2],res[5,3],res[5,6], conf[5,5],conf[5,6], nobs(ols))
}
###model with demeaned orthogonal treatment - milk analyzer
farmers_end$trader_demeaned  <- farmers_end$trader - mean(farmers_end$trader, na.rm=T)
farmers_end$vid_demeaned <- farmers_end$vid - mean(farmers_end$vid, na.rm=T)
 
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"treat*vid_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,3:7] <- c(res[2,2],res[2,3],res[2,6], conf[2,5],conf[2,6])

}
###model with demeaned orthogonal treatment - video tratment

farmers_end$treat_demeaned <- farmers_end$treat - mean(farmers_end$treat, na.rm=T)
for (i in 1:length(outcomes)) {
  ols <-  lm(as.formula(paste(paste(outcomes[i],"vid*treat_demeaned*trader_demeaned",sep="~"),b_outcomes[i],sep="+")), data=farmers_end)
  vcov_cluster <- vcovCR(ols,cluster=farmers_end$catch_ID,type="CR3")
  res <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)
  res_farmers[i,3:7] <- c(res[2,2],res[2,3],res[2,6], conf[2,5],conf[2,6])
  
}



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

#Volume of milk collected on an average day in last 7 days

MCCs_end$q25c[MCCs_end$q25c %in% c("n/a","999") ] <- NA
MCCs_end$q25c <- as.numeric(as.character(MCCs_end$q25c))
names(MCCs_end)[names(MCCs_end) == 'q25c'] <- 'quantity_bought'

#merge in baseline outcome
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","q25c")], by="MCC_ID", all.x=T)

names(MCCs_end)[names(MCCs_end) == 'q25c'] <- 'b_quantity_bought'

##sold to top 5 processors
MCCs_end$top_proc <- MCCs_end$q32.2=="True" & MCCs_end$q33.6!="True"
#merge in baseline outcome
MCCs_base$b_top_proc <- MCCs_base$q32.2=="True" & MCCs_base$q33.6!="True"
MCCs_end <- merge( MCCs_end, MCCs_base[c("MCC_ID","b_top_proc")], by="MCC_ID", all.x=T)

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

res_MCCs <- round(res_MCCs,digits=2)
res_MCCs[1:length(outcomes)-1,6] <- anderson_sharp_q(res_MCCs[1:length(outcomes)-1,5])

## collects results: ctrl mean, ctrl sd, effect, sd effect, p-val, q-val, nobs
## last line is index

saveRDS(res_MCCs, file= paste(path,"PAP/results/res_MCCs.RData", sep="/"))


