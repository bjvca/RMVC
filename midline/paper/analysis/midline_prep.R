rm(list=ls())
dta <-read.csv("/home/bjvca/data/projects/OneCG/RMVC/sample_submissions/latest_raw.csv") 

set.seed(18112024)

dta$one <- 1
MCCs <- aggregate(dta$one, list(dta$Milk.Center.ID), sum)

mcc30 <- subset(dta,Milk.Center.ID == 57)
aggregate(mcc30$one, list(mcc30$Farmer_ID), sum)

dta <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/sample_submissions/dta_reports.csv") 

### for analysis, set end date at end of april 2024 ### this is a period of about 182 days
dta$date <- as.Date(dta$date, format = "%Y-%m-%d")

dta <- subset(dta, date <"2024-05-01")
table(dta$MCC_ID)


sample_list <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/baseline/data/raw/list_wilber.csv")
setdiff(sample_list$MCC_ID, dta$MCC_ID)

dta$ones <- 1

submissions_MCC <- data.frame(aggregate(dta$ones, list(dta$MCC_ID),FUN=sum))
names(submissions_MCC)<- c("MCC_ID","submissions")


###merge in from baseline the capacity to scale submissions
baseline_MCCs <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/baseline/data/public/MCCs.csv")
### we average over rainy and dry season
baseline_MCCs$nr_ppl_wet <- baseline_MCCs$Q23
baseline_MCCs$nr_ppl_wet[baseline_MCCs$nr_ppl_wet<10] <- NA
baseline_MCCs$nr_ppl_wet[baseline_MCCs$nr_ppl_wet>200] <- NA

### we average over rainy and dry season
baseline_MCCs$nr_ppl_dry <- baseline_MCCs$Q24
baseline_MCCs$nr_ppl_dry[baseline_MCCs$nr_ppl_dry<10] <- NA
baseline_MCCs$nr_ppl_dry[baseline_MCCs$nr_ppl_dry>200] <- NA

###impute missing variables
library(arm)
random.imp <- function(a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

impute <- function(a, a.impute){
  ifelse (is.na(a), a.impute, a)
}

baseline_MCCs$nr_ppl_wet.imp <- random.imp(baseline_MCCs$nr_ppl_wet)
baseline_MCCs$nr_ppl_dry.imp <- random.imp(baseline_MCCs$nr_ppl_dry)

n.sims <- 10
for (s in 1:n.sims){
  lm.1 <- lm (nr_ppl_wet~nr_ppl_dry.imp, data=baseline_MCCs)
  pred.1 <- rnorm (predict(lm.1), sigma.hat(lm.1))
  nr_ppl_wet.imp <- impute(baseline_MCCs$nr_ppl_wet, pred.1)
  
  lm.2 <- lm (nr_ppl_dry ~ nr_ppl_wet.imp, data=baseline_MCCs)
  pred.2 <- rnorm (predict(lm.2), sigma.hat(lm.2))
  nr_ppl_dry.imp <- impute (baseline_MCCs$nr_ppl_dry, pred.2)
}

## take average
baseline_MCCs$nr_ppl <- (baseline_MCCs$nr_ppl_wet.imp + baseline_MCCs$nr_ppl_dry.imp)/2


baseline_MCCs <- merge(baseline_MCCs,submissions_MCC,all.x=T, by="MCC_ID" )

baseline_MCCs$submission_rate <- baseline_MCCs$submissions/(baseline_MCCs$nr_ppl*182)

png("/home/bjvca/data/projects/OneCG/RMVC/midline/paper/analysis/submissions_all.png",
    width=2200, height=1600, res = 300)
barplot(sort(baseline_MCCs$submission_rate), ylab="samples")
dev.off()

sum(baseline_MCCs$submission_rate < .01, na.rm=T)
sum(baseline_MCCs$submission_rate < .1, na.rm=T)
sum(baseline_MCCs$submission_rate > .5, na.rm=T)
summary(baseline_MCCs$submission_rate) 


png("/home/bjvca/data/projects/OneCG/RMVC/midline/paper/analysis/submissions.png",
    width=1400, height=2400,res=300)
par(mfrow=c(3,1))
plot(table(dta$date[dta$MCC_ID=="MCC_216"]), ylab="samples", main="MCC 1") ### consistent
plot(table(dta$date[dta$MCC_ID=="MCC_93"]), ylab="samples", main="MCC 2") ### inconsistent
plot(table(dta$date[dta$MCC_ID=="MCC_542"]), ylab="samples", main="MCC 3") ### never really started
dev.off()

###read in data on registered farmers from Android App
dta_farmers <- read.csv("https://milk.ug/export/farmers")
dta_farmers[dta_farmers$Milk.Center.ID == 57,]

### read in baseline data from farmers
baseline_farmers <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/baseline/data/public/farmers.csv")
#also in baseline
baseline_farmers$type <- "NA"
baseline_farmers$type[ baseline_farmers$farmer_ID %in% c("F_4302_D", "F_4308_T", "F_1848_D", "F_10821_D")] <- "both"

#only in baseline
baseline_farmers$type[ baseline_farmers$farmer_ID %in% c("F_4319_D",  "F_1844_D", "F_1857_T", "F_10832_T", "F_10839_D", "F_11051_D", "F_11052_T")] <- "base_only"

baseline_farmers$q24[baseline_farmers$q24 == 999] <- NA
baseline_farmers$q30[baseline_farmers$q30 == 999] <- NA
baseline_farmers$totcows <- baseline_farmers$q30 + baseline_farmers$q24

baseline_farmers$totcows[baseline_farmers$totcows  > 250]  <- NA

mean(baseline_farmers$totcows[baseline_farmers$type == "both"])
mean(baseline_farmers$totcows[baseline_farmers$type == "base_only"])
mean(baseline_farmers$totcows[baseline_farmers$type != "NA"])

baseline_farmers$q44
baseline_farmers$q45[baseline_farmers$q45 == 999] <- NA

mean(baseline_farmers$q44[baseline_farmers$type == "both"])
mean(baseline_farmers$q44[baseline_farmers$type == "base_only"])
mean(baseline_farmers$q44[baseline_farmers$type != "NA"])

mean(baseline_farmers$q45[baseline_farmers$type == "both"])
mean(baseline_farmers$q45[baseline_farmers$type == "base_only"])
mean(baseline_farmers$q45[baseline_farmers$type != "NA"])

baseline_farmers$qual_prem <- "No"
baseline_farmers$qual_prem[ baseline_farmers$qx56 == "Yes" | baseline_farmers$qx44 =="Yes"  | baseline_farmers$qx32 =="Yes"  | baseline_farmers$qx20 =="Yes"  | baseline_farmers$qx8 =="Yes"  | baseline_farmers$q61 =="Yes" ] <- "Yes"


mean(baseline_farmers$qual_prem[baseline_farmers$type == "both"]=="Yes")
mean(baseline_farmers$qual_prem[baseline_farmers$type == "base_only"]=="Yes")
mean(baseline_farmers$qual_prem[baseline_farmers$type != "NA"]=="Yes")

## how many farmers ever received a premium for quality
prop.table(table(baseline_farmers$qx8[baseline_farmers$qx8 %in% c("No","Yes")]))

library(arm)
baseline_MCCs <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/baseline/data/public/MCCs.csv")
table(baseline_MCCs$q29)
table(baseline_MCCs$q54)

baseline_MCCs[baseline_MCCs$MCC_ID == "MCC_18",]
baseline_MCCs[baseline_MCCs$MCC_ID == "MCC_29",]

baseline_MCCs$use_MA <- baseline_MCCs$MCC_ID %in% names(table(dta$MCC_ID))[table(dta$MCC_ID)>100]

baseline_MCCs <- subset(baseline_MCCs,lactoscan == "T")

###analysis of the determinants of analyzer use
baseline_MCCs$coop <- baseline_MCCs$secC_group.q8 ==2
baseline_MCCs$labour <- baseline_MCCs$q14m


baseline_MCCs$capacity <- baseline_MCCs$qual.q26
baseline_MCCs$premium <- baseline_MCCs$q29 =="1" 
baseline_MCCs$top_5 <- baseline_MCCs$q33.6=="False"



summary(lm(use_MA~coop+labour+nr_ppl+capacity+premium+top_5,data=baseline_MCCs))



sample_list <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/baseline/data/raw/list_wilber_gps.csv")
setdiff(sample_list$MCC_ID, dta$MCC_ID)


### anaysis of transactions
### start with looking at rejection rates
dta$Added.Water[dta$Added.Water>20] <- NA 
dta$Added.Water[dta$Added.Water==0.01] <- 1 
dta$Added.Water[dta$Added.Water==0.02] <- 2 
dta$Added.Water[dta$Added.Water==0.03] <- 3 
dta$Added.Water[dta$Added.Water==0.1] <- 10 
dta$Added.Water[dta$Added.Water==0.11] <- 11
dta$Added.Water[dta$Added.Water==0.12] <- 12 
dta$Added.Water[dta$Added.Water==0.13] <- 13 
dta$Added.Water[dta$Added.Water==0.14] <- 14 
dta$Added.Water[dta$Added.Water==1.1] <- 11 
dta$Added.Water[dta$Added.Water==1.2] <- 12 
dta$Added.Water[dta$Added.Water==1.3] <- 13
dta$Added.Water[dta$Added.Water==1.6] <- 16
dta$Added.Water[dta$Added.Water==1.8] <- 18
dta$Added.Water[dta$Added.Water==1.9] <- 19 
dta$Added.Water[dta$Added.Water==0.2] <- 20 
dta$Added.Water[dta$Added.Water==0.3] <- NA
dta$Added.Water[dta$Added.Water==0.6] <- NA
dta$Added.Water[dta$Added.Water==0.9] <- NA

dta$Added.Water[dta$Added.Water>1 & dta$Added.Water<2] <- NA
dta$Added.Water[dta$Added.Water>2 & dta$Added.Water<3] <- NA
dta$Added.Water[dta$Added.Water>3 & dta$Added.Water<4] <- NA
dta$Added.Water[dta$Added.Water>4 & dta$Added.Water<5] <- NA
dta$Added.Water[dta$Added.Water>5 & dta$Added.Water<6] <- NA
dta$Added.Water[dta$Added.Water>8 & dta$Added.Water<9] <- NA
dta$Added.Water[dta$Added.Water>9 & dta$Added.Water<10] <- NA

table(dta$Added.Water)

## run some simple regressions
summary(lm(as.numeric(Price)~as.numeric(Fat),data=dta))

dta$qual <- as.numeric(dta$Fat)>=4

summary(lm(as.numeric(Price)~qual,data=dta))

dim(dta)

### keep only MCCs with more than 500 submissions

selector <- names(table(dta$MCC_ID))[table(dta$MCC_ID)>500]
dta <- subset(dta, MCC_ID %in% selector)
### for each MCC, determine the first x submissions and calculate rejection rage


stack_first <- NA
stack_last <- NA

for (i in selector) {
  dta_sel <- subset(dta, MCC_ID == i )  
  
print(  mean(dta_sel[dta_sel$date==min(dta_sel$date),]$Added.Water, na.rm=T))
print(  mean(dta_sel[dta_sel$date==max(dta_sel$date),]$Added.Water, na.rm=T))
 
print(mean(dta_sel[dta_sel$date==min(dta_sel$date),]$Added.Water, na.rm=T) >= mean(dta_sel[dta_sel$date==max(dta_sel$date),]$Added.Water, na.rm=T))

stack_first <- c(stack_first,  dta_sel[dta_sel$date==min(dta_sel$date),]$Added.Water)
stack_last <-c(stack_last,  dta_sel[dta_sel$date==max(dta_sel$date),]$Added.Water)
}
## share with water (yes/no)
mean(stack_first>0, na.rm=T)

mean(stack_last>0, na.rm=T)


#average percentage water
mean(stack_first, na.rm=T)

mean(stack_last, na.rm=T)

### can we do this also for quantities?
dta$Alcohol.Test[dta$Alcohol.Test == "N/A"] <- NA

stack_first <- NA
stack_last <- NA

for (i in selector) {
  dta_sel <- subset(dta, MCC_ID == i )  
  
  print(  mean(dta_sel[dta_sel$date==min(dta_sel$date),]$Qty, na.rm=T))
  print(  mean(dta_sel[dta_sel$date==max(dta_sel$date),]$Qty, na.rm=T))
  

  stack_first <- c(stack_first,  dta_sel[dta_sel$date==min(dta_sel$date),]$Qty)
  stack_last <-c(stack_last,  dta_sel[dta_sel$date==max(dta_sel$date),]$Qty)
}



#quantity delivered
mean(stack_first, na.rm=T)

mean(stack_last, na.rm=T)


## can we also do this for fat content?
stack_first <- NA
stack_last <- NA

for (i in selector) {
  dta_sel <- subset(dta, MCC_ID == i )  
  
  print(  mean(dta_sel[dta_sel$date==min(dta_sel$date),]$Fat, na.rm=T))
  print(  mean(dta_sel[dta_sel$date==max(dta_sel$date),]$Fat, na.rm=T))
  
  print(mean(dta_sel[dta_sel$date==min(dta_sel$date),]$Fat, na.rm=T) >= mean(dta_sel[dta_sel$date==max(dta_sel$date),]$Fat, na.rm=T))
  
  stack_first <- c(stack_first,  dta_sel[dta_sel$date==min(dta_sel$date),]$Fat)
  stack_last <-c(stack_last,  dta_sel[dta_sel$date==max(dta_sel$date),]$Fat)
}
## share with water (yes/no)
mean(stack_first>0, na.rm=T)

mean(stack_last>0, na.rm=T)


#average percentage water
mean(stack_first, na.rm=T)

mean(stack_last, na.rm=T)

##no, this does not work because of seasonality in butter fat content - try exploiting staggered introduction of milk analyzers

## can we also do this for fat content?
stack_first <- NA
stack_last <- NA
stack_means <- NA

#loop over MCCs
for (i in selector) {
  ### for each MCC, select starting data
  first_day <- min(dta$date[dta$MCC_ID == i])
  ### store butter fat reading

  stack_first <- c(stack_first,  dta[dta$MCC_ID == i & dta$date == first_day,]$Fat)
  stack_last <-c(stack_last,  dta[dta$MCC_ID != i & dta$date ==first_day,]$Fat)
  
 stack_means <- c(stack_means, mean(stack_last,na.rm=T) - mean(stack_first,na.rm=T) )
}


#average percentage water
mean(stack_first, na.rm=T)

mean(stack_last, na.rm=T)

### correlation between prices and quality

dta <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/sample_submissions/dta_reports.csv") 

### for analysis, set end date at end of april 2024
dta$date <- as.Date(dta$date, format = "%Y-%m-%d")

dta <- subset(dta, date <"2024-05-01")
table(dta$MCC_ID)
## start with looking at rejection rates
dta$Added.Water[dta$Added.Water>20] <- NA 
dta$Added.Water[dta$Added.Water==0.01] <- 1 
dta$Added.Water[dta$Added.Water==0.02] <- 2 
dta$Added.Water[dta$Added.Water==0.03] <- 3 
dta$Added.Water[dta$Added.Water==0.1] <- 10 
dta$Added.Water[dta$Added.Water==0.11] <- 11
dta$Added.Water[dta$Added.Water==0.12] <- 12 
dta$Added.Water[dta$Added.Water==0.13] <- 13 
dta$Added.Water[dta$Added.Water==0.14] <- 14 
dta$Added.Water[dta$Added.Water==1.1] <- 11 
dta$Added.Water[dta$Added.Water==1.2] <- 12 
dta$Added.Water[dta$Added.Water==1.3] <- 13
dta$Added.Water[dta$Added.Water==1.6] <- 16
dta$Added.Water[dta$Added.Water==1.8] <- 18
dta$Added.Water[dta$Added.Water==1.9] <- 19 
dta$Added.Water[dta$Added.Water==0.2] <- 20 
dta$Added.Water[dta$Added.Water==0.3] <- NA
dta$Added.Water[dta$Added.Water==0.6] <- NA
dta$Added.Water[dta$Added.Water==0.9] <- NA

dta$Added.Water[dta$Added.Water>1 & dta$Added.Water<2] <- NA
dta$Added.Water[dta$Added.Water>2 & dta$Added.Water<3] <- NA
dta$Added.Water[dta$Added.Water>3 & dta$Added.Water<4] <- NA
dta$Added.Water[dta$Added.Water>4 & dta$Added.Water<5] <- NA
dta$Added.Water[dta$Added.Water>5 & dta$Added.Water<6] <- NA
dta$Added.Water[dta$Added.Water>8 & dta$Added.Water<9] <- NA
dta$Added.Water[dta$Added.Water>9 & dta$Added.Water<10] <- NA


dta$Price[dta$Price==0] <- NA
dta$SNF[dta$SNF==0] <- NA
dta$SNF[dta$SNF>=11] <- NA
dta$SNF[dta$SNF<6] <- NA

dta$Fat[dta$Fat<1] <- NA
dta$Fat[dta$Fat>6] <- NA


summary(lm(Price~Added.Water+ as.factor(MCC_ID),data=dta))
summary(lm(Price~Fat+ as.factor(MCC_ID),data=dta))
summary(lm(Price~SNF+ as.factor(MCC_ID),data=dta))

### do this at the MCC level

MCC_averages <- aggregate(dta[c("Price","Added.Water","Fat", "SNF")],by=list(dta$MCC_ID), mean, na.rm=T)
names(MCC_averages) <- c("MCC_ID","Price","Added.Water","Fat", "SNF")

summary(lm(Price~Added.Water,data=MCC_averages))
summary(lm(Price~Fat,data=MCC_averages))
summary(lm(Price~SNF,data=MCC_averages))

period <- 15
### compare start to end
stack_first <- dta[1,]
stack_first <- NA

#loop over MCCs
for (i in selector) {
  ### for each MCC, select starting data
  first_day <- min(dta$date[dta$MCC_ID == i])
  ### store butter fat reading
  
  stack_first <- rbind(stack_first,  dta[dta$MCC_ID == i & dta$date >= first_day & dta$date <= first_day+period,])

  

}


### within MCC
summary(lm(Price~Added.Water+ as.factor(MCC_ID),data=stack_first))
summary(lm(Price~Fat+ as.factor(MCC_ID),data=stack_first))
summary(lm(Price~SNF+ as.factor(MCC_ID),data=stack_first))
##between MCCs
MCC_averages <- aggregate(stack_first[c("Price","Added.Water","Fat", "SNF")],by=list(stack_first$MCC_ID), mean, na.rm=T)
names(MCC_averages) <- c("MCC_ID","Price","Added.Water","Fat", "SNF")

summary(lm(Price~Added.Water,data=MCC_averages))
summary(lm(Price~Fat,data=MCC_averages))
summary(lm(Price~SNF,data=MCC_averages))


stack_last <- dta[1,]
stack_last <- NA
#loop over MCCs
for (i in selector) {
  ### for each MCC, select starting data
  last_day <- max(dta$date[dta$MCC_ID == i])
  ### store butter fat reading
  
  stack_last <- rbind(stack_last,  dta[dta$MCC_ID == i & dta$date >= last_day-period & dta$date <= last_day,])
  
  
  
}

summary(lm(Price~Added.Water+ as.factor(MCC_ID),data=stack_last))
summary(lm(Price~Fat+ as.factor(MCC_ID),data=stack_last))
summary(lm(Price~SNF+ as.factor(MCC_ID),data=stack_last))

MCC_averages <- aggregate(stack_last[c("Price","Added.Water","Fat", "SNF")],by=list(stack_last$MCC_ID), mean, na.rm=T)
names(MCC_averages) <- c("MCC_ID","Price","Added.Water","Fat", "SNF")

summary(lm(Price~Added.Water,data=MCC_averages))
summary(lm(Price~Fat,data=MCC_averages))
summary(lm(Price~SNF,data=MCC_averages))