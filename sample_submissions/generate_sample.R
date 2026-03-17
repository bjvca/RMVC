rm(list=ls())

#dta_reports <- read.csv("/home/bjvca/data/projects/OneCG/RMVC/sample_submissions/master_export.csv") 
#dta_reports$delivery_date <- as.character(as.Date(dta_reports$Report.Date))
#dta_reports$delivery_time <- as.character(format(as.POSIXct(dta_reports$Report.Date), format = '%H:%M'))

#write.csv(dta_reports,"/home/bjvca/data/projects/OneCG/RMVC/sample_submissions/latest_raw.csv", row.names =  FALSE) 

dta_reports <- read.csv("/Users/B.VanCampenhout/data/projects/OneCG/RMVC/sample_submissions/latest_raw.csv") 

###update

#url <- "https://milk.ug/export/report?from=2025-08-01&to=2026-01-14"


url <- paste(paste("https://milk.ug/export/report",Sys.Date()-7,sep="?from="),Sys.Date(), sep="&to=")
dta_reports_update <- read.csv(url)

dta_reports_update$delivery_date <- as.character(as.Date(dta_reports_update$Report.Date))
dta_reports_update$delivery_time <- as.character(format(as.POSIXct(dta_reports_update$Report.Date), format = '%H:%M'))


dta_reports <- rbind(dta_reports,dta_reports_update)

dta_reports$delivery_date <- as.character(as.Date(dta_reports$Report.Date))
dta_reports$delivery_time <- as.character(format(as.POSIXct(dta_reports$Report.Date), format = '%H:%M'))

#remove duplicates
### remove duplicate records from dta_reports
dta_reports <- dta_reports[!duplicated(dta_reports[c("User.ID", "Milk.Center.ID", "Milk.Center", "Farmer_ID", "Farmer", "Alcohol.Test", "Qty", "Price", "Fat", "SNF", "Protein", "Added.Water", "Rejected", "Corrected.Lactometer.Reading", "Analogue.Lactometer.Test", "Analogue.Test.Done", "Milk.Analyzer.Test.Done", "delivery_date")]),]

write.csv(dta_reports, "/Users/B.VanCampenhout/data/projects/OneCG/RMVC/sample_submissions/latest_raw.csv",row.names = FALSE) 


dta_farmers <- read.csv("https://milk.ug/export/farmers")
dta_MCCs <-  read.csv("https://milk.ug/export/milkcenter")


### remove duplicate records from dta_reports
dta_reports <- dta_reports[!duplicated(dta_reports[c("User.ID", "Milk.Center.ID", "Milk.Center", "Farmer_ID", "Farmer", "Alcohol.Test", "Qty", "Price", "Fat", "SNF", "Protein", "Added.Water", "Rejected", "Corrected.Lactometer.Reading", "Analogue.Lactometer.Test", "Analogue.Test.Done", "Sale.Type","Milk.Analyzer.Test.Done", "delivery_date")]),]

#cat(paste(shQuote(names(dta_reports), type="cmd"), collapse=", "))

###merge in sample list
sample <- read.csv("/Users/B.VanCampenhout/data/projects/OneCG/RMVC/sample_submissions/list_endline_MCC.csv")

dta_MCCs <- merge(dta_MCCs, sample, by.x="manager", by.y="manager") 

### merge to farmer data

dta_farmers <- merge(dta_farmers,dta_MCCs[c("Milk.Center.ID","MCC_ID")],by="Milk.Center.ID")

### merge to reports data
dta_reports <- merge(dta_reports, dta_farmers, by = "Farmer_ID")

### this uniquely identifies based on "MCC_ID", "Farmer_ID", "sample_ID", 
#keep only relevant variable

dta_reports <- dta_reports[c("MCC_ID", "Farmer_ID", "sample_ID", "Alcohol.Test","Qty","Price","Fat","SNF","Protein","Added.Water","Analogue.Lactometer.Test", "Rejected","Corrected.Lactometer.Reading" ,"Analogue.Test.Done","Milk.Analyzer.Test.Done","Sale.Type","Report.Date","Created.Date")]

### generate farmer IDs within MCCs and sample_IDs within farmers

### create IDs for district, TA and village (q1, q2, q3)

i_mcc <- 1
dta_reports$mcc_new <- NA
dta_reports$farmer_new <- NA
dta_reports$sample_new <- NA

dta_reports$Farmer_ID <- as.character(dta_reports$Farmer_ID)
dta_reports$sample_ID <- as.character(dta_reports$sample_ID)
dta_reports$MCC_ID <- as.character(dta_reports$MCC_ID)

for (mcc in names(table(dta_reports$MCC_ID))) {
  print(mcc)
  i_farmer <- 1
  for (farmer in names(table(dta_reports$Farmer_ID[dta_reports$MCC_ID==mcc]))) {
    print(farmer)
    i_sample <- 1
    for (sample in names(table(dta_reports$sample_ID[dta_reports$MCC_ID == mcc & dta_reports$Farmer_ID == farmer]))) {
      print(sample)
      dta_reports$sample_new[dta_reports$MCC_ID == mcc & dta_reports$Farmer_ID == farmer & dta_reports$sample_ID == sample] <- i_sample
      i_sample <- i_sample + 1
    }
    dta_reports$farmer_new[dta_reports$MCC_ID == mcc & dta_reports$Farmer_ID == farmer] <- i_farmer
    i_farmer <- i_farmer + 1
  }
  dta_reports$mcc_new[dta_reports$MCC_ID == mcc] <- i_mcc
  i_mcc <- i_mcc + 1
}

dta_reports$farmer_new <- paste("F",dta_reports$farmer_new, sep="_")
dta_reports$sample_new <- paste("S",dta_reports$sample_new, sep="_")

dta_reports$mcc_new <- NULL
dta_reports$farmer_ID <- dta_reports$farmer_new
dta_reports$sample_ID <- dta_reports$sample_new
dta_reports$farmer_new <- NULL
dta_reports$sample_new <- NULL
dta_reports$Farmer_ID <- NULL


dta_reports <- dta_reports [c("MCC_ID","farmer_ID" ,"sample_ID",   "Alcohol.Test","Qty","Price","Fat","SNF","Protein","Added.Water","Analogue.Lactometer.Test", "Rejected","Corrected.Lactometer.Reading" ,"Analogue.Test.Done","Milk.Analyzer.Test.Done","Sale.Type","Report.Date","Created.Date")]


dta_reports$date <- as.Date(dta_reports$Report.Date)

dta_reports$time <- format(as.POSIXct(dta_reports$Report.Date), format = "%H:%M:%S")

dta_reports$Report.Date <- NULL
dta_reports$Create.Date <- NULL
dta_reports$Qty <- as.numeric(as.character(dta_reports$Qty))

dta_reports$Fat <- as.numeric(as.character(dta_reports$Fat))
dta_reports$SNF <- as.numeric(as.character(dta_reports$SNF))
dta_reports$Added.Water <- as.numeric(as.character(dta_reports$Added.Water))

dta_reports$Added.Water[dta_reports$Added.Water>20] <- NA 
dta_reports$Added.Water[dta_reports$Added.Water==0.01] <- 1 
dta_reports$Added.Water[dta_reports$Added.Water==0.02] <- 2 
dta_reports$Added.Water[dta_reports$Added.Water==0.03] <- 3 
dta_reports$Added.Water[dta_reports$Added.Water==0.1] <- 10 
dta_reports$Added.Water[dta_reports$Added.Water==0.11] <- 11
dta_reports$Added.Water[dta_reports$Added.Water==0.12] <- 12 
dta_reports$Added.Water[dta_reports$Added.Water==0.13] <- 13 
dta_reports$Added.Water[dta_reports$Added.Water==0.14] <- 14 
dta_reports$Added.Water[dta_reports$Added.Water==1.1] <- 11 
dta_reports$Added.Water[dta_reports$Added.Water==1.2] <- 12 
dta_reports$Added.Water[dta_reports$Added.Water==1.3] <- 13
dta_reports$Added.Water[dta_reports$Added.Water==1.6] <- 16
dta_reports$Added.Water[dta_reports$Added.Water==1.8] <- 18
dta_reports$Added.Water[dta_reports$Added.Water==1.9] <- 19 
dta_reports$Added.Water[dta_reports$Added.Water==0.2] <- 20 
dta_reports$Added.Water[dta_reports$Added.Water==0.3] <- NA
dta_reports$Added.Water[dta_reports$Added.Water==0.6] <- NA
dta_reports$Added.Water[dta_reports$Added.Water==0.9] <- NA

dta_reports$Added.Water[dta_reports$Added.Water>1 & dta_reports$Added.Water<2] <- NA
dta_reports$Added.Water[dta_reports$Added.Water>2 & dta_reports$Added.Water<3] <- NA
dta_reports$Added.Water[dta_reports$Added.Water>3 & dta_reports$Added.Water<4] <- NA
dta_reports$Added.Water[dta_reports$Added.Water>4 & dta_reports$Added.Water<5] <- NA
dta_reports$Added.Water[dta_reports$Added.Water>5 & dta_reports$Added.Water<6] <- NA
dta_reports$Added.Water[dta_reports$Added.Water>8 & dta_reports$Added.Water<9] <- NA
dta_reports$Added.Water[dta_reports$Added.Water>9 & dta_reports$Added.Water<10] <- NA


dta_reports$Price <- as.numeric(as.character(dta_reports$Price))
dta_reports$Protein <- as.numeric(as.character(dta_reports$Protein))
dta_reports$Corrected.Lactometer.Reading <- as.numeric(as.character(dta_reports$Corrected.Lactometer.Reading))
dta_reports$Analogue.Lactometer.Test <- as.numeric(as.character(dta_reports$Analogue.Lactometer.Test))

# load the library
library(forcats)
library(ggplot2)
# Reorder following the value of another column:
# dta_reports$MCC_ID <- na.omit(dta_reports$MCC_ID)
# ggplot(data=dta_reports[!is.na(dta_reports$Fat),], aes(x=reorder(MCC_ID, Fat), y=Fat )) +
#   geom_bar( stat = "summary", fill="#f68060", alpha=.6, width=.4) +
#   coord_flip() +
#   xlab("") +
#   theme_bw()
write.csv(dta_reports, "/Users/B.VanCampenhout/data/projects/OneCG/RMVC/sample_submissions/dta_reports.csv",row.names = FALSE) 
### additional layer of anonymization for public release
dta_reports$MCC_ID <- as.factor(dta_reports$MCC_ID)
levels(dta_reports$MCC_ID) <- paste("MA", 1:length(levels(dta_reports$MCC_ID)),sep="_")
write.csv(dta_reports, "/Users/B.VanCampenhout/data/projects/OneCG/RMVC/sample_submissions/dta_reports_masked.csv",row.names = FALSE) 

#ggplot(data=dta_reports, aes(x=date, y=Qty, group=MCC_ID, color=MCC_ID)) + stat_summary(fun = sum, geom = "line")
