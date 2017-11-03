## Script for getting and cleaning data Essenburgh
## Author: Gerrit Versteeg
## Last saved: 30-10-2017
##
##
##-----------------------------------------------------------------------------
##---------------- PART 1. GETTING THE DATA -----------------------------------
##
##---- step 0. Loading relevant packages
library("tidyr")
library("dplyr")
##
##
##---- step 1: read and concatenate all relevant csv.files into tibbles ----
CCP_1 <- tbl_df(read.csv("./data/CCP_20171020_1.csv",
                         skip = 2, header = TRUE, colClasses = "character"))
CCP_2 <- tbl_df(read.csv("./data/CCP_20171020-2.csv",
                         skip = 2, header = TRUE, colClasses = "character"))
CCP <- rbind(CCP_1, CCP_2)
if (nrow(CCP_1) + nrow(CCP_2) == nrow(CCP)) {
        rm(CCP_1, CCP_2)
}
CCP <- CCP[-1,] ## remove first line, because it contains subquestion headers
##
PS_1 <- tbl_df(read.csv("./data/PS-20171020_1.csv",
                        skip = 2, header = TRUE, colClasses = "character"))
PS_2 <- tbl_df(read.csv("./data/PS_20171020_2.csv",
                        skip = 2, header = TRUE, colClasses = "character"))
PS <- rbind(PS_1, PS_2)
if (nrow(PS_1) + nrow(PS_2) == nrow(PS)) {
        rm(PS_1, PS_2)
}
##---- step 2: import Lookup-table clinics into tibble ----
LUT <- tbl_df(read.csv2("./data/LUT.csv",
                        skip = 3, header = TRUE, 
                        encoding = "UTF-8", colClasses = "character"))
ColIndex <- c(4,6,7,9,10)
NameVector <- c("country", "c_id", "c_name", "NoE", "NoP")
LUT <- LUT[,ColIndex]
colnames(LUT) <- NameVector
##


##-----------------------------------------------------------------------------
##----- PART 2. CLEANING UP THE DATA ------------------------------------------
##
##---- step 3: Delete all columns that are empty ----
##
x <- nrow(PS)
y <- c(1:ncol(PS))
ColVector <- numeric()
for (i in y) {
        if (sum(PS[,i] == "") == x) {
                ColVector <- c(ColVector,-i)
        }
}
PS_C <- PS[,ColVector]
x <- nrow(CCP)
y <- c(1:ncol(CCP))
ColVector <- numeric()
for (i in y) {
        if (sum(CCP[,i] == "") == x) {
                ColVector <- c(ColVector,-i)
        }
}
CCP_C <- CCP[,ColVector]
##
##---- step 4: Add processing column and Loop through data ----
##
CCP_C <- mutate(CCP_C, code="")
x <- nrow(CCP_C)
for (i in 1:x) {
        cid <- substring(as.character(CCP_C[i,]$clinic_id),1,4)
        LUR <- LUT[which(LUT$c_id == cid),]
        if (nrow(LUR) == 1) {
                CCP_C[i,]$country <- LUT[1,]$country
                CCP_C[i,]$clinic <- LUT[1,]$c_name
                CCP_C[i,]$code <- "processable"
        }
}

##
##-----------------------------------------------------------------------------
## End of script
##-----------------------------------------------------------------------------

