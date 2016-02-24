setwd("/Users/njvijay/big_data/datascience_johnhopkins/getting_cleaning_data/Week1/Quiz1")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url=fileUrl,destfile="idaho_housing.csv",mode="w",method="curl")
list.files(".")

dateDownloaded <- date()
dateDownloaded


idaho_housing <- read.csv("idaho_housing.csv")
head(idaho_housing)

length(idaho_housing$VAL[!is.na(idaho_housing$VAL) & idaho_housing$VAL==24])


#XLS


setwd("/Users/njvijay/big_data/datascience_johnhopkins/getting_cleaning_data/Week1/Quiz1")
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(url=fileUrl1, destfile="gov_NGAP.xlsx", mode="w", method="curl")
dateDownloaded <- date()
print(dateDownloaded)


rowIndex <- 18:23
colIndx <- 7:15
dat <- read.xlsx(file="gov_NGAP.xlsx",sheetIndex=1,colIndex=colIndx,startRow=18, endRow=23, header=TRUE)
head(dat)


sum(dat$Zip*dat$Ext,na.rm=T)

#XML

library(XML)
fileUrl2 <- "http://d396qusza40orc.cloudfront.net/getdata/data/restaurants.xml"
doc <- xmlTreeParse(file=fileUrl2,useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)

names(rootNode)

zipcode <- xpathSApply(rootNode,"//zipcode",xmlValue)
length(zipcode[zipcode==21231])

#FREAD

library(data.table)
DT <- fread(input="fsspid.csv", sep=",")
system.time(DT[,mean(pwgtp15),by=SEX])
