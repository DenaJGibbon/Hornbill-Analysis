library(tidyr)
library(dplyr)
library(flextable)

# Part 1. Table summarizing features ------------------------------------


HornbillTableCount <- 
  AllHornBillData %>% count(Recorder, Call.type, sort=TRUE)

HornbillTableCount_wide <- spread(HornbillTableCount, Recorder,  n)

HornbillTableCount_wide <- as.data.frame(t(HornbillTableCount_wide))

Recorder <- rownames(HornbillTableCount_wide)

HornbillTableSum <- cbind.data.frame(Recorder,HornbillTableCount_wide)

colnames(HornbillTableSum) <- c('Recorder','Helmeted hornbill', 'Rhinoceros hornbill')

HornbillTableSum <- HornbillTableSum[-1,]

HelmetedTotal <- sum(as.numeric(HornbillTableSum$Helmeted))
RhinoTotal <- sum(as.numeric(HornbillTableSum$Rhinoceros))
Total <- 'Total calls'

TotalRow <- cbind.data.frame(Total,HelmetedTotal,RhinoTotal)

colnames(TotalRow) <- colnames(HornbillTableSum)
HornbillTableSum <- rbind.data.frame(HornbillTableSum,TotalRow)

myft <- flextable(
  (HornbillTableSum))
myft <- width(myft, width = 1)
myft <- bold(myft, part = "header") 
myft <- bold(myft, i = nrow(HornbillTableSum))
myft

save_as_docx(myft,path='Hornbill Call Summary.docx')
