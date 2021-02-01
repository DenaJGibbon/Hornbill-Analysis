# R Code for exploratory analysis of hornbill calls in Danum Valley

# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(glmmTMB)
library(bbmle)
library(sjPlot)

# List files in directory
HornbillList <- list.files('hornbilldata_all',
                           full.names = T)

# Create an empty dataframe to fill 
AllHornBillData <- data.frame()

# Loop to read in data and combine into a single dataframe
for(i in 1:length(HornbillList)){

  # Read in data
  HornbillData <-   read_xlsx(HornbillList[i],col_types = c("text", "numeric", "text", 
                                                            "date","date",
                                                            'text',"date"))

  # Convert Date column so that R can read it
  HornbillData$Date <-  as.Date(as.character(HornbillData$Date), format = "%Y%m%d")


  # Convert times so that R can read them 
  HornbillData <-  HornbillData %>% 
             mutate(`Start.time` = format(`Start.time`, "%H:%M:%S"),
                 `End.time` = format(`End.time`, "%H:%M:%S"),
                 `Duration` = format(`Duration`, "%H:%M:%S"))
  
  AllHornBillData <- rbind.data.frame(AllHornBillData,HornbillData)
}

# Check structure of data
head(AllHornBillData)

# Remove NA or  missing values
AllHornBillDataNArm <- na.omit(AllHornBillData)

# Create a new column that has only the hour in which calls were detected
AllHornBillDataNArm$Hour = str_split_fixed(AllHornBillDataNArm$Start.time,
                                       pattern = ':',n=3)[,1]

table(AllHornBillData$Recorder,AllHornBillData$Date)
# Reorder the hours for plotting
AllHornBillDataNArm <- AllHornBillDataNArm[order(AllHornBillDataNArm$Hour),]

AllHornBillDataNArm$Hour <- paste(AllHornBillDataNArm$Hour,rep(':00',nrow(AllHornBillDataNArm)),sep='')

# Assign the hornbills more plot-friendly names
AllHornBillDataNArm$Call.type <- plyr::revalue(AllHornBillDataNArm$Call.type,
              c(hornbill.rhino='Rhinoceros hornbill',
                hornbill.helmeted='Helmeted hornbill'))

# Plot the data by hornbill species
ggpubr::gghistogram(data=AllHornBillDataNArm,
                    x='Hour',stat='count',facet.by  ='Call.type', palette = matlab::jet.colors(2),
                    fill='Call.type',font.x = c(14, "bold"),
                    font.y = c(14, "bold"))+ xlab('Time')+
  ylab('Number of calls')+ guides(fill=F) + rotate_x_text()


# Plot the data by recorder
ggpubr::gghistogram(data=AllHornBillDataNArm,
                    x='Hour',stat='count',facet.by  ='Recorder',font.x = c(14, "bold"),
                    font.y = c(14, "bold"),
                    fill='Call.type',palette = (matlab::jet.colors(2)), ncol=3,
                    position = 'dodge')+ xlab('Time')+
  ylab('Number of calls')+ theme(legend.title = element_blank()) + rotate_x_text()

# Convert call duration into seconds
MinutestoSeconds <- as.numeric(str_split_fixed(AllHornBillDataNArm$Duration,pattern = ':',n=3)[,2])*60
Seconds <- as.numeric(str_split_fixed(AllHornBillDataNArm$Duration,pattern = ':',n=3)[,3] ) 

AllHornBillDataNArm$CallDur <- MinutestoSeconds+Seconds

# NOTE: Please check these as it appears there are some errors
AllHornBillDataNArm[AllHornBillDataNArm$CallDur > 500,]

# For now remove outliers
AllHornBillDataNArm <- droplevels(subset(AllHornBillDataNArm,CallDur < 500 &CallDur > 0 ))

# Create a series of GLMMS to test for differences in duration of calls
HornbillDurationModelNull <- glmmTMB(CallDur ~  (1|Recorder/Date),
                                 data=AllHornBillDataNArm,family="gaussian")

HornbillDurationModel <- glmmTMB(CallDur ~ Call.type+ (1|Recorder/Date),
                                  data=AllHornBillDataNArm,family="gaussian")

# Model selection using AIC shows that there are differences between call types in duration
AICctab(HornbillDurationModelNull,HornbillDurationModel,weights=T)
summary(HornbillDurationModel)


df_stats <-
  AllHornBillDataNArm %>% 
  group_by(Call.type) %>% 
  summarize(
    mean = mean(CallDur), 
    median = median(CallDur),
    sd= sd(CallDur)
  )

# Plot call duration by species
ggpubr::gghistogram(data=AllHornBillDataNArm, x= 'CallDur', stat="count", color= 'Call.type',  
                    palette = matlab::jet.colors (2),
                    fill = 'Call.type',font.x = c(14, "bold"),
                    font.y = c(14, "bold"))+ xlab('Call duration (s)')+ ylab('Frequency')+ 
                        theme(legend.title = element_blank()) 

# Create an index of recorders for subsetting
rec.index <- unique(AllHornBillData$Recorder)
AllHornBillData$Hour <- str_split_fixed(AllHornBillData$Start.time,pattern=':',n=2)[,1]

# Create a new dataframe with date, time, recorder and number of calls by call type
HornbillDetectionDF <- data.frame()

for(l in 1:length(rec.index)){
  which.rec <- as.character((rec.index[l]))
  small.df.sub <- subset(AllHornBillData, Recorder == which.rec)
  #smalldf.test <- subset(small.df.sub, Date == '2018-02-15')
  small.df.2 <- plyr::count(small.df.sub, c('Date',"Recorder","Call.type",'Hour'))
  colnames(small.df.2) <- c("date", "recorder", "call.type","hour","freq")
  HornbillDetectionDF <- rbind.data.frame(HornbillDetectionDF ,small.df.2)
}

# Revalue call types with 'NA' so that frequency value equals zero
HornbillDetectionDF <- 
  HornbillDetectionDF %>% mutate(freq = ifelse(call.type == "none" & HornbillDetectionDF$freq == 1, "0", freq))

# Convert frequency value to numeric
HornbillDetectionDF$freq <- as.numeric(HornbillDetectionDF$freq)

# Check output; there should now be zeroes which indicate there was not a call during that date/time
table(HornbillDetectionDF$freq)

# Subset so that only have bins where there were hornbill calls
HornbillDetectionDFCallsOnly <- 
  droplevels(subset(HornbillDetectionDF, freq > 0))

# Rename call types for plotting
HornbillDetectionDFCallsOnly$call.type <- plyr::revalue(HornbillDetectionDFCallsOnly$call.type,
                                               c(hornbill.rhino='Rhinoceros \n hornbill',
                                                 hornbill.helmeted='Helmeted \n hornbill'))

HornbillDetectionDFCallsOnly$call.type <- as.factor(HornbillDetectionDFCallsOnly$call.type)
HornbillDetectionDFCallsOnly$hour <- as.factor(HornbillDetectionDFCallsOnly$hour)

# Were there differences in recorders?
HornbillNcallModelNull <- glmmTMB(freq ~ recorder+  (1|date),
                                  data=HornbillDetectionDFCallsOnly,family="nbinom1")

HornbillNcallModelNullNoRecord <- glmmTMB(freq ~  (1|date),
                                  data=HornbillDetectionDFCallsOnly,family="nbinom1")

AICctab(HornbillNcallModelNull,HornbillNcallModelNullNoRecord)


sjPlot::plot_model(HornbillNcallModelNull, title='   ', type='eff')

# Create models to see if there are differences in detections
HornbillNcallModelNull <- glmmTMB(freq ~  (1|recorder/date),
                                     data=HornbillDetectionDFCallsOnly,family="nbinom1")

HornbillNcallModel <- glmmTMB(freq ~  call.type+ (1|recorder/date),
                              data=HornbillDetectionDFCallsOnly,family="nbinom1")

HornbillNcallModelAll.ARonly <- glmmTMB(freq ~  (1|recorder/date) + ar1(hour + 0|recorder),
                                 data=HornbillDetectionDFCallsOnly,family="nbinom1")

HornbillNcallModelAll.ARcalltype <- glmmTMB(freq ~call.type+  (1|recorder/date) + ar1(hour + 0|recorder),
                                        data=HornbillDetectionDFCallsOnly,family="nbinom1")

HornbillNcallModelAll <- glmmTMB(freq ~  call.type * hour + (1|recorder/date) + ar1(hour + 0|recorder),
                                   data=HornbillDetectionDFCallsOnly,family="nbinom1")

HornbillNcallModel.timeonly <- glmmTMB(freq ~  call.type + hour + (1|recorder/date),# + ar1(hour + 0|recorder),
                                 data=HornbillDetectionDFCallsOnly,family="nbinom1")


# Model selection; the top model includes call type 
AICctab(HornbillNcallModelNull,HornbillNcallModel,HornbillNcallModelAll.ARonly,
        HornbillNcallModelAll.ARcalltype,HornbillNcallModelAll,HornbillNcallModel.timeonly)

# Check model output; Rhino hornbill less likely to be detected
summary(HornbillNcallModelAll.ARcalltype)

# Plot model results

set_theme(base = theme_bw(base_size = 18))
# Create estimates showing differences in calling between two species
HornbillCoefPlot <- sjPlot::plot_model(HornbillNcallModel,type="pred",
                   title='',
                   axis.title = c('Species','Mean number of calls per hour'))

# Look at random effects structure; no recorder-level differences
HornbillRanefPlot <- sjPlot::plot_model(HornbillNcallModel,
                                        title='   ',
                   type='re')

HornbillRanefPlot[[2]] <- HornbillRanefPlot[[2]] + ylab('Recorder-level conditional modes')



ggpubr::ggarrange(HornbillCoefPlot$call.type,HornbillRanefPlot[[2]],
                  ncol=2,labels = c("A.","B."), label.x = 0.9,
                  font.label = list(size = 18, color = "black", face = "bold"))


libr