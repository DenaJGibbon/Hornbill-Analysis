# This is the R code for the analyses presented in the manuscript titled
# Lack of evidence for acoustic niche partitioning in two sympatric Bornean hornbill species

# Load libraries and data ------------------------------------

# Load required libraries 
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(glmmTMB)
library(lmerTest)
library(bbmle)
library(sjPlot)
library(emmeans)


# Read in the .csv file with the data
AllHornBillData <- read.csv('KennedyEtAlHornBillData.csv')

# Check the structure of the resulting data
head(AllHornBillData)

# Create a new column that has only the hour in which calls were detected
AllHornBillData$Hour = str_split_fixed(AllHornBillData$Start.time,
                                           pattern = ':',n=3)[,1]

# Analysis Part I Testing for differences in call duration ------------------------------------

# Prepare data by converting call duration into seconds
MinutestoSeconds <- as.numeric(str_split_fixed(AllHornBillData$Duration,pattern = ':',n=3)[,2])*60
Seconds <- as.numeric(str_split_fixed(AllHornBillData$Duration,pattern = ':',n=3)[,3] ) 

# Add a new column to the dataframe
AllHornBillData$CallDur <- MinutestoSeconds+Seconds

# Subset so have only call detections
AllHornBillDataDetectOnly <- droplevels(subset(AllHornBillData,CallDur > 0 ))

# Model selection with call duration as the outcome variable
# Null model
HornbillDurationModelNull <- glmmTMB(CallDur ~  (1|Recorder/Date),
                                     data=AllHornBillDataDetectOnly,family="gaussian")
# Full model
HornbillDurationModel <- glmmTMB(CallDur ~ Call.type+ (1|Recorder/Date),
                                 data=AllHornBillDataDetectOnly,family="gaussian")

# Model selection using AIC shows that there are differences between call types in duration
AICctab(HornbillDurationModelNull,HornbillDurationModel,weights=T) # Model with Call.type as predictor performs substantially better

# Use a KS test for differences between two distributions
Rhino.only <- subset(AllHornBillDataDetectOnly, Call.type =='hornbill.rhino')
Helmeted.only <- subset(AllHornBillDataDetectOnly, Call.type =='hornbill.helmeted')

# The test is significant so helmeted calls are of longer duration 
ks.test(Rhino.only$CallDur,Helmeted.only$CallDur)

# Figure 5 Total number of calling events for each hornbill species by hour ------------------------------------

# Assign the hornbills more plot-friendly names
AllHornBillDataDetectOnly$Call.type <- plyr::revalue(AllHornBillDataDetectOnly$Call.type,
                                                     c(hornbill.rhino='Rhinoceros hornbill',
                                                       hornbill.helmeted='Helmeted hornbill'))

# Reorder the hours for plotting
AllHornBillDataDetectOnly <- AllHornBillDataDetectOnly[order(AllHornBillDataDetectOnly$Hour),]

# Add ':00' to the hour for plotting
AllHornBillDataDetectOnly$Hour <- paste(AllHornBillDataDetectOnly$Hour,rep(':00',nrow(AllHornBillDataDetectOnly)),sep='')

# Create the plot
ggpubr::gghistogram(data=AllHornBillDataDetectOnly,
                    x='Hour',stat='count',facet.by  ='Call.type', palette = matlab::jet.colors(2),
                    fill='Call.type',font.x = c(14, "bold"),
                    font.y = c(14, "bold"))+ xlab('Time')+
  ylab('Number of calls')+ guides(fill=F) + rotate_x_text()

# Figure 6 Histogram of the number of calls of different duration (in seconds) by species. ------------------------------------
ggpubr::gghistogram(data=AllHornBillDataDetectOnly, x= 'CallDur', stat="count", color= 'Call.type',  
                    palette = matlab::jet.colors (2),
                    fill = 'Call.type',font.x = c(14, "bold"),
                    font.y = c(14, "bold"))+ xlab('Call duration (s)')+ ylab('Frequency')+ 
  theme(legend.title = element_blank()) 

# Figure 7 Number of helmeted and rhinoceros hornbill calling events by hour (local time) and recorder. ------------------------------------
ggpubr::gghistogram(data=AllHornBillDataDetectOnly,
                    x='Hour',stat='count',facet.by  ='Recorder',font.x = c(14, "bold"),
                    font.y = c(14, "bold"),
                    fill='Call.type',palette = (matlab::jet.colors(2)), ncol=3,
                    position = 'dodge')+ xlab('Time')+
  ylab('Number of calls')+ theme(legend.title = element_blank()) + rotate_x_text()

# Analysis Part II Testing for acoustic niche partitioning ------------------------------------

# Remove NA instances from data set 
AllHornBillData <- na.omit(AllHornBillData)

# Rename call types with more plot-friendly names
AllHornBillData$Call.type <- plyr::revalue(AllHornBillData$Call.type,
                                                     c(hornbill.rhino='Rhinoceros',
                                                       hornbill.helmeted='Helmeted'))

AllHornBillData$Hour <- as.factor(AllHornBillData$Hour)

# Then aggregate across dates and recorders
HornbillDataAggregateByDate <- data.frame(table(Call.type = AllHornBillData$Call.type,
                                Hour = AllHornBillData$Hour,
                                Recorder = AllHornBillData$Recorder))
# Check the resulting output
hist(HornbillDataAggregateByDate$Freq)

# Each hour and recorder combination has one entry for each species
table(HornbillDataAggregateByDate$Hour,HornbillDataAggregateByDate$Recorder) 

# Model selection
## The first two models are testing for spatial variation in detections
Hour.mod.nested.recorder.null <- 
  glmmTMB(Freq ~  (1|Recorder), family = "nbinom2", data=HornbillDataAggregateByDate)

Hour.mod.nested.recorder.null.nest <- # This is a nested random effect
  glmmTMB(Freq ~  (1|Recorder/Call.type), family = "nbinom2", data=HornbillDataAggregateByDate)

# First we compare the two models using AIC
AICctab(Hour.mod.nested.recorder.null,Hour.mod.nested.recorder.null.nest)
# Take away- we want both (nested) random effects

## The second two models are testing for spatial variation
Hour.mod.interaction.recorder.nointer <- 
  glmmTMB(Freq ~ Hour + Call.type + (1|Recorder/Call.type), family = "nbinom2", data=HornbillDataAggregateByDate)

Hour.mod.interaction.recorder.renest <- 
  glmmTMB(Freq ~ Hour*Call.type + (1|Recorder/Call.type), family = "nbinom2", data=HornbillDataAggregateByDate)

# Now we compare the models using AIC
AICctab(Hour.mod.nested.recorder.null,Hour.mod.nested.recorder.null.nest,
        Hour.mod.interaction.recorder.nointer,
        Hour.mod.interaction.recorder.renest,weights=T,
        base=T)

# Figure 8 Random effect plots ------------------------------------
# Code to extract relevant data for the BLUP plot
rr <- ranef(Hour.mod.interaction.recorder.nointer)

# Convert to dataframe
rr.df <- as.data.frame(rr)

# Pull out the nested terms
rr.df <- droplevels(rr.df[c(1:20),])

# Add a call.type column
rr.df$call.type <- str_split_fixed(rr.df$grp,pattern = ':',n=2)[,1]

# Change the order of the factor levels
rr.df$grp <- factor(rr.df$grp, levels = dput(sort(levels(rr.df$grp))))

# Reorder so species grouped by recorder
rr.df$grp <- factor(rr.df$grp, 
                    levels = dput(levels(rr.df$grp)[c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10,20)])
)

rr.df$grp

# Figure 8a Best linear unbiased prediction (BLUP) plot for the nested random effects 
Figure8a <- ggplot(rr.df, aes(x=grp, y=condval, colour=call.type,shape=call.type)) + 
 scale_colour_manual(values=c('red',"black"))+
  geom_errorbar(aes(ymin=condval-condsd, ymax=condval+condsd), width=.1) +
  geom_line() + coord_flip()+ geom_hline(yintercept = 0,size=0.5)+
  geom_point()+theme_bw()+xlab('Call type:Recorder')+
  ylab('Best Linear Unbiased Prediction') + ylim(-3.5,3.5)+
  guides(colour=guide_legend(title="Hornbill Species",face='bold'),
         shape=guide_legend(title="Hornbill Species",face='bold'))

# Create new dataframe
rr.df.recorder <- as.data.frame(rr)

# Pull out the recorder only random effects
rr.df.recorder <- droplevels(rr.df.recorder[-c(1:20),])

# Add column for call type
rr.df.recorder$call.type <- str_split_fixed(rr.df.recorder$grp,pattern = ':',n=2)[,1]

# Change the order of the factor levels
rr.df.recorder$grp <- factor(rr.df.recorder$grp, levels = dput(sort(levels(rr.df.recorder$grp))))

# Check to make sure in order
rr.df.recorder$grp


# Figure 8b Best linear unbiased prediction (BLUP) plot for recorder random effects 
Figure8b <- ggplot(rr.df.recorder, aes(x=grp, y=condval, fill='black')) + 
  # geom_point(size=2.5)+
  # scale_colour_manual(values=matlab::jet.colors(10))+
  # scale_shape_manual(values=c(seq(1,10,1)))+
  geom_errorbar(aes(ymin=condval-condsd, ymax=condval+condsd), width=.1) +
  geom_line() + coord_flip()+ geom_hline(yintercept = 0,size=0.5)+
  geom_point()+theme_bw()+xlab('Recorder')+
  ylab('Best Linear Unbiased Prediction') + ylim(-3.5,3.5)+theme(legend.position = "none")

# Combine RE plots for the manuscript
cowplot::plot_grid(Figure8a,NULL,Figure8b,NULL,
                   labels = c('A','','B',''), nrow=1,
                   label_x = c(.75,0,1,0), rel_widths = c(0.55,.05,0.35,0.05)
                   )

# Figure 9 Estimated marginal means plot ------------------------------------

# Calculate emm means for 
emm1.1 <- 
  emmeans(Hour.mod.interaction.recorder.renest, pairwise ~ 
            Call.type|Hour,type='response')

# Pull out the relevant information
EmMeansPlotDF <- plot((emm1.1),comparisons=T,plotit=F)

# Make add ':00' to the hour to make it more plot friendly
EmMeansPlotDF$Hour <- str_pad(EmMeansPlotDF$Hour,2,pad='0',side = c("left"))
EmMeansPlotDF$Hour <- paste(EmMeansPlotDF$Hour,':00',sep='')

# Set pd so that the two species do not overlap 
pd <- position_dodge(width = 0.5)

# Plot the results
ggplot(data=EmMeansPlotDF,aes(x=the.emmean, y=Hour, color=Call.type,shape=Call.type))+
  geom_point(position = pd)+ theme_bw()+
  scale_color_manual(values=c('red','black'))+
  geom_errorbar(aes(xmin=lower.CL, xmax=upper.CL), width=.1,position = pd)+
  xlab('Estimated marginal means')+ylab('Local Time')+ 
  labs(color = "Hornbill Species",shape='Hornbill Species')





