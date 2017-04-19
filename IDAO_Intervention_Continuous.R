## Individualized Data Analysis Organization
## 
## Analysis Code for Continuous Time Series Variable with intervention at Time T
## 
## -This code contains various method for analyzing for a statistical impact of an intervention on a 
## continuous time series variable
##*******************************************************************************************
## Load example dataset -- Activity data from Fitbit device (loaded from dashboard on Fitbit website)
activity <- read.csv("2016_activity.csv", as.is = TRUE)

# Remove commas from Activity
activity$Steps <- gsub(",", "", activity$Steps)
activity$Calories.Burned <- gsub(",", "", activity$Calories.Burned)
activity$Activity.Calories <- gsub(",", "", activity$Activity.Calories)

## Convert to appropriate formats
activity$Date <- as.Date(activity$Date)
activity$Calories.Burned <- as.integer(activity$Calories.Burned)
activity$Steps <- as.integer(activity$Steps)
activity$Minutes.Sedentary <- as.integer(activity$Minutes.Sedentary)
activity$Activity.Calories <- as.integer(activity$Activity.Calories)

str(activity) # Check to make sure no characters

# Remove technical outliers where not wearing
# Create indicator based on time recorded as some activity (note max minutes/day = 1440)
activity$timetotal <- activity$Minutes.Sedentary + activity$Minutes.Lightly.Active + activity$Minutes.Fairly.Active + activity$Minutes.Very.Active
# Remove outliers with missing or with less than 20% of total day
activity$percactive <- activity$timetotal/(1440)
activity <- subset(activity, activity$percactive > 0.2)
                   
## Graph Steps and Date as example
plot(activity$Date, activity$Steps, type="c", col= "black", 
     main= "Steps per day", cex.main=2, #Size of Main font (cex)
     xlab="Date",
     ylab= "Steps",
     cex.lab=1.4) #Size of label
lines(lowess(activity$Date, activity$Steps, f=1/6), col="green", lwd=2)
segments(min(activity$Date), mean(activity$Steps), max(activity$Date), mean(activity$Steps), col="red", lwd=2)
abline((linear=lm(activity$Steps~activity$Date)), col="blue", lwd=2)
legend(min(activity$Date), max(activity$Steps), #Location
       c("Lowess Smooth", paste("Mean (=", round(mean(activity$Steps),2)," units/day)"), paste("Linear Fit (Slope=", round(linear$coefficients[2], 1), "units/day)")), #Text
       col=c("green", "red", "blue"), #Line colors
       lty=c(1, 1, 1), #Line types
       lwd=c(2.5, 2.5, 2.5), #Line thickness
       bty="n", #No border ("o" if border)
       cex=1.3, #Text size
       y.intersp=0.85 #Spacing between text/lines
) 


##*******************************************************************************************
#ANALYTICAL APPROACHES: Intervention analysis on continuous data

# 1) Basic: Compare average in outcome before and after intervention at time T
#  - In this example, intervention at median date 4/16/2016

# Create indicator variable for 'intervention'
activity$intervention = 0
activity$intervention <- replace(activity$intervention, activity$Date > "2016-04-16", 1)

# Run ttest on steps comparing before and after intervention
ttest <- t.test(subset(activity$Steps, activity$intervention == 0), subset(activity$Steps, activity$intervention == 1))
ttest

# Run two-sample Wilcoxon test
wilcoxon <- wilcox.test(subset(activity$Steps, activity$intervention == 0), subset(activity$Steps, activity$intervention == 1))
wilcoxon





