library(dplyr)
library(tidyr)
library(ggplot2)
setwd("F:/School/understory.climate.contrast/data/loggers")
temp <- list.files(pattern="*.csv")
name <- list.files(pattern="*.csv")
datalist <- list()
for (i in 1:length(temp)) {
  ob <- read.csv(temp[i], header = FALSE)    #read in csv
  ob <- ob[-1,] #delete weird first row
  #colnames(ob) <- as.character(unlist(ob[1,]))
  ob <- ob[-1,]

  ob$loggername <- name[i]
  datalist[[i]] <- ob
}

allLoggers <- bind_rows(datalist)
colnames(allLoggers) <- c("1", "date.time", "temp", "light.intensity", "coupler.detached", "coupler.attached", "host.connected", "stopped", "logger.name", "end.of.file")

#now for some wrangling
# split day and time column

 
allLoggers <- dplyr::select(allLoggers, -(11))  
allLoggers <- separate(allLoggers, date.time, c("Date", "time"), sep = 9) %>% dplyr::select(-1) %>% separate("logger.name", c("plant.id", "treatment", sep = "-")) %>% dplyr::select(Date:light.intensity, plant.id:treatment) %>% separate(plant.id, c("species", "id"), sep = 1)

#delete rows from first days and last days
#delete rows from last day

allLoggers$Date <- gsub(" ","", allLoggers$Date)
allLoggers <- filter(allLoggers, Date != "05/11/17" & Date != "03/17/17" & Date != "03/18/17") 
allLoggers <- filter(allLoggers, Date != "") 
                     
#convert time from 12 to 24 hours
allLoggers$time <- format(strptime(allLoggers$time, "%I:%M:%S %p"), format="%H:%M:%S")

allLoggers$hour <- substr(allLoggers$time,1,2)
allLoggers$temp <- as.numeric(allLoggers$temp)
#EDA

str(allLoggers)


#larrea only

larrea <- filter(allLoggers, species == "L")
dailymax <- larrea %>% group_by(treatment, Date, id) %>% summarise(maxTemp = max(temp))
ggplot(data = dailymax, aes(Date, maxTemp, color = treatment)) + geom_point() 


dailymin <- larrea %>% group_by(treatment, Date, id) %>% summarise(minTemp = min(temp))
ggplot(data = dailymin, aes(Date, minTemp, color = treatment)) + geom_point() 
shapiro.test(dailymax$maxTemp)
t.test(dailymax$maxTemp ~ dailymax$treatment)
t.test(dailymin$minTemp ~ dailymin$treatment)

larrea$temp <- as.numeric(larrea$temp)
larrea$hour <- as.numeric(larrea$hour)
daytime <- filter(larrea, hour >= 9 & hour<21)
daytime <- na.omit(daytime)
nighttime <- filter(larrea, hour >=21 | hour<9)
nighttime <-na.omit(nighttime)
all <- larrea %>% group_by(treatment, Date, id) %>% summarise(meanTemp = mean(temp), varTemp = sd(temp))


meanday <- daytime %>% group_by(treatment, Date, id) %>% summarise(meanTemp = mean(temp), varTemp = sd(temp))

t.test(meanday$meanTemp ~ meanday$treatment)

meannight<- nighttime %>% group_by(treatment, Date, id) %>% summarise(meanTemp = mean(temp), varTemp = sd(temp))
t.test(meannight$meanTemp ~ meannight$treatment)
str(larrea)

larrea$hour <- as.factor(larrea$hour)
#figure for all
ggplot(data = larrea, aes(hour, ((temp-32)*0.5556))) + geom_boxplot(aes(fill = treatment)) + theme_Publication() + xlab("Hour") + ylab("Temperature in °C") + scale_fill_manual(values=c("#AF955A", "darkgreen"), name = "Microsite")




meannight$treatment <- as.factor(meannight$treatment)
kruskal.test(meannight$meanTemp ~ meannight$treatment)

ggplot(meannight, aes(meanTemp, fill = treatment)) + geom_density()       
shapiro.test(meannight$meanTemp)
m1 <- glm(log(meannight$meanTemp)~meannight$treatment)
shapiro.test(resid(m1))
summary(m1)

library(lme4)
meannight$id <- as.factor(meannight$id)
m1 <- glmer(meanTemp ~ treatment + (1|id), family = Gamma(link = "log"), data = meannight)
shapiro.test(resid(m1))
summary(m1)
car::Anova(m1, type =2)

m2 <- glmer(meanTemp ~ treatment + (1|id), family = Gamma(link = "log"), data = meanday)
summary(m2)
car::Anova(m2, type =2)


shapiro.test(meanday$meanTemp)
t.test(meannight$varTemp ~ meannight$treatment)
shapiro.test(meannight$varTemp)
t.test(meanday$varTemp ~ meanday$treatment)

meanAll <- rbind(meanday, meannight)

m3 <- glmer(varTemp ~ treatment + (1|id), family = Gamma(link = "log"), data = all)
summary(m3)
car::Anova(m3, type = 2)
DailyVar