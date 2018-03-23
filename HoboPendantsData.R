library(dplyr)
library(tidyr)
setwd("data/loggers")
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

allLoggers <- allLoggers %>% separate(date.time, c("Date", "time"), sep = 9) %>% select(-1) %>% separate(logger.name, c("plant.id", "treatment", sep = "-")) %>% select(Date:light.intensity, plant.id:treatment) %>% separate(plant.id, c("species", "id"), sep = 1)

#delete rows from first days and last days
#delete rows from last day
allLoggers <- allLoggers %>% filter(Date != "05/11/17") %>% filter(Date != "03/17/2017") %>% filter(Date != "03/18/2017")

#convert time from 12 to 24 hours
allLoggers$time <- format(strptime(allLoggers$time, "%I:%M:%S %p"), format="%H:%M:%S")

#EDA



