# ArsenicDV analysis 

library(tidyverse)
library(here)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

Arsenic<- read.csv(here("CSV_allAs.csv"))


#creating subsets

Control <- subset(Arsenic, Dose == "0")
ppb1 <- subset(Arsenic, Dose == "1")
ppb5 <- subset(Arsenic, Dose == "5")
ppb10 <- subset(Arsenic, Dose == "10")
ppb100 <- subset(Arsenic, Dose == "100")


#make histograms for outliers

ggplot()+ 
  geom_histogram(data=Control, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 
ggplot()+ 
  geom_histogram(data=ppb1, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 
ggplot()+ 
  geom_histogram(data=ppb5, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 
ggplot()+ 
  geom_histogram(data=ppb10, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 
ggplot()+ 
  geom_histogram(data=ppb100, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

# SORT it OUT

ArsenicSort1 <- Arsenic[order(Arsenic$Dose, -Arsenic$TD),]

# Remove those outliers (DO NOT TAKE FROM THE SORTED, NOTE YOU TAKE FROM ARSENIC, BY ROW NUMBER)

ArsenicOR1 <-(Arsenic[ -c(1239, 1235, 1250, 1240, 1242, 1238, 878, 879, 2244, 2238, 1400, 1388, 1389, 1463, 1420, 1656, 1715), ])

# Bye Bye Outliers, Recreate subsets

Control <- subset(ArsenicOR1, Dose == "0")
ppb1 <- subset(ArsenicOR1, Dose == "1")
ppb5 <- subset(ArsenicOR1, Dose == "5")
ppb10 <- subset(ArsenicOR1, Dose == "10")
ppb100 <- subset(ArsenicOR1, Dose == "100")


#  prep PLOT BABY PLOT

Control_meanTD <- Control %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
ppb1_meanTD <- ppb1 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
ppb5_meanTD <- ppb5 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
ppb10_meanTD <- ppb10 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
ppb100_meanTD <- ppb100 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

# Bring that shit together ROW

Everything_TD <- rbind(Control_meanTD, ppb1_meanTD, ppb5_meanTD, ppb10_meanTD, ppb100_meanTD)


# reassign it 

Dose = c(rep(0,25), rep(1,25), rep(5,25), rep(10,25), rep(100,25))

# Bring that shit together (again) COLUMN

Everything_TD <- cbind(Everything_TD, Dose)

# PLOT BABY PLOT

ggplot(data=Everything_TD, aes(x=Minute, y=mean_TD, group=as.factor(Dose), colour=as.factor(Dose))) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))+
  scale_color_brewer(palette  = "Dark2")
ggsave(here("Everything_TD.png"))
