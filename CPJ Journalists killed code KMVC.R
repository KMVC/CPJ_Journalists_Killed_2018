#load data and relevant libraries
library(tidyverse)
CPJ <- read.csv("CPJ Data.csv", na = c ( "", "NA"))

#removing variables containing little/no data
CPJ <- select(
  CPJ,
  year,
  fullName,
  primaryNationality, 
  gender,
  typeOfDeath, 
  employedAs, 
  organizations, 
  jobs,
  coverage, 
  mediums,
  country,
  location,
  locality,
  localOrForeign,
  sourcesOfFire, 
  motiveConfirmed, 
  tortured, 
  captive,
  threatened
)  
                           
#all deaths, by year, since 1992
ggplot(CPJ, aes(x = year)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust=0)) +
  geom_text(aes(label=..count..), stat="count", position=position_stack(0.7)) +
  ggtitle("All deaths, 1992-2018, by year")

#sources of fire
ggplot(CPJ, aes(x = sourcesOfFire)) + 
  geom_bar(fill = "pink") + 
  theme(axis.text.x = element_text(angle = 90, hjust=0)) + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.7)) + 
  ggtitle("Sources of Fire, 1992-2018")


#filter the data so that we are only looking at data from the last ten years
CPJ1 <- filter(CPJ, CPJ$year >= 2008)

#plot of number of deaths in the last ten years by type of death
ggplot(CPJ1, aes(x = year, fill= typeOfDeath)) + 
  geom_bar() + 
  ggtitle("Number of deaths, by type of death, 2008-2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

                  
#bar plot for overall number of deaths in the last ten years by type of employment
ggplot(CPJ1, aes(x = employedAs)) +
  geom_bar (fill = "darkgreen") + 
  ggtitle("Overall Number of Staff Vs. Freelancers Killed, 2008 to 2018") + 
  geom_text(aes(label=..count..), stat="count", position=position_stack(0.5))


#bar plot for number of deaths in the last ten years and type of employment
ggplot(CPJ1, aes(x = year, fill = employedAs)) + 
  geom_histogram(position = "dodge") + 
  ggtitle("Deaths by type of employment 2008-2018")+ 
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.5))


#bar plot of overall number of deaths by gender from 2008-2018
ggplot(CPJ1, aes(x = year, fill = gender)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Number of deaths by gender, 2008-2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.75))


#plot of how many tortured/not tortured from 2008-2018
ggplot(CPJ1, aes(x = year, fill = tortured)) + 
  geom_bar() + 
  ggtitle("Of journalists killed - also tortured? 2008-2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#plot showing number tortured and/or captured, 2008-2018
ggplot(CPJ1, aes(x = tortured, fill = captive )) + 
  geom_bar() + 
  ggtitle("Overall tortured and captive (of those killed), 2008-2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#how many threathened from 2008-2018?
ggplot(CPJ1, aes(x = threatened)) + 
  geom_bar(fill = "black") + 
  ggtitle("Threatened (of all killed), 2008-2018")


#threats over the years
ggplot(CPJ1, aes(x = year, fill = threatened)) + 
  geom_bar() + 
  ggtitle("Threatened (of those killed) by year, 2008-2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

#Examining 2018
CPJ2 <- filter(CPJ, CPJ$year == 2018)

#renaming Central African Republic and Israel and Occupied Palestine for better chart readability
levels(CPJ2$country) <- c(levels(CPJ2$country), "Israel&Palestine", "CAR") 
levels(CPJ2$primaryNationality) <-  c(levels(CPJ2$primaryNationality), "Israel&Palestine", "CAR")
CPJ2$country[CPJ2$country == "Israel and the Occupied Palestinian Territory"]  <-  "Israel&Palestine" 
CPJ2$country[CPJ2$country == "Central African Republic"]  <-  "CAR" 
CPJ2$primaryNationality[CPJ2$primaryNationality == 
                          "Israel and the Occupied Palestinian Territory"] <-  "Israel&Palestine"
CPJ2$primaryNationality[CPJ2$primaryNationality == "Central African Republic"]  <-  "CAR"

# plot of number of deaths by country(location of death) in 2018
ggplot(CPJ2, aes(x = country)) + 
  geom_bar(position = "dodge", fill = "red") + 
  ggtitle("Number of deaths by country in 2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#plot of primary nationality of those killed in 2018 and the location of their deaths
ggplot(CPJ2, aes(x = primaryNationality, fill = country)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Primary nationality and country (of death) of those killed,2018")+ 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#torture in 2018
ggplot(CPJ2, aes(x = tortured)) + 
  geom_bar(fill = "magenta1") + 
  ggtitle("Of journalists killed - also tortured? 2018")+ 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#captivity in 2018
ggplot(CPJ2, aes(x = captive)) + 
  geom_bar(fill = "cyan2") + 
  ggtitle("Of journalists killed - also held captive?  2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#threats in 2018
ggplot(CPJ2, aes(x = threatened)) + 
  geom_bar(fill = "tan") + 
  ggtitle(" Of journalists killed -  also threatened? 2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#threats by nationality 2018
ggplot(CPJ2, aes(x = primaryNationality, fill = threatened)) + 
  geom_bar() + 
  ggtitle("Threatened by primary nationality of journalist, 2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#captive by nationality, 2018
ggplot(CPJ2, aes(x = primaryNationality, fill = captive)) + 
  geom_bar() + 
  ggtitle("Captive by primary nationality of journalist, 2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

#tortured by primary natioanlity, 2018
ggplot(CPJ2, aes(x = primaryNationality, fill = tortured)) + 
  geom_bar() + 
  ggtitle("Tortured by primary nationality of journalist, 2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#filter so data only contains murders from the last 10 years
CPJ3 <-  filter(CPJ1, typeOfDeath == "Murder")

#plot of murders in  since 2008 
ggplot(CPJ3, aes(x = year)) + 
  geom_bar(fill = "white") +
  ggtitle("Number of murders, 2008-2018") + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

#bar plot for number of murders in the last ten years and type of employment
ggplot(CPJ3, aes(x = year, fill = employedAs)) + 
  geom_bar() + 
  ggtitle("Number of murders by type of employment, 2008-2018") +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
                  
#bar plot for murders by gender
ggplot(CPJ3, aes(x = year, fill = gender)) + 
  geom_bar(stat = "count", show.legend = TRUE) + 
  ggtitle("Number of murders by gender, 2008-2018") +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
                  




