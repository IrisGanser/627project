library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

# load vaccination rate data 
vaccine <- read.csv("child vaccination rates measles DTP.csv", stringsAsFactors = FALSE)
names(vaccine)[1] <- "LOCATION"
vaccine$LOCATION <- tolower(vaccine$LOCATION) 
str(vaccine)

vaccine_measles <- filter(vaccine, SUBJECT == "MEASLES") %>% 
  filter(TIME == 2015 | TIME == 2018) %>%
  mutate(Vacrate = Value/100) %>%
  select(c(LOCATION, SUBJECT, TIME, Vacrate))
# no missing data for vaccination rate

# vaccine confidence data
vaccine_conf <- read_excel("vac_confidence.xlsx")

# join datasets
vaccine_join <- full_join(vaccine_measles, vaccine_conf, by = c("LOCATION" = "geo", "TIME" = "year"))
vaccine_join$LOCATION <- as.factor(vaccine_join$LOCATION)
levels(vaccine_join$LOCATION)
vaccine_join <- vaccine_join %>% rename(Country = name)
vaccine_join$Country <- as.factor(vaccine_join$Country)

# delete the rows with completely missing OECD measles vaccine data
vaccine_join <- filter(vaccine_join, !is.na(SUBJECT))

# add country names in years missing vaccine information
vaccine_join <- vaccine_join %>% group_by(LOCATION) %>%
  mutate(Country = unique(Country[!is.na(Country)]))

# look at how much data is missing for vaccine opinions and in which countries
sum(is.na(vaccine_join$Vac_importance_agree))
vaccine_join$LOCATION[is.na(vaccine_join$Vac_importance_agree)]

# plot of vaccination rates
ggplot(vaccine_measles, aes(x = Vacrate)) +
  geom_histogram(bins = 20)+
  facet_wrap(facets = ~TIME) +
  labs(title = "Histogram of Measles vaccination rates in 44 OECD countries")


# look at GDP dataset
GDP <- read.csv("HEALTH_ECOR.csv")
GDP$Measure <- as.factor(GDP$Measure)
GDP <- GDP %>% filter(Measure == "/capita, US$ purchasing power parity") %>%
  select(-c(1:5, 7, 10:11)) %>% rename(GDP = Value)
GDP <- filter(GDP, Year %in% c(2015, 2018))
# missing 2018 data for Brazil and India

GDP$Country <- recode(GDP$Country, "China (People's Republic of)" = "China")
GDP$Country <- recode(GDP$Country, "Korea" = "South Korea")

df <- left_join(vaccine_join, GDP, by = c("Country" = "Country", "TIME" = "Year"))

# look at doctors visits dataset
visits <- read.csv("HEALTH_PROC.csv")
visits$Variable <- as.factor(visits$Variable)
visits <- visits %>% filter(Variable == "Doctors consultations (in all settings)", Year %in% c(2015, 2018)) %>%
  select(-c(1:5, 9:10)) %>% rename(doc.visits = Value)
visits$Country <- recode(visits$Country, "Korea" = "South Korea")

df <- left_join(df, visits, by = c("Country" = "Country", "TIME" = "Year"))
# 33 missing data points 
print(df %>% filter(is.na(doc.visits) == TRUE) %>% select(Country, TIME), n = 33)


# look at quality of healthcare dataset
qual <- read.csv("HEALTH_HCQI.csv")
qual$Indicator <- as.factor(qual$Indicator)
qual$Country <- recode(qual$Country, "China (People's Republic of)" = "China")
qual$Country <- recode(qual$Country, "Korea" = "South Korea")
qual$Periods <- as.numeric(qual$Periods)
print(qual %>% group_by(Indicator) %>% summarize(n = n()), n = 32)
qual <- filter(qual, Country %in% df$Country & Periods %in% c(2014, 2017) & Gender == "Total" & 
                 Indicator == "Thirty-day mortality after admission to hospital for ischemic stroke based on unlinked data" &
                 Value == "Age-sex standardised rate per 100 patients")
qual$Periods <- qual$Periods + 1 # to be able to join with vaccine rate data
qual <- qual %>% select(c(Country, Periods, Value.1)) %>%
  rename(mortality.stroke = Value.1)

df <- left_join(df, qual, by = c("Country", "TIME" = "Periods"))

# look at education dataset
edu <- read.csv("Tertiary_Edu.csv")
edu <- edu %>% filter(SUBJECT == "25_34", TIME %in% c(2015, 2018)) %>%
  rename(Location = ï..LOCATION, tert.edu = Value) %>% 
  select(c(Location, TIME, tert.edu))
edu$Location <- tolower(edu$Location) 

df <- left_join(df, edu, by = c("LOCATION" = "Location", "TIME" = "TIME"))
df %>% filter(is.na(tert.edu)) %>% select(Country, TIME)


# human development index
HDI <- read.csv("Human Development Index (HDI).csv", na.strings = "..")
HDI <- HDI %>% select(c(Country, X2015, X2018)) %>% 
  rename("2015" = X2015, "2018" = X2018) %>% 
  pivot_longer(2:3, names_to = "Year", values_to = "HDI")
HDI$Year <- as.numeric(HDI$Year)
HDI$Country <- recode(HDI$Country, "Czechia" = "Czech Republic")
HDI$Country <- recode(HDI$Country, "Korea (Republic of)" = "South Korea")
HDI$Country <- recode(HDI$Country, "Slovakia" = "Slovak Republic")
HDI$Country <- recode(HDI$Country, "Russian Federation" = "Russia")
df <- left_join(df, HDI, by = c("Country", "TIME" = "Year"))

# make final df
df <- df %>% ungroup %>% 
  select(-c(SUBJECT, LOCATION, grep("religious", names(df), value = TRUE))) %>%
  rename(Year = TIME, measles.vacrate = Vacrate) %>%
  relocate(3, 1)

# add HDI quintiles
df <- df %>% group_by(Year) %>% 
  mutate(HDI.quintile = cut(HDI, breaks = c(0, quantile(HDI, probs = seq(0, 1, 0.2))[2:6]), labels = FALSE))

# save dataset
write.csv(df, "project_data.csv", row.names = FALSE)
