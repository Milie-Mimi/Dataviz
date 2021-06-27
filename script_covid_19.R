####################################################################################################################
# vaccination <- read.table("vaccination-data.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ",")
vaccination <- read.csv("data/vaccination-data.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

####################################################################################################################
# Transformer FIRST_VACCINE_DATE et DATE_UPDATED en date
library(lubridate)
vaccination$FIRST_VACCINE_DATE <- dmy(vaccination$FIRST_VACCINE_DATE)
vaccination$DATE_UPDATED <- dmy(vaccination$DATE_UPDATED)

vaccination$DATE_UPDATED[is.na(vaccination$DATE_UPDATED)] <- "2021-09-04"

####################################################################################################################
# Suppression dernière colonne X et Datasource
vaccination$X <-NULL
vaccination$DATA_SOURCE <-NULL

####################################################################################################################
# Vérification du nombre de vaccinations en % (>100)
vaccination  %>% filter(TOTAL_VACCINATIONS_PER100 > 100)

# Récupération de la population du pays pour recalcul de cette variable
population <- read.csv("data/population.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)
vaccination = merge(vaccination, population, by.x = "ISO3", by.y = "iso_code", all.x = T, all.y = F)


# PERSONS_VACCINATED_1PLUS_DOSE_PER100 remplacer , par . puis transformer en numeric
library(stringr)
vaccination$PERSONS_VACCINATED_1PLUS_DOSE_PER100 <- str_replace(vaccination$PERSONS_VACCINATED_1PLUS_DOSE_PER100, ",", ".")
vaccination$PERSONS_VACCINATED_1PLUS_DOSE_PER100 <- as.numeric(vaccination$PERSONS_VACCINATED_1PLUS_DOSE_PER100)


# NA colonne Population et Continent
vaccination[is.na(vaccination$Population), c("COUNTRY", "Population")]

vaccination[vaccination$COUNTRY == "Aruba","Population"] <- 106314
vaccination[vaccination$COUNTRY == "Aruba","continent"] <- "South America" 

vaccination[vaccination$COUNTRY == "Samoa","Population"] <- 198410
vaccination[vaccination$COUNTRY == "Samoa","continent"] <- "Oceania"

vaccination[vaccination$COUNTRY == " Sint Eustatius and Saba","Population"] <- 3138
vaccination[vaccination$COUNTRY == " Sint Eustatius and Saba","continent"] <- "South America"

vaccination[vaccination$COUNTRY == "CuraÃ§ao","Population"] <- 157538
vaccination[vaccination$COUNTRY == "CuraÃ§ao","continent"] <- "South America"

vaccination[vaccination$COUNTRY == "Guadeloupe","Population"] <- 395700
vaccination[vaccination$COUNTRY == "Guadeloupe","continent"] <- "North America"  
  
vaccination[vaccination$COUNTRY == "French Guiana","Population"] <- 294071
vaccination[vaccination$COUNTRY == "French Guiana","continent"] <-"South America"
  
vaccination[vaccination$COUNTRY == "Guam","Population"] <- 167294
vaccination[vaccination$COUNTRY == "Guam","continent"] <- "Oceania"  
  
vaccination[vaccination$COUNTRY == "Northern Mariana Islands (Commonwealth of the)","Population"]<- 57216
vaccination[vaccination$COUNTRY == "Northern Mariana Islands (Commonwealth of the)","continent"]<-"Oceania"  
  
vaccination[vaccination$COUNTRY == "Martinique","Population"] <- 368783 
vaccination[vaccination$COUNTRY == "Martinique","continent"] <- "North America" 
  
vaccination[vaccination$COUNTRY == "New Caledonia","Population"] <- 271407
vaccination[vaccination$COUNTRY == "New Caledonia","continent"] <-"Oceania" 
  
vaccination[vaccination$COUNTRY == "Palau","Population"] <- 18008
vaccination[vaccination$COUNTRY == "Palau","continent"] <- "Oceania" 

vaccination[vaccination$COUNTRY == "Puerto Rico","Population"] <- 3193694 
vaccination[vaccination$COUNTRY == "Puerto Rico","continent"] <- "North America" 
  
vaccination[vaccination$COUNTRY == "French Polynesia","Population"] <- 279287
vaccination[vaccination$COUNTRY == "French Polynesia","continent"] <- "Oceania" 
  
vaccination[vaccination$COUNTRY == "Sint Maarten","Population"] <- 40733 
vaccination[vaccination$COUNTRY == "Sint Maarten","continent"] <- "North America"
  
vaccination[vaccination$COUNTRY == "British Virgin Islands","Population"] <- 30030
vaccination[vaccination$COUNTRY == "British Virgin Islands","continent"] <- "North America"
  
vaccination[vaccination$COUNTRY == "Wallis and Futuna","Population"] <- 15289  
vaccination[vaccination$COUNTRY == "Wallis and Futuna","continent"] <- "Oceania" 


# Recalcul per 100 en fonction de la population
vaccination$TOT_VACC_PER_100 <- format(vaccination$TOTAL_VACCINATIONS / vaccination$Population,scientific=FALSE, 
                                       digits=2)
vaccination$PERS_VACCINATED_1PLUS_DOSE_PER100 <- format(vaccination$PERSONS_VACCINATED_1PLUS_DOSE / vaccination$Population,
                                                        scientific=FALSE, digits=2)


vaccination$TOT_VACC_PER_100 <- as.numeric(vaccination$TOT_VACC_PER_100)
vaccination$PERS_VACCINATED_1PLUS_DOSE_PER100 <- as.numeric(vaccination$PERS_VACCINATED_1PLUS_DOSE_PER100)


####################################################################################################################
# NA Number Vaccines Types Used (pas de données, non retraitable)
vaccination[is.na(vaccination$NUMBER_VACCINES_TYPES_USED), c("COUNTRY", "VACCINES_USED", "NUMBER_VACCINES_TYPES_USED")]







####################################################################################################################
# DATASET COMPLET avec covid data_pour_merge
library(dplyr)
vaccination_final <- select(vaccination, ISO3, continent, COUNTRY, DATE_UPDATED, Population, TOTAL_VACCINATIONS, 
                            PERSONS_VACCINATED_1PLUS_DOSE, VACCINES_USED,FIRST_VACCINE_DATE, NUMBER_VACCINES_TYPES_USED, 
                            TOT_VACC_PER_100, PERS_VACCINATED_1PLUS_DOSE_PER100)



covid <- read.csv("data/covid_data_pour_merge.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

vaccination_final2 = merge(vaccination_final, covid, by.x = "ISO3", by.y = "iso_code", all.x = T, all.y = F)


vaccination_final2 <- select(vaccination_final2, ISO3, continent.x, COUNTRY,DATE_UPDATED,Population,TOTAL_VACCINATIONS,PERSONS_VACCINATED_1PLUS_DOSE,
                             VACCINES_USED,FIRST_VACCINE_DATE,NUMBER_VACCINES_TYPES_USED,TOT_VACC_PER_100,PERS_VACCINATED_1PLUS_DOSE_PER100,Max..de.median_age,
                             Max..de.aged_65_older,Max..de.aged_70_older,Max..de.gdp_per_capita,Max..de.cardiovasc_death_rate,Max..de.diabetes_prevalence,
                             Max..de.life_expectancy,Max..de.human_development_index)


vaccination_final2 <- vaccination_final2 %>% rename(CONTINENT = continent.x, 
                                                    POPULATION = Population,
                                                    MEDIAN_AGE = Max..de.median_age,
                                                    AGED_65_OLDER=Max..de.aged_65_older,
                                                    AGED_70_OLDER=Max..de.aged_70_older,
                                                    GDP_PER_CAPITA=Max..de.gdp_per_capita,
                                                    CARDIOVASC_DEATH_RATE=Max..de.cardiovasc_death_rate,
                                                    DIABETE_PREVALENCE=Max..de.diabetes_prevalence,
                                                    LIFE_EXPECTANCY=Max..de.life_expectancy,
                                                    HUMAN_DEV_INDEX=Max..de.human_development_index)

                                                    

# write.csv2(vaccination_final2, file = "data_output/vaccination_final.csv", row.names = FALSE) # decimal(,) et sep(;)

# Avec split des vaccins par ligne
library(tidyr)
vaccination_final_vacc <- vaccination_final2 %>% separate_rows(VACCINES_USED, sep=",")


# write.csv2(vaccination_final3, file = "data_output/vaccination_final_vacc.csv", row.names = FALSE) # decimal(,) et sep(;)










####################################################################################################################
# Table vaccination par continent
vaccins_continent <- 
  vaccination_final %>% 
  group_by(continent) %>% 
  summarise(first_vaccine_date = min(FIRST_VACCINE_DATE,na.rm = TRUE), TOT_VACC = sum(TOTAL_VACCINATIONS,na.rm = TRUE), TOT_VAC_1_DOSE = sum(PERSONS_VACCINATED_1PLUS_DOSE,na.rm = TRUE),
            POP = sum(Population, na.rm = TRUE)) %>%
  mutate(TOT_VACC_PER_100 = TOT_VACC / POP) %>%
  mutate(PERS_VACCINATED_1PLUS_DOSE_PER100 = TOT_VAC_1_DOSE / POP)

library(here)
dir.create("data_output") # pour créer le dossier data_output


# write.csv(vaccins_continent, file = "vaccins_continent1.csv", row.names = FALSE) # decimal(.) et sep(,)
write.csv2(vaccins_continent, file = "vaccins_continent.csv", row.names = FALSE) # decimal(,) et sep(;)


####################################################################################################################
# Nom des vaccins utilisés et compte par pays

library(dplyr)
library(tidyr)

types_vaccins <- vaccination_final %>% select(continent, COUNTRY, VACCINES_USED) %>%
  separate_rows(VACCINES_USED, sep=",")

write.csv2(types_vaccins, file = "types_vaccins.csv", row.names = FALSE) # decimal(,) et sep(;)



write.csv2(types_vaccins, file = "types_vaccins.csv", row.names = FALSE) # decimal(,) et sep(;)








####################################################################################################################
covid_data <- read.csv("data/covid_data.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)


# Transformation des variables
str(covid_data)

library(lubridate)
covid_data$date <- dmy(covid_data$date)


# % Pers ayant reçu au moins une dose de vaccin

vaccin_ev <- covid_data %>% select(location, date, total_vaccinations_per_hundred)

write.csv2(vaccin_ev, file = "data/data_output/vaccin_ev.csv", row.names = FALSE) # decimal(,) et sep(;)



# Prendre la date max de chaque pays
library(dplyr)
covid_max_date <- covid_data %>% filter(date == max(date) & people_vaccinated_per_hundred !is.na(people_vaccinated_per_hundred))


covid_max_date <- covid_max_date %>% 
  group_by(location) %>% 
  filter(date == max(date))

covid_max_date$people_vaccinated_per_hundred <- covid_max_date$people_vaccinated_per_hundred/100
covid_max_date$people_fully_vaccinated_per_hundred <- covid_max_date$people_fully_vaccinated_per_hundred/100



write.csv2(covid_max_date, file = "data_output/covid_max_date.csv", row.names = FALSE)


save.image("COVID")
load

################################################################################################################################

# Prendre la date max de chaque pays
library(dplyr)

covid_pour_max_date <- read.csv("data/covid_pour_max_date.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

library(lubridate)
covid_pour_max_date$date <- dmy(covid_pour_max_date$date)


covid_max_date <- covid_pour_max_date %>% filter( people_vaccinated_per_hundred.at_least_one_dose. != is.na(people_vaccinated_per_hundred.at_least_one_dose.))

covid_max_date <- covid_max_date %>% 
  group_by(location) %>% 
  filter(date == max(date))

covid_max_date$people_vaccinated_per_hundred <- covid_max_date$total_vaccinations_per_hundred/100
covid_max_date$people_vaccinated_per_hundred <- covid_max_date$people_vaccinated_per_hundred.at_least_one_dose./100
covid_max_date$people_fully_vaccinated_per_hundred <- covid_max_date$people_fully_vaccinated_per_hundred/100

covid_max_date <- covid_max_date %>% select (-icu_patients, 
                                            -icu_patients_per_million, 
                                            - hosp_patients, 
                                            - hosp_patients_per_million, 
                                            - weekly_icu_admissions,
                                            - weekly_icu_admissions_per_million,
                                            - weekly_hosp_admissions,
                                            - weekly_hosp_admissions_per_million)



write.csv2(covid_max_date, file = "data_output/covid_max_date.csv", row.names = FALSE)


save.image("COVID")
load("COVID")


