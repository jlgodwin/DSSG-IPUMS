rm(list = ls())

# Setup ####

## Working Directory ####
setwd("~/Dropbox/DSSG2022/IPUMS/DSSG-IPUMS/")

## Libraries ####
library(tidyverse)
library(ipumsr)
library(rgdal)
library(spdep)
library(survey)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(classInt)

## Parameters ####
write.yyyy.mm.dd <- "20220812"

# Load Data ####
ddi <- read_ipums_ddi("Data/usa_00004.xml")
data <- read_ipums_micro(ddi)

# Explore data ###
head(data)
summary(data)
names(data)
View(ddi$var_info)

## Geography ####
table(data$STATEFIP, useNA = "ifany")
table(data$COUNTYFIP, useNA = "ifany")

ddi$var_info %>%
  filter(var_name == "COUNTYFIP") %>%
  select(val_labels) %>%
  pluck(1) %>% as.data.frame() %>%
  filter(val %in% data$COUNTYFIP)

# Time ####
table(data$YEAR, useNA = "ifany")

# Sample ####
table(data$SAMPLE, useNA = "ifany")
ddi$var_info %>%
  filter(var_name == "SAMPLE") %>%
  select(val_labels) %>%
  pluck(1) %>% as.data.frame() %>%
  filter(val %in% data$SAMPLE)

# Survey/sampling design variables ####

## HH number ####
length(table(data$SERIAL))
nrow(data)
range(data$SERIAL)

## Strata ####
table(data$STRATA, useNA = "ifany")


## Household weight ####
summary(data$HHWT)

## Person weight ####

summary(data$PERWT)
range(summary(data$PERNUM))

## Demographics ####

summary(data[, c("SEX", "AGE", "RACE", "RACED", "HISPAN", "HISPAND")])


### RACE ####

data <- data %>% 
  left_join(ddi$var_info$val_labels[[18]],
            by = c("RACE" = "val")) %>% 
  rename("RACE_CHAR" = "lbl")
summary(data$RACE_CHAR %>% as.factor)

### SEX ####

data <- data %>% 
  left_join(ddi$var_info$val_labels[[16]],
            by = c("SEX" = "val")) %>% 
  rename("SEX_CHAR" = "lbl")
summary(data$SEX_CHAR %>% as.factor)

### HISPANIC ####

data <- data %>% 
  left_join(ddi$var_info$val_labels[[20]],
            by = c("HISPAN" = "val")) %>% 
  rename("HISPAN_CHAR" = "lbl")
summary(data$HISPAN_CHAR %>% as.factor)

data <- data %>% 
  mutate(HISPAN_BIN = ifelse(HISPAN_CHAR == "Not Hispanic",
                             "Not Hispanic", "Hispanic"))

## Household ####

### GQ ####

data <- data %>% 
  left_join(ddi$var_info$val_labels[[9]],
            by = c("GQ" = "val")) %>% 
  rename("GQ_CHAR" = "lbl")
summary(data$GQ_CHAR %>% as.factor)

data <- data %>% 
  mutate(HH_CHAR = ifelse(GQ_CHAR %in% c("Group quarters--Institutions",
                                         "Other group quarters"),
                          "Group quarters", "Households"))
summary(data$HH_CHAR %>% as.factor)

### MULTGEN, MULTGEND ####

data <- data %>% 
  left_join(ddi$var_info$val_labels[[10]],
            by = c("MULTGEN" = "val")) %>% 
  rename("MULTGEN_CHAR" = "lbl") %>% 
  left_join(ddi$var_info$val_labels[[11]],
            by = c("MULTGEND" = "val")) %>% 
  rename("MULTGEND_CHAR" = "lbl")
summary(data$MULTGEN_CHAR %>% as.factor)
summary(data$MULTGEND_CHAR %>% as.factor)

### RELATE ####

data <- data %>% 
  left_join(ddi$var_info$val_labels[[14]],
            by = c("RELATE" = "val")) %>% 
  rename("RELATE_CHAR" = "lbl") 

summary(data$RELATE_CHAR %>% as.factor)


### AGE ####

data <- data %>% 
  mutate(AGE_BIN = cut(AGE, seq(0,95,5), 
                       right = FALSE,
                       include.lowesst = TRUE))
summary(data$AGE_BIN)

# Households ####

## hh_size ####

hh_data <- data %>% 
  left_join(data %>% 
              group_by(SERIAL) %>% 
              summarise(hh_size = max(PERNUM),
                        hh_size_uni = length(unique(PERNUM))),
            by = c("SERIAL"))

## Check if defs differ
sum(hh_data$hh_size != hh_data$hh_size_uni)

table(hh_data$hh_size, useNA = "ifany")
table(hh_data$HH_CHAR, hh_data$hh_size, useNA = "ifany")

## Age ####

hh_by_size_age <- hh_data %>%
  filter(HH_CHAR == "Households") %>% 
  select(SERIAL, PERNUM, hh_size, AGE) %>% 
  mutate(AGE_BROAD_BIN = case_when(AGE < 15 ~ "0-15",
                                   AGE >= 15 & AGE < 18 ~ "15-18",
                                   AGE >= 18 & AGE < 65 ~ "18-65",
                                   AGE >= 65 ~ "65-95"),
         ABOVE_18 = ifelse(AGE >= 18, 1, 0)) %>% 
  group_by(hh_size, SERIAL) %>% 
  summarise(Under15 = ifelse("0-15" %in% AGE_BROAD_BIN, 1, 0),
            `15-18` = ifelse("15-18" %in% AGE_BROAD_BIN, 1, 0),
            `18-65` = ifelse("18-65" %in% AGE_BROAD_BIN, 1, 0),
            Above65 = ifelse("65-95" %in% AGE_BROAD_BIN, 1, 0),
            Above18 = ifelse(sum(ABOVE_18 > 0), 1, 0)) %>% 
  ungroup() %>% 
  group_by(hh_size) %>% 
  summarize(Total = n(),
            Under15 = sum(Under15),
            `15-18` = sum(`15-18`),
            `18-65` = sum(`18-65`),
            Above65 = sum(Above65),
            Above18 = sum(Above18))

if(!dir.exists("Tables/")){
  dir.create("Tables/")
}

write.csv(hh_by_size_age,
          file = "Tables/HH_by_size_age.csv",
          row.names = FALSE)

## Totals by Size with Standard Errors ####

census_des <- svydesign(ids = ~1, strata = ~STRATA, weights = ~HHWT,
                        data = hh_data %>% 
                          group_by(STRATA, hh_size) %>%
                          summarise(Total = unique(SERIAL),
                                    HHWT = unique(HHWT),
                                    COUNTYFIP = unique(COUNTYFIP)) %>% 
                          mutate(Counter = 1))
no_hh <- svyby(~Counter, by = ~hh_size,
               des = census_des,
               svytotal) %>% 
  rename("Total" = "Counter") %>% 
  mutate(Area = "WA") %>% 
  relocate(Area, .before = "hh_size")

no_hh_county <- svyby(~Counter, by = ~COUNTYFIP + hh_size,
                      des = census_des,
                      svytotal) %>% 
  rename("Total" = "Counter",
         "Area" = "COUNTYFIP")

no_hh_all <- rbind.data.frame(no_hh, no_hh_county)

write.csv(no_hh_all,
          file ="Tables/TotalHH_SE.csv",
          row.names = FALSE)
