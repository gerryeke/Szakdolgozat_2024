
###############################################################################################################
#                                       Könyvtárak                                                            #
###############################################################################################################


setwd("D:/Pénzügy mester/Szakdoga 2024/Adatok 2022/Bondora Adatok")
getwd()
require(dplyr)

#install.packages("openxlsx")
library(openxlsx)

#install.packages("lubridate")
library(lubridate)


install.packages("MLmetrics")
library(MLmetrics)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("caret")
library(caret)

#install.packages("lattice")
library(lattice)

#install.packages("pROC")
library(pROC)

#install.packages("D:/Pénzügy mester/Szakdoga 2024/Adatok 2022/package/caret.zip", repos = NULL, type = "source")

#install.packages("corrplot")
library(corrplot)

#install.packages("MASS")
library(MASS) 

#install.packages("Matrix")
library(Matrix)

#install.packages("glmnet")
library(glmnet)



###############################################################################################################
#                                     Adatok beolvasása                                                       #
###############################################################################################################

LoanData <- read.csv("LoanData_2024.csv", header=TRUE, sep = ",")
#LoanData <- read.csv("LoanData.csv", header=TRUE, sep = ",")
#LoanSchedules <- read.csv("LoanSchedules.csv", header=TRUE, sep = ",")
#MonthlyOverview <- read.csv("MonthlyOverview.csv", header=TRUE, sep = ",")
#RepaymentsData <- read.csv("RepaymentsData.csv", header=TRUE, sep = ",")

colnames(LoanData)


###############################################################################################################
#                                     LoanSchedules adatok                                                    #
###############################################################################################################

# Igen-nem flag
Default_types <- LoanSchedules %>% 
  group_by(IsLoanInDefault) %>% 
  summarise(count = n())

# Igen-nem flag
Future_types <- LoanSchedules %>% 
  group_by(IsFuturePayment) %>% 
  summarise(count = n())

# Használható mezõk: 
# loan_id  -----> Kötéshez
# Date -----> Dátum mezõ
# IsLoanInDefault -----> Flag
# Principal.Payment -----> Fizetési infromáció
# Interest.Payment -----> Fizetési infromáció
# Rescheduled.Accrued.Interest -----> Fizetési infromáció
# Rescheduled.Late.Fees -----> Fizetési infromáció
# IsFuturePayment -----> Flag

rm(Default_types, Future_types)

###############################################################################################################
#                                      LoanData adatok                                                        #
###############################################################################################################
#####################################################    Step 1: Üres változók törlése
# Nincs benne adat
City_types <- LoanData %>% 
  group_by(City) %>% 
  summarise(count = n())

# Nincs benne adat
County_types <- LoanData %>% 
  group_by(County) %>% 
  summarise(count = n())

# Nincs benne adat
DateOfBirth_types <- LoanData %>% 
  group_by(DateOfBirth) %>% 
  summarise(count = n())

# Nincs benne adat
EmploymentPosition_types <- LoanData %>% 
  group_by(EmploymentPosition) %>% 
  summarise(count = n())

# 4 oszlop törlése
LoanData <- LoanData %>% 
  select(-City, -County, -DateOfBirth, -EmploymentPosition)

rm(City_types, County_types, DateOfBirth_types, EmploymentPosition_types)


#####################################################    Step 2: Már applikáció utáni adatok
# Ez az 6 már azzal kapcsolatos, hogy ki akarja finanszírozni az ügyfelet --> Már késõi nálam

LoanData <- LoanData %>% 
  select(-ListedOnUTC, -BiddingStartedOn, -BidsPortfolioManager, -BidsApi, -BidsManual, -Restructured)

#2: Post Default adatok
LoanData <- LoanData %>% 
  select(-PlannedPrincipalPostDefault, -PlannedInterestPostDefault)

#3: Bondora elvileg beépíti más ratingek eredményét is. Én nem fogom/tudom, ezért törlöm

LoanData <- LoanData %>% 
  select(-CreditScoreEsMicroL, -CreditScoreEsEquifaxRisk,-CreditScoreFiAsiakasTietoRiskGrade,-CreditScoreEeMini)


#####################################################    Step 3: Egyéb törlések
# A sima Rating mezõ van töltve, ezke szinte sehol
LoanData <- LoanData %>% 
  select(-Rating_V0, -Rating_V1, -Rating_V2)


#####################################################    Step 4: Változók létrehozása és a kiinduló adat eldobása
#Külön-külön nem tartom értelmesnek csak a munkahelyi bevételt ezért a többit összevonom, hátha jó a változó
# 3 Income változóm lesz
LoanData <- LoanData %>%
  mutate(IncomeOtherType = rowSums(select(., IncomeFromSocialWelfare, 
                                          IncomeFromLeavePay, 
                                          IncomeFromChildSupport, 
                                          IncomeOther, 
                                          IncomeFromPension, 
                                          IncomeFromFamilyAllowance),
                                   na.rm = TRUE))

LoanData <- LoanData %>% 
  select(-IncomeFromSocialWelfare, -IncomeFromLeavePay, -IncomeFromChildSupport,-IncomeOther,-IncomeFromPension,-IncomeFromFamilyAllowance)



#2: New customer flag
NewCreditCustomer <- LoanData %>% 
  group_by(NewCreditCustomer) %>% 
  summarise(count = n())

LoanData <- LoanData %>%
  mutate(New_customer_flag = ifelse(NewCreditCustomer == "True", 1, 0))

NewCreditCustomer2 <- LoanData %>% 
  group_by(NewCreditCustomer,New_customer_flag) %>% 
  summarise(count = n())

rm(NewCreditCustomer,NewCreditCustomer2)

#3: Default flag: Történt-e default a hitel visszafizetés alatt?

# Bármikor
LoanData <- LoanData %>%
  mutate(Default_flag_lifetime = ifelse(DefaultDate != "", 1, 0))

Lifetime_default <- LoanData %>% 
  group_by(Default_flag_lifetime) %>% 
  summarise(count = n())

########################################
# Folyosítás utáni 2 évben default
LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_2y = ifelse(
      !is.na(DefaultDate) & DefaultDate <= LoanDate + years(2) & DefaultDate >= LoanDate,
      1, 
      0
    )
  )

# 89 helyen valami hiba van, szóval ezeket excelben megnézem külön egyesével és úgy döntöm el, hogy 0 vagy 1
na_rows <- LoanData %>%
  filter(is.na(Default_flag_2y)) %>%
  select(LoanId,LoanDate, DefaultDate, Default_flag_2y)

write.xlsx(na_rows, "na_rows.xlsx")


# Ezek a 0 értékûek
zero_ids_2y <- c("CFDE80E6-3D0D-47BC-AD0E-A5B600A92141", "754B8BB5-5325-4DCF-B71F-A5B8011A3D1D",
                 "6B2CFD5F-6E75-4606-BB3E-AB6F00F45600", "5B1862DC-32EC-410C-B5B4-AB7000AF8484",
                 "744470F8-C78F-4D8D-94A8-AB7000D1784C", "176EB963-8E3E-4BAE-9AEE-AB7000E19FD4",
                 "2826C10B-7F6E-4D9A-90E5-AB7000FFBA8C", "800A21B5-B9E3-4A6C-88F6-AB7001013A2F",
                 "31D67947-6AF8-438C-8756-AB7001063432", "268F45AE-A323-4119-83C6-AB70011A67C3",
                 "DD7E83C9-E841-4713-84C6-AB7001286C5E")

LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_2y = ifelse(
      LoanId %in% zero_ids_2y, 0,
      ifelse(
        !is.na(DefaultDate) & DefaultDate <= LoanDate + years(2) & DefaultDate >= LoanDate,
        1, 
        0
      )
    )
  )

# ezek meg az 1-ek
LoanData <- LoanData %>%
  mutate(
    Default_flag_2y = ifelse(is.na(Default_flag_2y), 1, Default_flag_2y)
  )


default_2y <- LoanData %>% 
  group_by(Default_flag_2y) %>% 
  summarise(count = n())

Default_chk <- LoanData %>% 
  group_by(Default_flag_2y,Default_flag_lifetime) %>% 
  summarise(count = n())

rm(na_rows,default_2y,Default_chk,Lifetime_default)


######################################## Eddig tartott a 2y default flag

########################################
# Folyosítás utáni 1 évben default
LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_1y = ifelse(
      !is.na(DefaultDate) & DefaultDate <= LoanDate + years(1) & DefaultDate >= LoanDate,
      1, 
      0
    )
  )

# 89 helyen valami hiba van, szóval ezeket excelben megnézem külön egyesével és úgy döntöm el, hogy 0 vagy 1
na_rows_1y <- LoanData %>%
  filter(is.na(Default_flag_1y)) %>%
  select(LoanId,LoanDate, DefaultDate, Default_flag_1y)

write.xlsx(na_rows_1y, "na_rows_1y.xlsx")


# Új azonosítók hozzáadása a képen láthatók alapján
zero_ids_1y <- c("932B0F92-8B44-499F-A056-00C6D6D1E312", "1D1A8C00-6A87-44B1-9CCE-290EC1786782",
                 "6D3679BA-7A84-4DB5-B581-A5B601472A82", "CC4A1E1A-F8D5-4607-8DA5-AB6F00F95675",
                 "D6081F32-73E7-46D1-B367-AB6F016D3033", "0053643B-A60F-4255-A3BB-AB70005DA65F",
                 "DEF5EA25-C64B-466A-9361-AB700072DC38", "3913236A-2733-4387-B5BB-AB700081E67D",
                 "BAA6CA9D-FAA0-4DAD-B8D8-AB700085C328", "BF87BE5C-2F1C-4F2B-B48A-AB7000898259",
                 "4DE2EA38-E21E-40EB-8483-AB70008F72CF", "667A1F1F-C1A8-421F-A6D8-AB7000A710B9",
                 "C5C20B9A-676F-4F77-8E72-AB7000BA3767", "0894744A-4238-4021-BAFF-AB7000BA7B9B",
                 "7CA0995A-A61F-4905-8434-AB7000C211DF", "DA9A45BE-F953-4776-8820-AB7000C6EA1C",
                 "3C5B75E2-8B61-4E41-A298-AB7000D10A63", "6B2B725B-BA42-4ED2-9C8C-AB7000E502FC",
                 "878176D6-FB9A-4106-B4ED-AB7000EDAA61", "5F6FFCF6-99C7-4006-A297-AB7000FD891C",
                 "93ECCBFA-2F85-41E0-B5C4-AB70016D10E6", "E621E1C3-1B89-4CD7-B89D-AB700170C738")

# Összesített zero_ids frissítése
zero_ids <- c(zero_ids_2y, zero_ids_1y)

LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_1y = ifelse(
      LoanId %in% zero_ids, 0,
      ifelse(
        !is.na(DefaultDate) & DefaultDate <= LoanDate + years(1) & DefaultDate >= LoanDate,
        1, 
        0
      )
    )
  )

# ezek meg az 1-ek
LoanData <- LoanData %>%
  mutate(
    Default_flag_1y = ifelse(is.na(Default_flag_1y), 1, Default_flag_1y)
  )


default_1y <- LoanData %>% 
  group_by(Default_flag_1y) %>% 
  summarise(count = n())

Default_chk <- LoanData %>% 
  group_by(Default_flag_1y,Default_flag_lifetime) %>% 
  summarise(count = n())

rm(na_rows,zero_ids,default_1y,Default_chk,Lifetime_default)


######################################## Eddig tartott a 1y default flag



########################################
# Folyosítás utáni 3 évben default
LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_3y = ifelse(
      !is.na(DefaultDate) & DefaultDate <= LoanDate + years(3) & DefaultDate >= LoanDate,
      1, 
      0
    )
  )

# Új azonosítók hozzáadása a képen láthatók alapján
zero_ids_3y <- c("6B2CFD5F-6E75-4606-BB3E-AB6F00F45600", "744470F8-C78F-4D8D-94A8-AB7000D1784C",
                 "176EB963-8E3E-4BAE-9AEE-AB7000E19FD4", "2826C10B-7F6E-4D9A-90E5-AB7000FFBA8C",
                 "800A21B5-B9E3-4A6C-88F6-AB7001013A2F", "31D67947-6AF8-438C-8756-AB7001063432",
                 "268F45AE-A323-4119-83C6-AB70011A67C3", "DD7E83C9-E841-4713-84C6-AB7001286C5E")

LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_3y = ifelse(
      LoanId %in% zero_ids_3y, 0,
      ifelse(
        !is.na(DefaultDate) & DefaultDate <= LoanDate + years(3) & DefaultDate >= LoanDate,
        1, 
        0
      )
    )
  )

# ezek meg az 1-ek
LoanData <- LoanData %>%
  mutate(
    Default_flag_3y = ifelse(is.na(Default_flag_3y), 1, Default_flag_3y)
  )

default_3y <- LoanData %>% 
  group_by(Default_flag_3y) %>% 
  summarise(count = n())

Default_chk <- LoanData %>% 
  group_by(Default_flag_3y,Default_flag_lifetime) %>% 
  summarise(count = n())

rm(na_rows,zero_ids,default_3y,Default_chk,Lifetime_default)


######################################## Eddig tartott a 3y default flag

########################################
# Folyosítás utáni 4 évben default
LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_4y = ifelse(
      !is.na(DefaultDate) & DefaultDate <= LoanDate + years(4) & DefaultDate >= LoanDate,
      1, 
      0
    )
  )

# Új azonosítók hozzáadása a képen láthatók alapján
zero_ids_4y <- c("744470F8-C78F-4D8D-94A8-AB7000D1784C","CFDE80E6-3D0D-47BC-AD0E-A5B600A92141")

LoanData <- LoanData %>%
  mutate(
    LoanDate = as.Date(LoanDate),
    DefaultDate = as.Date(DefaultDate),
    Default_flag_4y = ifelse(
      LoanId %in% zero_ids_4y, 0,
      ifelse(
        !is.na(DefaultDate) & DefaultDate <= LoanDate + years(4) & DefaultDate >= LoanDate,
        1, 
        0
      )
    )
  )

# ezek meg az 1-ek
LoanData <- LoanData %>%
  mutate(
    Default_flag_4y = ifelse(is.na(Default_flag_4y), 1, Default_flag_4y)
  )

default_4y <- LoanData %>% 
  group_by(Default_flag_4y) %>% 
  summarise(count = n())

Default_chk <- LoanData %>% 
  group_by(Default_flag_4y,Default_flag_lifetime) %>% 
  summarise(count = n())

rm(na_rows,zero_ids,default_4y,Default_chk,Lifetime_default,zero_ids_1y,zero_ids_2y,zero_ids_3y,zero_ids_4y)


######################################## Eddig tartott a 3y default flag

#4:
# PTI és Application_Time saját változók létrehozása
LoanData <- LoanData %>%
  mutate(
    # PTI kiszámítása
    PTI = (AppliedAmount / LoanDuration) / IncomeTotal,
    
    # Application_Time kiszámítása
    Application_Time = case_when(
      ApplicationSignedWeekday %in% c(6, 7) ~ 1,
      ApplicationSignedWeekday %in% 1:5 & ApplicationSignedHour >= 8 & ApplicationSignedHour <= 17 ~ 2,
      TRUE ~ 3
    )
  )



###############################################################################################################
#                                       Sorok szûrése                                                         #
###############################################################################################################


Rating_types <- LoanData %>% 
  group_by(Rating) %>% 
  summarise(count = n())
# 2733 missing törlése
LoanData <- LoanData %>%
  filter(!is.na(Rating) & Rating != "")

#2: Akiknél nem indult el a hitelezés, azokat is szûröm, mivel nem egyértelmûen mindenki default itt, szóval ez mást jelent, mint hogy az 1 hónapban defaultok lettek
ActiveScheduleFirstPaymentReached <- LoanData %>% 
  group_by(Default_flag_lifetime,ActiveScheduleFirstPaymentReached) %>% 
  summarise(count = n())

LoanData <- LoanData %>%
  filter(ActiveScheduleFirstPaymentReached == "True")

#3: Túl kevés a szlovák vagy holland megfigyelés, ezért törlöm õket
Country <- LoanData %>% 
  group_by(Country) %>% 
  summarise(count = n())

LoanData <- LoanData %>%
  filter(Country != "SK")

LoanData <- LoanData %>%
  filter(Country != "NL")


#4: Ha lenne ilyen, akkor az adathiba, de szerencsére nincsen
Adathiba <- LoanData %>%
  filter(DefaultDate < LoanDate) %>%
  summarise(Count = n())


rm(Rating_types,ActiveScheduleFirstPaymentReached,Country,Adathiba)
###############################################################################################################
#                                     Minták megalkotása                                                      #
###############################################################################################################



# Rating kategóriák átalakítása számmá
LoanData$Rating <- factor(LoanData$Rating, levels = c("AA", "A", "B", "C", "D", "E", "F", "HR"))

# Számok hozzárendelése a kategóriákhoz
LoanData$Rating_cat <- as.integer(LoanData$Rating)

# Igen-nem flag
Rating_cat_dist <- LoanData %>% 
  group_by(Rating_cat) %>% 
  summarise(count = n())

# Igen-nem flag
Rating_cat_dist2 <- LoanData %>% 
  group_by(Rating_cat,Country) %>% 
  summarise(count = n())





####### Live Data megfigyelések megállapítása
LoanData <- LoanData %>%
  mutate(LoanDate = as.Date(LoanDate),
         Live_data_flag = ifelse(LoanDate > as.Date("2022-03-31"), 1, 0))

# 226451 megfigyelés marad
Live_data_flag <- LoanData %>% 
  group_by(Live_data_flag) %>% 
  summarise(count = n())

rm(Live_data_flag)


#Év mezõ létrehozása, mert arra még szükség lesz a mintáknál:

LoanData <- LoanData %>%
  mutate(Year = as.integer(substr(LoanDate, 1, 4)))

###################################### Live Data

#Elõszõr levállogatom a Live data részeket, mert az nem kell
EE_live_data <- LoanData %>%
  filter(Country == "EE" & Live_data_flag == 1)

ES_live_data <- LoanData %>%
  filter(Country == "ES" & Live_data_flag == 1)

FI_live_data <- LoanData %>%
  filter(Country == "FI" & Live_data_flag == 1)

# ezekkel meg megyek tovább
EE_modelling_data <- LoanData %>%
  filter(Country == "EE" & Live_data_flag == 0)

ES_modelling_data <- LoanData %>%
  filter(Country == "ES" & Live_data_flag == 0)

FI_modelling_data <- LoanData %>%
  filter(Country == "FI" & Live_data_flag == 0)

#OK
Country <- LoanData %>% 
  group_by(Country,Live_data_flag) %>% 
  summarise(count = n())


#################################### Houldout set - 15%
################### Ezen hasonlítom össze majd a saját modellem eredményeit a Bondora modelljével

# A reprodukálhatóság érdekében elõre meghatározom (egyszer)
set.seed(14842)  

EE_modelling_data <- EE_modelling_data %>%
  group_by(Year) %>%
  mutate(Hold_out_set = if_else(row_number() %in% sample(row_number(), floor(0.15 * n())), 1, 0)) %>%
  ungroup()

#Jól mûködik
Hold_out_set_chk <- EE_modelling_data %>% 
  group_by(Hold_out_set,Year) %>% 
  summarise(count = n())


ES_modelling_data <- ES_modelling_data %>%
  group_by(Year) %>%
  mutate(Hold_out_set = if_else(row_number() %in% sample(row_number(), floor(0.15 * n())), 1, 0)) %>%
  ungroup()

FI_modelling_data <- FI_modelling_data %>%
  group_by(Year) %>%
  mutate(Hold_out_set = if_else(row_number() %in% sample(row_number(), floor(0.15 * n())), 1, 0)) %>%
  ungroup()

# Ezek a  végsõ Holdout set-ek
EE_houldout_set <- EE_modelling_data %>%
  filter(Hold_out_set == 1)

ES_houldout_set <- ES_modelling_data %>%
  filter(Hold_out_set == 1)

FI_houldout_set <- FI_modelling_data %>%
  filter(Hold_out_set == 1)

# Ezek meg a Train-Test mintarészek
EE_train_test <- EE_modelling_data %>%
  filter(Hold_out_set == 0)

ES_train_test <- ES_modelling_data %>%
  filter(Hold_out_set == 0)

FI_train_test <- FI_modelling_data %>%
  filter(Hold_out_set == 0)

# 5 fold megállapítása a módszertanhoz


set.seed(41832)  # A reprodukálhatóság biztosítása

total_rows <- nrow(EE_train_test)
per_fold_base <- total_rows %/% 5
remainder <- total_rows %% 5

EE_train_test <- EE_train_test %>%
  mutate(Fold = rep(1:5, times = c(rep(per_fold_base + 1, remainder), rep(per_fold_base, 5 - remainder)))) %>%
  mutate(Fold = sample(Fold)) %>%
  arrange(Fold)

Fold_EE <- EE_train_test %>% 
  group_by(Fold) %>% 
  summarise(count = n())

total_rows <- nrow(ES_train_test)
per_fold_base <- total_rows %/% 5
remainder <- total_rows %% 5

ES_train_test <- ES_train_test %>%
  mutate(Fold = rep(1:5, times = c(rep(per_fold_base + 1, remainder), rep(per_fold_base, 5 - remainder)))) %>%
  mutate(Fold = sample(Fold)) %>%
  arrange(Fold)

Fold_ES <- ES_train_test %>% 
  group_by(Fold) %>% 
  summarise(count = n())

total_rows <- nrow(FI_train_test)
per_fold_base <- total_rows %/% 5
remainder <- total_rows %% 5

FI_train_test <- FI_train_test %>%
  mutate(Fold = rep(1:5, times = c(rep(per_fold_base + 1, remainder), rep(per_fold_base, 5 - remainder)))) %>%
  mutate(Fold = sample(Fold)) %>%
  arrange(Fold)

Fold_FI <- FI_train_test %>% 
  group_by(Fold) %>% 
  summarise(count = n())

rm(total_rows,per_fold_base,remainder)





###############################################################################################################
#                                          WOE - Education                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(1),
    Education == 2 ~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(Education2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Excel fájl létrehozása az elsõ munkalappal
wb <- createWorkbook()
addWorksheet(wb, "EE_Education")
writeData(wb, "EE_Education", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#Education Dummy

EE_train_test <- EE_train_test %>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))

#####################################################################################

ES_train_test <- ES_train_test %>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(1),
    Education == 2 ~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(Education2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_Education")
writeData(wb, "ES_Education", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#Education Dummy
ES_train_test <- ES_train_test %>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(5),
    Education == 2~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(Education2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_Education")
writeData(wb, "FI_Education", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#Education Dummy
FI_train_test <- FI_train_test %>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))

###############################################################################################################
#                                          WOE - Gender                                                       #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(Gender2 = case_when(
    Gender == 2 ~ as.numeric(0),
    
    TRUE ~ as.numeric(Gender)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(Gender2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_Gender")
writeData(wb, "EE_Gender", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#Gender Dummy
EE_train_test <- EE_train_test %>%
  mutate(Gender_dummy0 = if_else(Gender2 == 0, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Gender_dummy1 = if_else(Gender2 == 1, 1, 0))


#####################################################################################

ES_train_test <- ES_train_test %>%
  mutate(Gender2 = Gender)

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(Gender2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_Gender")
writeData(wb, "ES_Gender", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#Gender Dummy
ES_train_test <- ES_train_test %>%
  mutate(Gender_dummy0 = if_else(Gender2 == 2, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(Gender_dummy1 = if_else(Gender2 == 0, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(Gender_dummy2 = if_else(Gender2 == 1, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(Gender2 = Gender)

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(Gender2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_Gender")
writeData(wb, "FI_Gender", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#Gender Dummy
FI_train_test <- FI_train_test %>%
  mutate(Gender_dummy0 = if_else(Gender2 == 0, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(Gender_dummy1 = if_else(Gender2 == 1, 1, 0))


###############################################################################################################
#                                       WOE - VerificationType                                                #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(VerificationType2 = case_when(
    VerificationType == 2 ~ as.numeric(1),
    is.na(VerificationType) ~ as.numeric(1),
    TRUE ~ as.numeric(VerificationType)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(VerificationType2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_VerificationType")
writeData(wb, "EE_VerificationType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN

#####################################################################################



ES_train_test <- ES_train_test %>%
  mutate(VerificationType2 = case_when(
    VerificationType == 2 ~ as.numeric(1),
    
    TRUE ~ as.numeric(VerificationType)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(VerificationType2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_VerificationType")
writeData(wb, "ES_VerificationType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(VerificationType2 = case_when(
    VerificationType == 2 ~ as.numeric(1),
    VerificationType == 3 ~ as.numeric(1),
    TRUE ~ as.numeric(VerificationType)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(VerificationType2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_VerificationType")
writeData(wb, "FI_VerificationType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#VerificationType Dummy
FI_train_test <- FI_train_test %>%
  mutate(VerificationType_dummy0 = if_else(VerificationType2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(VerificationType_dummy1 = if_else(VerificationType2 == 4, 1, 0))


###############################################################################################################
#                                         WOE - LanguageCode                                                  #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(LanguageCode2 = case_when(
    LanguageCode == 2 ~ as.numeric(3),
    LanguageCode == 4 ~ as.numeric(3),
    LanguageCode == 6 ~ as.numeric(3),
    LanguageCode == 9 ~ as.numeric(3),
    is.na(LanguageCode) ~ as.numeric(3),
    TRUE ~ as.numeric(LanguageCode)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(LanguageCode2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_LanguageCode")
writeData(wb, "EE_LanguageCode", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#VerificationType Dummy
EE_train_test <- EE_train_test %>%
  mutate(LanguageCode_dummy0 = if_else(LanguageCode2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(LanguageCode_dummy1 = if_else(LanguageCode2 == 3, 1, 0))

#####################################################################################


# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(LanguageCode) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_LanguageCode")
writeData(wb, "ES_LanguageCode", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN


#####################################################################################


# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(LanguageCode) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_LanguageCode")
writeData(wb, "FI_LanguageCode", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN



###############################################################################################################
#                                          WOE - LoanDuration                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(LoanDuration2 = case_when(
    LoanDuration > 36 ~ as.numeric(36),
    LoanDuration == 27 ~ as.numeric(24),
    LoanDuration == 18 ~ as.numeric(24),
    LoanDuration == 30 ~ as.numeric(24),
    LoanDuration < 12 ~ as.numeric(12),
    LoanDuration == 15 ~ as.numeric(12),
    TRUE ~ as.numeric(LoanDuration)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(LoanDuration2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_LoanDuration")
writeData(wb, "EE_LoanDuration", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#LoanDuration Dummy
EE_train_test <- EE_train_test %>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 36, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(LoanDuration_dummy2 = if_else(LoanDuration2 == 24, 1, 0))

#####################################################################################

ES_train_test <- ES_train_test %>%
  mutate(LoanDuration2 = case_when(
    LoanDuration < 36 ~ as.numeric(12),
    
    LoanDuration > 36 ~ as.numeric(36),
    
    TRUE ~ as.numeric(LoanDuration)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(LoanDuration2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_LoanDuration")
writeData(wb, "ES_LoanDuration", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#LoanDuration Dummy
ES_train_test <- ES_train_test %>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 36, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(LoanDuration2 = case_when(
    LoanDuration > 35 ~ as.numeric(60),
    LoanDuration == 18 ~ as.numeric(24),
    LoanDuration == 30 ~ as.numeric(24),
    LoanDuration < 12 ~ as.numeric(12),
    TRUE ~ as.numeric(LoanDuration)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(LoanDuration2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_LoanDuration")
writeData(wb, "FI_LoanDuration", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#LoanDuration Dummy
FI_train_test <- FI_train_test %>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 60, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(LoanDuration_dummy2 = if_else(LoanDuration2 == 24, 1, 0))


###############################################################################################################
#                                          WOE - UseOfLoan                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(1),
    UseOfLoan == 5 ~ as.numeric(1),
    UseOfLoan == 6 ~ as.numeric(1),
    UseOfLoan == 3 ~ as.numeric(1),
    UseOfLoan == 4 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(UseOfLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_UseOfLoan")
writeData(wb, "EE_UseOfLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#UseOfLoan Dummy
EE_train_test <- EE_train_test %>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))


EE_train_test <- EE_train_test %>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))


#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(1),
    UseOfLoan == 3 ~ as.numeric(1),
    UseOfLoan == 4 ~ as.numeric(1),
    UseOfLoan == 5 ~ as.numeric(1),
    UseOfLoan == 6 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(UseOfLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_UseOfLoan")
writeData(wb, "ES_UseOfLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#UseOfLoan Dummy
ES_train_test <- ES_train_test %>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(2),
    UseOfLoan == 3 ~ as.numeric(2),
    UseOfLoan == 4 ~ as.numeric(2),
    UseOfLoan == 5 ~ as.numeric(2),
    UseOfLoan == 6 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(UseOfLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_UseOfLoan")
writeData(wb, "FI_UseOfLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#UseOfLoan Dummy
FI_train_test <- FI_train_test %>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))


###############################################################################################################
#                                          WOE - NrOfDependants                                                    #
###############################################################################################################


# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(NrOfDependants) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_NrOfDependants")
writeData(wb, "EE_NrOfDependants", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN

#####################################################################################


# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(NrOfDependants) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_NrOfDependants")
writeData(wb, "ES_NrOfDependants", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN

#####################################################################################



# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(NrOfDependants) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_NrOfDependants")
writeData(wb, "FI_NrOfDependants", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN


###############################################################################################################
#                                          WOE - MaritalStatus                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 2 ~ as.numeric(1),
    MaritalStatus == 4 ~ as.numeric(1),
    MaritalStatus == 5 ~ as.numeric(1),
    is.na(MaritalStatus) ~ as.numeric(1),
    TRUE ~ as.numeric(MaritalStatus)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(MaritalStatus2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_MaritalStatus")
writeData(wb, "EE_MaritalStatus", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#MaritalStatus Dummy
EE_train_test <- EE_train_test %>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 3, 1, 0))



#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 2 ~ as.numeric(1),
    MaritalStatus == 3 ~ as.numeric(1),
    MaritalStatus == 4 ~ as.numeric(5),
    TRUE ~ as.numeric(MaritalStatus)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(MaritalStatus2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_MaritalStatus")
writeData(wb, "ES_MaritalStatus", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#MaritalStatus Dummy
ES_train_test <- ES_train_test %>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 5, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 3 ~ as.numeric(2),
    MaritalStatus == 4 ~ as.numeric(2),
    MaritalStatus == 5 ~ as.numeric(2),
    TRUE ~ as.numeric(MaritalStatus)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(MaritalStatus2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_MaritalStatus")
writeData(wb, "FI_MaritalStatus", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#MaritalStatus Dummy
FI_train_test <- FI_train_test %>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))


FI_train_test <- FI_train_test %>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 2, 1, 0))



###############################################################################################################
#                                          WOE - NoOfPreviousLoansBeforeLoan                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    NoOfPreviousLoansBeforeLoan < 3 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan < 10 ~ as.numeric(2),
    NoOfPreviousLoansBeforeLoan > 9 ~ as.numeric(3),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(NoOfPreviousLoansBeforeLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_NoOfPreviousLoansBeforeLoan")
writeData(wb, "EE_NoOfPreviousLoansBeforeLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NoOfPreviousLoansBeforeLoan Dummy
EE_train_test <- EE_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 2, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy2 = if_else(NoOfPreviousLoansBeforeLoan2 == 3, 1, 0))


#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    NoOfPreviousLoansBeforeLoan == 2 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan == 3 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan > 3 ~ as.numeric(3),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(NoOfPreviousLoansBeforeLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_NoOfPreviousLoansBeforeLoan")
writeData(wb, "ES_NoOfPreviousLoansBeforeLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NoOfPreviousLoansBeforeLoan Dummy
ES_train_test <- ES_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 0, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

#NoOfPreviousLoansBeforeLoan Dummy
ES_train_test <- ES_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy2 = if_else(NoOfPreviousLoansBeforeLoan2 == 3, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    
    NoOfPreviousLoansBeforeLoan < 4 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan > 3 ~ as.numeric(2),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(NoOfPreviousLoansBeforeLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_NoOfPreviousLoansBeforeLoan")
writeData(wb, "FI_NoOfPreviousLoansBeforeLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#NoOfPreviousLoansBeforeLoan Dummy
FI_train_test <- FI_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 2, 1, 0))



###############################################################################################################
#                                          WOE - Age                                                    #
###############################################################################################################


# Percentilis határok meghatározása
percentile_breaks_Age_EE <- quantile(EE_train_test$Age, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
EE_train_test <- EE_train_test %>%
  mutate(Age_cat = cut(Age, breaks = unique(percentile_breaks_Age_EE), include.lowest = TRUE),
         Age_cat_num = as.numeric(Age_cat))

EE_train_test <- EE_train_test %>%
  mutate(Age_cat_num2 = case_when(
    Age_cat_num == 1 ~ as.numeric(1),
    Age_cat_num == 2 ~ as.numeric(2),
    Age_cat_num == 3 ~ as.numeric(3),
    Age_cat_num == 4 ~ as.numeric(4),
    Age_cat_num == 5 ~ as.numeric(4),
    Age_cat_num == 6 ~ as.numeric(4),
    Age_cat_num == 7 ~ as.numeric(7),
    Age_cat_num == 8 ~ as.numeric(7),
    Age_cat_num == 9 ~ as.numeric(7),
    Age_cat_num == 10 ~ as.numeric(7),
    Age_cat_num == 11 ~ as.numeric(7),
    Age_cat_num == 12 ~ as.numeric(7),
    Age_cat_num == 13 ~ as.numeric(13),
    Age_cat_num == 14 ~ as.numeric(13),
    Age_cat_num == 15 ~ as.numeric(13),
    Age_cat_num == 16 ~ as.numeric(13),
    Age_cat_num == 17 ~ as.numeric(13),
    Age_cat_num == 18 ~ as.numeric(13),
    Age_cat_num == 19 ~ as.numeric(13),
    Age_cat_num == 20 ~ as.numeric(13),
    TRUE ~ as.numeric(Age_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(Age_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_Age")
writeData(wb, "EE_Age", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#Age Dummy
EE_train_test <- EE_train_test %>%
  mutate(Age_dummy0 = if_else(Age_cat_num2 == 7, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Age_dummy1 = if_else(Age_cat_num2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Age_dummy2 = if_else(Age_cat_num2 == 2, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Age_dummy3 = if_else(Age_cat_num2 == 3, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Age_dummy4 = if_else(Age_cat_num2 == 4, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(Age_dummy5 = if_else(Age_cat_num2 == 13, 1, 0))


#####################################################################################



# Percentilis határok meghatározása
percentile_breaks_Age_ES <- quantile(ES_train_test$Age, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
ES_train_test <- ES_train_test %>%
  mutate(Age_cat = cut(Age, breaks = unique(percentile_breaks_Age_ES), include.lowest = TRUE),
         Age_cat_num = as.numeric(Age_cat))

ES_train_test <- ES_train_test %>%
  mutate(Age_cat_num2 = case_when(
    Age_cat_num == 1 ~ as.numeric(1),
    Age_cat_num == 2 ~ as.numeric(2),
    Age_cat_num == 3 ~ as.numeric(2),
    Age_cat_num == 4 ~ as.numeric(4),
    Age_cat_num == 5 ~ as.numeric(4),
    Age_cat_num == 6 ~ as.numeric(4),
    Age_cat_num == 7 ~ as.numeric(4),
    Age_cat_num == 8 ~ as.numeric(4),
    Age_cat_num == 9 ~ as.numeric(4),
    Age_cat_num == 10 ~ as.numeric(4),
    Age_cat_num == 11 ~ as.numeric(11),
    Age_cat_num == 12 ~ as.numeric(12),
    Age_cat_num == 13 ~ as.numeric(12),
    Age_cat_num == 14 ~ as.numeric(12),
    Age_cat_num == 15 ~ as.numeric(12),
    Age_cat_num == 16 ~ as.numeric(16),
    Age_cat_num == 17 ~ as.numeric(17),
    Age_cat_num == 18 ~ as.numeric(17),
    Age_cat_num == 19 ~ as.numeric(17),
    Age_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(Age_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(Age_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_Age")
writeData(wb, "ES_Age", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NEM HOZOK LÉTRE VÁLTOZÓT AZ EREDMÉNYEK ALAPJÁN



#####################################################################################

# Percentilis határok meghatározása
percentile_breaks_Age_FI <- quantile(FI_train_test$Age, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
FI_train_test <- FI_train_test %>%
  mutate(Age_cat = cut(Age, breaks = unique(percentile_breaks_Age_FI), include.lowest = TRUE),
         Age_cat_num = as.numeric(Age_cat))

FI_train_test <- FI_train_test %>%
  mutate(Age_cat_num2 = case_when(
    Age_cat_num == 1 ~ as.numeric(1),
    Age_cat_num == 2 ~ as.numeric(1),
    Age_cat_num == 3 ~ as.numeric(1),
    Age_cat_num == 4 ~ as.numeric(1),
    Age_cat_num == 5 ~ as.numeric(1),
    Age_cat_num == 6 ~ as.numeric(1),
    Age_cat_num == 7 ~ as.numeric(1),
    Age_cat_num == 8 ~ as.numeric(1),
    Age_cat_num == 9 ~ as.numeric(1),
    Age_cat_num == 10 ~ as.numeric(1),
    Age_cat_num == 11 ~ as.numeric(1),
    Age_cat_num == 12 ~ as.numeric(1),
    Age_cat_num == 13 ~ as.numeric(1),
    Age_cat_num == 14 ~ as.numeric(1),
    Age_cat_num == 15 ~ as.numeric(1),
    Age_cat_num == 16 ~ as.numeric(1),
    Age_cat_num == 17 ~ as.numeric(1),
    Age_cat_num == 18 ~ as.numeric(1),
    Age_cat_num == 19 ~ as.numeric(19),
    Age_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(Age_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(Age_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_Age")
writeData(wb, "FI_Age", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#Age Dummy
FI_train_test <- FI_train_test %>%
  mutate(Age_dummy0 = if_else(Age_cat_num2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(Age_dummy1 = if_else(Age_cat_num2 == 19, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(Age_dummy2 = if_else(Age_cat_num2 == 20, 1, 0))




###############################################################################################################
#                                          WOE - PreviousEarlyRepaymentsCountBeforeLoan                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(PreviousEarlyRepaymentsCountBeforeLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_PERepaymentsCountBeforeLoan")
writeData(wb, "EE_PERepaymentsCountBeforeLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
EE_train_test <- EE_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))


#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(PreviousEarlyRepaymentsCountBeforeLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_PERepaymentsCountBeforeLoan")
writeData(wb, "ES_PERepaymentsCountBeforeLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
ES_train_test <- ES_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))



#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(PreviousEarlyRepaymentsCountBeforeLoan2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_PERepaymentsCountBeforeLoan")
writeData(wb, "FI_PERepaymentsCountBeforeLoan", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#PreviousEarlyRepaymentsCountBeforeLoan Dummy
FI_train_test <- FI_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))





###############################################################################################################
#                                          WOE - AppliedAmount                                                    #
###############################################################################################################


# Percentilis határok meghatározása
percentile_breaks_AppliedAmount_EE <- quantile(EE_train_test$AppliedAmount, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
EE_train_test <- EE_train_test %>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_EE), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

asdqwe <- EE_train_test %>% 
  group_by(AppliedAmount_cat,AppliedAmount_cat_num) %>% 
  summarise(count = n())

EE_train_test <- EE_train_test %>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(2),
    AppliedAmount_cat_num == 4 ~ as.numeric(2),
    AppliedAmount_cat_num == 5 ~ as.numeric(2),
    AppliedAmount_cat_num == 6 ~ as.numeric(2),
    AppliedAmount_cat_num == 7 ~ as.numeric(2),
    AppliedAmount_cat_num == 8 ~ as.numeric(8),
    AppliedAmount_cat_num == 9 ~ as.numeric(8),
    AppliedAmount_cat_num == 10 ~ as.numeric(8),
    AppliedAmount_cat_num == 11 ~ as.numeric(8),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    AppliedAmount_cat_num == 15 ~ as.numeric(12),
    AppliedAmount_cat_num == 16 ~ as.numeric(12),
    AppliedAmount_cat_num == 17 ~ as.numeric(12),
    AppliedAmount_cat_num == 18 ~ as.numeric(12),
    AppliedAmount_cat_num == 19 ~ as.numeric(12),
    AppliedAmount_cat_num == 20 ~ as.numeric(12),
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(AppliedAmount_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_AppliedAmount")
writeData(wb, "EE_AppliedAmount", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#AppliedAmount Dummy
EE_train_test <- EE_train_test %>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 8, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))


#####################################################################################



# Percentilis határok meghatározása
percentile_breaks_AppliedAmount_ES <- quantile(ES_train_test$AppliedAmount, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
ES_train_test <- ES_train_test %>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_ES), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

ES_train_test <- ES_train_test %>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(2),
    AppliedAmount_cat_num == 4 ~ as.numeric(2),
    AppliedAmount_cat_num == 5 ~ as.numeric(2),
    AppliedAmount_cat_num == 6 ~ as.numeric(6),
    AppliedAmount_cat_num == 7 ~ as.numeric(6),
    AppliedAmount_cat_num == 8 ~ as.numeric(6),
    AppliedAmount_cat_num == 9 ~ as.numeric(9),
    AppliedAmount_cat_num == 10 ~ as.numeric(9),
    AppliedAmount_cat_num == 11 ~ as.numeric(9),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(AppliedAmount_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_AppliedAmount")
writeData(wb, "ES_AppliedAmount", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#AppliedAmount Dummy
ES_train_test <- ES_train_test %>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 6, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 9, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(AppliedAmount_dummy4 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))



#####################################################################################

# Percentilis határok meghatározása
percentile_breaks_AppliedAmount_FI <- quantile(FI_train_test$AppliedAmount, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_FI), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(3),
    AppliedAmount_cat_num == 4 ~ as.numeric(4),
    AppliedAmount_cat_num == 5 ~ as.numeric(5),
    AppliedAmount_cat_num == 6 ~ as.numeric(5),
    AppliedAmount_cat_num == 7 ~ as.numeric(5),
    AppliedAmount_cat_num == 8 ~ as.numeric(5),
    AppliedAmount_cat_num == 9 ~ as.numeric(9),
    AppliedAmount_cat_num == 10 ~ as.numeric(9),
    AppliedAmount_cat_num == 11 ~ as.numeric(9),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    AppliedAmount_cat_num == 15 ~ as.numeric(12),
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(AppliedAmount_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_AppliedAmount")
writeData(wb, "FI_AppliedAmount", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#AppliedAmount Dummy
FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 3, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 4, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_dummy4 = if_else(AppliedAmount_cat_num2 == 5, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_dummy5 = if_else(AppliedAmount_cat_num2 == 9, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(AppliedAmount_dummy6 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))





###############################################################################################################
#                                          WOE - MonthlyPaymentDay                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(MonthlyPaymentDay2 = case_when(
    MonthlyPaymentDay < 15 ~ as.numeric(1),
    MonthlyPaymentDay < 40 ~ as.numeric(2),
    is.na(MonthlyPaymentDay) ~ as.numeric(1),
    TRUE ~ as.numeric(MonthlyPaymentDay)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(MonthlyPaymentDay2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_MonthlyPaymentDay")
writeData(wb, "EE_MonthlyPaymentDay", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#MonthlyPaymentDay Dummy
EE_train_test <- EE_train_test %>%
  mutate(MonthlyPaymentDay_dummy0 = if_else(MonthlyPaymentDay2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(MonthlyPaymentDay_dummy1 = if_else(MonthlyPaymentDay2 == 2, 1, 0))



#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(MonthlyPaymentDay2 = case_when(
    MonthlyPaymentDay < 15 ~ as.numeric(1),
    MonthlyPaymentDay < 40 ~ as.numeric(2),
    is.na(MonthlyPaymentDay) ~ as.numeric(1),
    TRUE ~ as.numeric(MonthlyPaymentDay)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(MonthlyPaymentDay2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_MonthlyPaymentDay")
writeData(wb, "ES_MonthlyPaymentDay", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#MonthlyPaymentDay Dummy
ES_train_test <- ES_train_test %>%
  mutate(MonthlyPaymentDay_dummy0 = if_else(MonthlyPaymentDay2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(MonthlyPaymentDay_dummy1 = if_else(MonthlyPaymentDay2 == 2, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(MonthlyPaymentDay2 = case_when(
    MonthlyPaymentDay < 15 ~ as.numeric(1),
    MonthlyPaymentDay < 40 ~ as.numeric(2),
    is.na(MonthlyPaymentDay) ~ as.numeric(1),
    TRUE ~ as.numeric(MonthlyPaymentDay)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(MonthlyPaymentDay2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_MonthlyPaymentDay")
writeData(wb, "FI_MonthlyPaymentDay", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

# ITT NEM JÓ A VÁLTOZÓ




###############################################################################################################
#                                          WOE - OccupationArea                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == 8 ~ as.numeric(1),
    OccupationArea == 7 ~ as.numeric(1),
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 12 ~ as.numeric(1),
    OccupationArea == 14 ~ as.numeric(1),
    OccupationArea == 18 ~ as.numeric(10),
    OccupationArea == 5 ~ as.numeric(3),
    OccupationArea == 4 ~ as.numeric(3),
    OccupationArea == 13 ~ as.numeric(3),
    OccupationArea == 9 ~ as.numeric(3),
    OccupationArea == 6 ~ as.numeric(3),
    OccupationArea == 19 ~ as.numeric(3),
    OccupationArea == 11 ~ as.numeric(10),
    OccupationArea == 15 ~ as.numeric(10),
    OccupationArea == 16 ~ as.numeric(10),
    OccupationArea == 17 ~ as.numeric(10),
    OccupationArea == 2 ~ as.numeric(3),
    is.na(OccupationArea) ~ as.numeric(3),
    TRUE ~ as.numeric(OccupationArea) 
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(OccupationArea2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_OccupationArea")
writeData(wb, "EE_OccupationArea", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#OccupationArea Dummy
EE_train_test <- EE_train_test %>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 3, 1, 0))

#OccupationArea Dummy
EE_train_test <- EE_train_test %>%
  mutate(OccupationArea_dummy2 = if_else(OccupationArea2 == 10, 1, 0))



#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 1 ~ as.numeric(1),
    OccupationArea == 2 ~ as.numeric(2),
    OccupationArea == 3 ~ as.numeric(1),
    OccupationArea == 4 ~ as.numeric(2),
    OccupationArea == 5 ~ as.numeric(2),
    OccupationArea == 6 ~ as.numeric(2),
    OccupationArea == 7 ~ as.numeric(1),
    OccupationArea == 8 ~ as.numeric(2),
    OccupationArea == 9 ~ as.numeric(2),
    OccupationArea == 10 ~ as.numeric(10),
    OccupationArea == 11 ~ as.numeric(10),
    OccupationArea == 12 ~ as.numeric(10),
    OccupationArea == 13 ~ as.numeric(10),
    OccupationArea == 14 ~ as.numeric(10),
    OccupationArea == 15 ~ as.numeric(10),
    OccupationArea == 16 ~ as.numeric(10),
    OccupationArea == 17 ~ as.numeric(2),
    OccupationArea == 18 ~ as.numeric(1),
    OccupationArea == 19 ~ as.numeric(2),
    is.na(OccupationArea) ~ as.numeric(1),
    TRUE ~ as.numeric(OccupationArea)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(OccupationArea2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_OccupationArea")
writeData(wb, "ES_OccupationArea", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#OccupationArea Dummy
#OccupationArea Dummy
ES_train_test <- ES_train_test %>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 2, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(OccupationArea_dummy2 = if_else(OccupationArea2 == 10, 1, 0))



#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 1 ~ as.numeric(1),
    OccupationArea > 2 ~ as.numeric(2),
    
    is.na(OccupationArea) ~ as.numeric(1),
    TRUE ~ as.numeric(OccupationArea)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(OccupationArea2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_OccupationArea")
writeData(wb, "FI_OccupationArea", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#OccupationArea Dummy
FI_train_test <- FI_train_test %>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 2, 1, 0))



###############################################################################################################
#                                          WOE - IncomeTotal                                                    #
###############################################################################################################


# Percentilis határok meghatározása
percentile_breaks_IncomeTotal_EE <- quantile(EE_train_test$IncomeTotal, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
EE_train_test <- EE_train_test %>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_EE), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

asdqwe <- EE_train_test %>% 
  group_by(IncomeTotal_cat,IncomeTotal_cat_num) %>% 
  summarise(count = n())

EE_train_test <- EE_train_test %>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(1),
    IncomeTotal_cat_num == 2 ~ as.numeric(2),
    IncomeTotal_cat_num == 3 ~ as.numeric(2),
    IncomeTotal_cat_num == 4 ~ as.numeric(2),
    IncomeTotal_cat_num == 5 ~ as.numeric(2),
    IncomeTotal_cat_num == 6 ~ as.numeric(2),
    IncomeTotal_cat_num == 7 ~ as.numeric(2),
    IncomeTotal_cat_num == 8 ~ as.numeric(2),
    IncomeTotal_cat_num == 9 ~ as.numeric(2),
    IncomeTotal_cat_num == 10 ~ as.numeric(2),
    IncomeTotal_cat_num == 11 ~ as.numeric(11),
    IncomeTotal_cat_num == 12 ~ as.numeric(11),
    IncomeTotal_cat_num == 13 ~ as.numeric(13),
    IncomeTotal_cat_num == 14 ~ as.numeric(13),
    IncomeTotal_cat_num == 15 ~ as.numeric(13),
    IncomeTotal_cat_num == 16 ~ as.numeric(13),
    IncomeTotal_cat_num == 17 ~ as.numeric(17),
    IncomeTotal_cat_num == 18 ~ as.numeric(17),
    IncomeTotal_cat_num == 19 ~ as.numeric(19),
    IncomeTotal_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(IncomeTotal_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_IncomeTotal")
writeData(wb, "EE_IncomeTotal", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeTotal Dummy
EE_train_test <- EE_train_test %>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 11, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 2, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 13, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(IncomeTotal_dummy4 = if_else(IncomeTotal_cat_num2 == 17, 1, 0))


#####################################################################################



# Percentilis határok meghatározása
percentile_breaks_IncomeTotal_ES <- quantile(ES_train_test$IncomeTotal, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
ES_train_test <- ES_train_test %>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_ES), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

ES_train_test <- ES_train_test %>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(1),
    IncomeTotal_cat_num == 2 ~ as.numeric(1),
    IncomeTotal_cat_num == 3 ~ as.numeric(1),
    IncomeTotal_cat_num == 4 ~ as.numeric(1),
    IncomeTotal_cat_num == 5 ~ as.numeric(1),
    IncomeTotal_cat_num == 6 ~ as.numeric(1),
    IncomeTotal_cat_num == 7 ~ as.numeric(1),
    IncomeTotal_cat_num == 8 ~ as.numeric(1),
    IncomeTotal_cat_num == 9 ~ as.numeric(1),
    IncomeTotal_cat_num == 10 ~ as.numeric(1),
    IncomeTotal_cat_num == 11 ~ as.numeric(1),
    IncomeTotal_cat_num == 12 ~ as.numeric(1),
    IncomeTotal_cat_num == 13 ~ as.numeric(13),
    IncomeTotal_cat_num == 14 ~ as.numeric(13),
    IncomeTotal_cat_num == 15 ~ as.numeric(13),
    IncomeTotal_cat_num == 16 ~ as.numeric(16),
    IncomeTotal_cat_num == 17 ~ as.numeric(16),
    IncomeTotal_cat_num == 18 ~ as.numeric(18),
    IncomeTotal_cat_num == 19 ~ as.numeric(18),
    
    
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(IncomeTotal_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_IncomeTotal")
writeData(wb, "ES_IncomeTotal", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeTotal Dummy
ES_train_test <- ES_train_test %>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 13, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 16, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 18, 1, 0))




#####################################################################################

# Percentilis határok meghatározása
percentile_breaks_IncomeTotal_FI <- quantile(FI_train_test$IncomeTotal, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
FI_train_test <- FI_train_test %>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_FI), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

FI_train_test <- FI_train_test %>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(2),
    IncomeTotal_cat_num == 2 ~ as.numeric(2),
    IncomeTotal_cat_num == 3 ~ as.numeric(2),
    IncomeTotal_cat_num == 4 ~ as.numeric(2),
    IncomeTotal_cat_num == 5 ~ as.numeric(2),
    IncomeTotal_cat_num == 6 ~ as.numeric(2),
    IncomeTotal_cat_num == 7 ~ as.numeric(2),
    IncomeTotal_cat_num == 8 ~ as.numeric(2),
    IncomeTotal_cat_num == 9 ~ as.numeric(2),
    IncomeTotal_cat_num == 10 ~ as.numeric(2),
    IncomeTotal_cat_num == 11 ~ as.numeric(11),
    IncomeTotal_cat_num == 12 ~ as.numeric(11),
    IncomeTotal_cat_num == 13 ~ as.numeric(11),
    IncomeTotal_cat_num == 14 ~ as.numeric(11),
    IncomeTotal_cat_num == 15 ~ as.numeric(11),
    IncomeTotal_cat_num == 16 ~ as.numeric(16),
    IncomeTotal_cat_num == 17 ~ as.numeric(16),
    IncomeTotal_cat_num == 18 ~ as.numeric(18),
    IncomeTotal_cat_num == 19 ~ as.numeric(18),
    IncomeTotal_cat_num == 20 ~ as.numeric(18),
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(IncomeTotal_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_IncomeTotal")
writeData(wb, "FI_IncomeTotal", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeTotal Dummy
#IncomeTotal Dummy
FI_train_test <- FI_train_test %>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 11, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 2, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 16, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 18, 1, 0))




###############################################################################################################
#                                          WOE - IncomeFromPrincipalEmployer                                                    #
###############################################################################################################


# Percentilis határok meghatározása
percentile_breaks_IncomeFromPrincipalEmployer_EE <- quantile(EE_train_test$IncomeFromPrincipalEmployer, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
EE_train_test <- EE_train_test %>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_EE), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

asdqwe <- EE_train_test %>% 
  group_by(IncomeFromPrincipalEmployer_cat,IncomeFromPrincipalEmployer_cat_num) %>% 
  summarise(count = n())

EE_train_test <- EE_train_test %>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 3 ~ as.numeric(3),
    
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(IncomeFromPrincipalEmployer_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_IncomeFromPrinc")
writeData(wb, "EE_IncomeFromPrinc", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeFromPrincipalEmployer Dummy
EE_train_test <- EE_train_test %>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 3, 1, 0))



#####################################################################################



# Percentilis határok meghatározása
percentile_breaks_IncomeFromPrincipalEmployer_ES <- quantile(ES_train_test$IncomeFromPrincipalEmployer, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
ES_train_test <- ES_train_test %>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_ES), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

asdqwe <- EE_train_test %>% 
  group_by(IncomeFromPrincipalEmployer_cat,IncomeFromPrincipalEmployer_cat_num) %>% 
  summarise(count = n())

ES_train_test <- ES_train_test %>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 3 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 4 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 5 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 6 ~ as.numeric(2),
    
    
    
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(IncomeFromPrincipalEmployer_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_IncomeFromPrincr")
writeData(wb, "ES_IncomeFromPrincr", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeFromPrincipalEmployer Dummy
ES_train_test <- ES_train_test %>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 2, 1, 0))



#####################################################################################

# Percentilis határok meghatározása
percentile_breaks_IncomeFromPrincipalEmployer_FI <- quantile(FI_train_test$IncomeFromPrincipalEmployer, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
FI_train_test <- FI_train_test %>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_FI), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

FI_train_test <- FI_train_test %>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(2),
    
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(IncomeFromPrincipalEmployer_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_IncomeFromPrinc")
writeData(wb, "FI_IncomeFromPrinc", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeFromPrincipalEmployer Dummy
#IncomeFromPrincipalEmployer Dummy
FI_train_test <- FI_train_test %>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 2, 1, 0))



###############################################################################################################
#                                          WOE - IncomeOtherType                                                    #
###############################################################################################################


# Percentilis határok meghatározása
percentile_breaks_IncomeOtherType_EE <- quantile(EE_train_test$IncomeOtherType, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
EE_train_test <- EE_train_test %>%
  mutate(IncomeOtherType_cat = cut(IncomeOtherType, breaks = unique(percentile_breaks_IncomeOtherType_EE), include.lowest = TRUE),
         IncomeOtherType_cat_num = as.numeric(IncomeOtherType_cat))

asdqwe <- EE_train_test %>% 
  group_by(IncomeOtherType_cat,IncomeOtherType_cat_num) %>% 
  summarise(count = n())

EE_train_test <- EE_train_test %>%
  mutate(IncomeOtherType_cat_num2 = case_when(
    IncomeOtherType_cat_num == 1 ~ as.numeric(1),
    IncomeOtherType_cat_num == 2 ~ as.numeric(2),
    IncomeOtherType_cat_num == 3 ~ as.numeric(3),
    IncomeOtherType_cat_num == 4 ~ as.numeric(4),
    IncomeOtherType_cat_num == 5 ~ as.numeric(5),
    IncomeOtherType_cat_num == 6 ~ as.numeric(6),
    IncomeOtherType_cat_num == 7 ~ as.numeric(7),
    IncomeOtherType_cat_num == 8 ~ as.numeric(8),
    IncomeOtherType_cat_num == 9 ~ as.numeric(9),
    IncomeOtherType_cat_num == 10 ~ as.numeric(10),
    IncomeOtherType_cat_num == 11 ~ as.numeric(11),
    IncomeOtherType_cat_num == 12 ~ as.numeric(12),
    IncomeOtherType_cat_num == 13 ~ as.numeric(13),
    IncomeOtherType_cat_num == 14 ~ as.numeric(14),
    IncomeOtherType_cat_num == 15 ~ as.numeric(15),
    IncomeOtherType_cat_num == 16 ~ as.numeric(16),
    IncomeOtherType_cat_num == 17 ~ as.numeric(17),
    IncomeOtherType_cat_num == 18 ~ as.numeric(18),
    IncomeOtherType_cat_num == 19 ~ as.numeric(19),
    IncomeOtherType_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeOtherType_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(IncomeOtherType_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_IncomeOtherType")
writeData(wb, "EE_IncomeOtherType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeOtherType Dummy
EE_train_test <- EE_train_test %>%
  mutate(IncomeOtherType_dummy0 = if_else(IncomeOtherType_cat_num2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(IncomeOtherType_dummy1 = if_else(IncomeOtherType_cat_num2 == 2, 1, 0))




#####################################################################################



# Percentilis határok meghatározása
percentile_breaks_IncomeOtherType_ES <- quantile(ES_train_test$IncomeOtherType, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
ES_train_test <- ES_train_test %>%
  mutate(IncomeOtherType_cat = cut(IncomeOtherType, breaks = unique(percentile_breaks_IncomeOtherType_ES), include.lowest = TRUE),
         IncomeOtherType_cat_num = as.numeric(IncomeOtherType_cat))

ES_train_test <- ES_train_test %>%
  mutate(IncomeOtherType_cat_num2 = case_when(
    IncomeOtherType_cat_num == 1 ~ as.numeric(1),
    IncomeOtherType_cat_num == 2 ~ as.numeric(2),
    IncomeOtherType_cat_num == 3 ~ as.numeric(3),
    IncomeOtherType_cat_num == 4 ~ as.numeric(4),
    IncomeOtherType_cat_num == 5 ~ as.numeric(5),
    IncomeOtherType_cat_num == 6 ~ as.numeric(6),
    IncomeOtherType_cat_num == 7 ~ as.numeric(7),
    IncomeOtherType_cat_num == 8 ~ as.numeric(8),
    IncomeOtherType_cat_num == 9 ~ as.numeric(9),
    IncomeOtherType_cat_num == 10 ~ as.numeric(10),
    IncomeOtherType_cat_num == 11 ~ as.numeric(11),
    IncomeOtherType_cat_num == 12 ~ as.numeric(12),
    IncomeOtherType_cat_num == 13 ~ as.numeric(13),
    IncomeOtherType_cat_num == 14 ~ as.numeric(14),
    IncomeOtherType_cat_num == 15 ~ as.numeric(15),
    IncomeOtherType_cat_num == 16 ~ as.numeric(16),
    IncomeOtherType_cat_num == 17 ~ as.numeric(17),
    IncomeOtherType_cat_num == 18 ~ as.numeric(18),
    IncomeOtherType_cat_num == 19 ~ as.numeric(19),
    IncomeOtherType_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeOtherType_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(IncomeOtherType_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_IncomeOtherType")
writeData(wb, "ES_IncomeOtherType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#IncomeOtherType Dummy
ES_train_test <- ES_train_test %>%
  mutate(IncomeOtherType_dummy0 = if_else(IncomeOtherType_cat_num2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(IncomeOtherType_dummy1 = if_else(IncomeOtherType_cat_num2 == 2, 1, 0))




#####################################################################################

# Új flag oszlop létrehozása
FI_train_test <- FI_train_test %>%
  mutate(IncomeOtherType_flag = if_else(IncomeOtherType == 0, 0, 1))


# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(IncomeOtherType_flag) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_IncomeOtherType")
writeData(wb, "FI_IncomeOtherType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NINCS VÁLTOZÓ MERT TÚL KEVÁS A MEGFIGYELÉS




###############################################################################################################
#                                          WOE - DebtToIncome                                                    #
###############################################################################################################


# Percentilis határok meghatározása
percentile_breaks_DebtToIncome_EE <- quantile(EE_train_test$DebtToIncome, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
EE_train_test <- EE_train_test %>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_EE), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

asdqwe <- EE_train_test %>% 
  group_by(DebtToIncome_cat,DebtToIncome_cat_num) %>% 
  summarise(count = n())

EE_train_test <- EE_train_test %>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(1),
    DebtToIncome_cat_num == 3 ~ as.numeric(1),
    DebtToIncome_cat_num == 4 ~ as.numeric(4),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(6),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(DebtToIncome_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_DebtToIncome")
writeData(wb, "EE_DebtToIncome", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#DebtToIncome Dummy
EE_train_test <- EE_train_test %>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 4, 1, 0))




#####################################################################################



# Percentilis határok meghatározása
percentile_breaks_DebtToIncome_ES <- quantile(ES_train_test$DebtToIncome, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
ES_train_test <- ES_train_test %>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_ES), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

ES_train_test <- ES_train_test %>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(1),
    DebtToIncome_cat_num == 3 ~ as.numeric(1),
    DebtToIncome_cat_num == 4 ~ as.numeric(1),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(5),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(DebtToIncome_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_DebtToIncome")
writeData(wb, "ES_DebtToIncome", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#DebtToIncome Dummy
ES_train_test <- ES_train_test %>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 5, 1, 0))





#####################################################################################

# Percentilis határok meghatározása
percentile_breaks_DebtToIncome_FI <- quantile(FI_train_test$DebtToIncome, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
FI_train_test <- FI_train_test %>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_FI), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

FI_train_test <- FI_train_test %>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(2),
    DebtToIncome_cat_num == 3 ~ as.numeric(3),
    DebtToIncome_cat_num == 4 ~ as.numeric(4),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(6),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(DebtToIncome_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_DebtToIncome")
writeData(wb, "FI_DebtToIncome", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#DebtToIncome Dummy
FI_train_test <- FI_train_test %>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 2, 1, 0))




###############################################################################################################
#                                          WOE - LiabilitiesTotal                                                    #
###############################################################################################################


# Percentilis határok meghatározása
percentile_breaks_LiabilitiesTotal_EE <- quantile(EE_train_test$LiabilitiesTotal, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
EE_train_test <- EE_train_test %>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_EE), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

asdqwe <- EE_train_test %>% 
  group_by(LiabilitiesTotal_cat,LiabilitiesTotal_cat_num) %>% 
  summarise(count = n())

EE_train_test <- EE_train_test %>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(14),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(LiabilitiesTotal_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_LiabilitiesTotal")
writeData(wb, "EE_LiabilitiesTotal", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#LiabilitiesTotal Dummy
EE_train_test <- EE_train_test %>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 4, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))




#####################################################################################



# Percentilis határok meghatározása
percentile_breaks_LiabilitiesTotal_ES <- quantile(ES_train_test$LiabilitiesTotal, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
ES_train_test <- ES_train_test %>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_ES), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

ES_train_test <- ES_train_test %>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(16),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(LiabilitiesTotal_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_LiabilitiesTotal")
writeData(wb, "ES_LiabilitiesTotal", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#LiabilitiesTotal Dummy
ES_train_test <- ES_train_test %>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 10, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(LiabilitiesTotal_dummy3 = if_else(LiabilitiesTotal_cat_num2 == 16, 1, 0))





#####################################################################################

# Percentilis határok meghatározása
percentile_breaks_LiabilitiesTotal_FI <- quantile(FI_train_test$LiabilitiesTotal, probs = seq(0, 1, by = 0.05), na.rm = TRUE, type = 7)

# Kategóriák létrehozása a percentilis határok alapján
FI_train_test <- FI_train_test %>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_FI), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

FI_train_test <- FI_train_test %>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(17),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(18),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(19),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(20),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(LiabilitiesTotal_cat_num2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_LiabilitiesTotal")
writeData(wb, "FI_LiabilitiesTotal", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#LiabilitiesTotal Dummy
FI_train_test <- FI_train_test %>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 11, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))





###############################################################################################################
#                                          WOE - ExistingLiabilities                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 2 ~ as.numeric(1),
    ExistingLiabilities == 3 ~ as.numeric(1),
    ExistingLiabilities == 4 ~ as.numeric(1),
    ExistingLiabilities > 5 ~ as.numeric(5),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(ExistingLiabilities2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_ExistingLiabilities")
writeData(wb, "EE_ExistingLiabilities", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#ExistingLiabilities Dummy
EE_train_test <- EE_train_test %>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 5, 1, 0))


#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 2 ~ as.numeric(1),
    ExistingLiabilities > 3 ~ as.numeric(3),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(ExistingLiabilities2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_ExistingLiabilities")
writeData(wb, "ES_ExistingLiabilities", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#ExistingLiabilities Dummy
ES_train_test <- ES_train_test %>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 1, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

#ExistingLiabilities Dummy
ES_train_test <- ES_train_test %>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 3, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 3 ~ as.numeric(2),
    ExistingLiabilities == 4 ~ as.numeric(2),
    ExistingLiabilities == 5 ~ as.numeric(2),
    ExistingLiabilities > 6 ~ as.numeric(6),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(ExistingLiabilities2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_ExistingLiabilities")
writeData(wb, "FI_ExistingLiabilities", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#ExistingLiabilities Dummy
FI_train_test <- FI_train_test %>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 2, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(ExistingLiabilities_dummy3 = if_else(ExistingLiabilities2 == 6, 1, 0))



###############################################################################################################
#                                          WOE - HomeOwnershipType                                                    #
###############################################################################################################


EE_train_test <- EE_train_test %>%
  mutate(HomeOwnershipType2 = case_when(
    HomeOwnershipType == -1 ~ as.numeric(1),
    HomeOwnershipType == 0 ~ as.numeric(1),
    HomeOwnershipType == 2 ~ as.numeric(2),
    HomeOwnershipType == 3 ~ as.numeric(2),
    HomeOwnershipType == 4 ~ as.numeric(2),
    HomeOwnershipType == 5 ~ as.numeric(2),
    HomeOwnershipType == 6 ~ as.numeric(2),
    HomeOwnershipType == 7 ~ as.numeric(1),
    HomeOwnershipType == 8 ~ as.numeric(1),
    HomeOwnershipType == 9 ~ as.numeric(1),
    HomeOwnershipType == 10 ~ as.numeric(1),
    is.na(HomeOwnershipType) ~ as.numeric(1),
    TRUE ~ as.numeric(HomeOwnershipType)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(HomeOwnershipType2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_HomeOwnershipType")
writeData(wb, "EE_HomeOwnershipType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#HomeOwnershipType Dummy
EE_train_test <- EE_train_test %>%
  mutate(HomeOwnershipType_dummy0 = if_else(HomeOwnershipType2 == 1, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(HomeOwnershipType_dummy1 = if_else(HomeOwnershipType2 == 2, 1, 0))




#####################################################################################


ES_train_test <- ES_train_test %>%
  mutate(HomeOwnershipType2 = case_when(
    HomeOwnershipType == -1 ~ as.numeric(1),
    HomeOwnershipType == 0 ~ as.numeric(1),
    HomeOwnershipType == 2 ~ as.numeric(2),
    HomeOwnershipType == 3 ~ as.numeric(2),
    HomeOwnershipType == 4 ~ as.numeric(2),
    HomeOwnershipType == 5 ~ as.numeric(2),
    HomeOwnershipType == 6 ~ as.numeric(2),
    HomeOwnershipType == 7 ~ as.numeric(1),
    HomeOwnershipType == 8 ~ as.numeric(1),
    HomeOwnershipType == 9 ~ as.numeric(1),
    HomeOwnershipType == 10 ~ as.numeric(1),
    is.na(HomeOwnershipType) ~ as.numeric(1),
    TRUE ~ as.numeric(HomeOwnershipType)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(HomeOwnershipType2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_HomeOwnershipType")
writeData(wb, "ES_HomeOwnershipType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NINCS VÁLTOZÓ AZ EREDMÉNYEK ALAPJÁN


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(HomeOwnershipType2 = case_when(
    HomeOwnershipType == -1 ~ as.numeric(1),
    HomeOwnershipType == 0 ~ as.numeric(1),
    HomeOwnershipType == 2 ~ as.numeric(2),
    HomeOwnershipType == 3 ~ as.numeric(2),
    HomeOwnershipType == 4 ~ as.numeric(2),
    HomeOwnershipType == 5 ~ as.numeric(2),
    HomeOwnershipType == 6 ~ as.numeric(2),
    HomeOwnershipType == 7 ~ as.numeric(1),
    HomeOwnershipType == 8 ~ as.numeric(1),
    HomeOwnershipType == 9 ~ as.numeric(1),
    HomeOwnershipType == 10 ~ as.numeric(1),
    is.na(HomeOwnershipType) ~ as.numeric(1),
    TRUE ~ as.numeric(HomeOwnershipType)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(HomeOwnershipType2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_HomeOwnershipType")
writeData(wb, "FI_HomeOwnershipType", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#HomeOwnershipType Dummy
FI_train_test <- FI_train_test %>%
  mutate(HomeOwnershipType_dummy0 = if_else(HomeOwnershipType2 == 1, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(HomeOwnershipType_dummy1 = if_else(HomeOwnershipType2 == 2, 1, 0))






###############################################################################################################
#                                       WOE - EmploymentDurationCurrentEmployer                                                #
###############################################################################################################


# A karakterláncok numerikus kódolása
EE_train_test <- EE_train_test %>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 1,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 7,
    EmploymentDurationCurrentEmployer == "Other" ~ 1,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(EmploymentDurationCurrentEmployer2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_DurationCurrentEmployer")
writeData(wb, "EE_DurationCurrentEmployer", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#HomeOwnershipType Dummy
EE_train_test <- EE_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

EE_train_test <- EE_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#HomeOwnershipType Dummy
EE_train_test <- EE_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 7, 1, 0))

#####################################################################################



ES_train_test <- ES_train_test %>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 1,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 7,
    EmploymentDurationCurrentEmployer == "Other" ~ 1,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(EmploymentDurationCurrentEmployer2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_DurationCurrentEmployer")
writeData(wb, "ES_DurationCurrentEmployer", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#HomeOwnershipType Dummy
ES_train_test <- ES_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

ES_train_test <- ES_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#HomeOwnershipType Dummy
ES_train_test <- ES_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 7, 1, 0))


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 6,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 4,
    EmploymentDurationCurrentEmployer == "Other" ~ 6,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(EmploymentDurationCurrentEmployer2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_DurationCurrentEmployer")
writeData(wb, "FI_DurationCurrentEmployer", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#EmploymentDurationCurrentEmployer Dummy
FI_train_test <- FI_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#EmploymentDurationCurrentEmployer Dummy
FI_train_test <- FI_train_test %>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 6, 1, 0))


###############################################################################################################
#                                          WOE - Application_Time                                                       #
###############################################################################################################


# Teljes adatkészlet összesítése
total_samples <- nrow(EE_train_test)
total_bad <- sum(EE_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- EE_train_test %>%
  group_by(Application_Time) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "EE_Application_Time")
writeData(wb, "EE_Application_Time", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NINCS VÁLTOZÓ

#####################################################################################

ES_train_test <- ES_train_test %>%
  mutate(Application_Time2 = Application_Time)

# Teljes adatkészlet összesítése
total_samples <- nrow(ES_train_test)
total_bad <- sum(ES_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- ES_train_test %>%
  group_by(Application_Time2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "ES_Application_Time")
writeData(wb, "ES_Application_Time", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)

#NINCS VÁLTOZÓ


#####################################################################################

FI_train_test <- FI_train_test %>%
  mutate(Application_Time2 = case_when(
    Application_Time == 2 ~ as.numeric(3),
    
    TRUE ~ as.numeric(Application_Time)
  ))

# Teljes adatkészlet összesítése
total_samples <- nrow(FI_train_test)
total_bad <- sum(FI_train_test$Default_flag_2y, na.rm = TRUE)
total_good = total_samples - total_bad

# Az összesítés a csoportosítás mellett, az új arány kiszámításával
aaaaa <- FI_train_test %>%
  group_by(Application_Time2) %>%
  summarise(
    N_i = n(),
    Bad_i = sum(Default_flag_2y, na.rm = TRUE),
    Good_i = n() - sum(Default_flag_2y, na.rm = TRUE),
    N = total_samples,
    Total_bad = total_bad,
    Total_good = total_good,
    WOE_i = log((Good_i) / (Total_good)) - log((Bad_i) / (Total_bad)),
    IV_i = WOE_i * ((Good_i/Total_good)-(Bad_i/Total_bad))
  )

# IV összegének kiszámítása
total_IV <- sum(aaaaa$IV_i, na.rm = TRUE)

# IV oszlop hozzáadása minden sorban ugyanazzal az értékkel
aaaaa <- aaaaa %>%
  mutate(IV = total_IV)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("WOE_var.xlsx")
addWorksheet(wb, "FI_Application_Time")
writeData(wb, "FI_Application_Time", aaaaa)
saveWorkbook(wb, "WOE_var.xlsx", overwrite = TRUE)


#Application_Time Dummy
FI_train_test <- FI_train_test %>%
  mutate(Application_Time_dummy0 = if_else(Application_Time2 == 3, 1, 0))

FI_train_test <- FI_train_test %>%
  mutate(Application_Time_dummy1 = if_else(Application_Time2 == 1, 1, 0))







###############################################################################################################
#                      Megvizsgálom, hogy tényleg minden változó megvan-e                                     #
###############################################################################################################



# A változók listája
expected_columns_EE <- c(
  "Education_dummy0", "Education_dummy1", "Education_dummy2",
  "Gender_dummy0", "Gender_dummy1",
  "LanguageCode_dummy0", "LanguageCode_dummy1",
  "LoanDuration_dummy0", "LoanDuration_dummy1", "LoanDuration_dummy2",
  "UseOfLoan_dummy0", "UseOfLoan_dummy1",
  "MaritalStatus_dummy0", "MaritalStatus_dummy1", 
  "NoOfPreviousLoansBeforeLoan_dummy0", "NoOfPreviousLoansBeforeLoan_dummy1", "NoOfPreviousLoansBeforeLoan_dummy2",
  "Age_dummy0", "Age_dummy1", "Age_dummy2", "Age_dummy3", "Age_dummy4", "Age_dummy5",
  "PreviousEarlyRepaymentsCountBeforeLoan_dummy0", "PreviousEarlyRepaymentsCountBeforeLoan_dummy1",
  "AppliedAmount_dummy0", "AppliedAmount_dummy1", "AppliedAmount_dummy2", "AppliedAmount_dummy3",
  "MonthlyPaymentDay_dummy0", "MonthlyPaymentDay_dummy1",
  "OccupationArea_dummy0", "OccupationArea_dummy1", "OccupationArea_dummy2",
  "IncomeTotal_dummy0", "IncomeTotal_dummy1", "IncomeTotal_dummy2", "IncomeTotal_dummy3", "IncomeTotal_dummy4",
  "IncomeFromPrincipalEmployer_dummy0", "IncomeFromPrincipalEmployer_dummy1",
  "IncomeOtherType_dummy0", "IncomeOtherType_dummy1",
  "DebtToIncome_dummy0", "DebtToIncome_dummy1",
  "LiabilitiesTotal_dummy0", "LiabilitiesTotal_dummy1", "LiabilitiesTotal_dummy2",
  "ExistingLiabilities_dummy0", "ExistingLiabilities_dummy1", "ExistingLiabilities_dummy2",
  "HomeOwnershipType_dummy0", "HomeOwnershipType_dummy1",
  "EmploymentDurationCurrentEmployer_dummy0", "EmploymentDurationCurrentEmployer_dummy1", "EmploymentDurationCurrentEmployer_dummy2"
)


# Ellenõrzés, hogy minden oszlop jelen van-e
missing_columns <- expected_columns_EE[!expected_columns_EE %in% names(EE_train_test)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}

#####################################################################################

# A változók listája
expected_columns_ES <- c(
  "Education_dummy0", "Education_dummy1", "Education_dummy2",
  "Gender_dummy0", "Gender_dummy1", "Gender_dummy2",
  "LoanDuration_dummy0", "LoanDuration_dummy1",
  "UseOfLoan_dummy0", "UseOfLoan_dummy1",
  "MaritalStatus_dummy0", "MaritalStatus_dummy1",
  "NoOfPreviousLoansBeforeLoan_dummy0", "NoOfPreviousLoansBeforeLoan_dummy1", "NoOfPreviousLoansBeforeLoan_dummy2",
  "PreviousEarlyRepaymentsCountBeforeLoan_dummy0", "PreviousEarlyRepaymentsCountBeforeLoan_dummy1",
  "AppliedAmount_dummy0", "AppliedAmount_dummy1", "AppliedAmount_dummy2", "AppliedAmount_dummy3", "AppliedAmount_dummy4",
  "MonthlyPaymentDay_dummy0", "MonthlyPaymentDay_dummy1",
  "OccupationArea_dummy0", "OccupationArea_dummy1",
  "IncomeTotal_dummy0", "IncomeTotal_dummy1", "IncomeTotal_dummy2", "IncomeTotal_dummy3",
  "IncomeFromPrincipalEmployer_dummy0", "IncomeFromPrincipalEmployer_dummy1",
  "IncomeOtherType_dummy0", "IncomeOtherType_dummy1",
  "DebtToIncome_dummy0", "DebtToIncome_dummy1",
  "LiabilitiesTotal_dummy0", "LiabilitiesTotal_dummy1", "LiabilitiesTotal_dummy2", "LiabilitiesTotal_dummy3",
  "ExistingLiabilities_dummy0", "ExistingLiabilities_dummy1", "ExistingLiabilities_dummy2",
  "EmploymentDurationCurrentEmployer_dummy0", "EmploymentDurationCurrentEmployer_dummy1", "EmploymentDurationCurrentEmployer_dummy2"
)


# Ellenõrzés, hogy minden oszlop jelen van-e az ES_train_test adatkeretben
missing_columns <- expected_columns_ES[!expected_columns_ES %in% names(ES_train_test)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}


#####################################################################################



# A változók listája
expected_columns_FI <- c(
  "Education_dummy0", "Education_dummy1", "Education_dummy2",
  "Gender_dummy0", "Gender_dummy1",
  "VerificationType_dummy0", "VerificationType_dummy1",
  "LoanDuration_dummy0", "LoanDuration_dummy1", "LoanDuration_dummy2",
  "UseOfLoan_dummy0", "UseOfLoan_dummy1",
  "MaritalStatus_dummy0", "MaritalStatus_dummy1",
  "NoOfPreviousLoansBeforeLoan_dummy0", "NoOfPreviousLoansBeforeLoan_dummy1",
  "Age_dummy0", "Age_dummy1", "Age_dummy2",
  "PreviousEarlyRepaymentsCountBeforeLoan_dummy0", "PreviousEarlyRepaymentsCountBeforeLoan_dummy1",
  "AppliedAmount_dummy0", "AppliedAmount_dummy1", "AppliedAmount_dummy2",
  "AppliedAmount_dummy3", "AppliedAmount_dummy4", "AppliedAmount_dummy5", "AppliedAmount_dummy6",
  "OccupationArea_dummy0", "OccupationArea_dummy1",
  "IncomeTotal_dummy0", "IncomeTotal_dummy1", "IncomeTotal_dummy2", "IncomeTotal_dummy3",
  "IncomeFromPrincipalEmployer_dummy0", "IncomeFromPrincipalEmployer_dummy1",
  "DebtToIncome_dummy0", "DebtToIncome_dummy1",
  "LiabilitiesTotal_dummy0", "LiabilitiesTotal_dummy1", "LiabilitiesTotal_dummy2",
  "ExistingLiabilities_dummy0", "ExistingLiabilities_dummy1", "ExistingLiabilities_dummy2", "ExistingLiabilities_dummy3",
  "HomeOwnershipType_dummy0", "HomeOwnershipType_dummy1",
  "EmploymentDurationCurrentEmployer_dummy0", "EmploymentDurationCurrentEmployer_dummy1", "EmploymentDurationCurrentEmployer_dummy2",
  "Application_Time_dummy0", "Application_Time_dummy1"
)


# Ellenõrzés, hogy minden oszlop jelen van-e az FI_train_test adatkeretben
missing_columns <- expected_columns_FI[!expected_columns_FI %in% names(FI_train_test)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}



###############################################################################################################
#                                GINI számítás változünként                                                   #
###############################################################################################################


# Az eredmények tárolása
gini_results_EE <- data.frame(Variable = character(), Gini = numeric(), stringsAsFactors = FALSE)

# Gini kiszámítása az összes változóra
for (variable in expected_columns_EE) {
  if (variable %in% names(EE_train_test)) {
    gini_value <- NormalizedGini(EE_train_test$Default_flag_2y, EE_train_test[[variable]])
    gini_abs <- abs(gini_value)
    
    # 10%-os küszöb
    gini_10 <- gini_abs > 0.10
    
    # 5%-os küszöb
    gini_5 <- gini_abs > 0.05
    
    gini_results_EE <- rbind(gini_results_EE, data.frame(Variable = variable, Gini = gini_value, GINI_10 = gini_10, GINI_5 = gini_5))
  } else {
    gini_results_EE <- rbind(gini_results_EE, data.frame(Variable = variable, Gini = NA, GINI_10 = NA, GINI_5 = NA))
  }
}


print(gini_results_EE)

# Excel fájl létrehozása az elsõ munkalappal
wb <- createWorkbook()
addWorksheet(wb, "GINI_EE")
writeData(wb, "GINI_EE", gini_results_EE)
saveWorkbook(wb, "GINI.xlsx", overwrite = TRUE)


# Eredmények tárolása
PSI_model_EE <- data.frame(Variable = character(), Count_1s = numeric(), stringsAsFactors = FALSE)

# Számolás minden változóra
for (variable in expected_columns_EE) {
  if (variable %in% names(EE_train_test)) {
    count_1s <- sum(EE_train_test[[variable]] == 1, na.rm = TRUE)
    PSI_model_EE <- rbind(PSI_model_EE, data.frame(Variable = variable, Count_1s = count_1s))
  } else {
    PSI_model_EE <- rbind(PSI_model_EE, data.frame(Variable = variable, Count_1s = NA))
  }
}

# Eredmények megjelenítése
print(PSI_model_EE)

# Excel fájl létrehozása az elsõ munkalappal
wb <- createWorkbook()
addWorksheet(wb, "PSI_EE")
writeData(wb, "PSI_EE", PSI_model_EE)
saveWorkbook(wb, "PSI.xlsx", overwrite = TRUE)

###############################################################################################################


# Az eredmények tárolása
gini_results_ES <- data.frame(Variable = character(), Gini = numeric(), stringsAsFactors = FALSE)

# Gini kiszámítása az összes változóra
for (variable in expected_columns_ES) {
  if (variable %in% names(ES_train_test)) {
    gini_value <- NormalizedGini(ES_train_test$Default_flag_2y, ES_train_test[[variable]])
    gini_abs <- abs(gini_value)
    
    # 10%-os küszöb
    gini_10 <- gini_abs > 0.10
    
    # 5%-os küszöb
    gini_5 <- gini_abs > 0.05
    
    gini_results_ES <- rbind(gini_results_ES, data.frame(Variable = variable, Gini = gini_value, GINI_10 = gini_10, GINI_5 = gini_5))
  } else {
    gini_results_ES <- rbind(gini_results_ES, data.frame(Variable = variable, Gini = NA, GINI_10 = NA, GINI_5 = NA))
  }
}


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("GINI.xlsx")
addWorksheet(wb, "GINI_ES")
writeData(wb, "GINI_ES", gini_results_ES)
saveWorkbook(wb, "GINI.xlsx", overwrite = TRUE)


# Eredmények tárolása
PSI_model_ES <- data.frame(Variable = character(), Count_1s = numeric(), stringsAsFactors = FALSE)

# Számolás minden változóra
for (variable in expected_columns_ES) {
  if (variable %in% names(ES_train_test)) {
    count_1s <- sum(ES_train_test[[variable]] == 1, na.rm = TRUE)
    PSI_model_ES <- rbind(PSI_model_ES, data.frame(Variable = variable, Count_1s = count_1s))
  } else {
    PSI_model_ES <- rbind(PSI_model_ES, data.frame(Variable = variable, Count_1s = NA))
  }
}

# Eredmények megjelenítése
print(PSI_model_ES)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PSI.xlsx")
addWorksheet(wb, "PSI_ES")
writeData(wb, "PSI_ES", PSI_model_ES)
saveWorkbook(wb, "PSI.xlsx", overwrite = TRUE)

###############################################################################################################



# Az eredmények tárolása
gini_results_FI <- data.frame(Variable = character(), Gini = numeric(), stringsAsFactors = FALSE)

# Gini kiszámítása az összes változóra
for (variable in expected_columns_FI) {
  if (variable %in% names(FI_train_test)) {
    gini_value <- NormalizedGini(FI_train_test$Default_flag_2y, FI_train_test[[variable]])
    gini_abs <- abs(gini_value)
    
    # 10%-os küszöb
    gini_10 <- gini_abs > 0.10
    
    # 5%-os küszöb
    gini_5 <- gini_abs > 0.05
    
    gini_results_FI <- rbind(gini_results_FI, data.frame(Variable = variable, Gini = gini_value, GINI_10 = gini_10, GINI_5 = gini_5))
  } else {
    gini_results_FI <- rbind(gini_results_FI, data.frame(Variable = variable, Gini = NA, GINI_10 = NA, GINI_5 = NA))
  }
}


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("GINI.xlsx")
addWorksheet(wb, "GINI_FI")
writeData(wb, "GINI_FI", gini_results_FI)
saveWorkbook(wb, "GINI.xlsx", overwrite = TRUE)


# Eredmények tárolása
PSI_model_FI <- data.frame(Variable = character(), Count_1s = numeric(), stringsAsFactors = FALSE)

# Számolás minden változóra
for (variable in expected_columns_FI) {
  if (variable %in% names(FI_train_test)) {
    count_1s <- sum(FI_train_test[[variable]] == 1, na.rm = TRUE)
    PSI_model_FI <- rbind(PSI_model_FI, data.frame(Variable = variable, Count_1s = count_1s))
  } else {
    PSI_model_FI <- rbind(PSI_model_FI, data.frame(Variable = variable, Count_1s = NA))
  }
}

# Eredmények megjelenítése
print(PSI_model_FI)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PSI.xlsx")
addWorksheet(wb, "PSI_FI")
writeData(wb, "PSI_FI", PSI_model_FI)
saveWorkbook(wb, "PSI.xlsx", overwrite = TRUE)

# 226451 megfigyelés marad
Live_data_flag <- FI_train_test %>% 
  group_by(Education_dummy0) %>% 
  summarise(count = n())

###############################################################################################################
#                                PSI Live Data felét is kiszámolom                                                   #
###############################################################################################################


# EE kategóriák alkalmazása a live data adatokra
#1:
EE_live_data <- EE_live_data %>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(1),
    Education == 2 ~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

EE_live_data <- EE_live_data %>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))

#2:
EE_live_data <- EE_live_data %>%
  mutate(Gender2 = case_when(
    Gender == 2 ~ as.numeric(0),
    
    TRUE ~ as.numeric(Gender)
  ))

#Gender Dummy
EE_live_data <- EE_live_data %>%
  mutate(Gender_dummy0 = if_else(Gender2 == 0, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Gender_dummy1 = if_else(Gender2 == 1, 1, 0))

#3:
EE_live_data <- EE_live_data %>%
  mutate(LanguageCode2 = case_when(
    LanguageCode == 2 ~ as.numeric(3),
    LanguageCode == 4 ~ as.numeric(3),
    LanguageCode == 6 ~ as.numeric(3),
    LanguageCode == 9 ~ as.numeric(3),
    is.na(LanguageCode) ~ as.numeric(3),
    TRUE ~ as.numeric(LanguageCode)
  ))

#VerificationType Dummy
EE_live_data <- EE_live_data %>%
  mutate(LanguageCode_dummy0 = if_else(LanguageCode2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(LanguageCode_dummy1 = if_else(LanguageCode2 == 3, 1, 0))

#5:
EE_live_data <- EE_live_data %>%
  mutate(LoanDuration2 = case_when(
    LoanDuration > 36 ~ as.numeric(36),
    LoanDuration == 27 ~ as.numeric(24),
    LoanDuration == 18 ~ as.numeric(24),
    LoanDuration == 30 ~ as.numeric(24),
    LoanDuration < 12 ~ as.numeric(12),
    LoanDuration == 15 ~ as.numeric(12),
    TRUE ~ as.numeric(LoanDuration)
  ))

#LoanDuration Dummy
EE_live_data <- EE_live_data %>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 36, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(LoanDuration_dummy2 = if_else(LoanDuration2 == 24, 1, 0))

#6:
EE_live_data <- EE_live_data %>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(1),
    UseOfLoan == 5 ~ as.numeric(1),
    UseOfLoan == 6 ~ as.numeric(1),
    UseOfLoan == 3 ~ as.numeric(1),
    UseOfLoan == 4 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

#UseOfLoan Dummy
EE_live_data <- EE_live_data %>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))

#7:
EE_live_data <- EE_live_data %>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 2 ~ as.numeric(1),
    MaritalStatus == 4 ~ as.numeric(1),
    MaritalStatus == 5 ~ as.numeric(1),
    is.na(MaritalStatus) ~ as.numeric(1),
    TRUE ~ as.numeric(MaritalStatus)
  ))

#MaritalStatus Dummy
EE_live_data <- EE_live_data %>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 3, 1, 0))

#8:
EE_live_data <- EE_live_data %>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    NoOfPreviousLoansBeforeLoan < 3 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan < 10 ~ as.numeric(2),
    NoOfPreviousLoansBeforeLoan > 9 ~ as.numeric(3),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

#NoOfPreviousLoansBeforeLoan Dummy
EE_live_data <- EE_live_data %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 2, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy2 = if_else(NoOfPreviousLoansBeforeLoan2 == 3, 1, 0))


#9:
# Kategóriák létrehozása a percentilis határok alapján
EE_live_data <- EE_live_data %>%
  mutate(Age_cat = cut(Age, breaks = unique(percentile_breaks_Age_EE), include.lowest = TRUE),
         Age_cat_num = as.numeric(Age_cat))

EE_live_data <- EE_live_data %>%
  mutate(Age_cat_num2 = case_when(
    Age_cat_num == 1 ~ as.numeric(1),
    Age_cat_num == 2 ~ as.numeric(2),
    Age_cat_num == 3 ~ as.numeric(3),
    Age_cat_num == 4 ~ as.numeric(4),
    Age_cat_num == 5 ~ as.numeric(4),
    Age_cat_num == 6 ~ as.numeric(4),
    Age_cat_num == 7 ~ as.numeric(7),
    Age_cat_num == 8 ~ as.numeric(7),
    Age_cat_num == 9 ~ as.numeric(7),
    Age_cat_num == 10 ~ as.numeric(7),
    Age_cat_num == 11 ~ as.numeric(7),
    Age_cat_num == 12 ~ as.numeric(7),
    Age_cat_num == 13 ~ as.numeric(13),
    Age_cat_num == 14 ~ as.numeric(13),
    Age_cat_num == 15 ~ as.numeric(13),
    Age_cat_num == 16 ~ as.numeric(13),
    Age_cat_num == 17 ~ as.numeric(13),
    Age_cat_num == 18 ~ as.numeric(13),
    Age_cat_num == 19 ~ as.numeric(13),
    Age_cat_num == 20 ~ as.numeric(13),
    TRUE ~ as.numeric(Age_cat_num)
  ))

#Age Dummy
EE_live_data <- EE_live_data %>%
  mutate(Age_dummy0 = if_else(Age_cat_num2 == 7, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Age_dummy1 = if_else(Age_cat_num2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Age_dummy2 = if_else(Age_cat_num2 == 2, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Age_dummy3 = if_else(Age_cat_num2 == 3, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Age_dummy4 = if_else(Age_cat_num2 == 4, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(Age_dummy5 = if_else(Age_cat_num2 == 13, 1, 0))



#10:
EE_live_data <- EE_live_data %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
EE_live_data <- EE_live_data %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))

#11:
# Kategóriák létrehozása a percentilis határok alapján
EE_live_data <- EE_live_data %>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_EE), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

EE_live_data <- EE_live_data %>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(2),
    AppliedAmount_cat_num == 4 ~ as.numeric(2),
    AppliedAmount_cat_num == 5 ~ as.numeric(2),
    AppliedAmount_cat_num == 6 ~ as.numeric(2),
    AppliedAmount_cat_num == 7 ~ as.numeric(2),
    AppliedAmount_cat_num == 8 ~ as.numeric(8),
    AppliedAmount_cat_num == 9 ~ as.numeric(8),
    AppliedAmount_cat_num == 10 ~ as.numeric(8),
    AppliedAmount_cat_num == 11 ~ as.numeric(8),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    AppliedAmount_cat_num == 15 ~ as.numeric(12),
    AppliedAmount_cat_num == 16 ~ as.numeric(12),
    AppliedAmount_cat_num == 17 ~ as.numeric(12),
    AppliedAmount_cat_num == 18 ~ as.numeric(12),
    AppliedAmount_cat_num == 19 ~ as.numeric(12),
    AppliedAmount_cat_num == 20 ~ as.numeric(12),
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

#AppliedAmount Dummy
EE_live_data <- EE_live_data %>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 8, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))


#12:
EE_live_data <- EE_live_data %>%
  mutate(MonthlyPaymentDay2 = case_when(
    MonthlyPaymentDay < 15 ~ as.numeric(1),
    MonthlyPaymentDay < 40 ~ as.numeric(2),
    is.na(MonthlyPaymentDay) ~ as.numeric(1),
    TRUE ~ as.numeric(MonthlyPaymentDay)
  ))

#MonthlyPaymentDay Dummy
EE_live_data <- EE_live_data %>%
  mutate(MonthlyPaymentDay_dummy0 = if_else(MonthlyPaymentDay2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(MonthlyPaymentDay_dummy1 = if_else(MonthlyPaymentDay2 == 2, 1, 0))

#3:
EE_live_data <- EE_live_data %>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == 8 ~ as.numeric(1),
    OccupationArea == 7 ~ as.numeric(1),
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 12 ~ as.numeric(1),
    OccupationArea == 14 ~ as.numeric(1),
    OccupationArea == 18 ~ as.numeric(10),
    OccupationArea == 5 ~ as.numeric(3),
    OccupationArea == 4 ~ as.numeric(3),
    OccupationArea == 13 ~ as.numeric(3),
    OccupationArea == 9 ~ as.numeric(3),
    OccupationArea == 6 ~ as.numeric(3),
    OccupationArea == 19 ~ as.numeric(3),
    OccupationArea == 11 ~ as.numeric(10),
    OccupationArea == 15 ~ as.numeric(10),
    OccupationArea == 16 ~ as.numeric(10),
    OccupationArea == 17 ~ as.numeric(10),
    OccupationArea == 2 ~ as.numeric(3),
    is.na(OccupationArea) ~ as.numeric(3),
    TRUE ~ as.numeric(OccupationArea) 
  ))

#OccupationArea Dummy
EE_live_data <- EE_live_data %>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 3, 1, 0))

#OccupationArea Dummy
EE_live_data <- EE_live_data %>%
  mutate(OccupationArea_dummy2 = if_else(OccupationArea2 == 10, 1, 0))

#14:
# Kategóriák létrehozása a percentilis határok alapján
EE_live_data <- EE_live_data %>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_EE), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

EE_live_data <- EE_live_data %>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(1),
    IncomeTotal_cat_num == 2 ~ as.numeric(2),
    IncomeTotal_cat_num == 3 ~ as.numeric(2),
    IncomeTotal_cat_num == 4 ~ as.numeric(2),
    IncomeTotal_cat_num == 5 ~ as.numeric(2),
    IncomeTotal_cat_num == 6 ~ as.numeric(2),
    IncomeTotal_cat_num == 7 ~ as.numeric(2),
    IncomeTotal_cat_num == 8 ~ as.numeric(2),
    IncomeTotal_cat_num == 9 ~ as.numeric(2),
    IncomeTotal_cat_num == 10 ~ as.numeric(2),
    IncomeTotal_cat_num == 11 ~ as.numeric(11),
    IncomeTotal_cat_num == 12 ~ as.numeric(11),
    IncomeTotal_cat_num == 13 ~ as.numeric(13),
    IncomeTotal_cat_num == 14 ~ as.numeric(13),
    IncomeTotal_cat_num == 15 ~ as.numeric(13),
    IncomeTotal_cat_num == 16 ~ as.numeric(13),
    IncomeTotal_cat_num == 17 ~ as.numeric(17),
    IncomeTotal_cat_num == 18 ~ as.numeric(17),
    IncomeTotal_cat_num == 19 ~ as.numeric(19),
    IncomeTotal_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))

#IncomeTotal Dummy
EE_live_data <- EE_live_data %>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 11, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 2, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 13, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(IncomeTotal_dummy4 = if_else(IncomeTotal_cat_num2 == 17, 1, 0))

#15:
# Kategóriák létrehozása a percentilis határok alapján
EE_live_data <- EE_live_data %>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_EE), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

EE_live_data <- EE_live_data %>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 3 ~ as.numeric(3),
    
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

#IncomeFromPrincipalEmployer Dummy
EE_live_data <- EE_live_data %>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 3, 1, 0))


#16:
# Kategóriák létrehozása a percentilis határok alapján
EE_live_data <- EE_live_data %>%
  mutate(IncomeOtherType_cat = cut(IncomeOtherType, breaks = unique(percentile_breaks_IncomeOtherType_EE), include.lowest = TRUE),
         IncomeOtherType_cat_num = as.numeric(IncomeOtherType_cat))

EE_live_data <- EE_live_data %>%
  mutate(IncomeOtherType_cat_num2 = case_when(
    IncomeOtherType_cat_num == 1 ~ as.numeric(1),
    IncomeOtherType_cat_num == 2 ~ as.numeric(2),
    IncomeOtherType_cat_num == 3 ~ as.numeric(3),
    IncomeOtherType_cat_num == 4 ~ as.numeric(4),
    IncomeOtherType_cat_num == 5 ~ as.numeric(5),
    IncomeOtherType_cat_num == 6 ~ as.numeric(6),
    IncomeOtherType_cat_num == 7 ~ as.numeric(7),
    IncomeOtherType_cat_num == 8 ~ as.numeric(8),
    IncomeOtherType_cat_num == 9 ~ as.numeric(9),
    IncomeOtherType_cat_num == 10 ~ as.numeric(10),
    IncomeOtherType_cat_num == 11 ~ as.numeric(11),
    IncomeOtherType_cat_num == 12 ~ as.numeric(12),
    IncomeOtherType_cat_num == 13 ~ as.numeric(13),
    IncomeOtherType_cat_num == 14 ~ as.numeric(14),
    IncomeOtherType_cat_num == 15 ~ as.numeric(15),
    IncomeOtherType_cat_num == 16 ~ as.numeric(16),
    IncomeOtherType_cat_num == 17 ~ as.numeric(17),
    IncomeOtherType_cat_num == 18 ~ as.numeric(18),
    IncomeOtherType_cat_num == 19 ~ as.numeric(19),
    IncomeOtherType_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeOtherType_cat_num)
  ))

#IncomeOtherType Dummy
EE_live_data <- EE_live_data %>%
  mutate(IncomeOtherType_dummy0 = if_else(IncomeOtherType_cat_num2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(IncomeOtherType_dummy1 = if_else(IncomeOtherType_cat_num2 == 2, 1, 0))

#17:
# Kategóriák létrehozása a percentilis határok alapján
EE_live_data <- EE_live_data %>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_EE), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

EE_live_data <- EE_live_data %>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(1),
    DebtToIncome_cat_num == 3 ~ as.numeric(1),
    DebtToIncome_cat_num == 4 ~ as.numeric(4),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(6),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

#DebtToIncome Dummy
EE_live_data <- EE_live_data %>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 4, 1, 0))

#18:
# Kategóriák létrehozása a percentilis határok alapján
EE_live_data <- EE_live_data %>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_EE), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

EE_live_data <- EE_live_data %>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(14),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))

#LiabilitiesTotal Dummy
EE_live_data <- EE_live_data %>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 4, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))

#19:
EE_live_data <- EE_live_data %>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 2 ~ as.numeric(1),
    ExistingLiabilities == 3 ~ as.numeric(1),
    ExistingLiabilities == 4 ~ as.numeric(1),
    ExistingLiabilities > 5 ~ as.numeric(5),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

#ExistingLiabilities Dummy
EE_live_data <- EE_live_data %>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 5, 1, 0))

#20:
EE_live_data <- EE_live_data %>%
  mutate(HomeOwnershipType2 = case_when(
    HomeOwnershipType == -1 ~ as.numeric(1),
    HomeOwnershipType == 0 ~ as.numeric(1),
    HomeOwnershipType == 2 ~ as.numeric(2),
    HomeOwnershipType == 3 ~ as.numeric(2),
    HomeOwnershipType == 4 ~ as.numeric(2),
    HomeOwnershipType == 5 ~ as.numeric(2),
    HomeOwnershipType == 6 ~ as.numeric(2),
    HomeOwnershipType == 7 ~ as.numeric(1),
    HomeOwnershipType == 8 ~ as.numeric(1),
    HomeOwnershipType == 9 ~ as.numeric(1),
    HomeOwnershipType == 10 ~ as.numeric(1),
    is.na(HomeOwnershipType) ~ as.numeric(1),
    TRUE ~ as.numeric(HomeOwnershipType)
  ))

#HomeOwnershipType Dummy
EE_live_data <- EE_live_data %>%
  mutate(HomeOwnershipType_dummy0 = if_else(HomeOwnershipType2 == 1, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(HomeOwnershipType_dummy1 = if_else(HomeOwnershipType2 == 2, 1, 0))


#21:
# A karakterláncok numerikus kódolása
EE_live_data <- EE_live_data %>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 1,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 7,
    EmploymentDurationCurrentEmployer == "Other" ~ 1,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

#HomeOwnershipType Dummy
EE_live_data <- EE_live_data %>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

EE_live_data <- EE_live_data %>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#HomeOwnershipType Dummy
EE_live_data <- EE_live_data %>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 7, 1, 0))

#########################################################################################################################################


# Ellenõrzés, hogy minden oszlop jelen van-e
missing_columns <- expected_columns_EE[!expected_columns_EE %in% names(EE_live_data)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}


# Eredmények tárolása
PSI_app_EE <- data.frame(Variable = character(), Count_1s = numeric(), stringsAsFactors = FALSE)

# Számolás minden változóra
for (variable in expected_columns_EE) {
  if (variable %in% names(EE_live_data)) {
    count_1s <- sum(EE_live_data[[variable]] == 1, na.rm = TRUE)
    PSI_app_EE <- rbind(PSI_app_EE, data.frame(Variable = variable, Count_1s = count_1s))
  } else {
    PSI_app_EE <- rbind(PSI_app_EE, data.frame(Variable = variable, Count_1s = NA))
  }
}


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PSI.xlsx")
addWorksheet(wb, "PSI_app_EE")
writeData(wb, "PSI_app_EE", PSI_app_EE)
saveWorkbook(wb, "PSI.xlsx", overwrite = TRUE)


#########################################################################################################################################
#########################################################################################################################################

# ES LIVE DATA

ES_live_data<- ES_live_data%>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(1),
    Education == 2 ~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

#Education Dummy
ES_live_data<- ES_live_data%>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))


ES_live_data<- ES_live_data%>%
  mutate(Gender2 = Gender)

#Gender Dummy
ES_live_data<- ES_live_data%>%
  mutate(Gender_dummy0 = if_else(Gender2 == 2, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(Gender_dummy1 = if_else(Gender2 == 0, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(Gender_dummy2 = if_else(Gender2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(LoanDuration2 = case_when(
    LoanDuration < 36 ~ as.numeric(12),
    
    LoanDuration > 36 ~ as.numeric(36),
    
    TRUE ~ as.numeric(LoanDuration)
  ))

#LoanDuration Dummy
ES_live_data<- ES_live_data%>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 36, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(1),
    UseOfLoan == 3 ~ as.numeric(1),
    UseOfLoan == 4 ~ as.numeric(1),
    UseOfLoan == 5 ~ as.numeric(1),
    UseOfLoan == 6 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

#UseOfLoan Dummy
ES_live_data<- ES_live_data%>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 2 ~ as.numeric(1),
    MaritalStatus == 3 ~ as.numeric(1),
    MaritalStatus == 4 ~ as.numeric(5),
    TRUE ~ as.numeric(MaritalStatus)
  ))

#MaritalStatus Dummy
ES_live_data<- ES_live_data%>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 5, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    NoOfPreviousLoansBeforeLoan == 2 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan == 3 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan > 3 ~ as.numeric(3),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

#NoOfPreviousLoansBeforeLoan Dummy
ES_live_data<- ES_live_data%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 0, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

#NoOfPreviousLoansBeforeLoan Dummy
ES_live_data<- ES_live_data%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy2 = if_else(NoOfPreviousLoansBeforeLoan2 == 3, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
ES_live_data<- ES_live_data%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_live_data<- ES_live_data%>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_ES), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

ES_live_data<- ES_live_data%>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(2),
    AppliedAmount_cat_num == 4 ~ as.numeric(2),
    AppliedAmount_cat_num == 5 ~ as.numeric(2),
    AppliedAmount_cat_num == 6 ~ as.numeric(6),
    AppliedAmount_cat_num == 7 ~ as.numeric(6),
    AppliedAmount_cat_num == 8 ~ as.numeric(6),
    AppliedAmount_cat_num == 9 ~ as.numeric(9),
    AppliedAmount_cat_num == 10 ~ as.numeric(9),
    AppliedAmount_cat_num == 11 ~ as.numeric(9),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

#AppliedAmount Dummy
ES_live_data<- ES_live_data%>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 6, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 9, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(AppliedAmount_dummy4 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(MonthlyPaymentDay2 = case_when(
    MonthlyPaymentDay < 15 ~ as.numeric(1),
    MonthlyPaymentDay < 40 ~ as.numeric(2),
    is.na(MonthlyPaymentDay) ~ as.numeric(1),
    TRUE ~ as.numeric(MonthlyPaymentDay)
  ))

#MonthlyPaymentDay Dummy
ES_live_data<- ES_live_data%>%
  mutate(MonthlyPaymentDay_dummy0 = if_else(MonthlyPaymentDay2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(MonthlyPaymentDay_dummy1 = if_else(MonthlyPaymentDay2 == 2, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 1 ~ as.numeric(1),
    OccupationArea == 2 ~ as.numeric(2),
    OccupationArea == 3 ~ as.numeric(1),
    OccupationArea == 4 ~ as.numeric(2),
    OccupationArea == 5 ~ as.numeric(2),
    OccupationArea == 6 ~ as.numeric(2),
    OccupationArea == 7 ~ as.numeric(1),
    OccupationArea == 8 ~ as.numeric(2),
    OccupationArea == 9 ~ as.numeric(2),
    OccupationArea == 10 ~ as.numeric(10),
    OccupationArea == 11 ~ as.numeric(10),
    OccupationArea == 12 ~ as.numeric(10),
    OccupationArea == 13 ~ as.numeric(10),
    OccupationArea == 14 ~ as.numeric(10),
    OccupationArea == 15 ~ as.numeric(10),
    OccupationArea == 16 ~ as.numeric(10),
    OccupationArea == 17 ~ as.numeric(2),
    OccupationArea == 18 ~ as.numeric(1),
    OccupationArea == 19 ~ as.numeric(2),
    is.na(OccupationArea) ~ as.numeric(1),
    TRUE ~ as.numeric(OccupationArea)
  ))

#OccupationArea Dummy
ES_live_data<- ES_live_data%>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 2, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(OccupationArea_dummy2 = if_else(OccupationArea2 == 10, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_live_data<- ES_live_data%>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_ES), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

ES_live_data<- ES_live_data%>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(1),
    IncomeTotal_cat_num == 2 ~ as.numeric(1),
    IncomeTotal_cat_num == 3 ~ as.numeric(1),
    IncomeTotal_cat_num == 4 ~ as.numeric(1),
    IncomeTotal_cat_num == 5 ~ as.numeric(1),
    IncomeTotal_cat_num == 6 ~ as.numeric(1),
    IncomeTotal_cat_num == 7 ~ as.numeric(1),
    IncomeTotal_cat_num == 8 ~ as.numeric(1),
    IncomeTotal_cat_num == 9 ~ as.numeric(1),
    IncomeTotal_cat_num == 10 ~ as.numeric(1),
    IncomeTotal_cat_num == 11 ~ as.numeric(1),
    IncomeTotal_cat_num == 12 ~ as.numeric(1),
    IncomeTotal_cat_num == 13 ~ as.numeric(13),
    IncomeTotal_cat_num == 14 ~ as.numeric(13),
    IncomeTotal_cat_num == 15 ~ as.numeric(13),
    IncomeTotal_cat_num == 16 ~ as.numeric(16),
    IncomeTotal_cat_num == 17 ~ as.numeric(16),
    IncomeTotal_cat_num == 18 ~ as.numeric(18),
    IncomeTotal_cat_num == 19 ~ as.numeric(18),
    
    
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))

#IncomeTotal Dummy
ES_live_data<- ES_live_data%>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 13, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 16, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 18, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_live_data<- ES_live_data%>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_ES), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

ES_live_data<- ES_live_data%>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 3 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 4 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 5 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 6 ~ as.numeric(2),
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

#IncomeFromPrincipalEmployer Dummy
ES_live_data<- ES_live_data%>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_live_data<- ES_live_data%>%
  mutate(IncomeOtherType_cat = cut(IncomeOtherType, breaks = unique(percentile_breaks_IncomeOtherType_ES), include.lowest = TRUE),
         IncomeOtherType_cat_num = as.numeric(IncomeOtherType_cat))

ES_live_data<- ES_live_data%>%
  mutate(IncomeOtherType_cat_num2 = case_when(
    IncomeOtherType_cat_num == 1 ~ as.numeric(1),
    IncomeOtherType_cat_num == 2 ~ as.numeric(2),
    IncomeOtherType_cat_num == 3 ~ as.numeric(3),
    IncomeOtherType_cat_num == 4 ~ as.numeric(4),
    IncomeOtherType_cat_num == 5 ~ as.numeric(5),
    IncomeOtherType_cat_num == 6 ~ as.numeric(6),
    IncomeOtherType_cat_num == 7 ~ as.numeric(7),
    IncomeOtherType_cat_num == 8 ~ as.numeric(8),
    IncomeOtherType_cat_num == 9 ~ as.numeric(9),
    IncomeOtherType_cat_num == 10 ~ as.numeric(10),
    IncomeOtherType_cat_num == 11 ~ as.numeric(11),
    IncomeOtherType_cat_num == 12 ~ as.numeric(12),
    IncomeOtherType_cat_num == 13 ~ as.numeric(13),
    IncomeOtherType_cat_num == 14 ~ as.numeric(14),
    IncomeOtherType_cat_num == 15 ~ as.numeric(15),
    IncomeOtherType_cat_num == 16 ~ as.numeric(16),
    IncomeOtherType_cat_num == 17 ~ as.numeric(17),
    IncomeOtherType_cat_num == 18 ~ as.numeric(18),
    IncomeOtherType_cat_num == 19 ~ as.numeric(19),
    IncomeOtherType_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeOtherType_cat_num)
  ))

#IncomeOtherType Dummy
ES_live_data<- ES_live_data%>%
  mutate(IncomeOtherType_dummy0 = if_else(IncomeOtherType_cat_num2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(IncomeOtherType_dummy1 = if_else(IncomeOtherType_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_live_data<- ES_live_data%>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_ES), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

ES_live_data<- ES_live_data%>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(1),
    DebtToIncome_cat_num == 3 ~ as.numeric(1),
    DebtToIncome_cat_num == 4 ~ as.numeric(1),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(5),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

#DebtToIncome Dummy
ES_live_data<- ES_live_data%>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 5, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_live_data<- ES_live_data%>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_ES), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

ES_live_data<- ES_live_data%>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(16),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))

#LiabilitiesTotal Dummy
ES_live_data<- ES_live_data%>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 10, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(LiabilitiesTotal_dummy3 = if_else(LiabilitiesTotal_cat_num2 == 16, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 2 ~ as.numeric(1),
    ExistingLiabilities > 3 ~ as.numeric(3),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

#ExistingLiabilities Dummy
ES_live_data<- ES_live_data%>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 1, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

#ExistingLiabilities Dummy
ES_live_data<- ES_live_data%>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 3, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 1,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 7,
    EmploymentDurationCurrentEmployer == "Other" ~ 1,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

#HomeOwnershipType Dummy
ES_live_data<- ES_live_data%>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

ES_live_data<- ES_live_data%>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#HomeOwnershipType Dummy
ES_live_data<- ES_live_data%>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 7, 1, 0))

############################################################################################################################


# Ellenõrzés, hogy minden oszlop jelen van-e
missing_columns <- expected_columns_ES[!expected_columns_ES %in% names(ES_live_data)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}



# Eredmények tárolása
PSI_app_ES <- data.frame(Variable = character(), Count_1s = numeric(), stringsAsFactors = FALSE)

# Számolás minden változóra
for (variable in expected_columns_ES) {
  if (variable %in% names(ES_live_data)) {
    count_1s <- sum(ES_live_data[[variable]] == 1, na.rm = TRUE)
    PSI_app_ES <- rbind(PSI_app_ES, data.frame(Variable = variable, Count_1s = count_1s))
  } else {
    PSI_app_ES <- rbind(PSI_app_ES, data.frame(Variable = variable, Count_1s = NA))
  }
}
print(PSI_app_ES)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PSI.xlsx")
addWorksheet(wb, "PSI_app_ES")
writeData(wb, "PSI_app_ES", PSI_app_ES)
saveWorkbook(wb, "PSI.xlsx", overwrite = TRUE)


#########################################################################################################################################
#########################################################################################################################################




FI_live_data<- FI_live_data%>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(5),
    Education == 2~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

#Education Dummy
FI_live_data<- FI_live_data%>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Gender2 = Gender)

#Gender Dummy
FI_live_data<- FI_live_data%>%
  mutate(Gender_dummy0 = if_else(Gender2 == 0, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Gender_dummy1 = if_else(Gender2 == 1, 1, 0))


FI_live_data<- FI_live_data%>%
  mutate(VerificationType2 = case_when(
    VerificationType == 2 ~ as.numeric(1),
    VerificationType == 3 ~ as.numeric(1),
    TRUE ~ as.numeric(VerificationType)
  ))

#VerificationType Dummy
FI_live_data<- FI_live_data%>%
  mutate(VerificationType_dummy0 = if_else(VerificationType2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(VerificationType_dummy1 = if_else(VerificationType2 == 4, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(LoanDuration2 = case_when(
    LoanDuration > 35 ~ as.numeric(60),
    LoanDuration == 18 ~ as.numeric(24),
    LoanDuration == 30 ~ as.numeric(24),
    LoanDuration < 12 ~ as.numeric(12),
    TRUE ~ as.numeric(LoanDuration)
  ))

#LoanDuration Dummy
FI_live_data<- FI_live_data%>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 60, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(LoanDuration_dummy2 = if_else(LoanDuration2 == 24, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(2),
    UseOfLoan == 3 ~ as.numeric(2),
    UseOfLoan == 4 ~ as.numeric(2),
    UseOfLoan == 5 ~ as.numeric(2),
    UseOfLoan == 6 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

#UseOfLoan Dummy
FI_live_data<- FI_live_data%>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 3 ~ as.numeric(2),
    MaritalStatus == 4 ~ as.numeric(2),
    MaritalStatus == 5 ~ as.numeric(2),
    TRUE ~ as.numeric(MaritalStatus)
  ))

#MaritalStatus Dummy
FI_live_data<- FI_live_data%>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 2, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    
    NoOfPreviousLoansBeforeLoan < 4 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan > 3 ~ as.numeric(2),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

#NoOfPreviousLoansBeforeLoan Dummy
FI_live_data<- FI_live_data%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_live_data<- FI_live_data%>%
  mutate(Age_cat = cut(Age, breaks = unique(percentile_breaks_Age_FI), include.lowest = TRUE),
         Age_cat_num = as.numeric(Age_cat))

FI_live_data<- FI_live_data%>%
  mutate(Age_cat_num2 = case_when(
    Age_cat_num == 1 ~ as.numeric(1),
    Age_cat_num == 2 ~ as.numeric(1),
    Age_cat_num == 3 ~ as.numeric(1),
    Age_cat_num == 4 ~ as.numeric(1),
    Age_cat_num == 5 ~ as.numeric(1),
    Age_cat_num == 6 ~ as.numeric(1),
    Age_cat_num == 7 ~ as.numeric(1),
    Age_cat_num == 8 ~ as.numeric(1),
    Age_cat_num == 9 ~ as.numeric(1),
    Age_cat_num == 10 ~ as.numeric(1),
    Age_cat_num == 11 ~ as.numeric(1),
    Age_cat_num == 12 ~ as.numeric(1),
    Age_cat_num == 13 ~ as.numeric(1),
    Age_cat_num == 14 ~ as.numeric(1),
    Age_cat_num == 15 ~ as.numeric(1),
    Age_cat_num == 16 ~ as.numeric(1),
    Age_cat_num == 17 ~ as.numeric(1),
    Age_cat_num == 18 ~ as.numeric(1),
    Age_cat_num == 19 ~ as.numeric(19),
    Age_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(Age_cat_num)
  ))

#Age Dummy
FI_live_data<- FI_live_data%>%
  mutate(Age_dummy0 = if_else(Age_cat_num2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Age_dummy1 = if_else(Age_cat_num2 == 19, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Age_dummy2 = if_else(Age_cat_num2 == 20, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
FI_live_data<- FI_live_data%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_FI), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(3),
    AppliedAmount_cat_num == 4 ~ as.numeric(4),
    AppliedAmount_cat_num == 5 ~ as.numeric(5),
    AppliedAmount_cat_num == 6 ~ as.numeric(5),
    AppliedAmount_cat_num == 7 ~ as.numeric(5),
    AppliedAmount_cat_num == 8 ~ as.numeric(5),
    AppliedAmount_cat_num == 9 ~ as.numeric(9),
    AppliedAmount_cat_num == 10 ~ as.numeric(9),
    AppliedAmount_cat_num == 11 ~ as.numeric(9),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    AppliedAmount_cat_num == 15 ~ as.numeric(12),
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

#AppliedAmount Dummy
FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 3, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 4, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_dummy4 = if_else(AppliedAmount_cat_num2 == 5, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_dummy5 = if_else(AppliedAmount_cat_num2 == 9, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(AppliedAmount_dummy6 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 1 ~ as.numeric(1),
    OccupationArea > 2 ~ as.numeric(2),
    
    is.na(OccupationArea) ~ as.numeric(1),
    TRUE ~ as.numeric(OccupationArea)
  ))

#OccupationArea Dummy
FI_live_data<- FI_live_data%>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_live_data<- FI_live_data%>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_FI), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

FI_live_data<- FI_live_data%>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(2),
    IncomeTotal_cat_num == 2 ~ as.numeric(2),
    IncomeTotal_cat_num == 3 ~ as.numeric(2),
    IncomeTotal_cat_num == 4 ~ as.numeric(2),
    IncomeTotal_cat_num == 5 ~ as.numeric(2),
    IncomeTotal_cat_num == 6 ~ as.numeric(2),
    IncomeTotal_cat_num == 7 ~ as.numeric(2),
    IncomeTotal_cat_num == 8 ~ as.numeric(2),
    IncomeTotal_cat_num == 9 ~ as.numeric(2),
    IncomeTotal_cat_num == 10 ~ as.numeric(2),
    IncomeTotal_cat_num == 11 ~ as.numeric(11),
    IncomeTotal_cat_num == 12 ~ as.numeric(11),
    IncomeTotal_cat_num == 13 ~ as.numeric(11),
    IncomeTotal_cat_num == 14 ~ as.numeric(11),
    IncomeTotal_cat_num == 15 ~ as.numeric(11),
    IncomeTotal_cat_num == 16 ~ as.numeric(16),
    IncomeTotal_cat_num == 17 ~ as.numeric(16),
    IncomeTotal_cat_num == 18 ~ as.numeric(18),
    IncomeTotal_cat_num == 19 ~ as.numeric(18),
    IncomeTotal_cat_num == 20 ~ as.numeric(18),
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))


#IncomeTotal Dummy
FI_live_data<- FI_live_data%>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 11, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 2, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 16, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 18, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_live_data<- FI_live_data%>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_FI), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

FI_live_data<- FI_live_data%>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(2),
    
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

#IncomeFromPrincipalEmployer Dummy
FI_live_data<- FI_live_data%>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_live_data<- FI_live_data%>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_FI), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

FI_live_data<- FI_live_data%>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(2),
    DebtToIncome_cat_num == 3 ~ as.numeric(3),
    DebtToIncome_cat_num == 4 ~ as.numeric(4),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(6),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

#DebtToIncome Dummy
FI_live_data<- FI_live_data%>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_live_data<- FI_live_data%>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_FI), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

FI_live_data<- FI_live_data%>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(17),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(18),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(19),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(20),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))


#LiabilitiesTotal Dummy
FI_live_data<- FI_live_data%>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 11, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 3 ~ as.numeric(2),
    ExistingLiabilities == 4 ~ as.numeric(2),
    ExistingLiabilities == 5 ~ as.numeric(2),
    ExistingLiabilities > 6 ~ as.numeric(6),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

#ExistingLiabilities Dummy
FI_live_data<- FI_live_data%>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 2, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(ExistingLiabilities_dummy3 = if_else(ExistingLiabilities2 == 6, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(HomeOwnershipType2 = case_when(
    HomeOwnershipType == -1 ~ as.numeric(1),
    HomeOwnershipType == 0 ~ as.numeric(1),
    HomeOwnershipType == 2 ~ as.numeric(2),
    HomeOwnershipType == 3 ~ as.numeric(2),
    HomeOwnershipType == 4 ~ as.numeric(2),
    HomeOwnershipType == 5 ~ as.numeric(2),
    HomeOwnershipType == 6 ~ as.numeric(2),
    HomeOwnershipType == 7 ~ as.numeric(1),
    HomeOwnershipType == 8 ~ as.numeric(1),
    HomeOwnershipType == 9 ~ as.numeric(1),
    HomeOwnershipType == 10 ~ as.numeric(1),
    is.na(HomeOwnershipType) ~ as.numeric(1),
    TRUE ~ as.numeric(HomeOwnershipType)
  ))

#HomeOwnershipType Dummy
FI_live_data<- FI_live_data%>%
  mutate(HomeOwnershipType_dummy0 = if_else(HomeOwnershipType2 == 1, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(HomeOwnershipType_dummy1 = if_else(HomeOwnershipType2 == 2, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 6,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 4,
    EmploymentDurationCurrentEmployer == "Other" ~ 6,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

#EmploymentDurationCurrentEmployer Dummy
FI_live_data<- FI_live_data%>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#EmploymentDurationCurrentEmployer Dummy
FI_live_data<- FI_live_data%>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 6, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Application_Time2 = case_when(
    Application_Time == 2 ~ as.numeric(3),
    
    TRUE ~ as.numeric(Application_Time)
  ))

#Application_Time Dummy
FI_live_data<- FI_live_data%>%
  mutate(Application_Time_dummy0 = if_else(Application_Time2 == 3, 1, 0))

FI_live_data<- FI_live_data%>%
  mutate(Application_Time_dummy1 = if_else(Application_Time2 == 1, 1, 0))



############################################################################################################################


# Ellenõrzés, hogy minden oszlop jelen van-e
missing_columns <- expected_columns_FI[!expected_columns_FI %in% names(FI_live_data)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}



# Eredmények tárolása
PSI_app_FI <- data.frame(Variable = character(), Count_1s = numeric(), stringsAsFactors = FALSE)

# Számolás minden változóra
for (variable in expected_columns_FI) {
  if (variable %in% names(FI_live_data)) {
    count_1s <- sum(FI_live_data[[variable]] == 1, na.rm = TRUE)
    PSI_app_FI <- rbind(PSI_app_FI, data.frame(Variable = variable, Count_1s = count_1s))
  } else {
    PSI_app_FI <- rbind(PSI_app_FI, data.frame(Variable = variable, Count_1s = NA))
  }
}
print(PSI_app_FI)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PSI.xlsx")
addWorksheet(wb, "PSI_app_FI")
writeData(wb, "PSI_app_FI", PSI_app_FI)
saveWorkbook(wb, "PSI.xlsx", overwrite = TRUE)


#########################################################################################################################################
#########################################################################################################################################

###############################################################################################################
#                                            Korrelációk                                                     #
###############################################################################################################


# Végsõ változó lista
Multivar_EE <- c(
  "Education_dummy1", "Education_dummy2",
  "Gender_dummy1",
  "LoanDuration_dummy1", "LoanDuration_dummy2",
  "NoOfPreviousLoansBeforeLoan_dummy1", "NoOfPreviousLoansBeforeLoan_dummy2",
  "Age_dummy1", "Age_dummy2", "Age_dummy3", "Age_dummy5",
  "AppliedAmount_dummy1", "AppliedAmount_dummy2", "AppliedAmount_dummy3",
  "MonthlyPaymentDay_dummy1",
  "OccupationArea_dummy2",
  "IncomeTotal_dummy1", "IncomeTotal_dummy2", "IncomeTotal_dummy3", "IncomeTotal_dummy4",
  "HomeOwnershipType_dummy1",
  "EmploymentDurationCurrentEmployer_dummy1", "EmploymentDurationCurrentEmployer_dummy2"
)

# Korrelációs mátrix
cor_matrix_EE <- cor(EE_train_test[, Multivar_EE], use = "complete.obs", method = "pearson")


# Excel fájl létrehozása az elsõ munkalappal
wb <- createWorkbook()
addWorksheet(wb, "Corr_EE")
writeData(wb, "Corr_EE", cor_matrix_EE)
saveWorkbook(wb, "Correlation.xlsx", overwrite = TRUE)


###############################################################################################################

# Végsõ változó lista
Multivar_ES <- c(
  "Education_dummy1",
  "Education_dummy2",
  "MaritalStatus_dummy1",
  "NoOfPreviousLoansBeforeLoan_dummy1",
  "NoOfPreviousLoansBeforeLoan_dummy2",
  "PreviousEarlyRepaymentsCountBeforeLoan_dummy1",
  "AppliedAmount_dummy1",
  "IncomeTotal_dummy1",
  "IncomeTotal_dummy2",
  "IncomeTotal_dummy3",
  "LiabilitiesTotal_dummy3",
  "EmploymentDurationCurrentEmployer_dummy1",
  "EmploymentDurationCurrentEmployer_dummy2"
)

# Korrelációs mátrix
cor_matrix_ES <- cor(ES_train_test[, Multivar_ES], use = "complete.obs", method = "pearson")

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Correlation.xlsx")
addWorksheet(wb, "Corr_ES")
writeData(wb, "Corr_ES", cor_matrix_ES)
saveWorkbook(wb, "Correlation.xlsx", overwrite = TRUE)


###############################################################################################################


# Végsõ változó lista
Multivar_FI <- c(
  "Education_dummy1",
  "Education_dummy2",
  "Gender_dummy1",
  "LoanDuration_dummy1",
  "LoanDuration_dummy2",
  "NoOfPreviousLoansBeforeLoan_dummy1",
  "Age_dummy0",
  "Age_dummy2",
  "PreviousEarlyRepaymentsCountBeforeLoan_dummy1",
  "AppliedAmount_dummy1",
  "AppliedAmount_dummy2",
  "AppliedAmount_dummy3",
  "AppliedAmount_dummy4",
  "AppliedAmount_dummy5",
  "AppliedAmount_dummy6",
  "IncomeTotal_dummy1",
  "IncomeTotal_dummy2",
  "IncomeTotal_dummy3",
  "LiabilitiesTotal_dummy0",
  "LiabilitiesTotal_dummy2",
  "ExistingLiabilities_dummy2",
  "ExistingLiabilities_dummy3",
  "HomeOwnershipType_dummy1",
  "EmploymentDurationCurrentEmployer_dummy1",
  "EmploymentDurationCurrentEmployer_dummy2"
)

# Korrelációs mátrix
cor_matrix_FI <- cor(FI_train_test[, Multivar_FI], use = "complete.obs", method = "pearson")

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Correlation.xlsx")
addWorksheet(wb, "Corr_FI")
writeData(wb, "Corr_FI", cor_matrix_FI)
saveWorkbook(wb, "Correlation.xlsx", overwrite = TRUE)

#########################################################################################################################################
#########################################################################################################################################

###############################################################################################################
#                                            MULTIVAR - EE                                                    #
###############################################################################################################

train_sample_EE <- EE_train_test[EE_train_test$Fold %in% c(2, 3, 4, 5), ]
test_sample_EE <- EE_train_test[EE_train_test$Fold == 1, ]

# Adatok elõkészítése
x <- model.matrix(train_sample_EE$Default_flag_2y ~ . - 1, data = train_sample_EE[, Multivar_EE])
y <- train_sample_EE$Default_flag_2y

# Adatok konvertálása glmnet formátumra
x <- as.matrix(x)

# Lasso modell illesztése
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)  # alpha = 1 a Lasso szabályozáshoz

# Keresztvalidáció a lambda kiválasztásához
cv_model <- cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha = 1)
#plot(cv_model)

# A legjobb lambda használata
best_lambda <- cv_model$lambda.1se
lasso_best <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

coef_lasso_EE <- print(coef(lasso_best))

# Adatok elõkészítése exportálásra
coef_data <- as.data.frame(coef_lasso_EE)
lambda_data <- data.frame(best_lambda_EE = best_lambda_EE)
coef_lasso_vector <- as.vector(coef(lasso_best))
coef_names <- rownames(coef(lasso_best))

# Adatok DataFrame-be helyezése
coef_data <- data.frame(CoefficientName = coef_names, CoefficientValue = coef_lasso_vector)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Multivar.xlsx")
addWorksheet(wb, "EE_5")
writeData(wb, "EE_5", coef_data)
saveWorkbook(wb, "Multivar.xlsx", overwrite = TRUE)


# Excel fájl létrehozása az elsõ munkalappal
wb <- createWorkbook()
addWorksheet(wb, "EE_1")
writeData(wb, "EE_1", coef_data)
saveWorkbook(wb, "Multivar.xlsx", overwrite = TRUE)

#####################################################
#####################################################

Final_vars_EE <- c(
  "Education_dummy1",
  "Education_dummy2",
  "Gender_dummy1",
  "LoanDuration_dummy1",
  "LoanDuration_dummy2",
  "NoOfPreviousLoansBeforeLoan_dummy1",
  "NoOfPreviousLoansBeforeLoan_dummy2",
  "Age_dummy1",
  "Age_dummy2",
  "Age_dummy5",
  "AppliedAmount_dummy2",
  "AppliedAmount_dummy3",
  "MonthlyPaymentDay_dummy1",
  "IncomeTotal_dummy2",
  "IncomeTotal_dummy4",
  "HomeOwnershipType_dummy1",
  "EmploymentDurationCurrentEmployer_dummy1",
  "EmploymentDurationCurrentEmployer_dummy2"
)


train_sample_EE <- EE_train_test[EE_train_test$Fold %in% c(1, 2, 3, 4, 5), ]

# Adatok elõkészítése
x <- model.matrix(train_sample_EE$Default_flag_2y ~ . - 1, data = train_sample_EE[, Final_vars_EE])
y <- train_sample_EE$Default_flag_2y

# Adatok konvertálása glmnet formátumra
x <- as.matrix(x)

# Lasso modell illesztése
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)  # alpha = 1 a Lasso szabályozáshoz

# Keresztvalidáció a lambda kiválasztásához
cv_model <- cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha = 1)
#plot(cv_model)

# A legjobb lambda használata
best_lambda <- cv_model$lambda.1se
lasso_best <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

coef_lasso_EE <- print(coef(lasso_best))

# Adatok elõkészítése exportálásra
coef_data <- as.data.frame(coef_lasso_EE)
lambda_data <- data.frame(best_lambda_EE = best_lambda_EE)
coef_lasso_vector <- as.vector(coef(lasso_best))
coef_names <- rownames(coef(lasso_best))

# Adatok DataFrame-be helyezése
coef_data <- data.frame(CoefficientName = coef_names, CoefficientValue = coef_lasso_vector)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Multivar.xlsx")
addWorksheet(wb, "EE_Fin")
writeData(wb, "EE_Fin", coef_data)
saveWorkbook(wb, "Multivar.xlsx", overwrite = TRUE)


###############################################################################################################
#                                            MULTIVAR - ES                                                    #
###############################################################################################################

train_sample_ES <- ES_train_test[ES_train_test$Fold %in% c(1, 2, 3, 4), ]
test_sample_ES <- ES_train_test[ES_train_test$Fold == 5, ]

# Adatok elõkészítése
x <- model.matrix(train_sample_ES$Default_flag_2y ~ . - 1, data = train_sample_ES[, Multivar_ES])
y <- train_sample_ES$Default_flag_2y

# Adatok konvertálása glmnet formátumra
x <- as.matrix(x)

# Lasso modell illesztése
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)  # alpha = 1 a Lasso szabályozáshoz

# Keresztvalidáció a lambda kiválasztásához
cv_model <- cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha = 1)
#plot(cv_model)

# A legjobb lambda használata
best_lambda <- cv_model$lambda.min
lasso_best <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

coef_lasso_ES <- print(coef(lasso_best))

# Adatok elõkészítése exportálásra
coef_lasso_vector <- as.vector(coef(lasso_best))
coef_names <- rownames(coef(lasso_best))

# Adatok DataFrame-be helyezése
coef_data <- data.frame(CoefficientName = coef_names, CoefficientValue = coef_lasso_vector)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Multivar.xlsx")
addWorksheet(wb, "ES_5")
writeData(wb, "ES_5", coef_data)
saveWorkbook(wb, "Multivar.xlsx", overwrite = TRUE)


#####################################################
#####################################################

Final_vars_ES <- c(
  "Education_dummy1",
  "Education_dummy2",
  "MaritalStatus_dummy1",
  "NoOfPreviousLoansBeforeLoan_dummy1",
  "NoOfPreviousLoansBeforeLoan_dummy2",
  "PreviousEarlyRepaymentsCountBeforeLoan_dummy1",
  "AppliedAmount_dummy1",
  "IncomeTotal_dummy1",
  
  "IncomeTotal_dummy3",
  "LiabilitiesTotal_dummy3",
  "EmploymentDurationCurrentEmployer_dummy1",
  "EmploymentDurationCurrentEmployer_dummy2"
)


train_sample_ES <- ES_train_test[ES_train_test$Fold %in% c(1, 2, 3, 4, 5), ]

# Adatok elõkészítése
x <- model.matrix(train_sample_ES$Default_flag_2y ~ . - 1, data = train_sample_ES[, Final_vars_ES])
y <- train_sample_ES$Default_flag_2y

# Adatok konvertálása glmnet formátumra
x <- as.matrix(x)

# Lasso modell illesztése
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)  # alpha = 1 a Lasso szabályozáshoz

# Keresztvalidáció a lambda kiválasztásához
cv_model <- cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha = 1)
#plot(cv_model)

# A legjobb lambda használata
best_lambda <- cv_model$lambda.min
lasso_best <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

coef_lasso_ES <- print(coef(lasso_best))

# Adatok elõkészítése exportálásra
coef_data <- as.data.frame(coef_lasso_ES)
lambda_data <- data.frame(best_lambda_ES = best_lambda_ES)
coef_lasso_vector <- as.vector(coef(lasso_best))
coef_names <- rownames(coef(lasso_best))

# Adatok DataFrame-be helyezése
coef_data <- data.frame(CoefficientName = coef_names, CoefficientValue = coef_lasso_vector)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Multivar.xlsx")
addWorksheet(wb, "ES_Fin")
writeData(wb, "ES_Fin", coef_data)
saveWorkbook(wb, "Multivar.xlsx", overwrite = TRUE)



###############################################################################################################
#                                            MULTIVAR - FI                                                    #
###############################################################################################################

train_sample_FI <- FI_train_test[FI_train_test$Fold %in% c(1, 2, 3, 4), ]
test_sample_FI <- FI_train_test[FI_train_test$Fold == 5, ]

# Adatok elõkészítése
x <- model.matrix(train_sample_FI$Default_flag_2y ~ . - 1, data = train_sample_FI[, Multivar_FI])
y <- train_sample_FI$Default_flag_2y

# Adatok konvertálása glmnet formátumra
x <- as.matrix(x)

# Lasso modell illesztése
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)  # alpha = 1 a Lasso szabályozáshoz

# Keresztvalidáció a lambda kiválasztásához
cv_model <- cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha = 1)
#plot(cv_model)

# A legjobb lambda használata
best_lambda <- cv_model$lambda.1se
lasso_best <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

coef_lasso_FI <- print(coef(lasso_best))

# Adatok elõkészítése exportálásra
coef_lasso_vector <- as.vector(coef(lasso_best))
coef_names <- rownames(coef(lasso_best))

# Adatok DataFrame-be helyezése
coef_data <- data.frame(CoefficientName = coef_names, CoefficientValue = coef_lasso_vector)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Multivar.xlsx")
addWorksheet(wb, "FI_5")
writeData(wb, "FI_5", coef_data)
saveWorkbook(wb, "Multivar.xlsx", overwrite = TRUE)


#####################################################
#####################################################

Final_vars_FI <- c(
  "Education_dummy1",
  "Education_dummy2",
  "Gender_dummy1",
  "LoanDuration_dummy1",
  "LoanDuration_dummy2",
  "NoOfPreviousLoansBeforeLoan_dummy1",
  "AppliedAmount_dummy1",
  "AppliedAmount_dummy2",
  "AppliedAmount_dummy3",
  "AppliedAmount_dummy4",
  "AppliedAmount_dummy5",
  "AppliedAmount_dummy6",
  "IncomeTotal_dummy1",
  "IncomeTotal_dummy2",
  "IncomeTotal_dummy3",
  "LiabilitiesTotal_dummy0",
  "LiabilitiesTotal_dummy2",
  "ExistingLiabilities_dummy2",
  "ExistingLiabilities_dummy3",
  "HomeOwnershipType_dummy1",
  "EmploymentDurationCurrentEmployer_dummy1",
  "EmploymentDurationCurrentEmployer_dummy2"
)


train_sample_FI <- FI_train_test[FI_train_test$Fold %in% c(1, 2, 3, 4, 5), ]

# Adatok elõkészítése
x <- model.matrix(train_sample_FI$Default_flag_2y ~ . - 1, data = train_sample_FI[, Final_vars_FI])
y <- train_sample_FI$Default_flag_2y

# Adatok konvertálása glmnet formátumra
x <- as.matrix(x)

# Lasso modell illesztése
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)  # alpha = 1 a Lasso szabályozáshoz

# Keresztvalidáció a lambda kiválasztásához
cv_model <- cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha = 1)
#plot(cv_model)

# A legjobb lambda használata
best_lambda <- cv_model$lambda.min
lasso_best <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

coef_lasso_FI <- print(coef(lasso_best))

# Adatok elõkészítése exportálásra
coef_data <- as.data.frame(coef_lasso_FI)
lambda_data <- data.frame(best_lambda_FI = best_lambda_FI)
coef_lasso_vector <- as.vector(coef(lasso_best))
coef_names <- rownames(coef(lasso_best))

# Adatok DataFrame-be helyezése
coef_data <- data.frame(CoefficientName = coef_names, CoefficientValue = coef_lasso_vector)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Multivar.xlsx")
addWorksheet(wb, "FI_Fin")
writeData(wb, "FI_Fin", coef_data)
saveWorkbook(wb, "Multivar.xlsx", overwrite = TRUE)







###############################################################################################################
#                                Holdout SET modelt kiszámolom                                                   #
###############################################################################################################


# EE kategóriák alkalmazása a live data adatokra
#1:
EE_houldout_set<- EE_houldout_set%>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(1),
    Education == 2 ~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))

#2:
EE_houldout_set<- EE_houldout_set%>%
  mutate(Gender2 = case_when(
    Gender == 2 ~ as.numeric(0),
    
    TRUE ~ as.numeric(Gender)
  ))

#Gender Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(Gender_dummy0 = if_else(Gender2 == 0, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Gender_dummy1 = if_else(Gender2 == 1, 1, 0))

#3:
EE_houldout_set<- EE_houldout_set%>%
  mutate(LanguageCode2 = case_when(
    LanguageCode == 2 ~ as.numeric(3),
    LanguageCode == 4 ~ as.numeric(3),
    LanguageCode == 6 ~ as.numeric(3),
    LanguageCode == 9 ~ as.numeric(3),
    is.na(LanguageCode) ~ as.numeric(3),
    TRUE ~ as.numeric(LanguageCode)
  ))

#VerificationType Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(LanguageCode_dummy0 = if_else(LanguageCode2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(LanguageCode_dummy1 = if_else(LanguageCode2 == 3, 1, 0))

#5:
EE_houldout_set<- EE_houldout_set%>%
  mutate(LoanDuration2 = case_when(
    LoanDuration > 36 ~ as.numeric(36),
    LoanDuration == 27 ~ as.numeric(24),
    LoanDuration == 18 ~ as.numeric(24),
    LoanDuration == 30 ~ as.numeric(24),
    LoanDuration < 12 ~ as.numeric(12),
    LoanDuration == 15 ~ as.numeric(12),
    TRUE ~ as.numeric(LoanDuration)
  ))

#LoanDuration Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 36, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(LoanDuration_dummy2 = if_else(LoanDuration2 == 24, 1, 0))

#6:
EE_houldout_set<- EE_houldout_set%>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(1),
    UseOfLoan == 5 ~ as.numeric(1),
    UseOfLoan == 6 ~ as.numeric(1),
    UseOfLoan == 3 ~ as.numeric(1),
    UseOfLoan == 4 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

#UseOfLoan Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))

#7:
EE_houldout_set<- EE_houldout_set%>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 2 ~ as.numeric(1),
    MaritalStatus == 4 ~ as.numeric(1),
    MaritalStatus == 5 ~ as.numeric(1),
    is.na(MaritalStatus) ~ as.numeric(1),
    TRUE ~ as.numeric(MaritalStatus)
  ))

#MaritalStatus Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 3, 1, 0))

#8:
EE_houldout_set<- EE_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    NoOfPreviousLoansBeforeLoan < 3 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan < 10 ~ as.numeric(2),
    NoOfPreviousLoansBeforeLoan > 9 ~ as.numeric(3),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

#NoOfPreviousLoansBeforeLoan Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 2, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy2 = if_else(NoOfPreviousLoansBeforeLoan2 == 3, 1, 0))


#9:
# Kategóriák létrehozása a percentilis határok alapján
EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_cat = cut(Age, breaks = unique(percentile_breaks_Age_EE), include.lowest = TRUE),
         Age_cat_num = as.numeric(Age_cat))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_cat_num2 = case_when(
    Age_cat_num == 1 ~ as.numeric(1),
    Age_cat_num == 2 ~ as.numeric(2),
    Age_cat_num == 3 ~ as.numeric(3),
    Age_cat_num == 4 ~ as.numeric(4),
    Age_cat_num == 5 ~ as.numeric(4),
    Age_cat_num == 6 ~ as.numeric(4),
    Age_cat_num == 7 ~ as.numeric(7),
    Age_cat_num == 8 ~ as.numeric(7),
    Age_cat_num == 9 ~ as.numeric(7),
    Age_cat_num == 10 ~ as.numeric(7),
    Age_cat_num == 11 ~ as.numeric(7),
    Age_cat_num == 12 ~ as.numeric(7),
    Age_cat_num == 13 ~ as.numeric(13),
    Age_cat_num == 14 ~ as.numeric(13),
    Age_cat_num == 15 ~ as.numeric(13),
    Age_cat_num == 16 ~ as.numeric(13),
    Age_cat_num == 17 ~ as.numeric(13),
    Age_cat_num == 18 ~ as.numeric(13),
    Age_cat_num == 19 ~ as.numeric(13),
    Age_cat_num == 20 ~ as.numeric(13),
    TRUE ~ as.numeric(Age_cat_num)
  ))

#Age Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_dummy0 = if_else(Age_cat_num2 == 7, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_dummy1 = if_else(Age_cat_num2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_dummy2 = if_else(Age_cat_num2 == 2, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_dummy3 = if_else(Age_cat_num2 == 3, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_dummy4 = if_else(Age_cat_num2 == 4, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(Age_dummy5 = if_else(Age_cat_num2 == 13, 1, 0))



#10:
EE_houldout_set<- EE_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))

#11:
# Kategóriák létrehozása a percentilis határok alapján
EE_houldout_set<- EE_houldout_set%>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_EE), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

EE_houldout_set<- EE_houldout_set%>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(2),
    AppliedAmount_cat_num == 4 ~ as.numeric(2),
    AppliedAmount_cat_num == 5 ~ as.numeric(2),
    AppliedAmount_cat_num == 6 ~ as.numeric(2),
    AppliedAmount_cat_num == 7 ~ as.numeric(2),
    AppliedAmount_cat_num == 8 ~ as.numeric(8),
    AppliedAmount_cat_num == 9 ~ as.numeric(8),
    AppliedAmount_cat_num == 10 ~ as.numeric(8),
    AppliedAmount_cat_num == 11 ~ as.numeric(8),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    AppliedAmount_cat_num == 15 ~ as.numeric(12),
    AppliedAmount_cat_num == 16 ~ as.numeric(12),
    AppliedAmount_cat_num == 17 ~ as.numeric(12),
    AppliedAmount_cat_num == 18 ~ as.numeric(12),
    AppliedAmount_cat_num == 19 ~ as.numeric(12),
    AppliedAmount_cat_num == 20 ~ as.numeric(12),
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

#AppliedAmount Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 8, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))


#12:
EE_houldout_set<- EE_houldout_set%>%
  mutate(MonthlyPaymentDay2 = case_when(
    MonthlyPaymentDay < 15 ~ as.numeric(1),
    MonthlyPaymentDay < 40 ~ as.numeric(2),
    is.na(MonthlyPaymentDay) ~ as.numeric(1),
    TRUE ~ as.numeric(MonthlyPaymentDay)
  ))

#MonthlyPaymentDay Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(MonthlyPaymentDay_dummy0 = if_else(MonthlyPaymentDay2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(MonthlyPaymentDay_dummy1 = if_else(MonthlyPaymentDay2 == 2, 1, 0))

#3:
EE_houldout_set<- EE_houldout_set%>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == 8 ~ as.numeric(1),
    OccupationArea == 7 ~ as.numeric(1),
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 12 ~ as.numeric(1),
    OccupationArea == 14 ~ as.numeric(1),
    OccupationArea == 18 ~ as.numeric(10),
    OccupationArea == 5 ~ as.numeric(3),
    OccupationArea == 4 ~ as.numeric(3),
    OccupationArea == 13 ~ as.numeric(3),
    OccupationArea == 9 ~ as.numeric(3),
    OccupationArea == 6 ~ as.numeric(3),
    OccupationArea == 19 ~ as.numeric(3),
    OccupationArea == 11 ~ as.numeric(10),
    OccupationArea == 15 ~ as.numeric(10),
    OccupationArea == 16 ~ as.numeric(10),
    OccupationArea == 17 ~ as.numeric(10),
    OccupationArea == 2 ~ as.numeric(3),
    is.na(OccupationArea) ~ as.numeric(3),
    TRUE ~ as.numeric(OccupationArea) 
  ))

#OccupationArea Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 3, 1, 0))

#OccupationArea Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(OccupationArea_dummy2 = if_else(OccupationArea2 == 10, 1, 0))

#14:
# Kategóriák létrehozása a percentilis határok alapján
EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_EE), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(1),
    IncomeTotal_cat_num == 2 ~ as.numeric(2),
    IncomeTotal_cat_num == 3 ~ as.numeric(2),
    IncomeTotal_cat_num == 4 ~ as.numeric(2),
    IncomeTotal_cat_num == 5 ~ as.numeric(2),
    IncomeTotal_cat_num == 6 ~ as.numeric(2),
    IncomeTotal_cat_num == 7 ~ as.numeric(2),
    IncomeTotal_cat_num == 8 ~ as.numeric(2),
    IncomeTotal_cat_num == 9 ~ as.numeric(2),
    IncomeTotal_cat_num == 10 ~ as.numeric(2),
    IncomeTotal_cat_num == 11 ~ as.numeric(11),
    IncomeTotal_cat_num == 12 ~ as.numeric(11),
    IncomeTotal_cat_num == 13 ~ as.numeric(13),
    IncomeTotal_cat_num == 14 ~ as.numeric(13),
    IncomeTotal_cat_num == 15 ~ as.numeric(13),
    IncomeTotal_cat_num == 16 ~ as.numeric(13),
    IncomeTotal_cat_num == 17 ~ as.numeric(17),
    IncomeTotal_cat_num == 18 ~ as.numeric(17),
    IncomeTotal_cat_num == 19 ~ as.numeric(19),
    IncomeTotal_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))

#IncomeTotal Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 11, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 2, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 13, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeTotal_dummy4 = if_else(IncomeTotal_cat_num2 == 17, 1, 0))

#15:
# Kategóriák létrehozása a percentilis határok alapján
EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_EE), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 3 ~ as.numeric(3),
    
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

#IncomeFromPrincipalEmployer Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 3, 1, 0))


#16:
# Kategóriák létrehozása a percentilis határok alapján
EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeOtherType_cat = cut(IncomeOtherType, breaks = unique(percentile_breaks_IncomeOtherType_EE), include.lowest = TRUE),
         IncomeOtherType_cat_num = as.numeric(IncomeOtherType_cat))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeOtherType_cat_num2 = case_when(
    IncomeOtherType_cat_num == 1 ~ as.numeric(1),
    IncomeOtherType_cat_num == 2 ~ as.numeric(2),
    IncomeOtherType_cat_num == 3 ~ as.numeric(3),
    IncomeOtherType_cat_num == 4 ~ as.numeric(4),
    IncomeOtherType_cat_num == 5 ~ as.numeric(5),
    IncomeOtherType_cat_num == 6 ~ as.numeric(6),
    IncomeOtherType_cat_num == 7 ~ as.numeric(7),
    IncomeOtherType_cat_num == 8 ~ as.numeric(8),
    IncomeOtherType_cat_num == 9 ~ as.numeric(9),
    IncomeOtherType_cat_num == 10 ~ as.numeric(10),
    IncomeOtherType_cat_num == 11 ~ as.numeric(11),
    IncomeOtherType_cat_num == 12 ~ as.numeric(12),
    IncomeOtherType_cat_num == 13 ~ as.numeric(13),
    IncomeOtherType_cat_num == 14 ~ as.numeric(14),
    IncomeOtherType_cat_num == 15 ~ as.numeric(15),
    IncomeOtherType_cat_num == 16 ~ as.numeric(16),
    IncomeOtherType_cat_num == 17 ~ as.numeric(17),
    IncomeOtherType_cat_num == 18 ~ as.numeric(18),
    IncomeOtherType_cat_num == 19 ~ as.numeric(19),
    IncomeOtherType_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeOtherType_cat_num)
  ))

#IncomeOtherType Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeOtherType_dummy0 = if_else(IncomeOtherType_cat_num2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(IncomeOtherType_dummy1 = if_else(IncomeOtherType_cat_num2 == 2, 1, 0))

#17:
# Kategóriák létrehozása a percentilis határok alapján
EE_houldout_set<- EE_houldout_set%>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_EE), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

EE_houldout_set<- EE_houldout_set%>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(1),
    DebtToIncome_cat_num == 3 ~ as.numeric(1),
    DebtToIncome_cat_num == 4 ~ as.numeric(4),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(6),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

#DebtToIncome Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 4, 1, 0))

#18:
# Kategóriák létrehozása a percentilis határok alapján
EE_houldout_set<- EE_houldout_set%>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_EE), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

EE_houldout_set<- EE_houldout_set%>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(4),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(14),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))

#LiabilitiesTotal Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 4, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))

#19:
EE_houldout_set<- EE_houldout_set%>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 2 ~ as.numeric(1),
    ExistingLiabilities == 3 ~ as.numeric(1),
    ExistingLiabilities == 4 ~ as.numeric(1),
    ExistingLiabilities > 5 ~ as.numeric(5),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

#ExistingLiabilities Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 5, 1, 0))

#20:
EE_houldout_set<- EE_houldout_set%>%
  mutate(HomeOwnershipType2 = case_when(
    HomeOwnershipType == -1 ~ as.numeric(1),
    HomeOwnershipType == 0 ~ as.numeric(1),
    HomeOwnershipType == 2 ~ as.numeric(2),
    HomeOwnershipType == 3 ~ as.numeric(2),
    HomeOwnershipType == 4 ~ as.numeric(2),
    HomeOwnershipType == 5 ~ as.numeric(2),
    HomeOwnershipType == 6 ~ as.numeric(2),
    HomeOwnershipType == 7 ~ as.numeric(1),
    HomeOwnershipType == 8 ~ as.numeric(1),
    HomeOwnershipType == 9 ~ as.numeric(1),
    HomeOwnershipType == 10 ~ as.numeric(1),
    is.na(HomeOwnershipType) ~ as.numeric(1),
    TRUE ~ as.numeric(HomeOwnershipType)
  ))

#HomeOwnershipType Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(HomeOwnershipType_dummy0 = if_else(HomeOwnershipType2 == 1, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(HomeOwnershipType_dummy1 = if_else(HomeOwnershipType2 == 2, 1, 0))


#21:
# A karakterláncok numerikus kódolása
EE_houldout_set<- EE_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 1,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 7,
    EmploymentDurationCurrentEmployer == "Other" ~ 1,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

#HomeOwnershipType Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

EE_houldout_set<- EE_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#HomeOwnershipType Dummy
EE_houldout_set<- EE_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 7, 1, 0))

#########################################################################################################################################




# ES LIVE DATA

ES_houldout_set<- ES_houldout_set%>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(1),
    Education == 2 ~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

#Education Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))


ES_houldout_set<- ES_houldout_set%>%
  mutate(Gender2 = Gender)

#Gender Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(Gender_dummy0 = if_else(Gender2 == 2, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(Gender_dummy1 = if_else(Gender2 == 0, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(Gender_dummy2 = if_else(Gender2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(LoanDuration2 = case_when(
    LoanDuration < 36 ~ as.numeric(12),
    
    LoanDuration > 36 ~ as.numeric(36),
    
    TRUE ~ as.numeric(LoanDuration)
  ))

#LoanDuration Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 36, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(1),
    UseOfLoan == 3 ~ as.numeric(1),
    UseOfLoan == 4 ~ as.numeric(1),
    UseOfLoan == 5 ~ as.numeric(1),
    UseOfLoan == 6 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

#UseOfLoan Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 2 ~ as.numeric(1),
    MaritalStatus == 3 ~ as.numeric(1),
    MaritalStatus == 4 ~ as.numeric(5),
    TRUE ~ as.numeric(MaritalStatus)
  ))

#MaritalStatus Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 5, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    NoOfPreviousLoansBeforeLoan == 2 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan == 3 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan > 3 ~ as.numeric(3),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

#NoOfPreviousLoansBeforeLoan Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 0, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

#NoOfPreviousLoansBeforeLoan Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy2 = if_else(NoOfPreviousLoansBeforeLoan2 == 3, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_houldout_set<- ES_houldout_set%>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_ES), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

ES_houldout_set<- ES_houldout_set%>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(2),
    AppliedAmount_cat_num == 4 ~ as.numeric(2),
    AppliedAmount_cat_num == 5 ~ as.numeric(2),
    AppliedAmount_cat_num == 6 ~ as.numeric(6),
    AppliedAmount_cat_num == 7 ~ as.numeric(6),
    AppliedAmount_cat_num == 8 ~ as.numeric(6),
    AppliedAmount_cat_num == 9 ~ as.numeric(9),
    AppliedAmount_cat_num == 10 ~ as.numeric(9),
    AppliedAmount_cat_num == 11 ~ as.numeric(9),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

#AppliedAmount Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 6, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 9, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(AppliedAmount_dummy4 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(MonthlyPaymentDay2 = case_when(
    MonthlyPaymentDay < 15 ~ as.numeric(1),
    MonthlyPaymentDay < 40 ~ as.numeric(2),
    is.na(MonthlyPaymentDay) ~ as.numeric(1),
    TRUE ~ as.numeric(MonthlyPaymentDay)
  ))

#MonthlyPaymentDay Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(MonthlyPaymentDay_dummy0 = if_else(MonthlyPaymentDay2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(MonthlyPaymentDay_dummy1 = if_else(MonthlyPaymentDay2 == 2, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 1 ~ as.numeric(1),
    OccupationArea == 2 ~ as.numeric(2),
    OccupationArea == 3 ~ as.numeric(1),
    OccupationArea == 4 ~ as.numeric(2),
    OccupationArea == 5 ~ as.numeric(2),
    OccupationArea == 6 ~ as.numeric(2),
    OccupationArea == 7 ~ as.numeric(1),
    OccupationArea == 8 ~ as.numeric(2),
    OccupationArea == 9 ~ as.numeric(2),
    OccupationArea == 10 ~ as.numeric(10),
    OccupationArea == 11 ~ as.numeric(10),
    OccupationArea == 12 ~ as.numeric(10),
    OccupationArea == 13 ~ as.numeric(10),
    OccupationArea == 14 ~ as.numeric(10),
    OccupationArea == 15 ~ as.numeric(10),
    OccupationArea == 16 ~ as.numeric(10),
    OccupationArea == 17 ~ as.numeric(2),
    OccupationArea == 18 ~ as.numeric(1),
    OccupationArea == 19 ~ as.numeric(2),
    is.na(OccupationArea) ~ as.numeric(1),
    TRUE ~ as.numeric(OccupationArea)
  ))

#OccupationArea Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 2, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(OccupationArea_dummy2 = if_else(OccupationArea2 == 10, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_ES), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(1),
    IncomeTotal_cat_num == 2 ~ as.numeric(1),
    IncomeTotal_cat_num == 3 ~ as.numeric(1),
    IncomeTotal_cat_num == 4 ~ as.numeric(1),
    IncomeTotal_cat_num == 5 ~ as.numeric(1),
    IncomeTotal_cat_num == 6 ~ as.numeric(1),
    IncomeTotal_cat_num == 7 ~ as.numeric(1),
    IncomeTotal_cat_num == 8 ~ as.numeric(1),
    IncomeTotal_cat_num == 9 ~ as.numeric(1),
    IncomeTotal_cat_num == 10 ~ as.numeric(1),
    IncomeTotal_cat_num == 11 ~ as.numeric(1),
    IncomeTotal_cat_num == 12 ~ as.numeric(1),
    IncomeTotal_cat_num == 13 ~ as.numeric(13),
    IncomeTotal_cat_num == 14 ~ as.numeric(13),
    IncomeTotal_cat_num == 15 ~ as.numeric(13),
    IncomeTotal_cat_num == 16 ~ as.numeric(16),
    IncomeTotal_cat_num == 17 ~ as.numeric(16),
    IncomeTotal_cat_num == 18 ~ as.numeric(18),
    IncomeTotal_cat_num == 19 ~ as.numeric(18),
    
    
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))

#IncomeTotal Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 13, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 16, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 18, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_ES), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 3 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 4 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 5 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 6 ~ as.numeric(2),
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

#IncomeFromPrincipalEmployer Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeOtherType_cat = cut(IncomeOtherType, breaks = unique(percentile_breaks_IncomeOtherType_ES), include.lowest = TRUE),
         IncomeOtherType_cat_num = as.numeric(IncomeOtherType_cat))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeOtherType_cat_num2 = case_when(
    IncomeOtherType_cat_num == 1 ~ as.numeric(1),
    IncomeOtherType_cat_num == 2 ~ as.numeric(2),
    IncomeOtherType_cat_num == 3 ~ as.numeric(3),
    IncomeOtherType_cat_num == 4 ~ as.numeric(4),
    IncomeOtherType_cat_num == 5 ~ as.numeric(5),
    IncomeOtherType_cat_num == 6 ~ as.numeric(6),
    IncomeOtherType_cat_num == 7 ~ as.numeric(7),
    IncomeOtherType_cat_num == 8 ~ as.numeric(8),
    IncomeOtherType_cat_num == 9 ~ as.numeric(9),
    IncomeOtherType_cat_num == 10 ~ as.numeric(10),
    IncomeOtherType_cat_num == 11 ~ as.numeric(11),
    IncomeOtherType_cat_num == 12 ~ as.numeric(12),
    IncomeOtherType_cat_num == 13 ~ as.numeric(13),
    IncomeOtherType_cat_num == 14 ~ as.numeric(14),
    IncomeOtherType_cat_num == 15 ~ as.numeric(15),
    IncomeOtherType_cat_num == 16 ~ as.numeric(16),
    IncomeOtherType_cat_num == 17 ~ as.numeric(17),
    IncomeOtherType_cat_num == 18 ~ as.numeric(18),
    IncomeOtherType_cat_num == 19 ~ as.numeric(19),
    IncomeOtherType_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(IncomeOtherType_cat_num)
  ))

#IncomeOtherType Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeOtherType_dummy0 = if_else(IncomeOtherType_cat_num2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(IncomeOtherType_dummy1 = if_else(IncomeOtherType_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_houldout_set<- ES_houldout_set%>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_ES), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

ES_houldout_set<- ES_houldout_set%>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(1),
    DebtToIncome_cat_num == 3 ~ as.numeric(1),
    DebtToIncome_cat_num == 4 ~ as.numeric(1),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(5),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

#DebtToIncome Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 5, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
ES_houldout_set<- ES_houldout_set%>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_ES), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

ES_houldout_set<- ES_houldout_set%>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(10),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(16),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))

#LiabilitiesTotal Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 10, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(LiabilitiesTotal_dummy3 = if_else(LiabilitiesTotal_cat_num2 == 16, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 2 ~ as.numeric(1),
    ExistingLiabilities > 3 ~ as.numeric(3),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

#ExistingLiabilities Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 1, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

#ExistingLiabilities Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 3, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 1,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 7,
    EmploymentDurationCurrentEmployer == "Other" ~ 1,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 4,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

#HomeOwnershipType Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

ES_houldout_set<- ES_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#HomeOwnershipType Dummy
ES_houldout_set<- ES_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 7, 1, 0))

######################################################################################################################################



FI_houldout_set<- FI_houldout_set%>%
  mutate(Education2 = case_when(
    Education == -1 ~ as.numeric(5),
    Education == 2~ as.numeric(1),
    Education == 3 ~ as.numeric(4),
    is.na(Education) ~ as.numeric(1),
    TRUE ~ as.numeric(Education)
  ))

#Education Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(Education_dummy0 = if_else(Education2 == 4, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Education_dummy1 = if_else(Education2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Education_dummy2 = if_else(Education2 == 5, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Gender2 = Gender)

#Gender Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(Gender_dummy0 = if_else(Gender2 == 0, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Gender_dummy1 = if_else(Gender2 == 1, 1, 0))


FI_houldout_set<- FI_houldout_set%>%
  mutate(VerificationType2 = case_when(
    VerificationType == 2 ~ as.numeric(1),
    VerificationType == 3 ~ as.numeric(1),
    TRUE ~ as.numeric(VerificationType)
  ))

#VerificationType Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(VerificationType_dummy0 = if_else(VerificationType2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(VerificationType_dummy1 = if_else(VerificationType2 == 4, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(LoanDuration2 = case_when(
    LoanDuration > 35 ~ as.numeric(60),
    LoanDuration == 18 ~ as.numeric(24),
    LoanDuration == 30 ~ as.numeric(24),
    LoanDuration < 12 ~ as.numeric(12),
    TRUE ~ as.numeric(LoanDuration)
  ))

#LoanDuration Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(LoanDuration_dummy0 = if_else(LoanDuration2 == 60, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(LoanDuration_dummy1 = if_else(LoanDuration2 == 12, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(LoanDuration_dummy2 = if_else(LoanDuration2 == 24, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(UseOfLoan2 = case_when(
    UseOfLoan == -1 ~ as.numeric(1),
    UseOfLoan == 0 ~ as.numeric(2),
    UseOfLoan == 3 ~ as.numeric(2),
    UseOfLoan == 4 ~ as.numeric(2),
    UseOfLoan == 5 ~ as.numeric(2),
    UseOfLoan == 6 ~ as.numeric(2),
    UseOfLoan == 8 ~ as.numeric(2),
    UseOfLoan == 7 ~ as.numeric(2),
    TRUE ~ as.numeric(UseOfLoan)
  ))

#UseOfLoan Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(UseOfLoan_dummy0 = if_else(UseOfLoan2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(UseOfLoan_dummy1 = if_else(UseOfLoan2 == 2, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(MaritalStatus2 = case_when(
    MaritalStatus == -1 ~ as.numeric(1),
    MaritalStatus == 3 ~ as.numeric(2),
    MaritalStatus == 4 ~ as.numeric(2),
    MaritalStatus == 5 ~ as.numeric(2),
    TRUE ~ as.numeric(MaritalStatus)
  ))

#MaritalStatus Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(MaritalStatus_dummy0 = if_else(MaritalStatus2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(MaritalStatus_dummy1 = if_else(MaritalStatus2 == 2, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan2 = case_when(
    
    NoOfPreviousLoansBeforeLoan < 4 ~ as.numeric(1),
    NoOfPreviousLoansBeforeLoan > 3 ~ as.numeric(2),
    is.na(NoOfPreviousLoansBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(NoOfPreviousLoansBeforeLoan)
  ))

#NoOfPreviousLoansBeforeLoan Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy0 = if_else(NoOfPreviousLoansBeforeLoan2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(NoOfPreviousLoansBeforeLoan_dummy1 = if_else(NoOfPreviousLoansBeforeLoan2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_houldout_set<- FI_houldout_set%>%
  mutate(Age_cat = cut(Age, breaks = unique(percentile_breaks_Age_FI), include.lowest = TRUE),
         Age_cat_num = as.numeric(Age_cat))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Age_cat_num2 = case_when(
    Age_cat_num == 1 ~ as.numeric(1),
    Age_cat_num == 2 ~ as.numeric(1),
    Age_cat_num == 3 ~ as.numeric(1),
    Age_cat_num == 4 ~ as.numeric(1),
    Age_cat_num == 5 ~ as.numeric(1),
    Age_cat_num == 6 ~ as.numeric(1),
    Age_cat_num == 7 ~ as.numeric(1),
    Age_cat_num == 8 ~ as.numeric(1),
    Age_cat_num == 9 ~ as.numeric(1),
    Age_cat_num == 10 ~ as.numeric(1),
    Age_cat_num == 11 ~ as.numeric(1),
    Age_cat_num == 12 ~ as.numeric(1),
    Age_cat_num == 13 ~ as.numeric(1),
    Age_cat_num == 14 ~ as.numeric(1),
    Age_cat_num == 15 ~ as.numeric(1),
    Age_cat_num == 16 ~ as.numeric(1),
    Age_cat_num == 17 ~ as.numeric(1),
    Age_cat_num == 18 ~ as.numeric(1),
    Age_cat_num == 19 ~ as.numeric(19),
    Age_cat_num == 20 ~ as.numeric(20),
    TRUE ~ as.numeric(Age_cat_num)
  ))

#Age Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(Age_dummy0 = if_else(Age_cat_num2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Age_dummy1 = if_else(Age_cat_num2 == 19, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Age_dummy2 = if_else(Age_cat_num2 == 20, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan2 = case_when(
    PreviousEarlyRepaymentsCountBeforeLoan > 1 ~ as.numeric(1),
    is.na(PreviousEarlyRepaymentsCountBeforeLoan) ~ as.numeric(1),
    TRUE ~ as.numeric(PreviousEarlyRepaymentsCountBeforeLoan)
  ))

#PreviousEarlyRepaymentsCountBeforeLoan Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy0 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 0, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(PreviousEarlyRepaymentsCountBeforeLoan_dummy1 = if_else(PreviousEarlyRepaymentsCountBeforeLoan2 == 1, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_cat = cut(AppliedAmount, breaks = unique(percentile_breaks_AppliedAmount_FI), include.lowest = TRUE),
         AppliedAmount_cat_num = as.numeric(AppliedAmount_cat))

FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_cat_num2 = case_when(
    AppliedAmount_cat_num == 1 ~ as.numeric(1),
    AppliedAmount_cat_num == 2 ~ as.numeric(2),
    AppliedAmount_cat_num == 3 ~ as.numeric(3),
    AppliedAmount_cat_num == 4 ~ as.numeric(4),
    AppliedAmount_cat_num == 5 ~ as.numeric(5),
    AppliedAmount_cat_num == 6 ~ as.numeric(5),
    AppliedAmount_cat_num == 7 ~ as.numeric(5),
    AppliedAmount_cat_num == 8 ~ as.numeric(5),
    AppliedAmount_cat_num == 9 ~ as.numeric(9),
    AppliedAmount_cat_num == 10 ~ as.numeric(9),
    AppliedAmount_cat_num == 11 ~ as.numeric(9),
    AppliedAmount_cat_num == 12 ~ as.numeric(12),
    AppliedAmount_cat_num == 13 ~ as.numeric(12),
    AppliedAmount_cat_num == 14 ~ as.numeric(12),
    AppliedAmount_cat_num == 15 ~ as.numeric(12),
    TRUE ~ as.numeric(AppliedAmount_cat_num)
  ))

#AppliedAmount Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_dummy0 = if_else(AppliedAmount_cat_num2 == 2, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_dummy1 = if_else(AppliedAmount_cat_num2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_dummy2 = if_else(AppliedAmount_cat_num2 == 3, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_dummy3 = if_else(AppliedAmount_cat_num2 == 4, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_dummy4 = if_else(AppliedAmount_cat_num2 == 5, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_dummy5 = if_else(AppliedAmount_cat_num2 == 9, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(AppliedAmount_dummy6 = if_else(AppliedAmount_cat_num2 == 12, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(OccupationArea2 = case_when(
    OccupationArea == -1 ~ as.numeric(1),
    OccupationArea == 1 ~ as.numeric(1),
    OccupationArea > 2 ~ as.numeric(2),
    
    is.na(OccupationArea) ~ as.numeric(1),
    TRUE ~ as.numeric(OccupationArea)
  ))

#OccupationArea Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(OccupationArea_dummy0 = if_else(OccupationArea2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(OccupationArea_dummy1 = if_else(OccupationArea2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeTotal_cat = cut(IncomeTotal, breaks = unique(percentile_breaks_IncomeTotal_FI), include.lowest = TRUE),
         IncomeTotal_cat_num = as.numeric(IncomeTotal_cat))

FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeTotal_cat_num2 = case_when(
    IncomeTotal_cat_num == 1 ~ as.numeric(2),
    IncomeTotal_cat_num == 2 ~ as.numeric(2),
    IncomeTotal_cat_num == 3 ~ as.numeric(2),
    IncomeTotal_cat_num == 4 ~ as.numeric(2),
    IncomeTotal_cat_num == 5 ~ as.numeric(2),
    IncomeTotal_cat_num == 6 ~ as.numeric(2),
    IncomeTotal_cat_num == 7 ~ as.numeric(2),
    IncomeTotal_cat_num == 8 ~ as.numeric(2),
    IncomeTotal_cat_num == 9 ~ as.numeric(2),
    IncomeTotal_cat_num == 10 ~ as.numeric(2),
    IncomeTotal_cat_num == 11 ~ as.numeric(11),
    IncomeTotal_cat_num == 12 ~ as.numeric(11),
    IncomeTotal_cat_num == 13 ~ as.numeric(11),
    IncomeTotal_cat_num == 14 ~ as.numeric(11),
    IncomeTotal_cat_num == 15 ~ as.numeric(11),
    IncomeTotal_cat_num == 16 ~ as.numeric(16),
    IncomeTotal_cat_num == 17 ~ as.numeric(16),
    IncomeTotal_cat_num == 18 ~ as.numeric(18),
    IncomeTotal_cat_num == 19 ~ as.numeric(18),
    IncomeTotal_cat_num == 20 ~ as.numeric(18),
    TRUE ~ as.numeric(IncomeTotal_cat_num)
  ))


#IncomeTotal Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeTotal_dummy0 = if_else(IncomeTotal_cat_num2 == 11, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeTotal_dummy1 = if_else(IncomeTotal_cat_num2 == 2, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeTotal_dummy2 = if_else(IncomeTotal_cat_num2 == 16, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeTotal_dummy3 = if_else(IncomeTotal_cat_num2 == 18, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_cat = cut(IncomeFromPrincipalEmployer, breaks = unique(percentile_breaks_IncomeFromPrincipalEmployer_FI), include.lowest = TRUE),
         IncomeFromPrincipalEmployer_cat_num = as.numeric(IncomeFromPrincipalEmployer_cat))

FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_cat_num2 = case_when(
    IncomeFromPrincipalEmployer_cat_num == 1 ~ as.numeric(1),
    IncomeFromPrincipalEmployer_cat_num == 2 ~ as.numeric(2),
    
    TRUE ~ as.numeric(IncomeFromPrincipalEmployer_cat_num)
  ))

#IncomeFromPrincipalEmployer Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_dummy0 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(IncomeFromPrincipalEmployer_dummy1 = if_else(IncomeFromPrincipalEmployer_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_houldout_set<- FI_houldout_set%>%
  mutate(DebtToIncome_cat = cut(DebtToIncome, breaks = unique(percentile_breaks_DebtToIncome_FI), include.lowest = TRUE),
         DebtToIncome_cat_num = as.numeric(DebtToIncome_cat))

FI_houldout_set<- FI_houldout_set%>%
  mutate(DebtToIncome_cat_num2 = case_when(
    DebtToIncome_cat_num == 1 ~ as.numeric(1),
    DebtToIncome_cat_num == 2 ~ as.numeric(2),
    DebtToIncome_cat_num == 3 ~ as.numeric(3),
    DebtToIncome_cat_num == 4 ~ as.numeric(4),
    DebtToIncome_cat_num == 5 ~ as.numeric(5),
    DebtToIncome_cat_num == 6 ~ as.numeric(6),
    DebtToIncome_cat_num == 7 ~ as.numeric(7),
    DebtToIncome_cat_num == 8 ~ as.numeric(8),
    DebtToIncome_cat_num == 9 ~ as.numeric(9),
    DebtToIncome_cat_num == 10 ~ as.numeric(10),
    DebtToIncome_cat_num == 11 ~ as.numeric(11),
    DebtToIncome_cat_num == 12 ~ as.numeric(12),
    DebtToIncome_cat_num == 13 ~ as.numeric(13),
    DebtToIncome_cat_num == 14 ~ as.numeric(14),
    DebtToIncome_cat_num == 15 ~ as.numeric(15),
    DebtToIncome_cat_num == 16 ~ as.numeric(16),
    DebtToIncome_cat_num == 17 ~ as.numeric(17),
    DebtToIncome_cat_num == 18 ~ as.numeric(18),
    DebtToIncome_cat_num == 19 ~ as.numeric(19),
    DebtToIncome_cat_num == 20 ~ as.numeric(20),
    is.na(DebtToIncome_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(DebtToIncome_cat_num)
  ))

#DebtToIncome Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(DebtToIncome_dummy0 = if_else(DebtToIncome_cat_num2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(DebtToIncome_dummy1 = if_else(DebtToIncome_cat_num2 == 2, 1, 0))

# Kategóriák létrehozása a percentilis határok alapján
FI_houldout_set<- FI_houldout_set%>%
  mutate(LiabilitiesTotal_cat = cut(LiabilitiesTotal, breaks = unique(percentile_breaks_LiabilitiesTotal_FI), include.lowest = TRUE),
         LiabilitiesTotal_cat_num = as.numeric(LiabilitiesTotal_cat))

FI_houldout_set<- FI_houldout_set%>%
  mutate(LiabilitiesTotal_cat_num2 = case_when(
    LiabilitiesTotal_cat_num == 1 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 2 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 3 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 4 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 5 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 6 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 7 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 8 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 9 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 10 ~ as.numeric(1),
    LiabilitiesTotal_cat_num == 11 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 12 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 13 ~ as.numeric(11),
    LiabilitiesTotal_cat_num == 14 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 15 ~ as.numeric(14),
    LiabilitiesTotal_cat_num == 16 ~ as.numeric(16),
    LiabilitiesTotal_cat_num == 17 ~ as.numeric(17),
    LiabilitiesTotal_cat_num == 18 ~ as.numeric(18),
    LiabilitiesTotal_cat_num == 19 ~ as.numeric(19),
    LiabilitiesTotal_cat_num == 20 ~ as.numeric(20),
    is.na(LiabilitiesTotal_cat_num) ~ as.numeric(1),
    TRUE ~ as.numeric(LiabilitiesTotal_cat_num)
  ))


#LiabilitiesTotal Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(LiabilitiesTotal_dummy0 = if_else(LiabilitiesTotal_cat_num2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(LiabilitiesTotal_dummy1 = if_else(LiabilitiesTotal_cat_num2 == 11, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(LiabilitiesTotal_dummy2 = if_else(LiabilitiesTotal_cat_num2 == 14, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(ExistingLiabilities2 = case_when(
    ExistingLiabilities == 3 ~ as.numeric(2),
    ExistingLiabilities == 4 ~ as.numeric(2),
    ExistingLiabilities == 5 ~ as.numeric(2),
    ExistingLiabilities > 6 ~ as.numeric(6),
    is.na(ExistingLiabilities) ~ as.numeric(1),
    TRUE ~ as.numeric(ExistingLiabilities)
  ))

#ExistingLiabilities Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(ExistingLiabilities_dummy0 = if_else(ExistingLiabilities2 == 2, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(ExistingLiabilities_dummy1 = if_else(ExistingLiabilities2 == 0, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(ExistingLiabilities_dummy2 = if_else(ExistingLiabilities2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(ExistingLiabilities_dummy3 = if_else(ExistingLiabilities2 == 6, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(HomeOwnershipType2 = case_when(
    HomeOwnershipType == -1 ~ as.numeric(1),
    HomeOwnershipType == 0 ~ as.numeric(1),
    HomeOwnershipType == 2 ~ as.numeric(2),
    HomeOwnershipType == 3 ~ as.numeric(2),
    HomeOwnershipType == 4 ~ as.numeric(2),
    HomeOwnershipType == 5 ~ as.numeric(2),
    HomeOwnershipType == 6 ~ as.numeric(2),
    HomeOwnershipType == 7 ~ as.numeric(1),
    HomeOwnershipType == 8 ~ as.numeric(1),
    HomeOwnershipType == 9 ~ as.numeric(1),
    HomeOwnershipType == 10 ~ as.numeric(1),
    is.na(HomeOwnershipType) ~ as.numeric(1),
    TRUE ~ as.numeric(HomeOwnershipType)
  ))

#HomeOwnershipType Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(HomeOwnershipType_dummy0 = if_else(HomeOwnershipType2 == 1, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(HomeOwnershipType_dummy1 = if_else(HomeOwnershipType2 == 2, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer2 = case_when(
    EmploymentDurationCurrentEmployer == "" ~ 6,
    EmploymentDurationCurrentEmployer == "MoreThan5Years" ~ 4,
    EmploymentDurationCurrentEmployer == "Other" ~ 6,
    EmploymentDurationCurrentEmployer == "Retiree" ~ 1,
    EmploymentDurationCurrentEmployer == "TrialPeriod" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo1Year" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo2Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo3Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo4Years" ~ 1,
    EmploymentDurationCurrentEmployer == "UpTo5Years" ~ 4,
    TRUE ~ NA_real_
  ))

#EmploymentDurationCurrentEmployer Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy0 = if_else(EmploymentDurationCurrentEmployer2 == 4, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy1 = if_else(EmploymentDurationCurrentEmployer2 == 1, 1, 0))

#EmploymentDurationCurrentEmployer Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(EmploymentDurationCurrentEmployer_dummy2 = if_else(EmploymentDurationCurrentEmployer2 == 6, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Application_Time2 = case_when(
    Application_Time == 2 ~ as.numeric(3),
    
    TRUE ~ as.numeric(Application_Time)
  ))

#Application_Time Dummy
FI_houldout_set<- FI_houldout_set%>%
  mutate(Application_Time_dummy0 = if_else(Application_Time2 == 3, 1, 0))

FI_houldout_set<- FI_houldout_set%>%
  mutate(Application_Time_dummy1 = if_else(Application_Time2 == 1, 1, 0))


#########################################################################################################################################
#########################################################################################################################################
# Eredmények összehasonlítása #

#########################################################################################################################################
#########################################################################################################################################


# Ellenõrzés, hogy minden oszlop jelen van-e
missing_columns <- expected_columns_EE[!expected_columns_EE %in% names(EE_houldout_set)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}

# A modellezett PD becslése az együtthatók felhasználásával
EE_houldout_set <- EE_houldout_set %>%
  mutate(PD_EST_EE = -0.783481644 +
           0.252559373 * Education_dummy1 +
           -0.307016092 * Education_dummy2 +
           -0.117883285 * Gender_dummy1 +
           -0.988554774 * LoanDuration_dummy1 +
           -0.675477966 * LoanDuration_dummy2 +
           -0.087560342 * NoOfPreviousLoansBeforeLoan_dummy1 +
           -0.339152801 * NoOfPreviousLoansBeforeLoan_dummy2 +
           0.229659225 * Age_dummy1 +
           0.034193207 * Age_dummy2 +
           -0.129655529 * Age_dummy5 +
           -0.205238154 * AppliedAmount_dummy2 +
           0.039073354 * AppliedAmount_dummy3 +
           0.124734707 * MonthlyPaymentDay_dummy1 +
           0.091273373 * IncomeTotal_dummy2 +
           -0.051563673 * IncomeTotal_dummy4 +
           0.313899859 * HomeOwnershipType_dummy1 +
           0.036477666 * EmploymentDurationCurrentEmployer_dummy1 +
           -0.131059235 * EmploymentDurationCurrentEmployer_dummy2)

# Valószínûséggé konvertálás
EE_houldout_set <- EE_houldout_set %>%
  mutate(PD_EST_EE = 1 / (1 + exp(-PD_EST_EE)))

# Gini-együttható számítása: PD
roc_curve <- roc(response = EE_houldout_set$Default_flag_2y, predictor = EE_houldout_set$PD_EST_EE)
auc_value <- auc(roc_curve)
gini_coefficient_EE_sajat_PD <- 2 * auc_value - 1

# Rating kategóriák átalakítása számmá
EE_houldout_set$Rating <- factor(EE_houldout_set$Rating, levels = c("AA", "A", "B", "C", "D", "E", "F", "HR"))

# Számok hozzárendelése a kategóriákhoz
EE_houldout_set$Rating_cat <- as.integer(EE_houldout_set$Rating)

# Gini-együttható számítása: PD
roc_curve <- roc(response = EE_houldout_set$Default_flag_1y, predictor = EE_houldout_set$ProbabilityOfDefault)
auc_value <- auc(roc_curve)
gini_coefficient_EE_Bondora_PD <- 2 * auc_value - 1

# Gini-együttható számítása: ECL
roc_curve <- roc(response = EE_houldout_set$Default_flag_1y, predictor = EE_houldout_set$ExpectedLoss)
auc_value <- auc(roc_curve)
gini_coefficient_EE_Bondora_ECL <- 2 * auc_value - 1

# Gini-együttható számítása: Rating
roc_curve <- roc(response = EE_houldout_set$Default_flag_1y, predictor = EE_houldout_set$Rating_cat)
auc_value <- auc(roc_curve)
gini_coefficient_EE_Bondora_Rating <- 2 * auc_value - 1

# Gini-együttható kiíratása
print(paste("Gini-együttható Bondora PD: ", gini_coefficient_EE_Bondora_PD))
print(paste("Gini-együttható Bondora ECL: ", gini_coefficient_EE_Bondora_ECL))
print(paste("Gini-együttható Bondora Rating: ", gini_coefficient_EE_Bondora_Rating))
# Gini-együttható sajátok
print(paste("Gini-együttható saját PD: ", gini_coefficient_EE_sajat_PD))




############################################################################################################################


# Ellenõrzés, hogy minden oszlop jelen van-e
missing_columns <- expected_columns_ES[!expected_columns_ES %in% names(ES_houldout_set)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}


ES_houldout_set <- ES_houldout_set %>%
  mutate(PD_EST_EE = 1.182980292 + 
           0.274953097 * Education_dummy1 +
           -0.202612112 * Education_dummy2 +
           0.091691831 * MaritalStatus_dummy1 +
           -0.350833817 * NoOfPreviousLoansBeforeLoan_dummy1 +
           -0.92061609 * NoOfPreviousLoansBeforeLoan_dummy2 +
           -0.490053256 * PreviousEarlyRepaymentsCountBeforeLoan_dummy1 +
           -0.292883369 * AppliedAmount_dummy1 +
           0.131676199 * IncomeTotal_dummy1 +
           -0.177187646 * IncomeTotal_dummy3 +
           -0.199056236 * LiabilitiesTotal_dummy3 +
           0.044620787 * EmploymentDurationCurrentEmployer_dummy1 +
           -0.140267864 * EmploymentDurationCurrentEmployer_dummy2)


# Valószínûséggé konvertálás
ES_houldout_set <- ES_houldout_set %>%
  mutate(PD_EST_EE = 1 / (1 + exp(-PD_EST_EE)))

# Gini-együttható számítása: PD
roc_curve <- roc(response = ES_houldout_set$Default_flag_2y, predictor = ES_houldout_set$PD_EST_EE)
auc_value <- auc(roc_curve)
gini_coefficient_ES_sajat_PD <- 2 * auc_value - 1

# Rating kategóriák átalakítása számmá
ES_houldout_set$Rating <- factor(ES_houldout_set$Rating, levels = c("AA", "A", "B", "C", "D", "E", "F", "HR"))

# Számok hozzárendelése a kategóriákhoz
ES_houldout_set$Rating_cat <- as.integer(ES_houldout_set$Rating)

# Gini-együttható számítása: PD
roc_curve <- roc(response = ES_houldout_set$Default_flag_1y, predictor = ES_houldout_set$ProbabilityOfDefault)
auc_value <- auc(roc_curve)
gini_coefficient_ES_Bondora_PD <- 2 * auc_value - 1

# Gini-együttható számítása: ECL
roc_curve <- roc(response = ES_houldout_set$Default_flag_1y, predictor = ES_houldout_set$ExpectedLoss)
auc_value <- auc(roc_curve)
gini_coefficient_ES_Bondora_ECL <- 2 * auc_value - 1

# Gini-együttható számítása: Rating
roc_curve <- roc(response = ES_houldout_set$Default_flag_1y, predictor = ES_houldout_set$Rating_cat)
auc_value <- auc(roc_curve)
gini_coefficient_ES_Bondora_Rating <- 2 * auc_value - 1

# Gini-együttható kiíratása
print(paste("Gini-együttható: ", gini_coefficient_ES_Bondora_PD))
print(paste("Gini-együttható: ", gini_coefficient_ES_Bondora_ECL))
print(paste("Gini-együttható: ", gini_coefficient_ES_Bondora_Rating))
# Gini-együttható sajátok
print(paste("Gini-együttható saját PD: ", gini_coefficient_ES_sajat_PD))




############################################################################################################################

# Ellenõrzés, hogy minden oszlop jelen van-e
missing_columns <- expected_columns_FI[!expected_columns_FI %in% names(FI_houldout_set)]

if (length(missing_columns) == 0) {
  print("All expected columns are present.")
} else {
  print("Missing columns:")
  print(missing_columns)
}

FI_houldout_set <- FI_houldout_set %>%
  mutate(PD_EST_EE = -0.208486721 +
           0.124688476 * Education_dummy1 +
           -0.179850376 * Education_dummy2 +
           -0.33586684 * Gender_dummy1 +
           -1.367848364 * LoanDuration_dummy1 +
           -0.82332706 * LoanDuration_dummy2 +
           -0.162115232 * NoOfPreviousLoansBeforeLoan_dummy1 +
           -1.001652291 * AppliedAmount_dummy1 +
           -0.786315038 * AppliedAmount_dummy2 +
           -0.54635562 * AppliedAmount_dummy3 +
           0.250119427 * AppliedAmount_dummy4 +
           -0.313068949 * AppliedAmount_dummy5 +
           0.715680449 * AppliedAmount_dummy6 +
           0.092283049 * IncomeTotal_dummy1 +
           -0.153550594 * IncomeTotal_dummy2 +
           -0.277480312 * IncomeTotal_dummy3 +
           -0.070746466 * LiabilitiesTotal_dummy0 +
           0.080086424 * LiabilitiesTotal_dummy2 +
           -0.129396713 * ExistingLiabilities_dummy2 +
           0.060173164 * ExistingLiabilities_dummy3 +
           0.447039034 * HomeOwnershipType_dummy1 +
           0.089171237 * EmploymentDurationCurrentEmployer_dummy1 +
           -0.114320464 * EmploymentDurationCurrentEmployer_dummy2)

# Valószínûséggé konvertálás
FI_houldout_set <- FI_houldout_set %>%
  mutate(PD_EST_EE = 1 / (1 + exp(-PD_EST_EE)))

# Gini-együttható számítása: PD
roc_curve <- roc(response = FI_houldout_set$Default_flag_2y, predictor = FI_houldout_set$PD_EST_EE)
auc_value <- auc(roc_curve)
gini_coefficient_FI_sajat_PD <- 2 * auc_value - 1

# Rating kategóriák átalakítása számmá
FI_houldout_set$Rating <- factor(FI_houldout_set$Rating, levels = c("AA", "A", "B", "C", "D", "E", "F", "HR"))

# Számok hozzárendelése a kategóriákhoz
FI_houldout_set$Rating_cat <- as.integer(FI_houldout_set$Rating)

# Gini-együttható számítása: PD
roc_curve <- roc(response = FI_houldout_set$Default_flag_1y, predictor = FI_houldout_set$ProbabilityOfDefault)
auc_value <- auc(roc_curve)
gini_coefficient_FI_Bondora_PD <- 2 * auc_value - 1

# Gini-együttható számítása: ECL
roc_curve <- roc(response = FI_houldout_set$Default_flag_1y, predictor = FI_houldout_set$ExpectedLoss)
auc_value <- auc(roc_curve)
gini_coefficient_FI_Bondora_ECL <- 2 * auc_value - 1

# Gini-együttható számítása: Rating
roc_curve <- roc(response = FI_houldout_set$Default_flag_1y, predictor = FI_houldout_set$Rating_cat)
auc_value <- auc(roc_curve)
gini_coefficient_FI_Bondora_Rating <- 2 * auc_value - 1

# Gini-együttható kiíratása
print(paste("Gini-együttható: ", gini_coefficient_FI_Bondora_PD))
print(paste("Gini-együttható: ", gini_coefficient_FI_Bondora_ECL))
print(paste("Gini-együttható: ", gini_coefficient_FI_Bondora_Rating))
# Gini-együttható sajátok
print(paste("Gini-együttható saját PD: ", gini_coefficient_FI_sajat_PD))


############################################################################################################################


EE_selected <- subset( EE_houldout_set, select=c(ProbabilityOfDefault, PD_EST_EE, Default_flag_2y, Default_flag_1y, ExpectedLoss, Rating_cat)) 
ES_selected <- subset( ES_houldout_set, select=c(ProbabilityOfDefault, PD_EST_EE, Default_flag_2y, Default_flag_1y, ExpectedLoss, Rating_cat)) 
FI_selected <- subset( FI_houldout_set, select=c(ProbabilityOfDefault, PD_EST_EE, Default_flag_2y, Default_flag_1y, ExpectedLoss, Rating_cat)) 
combined_data <- bind_rows(EE_selected, ES_selected, FI_selected)



# Gini-együttható számítása
roc_curve_all <- roc(response = combined_data$Default_flag_2y, predictor = combined_data$PD_EST_EE)
auc_value <- auc(roc_curve_all)
gini_coefficient_ALL_sajat <- 2 * auc_value - 1

# Gini-együttható számítása: PD
roc_curve_all2 <- roc(response = combined_data$Default_flag_1y, predictor = combined_data$ProbabilityOfDefault)
auc_value <- auc(roc_curve_all2)
gini_coefficient_ALL_Bondora_PD <- 2 * auc_value - 1

# Gini-együttható számítása: ECL
roc_curve_all2 <- roc(response = combined_data$Default_flag_1y, predictor = combined_data$ExpectedLoss)
auc_value <- auc(roc_curve_all2)
gini_coefficient_ALL_Bondora_ECL <- 2 * auc_value - 1

# Gini-együttható számítása: Rating
roc_curve_all2 <- roc(response = combined_data$Default_flag_1y, predictor = combined_data$Rating_cat)
auc_value <- auc(roc_curve_all2)
gini_coefficient_ALL_Bondora_Rating <- 2 * auc_value - 1

# Gini-együttható kiíratása
print(paste("Gini-együttható: ", gini_coefficient_ALL_Bondora_PD))
print(paste("Gini-együttható: ", gini_coefficient_ALL_Bondora_ECL))
print(paste("Gini-együttható: ", gini_coefficient_ALL_Bondora_Rating))
# Gini-együttható kiíratása
print(paste("Gini-együttható Saját pd: ", gini_coefficient_ALL_sajat))


#########################################################################################################################################
#########################################################################################################################################
# PD eloszlások

#########################################################################################################################################
#########################################################################################################################################


PD_sajat_EE_model <- EE_modelling_data$Default_flag_2y
PD_sajat_ES_model <- ES_modelling_data$Default_flag_2y
PD_sajat_FI_model <- FI_modelling_data$Default_flag_2y

PD_sajat_EE_holdout <- EE_selected$PD_EST_EE
PD_sajat_ES_holdout <- ES_selected$PD_EST_EE
PD_sajat_FI_holdout <- FI_selected$PD_EST_EE


# Excel fájl létrehozása az elsõ munkalappal
wb <- createWorkbook()
addWorksheet(wb, "EE_model")
writeData(wb, "EE_model", PD_sajat_EE_model)
saveWorkbook(wb, "PD_dist.xlsx", overwrite = TRUE)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PD_dist.xlsx")
addWorksheet(wb, "ES_model")
writeData(wb, "ES_model", PD_sajat_ES_model)
saveWorkbook(wb, "PD_dist.xlsx", overwrite = TRUE)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PD_dist.xlsx")
addWorksheet(wb, "FI_model")
writeData(wb, "FI_model", PD_sajat_FI_model)
saveWorkbook(wb, "PD_dist.xlsx", overwrite = TRUE)


# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PD_dist.xlsx")
addWorksheet(wb, "EE_hold")
writeData(wb, "EE_hold", PD_sajat_EE_holdout)
saveWorkbook(wb, "PD_dist.xlsx", overwrite = TRUE)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PD_dist.xlsx")
addWorksheet(wb, "ES_hold")
writeData(wb, "ES_hold", PD_sajat_ES_holdout)
saveWorkbook(wb, "PD_dist.xlsx", overwrite = TRUE)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("PD_dist.xlsx")
addWorksheet(wb, "FI_hold")
writeData(wb, "FI_hold", PD_sajat_FI_holdout)
saveWorkbook(wb, "PD_dist.xlsx", overwrite = TRUE)



#########################################################################################################################################
#########################################################################################################################################
# Default eloszlások

#########################################################################################################################################
#########################################################################################################################################


total_samples <- nrow(EE_modelling_data)

Default_chk <- EE_modelling_data %>% 
  group_by(Default_flag_1y,Default_flag_2y,Default_flag_3y,Default_flag_4y,Default_flag_lifetime) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Excel fájl létrehozása az elsõ munkalappal
wb <- createWorkbook()
addWorksheet(wb, "EE_modelling_data")
writeData(wb, "EE_modelling_data", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)
###################


total_samples <- nrow(EE_modelling_data)

Default_chk <- EE_modelling_data %>% 
  group_by(Year,Default_flag_2y) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "EE_modelling_data_Year")
writeData(wb, "EE_modelling_data_Year", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)
###################


total_samples <- nrow(ES_modelling_data)

Default_chk <- ES_modelling_data %>% 
  group_by(Default_flag_1y,Default_flag_2y,Default_flag_3y,Default_flag_4y,Default_flag_lifetime) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "ES_modelling_data")
writeData(wb, "ES_modelling_data", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)
###################


total_samples <- nrow(ES_modelling_data)

Default_chk <- ES_modelling_data %>% 
  group_by(Year,Default_flag_2y) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "ES_modelling_data_Year")
writeData(wb, "ES_modelling_data_Year", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)
###################

total_samples <- nrow(FI_modelling_data)

Default_chk <- FI_modelling_data %>% 
  group_by(Default_flag_1y,Default_flag_2y,Default_flag_3y,Default_flag_4y,Default_flag_lifetime) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "FI_modelling_data")
writeData(wb, "FI_modelling_data", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)
###################


Default_chk <- FI_modelling_data %>% 
  group_by(Year,Default_flag_2y) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "FI_modelling_data_Year")
writeData(wb, "FI_modelling_data_Year", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)
###################

modelling_data <- rbind(EE_modelling_data, ES_modelling_data, FI_modelling_data)

total_samples <- nrow(modelling_data)

Default_chk <- modelling_data %>% 
  group_by(Default_flag_1y,Default_flag_2y,Default_flag_3y,Default_flag_4y,Default_flag_lifetime) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "modelling_data")
writeData(wb, "modelling_data", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)
###################


total_samples <- nrow(modelling_data)

Default_chk <- modelling_data %>% 
  group_by(Year,Default_flag_2y) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "modelling_data_Year")
writeData(wb, "modelling_data_Year", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)


###################
total_samples <- nrow(LoanData)

Default_chk <- LoanData %>% 
  group_by(Year,Default_flag_2y,Country) %>% 
  summarise(
    count = n(),
    N = total_samples)

# Létezõ Excel-fájl megnyitása és új munkalap hozzáadása
wb <- loadWorkbook("Default_flag.xlsx")
addWorksheet(wb, "Dist_live_data")
writeData(wb, "Dist_live_data", Default_chk)
saveWorkbook(wb, "Default_flag.xlsx", overwrite = TRUE)


#########################################################################################################################################


# Defaulthoz az összes lehetséges változó és minta

# Nem Live Data
EE_modelling_data
ES_modelling_data
FI_modelling_data
modelling_data <- rbind(EE_modelling_data, ES_modelling_data, FI_modelling_data)

# Holdout set
EE_houldout_set
ES_houldout_set
FI_houldout_set
houldout_set <- rbind(EE_houldout_set, ES_houldout_set, FI_houldout_set)

# Train-Test
EE_train_test
ES_train_test
FI_train_test
train_test <- rbind(EE_train_test, ES_train_test, FI_train_test)


Year
Fold
Default_flag_lifetime
Default_flag_4y
Default_flag_3y
Default_flag_2y
Default_flag_1y

#########################################################################################################################################
#########################################################################################################################################


