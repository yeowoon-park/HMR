
# import libraries --------------------------------------------------------
library(plyr)
library(dplyr)
library(readxl)
library(writexl)
library(fastDummies)
# load sales volume data of Market Kurly-------------------------------------------------------------------------
sales <- read_excel("salestotal.xlsx", sheet="Data", range="A1:BE4434", col_names=TRUE, col_types="guess", na="NA")
product <- read_excel("hmr_1.xlsx", sheet="final", range="A1:L3368", col_names=TRUE, col_types="guess", na="NA")

#2017
X2017_1 <- sales %>% select(product, X2017_1) %>% rename("volume"="X2017_1") %>% mutate(year = 2017) %>% mutate(month = 1) 
X2017_2 <- sales %>% select(product, X2017_2) %>% rename("volume"="X2017_2") %>% mutate(year = 2017) %>% mutate(month = 2)
X2017_3 <- sales %>% select(product, X2017_3) %>% rename("volume"="X2017_3") %>% mutate(year = 2017) %>% mutate(month = 3)
X2017_4 <- sales %>% select(product, X2017_4) %>% rename("volume"="X2017_4") %>% mutate(year = 2017) %>% mutate(month = 4)
X2017_5 <- sales %>% select(product, X2017_5) %>% rename("volume"="X2017_5") %>% mutate(year = 2017) %>% mutate(month = 5)
X2017_6 <- sales %>% select(product, X2017_6) %>% rename("volume"="X2017_6") %>% mutate(year = 2017) %>% mutate(month = 6)
X2017_7 <- sales %>% select(product, X2017_7) %>% rename("volume"="X2017_7") %>% mutate(year = 2017) %>% mutate(month = 7)
X2017_8 <- sales %>% select(product, X2017_8) %>% rename("volume"="X2017_8") %>% mutate(year = 2017) %>% mutate(month = 8)
X2017_9 <- sales %>% select(product, X2017_9) %>% rename("volume"="X2017_9") %>% mutate(year = 2017) %>% mutate(month = 9)
X2017_10 <- sales %>% select(product, X2017_10) %>% rename("volume"="X2017_10") %>% mutate(year = 2017) %>% mutate(month = 10)
X2017_11 <- sales %>% select(product, X2017_11) %>% rename("volume"="X2017_11") %>% mutate(year = 2017) %>% mutate(month = 11)
X2017_12 <- sales %>% select(product, X2017_12) %>% rename("volume"="X2017_12") %>% mutate(year = 2017) %>% mutate(month = 12)

#2018
X2018_1 <- sales %>% select(product, X2018_1) %>% rename("volume"="X2018_1") %>% mutate(year = 2018) %>% mutate(month = 1) 
X2018_2 <- sales %>% select(product, X2018_2) %>% rename("volume"="X2018_2") %>% mutate(year = 2018) %>% mutate(month = 2) 
X2018_3 <- sales %>% select(product, X2018_3) %>% rename("volume"="X2018_3") %>% mutate(year = 2018) %>% mutate(month = 3) 
X2018_4 <- sales %>% select(product, X2018_4) %>% rename("volume"="X2018_4") %>% mutate(year = 2018) %>% mutate(month = 4) 
X2018_5 <- sales %>% select(product, X2018_5) %>% rename("volume"="X2018_5") %>% mutate(year = 2018) %>% mutate(month = 5) 
X2018_6 <- sales %>% select(product, X2018_6) %>% rename("volume"="X2018_6") %>% mutate(year = 2018) %>% mutate(month = 6) 
X2018_7 <- sales %>% select(product, X2018_7) %>% rename("volume"="X2018_7") %>% mutate(year = 2018) %>% mutate(month = 7) 
X2018_8 <- sales %>% select(product, X2018_8) %>% rename("volume"="X2018_8") %>% mutate(year = 2018) %>% mutate(month = 8) 
X2018_9 <- sales %>% select(product, X2018_9) %>% rename("volume"="X2018_9") %>% mutate(year = 2018) %>% mutate(month = 9) 
X2018_10 <- sales %>% select(product, X2018_10) %>% rename("volume"="X2018_10") %>% mutate(year = 2018) %>% mutate(month = 10) 
X2018_11 <- sales %>% select(product, X2018_11) %>% rename("volume"="X2018_11") %>% mutate(year = 2018) %>% mutate(month = 11) 
X2018_12 <- sales %>% select(product, X2018_12) %>% rename("volume"="X2018_12") %>% mutate(year = 2018) %>% mutate(month = 12) 

#2019
X2019_1 <- sales %>% select(product, X2019_1) %>% rename("volume"="X2019_1") %>% mutate(year = 2019) %>% mutate(month = 1)
X2019_2 <- sales %>% select(product, X2019_2) %>% rename("volume"="X2019_2") %>% mutate(year = 2019) %>% mutate(month = 2)
X2019_3 <- sales %>% select(product, X2019_3) %>% rename("volume"="X2019_3") %>% mutate(year = 2019) %>% mutate(month = 3)
X2019_4 <- sales %>% select(product, X2019_4) %>% rename("volume"="X2019_4") %>% mutate(year = 2019) %>% mutate(month = 4)
X2019_5 <- sales %>% select(product, X2019_5) %>% rename("volume"="X2019_5") %>% mutate(year = 2019) %>% mutate(month = 5)
X2019_6 <- sales %>% select(product, X2019_6) %>% rename("volume"="X2019_6") %>% mutate(year = 2019) %>% mutate(month = 6)
X2019_7 <- sales %>% select(product, X2019_7) %>% rename("volume"="X2019_7") %>% mutate(year = 2019) %>% mutate(month = 7)
X2019_8 <- sales %>% select(product, X2019_8) %>% rename("volume"="X2019_8") %>% mutate(year = 2019) %>% mutate(month = 8)
X2019_9 <- sales %>% select(product, X2019_9) %>% rename("volume"="X2019_9") %>% mutate(year = 2019) %>% mutate(month = 9)
X2019_10 <- sales %>% select(product, X2019_10) %>% rename("volume"="X2019_10") %>% mutate(year = 2019) %>% mutate(month = 10)
X2019_11 <- sales %>% select(product, X2019_11) %>% rename("volume"="X2019_11") %>% mutate(year = 2019) %>% mutate(month = 11)
X2019_12 <- sales %>% select(product, X2019_12) %>% rename("volume"="X2019_12") %>% mutate(year = 2019) %>% mutate(month = 12)

#2020
X2020_1 <- sales %>% select(product, X2020_1) %>% rename("volume"="X2020_1") %>% mutate(year = 2020) %>% mutate(month = 1)
X2020_2 <- sales %>% select(product, X2020_2) %>% rename("volume"="X2020_2") %>% mutate(year = 2020) %>% mutate(month = 2)
X2020_3 <- sales %>% select(product, X2020_3) %>% rename("volume"="X2020_3") %>% mutate(year = 2020) %>% mutate(month = 3)
X2020_4 <- sales %>% select(product, X2020_4) %>% rename("volume"="X2020_4") %>% mutate(year = 2020) %>% mutate(month = 4)
X2020_5 <- sales %>% select(product, X2020_5) %>% rename("volume"="X2020_5") %>% mutate(year = 2020) %>% mutate(month = 5)
X2020_6 <- sales %>% select(product, X2020_6) %>% rename("volume"="X2020_6") %>% mutate(year = 2020) %>% mutate(month = 6)
X2020_7 <- sales %>% select(product, X2020_7) %>% rename("volume"="X2020_7") %>% mutate(year = 2020) %>% mutate(month = 7)
X2020_8 <- sales %>% select(product, X2020_8) %>% rename("volume"="X2020_8") %>% mutate(year = 2020) %>% mutate(month = 8)
X2020_9 <- sales %>% select(product, X2020_9) %>% rename("volume"="X2020_9") %>% mutate(year = 2020) %>% mutate(month = 9)
X2020_10 <- sales %>% select(product, X2020_10) %>% rename("volume"="X2020_10") %>% mutate(year = 2020) %>% mutate(month = 10)
X2020_11 <- sales %>% select(product, X2020_11) %>% rename("volume"="X2020_11") %>% mutate(year = 2020) %>% mutate(month = 11)
X2020_12 <- sales %>% select(product, X2020_12) %>% rename("volume"="X2020_12") %>% mutate(year = 2020) %>% mutate(month = 12)

#2021
X2021_1 <- sales %>% select(product, X2021_1) %>% rename("volume"="X2021_1") %>% mutate(year = 2021) %>% mutate(month = 1)
X2021_2 <- sales %>% select(product, X2021_2) %>% rename("volume"="X2021_2") %>% mutate(year = 2021) %>% mutate(month = 2)
X2021_3 <- sales %>% select(product, X2021_3) %>% rename("volume"="X2021_3") %>% mutate(year = 2021) %>% mutate(month = 3)
X2021_4 <- sales %>% select(product, X2021_4) %>% rename("volume"="X2021_4") %>% mutate(year = 2021) %>% mutate(month = 4)
X2021_5 <- sales %>% select(product, X2021_5) %>% rename("volume"="X2021_5") %>% mutate(year = 2021) %>% mutate(month = 5)
X2021_6 <- sales %>% select(product, X2021_6) %>% rename("volume"="X2021_6") %>% mutate(year = 2021) %>% mutate(month = 6)
X2021_7 <- sales %>% select(product, X2021_7) %>% rename("volume"="X2021_7") %>% mutate(year = 2021) %>% mutate(month = 7)
X2021_8 <- sales %>% select(product, X2021_8) %>% rename("volume"="X2021_8") %>% mutate(year = 2021) %>% mutate(month = 8)


X2017 <- rbind(X2017_1, X2017_2, X2017_3, X2017_4, X2017_5, X2017_6, X2017_7, X2017_8, X2017_9, X2017_10, X2017_11, X2017_12)
X2018 <- rbind(X2018_1, X2018_2, X2018_3, X2018_4, X2018_5, X2018_6, X2018_7, X2018_8, X2018_9, X2018_10, X2018_11, X2018_12)
X2019 <- rbind(X2019_1, X2019_2, X2019_3, X2019_4, X2019_5, X2019_6, X2019_7, X2019_8, X2019_9, X2019_10, X2019_11, X2019_12)
X2020 <- rbind(X2020_1, X2020_2, X2020_3, X2020_4, X2020_5, X2020_6, X2020_7, X2020_8, X2020_9, X2020_10, X2020_11, X2020_12)
X2021 <- rbind(X2021_1, X2021_2, X2021_3, X2021_4, X2021_5, X2021_6, X2021_7, X2021_8)

total <- rbind(X2017, X2018, X2019, X2020, X2021)
rm(sales)

# left join sales to product info -----------------------------------------
final <- left_join(product, total,  by = c('product'='product'))

final <- transform(final, sold = ifelse(volume==0, 0, 1)) %>% mutate(sales = price*volume)

dta <- as.data.frame(subset(final, select=c(product, brand, price, rmr, pb, g, storage, recipe, carb, carbtype, mprocat, mprocat2, year, month, volume, sales, sold)))

data <- dummy_cols(.data = dta, select_columns = c("storage"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("recipe"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("carb"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("carbtype"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("mprocat"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("mprocat2"), remove_first_dummy = FALSE)

data$y_m <- paste(data$year,"_", data$month, sep = "")

summary(data)

mprocat_t <- data %>% filter(mprocat2_t_meat == "1") %>% group_by(year, month) %>% summarise(mprocat_t = sum(sold))
mprocat_s <- data %>% filter(mprocat2_s_meat == "1") %>% group_by(year, month) %>% summarise(mprocat_s = sum(sold))
mprocat_p <- data %>% filter(mprocat2_p_meat == "1") %>% group_by(year, month) %>% summarise(mprocat_p = sum(sold))

sku <- data %>% group_by(year, month) %>% summarise(sku = sum(sold))

launch <- unique(data %>% filter(sold == "1") %>% 
  group_by(product) %>% 
  mutate(launch.year = min(year), launch.month = min(month)) %>%
  summarise(launch.year = launch.year,
            launch.month = launch.month))

launch.product <- launch %>% select(product)
product.name <- product %>% select(product)
no_sales <- anti_join(product.name, launch.product,  by = c('product'='product'))

data1 <- left_join(data, mprocat_t,  by = c('year'='year', 'month'='month'))
data2 <- left_join(data1, mprocat_s,  by = c('year'='year', 'month'='month'))
data3 <- left_join(data2, mprocat_p,  by = c('year'='year', 'month'='month'))
data4 <- left_join(data3, sku,  by = c('year'='year', 'month'='month'))
data5 <- left_join(data4, launch,  by = c('product'='product'))
data6 <- data5 %>% mutate(launch = (year-launch.year)*12+(month-launch.month)+1)
data7 <- data6 %>% mutate(t = (month-2017)*12+(month))

final <- transform(data7, volume = ifelse(volume==0, NA, volume))
final <- transform(final, sales = ifelse(volume==0, NA, sales))

write_xlsx(final, path = "dta3367.xlsx")
