# import libraries --------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

load("hmr.data.final4.Rdata")
write_xlsx(hmr.data.final4, path = "hmr_1.xlsx")

# 20-21년 판매량 상위 제품 뽑기
# totvolume = 판매량, price = 가격, g = 용량, storage =품온
beef <- hmr.data.final4 %>% 
  filter(cat_small1 == "쇠고기가공식품") %>% 
  filter(year >= "2020") %>% 
  group_by(product) %>% 
  summarise(totvolume = sum(volume, na.rm=TRUE),
            price = price,
            g = g,
            storage = storage) %>% 
  arrange(desc(totvolume))

beef <- unique(beef) %>% arrange(desc(totvolume))



# 출시 연월 구하기 (위에서 top4 제품명 검색해서 출시연월 적기)
chicken.ym <- hmr.data.final4 %>% 
  filter(cat_small1 == "닭고기가공식품") %>% 
  filter(sold == "1") %>% 
  group_by(product) %>% 
  mutate(min.year = min(year), min.month = min(month)) %>%
  summarise(min.year = min.year,
            min.month = min.month)

chicken.ym <- unique(chicken.ym)
