# import libraries --------------------------------------------------------
library(plyr)
library(dplyr)
library(readxl)
library(writexl)
library(fastDummies)
library(tidyr)
# load sales volume data of Market Kurly-------------------------------------------------------------------------
load("hmr.data.final2.Rdata")
hmr.data <- hmr.data.final2

# detail category analysis --------------------------------------------------------

# select 육류가공식품=meat 
hmr.meat <- hmr.data %>% filter(cat_small1_new == "육류가공식품")

## 1) 첫번째 페이지 왼쪽 그래프
# stew year sales, sold (연별 육류가공식품 매출액 및 제품 수)
# output = meat.y.sales
meat.y.sales <- hmr.meat %>%
  group_by(year) %>% 
  dplyr::summarise(totsales = sum(sales, na.rm=TRUE),
                   totsku = sum(sold))


# stew sku CAGR (17-20년 제품 수 CAGR)
# output = stew.sku.cagr
meat.sku.cagr <- meat.y.sales %>% 
  select(year, totsku) %>% 
  spread(year, totsku)

Oldlist <- colnames(meat.sku.cagr)
Newlist <- c("sku2017","sku2018","sku2019","sku2020","sku2021")
colnames(meat.sku.cagr) <- Newlist

meat.sku.cagr <- meat.sku.cagr %>% 
  mutate(skuCAGR=((sku2020/sku2017)^(1/(2020-2017)))-1) %>% 
  select(skuCAGR)



## 2) 첫번째 페이지 제품당 매출액 및 판매량 추이 그래프
# stew year month salessku and volumesku (output = stew.ym.sales)
meat.ym.sales <- hmr.meat %>%
  group_by(year, month) %>% 
  dplyr::summarise(totsales = sum(sales, na.rm=TRUE),
                   totsku = sum(sold),
                   salessku = totsales/totsku) %>% 
  select(year, month, salessku) %>% 
  spread(year, salessku)

write_xlsx(meat.ym.sales, path = "meat.ym.sales.xlsx")

meat.ym.volume <- hmr.meat %>%
  group_by(year, month) %>% 
  dplyr::summarise(totsku = sum(sold),
                   totvolume = sum(volume, na.rm=TRUE), 
                   volumesku = totvolume/totsku) %>% 
  select(year, month, volumesku) %>% 
  spread(year, volumesku)

write_xlsx(meat.ym.volume, path = "meat.ym.volume.xlsx")

## 3) 두번째 페이지 왼쪽 그래프 = 품목별 판매량 비중 추이
# storage year volume ratio (output = stew.vol)
meat.vol <- hmr.meat %>% 
  group_by(year) %>%
  dplyr::summarise(totvolume = sum(volume, na.rm=TRUE))


meat.detail.vol <- hmr.meat %>%  
  group_by(year, cat_small1) %>%
  dplyr::summarise(totvolume = sum(volume, na.rm=TRUE)) %>%
  spread(cat_small1, totvolume)

names(meat.detail.vol)


meat.vol.rat <- meat.detail.vol %>%
  left_join(meat.vol, by = "year") %>% 
  mutate(쇠고기가공식품 = 쇠고기가공식품/totvolume*100,
            돼지고기가공식품 = 돼지고기가공식품/totvolume*100,
            닭고기가공식품 = 닭고기가공식품/totvolume*100,
            오리고기가공식품 = 오리고기가공식품/totvolume*100,
            난류가공식품 = 난류가공식품/totvolume*100,
            육류가공식품 = totvolume) %>% 
  select(c(year, 쇠고기가공식품, 돼지고기가공식품, 닭고기가공식품, 오리고기가공식품, 난류가공식품, 육류가공식품))




## 4) 두번째 페이지 중간 그래프
# step1 : stew.vol 데이터 기준으로 즉석국탕찌개류 품목 중 판매량 비중이 증가한 품목 select
# step2 : 국탕류 월별 제품당 판매량 추이 구하기
chicken.ym.volume <- hmr.meat %>%
  filter(cat_small1 == "닭고기가공식품") %>% 
  group_by(year, month) %>% 
  dplyr::summarise(totsku = sum(sold),
                   totvolume = sum(volume, na.rm=TRUE), 
                   volumesku = totvolume/totsku) %>% 
  select(year, month, volumesku) %>% 
  spread(year, volumesku)

write_xlsx(chicken.ym.volume, path = "chicken.ym.volume.xlsx")

pork.ym.volume <- hmr.meat %>%
  filter(cat_small1 == "돼지고기가공식품") %>% 
  group_by(year, month) %>% 
  dplyr::summarise(totsku = sum(sold),
                   totvolume = sum(volume, na.rm=TRUE), 
                   volumesku = totvolume/totsku) %>% 
  select(year, month, volumesku) %>% 
  spread(year, volumesku)

write_xlsx(pork.ym.volume, path = "pork.ym.volume.xlsx")

beef.ym.volume <- hmr.meat %>%
  filter(cat_small1 == "쇠고기가공식품") %>% 
  group_by(year, month) %>% 
  dplyr::summarise(totsku = sum(sold),
                   totvolume = sum(volume, na.rm=TRUE), 
                   volumesku = totvolume/totsku) %>% 
  select(year, month, volumesku) %>% 
  spread(year, volumesku)

write_xlsx(beef.ym.volume, path = "beef.ym.volume.xlsx")

## output 엑셀파일로 저장하기
write_xlsx(list(sheet1.1 = meat.y.sales, sheet1.2 = meat.sku.cagr, sheet2 = meat.ym.sales, sheet3 = meat.ym.volume,
                sheet4 = meat.vol.rat, sheet5 = chicken.ym.volume), 'meat_cat_result.xlsx')



# step3 : 특정 연월 2021년 판매량 top 제품 뽑기
# 21년 3월
meat.21.03 <- hmr.meat %>% 
  filter(cat_small1 == "돼지고기가공식품") %>% 
  filter(year=="2021"&month=="8") %>% 
  group_by(product) %>% 
  dplyr::summarise(totvolume2 = sum(volume, na.rm=TRUE))


#21년 2월
meat.21.02 <- hmr.meat %>% 
  filter(cat_small1 == "돼지고기가공식품") %>% 
  filter(year=="2021"&month=="5") %>% 
  group_by(product) %>% 
  dplyr::summarise(totvolume1 = sum(volume, na.rm=TRUE))


#21년 2월 대비 3월 판매량 증가율
meat.product <- meat.21.03 %>% 
  left_join(meat.21.02, by = "product") %>% 
  mutate(rat = (totvolume2-totvolume1)/totvolume1*100)
