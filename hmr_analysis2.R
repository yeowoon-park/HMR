# import libraries --------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)


# load sales volume data --------------------------------------------------------
load("hmr.data.final.Rdata")

# detail category analysis --------------------------------------------------------

# select 즉석국탕찌개=stew
hmr.meat <- hmr.data.final %>% filter(cat_small1_final == "육류가공식품")

## 1) 첫번째 페이지 왼쪽 그래프  --------------------------------------------------------
# stew year sales, sold (연별 즉석국탕찌개류 매출액 및 제품 수 = stew.y.tot)
# 연별 매출액
meat.y.sales <- hmr.meat %>%
  group_by(year) %>% 
  dplyr::summarise(totsales = sum(sales, na.rm=TRUE))

# 연별 SKU 수
meat.y.sku <- hmr.meat %>% filter(sold=="1") %>%
  group_by(year, product) %>% tally() %>% 
  group_by(year) %>% summarise(totsku=n())

meat.y.tot <- meat.y.sales %>% left_join(meat.y.sku, by="year")


# stew sku CAGR (18-20년 제품 수 CAGR)
# output = stew.sku.cagr
meat.sku.cagr <- meat.y.sku %>% 
  select(year, totsku) %>% 
  spread(year, totsku)

Oldlist <- colnames(meat.sku.cagr)
Newlist <- c("sku2017","sku2018","sku2019","sku2020","sku2021")
colnames(meat.sku.cagr) <- Newlist

meat.sku.cagr <- meat.sku.cagr %>% 
  mutate(skuCAGR=((sku2020/sku2018)^(1/(2020-2018)))-1) %>% 
  select(skuCAGR)


# 21년 매출액 추정치 구하기 (=ex2021_sales)
# 18~20년 전체 매출액
ex1 <- hmr.meat %>% filter(year %in% c(2018, 2019, 2020)) %>% 
  summarise(ex_sales=sum(sales, na.rm=TRUE))

# 18~20년 1~8월 매출액
ex2 <- hmr.meat %>% filter(year %in% c(2018, 2019, 2020)) %>% 
  filter(!month %in% c(9,10,11,12)) %>% 
  summarise(ex_1_8_sales=sum(sales, na.rm=TRUE))

# 21년 1~8월 매출액 (ex2021_sales)
ex3 <- hmr.meat %>% filter(year %in% c(2021)) %>%
  summarise(ex_2021_1_8_sales=sum(sales, na.rm=TRUE))

ex1_2 <- cbind(ex1, ex2)
ex2021_sales <- cbind(ex1_2, ex3) %>% 
  mutate(ex.rat = ex_1_8_sales/ex_sales,
         ex_2021_9_12_sales = ex_2021_1_8_sales/ex.rat-ex_2021_1_8_sales,
         ex_2021_sales = ex_2021_1_8_sales+ex_2021_9_12_sales) %>% 
  select(ex_2021_sales)



# 21년 제품 수 추정치 구하기 (ex2021_sku)
ex4 <- meat.y.sku %>% filter(year == "2020") %>% select(-(year)) 
ex2021_sku <- cbind(ex4, meat.sku.cagr) %>% 
  mutate(ex_2021_sku = (totsku*meat.sku.cagr)+totsku,
         ex_2021_9_12_sku = ex_2021_sku-totsku) %>% 
  select(c(3:4))


# 첫번째 그래프에 사용할 데이터 엑셀로 추출
# 추정치는 엑셀로 따로 안 뽑았습니다 그냥 그대로 복사해서 그래프에 붙여넣기
write_xlsx(list(sheet1 = meat.y.tot, sheet2 = meat.sku.cagr), 'graph1.xlsx')
