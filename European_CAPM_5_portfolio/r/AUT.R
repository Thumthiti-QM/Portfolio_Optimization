#########################################################################################
# Step 1 - Loading related data
#########################################################################################
install.packages(c("moments","RPostgres"))

# Related libraries
library(tidyverse); library(tidyr); library(zoo);
library(lubridate); library(readr); library(moments); 
library(RPostgres); library(scales); library(broom);
require(data.table)

portfolio <- "FF"

# calculate BE
AUT.BE <- read.csv("AUT_yly.csv")
AUT.BE <- AUT.BE %>%
  filter(!is.na(cequ))

AUT.BE[,5][is.na(AUT.BE[,5])] <- 0

AUT.BE <- AUT.BE %>%
  mutate(BE = cequ + dtax)

# create portfolio 
AUT.ret <- read.csv("AUT_mly.csv")

AUT.ret <- AUT.ret %>%
  select(-c(ret,mv))
 
AUT.ret <- AUT.ret %>%
  filter(ret_usd<= 890) %>%
  filter(!is.na(ret_usd)) 

 AUT.ret <- AUT.ret %>%
  mutate(YEAR = year(Date))

AUT.ret <- AUT.ret %>%
  left_join(AUT.BE, by=c("YEAR","Id"))

AUT.ret <- AUT.ret %>%
  select(-ï..country)

AUT.ret <- AUT.ret %>%
  mutate(BEME = BE/mv_usd)

AUT.ret <- AUT.ret %>%
  mutate(ym = as.yearmon(Date))

AUT_quantiles <- AUT.ret %>% 
  filter(month(ym) == 6) %>%
  group_by(ym) %>%
  summarize(BSmed = median(mv_usd, na.rm = TRUE),
            HNLq30 = quantile(BEME, probs = 0.30, na.rm = TRUE),
            HNLq70 = quantile(BEME, probs = 0.70, na.rm = TRUE)) %>%
  mutate(ym = as.Date(ym) %m+% months(1)) %>%
  mutate(ym = as.yearmon(ym)) 

AUT.ret.QUANTILE <- AUT.ret %>%
  left_join(AUT_quantiles, by=c("ym")) %>%
  arrange(ym) %>%
  mutate_at(vars(BSmed,HNLq30,HNLq70), funs(na.locf(.,na.rm = FALSE)))   

AUT.ret.QUANTILE <- AUT.ret.QUANTILE %>%
  mutate(BS = case_when( mv_usd < BSmed ~ "S",
                         mv_usd >= BSmed ~ "B"),
         HNL = case_when( BEME < HNLq30 ~ "L",
                          BEME >= HNLq30 & BEME < HNLq70 ~ "N",
                          BEME >= HNLq70 ~ "H"))  

AUT.ret.QUANTILE <- AUT.ret.QUANTILE %>%
  mutate(LABEL = paste0(BS,HNL))

AUT.ret.QUANTILE <- AUT.ret.QUANTILE %>%
  select(-c(BS,HNL))

AUT.ret.QUANTILE <- AUT.ret.QUANTILE %>%
  filter(!is.na(ret_usd))

AUT_mly_FF3 <- AUT.ret.QUANTILE %>%
  group_by(LABEL, ym) %>%
  summarize(ret_VW = weighted.mean(ret_usd, mv_usd, na.rm = TRUE),
            ret_EW = mean(ret_usd, na.rm = TRUE)) %>%
  ungroup() %>%
  melt(id.vars = c("ym", "LABEL")) %>%
  reshape(idvar= c("ym", "variable"),
          timevar = "LABEL",
          direction ="wide") %>%
  mutate(SMB = ((value.SH + value.SN + value.SL)/3) - ((value.BH + value.BN + value.BL)/3),
         HML = ((value.SH + value.BH)/2) - ((value.SL + value.BL)/2)) %>%
  select(ym,variable,SMB,HML)

AUT.retANALYZE <- AUT.ret %>%
  filter(!is.na(ret_usd)) %>%
  left_join(AUT_mly_FF3, by = "ym") %>%
  filter(variable == "ret_EW")

REG_MODEL <- lm("ret_usd ~ SMB + HML", data = AUT.retANALYZE)

#AUT.retANALYZE %>%
#  do(tidy(lm(ret_usd ~ SMB + HML, data = .))) %>%
#  kable(digits = 2) %>%
#  kable_styling(bootstrap_options = c( "hover", "condensed", "responsive"))

summary(REG_MODEL)

AUT.retANALYZE_remove_retBE <- AUT.retANALYZE %>% filter(!is.na(BE)) %>% filter(!is.na(ret_usd)) 

AUT.retANALYZE
names(AUT.retANALYZE)


AUT.retANALYZE_remove_retBE %>%
  summarize(unique_types = n_distinct(Id))


dim(AUT.retANALYZE)
dim(AUT.retANALYZE_remove_retBE)
view(AUT.retANALYZE_remove_retBE)


## find non active stock 
AUT.retANALYZE_remove_retBE %>% summarize(max(ym))


AUT.retANALYZE %>% select(Id) %>% unique() %>% count()
list_11 <- AUT.retANALYZE %>% filter(YEAR >= 2000& YEAR < 2018)  %>% filter(is.na(BE) | is.na(ret_usd)) %>% select(Id) %>% unique() 
#%>% count()

list_11
AUT.retANALYZE
df= AUT.retANALYZE %>% anti_join(list_11,by="Id")
df %>% filter(YEAR >= 2000 & YEAR < 2018) %>% count()
df3 <- df %>% filter(YEAR >= 2000& YEAR < 2018) %>% group_by(Id) %>% summarize(cnt = n())
view(df3)

AUT.retANALYZE2 <- AUT.retANALYZE %>% select(Id , Date , ret_usd , up , YEAR , ym , variable , SMB, HML)

AUT.retANALYZE %>% filter(YEAR > 2013) %>% filter(is.na(BE) | is.na(ret_usd))  %>% select(Id, BE, ret_usd)


Aut.retANALYZE_quarter = AUT.retANALYZE %>% mutate(mth = month(ym)) %>% mutate(quarter = case_when(
  .$mth <= 3 ~ 1,
  .$mth <= 6 ~ 2,
  .$mth <= 9 ~ 3,
  .$mth <= 12 ~ 4,
))

Aut.retANALYZE_quarter_agg <- Aut.retANALYZE_quarter %>% group_by(Id, YEAR , quarter) %>% summarize(ret_usd = last(ret_usd), mv_usd = last(mv_usd))


AUT.BE <- read.csv("AUT_yly.csv")
AUT.BE <- AUT.BE %>%
  filter(!is.na(cequ))

AUT.BE[,5][is.na(AUT.BE[,5])] <- 0

AUT.BE <- AUT.BE %>%
  mutate(BE = cequ + dtax) %>% mutate(OLD_YEAR = YEAR , YEAR = YEAR +1 )


Aut.joined <- AUT.retANALYZE2 %>% left_join(AUT.BE , by =c("YEAR", "Id") )


Aut.joined 
#672547


Aut.joined

list_11 <- Aut.joined  %>% filter(is.na(BE) | is.na(ret_usd) | abs(ret_usd) > 890 )   %>% select("Id") %>% unique()
list_11

df= Aut.joined %>% anti_join(list_11,by="Id")
df %>% filter(YEAR > 2006) %>% count()
grouped_data <- df %>% filter(YEAR > 2006) %>% group_by(Id) %>% summarize(cnt = n())
view(grouped_data)

df_ordered <- df %>% arrange(desc(ym))
df_ordered

df_ordered %>% filter(Id == 672520)  %>% group_by(YEAR) %>% summarize(cnt = n())

df_ordered %>% filter(Id == 672520) %>% filter(YEAR == 2017)



AUT.retANALYZE  %>% filter(Id == 672520) %>% filter(YEAR == 2017)



log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

df_ordered %>%
  spread(Id, value = ret_usd) %>%
  tk_xts()


############### done cleaning

## 1 mv





## 
library(dplyr)
mtcars %>% 
  mutate(category = case_when(
    .$cyl == 4 & .$disp < median(.$disp) ~ "4 cylinders, small displacement",
    .$cyl == 8 & .$disp > median(.$disp) ~ "8 cylinders, large displacement",
    TRUE ~ "other"
  )
  )



dec <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Dec 2017") %>% select(Id) %>% unique()
nov <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Nov 2017") %>% select(Id) %>% unique()
oct <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Oct 2017") %>% select(Id) %>% unique()
sep <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Sep 2017") %>% select(Id) %>% unique()
aug <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Aug 2017") %>% select(Id)%>% unique()
jul <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Jul 2017") %>% select(Id) %>% unique()
jun <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Jun 2017") %>% select(Id)%>% unique()
may <- AUT.retANALYZE_remove_retBE %>% filter(ym == "May 2017") %>% select(Id)%>% unique()
apr <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Apr 2017") %>% select(Id)%>% unique()
mar <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Mar 2017") %>% select(Id)%>% unique()
feb <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Feb 2017") %>% select(Id)%>% unique()
jan <- AUT.retANALYZE_remove_retBE %>% filter(ym == "Jan 2017") %>% select(Id)%>% unique()

dec %>% dim()
nov %>% dim()
oct %>% dim()
sep %>% dim()


# because 2018 data not full year


AUT.ret %>% filter(ym == "Dec 2017") %>% filter(!is.na(BE)) %>% filter(!is.na(ret_usd)) %>% 
  summarize(unique_stock = n_distinct(Id))


AUT.ret%>% 
  summarize(unique_stock = n_distinct(Id))

AUT.BE %>% 
  summarize(unique_stock = n_distinct(Id))


library(dplyr)
df= df1 %>% inner_join(df2,by="CustomerId")
df


############################ start again



AUT.BE <- read.csv("AUT_yly.csv")
AUT.BE <- AUT.BE %>%
  filter(!is.na(cequ))

AUT.BE[,5][is.na(AUT.BE[,5])] <- 0

AUT.BE <- AUT.BE %>%
  mutate(BE = cequ + dtax) %>% mutate(OLD_YEAR = YEAR , YEAR = YEAR +1 )

AUT.retANALYZE2 <- AUT.retANALYZE %>% select(Id , Date , ret_usd , up , mv_usd, YEAR , ym , variable , SMB, HML)

Aut.joined <- AUT.retANALYZE2 %>% left_join(AUT.BE , by =c("YEAR", "Id") )


list_11 <- Aut.joined %>% filter(OLD_YEAR >= 1999& YEAR < 2018)  %>% filter(is.na(BE) | is.na(ret_usd)) %>% select(Id) %>% unique() 


df= Aut.joined %>% anti_join(list_11,by="Id")
df %>% filter(OLD_YEAR >= 1999 & YEAR < 2018) %>% count()
df3 <- df %>% filter(OLD_YEAR >= 1999& YEAR < 2018) %>% group_by(Id) %>% summarize(cnt = n())
view(df3)
n_cnt = ((2017-2000)+1) *12
n_cnt
list_stock2 = df3 %>% filter(cnt == n_cnt) %>% select(Id) %>% unique()


df_joined <- df %>% inner_join(list_stock2 , by =c("Id") ) %>% filter(OLD_YEAR >= 1999& YEAR < 2018)

##  number = 216

df_joined

log_ret_tidy <- df_joined %>% mutate(ret_usd = ret_usd/100) %>%
  select(Id , ym,ret_usd) 

log_ret_xts <- log_ret_tidy %>%
  spread(Id, value = ret_usd) %>%
  tk_xts()


mean_ret <- colMeans(log_ret_xts)
mean_ret

cov_mat <- cov(log_ret_xts) * 12
cov_mat
print(round(cov_mat,4))


wts = runif(n = length(mean_ret ))
sum(wts)
wts <- wts/sum(wts)
wts
sum(wts)


port_returns <- (sum(wts * mean_ret) + 1)^12 - 1
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

print(port_returns)
print(port_risk)

# eq weight
weight = 1/length(mean_ret )
wts = rep(weight, length(mean_ret )) %>% as.matrix()
length(mean_ret )


mat = log_ret_xts %>% as.matrix() 




retport_eqw = mat %*% wts


df_joined

## MV weight
log_ret_tidy_mv <- df_joined %>% mutate(ret_usd = ret_usd/100) %>%
  select(Id , ym, mv_usd) 

log_ret_xts_mv <- log_ret_tidy_mv %>%
  spread(Id, value = mv_usd) %>%
  tk_xts()  
  

dfsum <- log_ret_xts_mv %>% as.data.frame() %>% rowSums() %>% as.data.frame()

names(dfsum) <- "mv"

log_ret_xts_mv %>% as.data.frame()

inv_mv_df = dfsum %>% mutate(inv_mv = 1/mv) %>% select(inv_mv)



## st


log_ret_xts_mv 

log_ret_xts_mv
log_ret_xts_mv %>% dim()
inv_mv_mat %>% dim()

log_ret_xts_mv* (dfsum**-1)

mv_mat = log_ret_xts_mv * inv_mv_mat
mv_mat %>% rowSums()
mv_mat %>% colSums()
############ HERE

inv_mv_mat

inv_mv_mat = matrix( nrow = nrow(inv_mv_df) , rep( t( inv_mv_df ) , length(mean_ret)  ) , byrow = TRUE )
inv_mv_mat %>% dim()




tot_mat  <- mat * inv_mv_mat
inv_mv_mat%>% rowSums()
tot_mat %>% rowSums()




uninstall.packages("dplyr")

install.packages("tidyquant")
install.packages("timetk")
install.packages("rlang")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("installr")

detach(package:tidyverse, unload = TRUE)
packageDescription("dplyr", fields = "Depends")
updateR()

library(installr)
library(tidyverse)
library(rlang)
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(dplyr)


log_ret_xts <- df_joined %>%
  spread(symbol, value = ret_usd) %>%
  tk_xts()

names(df_joined %>% select("Id","")



df_joined %>%
  spread(Id, value = ret_usd) %>%
  tk_xts()















summary(REG_MODEL)

AUT.retANALYZE_remove_retBE <- AUT.retANALYZE %>% filter(!is.na(BE)) %>% filter(!is.na(ret_usd)) 

AUT.retANALYZE
names(AUT.retANALYZE)


AUT.retANALYZE_remove_retBE %>%
  summarize(unique_types = n_distinct(Id))


dim(AUT.retANALYZE)
dim(AUT.retANALYZE_remove_retBE)
view(AUT.retANALYZE_remove_retBE)


## find non active stock 
AUT.retANALYZE_remove_retBE %>% summarize(max(ym))


AUT.retANALYZE %>% select(Id) %>% unique() %>% count()
list_11 <- AUT.retANALYZE %>% filter(YEAR >= 2000& YEAR < 2018)  %>% filter(is.na(BE) | is.na(ret_usd)) %>% select(Id) %>% unique() 
#%>% count()

list_11
AUT.retANALYZE
df= AUT.retANALYZE %>% anti_join(list_11,by="Id")
df %>% filter(YEAR >= 2000 & YEAR < 2018) %>% count()
df3 <- df %>% filter(YEAR >= 2000& YEAR < 2018) %>% group_by(Id) %>% summarize(cnt = n())
view(df3)

AUT.retANALYZE2 <- AUT.retANALYZE %>% select(Id , Date , ret_usd , up , YEAR , ym , variable , SMB, HML)

AUT.retANALYZE %>% filter(YEAR > 2013) %>% filter(is.na(BE) | is.na(ret_usd))  %>% select(Id, BE, ret_usd)


Aut.retANALYZE_quarter = AUT.retANALYZE %>% mutate(mth = month(ym)) %>% mutate(quarter = case_when(
  .$mth <= 3 ~ 1,
  .$mth <= 6 ~ 2,
  .$mth <= 9 ~ 3,
  .$mth <= 12 ~ 4,
))

Aut.retANALYZE_quarter_agg <- Aut.retANALYZE_quarter %>% group_by(Id, YEAR , quarter) %>% summarize(ret_usd = last(ret_usd), mv_usd = last(mv_usd))


AUT.BE <- read.csv("AUT_yly.csv")
AUT.BE <- AUT.BE %>%
  filter(!is.na(cequ))

AUT.BE[,5][is.na(AUT.BE[,5])] <- 0

AUT.BE <- AUT.BE %>%
  mutate(BE = cequ + dtax) %>% mutate(OLD_YEAR = YEAR , YEAR = YEAR +1 )


Aut.joined <- AUT.retANALYZE2 %>% left_join(AUT.BE , by =c("YEAR", "Id") )


Aut.joined 
#672547


Aut.joined

list_11 <- Aut.joined  %>% filter(is.na(BE) | is.na(ret_usd) | abs(ret_usd) > 890 )   %>% select("Id") %>% unique()
list_11

df= Aut.joined %>% anti_join(list_11,by="Id")
df %>% filter(YEAR > 2006) %>% count()
grouped_data <- df %>% filter(YEAR > 2006) %>% group_by(Id) %>% summarize(cnt = n())
view(grouped_data)

df_ordered <- df %>% arrange(desc(ym))
df_ordered

df_ordered %>% filter(Id == 672520)  %>% group_by(YEAR) %>% summarize(cnt = n())

df_ordered %>% filter(Id == 672520) %>% filter(YEAR == 2017)



AUT.retANALYZE  %>% filter(Id == 672520) %>% filter(YEAR == 2017)



log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

df_ordered %>%
  spread(Id, value = ret_usd) %>%
  tk_xts()
