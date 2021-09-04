#### Customer analysis - project 1

library(tidyverse)
library(readxl)
library(knitr)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)
library(pastecs)
library(fpc)
library(magrittr)


### DATA
# # Loading
data <- read.csv("20200406_CSKH.csv")
df <- data

# CLeaning
df$BILL_DATE <- as.Date(df$BILL_DATE, format= "%Y-%m-%d")
table(df$ACCNT_GRP) # retail, check xem KH le, khach hang doanh nghiep 

# df2 <- df %>% filter(ACCNT_GRP == "Retail")

# Loc ra khach hang theo cong ty - CECL 
df$PLANT <- as.character(df$PLANT)
CECL <- df %>% 
  filter(str_detect(PLANT, "^4"))

# Remove negative value 
CECL <- CECL %>% filter(!NET_VAL_S_100 <= 100000 & !QUANT_B <= 0)

# Loai bo KH VL, khach hang noi bo
CECL <- CECL %>% filter(!str_detect(CECL$SOLD_TO, "^VL"))
CECL <- CECL %>% filter(!str_detect(CECL$SOLD_TO, "^E0"))
CECL <- CECL %>% filter(!str_detect(CECL$SOLD_TO, "^E0"))
CECL <- CECL %>% filter(BILL_TYPE != "Intercompany Billing")
CECL <- CECL %>% filter(BILL_TYPE_K != "ZVC") #Chi lay ZSO la ban hang, ZVC la voucher

table(CECL$BILL_TYPE_K)

# RFM, Ticket size trung binh/KH
dsRFM <- CECL %>% group_by(SOLD_TO) %>% summarise(recency = as.numeric(as.Date("2020-04-01") - max(as.Date(BILL_DATE))), frequency = n(), monetary = sum(as.numeric(NET_VAL_S_100)))

dsRFM <- dsRFM %>% mutate(ticketSize = monetary/frequency)

# Remove outlier = KH mua SL lon
dsRFM <- dsRFM %>% filter(!SOLD_TO %in% c("0101680800", "0101563814", "0101572263", "0101214910"))

# His
par(mfrow=c(2,2))

ggplot(dsRFM, aes(x=recency)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.2, fill="#FF6666")

ggplot(dsRFM, aes(x=frequency)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.2, fill="#FF6666")

ggplot(dsRFM, aes(x=monetary)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.2, fill="#FF6666")

ggplot(dsRFM, aes(x=ticketSize)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.2, fill="#FF6666")



# Basic statistics
options(digits = 2, scipen = 99)
stat.desc(select(dsRFM, -SOLD_TO))

quantile(dsRFM$recency) #quantile 
quants <- c(0,0.05,0.25,0.50,0.75,0.90,0.95,0.99,1)
apply( dsRFM[2:4] , 2 , quantile , probs = quants , na.rm = TRUE )


### K-MEANS SEGMENTATION
dsData <- dsRFM

## Normalize du lieu
row.names(dsData) <- dsRFM$SOLD_TO
dsData <- scale(dsData[,2:4])
summary(dsData)

# ## Tim k toi uu 
# # Cach 1: 
# library(factoextra)
# set.seed(123)
# fviz_nbclust(dsData, kmeans, method = "wss") + 
#   geom_vline(xintercept = 4, linetype = 2)

# Cach 2: Tinh tay 
tot_withinss <- c()
for (i in 1:10) {
  set.seed(123)
  u <- kmeans(dsData, i, nstart = 25)
  k <- u$tot.withinss
  tot_withinss <- c(tot_withinss, k)
}

mydf <- data_frame(TWSS = tot_withinss, N = 1:10)
mydf %>%
  ggplot(aes(N, TWSS)) +
  geom_line() +
  geom_point() +
  geom_point(data = mydf %>% filter(N == 4), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(title = "Number of Clusters v.s TWSS")

# Phân cụm với k = 4: 
set.seed(123)
km.res <- kmeans(dsData, 4, nstart = 25)
km.res

# Gan cluster vao data
dsRFM %<>% mutate(cluster = km.res$cluster)

# Số quan sát ở mỗi cụm: 
# Tính toán trung  bình các cụm (hay chính là các centroid): 
km.res$centers

# Số quan sát thuộc mỗi cụm: 
km.res$cluster %>% table()

# Vẽ phác thảo các quan sát thuộc bộ dữ liệu với 4 cụm: 
# fviz_cluster(km.res, data = dsData,
#              ellipse.type = "euclid", 
#              star.plot = TRUE,
#              repel = TRUE)

dsRFM$Name_Group <- ifelse(dsRFM$cluster == "1", "Treasure",
                     ifelse(dsRFM$cluster == "4", "High Potential",
                            ifelse(dsRFM$cluster == "2", "Potential","New")))

dsRFM$Churn <- ifelse(dsRFM$recency <= 90, "1-3 thang",ifelse(dsRFM$recency > 90 & dsRFM$recency <= 180, "3-6 thang", "9-12 thang"))
                                   
# Tinh mean cho moi column numeric
dsRFM$cluster <- as.character(dsRFM$cluster)
dsRFM %>% summarise_if(is.numeric, mean) 

# Phan loai theo Group (Contigency table)
h <- dsRFM %>% group_by(SOLD_TO, Name_Group, Churn) %>% summarise(frequency = n())
L <- dsRFM %>% group_by(Name_Group, Churn) %>% summarise(sum = sum(as.numeric(frequency)))

# Plot 1
ggplot(L, aes(x = Name_Group, y = sum, fill = Churn, label = sum)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + xlab("Cluster") + ylab("Số lượng KH") + scale_fill_brewer(palette="Paired")+
  theme_minimal()

# Plot 2
percentData <- h  %>% group_by(Name_Group) %>% count(Churn) %>%
    mutate(ratio=scales::percent(n/sum(n)))
ggplot(h,aes(x=factor(Name_Group),fill=factor(Churn)))+
    geom_bar(position="fill")+
    geom_text(data=percentData, aes(y=n,label=ratio), 
    position=position_fill(vjust=0.5)) + scale_fill_brewer(palette="Paired")+
  theme_minimal() + xlab("Cluster") + ylab("Số lượng KH") + scale_fill_brewer(palette="Paired")


## Phan loai san pham mua nhieu
Product <- merge(dsRFM, CECL, by = "SOLD_TO")
Product <- Product[!(!is.na(Product$MATL_GROUP_4) & Product$MATL_GROUP_4==""), ] 
# https://stackoverflow.com/questions/6437164/removing-empty-rows-of-a-data-file-in-r


library(ggpubr)
theme_set(theme_pubr())

Product_1 <- Product %>%
  group_by(MATL_GROUP_4) %>%
  summarise(counts = n()) %>% arrange(desc(counts))
Product_1

top_n(Product_1, n=5, counts) %>%
          ggplot(aes(x = reorder(MATL_GROUP_4, -counts), y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() +
  xlab("Dòng hàng") + ylab("Số lượng mua")


# Do thi nhung mon mua nhieu: 
Product_2 <- Product %>%
  group_by(Name_Group, MATL_GROUP_4) %>%
  summarise(counts = n()) %>% arrange(desc(counts))


Product_3 <- Product_2 %>% filter(Name_Group == "Treasure") 
top_n(Product_3, n=5, counts) %>%
          ggplot(aes(x = reorder(MATL_GROUP_4, -counts), y = counts)) +
  geom_bar(fill = "#3B1281", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() +
  xlab("") + ylab("Số lượng mua")
  
Product_4 <- Product_2 %>% filter(Name_Group == "High Potential") 
top_n(Product_4, n=5, counts) %>%
          ggplot(aes(x = reorder(MATL_GROUP_4, -counts), y = counts)) +
  geom_bar(fill = "#42E18B", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() +
  xlab("") + ylab("Số lượng mua")

Product_5 <- Product_2 %>% filter(Name_Group == "Potential") 
top_n(Product_5, n=5, counts) %>%
          ggplot(aes(x = reorder(MATL_GROUP_4, -counts), y = counts)) +
  geom_bar(fill = "#C53089", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() +
  xlab("") + ylab("Số lượng mua")

Product_6 <- Product_2 %>% filter(Name_Group == "New") 
top_n(Product_6, n=5, counts) %>%
          ggplot(aes(x = reorder(MATL_GROUP_4, -counts), y = counts)) +
  geom_bar(fill = "#F96522", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() +
  xlab("") + ylab("Số lượng mua")


####################
# SANKEY CHART GIỮA CÁC LẦN MUA - CHUAN BI DU LIEU 
####################
#### Nguoi ta da mua gi qua nhung lan khac nhau
WhatBuy <- Product %>% filter(frequency == 3)

WhatBuy_reshape_1 <- aggregate(MATL_GROUP_4 ~ SOLD_TO, FUN = paste, collapse = "|", data = WhatBuy)

### Count simple 
WhatBuy_reshape_2 <- WhatBuy_reshape_1 %>%
  group_by(MATL_GROUP_4) %>%
  summarise(count = n())

###  
WhatBuy_reshape_3 <- merge(x = WhatBuy_reshape_1, y = dsRFM[ , c("SOLD_TO", "Churn", "Name_Group")], by = "SOLD_TO", all.x=TRUE)

WhatBuy_reshape_4 <- WhatBuy_reshape_3 %>%
  group_by(MATL_GROUP_4) %>%
  summarise(count = n())

WhatBuy_reshape_5 <- WhatBuy_reshape_3 %>%
  group_by(Churn, MATL_GROUP_4) %>%
  summarise(count = n())

WhatBuy_reshape_6 <- WhatBuy_reshape_3 %>%
  group_by(Name_Group, Churn, MATL_GROUP_4) %>%
  summarise(count = n())


####################
# SANKEY CHART GIỮA CÁC LẦN MUA - Voi frequency = 3
####################
### Create the matrix 
a <- strsplit(WhatBuy_reshape_6$MATL_GROUP_4, "\\|") #cat chuoi 1 cot ra thanh nhieu cot 
n.obs <- sapply(a, length)
seq.max <- seq_len(max(n.obs))
a_mat <- t(sapply(a, "[", i = seq.max))
class(a_mat)


colnames(a_mat) <- c("L1","L2","L3") #name the column 
a3 <- a_mat %>% as_data_frame(a_mat) #Convert matrix to data frame 

a3$Name_Group <- WhatBuy_reshape_6$Name_Group
a5 <- merge(WhatBuy_reshape_6, a3, by="row.names")
a5$Row.names <- NULL
a5$Name_Group.x <- NULL
a5$Name_Group.y <- NULL
a5$MATL_GROUP_4 <- NULL

require(alluvial)
library(ggalluvial)
titanic_wide <- a5[1:20,] #Lay mau de ve cho de nhin 
str(titanic_wide)
titanic_wide$L1 <- as.factor(titanic_wide$L1)
titanic_wide$L2 <- as.factor(titanic_wide$L2)
titanic_wide$L3 <- as.factor(titanic_wide$L3)


ggplot(data = titanic_wide,
       aes(axis1 = L1, axis2 = L2, axis3 = L3, y = count)) + scale_x_discrete(limits = c("L1", "L2", "L3"), expand = c(.1, .05)) + geom_alluvium(aes(fill = Churn))+ geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) + theme_minimal()+ theme(legend.position = 'bottom')


####################
# DOANH THU THAY THOI GIUA CAC LAN MUA - CHANGE IN $ BUCKET  
####################
WhatBuy <- Product %>% filter(frequency == 3)

### Tao 1 cot moi va dat ten khoang doanh thu
WhatBuy$Revenue_group <- ifelse(WhatBuy$NET_VAL_S_100 <= 500000,"Duoi 500k", 
                                ifelse(WhatBuy$NET_VAL_S_100 >500000 & WhatBuy$NET_VAL_S_100 <= 2000000,"Tu 500k - 2tr",
                                       ifelse(WhatBuy$NET_VAL_S_100 > 2000000 & WhatBuy$NET_VAL_S_10<= 6000000, "Tu 2tr - 6tr", "Tren 6 tr")))

### 
WhatBuy_revenue_1 <- aggregate(Revenue_group ~ SOLD_TO, FUN = paste, collapse = "|", data = WhatBuy)

### Count simple 
WhatBuy_revenue_2 <- WhatBuy_revenue_1 %>%
  group_by(Revenue_group) %>%
  summarise(count = n())

###  
WhatBuy_revenue_3 <- merge(x = WhatBuy_revenue_1, y = dsRFM[ , c("SOLD_TO", "Churn", "Name_Group")], by = "SOLD_TO", all.x=TRUE)

WhatBuy_revenue_4 <- WhatBuy_revenue_3 %>%
  group_by(Revenue_group) %>%
  summarise(count = n())

WhatBuy_revenue_5 <- WhatBuy_revenue_3 %>%
  group_by(Churn, Revenue_group) %>%
  summarise(count = n())

WhatBuy_revenue_6 <- WhatBuy_revenue_3 %>%
  group_by(Name_Group, Churn, Revenue_group) %>%
  summarise(count = n())

####################
# SANKEY CHART GIỮA CÁC LẦN MUA - Voi frequency = 3
####################
### Create the matrix 
aa <- strsplit(WhatBuy_revenue_6$Revenue_group, "\\|") #cat chuoi 1 cot ra thanh nhieu cot 
n.obs <- sapply(aa, length)
seq.max <- seq_len(max(n.obs))
a_mat_revenue <- t(sapply(aa, "[", i = seq.max))
class(a_mat_revenue)


colnames(a_mat_revenue) <- c("L1","L2","L3") #name the column 
a33 <- a_mat_revenue %>% as_data_frame(a_mat_revenue) #Convert matrix to data frame 

a33$Name_Group <- WhatBuy_revenue_6$Name_Group
a55 <- merge(WhatBuy_revenue_6, a33, by="row.names")
a55$Row.names <- NULL
a55$Name_Group.x <- NULL
a55$Name_Group.y <- NULL
a55$MATL_GROUP_4 <- NULL

require(alluvial)
library(ggalluvial)
revenue_titanic_wide <- a55[1:30,] #Lay mau de ve cho de nhin 
str(revenue_titanic_wide)
revenue_titanic_wide$L1 <- as.factor(revenue_titanic_wide$L1)
revenue_titanic_wide$L2 <- as.factor(revenue_titanic_wide$L2)
revenue_titanic_wide$L3 <- as.factor(revenue_titanic_wide$L3)


ggplot(data = revenue_titanic_wide,
       aes(axis1 = L1, axis2 = L2, axis3 = L3, y = count)) + scale_x_discrete(limits = c("L1", "L2", "L3"), expand = c(.1, .05)) + geom_alluvium(aes(fill = Churn))+ geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) + theme_minimal()+ theme(legend.position = 'bottom')

