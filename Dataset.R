suppressMessages({
library(readr)
library(dplyr)
library(caret)
})
setwd("C:/Users/seol/Desktop/Analysis/DACON")
##### Data set Making ######
Auction_master_train <- read_csv("Auction_master_kr/Auction_master_train.csv", 
                                 col_types = cols(Appraisal_company = col_skip(),
                                                  Creditor = col_skip(),
                                                  Close_date = col_skip(), Close_result = col_skip(), 
                                                  Final_result = col_skip(), First_auction_date = col_skip(), 
                                                  addr_bunji1 = col_skip(), addr_bunji2 = col_skip(), 
                                                  addr_dong = col_skip(), addr_etc = col_skip(), 
                                                  addr_li = col_skip(), addr_san = col_skip(), 
                                                  point.x = col_skip(), point.y = col_skip(), 
                                                  road_bunji1 = col_skip(), road_bunji2 = col_skip(), 
                                                  road_name = col_skip(),
                                                  Total_building_area=col_skip(),Total_land_real_area=col_skip(),
                                                  Specific= col_skip()))

Auction_master_test <- read_csv("Auction_master_kr/Auction_master_test.csv", 
                                col_types = cols(Appraisal_company = col_skip(),
                                                 Creditor = col_skip(),
                                                 Close_date = col_skip(), Close_result = col_skip(),
                                                 Final_result = col_skip(), First_auction_date = col_skip(), 
                                                 addr_bunji1 = col_skip(), addr_bunji2 = col_skip(), 
                                                 addr_dong = col_skip(), addr_etc = col_skip(), 
                                                 addr_li = col_skip(), addr_san = col_skip(), 
                                                 point.x = col_skip(), point.y = col_skip(), 
                                                 road_bunji1 = col_skip(), road_bunji2 = col_skip(), 
                                                 road_name = col_skip(),
                                                 Total_building_area=col_skip(),Total_land_real_area=col_skip(),
                                                 Specific= col_skip()))



Auction_master_train %>% ggplot(aes(y=Hammer_price))+
  geom_boxplot()+
  scale_y_continuous(labels=comma)

#install.packages('dplyr')

#outlier removal
train<-Auction_master_train %>% filter(Hammer_price<5000000000) 
# EDA
train %>% ggplot(aes(group=Auction_miscarriage_count,y=Hammer_price))+
  geom_boxplot()+
  scale_y_continuous(labels=comma)

train %>% 
  group_by(addr_do,addr_si) %>% 
  summarize(mean_hammer= mean(Hammer_price)) 

train %>% 
  group_by(Auction_miscarriage_count) %>% 
  tally()
train %>% 
  group_by(Auction_miscarriage_count) %>% 
  dplyr::summarize(mean_hammer=mean(Hammer_price))

train %>% 
  group_by(addr_si) %>% 
  tally() %>% View()



# outlier 확인
quantile(train$Hammer_price)
train %>% ggplot(aes(y=Hammer_price))+
  geom_boxplot()+
  scale_y_continuous(labels=comma)+
  geom_hline(yintercept = quantile(train$Hammer_price,probs = 0.90),color='red')
# outlier 제거 --상위 10%절삭
train_rm_outline <- train %>% filter(Hammer_price <quantile(train$Hammer_price,probs = 0.90))
train_rm_outline %>% ggplot(aes(y=Hammer_price))+
  geom_boxplot()+
  scale_y_continuous(labels=comma)

train_add<-train_rm_outline %>% 
  group_by(addr_si) %>% 
  summarise(mean_hammer=mean(Hammer_price)) %>% arrange(desc(mean_hammer))

train_add %>% ggplot(aes(y=mean_hammer,x=reorder(addr_si, -mean_hammer, fun= mean, desc=T)))+
  geom_point()+
  scale_y_continuous(labels=comma)+
  theme(axis.text.x = element_text(angle=-90))

train_2 <- train_rm_outline %>% select(-point.x,-point.y, -addr_do,-Auction_key)

df_train <- Auction_master_train

## 이상치제거 및 변환 ##
df_train %>% summary    # Current_floor ==0 , Total_land_gross_area ==3511936
    # [land_auction_area=0 -> 건물만 경매, Claim_price=0 -> 형식적 경매]
#df_train %>% arrange(desc(Total_land_gross_area)) %>% View()
df_train$Total_land_gross_area[df_train$Total_land_gross_area==3511936]<-351193.6
df_train$Total_land_gross_area[df_train$Total_land_gross_area>=825056] <- 82056.4
df_train$Current_floor[df_train$Current_floor==0][1]<-13
df_train$Current_floor[df_train$Current_floor==0][1]<-14
#df_train %>% View

df_test <- Auction_master_test
df_test %>% summary     # Current_floor = -1
df_test[df_test$Current_floor==0,]
df_test$Current_floor[df_test$Current_floor==0][1]<-3
df_test$Current_floor[df_test$Current_floor==0][1]<-6
df_test$Current_floor[df_test$Current_floor==0][1]<-3
df_test$Current_floor[df_test$Current_floor==0][1]<-1
df_test[df_test$Current_floor==-1,]
df_test$Current_floor[df_test$Current_floor==-1][1]<-1 # -값은 임의로 변경 -> scaling 오류 피하기 
n<-nrow(df_train)
nrow(df_test)
data_full <- df_train %>% rbind(df_test)

## date 처리, 아파트 경과년도 생성, 대출금리  ## 
library(lubridate)
library(zoo)
a<-data_full %>% select(ends_with('date'),-c(Final_auction_date)) %>% colnames()
data_full[,a] <- data_full[,a] %>% apply(2,year)
#View(data_full)
data_full$Final_auction_date<-data_full$Final_auction_date %>% zoo::as.yearmon(format="%Y-%m-%d UTC") %>% format(format='%Y-%m')

i_rate<-read_csv('i_rate.csv')  #대출금리 자료 -http://housta.khug.or.kr/khhi/web/hi/fi/hifi050008.jsp
i_rate %>% t() %>% as.data.frame %>% colnames
i_rate<-i_rate %>% t() %>% as.data.frame %>% select(V3)
i_rate$Final_auction_date<- rownames(i_rate)
i_rate %>% head()
i_rate<- i_rate[2:nrow(i_rate),]
rownames(i_rate)<-c()
colnames(i_rate)[1]<-'interest_rate'
i_rate %>% glimpse()
i_rate$Final_auction_date <- i_rate$Final_auction_date %>% as.yearmon(format='%Y.%m') %>% format('%Y-%m')
head(i_rate)
data_full<-data_full %>% merge(i_rate,by='Final_auction_date')
data_full <- data_full %>% select(-c(Final_auction_date)) 
data_full$interest_rate <- data_full$interest_rate %>% as.character %>% as.numeric
data_full %>% glimpse()
#install.packages('naniar')
library(naniar)
data_full<-replace_with_na(data_full,list(Preserve_regist_date=1111)) 

data_full$Preserve_regist_date<-tidyr::replace_na(data_full$Preserve_regist_date,round(mean(data_full$Preserve_regist_date,na.rm = T),digits=0)) 
#data_full %>% View

data_full <-data_full %>% mutate(date_diff=Appraisal_date-Preserve_regist_date)
data_full <- data_full %>% select(-c(Appraisal_date,Preserve_regist_date))
data_full$date_diff[data_full$date_diff %in% -1]<-0
class(data_full$date_diff)
data_full$date_diff<- as.vector(data_full$date_diff)


#View(data_full)

#################
regist <- read_csv("Auction_master_kr/Auction_regist.csv")
rent <- read_csv("Auction_master_kr/Auction_rent.csv")
View(rent)
View(regist)

rent_in <- data.frame(unique(rent[,"Auctiuon_key"]))
rent_in$rent <- 'Y'
colnames(rent_in)[1]<-'Auction_key'
data_full<-data_full %>% merge(rent_in, by='Auction_key',all=TRUE)

data_full$rent[data_full$rent %in% NA ] <- 'N'
colnames(data_full)
#colnames(data_full)[21] <- 'rent'

#####regist   
regist_n <- regist[,c("Auction_key","Regist_price")]
regist_n<-regist_n %>% filter(Regist_price!=0)
#regist_1 <- regist_n %>% group_by(Auction_key) %>% filter(row_number()==n()) #row_number()
regist_n %>% group_by(Auction_key) %>% summarise(regist_sum=sum(Regist_price)) %>% View

regist %>% select(Regist_class) %>% unique()

regist[,"Regist_class"] %>% table()
#regist %>% View()

regist_ga<- regist[regist$Regist_class=='가등기',] %>% filter(!is.na(Auction_key)) %>% select(Auction_key)
regist[regist$Regist_class=='가처분',]

#################
data_full <- data_full %>% arrange(desc(Hammer_price))
#data_full %>% View
data_full %>% glimpse
data_r <- data_full %>% mutate(Claim_price=Claim_price/Total_building_auction_area,
                               Total_appraisal_price=Total_appraisal_price/Total_building_auction_area,
                               Minimum_sales_price=Minimum_sales_price/Total_building_auction_area,
                               Hammer_price=Hammer_price/Total_building_auction_area) %>% 
  mutate(Current_floor=as.double(Current_floor),
         Total_floor=as.double(Total_floor)) %>% select(-c(Total_building_auction_area,Auction_key))
data_r <- data_r %>% mutate_if(is.character,as.factor) %>% mutate_if(is.integer,as.factor)


minmax <- function(x){
  (x-min(x)) / (max(x)-min(x))
}

##### data Scailing , dummy ########
b<-data_r %>% select_if(is.numeric) %>% colnames()
data_r_num<- data_r[,b]
#data_r[,b] <- data_r[,b] %>% apply(2,minmax) %>% as.data.frame()
library(GGally)
library(fBasics)
data_r[,b] %>% ggpairs()
data_r[,b] %>% apply(2,skewness)
data_r[,b] %>% View

### preprocess ### - Normalizing
# Normalizing - Countinous variables
d_price<-data_r %>% select(ends_with('price')) %>% colnames()
d_area<- data_r %>% select(ends_with('area')) %>% colnames()
data_r[,d_price]<-data_r[,d_price] %>% apply(2,function(x){log(x+1)}) %>% as.data.frame()
data_r[,d_area]<- data_r[,d_area] %>% apply(2,function(x){log(x+1)}) %>% as.data.frame()
data_r_num_log <- data_r_num %>% apply(2,function(x){log(x+1)}) %>% as.data.frame()
data_r[,b] %>% ggpairs()
data_r[,b] %>% View()

## Min max scaling  

data_r[,b] <- data_r[,b] %>% apply(2,minmax) %>% as.data.frame() # Scaling
data_r[,b] %>% sapply(range) # Check


data_r %>% View()
dummies <- dummyVars(~.,data_r)
processed_data <- predict(dummies,data_r)
#write.csv(processed_data,'processed_data.csv')

colnames(processed_data)
processed_data %>% View

train_set<-processed_data[1:n,] %>% as.data.frame()
test_set<-processed_data[(n+1):nrow(processed_data),] %>% as.data.frame()
nrow(train_set)
nrow(test_set)

View(data_full)


######### making partition ##########
inTrain <- createDataPartition(y=train_set$Hammer_price,p=0.75,list=FALSE)
training <- train_set[inTrain,]
testing <- train_set[-inTrain,]
testing <- testing %>% select(-c(Hammer_price))   # Cross validation


###### Model Making & Fitting ########            ## Dataset 변화에 따른 RMSE값 변화 


### NNET #####
library(neuralnet)
n <- colnames(train_set)
n
f <- as.formula(paste("Hammer_price ~", paste(n[!n %in% "Hammer_price"], collapse = " + ")))
f

nn <- neuralnet(
  f , threshold = 0.01,algorithm = "rprop+",
  learningrate.factor =
    list(minus = 0.5, plus = 1.2),
  data=train_set, hidden=c(74,37,18,10), err.fct="sse",
  linear.output=TRUE)


test_set2<-test_set %>% select(-c(Hammer_price))
nn_predict <- compute(nn,test_set2)
nn_predict_rescale <-nn_predict$net.result*(max(data_r_num_log$Hammer_price)-min(data_r_num_log$Hammer_price))+min(data_r_num_log$Hammer_price)
submit <- exp(nn_predict_rescale)-1
submit_final<- submit * Auction_master_test$Total_building_auction_area 
write.csv(submit_final,'NN_new_dataset.csv')




#################
rf.caret <- train(f, data = training,
                  method='rf', # random forest 옵션
                  trControl=my_control)
library(randomForest)
rf <- randomForest(f, 
                   data = training, 
                   ntree = 10000, # Tree 개수
                   importance = TRUE,
                   type='regression')

varImpPlot(rf, type=1, n.var=10) 

###### NNET ############
fitGrid <- expand.grid(.size = c(3,5), .decay = c(0.1))
library(pROC)
fitControl <- trainControl(method = "cv", # Cross validation 으로 찾도록 지정
                           number = 10)
model.ct.nn <- train(Hammer_price ~ .,
                     data = train_set,
                     method = 'nnet',
                     maxit = 1000,
                     linout = TRUE,
                     trControl = fitControl,
                     tuneGrid = fitGrid,
                     metric = "RMSE", 
                     allowParallel = TRUE)
preds.ct.nn.f <- predict(model.ct.nn, newdata=test_set)

preds.ct.nn.f.l <- preds.ct.nn.f*(max(data_r_num_log$Hammer_price)-min(data_r_num_log$Hammer_price))+min(data_r_num_log$Hammer_price)
preds.ct.nn.f.l<-preds.ct.nn.f.l*df_test$Total_building_auction_area


########### Model Stacking, Ensemble #############
#install.packages('pROC')
library(pROC)
#install.packages('mlbench')
library(mlbench)
#install.packages('caretEnsemble')
library(caretEnsemble)
library(rpart)

View(train_set)
set.seed(2018)
inTrain <- createDataPartition(y=train_set$Hammer_price,p=0.75,list=FALSE)
training <- train_set[inTrain,]
testing <- train_set[-inTrain,]

my_control <- trainControl(
  method='cv',
  number=10,)
model_list <- caretList(
  Hammer_price~., data=training,
  trControl=my_control,
  metric = "RMSE",
  methodList=c("glm", "rpart",'avNNet')
)
p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)

modelCor(resamples(model_list))

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="RMSE",
  trControl=trainControl(
    number=3,
  ))
summary(greedy_ensemble)

glm_ensemble <- caretStack(
  model_list,
  method="nnet",
  metric="RMSE",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
  )
)
model_preds2 <- predict(glm_ensemble, newdata=testing)
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
install.packages('caTools')
library(caTools)
colAUC(model_preds2, testing$Hammer_price)

test_result <- predict(glm_ensemble, newdata = test_set)
nn_fit <- unlist(nn$net.result)*(max(df_train_num_log$Hammer_price)-min(df_train_num_log$Hammer_price))+min(df_train_num_log$Hammer_price)
nn_fit <- exp(nn_fit)-1
test_result<-test_result*(max(data_r_num_log$Hammer_price)-min(data_r_num_log$Hammer_price))+min(data_r_num_log$Hammer_price)
test_restult <- exp(test_result)-1

Final_result<-test_restult*df_test$Total_building_auction_area

write.csv(Final_result,'model_stacking.csv')
