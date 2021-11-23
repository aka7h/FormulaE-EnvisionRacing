library(tidyverse)
library(skimr)
library(lubridate)
library(Metrics)
library(catboost)
library(h2o)
train <- read_csv('train.csv')
tr_weather <- read_csv('train_weather.csv')
test <- read_csv('test.csv')
te_weather <- read_csv('test_weather.csv')
sample_sub <- read_csv("submission.csv")

all_pred = tibble(predict = numeric(),RR=character())
add_pred <- function(df){
  all_pred <- bind_rows(all_pred,df)
  return(all_pred)
}

TEAM_NAME <- tibble(
  NUMBER = c(4,37,5,17,
             6,7,8,88,
             10,20,11,33,
             13,25,22,23,
             27,28,29,94,
             36,99,48,71),
  TEAM = c("EVR","EVR","MEQ","MEQ",
           "DRA","DRA","NIO","NIO",
           "JR","JR","AUDI","AUDI",
           "DS","DS","NIS","NIS",
           "BMW","BMW","MAH","MAH",
           "TAG","TAG","ROK","ROK"))


train <- train %>% select(-TEAM) %>% left_join(.,TEAM_NAME,by="NUMBER")
test <- test %>% select(-TEAM) %>% left_join(.,TEAM_NAME,by="NUMBER")

te_weather <- te_weather %>% mutate(RAIN = as.character(RAIN)) %>%
  rename(c("EVENT"="EVENTS"))


weather <- bind_rows(tr_weather,te_weather)


event <- weather %>% 
  separate(TIME_UTC_STR,
           c("TIME_UTC_DATE","TIME_UTC_TIME"),
           sep=" ",remove=FALSE) %>%
  mutate(TIME_UTC_DATE = dmy(TIME_UTC_DATE),
         TIME_UTC_TIME = hm(TIME_UTC_TIME),
         TIME_UTC_STR = dmy_hm(TIME_UTC_STR)) %>%
  group_by(EVENT,LOCATION,TIME_UTC_DATE) %>%
  summarise(MIN_TIM = min(TIME_UTC_SECONDS),MAX_TIM = max(TIME_UTC_SECONDS)) %>%
  mutate(SESSION_TIME = (MAX_TIM-MIN_TIM),
         RACES = n()) %>%
  arrange(EVENT,LOCATION,TIME_UTC_DATE) %>%
  mutate(ROW_N=row_number())


w_pressure <- weather %>% distinct(PRESSURE) %>%
  mutate(PR =floor(PRESSURE)) %>%
  mutate(PRESSURE_2 = case_when(PR>770000&PR<777000~PR/1000,
                                PR>100000&PR<120000~PR/100,
                                PR>10000&PR<11000~PR/10,
                                PR>77000&PR<777000~PR/100,
                                TRUE~PR))


weather_clean <- weather %>%
  mutate(WIND_SPEED = as.numeric(substr(str_replace(WIND_SPEED,",","."),1,4)),
         RAIN=str_replace(RAIN,",","."),
         AIR_TEMP = as.integer(substr(as.character(round(AIR_TEMP,0)),1,2)),
         TRACK_TEMP = as.numeric(substr(as.character(round(TRACK_TEMP,0)),1,2)),
         HUMIDITY = as.numeric(substr(as.character(round(HUMIDITY,0)),1,2))
  ) %>% 
  left_join(.,select(w_pressure,PRESSURE,PRESSURE_2),by="PRESSURE") %>%
  select(-PRESSURE) %>%
  mutate(TIME_UTC_STR = dmy_hm(TIME_UTC_STR))


w_index2 <- weather %>%
  ungroup() %>%
  separate(TIME_UTC_STR,
           c("TIME_UTC_DATE","TIME_UTC_TIME"),
           sep=" ",remove=FALSE) %>%
  mutate(TIME_UTC_DATE = dmy(TIME_UTC_DATE),
         TIME_UTC_TIME = hm(TIME_UTC_TIME),
         TIME_UTC_STR = dmy_hm(TIME_UTC_STR)) %>%
  select(TIME_UTC_STR,EVENT,LOCATION,TIME_UTC_DATE) %>% 
  group_by(EVENT,LOCATION,TIME_UTC_DATE) %>%
  summarise(MIN_DT = min(TIME_UTC_STR)) %>%
  arrange(EVENT,LOCATION) %>%
  mutate(ROW_N=row_number()) %>%
  nest(MIN_DT) %>%
  mutate(TIME_UTC_STR = map(data,
                            ~seq.POSIXt(from = .x$MIN_DT,
                                        by="min",
                                        length.out = 60))) %>%
  unnest(TIME_UTC_STR) %>%
  mutate(DATE = date(TIME_UTC_STR)) %>%
  group_by(EVENT,LOCATION,DATE) %>%
  mutate(T_IDX = row_number()) 




tr_wd <- weather %>%
  ungroup() %>%
  separate(TIME_UTC_STR,
           c("TIME_UTC_DATE","TIME_UTC_TIME"),
           sep=" ",remove=FALSE) %>%
  mutate(TIME_UTC_DATE = dmy(TIME_UTC_DATE),
         TIME_UTC_TIME = hm(TIME_UTC_TIME),
         TIME_UTC_STR = dmy_hm(TIME_UTC_STR)) %>%
  #filter(TIME_UTC_DATE!="2021-08-13") %>%
  group_by(EVENT,LOCATION,TIME_UTC_DATE) %>%
  summarise(MIN_TIM = min(TIME_UTC_SECONDS)) %>%
  mutate(RACES = n()) %>%
  arrange(EVENT,LOCATION) %>%
  mutate(ROW_N=row_number()) %>%
  select(EVENT,LOCATION,TIME_UTC_DATE,ROW_N)




w_final <- w_index2 %>% 
  left_join(.,weather_clean,by=c("EVENT","LOCATION","TIME_UTC_STR")) %>%
  group_by(EVENT,LOCATION) %>%
  mutate_at(vars(AIR_TEMP,TRACK_TEMP,HUMIDITY,PRESSURE_2),
            ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% #mean imputation
  mutate_at(vars(TIME_UTC_SECONDS),
            ~ifelse(is.na(.x), as.numeric(TIME_UTC_STR), .x)) %>% 
  fill(WIND_SPEED,WIND_DIRECTION,RAIN,PRESSURE_2,T_IDX,.direction="down") %>%
  select(-data) %>% mutate(T_SEC = T_IDX*60) 



train <- train %>% 
  ungroup() %>%
  group_by(EVENT,LOCATION,NUMBER,LAP_NUMBER) %>%
  mutate(ROW_N = row_number())

test <- test %>% 
  ungroup() %>%
  group_by(EVENT,LOCATION,NUMBER,LAP_NUMBER) %>%
  mutate(ROW_N = row_number())


tr_sector <- train %>%
  select(EVENT,LOCATION,NUMBER,LAP_NUMBER,LAP_TIME,S1_LARGE,S2_LARGE,
         S3_LARGE,PIT_TIME,CROSSING_FINISH_LINE_IN_PIT,ROW_N) %>%
  group_by(EVENT,LOCATION,NUMBER) %>%
  mutate(TOTAL_LAPS = n()) %>% ungroup() %>%
  gather(.,"SESSION","TIME",6:9) %>%
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER,SESSION) %>%
  # here the pit time come times combines added to S3 of previous lap or S1 lap where the pit time is generated. 
  # majority we see that the pit time is added to S1, very few corner cases we see in S3
  group_by(EVENT,LOCATION,NUMBER) %>%
  separate(TIME,c("MIN","SEC"),sep=":",remove=FALSE,extra="drop") %>%
  mutate(TOTAL=(as.integer(MIN)*60)+as.integer(SEC)) %>%
  mutate(PREV_SESS = lag(SESSION),NXT_SESS = lead(SESSION), 
         PREV_TM = lag(TOTAL), NXT_TM = lead(TOTAL)) %>% 
  mutate(PREV_FL = lag(CROSSING_FINISH_LINE_IN_PIT,4),
         NXT_FL = lead(CROSSING_FINISH_LINE_IN_PIT,4)) %>%
  replace_na(list(MIN=0,SEC=0,TOTAL=0,PREV_TM=0,NXT_TM=0)) %>% 
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER,SESSION) %>%
  group_by(EVENT,LOCATION,NUMBER) %>%
  mutate(ACTUAL_TIME = case_when(LAP_NUMBER==1&SESSION=="S1_LARGE"&PREV_SESS=="PIT_TIME"~TOTAL-PREV_TM,
                                 CROSSING_FINISH_LINE_IN_PIT=="B"&SESSION=="S3_LARGE"&(TOTAL-NXT_TM)>0~TOTAL-NXT_TM,
                                 CROSSING_FINISH_LINE_IN_PIT=="B"&SESSION=="S3_LARGE"&(TOTAL-NXT_TM)<0~TOTAL,
                                 LAP_NUMBER!=TOTAL_LAPS&SESSION=="S1_LARGE"&PREV_SESS=="PIT_TIME"&(TOTAL-PREV_TM)<0~TOTAL,
                                 LAP_NUMBER!=TOTAL_LAPS&SESSION=="S1_LARGE"&PREV_SESS=="PIT_TIME"&(TOTAL-PREV_TM)>0~TOTAL-PREV_TM,
                                 LAP_NUMBER==TOTAL_LAPS~TOTAL,
                                 TRUE~TOTAL)) %>%
  mutate(CUML_TM = cumsum(ACTUAL_TIME)) %>%
  mutate(CUML_TM_MIN = ceiling(CUML_TM/60)) %>%
  filter(CUML_TM_MIN<=60)


te_sector <- test %>%
  select(EVENT,LOCATION,NUMBER,LAP_NUMBER,LAP_TIME,S1_LARGE,S2_LARGE,
         S3_LARGE,PIT_TIME,CROSSING_FINISH_LINE_IN_PIT,ROW_N) %>%
  group_by(EVENT,LOCATION,NUMBER) %>%
  mutate(TOTAL_LAPS = n()) %>% ungroup() %>%
  gather(.,"SESSION","TIME",6:9) %>%
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER,SESSION) %>%
  # here the pit time come times combines added to S3 of previous lap or S1 lap where the pit time is generated. 
  # majority we see that the pit time is added to S1, very few corner cases we see in S3
  group_by(EVENT,LOCATION,NUMBER) %>%
  separate(TIME,c("MIN","SEC"),sep=":",remove=FALSE,extra="drop") %>%
  mutate(TOTAL=(as.integer(MIN)*60)+as.integer(SEC)) %>%
  mutate(PREV_SESS = lag(SESSION),NXT_SESS = lead(SESSION), 
         PREV_TM = lag(TOTAL), NXT_TM = lead(TOTAL)) %>% 
  mutate(PREV_FL = lag(CROSSING_FINISH_LINE_IN_PIT,4),
         NXT_FL = lead(CROSSING_FINISH_LINE_IN_PIT,4)) %>%
  replace_na(list(MIN=0,SEC=0,TOTAL=0,PREV_TM=0,NXT_TM=0)) %>% 
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER,SESSION) %>%
  group_by(EVENT,LOCATION,NUMBER) %>%
  mutate(ACTUAL_TIME = case_when(LAP_NUMBER==1&SESSION=="S1_LARGE"&PREV_SESS=="PIT_TIME"~TOTAL-PREV_TM,
                                 CROSSING_FINISH_LINE_IN_PIT=="B"&SESSION=="S3_LARGE"&(TOTAL-NXT_TM)>0~TOTAL-NXT_TM,
                                 CROSSING_FINISH_LINE_IN_PIT=="B"&SESSION=="S3_LARGE"&(TOTAL-NXT_TM)<0~TOTAL,
                                 LAP_NUMBER!=TOTAL_LAPS&SESSION=="S1_LARGE"&PREV_SESS=="PIT_TIME"&(TOTAL-PREV_TM)<0~TOTAL,
                                 LAP_NUMBER!=TOTAL_LAPS&SESSION=="S1_LARGE"&PREV_SESS=="PIT_TIME"&(TOTAL-PREV_TM)>0~TOTAL-PREV_TM,
                                 LAP_NUMBER==TOTAL_LAPS~TOTAL,
                                 TRUE~TOTAL)) %>%
  mutate(CUML_TM = cumsum(ACTUAL_TIME)) %>%
  mutate(CUML_TM_MIN = ceiling(CUML_TM/60)) %>%
  filter(CUML_TM_MIN<=60)



tr_sec_final <- tr_sector %>%
  select(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER,SESSION,CUML_TM_MIN) %>%
  left_join(.,w_final,by=c("EVENT","LOCATION","ROW_N","CUML_TM_MIN"="T_IDX")) %>%
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER) %>%
  select(-c(TIME_UTC_SECONDS,T_SEC))

te_sec_final <- te_sector %>%
  select(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER,SESSION,CUML_TM_MIN) %>%
  left_join(.,w_final,by=c("EVENT","LOCATION","ROW_N","CUML_TM_MIN"="T_IDX")) %>%
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER) %>%
  select(-c(TIME_UTC_SECONDS,T_SEC))



#retransform the data 
tr_sec_wide <- tr_sec_final %>%
  pivot_wider(.,
              id_cols = c(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N,TIME_UTC_DATE),
              names_from = SESSION,
              values_from = c(AIR_TEMP,TRACK_TEMP,HUMIDITY,PRESSURE_2,
                              WIND_SPEED,WIND_DIRECTION,RAIN)) %>%
  filter(!(LOCATION=="Location 6"&EVENT=="Free Practice 3"))

te_sec_wide <- te_sec_final %>%
  pivot_wider(.,
              id_cols = c(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N,TIME_UTC_DATE),
              names_from = SESSION,
              values_from = c(AIR_TEMP,TRACK_TEMP,HUMIDITY,PRESSURE_2,
                              WIND_SPEED,WIND_DIRECTION,RAIN))



tr_sector_time <- tr_sector %>%
  select(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N,SESSION,ACTUAL_TIME) %>%
  pivot_wider(.,
              id_cols = c(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N),
              names_from = SESSION,
              values_from = ACTUAL_TIME)


te_sector_time <- te_sector %>%
  select(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N,SESSION,ACTUAL_TIME) %>%
  pivot_wider(.,
              id_cols = c(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N),
              names_from = SESSION,
              values_from = ACTUAL_TIME)

#' We are seeing free practice 3 - location 6 in the train data, but the same is not availble in the 
#' weather data. hence removing the same from the tables. 
#' 


tr_clean <- train %>% select(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N,LAP_TIME,contains("IMPROVEMENT"),
                             GROUP,KPH,POWER,ELAPSED,TEAM,DRIVER_NAME) %>%
  inner_join(.,tr_sec_wide,by=c("EVENT","LOCATION","NUMBER","LAP_NUMBER","ROW_N")) %>%
  inner_join(.,tr_sector_time,by=c("EVENT","LOCATION","NUMBER","LAP_NUMBER","ROW_N"))


te_clean <- test %>% select(EVENT,LOCATION,NUMBER,LAP_NUMBER,ROW_N,LAP_TIME,contains("IMPROVEMENT"),
                            GROUP,KPH,POWER,ELAPSED,TEAM,DRIVER_NAME) %>%
  inner_join(.,te_sec_wide,by=c("EVENT","LOCATION","NUMBER","LAP_NUMBER","ROW_N")) %>%
  inner_join(.,te_sector_time,by=c("EVENT","LOCATION","NUMBER","LAP_NUMBER","ROW_N"))



tr_clean <- tr_clean %>% 
  separate(ELAPSED,c("MIN","SEC"),sep=":",extra="drop",remove=FALSE) %>%
  mutate(ELAPSED_SEC = floor((as.integer(MIN)*60+as.integer(SEC))/60)) %>%
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER) %>% drop_na(TIME_UTC_DATE) %>% 
  mutate_at(vars(c(EVENT,LOCATION,NUMBER,LAP_NUMBER)),as.factor) %>%
  mutate_at(vars(starts_with("RAIN")),as.numeric) %>%
  replace_na(list(GROUP=0,POWER=150)) %>%
  group_by(EVENT,LOCATION,NUMBER,ROW_N) %>%
  mutate_at(vars(KPH),
            ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

te_clean <- te_clean %>% 
  separate(ELAPSED,c("MIN","SEC"),sep=":",extra="drop",remove=FALSE) %>%
  mutate(ELAPSED_SEC = floor((as.integer(MIN)*60+as.integer(SEC))/60)) %>%
  arrange(EVENT,LOCATION,ROW_N,NUMBER,LAP_NUMBER) %>% drop_na(TIME_UTC_DATE) %>% 
  mutate_at(vars(c(EVENT,LOCATION,NUMBER,LAP_NUMBER)),as.factor) %>%
  mutate_at(vars(starts_with("RAIN")),as.numeric) %>%
  replace_na(list(GROUP=0,POWER=150)) %>%
  group_by(EVENT,LOCATION,NUMBER,ROW_N) %>%
  mutate_at(vars(KPH),
            ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

tr_clean$tr <- "TRAIN"
te_clean$tr <- "TEST"

full_clean <- bind_rows(tr_clean,te_clean) %>% 
  fill(AIR_TEMP_S1_LARGE:RAIN_S3_LARGE,.direction="updown") %>%
  replace_na(list(S3_LARGE=0,S1_LARGE=0,S2_LARGE=0)) #considering that they either aborted the lap mid way

sapply(full_clean, function(x)sum(is.na(x)))



full_clean <- full_clean %>%  ungroup() %>%
  mutate_at(vars(contains(c("AIR","WIND","TRACK","PRESSURE","HUMIDITY","RAIN"))),scale) %>%
  mutate_at(vars(contains(c("AIR","WIND","TRACK","PRESSURE","HUMIDITY","RAIN"))),as.vector) %>%
  select(-c(ELAPSED,MIN,SEC,TIME_UTC_DATE,ROW_N,ELAPSED_SEC,DRIVER_NAME,
            S1_IMPROVEMENT,S2_IMPROVEMENT,S3_IMPROVEMENT,LAP_IMPROVEMENT,
            starts_with("RAIN"))) %>%
  mutate(EVENT = str_replace_all(EVENT, "[:digit:]", ""))


CAT_COLS <- c("EVENT","LOCATION","NUMBER")
REMOVE_COLS <- c("ROW_N","LAP_TIME","TIME_UTC_DATE","ELAPSED","MIN","SEC","tr")
NUM_COLS <- setdiff(colnames(full_clean),c(CAT_COLS,REMOVE_COLS))
Y <- "LAP_TIME"
X <- c(NUM_COLS,CAT_COLS)
X 


X1_LR <- c("S1_LARGE","S2_LARGE","S3_LARGE","PIT_TIME","EVENT")

X2_ <- X[c(1:30,34:36)] #consindering all excelt sector and pit time 

full_clean <- full_clean %>% ungroup() %>% mutate_at(all_of(CAT_COLS),as.factor) %>%
  mutate(LAP_NUMBER = as.numeric(LAP_NUMBER))
#skim(full_clean)


tr_d <- full_clean %>% filter(tr=="TRAIN")
te_d <- full_clean %>% filter(tr=="TEST")

str(tr_d)

#running a simple linear regression
lm_sector_pit_l3 <- lm(LAP_TIME~S1_LARGE+S2_LARGE+S3_LARGE+PIT_TIME+EVENT,
                       data = filter(tr_d,LAP_NUMBER<3))
summary(lm_sector_pit_l3)

formula(lm_sector_pit_l3)

te_fitted_lm_sec_pit_l3 <- predict(lm_sector_pit_l3,te_d)

all_pred <- add_pred(
          tibble(predict = te_fitted_lm_sec_pit_l3, 
                 RR = "Baseline_LM")
)

#running a simple h2o glm 

splits <- rsample::initial_split(tr_d,prop = 0.8)
tr_ <- rsample::training(splits)
va_ <- rsample::testing(splits)


h2o.init()

#h20
tr_h2o <- as.h2o(tr_[,c(X,Y)] %>% filter(.,LAP_NUMBER < 5))
va_h2o <- as.h2o(va_[,c(X,Y)])
te_h2o <- as.h2o(te_d[,c(X,Y)])
full_h2o <- as.h2o(tr_d[,c(X,Y)]  %>% filter(.,LAP_NUMBER < 5))

almname <- paste('ak_h2o_automl',format(Sys.time(),"%d%H%M%S"),sep = '_')
autoML <- h2o.automl(X1_LR,Y,training_frame = full_h2o,
                     validation_frame = va_h2o,seed=223, 
                     stopping_metric=c("RMSLE"),
                     sort_metric = c("RMSLE"), max_models = 10,
                     include_algos  = c("GLM"),
                     preprocessing = "target_encoding")



autoML@leader

leader_name <- as_tibble(autoML@leaderboard) %>% dplyr::slice(1) %>% dplyr::pull(model_id)

leader_model <- h2o.getModel(leader_name)

h2o.saveModel(leader_model,path="./models/",filename = leader_name)

h2o.rmsle(leader_model)

te_pred_LR <- h2o.predict(leader_model,te_h2o)%>% as_tibble() %>%
  mutate(RR = leader_name)


all_pred <- add_pred(te_pred_LR)


dd %>% ggplot(aes(LAP_TIME,color=RR))+geom_density()

h2o_tr_res <- h2o.predict(leader_model,full_h2o) %>% as_tibble() %>% 
  bind_cols(.,filter(tr_d[,c("LAP_NUMBER","LAP_TIME")],LAP_NUMBER<5)) %>% 
  mutate(residual = LAP_TIME-predict)

tr_lap_5 <- filter(tr_d,LAP_NUMBER<5) %>% mutate_if(is.character,as.factor)
te_lap_5 <- te_d %>% mutate_if(is.character,as.factor)


#running a simple cat boost with other data
#tr_pool <- catboost.load_pool(data =  tr_[,X2_],label = unlist(tr_[,Y]))
#va_pool <- catboost.load_pool(data =  va_[,X2_],label = unlist(va_[,Y]))
te_pool <- catboost.load_pool(data =  te_lap_5[,X2_])
full_pool <- catboost.load_pool(data =  tr_lap_5[,X2_],
                                label = unlist(h2o_tr_res[,"residual"]))



final_params <- list(iterations=1000,
                     learning_rate=0.03,
                     depth=6,
                     loss_function="RMSE",
                     random_seed = 55,
                     od_pval = 10^-5,
                     use_best_model=TRUE,
                     metric_period=100
          )

model_final_train <- catboost.train(full_pool,params=final_params)

te_re_pred <- catboost.predict(model_final_train,te_pool)
te_re_pred

dd <- bind_cols(all_pred,tibble(RES = c(te_re_pred,te_re_pred,te_re_pred)))


dd <- dd %>% mutate(LAP_TIME = RES+predict)


dd %>% distinct(RR)

submission <- data.frame(LAP_TIME=dd[841:1260,"LAP_TIME"])
filename <- paste('GLM2_CATB',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(submission,paste0(filename,'.csv',collapse = ''),row.names = FALSE)
