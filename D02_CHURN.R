## I build a dataset to calculate the average days that pass from one purchase to another ##
df7_purchases <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_SCONTRINO) %>% 
  summarise(ID_CLI=max(ID_CLI), TIC_DATE=max(TIC_DATE))

df7_num_purchases <- df7_purchases %>% group_by(ID_CLI) %>% 
  summarise(tot = n()) %>% 
  filter(tot>1)
  
df7_num_purchases <- left_join(df7_num_purchases, df7_purchases,by="ID_CLI")

df7_interval_purchases <- df7_num_purchases %>% 
  arrange(desc(TIC_DATE)) %>% 
  group_by(ID_CLI) %>% 
  summarise(LAST_DAY=nth(TIC_DATE,1),SECOND_LAST_DAY=nth(TIC_DATE,2)) 

df7_interval_purchases <- df7_interval_purchases %>% 
  mutate(INTERVAL_DAYS = round(difftime(LAST_DAY, SECOND_LAST_DAY, units = "days")))

## an average of 45 days pass between one purchase and the next ##
df7_interval_purchases %>% summarise(round(mean(INTERVAL_DAYS))) #45 days 

## 80% of customers buy again before 76 days ##
df7_interval_purchases %>% summarise(quantile(INTERVAL_DAYS,.80)) #60 days 

## I check the dates of the first and last receipt ##
min(df7_purchases$TIC_DATE) #"2018-05-01"
max(df7_purchases$TIC_DATE) #"2019-04-30"

## plot propensity to churn ##
df7_interval_purchases <- df7_interval_purchases %>%
  group_by(INTERVAL_DAYS) %>%
  summarize(NUM_CLI = sum(n_distinct(ID_CLI)))

df7_interval_purchases$CUM_SUM <- cumsum(df7_interval_purchases$NUM_CLI)
df7_interval_purchases$PERC_CUM <- (df7_interval_purchases$CUM_SUM / max(df7_interval_purchases$CUM_SUM)) * 100

df7_interval_purchases %>%
  ggplot(aes(x = INTERVAL_DAYS, y = PERC_CUM)) + 
  geom_line(color = "red", size = 0.8) +
  geom_vline(xintercept=75, color = "blue", size = 0.2) + 
  geom_hline(yintercept=80, color = "blue", size = 0.2) +
  labs(x="Average days next purchase", y="%Customers", 
       title="Cumulative repurchase percentage") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_dark()
 
## I consider a churner customer who does not buy within 75 days ##
df7_churners <- df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%
  summarize(LAST_PURCHASE_DATE = max(TIC_DATETIME),
            TOT_PURCHASE = sum(IMPORTO_LORDO),
            NUM_OF_PURCHASE=n()) %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE < as.Date("2019-02-16"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOT_PURCHASE,NUM_OF_PURCHASE)

## final dataset ##
df_7_final <- df_1_cli_fid_clean %>%
  select(ID_CLI, FIRST_ID_NEG, LAST_TYP_CLI_FID, LAST_COD_FID, LAST_STATUS_FID) %>%
  left_join(df_2_cli_account_clean
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  select(-ID_ADDRESS, -TYP_JOB)

df_7_final <- df7_churners %>%
  left_join(df_7_final, by="ID_CLI") %>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION))

## prepare the dataset for the models ##
model_set <- df_7_final
str(model_set)
model_set$CHURN <- as.factor(model_set$CHURN)
model_set <- model_set[,-c(2,3,13)] ## ID_CLI, LAST_PURCHASE_DATE and PRV ##
                                    ## They are variables that do not interest us ##
## plot class imbalance ##
ggplot(data=model_set, aes(x=CHURN)) +
  geom_bar( fill="lightblue", color="black") +
  labs(x="No churn (0) / churn (1)", y="Number of customers",
       title="Class imbalance") +
  theme_minimal()

## train and test set ##
random_index <- createDataPartition(model_set$CHURN, p = .70, list = FALSE, times = 1)
train <- model_set[random_index,]
test <- model_set[-random_index,]

## check that the dataset is balanced ##
churn_0 <- train %>% filter(CHURN == 0)
nrow(churn_0) #60780 rows 

churn_1 <- train %>% filter(CHURN == 1)
nrow(churn_1) #87708 rows

churn_1_balanced <- churn_1[sample(nrow(churn_1), nrow(churn_0)),]

train_balanced <- rbind(churn_1_balanced, churn_0)

train <- train_balanced

## Random Forest ##
RF <- randomForest(CHURN ~ ., data = train, ntree = 100)
print(RF)

## prediction RF ##
predict_RF <- rpart.predict(RF, test[,-1], type = "class")
confusionMatrix(predict_RF, test$CHURN)

## Naive Bayes ##
NB <- naiveBayes(CHURN ~ ., train)
print(NB)

## prediction NB ##
predict_NB <- predict(NB, test[,-1])
confusionMatrix(predict_NB, test$CHURN)

## Generalized Linear Models ##
GLM <- glm(CHURN ~ ., train, family = "binomial")
print(GLM)

## prediction GLM ##
p = predict(GLM, test)
predict_GLM = if_else(p>0.5,1,0)
predict_GLM <- as.factor(predict_GLM)
table_GLM <- table(predict_GLM, test$CHURN)
confusionMatrix(table_GLM)

## #Recursive Partitioning And Regression Trees ##
TREE <- rpart(CHURN ~ ., data = train)
summary(TREE)
rpart.plot(TREE, extra = "auto")
printcp(TREE)

## prediction RPART ##
predict_RPART <- rpart.predict(TREE, test[,-1],type = "class")
predict_RPART <- unlist(predict_RPART)
confusionMatrix(predict_RPART, test$CHURN)

## Bagging Classification ##
BAG <- bagging(CHURN ~ ., train, nbagg = 25)

## prediction BAG ##
predict_BAG <- predict(BAG, test[,-1])
confusionMatrix(predict_BAG, test$CHURN)

## model evaluation ##
value_1 <- recall(predict_RF, test$CHURN, relevant = "1")
value_2 <- precision(predict_RF, test$CHURN, relevant = "1") 
value_3 <- F1_Score(predict_RF ,test$CHURN,positive = '1') 
value_4 <- Accuracy(predict_RF, test$CHURN) 
RF_values <- c(value_1, value_2, value_3, value_4)

value_1 <- recall(predict_NB, test$CHURN, relevant = "1")
value_2 <- precision(predict_NB, test$CHURN, relevant = "1") 
value_3 <- F1_Score(predict_NB ,test$CHURN,positive = '1') 
value_4 <- Accuracy(predict_NB, test$CHURN) 
NB_values <- c(value_1, value_2, value_3, value_4)

value_1 <- recall(predict_GLM, test$CHURN, relevant = "1")
value_2 <- precision(predict_GLM, test$CHURN, relevant = "1") 
value_3 <- F1_Score(predict_GLM ,test$CHURN,positive = '1') 
value_4 <- Accuracy(predict_GLM, test$CHURN) 
GLM_values <- c(value_1, value_2, value_3, value_4)

value_1 <- recall(predict_RPART, test$CHURN, relevant = "1")
value_2 <- precision(predict_RPART, test$CHURN, relevant = "1") 
value_3 <- F1_Score(predict_RPART ,test$CHURN,positive = '1') 
value_4 <- Accuracy(predict_RPART, test$CHURN) 
RPART_values <- c(value_1, value_2, value_3, value_4)

value_1 <- recall(predict_BAG, test$CHURN, relevant = "1")
value_2 <- precision(predict_BAG, test$CHURN, relevant = "1") 
value_3 <- F1_Score(predict_BAG ,test$CHURN,positive = '1') 
value_4 <- Accuracy(predict_BAG, test$CHURN) 
BAG_values <- c(value_1, value_2, value_3, value_4)

## results ##
results <- as.data.frame(rbind(RF_values, NB_values, GLM_values, RPART_values, 
                               BAG_values))
colnames(results) <- c("RECALL", "PRECISION", "F1_SCORE", "ACCURACY")
results

## plotting ROC CURVE ##
par("mfrow"=c(1,1))
roc_RF <- roc(test$CHURN ~ as.numeric(unlist(predict_RF)),plot=TRUE,
              print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")
roc_NB <- roc(test$CHURN ~ as.numeric(unlist(predict_NB)),plot=TRUE,
              print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.2,
              legacy.axes=TRUE,add = TRUE)
roc_GLM <- roc(test$CHURN ~ as.numeric(unlist(predict_GLM)),plot=TRUE,
              print.auc=TRUE,col="violet",lwd = 4,print.auc.y=0.1,
              legacy.axes=TRUE,add = TRUE)
roc_RPART <- roc(test$CHURN ~ as.numeric(unlist(predict_RPART)),plot=TRUE,
               print.auc=TRUE,col="orange",lwd = 4,print.auc.y=0.4,
               legacy.axes=TRUE,add = TRUE)
roc_BAG <- roc(test$CHURN ~ as.numeric(unlist(predict_BAG)),plot=TRUE,
               print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.3,
               legacy.axes=TRUE,add = TRUE)
legend("left", legend=c("RF", "NB", "GLM", "RPART","BAG"),
       fill =c("green","red", "violet", "orange", "blue"), cex = .55,
       inset = .1, bty = "n")

F1 <- data.frame(Models = c("RF", "NB", "GLM", "RPART","BAG"), 
                 F1_SCORE = results[,3])

## plotting F1 Measure ##
ggplot(F1, aes(x = Models, y = F1_SCORE)) + 
  geom_bar(stat="identity", 
           fill = c("aquamarine","cyan", "blueviolet","mediumpurple", 
                    "blue"), color = "black") +
  labs(x="Models", y="F1-Measure", 
       title="F1 measure of models") +
  scale_y_continuous(breaks=seq(0,2,.15)) +
  theme_minimal()
