rm(list=ls())
train <- read.csv("E:/R/competion case study/train_LZdllcl.csv",na.strings=c(""," ","NA"))


str(train)
summary(train)
View(train)

# after analysing the summary we get to know that previous_year_rating contains NA values.
train$previous_year_rating[(is.na(train$previous_year_rating))] = 3
summary(train$previous_year_rating)

train$dept_analy <- as.numeric(train$department == "Analytics")
train$dept_fin <- as.numeric(train$department == "Finance")
train$dept_hr <- as.numeric(train$department == "HR")
train$dept_leg <- as.numeric(train$department == "Legal")
train$dept_oper <- as.numeric(train$department == "Operations")
train$dept_proc <- as.numeric(train$department == "Procurement")
train$dept_sale <- as.numeric(train$department == "Sales & Marketing")
train$dept_tech <- as.numeric(train$department == "Technology")


#dummy_department <- dummy_cols(train$department,remove_first_dummy = T)
#data <- data.frame(c(train,dummy_department))



summary(train)
View(train)

train$gender_m <-  as.numeric(train$gender == "m")

train$recr_re <- as.numeric(train$recruitment_channel == "referred")
train$recr_sou <- as.numeric(train$recruitment_channel == "sourcing")

train$region_1 <- as.numeric(train$region == "region_1")
train$region_2 <- as.numeric(train$region == "region_2")
train$region_3 <- as.numeric(train$region == "region_3")
train$region_4 <- as.numeric(train$region == "region_4")
train$region_5 <- as.numeric(train$region == "region_5")
train$region_6 <- as.numeric(train$region == "region_6")
train$region_7 <- as.numeric(train$region == "region_7")
train$region_8 <- as.numeric(train$region == "region_8")
train$region_9 <- as.numeric(train$region == "region_9")
train$region_10 <- as.numeric(train$region == "region_10")
train$region_11 <- as.numeric(train$region == "region_11")
train$region_12 <- as.numeric(train$region == "region_12")
train$region_13 <- as.numeric(train$region == "region_13")
train$region_14 <- as.numeric(train$region == "region_14")
train$region_15 <- as.numeric(train$region == "region_15")
train$region_16 <- as.numeric(train$region == "region_16")
train$region_17 <- as.numeric(train$region == "region_17")
train$region_18 <- as.numeric(train$region == "region_18")
train$region_19 <- as.numeric(train$region == "region_19")
train$region_20 <- as.numeric(train$region == "region_20")
train$region_21 <- as.numeric(train$region == "region_21")
train$region_22<- as.numeric(train$region == "region_22")
train$region_23 <- as.numeric(train$region == "region_23")
train$region_24 <- as.numeric(train$region == "region_24")
train$region_25 <- as.numeric(train$region == "region_25")
train$region_26 <- as.numeric(train$region == "region_26")
train$region_27 <- as.numeric(train$region == "region_27")
train$region_28 <- as.numeric(train$region == "region_28")
train$region_29 <- as.numeric(train$region == "region_29")
train$region_30 <- as.numeric(train$region == "region_30")
train$region_31 <- as.numeric(train$region == "region_31")
train$region_32 <- as.numeric(train$region == "region_32")
train$region_33<- as.numeric(train$region == "region_33")

summary(train)

final_train <- train[-c(1,2,3,4,5,6)]
summary(final_train)

m <- cor(final_train)
corrplot(m,method = "circle")

model <- glm(is_promoted ~ . ,data = final_train)
step(model)
summary(model)

model_2 <-  glm(formula = is_promoted ~ no_of_trainings + age + previous_year_rating + 
                  length_of_service + KPIs_met..80. + awards_won. + avg_training_score + 
                  dept_analy + dept_fin + dept_hr + dept_leg + dept_oper + 
                  dept_proc + dept_sale + dept_tech + region_2 + region_3 + 
                  region_4 + region_7 + region_10 + region_13 + region_15 + 
                  region_17 + region_22 + region_23 + region_25 + region_27 + 
                  region_28 + region_30, data = final_train)
summary(model_2)

model_3 <- glm(formula = is_promoted ~ no_of_trainings + age + previous_year_rating + 
                 length_of_service + KPIs_met..80. + awards_won. + avg_training_score + 
                 dept_analy + dept_fin + dept_hr + dept_leg + dept_oper + 
                 dept_proc + dept_sale + dept_tech + region_2 + region_3 + 
                 region_4 + region_7  + region_13 + region_15 + 
                 region_17 + region_22 + region_23 + region_25 + region_27 + 
                 region_28 + region_30, data = final_train)

final_train$score = predict(model_3,newdata = final_train,type= "response")
head(final_train$score)

prediction <- ifelse(final_train$score>=0.6,1,0)
confusionMatrix(as.factor(prediction),as.factor(final_train$is_promoted),positive="1")

prediction <- ifelse(final_train$score>=0.8,1,0)
confusionMatrix(as.factor(prediction),as.factor(final_train$is_promoted),positive="1")