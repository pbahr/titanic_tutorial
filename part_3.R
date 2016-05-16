# ---- cv-1 ----
cvCtrl <- trainControl(method= "cv", number = 10) # use 10-fold cross validation
sex.class.age.tree <- train(Survived ~ Sex.factor + Pclass.factor + Age, 
                            data= train.data.impute, method= "rpart",
                            trControl= cvCtrl)

# ---- cv-2 ----
sex.class.age.survival <- predict(sex.class.age.tree, train.data.impute)
conMat <- confusionMatrix(sex.class.age.survival, train.data.impute$Survived)

# ---- cv-3 ----
cvCtrl <- trainControl(method= "cv", number = 10)
sex.class.age.tree <- train(Survived ~ Sex.factor + Pclass.factor + Age, 
                            data= train.data.impute, method= "rpart",
                            trControl= cvCtrl,
                            tuneLength= 5)

# ---- cv-4 ----
sex.class.age.survival <- predict(sex.class.age.tree, train.data.impute)
conMat <- confusionMatrix(sex.class.age.survival, train.data.impute$Survived)

# ---- cv-5 ----
sex.class.age.tl.solution <- get.solution(sex.class.age.tree, test.data.impute)

if(!file.exists("output/new versions/sex_class_age_tl_cv.csv"))
    write.csv(sex.class.age.tl.solution, file= "output/new versions/sex_class_age_tl_cv.csv" , row.names= FALSE)

# ---- cv-6 ----
cvCtrl <- trainControl(method= "cv", number = 10)
sex.class.age.tree <- train(Survived ~ Sex.factor + Pclass.factor + Age, 
                            data= train.data.impute, method= "rpart",
                            trControl= cvCtrl,
                            tuneLength= 10)

# ---- cv-7 ----
cvCtrl <- trainControl(method= "cv", number = 10)
sex.class.fare.age.tree <- train(Survived ~ Sex + Pclass + Fare + Age, 
                                 data= train.data.impute, method= "rpart",
                                 trControl= cvCtrl,
                                 tuneLength= 5)

sex.class.fare.age.survival <- predict(sex.class.fare.age.tree, train.data.impute)
conMat <- confusionMatrix(sex.class.fare.age.survival, train.data.impute$Survived)

# ---- cv-8 ----
sex.class.fare.age.solution <- get.solution(sex.class.fare.age.tree, test.data.impute)
write.csv(sex.class.fare.age.solution, file= "output/new versions/sex_class_fare_age_cv_tl.csv" , row.names= FALSE)

