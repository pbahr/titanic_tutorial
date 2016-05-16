source("part_1.R")

# ---- ml-1 ----
library(caret)
library(rpart)

# ---- ml-2 ----
sex.model.tree <- train(Survived ~ Sex.factor + Pclass.factor, data= train.data, method= "rpart")
sex.class.survival <- predict(sex.model.tree, train.data)
conMat <- confusionMatrix(sex.class.survival, train.data$Survived)

# ---- ml-3 ----
sex.class.fare.tree <- train(Survived ~ Sex.factor + Pclass.factor + Fare, data= train.data, method= "rpart")
sex.class.fare.survival <- predict(sex.class.fare.tree, train.data)
conMat <- confusionMatrix(sex.class.fare.survival, train.data$Survived)

# ---- ml-6 ----
full.data$Fare[is.na(full.data$Fare)] <- median(full.data$Fare, na.rm = T)
test.data <- full.data[(train.len+1):nrow(full.data), ]

sex.class.fare.solution <- get.solution(sex.class.fare.tree, test.data)

# ---- ml-7 ----
# Write your solution to a csv file with the name my_solution.csv
if (!file.exists("output/new versions/sex_class_fare.csv"))
    write.csv(sex.class.fare.solution, file= "output/new versions/sex_class_fare.csv" , row.names= FALSE)

# ---- ml-8 ----
# Pick columns to define neighbors
relevant.vars <- c(2, 5:7, 9)
# Data set to impute from
impute.data <- full.data[,relevant.vars]

# ---- ml-9 ----
# Prepare the preProcess object
pp <- preProcess(impute.data, method = c("knnImpute"))

# get imputed age, and the original mean and standard deviation
imputedAge <- predict(pp, newdata = impute.data)$Age
meanAge <- pp$mean["Age"] 
stdAge <- pp$std["Age"]

# back to original scale
imputedAge <- imputedAge * stdAge + meanAge

full.data.impute <- full.data
selector <- is.na(full.data$Age)
full.data.impute$Age[selector] <- imputedAge[selector]

# ---- ml-10 ----
train.data.impute <- full.data.impute[1:train.len, ]
test.data.impute <- full.data.impute[(train.len+1):nrow(full.data.impute), ]
# Add the outcome column
train.data.impute$Survived <- train.outcome

# ---- ml-11 ----
sex.class.age.tree <- train(Survived ~ Sex + Pclass + Age, data= train.data.impute, method= "rpart")
sex.class.age.survival <- predict(sex.class.age.tree, train.data.impute)
conMat <- confusionMatrix(sex.class.age.survival, train.data.impute$Survived)

# ---- ml-12 ----
sex.class.age.solution <- get.solution(sex.class.age.tree, test.data.impute)

if (!file.exists("output/new versions/sex_class_age.csv"))
    write.csv(sex.class.age.solution, file= "output/new versions/sex_class_age.csv" , row.names= FALSE)


