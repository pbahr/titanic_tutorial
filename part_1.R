# ---- loading-1 ----
source("library.R")

# ---- loading-2 ----
# Load data
train.data <- read.csv("input/train.csv", stringsAsFactors = F)
test.data <- read.csv("input/test.csv", stringsAsFactors = F)

# Keep the outcome in a separate variable
train.outcome <- train.data$Survived
train.outcome <- factor(train.outcome)

# Remove "Survived" column
train.data <- train.data[,-2]

train.len <- nrow(train.data)
test.len <- nrow(test.data)

# Combine training & testing data
full.data <- rbind(train.data, test.data)

# Create factor version of categorical vars
full.data$Pclass.factor <- factor(full.data$Pclass)
levels(full.data$Pclass.factor) <- paste0("Class.", levels(full.data$Pclass.factor))

full.data$Sex.factor <- factor(full.data$Sex)
full.data$Embarked.factor <- factor(full.data$Embarked)

# ---- loading-3 ----
# Split data back into training and testing
train.data <- full.data[1:train.len, ]
test.data <- full.data[(train.len+1):nrow(full.data), ]

# Check the summary of quantitative and categorical variables
sapply(train.data[, -c(1, 3, 8, 10)], summary)

# Add the outcome column
train.data$Survived <- train.outcome

# ---- eda-1 ----
prop.sex <- prop.table(table(train.data$Sex.factor, useNA = "ifany"))
prop.class <-  prop.table(table(train.data$Pclass.factor, useNA = "ifany"))

percent(prop.sex, digits = 2)
percent(prop.class, digits = 2)

# ---- eda-2 ----
table(train.data$Survived, useNA = "ifany")

prop.survived <- prop.table(table(train.data$Survived, useNA = "ifany"))
percent(prop.survived, digits = 2)

prop.sex.survived <- prop.table(table(Sex= train.data$Sex.factor, Survived= train.data$Survived), margin = 1)
prop.class.survived <- prop.table(table(Pclass= train.data$Pclass.factor, Survived= train.data$Survived), margin = 1)

percent(prop.sex.survived, digits = 2)
percent(prop.class.survived, digits = 2)

# ---- eda-3 ----
prop.table(table(Pclass= train.data$Pclass, Sex= train.data$Sex), margin = 1)

# ---- eda-4 ----
prop.class.sex.survived <- prop.table(table(Pclass= train.data$Pclass, Sex= train.data$Sex.factor, Survived= train.data$Survived), margin = 1:2)
percent(prop.class.sex.survived, digits = 2)

# ---- eda-5 ----
percent(train.len/(train.len + test.len), digits = 2)
percent(test.len/(train.len + test.len), digits = 2)

# ---- ga-1 ----
library(ggplot2)

# ---- ga-2 ----
ggplot(train.data, aes(Sex.factor, fill= Survived)) +
    geom_bar(stat = "bin", position = "stack") +
    ggtitle("Women have higher chance of survival")

ggplot(train.data, aes(x=Pclass.factor, fill= Survived)) +
    geom_bar(stat = "bin", position = "stack") +
    ggtitle("In class 1 most people survived and in class 3 most did not")

ggplot(train.data, aes(x=Pclass.factor, fill= Survived))+
    geom_bar(stat = "bin", position = "stack") +
    facet_wrap(~Sex.factor) +
    ggtitle("Most women survived across passenger classes")

ggplot(train.data, aes(x= Pclass.factor, y= Age, color= Survived)) +
    geom_violin() +
    geom_jitter(alpha= .5) +
    ggtitle("Age Matters!")

# ---- ga-3 ----
ggplot(train.data, aes(x= Sex.factor, y= Age, color= Survived)) +
    geom_jitter() +
    geom_violin() +
    facet_wrap(~Pclass.factor)

ggplot(train.data, aes(y=Fare, x= Survived)) +
    geom_boxplot() +
    facet_wrap(~Pclass.factor, ncol = 3, scales = "free") +
    ggtitle("People who survived paid higher fare on average")

# ---- sa-1 ----
aggregate(Fare ~ Survived + Pclass.factor, data=train.data, mean)

t.test(Fare ~ Survived, data= train.data, subset = train.data$Pclass.factor == "Class.1")$conf.int
t.test(Fare ~ Survived, data= train.data, subset = train.data$Pclass.factor == "Class.2")$conf.int
t.test(Fare ~ Survived, data= train.data, subset = train.data$Pclass.factor == "Class.3")$conf.int

