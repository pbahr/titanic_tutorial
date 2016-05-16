library(reshape2)
library(ggplot2)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

percent <- function(x, digits= 2) {
    round(x * 100, digits = digits)
}

plot.decision.tree <- function(tree) {
    fancyRpartPlot(tree)
}

# get a prediction data frame using test data set
get.solution <- function(tree, data) {
    my_prediction <- predict(tree, data)
    
    # Create a data frame with two columns: PassengerId & Survived. Survived contains the predictions
    my_solution <- data.frame(PassengerId = data$PassengerId, Survived = my_prediction )
    
    my_solution
}

show.benchmark <- function(tree) {
    benchmark <- train
    
    benchmark$pred <- predict(tree, benchmark, "class")
    benchmark$Survived <- factor(benchmark$Survived)
    levels(benchmark$Survived) <- c("Failed", "Survived")
    levels(benchmark$pred) <- c("F", "T")
    
    t <- prop.table(table(benchmark$pred, benchmark$Survived))
    
    print(paste0("False Negative:", round(t[1,2]*100, 2)))
    print(paste0("False Positive:", round(t[2,1]*100, 2)))
    print(paste0("Total Error:", round(t[2,1]*100 + t[1,2]*100, 2)))
}

plot.cormat <- function(mydata) {
    cormat <- round(cor(mydata), 2)
    head(cormat)
    
    cormat[upper.tri(cormat)] <- NA
    
    melted_cormat <- melt(cormat)
    head(melted_cormat)
    
    ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile(color= "white") +
        scale_fill_gradient2(low= "blue", high="red", mid="white", midpoint = 0, 
                             limit=c(-1,1), space="Lab", name= "Pearson\nCorrelation") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1))+
        coord_fixed()
}

plot.x.y.factor <- function(x.name, y.name, category.name, data) {
    data[,category.name] <- factor(data[,category.name])
    ggplot(data, aes_string(x= x.name, y= y.name, color= category.name)) +
        geom_point() +
        geom_smooth(method= lm)
}

