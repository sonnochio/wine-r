easypackages::libraries("dplyr", "ggplot2", "tidyr", "corrplot", "corrr", "magrittr",   "e1071","ggplot2","RColorBrewer", "viridis","gridExtra","nnet")
library(kableExtra)
options(scipen = 5)      #To force R to not use scientfic notation

#set up project root to be the folder
setwd('/Users/sonnykong/code/sonnochio/wine-explore-r')

#read the csv
dataset <- read.csv("winequality-red.csv",sep = ";",header = TRUE)

#summary
str(dataset)

#column names
colnames(dataset)
#column names for better reading
strsplit(colnames(dataset), "\\.")

#plot quality histogram and density distribution 
myplot <- ggplot(data = dataset, aes(x = quality ))
myplot+theme_bw()+
  geom_histogram(aes(y=after_stat(density)),color = 'black', fill = 'white', binwidth = 1)+
  geom_density(alpha=0.2, fill='blue')+
  labs(title = "quality density", x="quality", y="density")

#print key summary
summary(dataset$quality)
#We have a lot of wine types with the quality of 5 and 6

# Select interested variables , they are all quantitative
# pH
# alcohol
# density
# volatile.acidity

#convert qualitative variables 
dataset$quality <- factor(dataset$quality)
# str(dataset)

#graphing the variables
#pH vs. Quality
ggplot(dataset, aes(y=quality, x=pH, group=quality, fill=quality)) +
  theme_bw()+
  geom_boxplot(outlier.colour="red")+
  theme(legend.position="none")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "pH vs. Quality", x="pH", y="Quality")

#alcohol vs. Quality
ggplot(dataset, aes(y=quality, x=alcohol, group=quality, fill=quality)) +
  theme_bw()+
  geom_boxplot(outlier.colour="red")+
  theme(legend.position="none")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "alcohol vs. Quality", x="alcohol", y="Quality")

#density vs. Quality
ggplot(dataset, aes(y=quality, x=density, group=quality, fill=quality)) +
  theme_bw()+
  geom_boxplot(outlier.colour="red")+
  theme(legend.position="none")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "density vs. Quality", x="density", y="Quality")

#volatile.acidity vs. Quality
ggplot(dataset, aes(y=quality, x=volatile.acidity, group=quality, fill=quality)) +
  theme_bw()+
  geom_boxplot(outlier.colour="red")+
  theme(legend.position="none")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Volatile Acidity vs. Quality", x="Volatile Acidity", y="Quality")

#Findings:all selected features and the quality has strong  correlation

M <- dataset
M <- M %>% mutate_if(is.factor, as.numeric)
M <- cor(M)
corrplot(M, method = "color" )

summary(dataset)
#remove coeff - 1 and duplicates
M[lower.tri(M,diag=TRUE)] <- NA                  
M[M == 1] <- NA
corrplot(M, method = "color" , na.label = " ")

#Select significant features
M <- as.data.frame(as.table(M))
M <- na.omit(M)

#after testing, 0.2 produces the top 4 correlation to quality
M <-subset(M, abs(Freq) > 0.2)  
M <- M[order(-abs(M$Freq)),]

#reshape the data frame back to a cor matix
M1 <- reshape2::acast(M, Var1~Var2, value.var="Freq")
M1
corrplot(M1, method = "circle" , na.label = " ",tl.col="black",)


#now we put the new selected features into a new data frame

new_features <- as.character(M[M$Var2 == "quality", "Var1"])
new_features <- c(new_features, "quality")
new_features
new_df <- data.frame(matrix(nrow = nrow(dataset), ncol = length(new_features)))

for (i in seq_along(new_features)) {
  new_df[, i] <- dataset[[new_features[i]]]
}

colnames(new_df) <- new_features

clean_df <- new_df

pairs(new_df)

#Normalize quantitative features and analysis
colnames(new_df)

#Alcohol
plot1<- ggplot(new_df,aes(x=alcohol))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot2 <- ggplot(new_df, aes(sample=alcohol))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot1, plot2, ncol=2)
#log to remove skewness
new_df$log_alcohol <- log(new_df$alcohol)

plot3<- ggplot(new_df,aes(x=log_alcohol))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot4 <- ggplot(new_df, aes(sample=log_alcohol))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot3, plot4, ncol=2)



#volatile.acidity
plot1<- ggplot(new_df,aes(x=volatile.acidity))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot2 <- ggplot(new_df, aes(sample=volatile.acidity))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot1, plot2, ncol=2)

#log to remove skewness
new_df$log_volatile.acidity <- log(new_df$volatile.acidity)

plot1<- ggplot(new_df,aes(x=log_volatile.acidity))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot2 <- ggplot(new_df, aes(sample=log_volatile.acidity))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot1, plot2, ncol=2)


#sulphates
plot1<- ggplot(new_df,aes(x=sulphates))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot2 <- ggplot(new_df, aes(sample=sulphates))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot1, plot2, ncol=2)

#log to remove skewness
new_df$log_sulphates <- log(new_df$sulphates)

plot1<- ggplot(new_df,aes(x=log_sulphates))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot2 <- ggplot(new_df, aes(sample=log_sulphates))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot1, plot2, ncol=2)


#citric.acid
plot1<- ggplot(new_df,aes(x=citric.acid))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot2 <- ggplot(new_df, aes(sample=citric.acid))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot1, plot2, ncol=2)

#log to remove skewness
#There was some zeros in citric.acid, we need to remove them

new_df <- transform(new_df, cat_citric.acid = ifelse(citric.acid>0, 1, 0))
new_df$log_citric.acid <- log(new_df$citric.acid)
new_df <- transform(new_df, log_citric.acid =  ifelse(cat_citric.acid==1,log_citric.acid,0))

plot1<- ggplot(new_df,aes(x=log_citric.acid))+
  theme_classic()+
  geom_density(fill="green", color="gray", alpha=0.8)+
  geom_density(color="black", alpha=1, adjust = 5, lwd=0.5)+
  labs(title = "Alcohol Density", x="Alcohol", y="Density")

plot2 <- ggplot(new_df, aes(sample=log_citric.acid))+
  theme_classic()+
  stat_qq(color="green")+
  stat_qq_line(lty=2)+
  labs(title="Probablity Plot for Alcohol")

grid.arrange(plot1, plot2, ncol=2)



# Homoscedasticity Analysis
ggplot(new_df, aes(x=log_alcohol,y=quality,fill=quality))+
  theme_classic()+
  geom_boxplot()

ggplot(new_df, aes(x=log_volatile.acidity,y=quality,fill=quality))+
  theme_classic()+
  geom_boxplot()

ggplot(new_df, aes(x=log_sulphates,y=quality,fill=quality))+
  theme_classic()+
  geom_boxplot()

ggplot(new_df, aes(x=log_citric.acid,y=quality,fill=quality))+
  theme_classic()+
  geom_boxplot()



#plot the top 2 features 
ggplot(new_df,aes(x=alcohol, y=volatile.acidity, fill=quality))+
  geom_point()+
  scale_fill_viridis(discrete = TRUE) 



plot(x=clean_df$alcohol, y=clean_df$volatile.acidity, col=as.numeric(clean_df$quality),
     pch=as.numeric(clean_df$quality), 
     xlab = "alcohol", ylab="volatile acidity",
     main = "Alcohol vs. Volatile Acidity")

legend(x = "topright", legend = levels(new_df$quality), pch = 1:length(levels(clean_df$quality)),
       col = 1:length(levels(clean_df$quality)), title = "Quality")


require("e1071")


model <- svm(quality ~ alcohol+volatile.acidity, data=clean_df, cost=10)

plot(model, new_df,alcohol ~ volatile.acidity )


#reduce labels

simple_df <- subset(clean_df, quality %in% c(3, 5, 7))

plot(x=simple_df$alcohol, y=simple_df$volatile.acidity, col=as.numeric(simple_df$quality),
     pch=as.numeric(simple_df$quality), 
     xlab = "alcohol", ylab="volatile acidity",
     main = "Alcohol vs. Volatile Acidity")

legend(x = "topright", legend = levels(new_df$quality), pch = 1:length(levels(simple_df$quality)),
       col = 1:length(levels(simple_df$quality)), title = "Quality")

model2 <- svm(quality ~ alcohol+volatile.acidity, data=simple_df, cross=10, cost=100)

plot(model2, simple_df,alcohol ~ volatile.acidity )

#Use Neural Network for all labels

#split dataset randomly into train and test
# Generate random indices
rand_index <- sample(nrow(clean_df))

# Calculate the chunk sizes
chunk_size <- round(c(4/5, 1/5) * nrow(clean_df))
chunk_size
# Split the data into chunks
chunks <- split(rand_index, rep(1:2, chunk_size))

chunks
# Extract the data from the chunks
train_index <- chunks[[1]]
test_index <- chunks[[2]]
train_data <- clean_df[train_index,1:5]
test_data <- clean_df[test_index,1:5]



# prepping train dataset
train_df_without_quality <- subset(train_data, select = -quality)
train_df_norm <- as.data.frame(scale(train_df_without_quality))
str(train_df_norm)
target <- train_data$quality
train_df <- cbind(train_df_norm, target)
str(train_df)
train_df$Y <- class.ind(train_df$target)
train_df <- subset(train_df, select = -target)
str(train_df)
# train_data$Y <- class.ind(train_data$quality)
# train_data <- subset(train_data, select = -quality)
# str(train_data)

#prepping test dataset
test_df_without_quality <- subset(test_data, select = -quality)
test_df_norm <- as.data.frame(scale(test_df_without_quality))
str(test_df_norm)
target <- test_data$quality
test_df <- cbind(test_df_norm, target)
test_df$Y <- class.ind(test_df$target)
test_df <- subset(test_df, select = -target)
str(test_df)

# test_data$Y <- class.ind(test_data$quality)
# test_data <- subset(test_data, select = -quality)
# str(test_data)



as.list(levels(clean_df$quality))
# train neural network model
# nn_model <- nnet(Y ~ ., data=train_data, size=79, softmax = TRUE)
nn_model <- nnet(Y ~ ., data=train_df, size=10, softmax = TRUE)


#testing the result with the test set
test_df[1,]
as.character(as.list(levels(clean_df$quality))[which.max(predict(nn_model,test_df[1,]))])

#success!
predict(nn_model,test_df[1,])

