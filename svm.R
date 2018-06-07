setwd('F:/Prjct/aswanth')

# use e1071 for svm
library("e1071")

data <- read.csv('turkiye-student-evaluation_generic.csv')

attach(data)

x <- subset(data, select=-class) #excluding instr

y <- class

#Create SVM Model and show summary .... ....
svm_model <- svm(class ~ ., data=data)
summary(svm_model)

#Run Prediction and you can measuring the execution time in R .... ....
pred <- predict(svm_model,x)
system.time(pred <- predict(svm_model,x))

#See the confusion matrix result of prediction .... ....
c(class(pred),class(y))

table(pred,y)

#Tuning SVM to find the best cost and gamma .... ....
# svm_tune <- tune(svm, train.x=x, train.y=y, 
#                  kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune <-  tune(svm, class~., data = data, 
                  ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
                  tunecontrol = tune.control(sampling = "fix"))

best_mod = svm_tune$best.model
best_mod

ypred <- predict(best_mod, data)

misclass <- table(predict = ypred, truth = data$class)
misclass
