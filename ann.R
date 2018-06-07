setwd('F:/Prjct/aswanth')
set.seed(500)

data <- read.csv('turkiye-student-evaluation_generic.csv')

data = data[order(data$class),]

View(data)

apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))

train <- data[index,]

test <- data[-index,]

lm.fit <- glm(instr~., data=train)

summary(lm.fit)

pr.lm <- predict(lm.fit,test)

MSE.lm <- sum((pr.lm - test$instr)^2)/nrow(test)

maxs <- apply(data, 2, max)

mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]

test_ <- scaled[-index,]

require("neuralnet")

n <- names(train_)

n

f <- as.formula(paste("instr ~", paste(n[!n %in% "instr"], collapse = " + ")))

f

nn <- neuralnet(f,data = train_, hidden=1,linear.output=TRUE)

plot(nn)

pr.nn <- compute(nn,test_[,1:32])

predicted_G3 <- pr.nn$net.result

cor(predicted_G3, test_$class)[ , 1]

plot(predicted_G3, test_$class, 
     main = "1 hidden node layers", ylab = "Real G3")  # line em up, aid visualisation
abline(a = 0, b = 1, col = "black") 



net_model2 <- neuralnet(f,
                        data = train_, hidden = 3, linear.output = FALSE)
print(net_model2)

plot(net_model2)

model_results2 <- compute(net_model2, test_[,1:32])
predicted_G3_2 <- model_results2$net.result

cor(predicted_G3_2, test_$instr)[ , 1]  # can vary depending on random seed

plot(predicted_G3_2, test_$instr,
     main = "5 hidden node layers", ylab = "Real G3")  # line em up, aid visualisation
abline(a = 0, b = 1, col = "black") 

net <- neuralnet(f, data, err.fct="sse", linear.output=FALSE, hidden=c(4,4,4))
plot(net)

table(Actual=data$instr,instrified=net$response)