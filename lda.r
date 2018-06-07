setwd('F:/Prjct/aswanth')

require(MASS)

data <- read.csv('turkiye-student-evaluation_generic.csv')

r <- lda(formula = class ~ .,data = data)

r$prior

r$counts

r$means

r$scaling

r$svd
prop = r$svd^2/sum(r$svd^2)
prop

r2 <- lda(formula = class ~ ., 
          data = data, 
          CV = TRUE)

head(r2$class)

head(r2$posterior, 3)

train <- sample(1:2910, 75)
train
r3 <- lda(class ~ ., # training model
          data)

plda = predict(object = r, # predictions
               newdata = data[-train, ])

head(plda$class)
head(plda$posterior, 3)
head(plda$x, 3)

table(Actual=data$class,Classified=r2$class)
