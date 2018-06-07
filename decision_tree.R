setwd('F:/Prjct/aswanth')

data <- read.csv('turkiye-student-evaluation_generic.csv')

library(OneR)
model <- OneR(data, verbose = TRUE)

data.frame(data)

library(plyr)

n <- names(data)

n

f <- as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))

f

apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))


maxs <- apply(data, 2, max)

mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))


train_ <- scaled[index,]

test_ <- scaled[-index,]

count(data$class)

# data$class[data$class == "VG"] <- "Good"
# count(data$class)
# 
# data$class[data$class < 3] <- "Bad"
# count(data$class)
# 
# data$class[data$class == 3] <- "Fair"
# count(data$class)
# 
library(rpart)
library(rpart.plot)
str(data)

# Step1: Begin with a small cp. 
set.seed(123)
tree <- rpart(class~ ., data=train_, control = rpart.control(cp = 0.0001))

rpart.plot(tree)

printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)

conf.matrix <- table(train_$class, predict(tree.pruned))

predictions = predict(tree, test_)
table(predictions, test_$class)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)


plot(tree.pruned)
text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)

tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=tot_count)

only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[tree.pruned$frame$yval]

par(xpd=TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)