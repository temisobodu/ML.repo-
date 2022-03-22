#decision tree

attach(ml_dataset_survival)
install.packages("FSelector")
install.packages("rpart")
install.packages("caret", dependencies = T)
install.packages("rpart.plot")
install.packages("xlsx")
install.packages("data.tree")
install.packages("caTools")
?caret
??caret

library(FSelector)
sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0-25\\Jre")
system("/usr/libexec/java_home", intern = TRUE)
??Java
??rJava
install.packages("rJava")
library(rJava)
library(dpylr)
install.packages("glmulti")
df = (ml_dataset_survival)
df = select(df, survived, pclass, sex, age)
df = mutate(df, survived = factor(survived), class = as.numeric(pclass), age = as.numeric(age))
df
View(df)


set.seed(123)
sample = sample.split(df$survived,SplitRatio= .7)
sample

train11 = subset(df, sample ==T)
test11 = subset(df, sample == F)

#training the decision tree classifier

library(rpart)
tree = rpart(survived~., data = train11)
tree
#predictions

tree.survived.pred = predict(tree, test11, type = 'class')
tree.survived.pred

#confusionmatrix
??confusionmatrix
library(caret)
require(caret)
confusionMatrix(tree.survived.pred,test11$survived)

#visualize decisiontree

library(rpart.plot)
require(rpart.plot)

prp(tree)

#visualization in 2D

test12 = test11[,c(2,4,1)]
test12

set = test12
x1 = seq(min(set[,1]) -1,max(set[,1]) +1, by =0.1)
x2 = seq(min(set[,2], na.rm = T) -1,max(set[,2], na.rm = T) +1, by =0.1)
x1
min(set[,1])
max(set[,1])
grid_set = expand.grid(x1,x2)
grid_set
colnames(grid_set) = c('pclass', 'age')
y_grid = tree.survived.pred
plot(set[,-3], main = 'decision tree classifier',
     xlab ='class', ylab = 'age',
     xlim =range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1),length(x2), add =T)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen", "tomato"))
points(set, pch =21, bg = ifelse(set[,3] ==1, "green4", "red3"))
