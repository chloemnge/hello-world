# TP2 Apprentissage Statistique
library(class)
data(iris)
head(iris)
summary(iris)
  
  # separation des donnees
train = iris[c(1:30,51:80,101:130),1:5]
test = iris[c(31:50,81:100,131:150),1:5]
  # prediction avec algo k-nn
pred = knn(train[,1:4], test[,1:4], train[,5], k = 3)
  # display the confusion matrix
table(pred,test[,5])

# 5-fold cross-validation to select k
# from the set {1,...,10}
fold = sample(rep(1:5,each=18)) # creation des groupes B_v
cvpred = matrix(NA,nrow=90,ncol=10) # initialisation de la matrice des prédicteurs
for (k in 1:10)
  for (v in 1:5)
  {
    sample1 = train[which(fold!=v),1:4]
    sample2 = train[which(fold==v),1:4]
    class1 = train[which(fold!=v),5]
    cvpred[which(fold==v),k] = knn(sample1,sample2,class1,k=k)
  }
class = as.numeric(train[,5])
# display misclassification rates for k=1:10
apply(cvpred,2,function(x) sum(class!=x)) # calcule l'erreur de classif