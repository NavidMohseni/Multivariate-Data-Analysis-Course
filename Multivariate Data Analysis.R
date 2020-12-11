#Multivariate Analysis



#PCA
#It is just for continous

library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggcorrplot)
library(dplyr)
library(psych)


princomp(x, cor = T, scores = T)
plot()
summary()
screeplot(pca, type = "l")
#OR

pca <- PCA(deca, scale.unit = TRUE)
pca
pca$eig
#Most corr Variables with the first 2 pc
dimdesc(cars.pca, axes = 1:2)
summary(pca)
#Scree plot or eigens more than 1
fviz_screeplot(pca, addlabels = T, linecolor = "salmon4")

cars.cor <- cor(cars, use = "complete.obs")
cars.cor
cor_viz <- ggcorrplot(cars.cor)
cor_viz
viz_pca_ind(cars.pca)
fviz_pca_var(cars.pca, col.var = "contrib", repel = TRUE)
cars$cyl <- as.factor(cars$cyl)
fviz_pca_ind(cars.pca, habillage = cars$cyl, addEllipses = TRUE, repel = TRUE)
fviz_contrib(cars.pca, choice = "var", axes = 2, top = 8)

?fviz_pca_biplot
fviz_pca_biplot(cars.pca)
fviz_pca_biplot(cars.pca, axes = 1:2)
fviz_screeplot(cars.pca, addlabels = T)








#EFA



efa_cor <- cor(data, use = "pairwise.complete.obs")
scree(efa_cor)
cortest.bartlett()
KMO()
EFA <- fa(nfactors = )
#TLI more than .9 is good
#We can do with 2 fa with different nfactors and then compare the BIC (low BIC is better)
EFA$loadings
EFA$scores
fa.parallel(x = corr , n.obs = 12 , fm ="mle", fa ="fa")
fa.diagram()









#Multivariate


library(mvtnorm)
library(MVN)
rmvnorm(sample, mean, sigma = matrix)
qmvnorm(p = .95, tail = "both", mean, sigma)





#CFA

library(lavaan)

#no comma, just space
#   =~ means create latent Variables
#   ~~ means covariance between variables ( if both side is the same variable thus it is variance)
        #it is often use for residuals
#   ~  means create direct prediction btw variables
HS.model <- ' visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
#exogenous latent variables are correlated by default
fit <- cfa(model = HS.model, data=HolzingerSwineford1939)
summary(fit, standardized = TRUE, fit.measures=TRUE)
#std.all near 1 is strong relationship (More than .3 is good)
#TLI and CFI more than .9 also RMSEA and SRMR < .1 is good
# old ~~ 0*new makes covariance = 0

parTable(fit)
#if goodness of fits was not good we can use modification and find largest mi and use it in model
modificationindices(modelfit, sort = TRUE)

anova(fit1, fit2)
fitmeasures(fit, c("aic", "ecvi")) # lower better 


#SEM



library(foreign)
read.spss("C:/Users/Asus/Desktop/hoosh.sav")
Data <- read.spss("C:/Users/Asus/Desktop/hoosh.sav")
library(lavaan)
library(semPlot)

iq.model <- 'MC =~ mc1 + mc2 + mc3 + mc4
CO =~ cog1 + cog2 + cog3 + cog4
MO =~ mot1 + mot2 + mot3 + mot4
BH =~ beh1 + beh2 + beh3 + beh4
MC ~~ CO
MC ~~ MO
MC ~~ BH
CO ~~ MO
CO ~~ BH
MO ~~ BH'
Data1 <- as.data.frame(Data)
iq.model.fit <- cfa(model = iq.model, data = Data1)
summary(iq.model.fit, standardized = TRUE, fit.measures = TRUE)
# or lavaan()
semPaths(object = iq.model.fit, layout = "tree", rotation = 1, 
         whatLabels = "std", edge.label.cex = 1, what = "std", edge.color = "blue")






#Discrimination

library(MASS)
library(car)
library(rattle)
data(wine)
attach(wine)
head(wine)
#lda(categorical ~ . , data = )
#qda() for quadratic discriminant
wine.lda <- lda(Type ~ ., data=wine)
wine.lda

wine.lda.values <- predict(wine.lda)
wine.lda.values
ldahist(data = wine.lda.values$x[,1], g=Type)
ldahist(data = wine.lda.values$x[,2], g=Type)

#predict(lda output,newdata=data.frame(X1=3.21,X2=497))
library(klaR)
partimat(Type ~.,data=adm,method="lda")



zlin=lda(Default~.,data = cred1)
# Confusion Matrix:  #Default is our categorical
table(predict(zlin)$class, Default)














#Regression Trees
library(rpart)
#rpart(formula, data=, method=,control=) where
#formula	is in the format 
#outcome ~ predictor1+predictor2+predictor3+ect.
#data=	specifies the data frame
#method=	"class" for a classification tree 
#"anova" for a regression tree
#control=	optional parameters for controlling tree growth. For example, control=rpart.control(minsplit=30, cp=0.001) requires that the minimum number of observations in a node be 30 before 

#prune(fit , cp = )

fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for Kyphosis")

# Random Forest prediction of Kyphosis data
library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fit) # view results 
importance(fit) # importance of each predictor











#Kmeans


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
k2 <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(k2, data = df)
fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

fviz_nbclust(df, kmeans, method = "wss")















#Gee Analysis
library(geepack)
library(gee)
btb_gee <- gee(bdi ~ bdi.pre + trt + length + drug,
               + data = BtheB_long, id = subject, family = gaussian,
               + corstr = "independence")
summary(btb_gee)







#Path Analysis
source('http://openmx.psyc.virginia.edu/getOpenMx.R')
library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(foreign)
data <- read.spss("C:\\Users\\Asus\\Desktop\\Hypothermia.sav")
data.frame(data)
str(data)
cov(data)
dat<-lav_matrix_lower2full(c(1,.47,1,.66,.50,1,.77,.41,.46,1,.52,.38,.50,.50,1))
rownames(dat)<-colnames(dat)<-c("att",'sn','pbc','int','beh')
model<-' int~att+sn+pbc
         beh~int+pbc'
summary(model)

model <- 'GA ~ apgar + weight
          twin ~ weight
          weight ~ Body.Temp + apgar
          Body.Temp ~ apgar'
data1 <- data.frame(data)
data1
iq.model.fit <- cfa(model = model, data = data1)
summary(iq.model.fit, standardized = TRUE, fit.measures = TRUE)  











#Profile Analysis

library(ProfileR)
#item1 item2 item3 item4 spouse
#1 2 3 5 5 Husband
#2 5 5 4 4 Husband
#3 4 5 5 5 Husband
#4 4 3 4 4 Husband
#5 3 3 5 5 Husband
#6 3 3 4 5 Husband
mod <- pbg(spouse[, 1:4], spouse[, 5], labels = FALSE,
           profile.plot = TRUE)
summary(mod)
paos(nutrient, scale = TRUE)
results<-sem(model,sample.cov=dat,sample.nobs=131)
summary(results,standardized=T,fit=T,rsquare=T)


