library(np)
library(ggplot2)
install.packages("Formula")
library(Formula)
install.packages("hydroGOF")
library(hydroGOF)
# load all the packages needed

set.seed(1)
# set seed to 1 to let you see the same random stimulations as we did

# the true model is y=x^3 + epsilon
sim_f <- function(x)
{
  x*x*x
}  

# simulate 150 outcomes respectively for x and e from the normal distribution and then generate y based on the x and e stimulated and the true function 
n <- 1
sim_x <- vector(length = 150)
sim_y <- vector(length = 150)
sim_e <- vector(length = 150)
for (n in 1:150 ) {
  sim_x[n]<-rnorm(1)
  sim_e[n]<-rnorm(1)
  sim_y[n]<-sim_f(sim_x[n])+sim_e[n]
  n<-n+1
}

sim <- cbind(sim_x,sim_y,sim_e)
sim <- data.frame(sim)
sim

# separate the simulations into two sets: a training set and a testing set
sim_train <- slice(sim,1:120)
Signal <- rep(1,times=120)
sim_train<- mutate(sim_train,Signal)
sim_test <- slice(sim,121:150)
Signal <- rep(0,times=30)
sim_test <- mutate(sim_test,Signal)
sim <- union(sim_test,sim_train)
sim_train
sim_test
sim


# train a linear model on the training data
lm.fit <- lm(sim_y ~ sim_x ,data = sim_train)
summary(lm.fit)

# train a low (high) flexibility model of bandwidth = 0.5 (0.01) based on the training data.
# predict y with the low (high) flexibility model trained based on the x in training data.
ll.fit.lowflex <- npreg(sim_y ~ sim_x, bws = 0.5, data = sim_train, method = "ll" )
summary(ll.fit.lowflex)
predictionsl <- predict(ll.fit.lowflex)
yhatl1 <- data.frame(predictionsl)


ll.fit.highflex <- npreg(sim_y~sim_x, bws = 0.01, data = sim_train, method = "ll")
summary(ll.fit.highflex)
predictionsh <- predict(ll.fit.highflex)
yhath1 <- data.frame(predictionsh)

# gather the simulations and the predicted values in the same data frame and plot them on the same graph
new <- mutate(sim_train, yhath = yhath1$predictionsh, yhatl = yhatl1$predictionsl)
new
ggplot(data = new)+
  geom_point(mapping = aes(x=sim_x, y=sim_y))+
  geom_line(mapping = aes(x=sim_x, y=predictionsh), color = "blue")+
  geom_line(mapping = aes(x=sim_x, y=predictionsl), color = "red")+
  geom_line(mapping = aes(x=sim_x, y=sim_x^3),color = "black")

# perform similar procedures (predicting with the two models, gathering the data and ploting them on the same graphs)
predictionsltest <- predict(ll.fit.lowflex, newdata = sim_test)
yhatl1test <- data.frame(predictionsltest)
predictionshtest <- predict(ll.fit.highflex, newdata = sim_test)
yhath1test <- data.frame(predictionshtest)
newtest <- mutate(sim_test, yhathtest = yhath1test$predictionshtest, yhatltest = yhatl1test$predictionsltest)
newtest
ggplot(data = newtest,aes(x=sim_x)) +
  geom_point(mapping = aes(y=sim_y))+
  geom_line(mapping = aes(y=yhatltest), color = "red")+
  geom_line(mapping = aes(y=sim_x^3), color = "black")+
  geom_line(mapping = aes(y=yhathtest), color = "blue")

# create a vector containing values from 0.01 to 0.5 increasing by 0.001 at each time 
bandwidth <- seq(from = 0.01, to = 0.5, by = 0.001)
bandwidth


a <- list()
for(i in seq(0.01,0.5,0.001)){
  a <- list(a, npreg(sim_y~sim_x, bws = i, data = sim_train ,regtype = "ll" ))
}
a


bw <- seq(0.01:0.5,by=0.001)  

a <- c()
for(i in seq(0.01,0.5,0.001)){
  a <- c(a, npreg(sim_y~sim_x, bws = i, data = sim_train ,regtype = "ll" ))
}
a

x <- 0.01
p <- 1
MSEtrain <- c(1:491)
MSEtest <- c(1:491)
for(x in bw){
  model <- npreg(bws = x,data = sim_train,xdat= sim_train$sim_x,ydat = sim_train$sim_y,method = 'll')
  fit <- fitted(model)
  predtest <- predict(model, newdata = sim_test)
  
n <- 1
s <- 0
u <- 0
m <- 1
for (n in 1:120) {
  s <- (fit[n]-sim_train$sim_y[n])^2+s
  n <- n+1
}
for (m in 1:30) {
  u <- (predtest[m]-sim_test$sim_y[m])^2+u
  m <- m+1
}
MSEtrain[p] <- s/120
MSEtest[p] <- u/30
x <- x+0.001
p <- p+1

}

M <- cbind(bw,MSEtrain,MSEtest)
M <- as.data.frame(M)
tbl_df(M)

### step10

ggplot(data = M)+
  geom_line(mapping = aes(x=bw,y=MSEtrain,color='blue'))+
  geom_line(mapping = aes(x=bw,y=MSEtest,color='yellow'))




