setwd('C:/Users/Magha/OneDrive/Documents/R/Machine Learning HW/energy_dataset.csv')

data1 <- read.csv("energy_dataset.csv")

library(tidyr)
data1 <- data1 %>% drop_na()

data <- data1
data <- data[-c(1,7)]
data <- data %>% drop_na()

#data<-data[1:500,]


for (i in 1:9){
  #print(i)
  data[,i]<-(data[i]-min(data[i]))/(max(data[i])-min(data[i]))
}

ind <- sample(1:nrow(data),500 )
train_data <-data[ind,]
test_data<- data[-ind,]

#install.packages('neuralnet')
library(neuralnet)

sapply(data,class)
#increase hidden layers smaller is better
n <- neuralnet(price.actual~generation.fossil.gas+generation.fossil.hard.coal+generation.fossil.oil+generation.nuclear+
              generation.solar+total.load.forecast+total.load.actual+price.day.ahead, data=train_data, hidden = c(5), 
              linear.output = FALSE)

plot(n)

output <- compute(n,train_data[,-9])
prediction <- output$net.result * (max(data1[-ind,9])- min(data1[-ind,9])) + min(data1[-ind, 9])
actual <- train_data[ 9]

MSE <- sum((prediction-actual)^2)/nrow(train_data)

train_data$fitted<-prediction

train_data$fitted
print(MSE)

#Smaller MSE is better
