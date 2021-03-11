library(neuralnet)
library(NeuralNetTools)
library(caret)
library(plyr) 
library(boot)
library(ISLR)


max = apply(Total_Test_Skripsi , 2 , max)
min = apply(Total_Test_Skripsi , 2 , min)
scaled = as.data.frame(scale(Total_Test_Skripsi, center = min, scale = max - min))

nn = neuralnet(formula = USF ~ RA + PC + A + D, data = scaled, learningrate = 0.25,
               hidden = 8, err.fct = "sse", linear.output = FALSE)

nnx = neuralnet(formula = ITU ~ USF + D, data = scaled, learningrate = 0.25,
                hidden = 7, err.fct = "sse", linear.output = FALSE)

plot(nn)
plot(nnx)
garson(nn)
garson(nn, bar_plot = FALSE)
garson(nnx)
garson(nnx, bar_plot = FALSE)
plotnet(nn)

cv.rmse.test.ITU = NULL
cv.rmse.train.ITU = NULL
cv.rmse.test.USF = NULL
cv.rmse.train.USF = NULL
k <- 10
for(i in 1:k){
  index <- sample(1:nrow(scaled),round(0.9*nrow(scaled)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  pr.nn.test.usf <- compute(nn,test.cv)
  pr.nn.test.usf <- pr.nn.test.usf$net.result
  pr.nn.train.usf <- compute(nn,train.cv)
  pr.nn.train.usf <- pr.nn.train.usf$net.result
  cv.rmse.test.USF[i] <- sqrt(sum((test.cv$USF - pr.nn.test.usf)^2)/nrow(test.cv))    
  cv.rmse.train.USF[i] <- sqrt(sum((train.cv$USF - pr.nn.train.usf)^2)/nrow(train.cv))  
}

for(i in 1:k){
  index <- sample(1:nrow(scaled),round(0.9*nrow(scaled)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  pr.nn.test.itu <- compute(nnx,test.cv)
  pr.nn.test.itu <- pr.nn.test.itu$net.result
  pr.nn.train.itu <- compute(nnx,train.cv)
  pr.nn.train.itu <- pr.nn.train.itu$net.result
  cv.rmse.test.ITU[i] <- sqrt(sum((test.cv$ITU - pr.nn.test.itu)^2)/nrow(test.cv))    
  cv.rmse.train.ITU[i] <- sqrt(sum((train.cv$ITU - pr.nn.train.itu)^2)/nrow(train.cv))  
}

library(openxlsx)

df = data.frame(cv.rmse.train.ITU, cv.rmse.test.ITU, cv.rmse.train.USF, cv.rmse.test.USF)
write.xlsx(df, "C:/Users/Bayu/Documents/R/new cv nn.xlsx")


