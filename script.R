library(neuralnet)
library(NeuralNetTools)
library(plyr) 
library(boot)
library(ISLR)
library(readxl)

Total_Test_Skripsi <- read_excel("C:/Users/Bayu/Downloads/Total Test Skripsi.xlsx")

#Scaling Data
max = apply(Total_Test_Skripsi , 2 , max)
min = apply(Total_Test_Skripsi , 2 , min)
scaled = as.data.frame(scale(Total_Test_Skripsi, center = min, scale = max - min))

#Modelling
nnx = neuralnet(formula = ITU ~ USF + D, data = scaled, learningrate = 0.25,
                hidden = 7, err.fct = "sse", linear.output = FALSE)


plot(nnx)
garson(nnx)
garson(nnx, bar_plot = FALSE)


#K-Fold Cross Validation
cv.rmse.test.ITU = NULL
cv.rmse.train.ITU = NULL
k = 10
for(i in 1:k){
  index = sample(1:nrow(scaled),round(0.9*nrow(scaled)))
  train.cv = scaled[index,]
  test.cv = scaled[-index,]
  pr.nn.test.itu = compute(nnx,test.cv)
  pr.nn.test.itu = pr.nn.test.itu$net.result
  pr.nn.train.itu = compute(nnx,train.cv)
  pr.nn.train.itu = pr.nn.train.itu$net.result
  cv.rmse.test.ITU[i] = sqrt(sum((test.cv$ITU - pr.nn.test.itu)^2)/nrow(test.cv))    
  cv.rmse.train.ITU[i] = sqrt(sum((train.cv$ITU - pr.nn.train.itu)^2)/nrow(train.cv))  
}

#Export hasil validasi model
library(openxlsx)
df = data.frame(cv.rmse.train.ITU, cv.rmse.test.ITU)
write.xlsx(df, "C:/Users/Bayu/Documents/R/new cv nn.xlsx")


