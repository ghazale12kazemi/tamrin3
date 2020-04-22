library(dplyr)
library(ggplot2)
library(readr)
set.seed(100)

df = read.table("abalone.data",sep = ",")

head(df)

names(df) = c("Sex","Length","Diam","Height","Whole","Shucked","Viscera","Shell","Rings")

df
head(df,2)

summary(df)


#################################
cor(df$Rings,df$Length)

#df$Rings = b0 + b1*df$Length + e

model = lm(Rings~Length,data = df)

summary(model)

plot(df$Rings~df$Length)
abline(model,col = "red")

coeffs = model$coefficients
coeffs[1]
coeffs[2]
ggplot(df,aes(x=Length, y=Rings, color=Sex)) + geom_point() + geom_abline(intercept = coeffs[1], slope = coeffs[2], size = 1.5)
ggplot(df,aes(x=Length, y=Rings, color=Sex)) + geom_point() + geom_smooth(method = "lm")
ggplot(df,aes(x=Length, y=Rings)) + geom_point() + geom_smooth(method = "lm")
ggplot(df,aes(x=Length, y=Rings)) + geom_point() + geom_smooth()

plot(model)

res = data_frame(Res = abs(model$residuals),X = df$Length)
head(res)
summary(res)
model_res = lm(Res~X,data = res)
summary(model_res)
coeffs = model_res$coefficients

ggplot(res,aes(x=X, y=Res)) + geom_point() + geom_abline(intercept = coeffs[1], slope = coeffs[2],color = "red",size = 1.5)
ggplot(res,aes(x=X, y=Res)) + geom_point() + geom_smooth(method = "lm")
coeffs

library(dHSIC)
smpl = sample_n(res,1000)
dim(smpl)
plot(smpl$Res~smpl$X)
test = dhsic.test(smpl$Res,smpl$X)
test$p.value

cor(res)
#---

cor(df)
class(df$Sex)
sapply(df,class)

df_num = df %>% select(-Sex)
head(df_num)
model_mult = lm(Rings~.,df_num)

summary(model_mult)

cor(df_num)
cov(df_num)

var(df_num$Rings)
var(model_mult$residuals)
var(model$residuals)

df_num_adj = df_num %>% select(-Length)

model_mult_adj = lm(Rings~.,df_num_adj)

var(df_num$Rings)
var(model_mult_adj$residuals)

summary(model_mult_adj)
model_mult_adj$coefficients
model_mult$coefficients[I(-2)]

a = names(df)
a

##################

df %>% select(c("Length","Diam","Height","Whole","Shucked","Viscera","Shell","Rings"))
model = lm(df$Rings~df$Length+df$Diam+df$Height+df$Height+df$Shucked+df$Shucked+df$Viscera)
