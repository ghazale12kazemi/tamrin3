library(dplyr)
library(ggplot2)
library(readr)
set.seed(100)

houses <- read.csv("houses.csv")

summary(houses)
class(houses)
dim(houses)

classes <- sapply(houses, class)
classes

length(colnames(houses))

colnames(houses)

new_houses = houses

new_houses %>% select_if(is.numeric)
new_houses %>% select(c("price","area","age","district","percent","totalPrice","region"))
new_houses<- dplyr::select ( houses,price,area,age,district,percent,totalPrice,region)
new_houses
summary(new_houses) 
sapply(new_houses, function(new_houses) c( "Stand dev" = sd(new_houses), 
                         "Mean"= mean(new_houses,na.rm=TRUE),
                         "n" = length(new_houses),
                         "Median" = median(new_houses),
                         "CoeffofVariation" = sd(new_houses)/mean(new_houses,na.rm=TRUE),
                         "Minimum" = min(new_houses),
                         "Maximun" = max(new_houses),
                         "Upper Quantile" = quantile(new_houses,1),
                         "LowerQuartile" = quantile(new_houses,0)
)
)

with(new_houses,plot(area,totalPrice))
plot(log(new_houses$totalPrice), log(new_houses$area))
hist(new_houses$totalPrice)
boxplot(new_houses$totalPrice, horizontal = T)

par(mfrow=c(2,3))
hist(new_houses$price)
hist(new_houses$area)
hist(new_houses$age)
hist(new_houses$district)
hist(new_houses$percent)
hist(new_houses$totalPrice)


houses_rm <- houses
#area
houses_rm <- subset(houses_rm, (houses_rm$area < mean(houses_rm$area) + 3 * sd(houses_rm$area)) & (houses_rm$area > mean(houses_rm$area) - 3 * sd(houses_rm$area)))
#totalPrice
houses_rm <- subset(houses_rm, (houses_rm$totalPrice < mean(houses_rm$totalPrice) + 3 * sd(houses_rm$totalPrice)) & (houses_rm$totalPrice > mean(houses_rm$totalPrice) - 3 * sd(houses_rm$totalPrice)))
#price
houses_rm <- subset(houses_rm, (houses_rm$price < mean(houses_rm$price) + 2 * sd(houses_rm$price)) & (houses_rm$price > mean(houses_rm$price) - 2 * sd(houses_rm$price)))
#age
houses_rm <- subset(houses_rm, (houses_rm$age < mean(houses_rm$age) + 2 * sd(houses_rm$age)) & (houses_rm$age > mean(houses_rm$age) - 2 * sd(houses_rm$age)))
#district
houses_rm <- subset(houses_rm, (houses_rm$district < mean(houses_rm$district) + 2 * sd(houses_rm$district)) & (houses_rm$district > mean(houses_rm$district) - 2 * sd(houses_rm$district)))

dim(houses_rm) 
par(mfrow=c(3,4))

hist(houses_rm$price)
boxplot(houses_rm$price, horizontal = T)

hist(houses_rm$area)
boxplot(houses_rm$area, horizontal = T)

hist(houses_rm$age)
boxplot(houses_rm$age, horizontal = T)

hist(houses_rm$district)
boxplot(houses_rm$district, horizontal = T)

hist(houses_rm$totalPrice)
boxplot(houses_rm$totalPrice, horizontal = T)

plot(houses_rm$area,houses_rm$price)

pairs(houses_rm)

cor(houses_rm$area, houses_rm$price)
plot(houses_rm$area,houses_rm$price)

cor(houses_rm$age, houses_rm$price)
plot(houses_rm$age,houses_rm$price)

cor(houses_rm$district, houses_rm$price)
plot(houses_rm$district,houses_rm$price)

#district,-0.6003424

linearmodel = lm(houses_rm$price~houses_rm$district)
linearmodel

hp_price = -0.406 *houses_rm$district + 9.591
hp_price

par(mfrow=c(2,1))
plot(houses_rm$price,houses_rm$district)
abline(lm(houses_rm$price~houses_rm$district),col('blue'))
plot(hp_price,houses_rm$district)
abline(lm(houses_rm$price~houses_rm$district),col ~ "red")

houseLm <- lm(houses_rm$price~houses_rm$area+houses_rm$age+houses_rm$district)
summary(houseLm)

install.packages("coefplot")
library(coefplot)
coefplot(houseLm)
head(fortify(houseLm))

residPlot <- ggplot(aes(x = .fitted, y=.resid), data = houseLm) + geom_point()+ geom_hline(yintercept = 0)+labs(x = "Price value",y = "Residual")
residPlot
