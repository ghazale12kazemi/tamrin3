library(dplyr)
library(ggplot2)
library(readr)
set.seed(100)

edu <-  read_xls("edu.xls")
names(edu)
levels(as.factor(edu$female))

dum <- c("female", "black" ,"hispanic","dadcoll" , "momcoll",  "ownhome" , "urban", "ed","incomehi")
dum
ggplot(aes(x = bytest,y = ed,colour = black > 0), data = edu)+geom_point()
blackedu <- subset(edu, edu$black == 1)
whiteedu <- subset(edu, edu$black == 0)

linearmodel = lm(edu$bytest~edu$ed)
abline(lm(edu$bytest~edu$ed),col ~ "red")


ggplot(aes(x = bytest,y = ed,colour = female > 0), data = edu)+geom_point()

new_edu <- edu
new_edu %>% select(as.factor(c("female", "black" ,"hispanic","dadcoll" , "momcoll",  "ownhome" , "urban", "ed","incomehi")))


lmodel <- lm(edu$bytest~edu$ed+edu$female+edu$black+edu$hispanic+edu$dadcoll+edu$momcoll+edu$ownhome+edu$urban+edu$cue80+edu$stwmfg80+edu$dist+edu$tuition+edu$incomehi)
summary(lmodel)

install.packages("dummies")
library(dummies)

# example data
dumdist <- as.numeric(edu$dist <= 10)
dumdist
