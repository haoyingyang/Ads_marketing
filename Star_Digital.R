#Star Digital Ads Marketing

setwd("/Users/yanghaoying/Desktop/project/star_digital")

Star_Digital <- read.csv("star_digital.csv",stringsAsFactors=FALSE)

summary(Star_Digital)

# question1
ad_effect<-glm(purchase~test,data=Star_Digital, family=binomial())
summary(ad_effect)

#question2: 

attach(Star_Digital)
Star_Digital$imp_1<-imp_1*test
Star_Digital$imp_2<-imp_2*test
Star_Digital$imp_3<-imp_3*test
Star_Digital$imp_4<-imp_4*test
Star_Digital$imp_5<-imp_5*test
Star_Digital$imp_6<-imp_6*test


# fit.full <- glm(purchase ~ imp_1 + imp_2 + imp_3 + imp_4 + imp_5 + imp_6,
#								data= Star_Digital,family=binomial())
#summary(fit.full)

Star_Digital$rowsums6 <- rowSums(Star_Digital[,4:9])
fit <- glm(purchase ~ rowsums6, data= Star_Digital,family=binomial())
summary(fit)
fit.full <- glm(purchase ~ imp_1+imp_2+imp_3+imp_4+imp_5+imp_6, data= Star_Digital,family=binomial())
summary(fit.full)
# because log(odds) are difficult to interpret, you can exponentiate them
exp(coef(fit.full))
exp(coef(fit))

#question 3
Star_Digital$rowsums5 <- rowSums(Star_Digital[,4:8])	

fit.6 <- glm(purchase ~ imp_6, data = Star_Digital, family=binomial())
summary(fit.6)
exp(coef(fit.6))

fit.rowsums5 <- glm(purchase ~ rowsums5, data = Star_Digital, family=binomial())
summary(fit.rowsums5)
exp(coef(fit.rowsums5))

