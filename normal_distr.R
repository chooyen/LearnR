#P(X<=3)
pbinom(q=3,size = 20,prob = 1/6,lower.tail = TRUE)

#to find the 25 percentile
qnorm(p=0.25,mean = 75,sd=5,lower.tail = T)

#To draw a normal distrbution
#x~N(75,5)
#P(>80)
x=seq(from=55,to =95,by=0.25)
y <- dnorm(x,mean = 75,sd=5)
abline(v=75)
polygon(c(x[x>=80], max(x), 80), c(y[x>=80], 0, 0), col=6)

Set.seed(15051)                                              
N <- 1000                                                    
x2 <- rnorm(N, 2)  

plot(density(x2),                                            # Draw density plot
     main = "",                                              # No main title
     xlab = "x2")                                            # Set name of x-axis to x2

polygon(c(min(density(x2)$x), density(x2)$x),                # X-Coordinates of polygon
        c(0, density(x2)$y),                                 # Y-Coordinates of polygon
        col = "2")                                           # Color of polygon

#One-Sample t Test & Confidence Interval in R

LungCapData <- read.csv(file.choose(),header = TRUE)
hist(LungCap)

#Ho:mu<8
#one-sided 95% CI

t.test(LungCap,mu=8, alternative = "less",conf.level = 0.95)

#Ho:mu=8
#two-sided 95% CI

test <- t.test(LungCap,mu=8, alternative = "two.sided",conf.level = 0.95)

attributes(test)
test$p.value

#Two-Sample t Test & Confidence Interval in R
boxplot(LungCap~Smoke)

#Ho: mean lung cap of smoker =of non smoker
#two-sided test
#assume non equal variance

t.test(LungCap~Smoke,mu=0, alternative="two.sided",conf.level=0.95,
       var.eq=FALSE,paired=FALSE)

#use variance to compare
var(LungCap[Smoke=="yes"])

var(LungCap[Smoke=="no"])   #two times for for non-smoker

#Levene Test
#contain in car package
#Ho; Population variance are equal

library(car)

leveneTest(LungCap~Smoke)
