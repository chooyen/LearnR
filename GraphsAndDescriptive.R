 #Read data into dataframe
LungCapData <- read.csv(file.choose(),header = TRUE)
attach(LungCapData)
count <- table(Gender)
kableExtra::kable(count)
count/725
percent <- count/725

#Creating barplot
barplot(percent,main = "Title",xlab="%",ylab="gender",las=1,
        names.arg = c("Female","Male"),horiz = TRUE)

barplot(percent,main = "Title",xlab="%",ylab="gender",las=1,
        names= c("Female","Male"),horiz = TRUE,col = "red")

#Creating piechart
pie(count,main = "Title");box()

#Creating boxplot
boxplot(LungCap~Gender+Smoke,main="Lung Capacity",ylab="Capacity",
        ylim=c(0,16),las=1)

quantile(LungCap,probs = c(0,0.25,0.5,1))

#Another way to plot 
boxplot(LungCap[Gender=="female"],LungCap[Gender=="male"],
        main="Lung Capacity",ylab="Capacity",names = c("Female","Male"),
        ylim=c(0,16),las=1)

#Boxplot with two factors
AgeGroup <- cut(Age,breaks = c(0,13,15,17,25), 
                labels =c("<13","14/15","16/17","18+") )

boxplot(LungCap~Smoke,main="Lung Cap Vs Smoker",ylab="Capacity",
        ylim=c(0,16),las=1)

#Boxplot of Age more than 18 and smokers > 18
boxplot(LungCap[Age>=18]~Smoke[Age>=18],main="Lung Cap Vs Smoker",ylab="Capacity",
        ylim=c(0,16),las=1)

#Boxplot of lungCap based on Smoker & AgeGroup
boxplot(LungCap~Smoke*AgeGroup,main="Lung Cap Vs Smoker",
        ylab="Capacity vs Smoke, by Age Group",
        ,las=2, col=c(4,2))
box()

axis(2, at=seq(0,20,2),seq(0,20,2),las=1)

#Modify and customise plot- Size
plot(Age,Height,main="Ht against Age", cex=0.5, cex.main=2,
     cex.lab=1.5, cex.axis=0.7)

#Modify and customise plot- Size
plot(Age,Height,main="Scatter",font.main=3, font.lab=2,font.axis=3)

#Modify and customise plot- Color
plot(Age,Height,main="Scatter",col= 2, col.main= 2, col.lab=2,col.axis=3)

#Modify and customise plot- character
plot(Age,Height,main="Scatter",pch=2)

#Add a regression line
abline(lm(Height~Age),col=2,lty=2,lwd=6)

#Identify gender on the same plot using plotting character
plot(Age[Gender=="male"],Height[Gender=="male"],col=4,pch="m",
     main="Height Vs Age", xlab="Age", ylab ="Height")

points(Age[Gender=="female"],Height[Gender=="female"],col=6,pch="f")

#split into different plot
par(mfrow=c(1,2))
plot(Age[Gender=="male"],Height[Gender=="male"],
     main="Height Vs Age for males", xlab="Age", ylab ="Height",
     xlim=c(0,20),ylim = c(45,85))

plot(Age[Gender=="female"],Height[Gender=="female"],
     main="Height Vs Age for female", xlab="Age", ylab ="Height",
     xlim=c(0,20),ylim = c(45,85))

#Relabeling axis
par(mfrow=c(1,1))

plot(Age,Height, main="TITLE",axes=F)
axis(side = 1,at=c(7,12.3,15),labels = c("sev","median","15"))

axis(side = 2,at=c(55,65,75),labels = c("55","65","75"),las=2)
box()

#Adding Legends
plot(LungCap[Smoke=="no"]~Age[Smoke=="no"],main="scatter plot",col=4,
     xlab="Age",ylab = "LungCap",pch=16)
points(LungCap[Smoke=="yes"]~Age[Smoke=="yes"],col=2,pch=17)
 legend(x=3.5,y=14,adj = 0,legend = c("Non-smoker","Smoker"),
        col=c(4,2),pch=c(16,17),bty = "n")