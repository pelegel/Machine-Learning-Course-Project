library(psych)
library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
install.packages("corrplot")
library(corrplot)
#install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.9.tar.gz", repo=NULL, type="source") # for specific rlang version, in this case 0.4.9. For latest version can run install.packages("rlang")

 #install.packages("ggplot2")





#arrange in table
filePath=choose.files() 
table<-read.csv(filePath,header=TRUE)


cityRange_price<-boxplot(table$price ~ table$cityPartRange,col = rainbow(ncol(trees)) )


#cityPartRange & price
bpRangePrice<-ggplot(data = table, aes(x=as.character(cityPartRange), y=price)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1', 'deeppink', 'gold', 'mediumspringgreen','mediumpurple3', 'paleturquoise1', 'palevioletred3', 'orangered',  'lightsalmon',  'khaki1'))+ylab('Price')+xlab('City Part Range')
bpRangePrice

#anova
av1 <- aov(table$price~table$cityPartRange)
summary(av1)



#squared meters & price
scatteSMP<-ggplot(table, aes(squareMeters, price)) + 
  geom_point()
scatteSMP

#correlation
correl<-cor.test(table$squareMeters, table$price, method=c("pearson"))
correl


#pool & yard
pl <- ggplot(data = table,aes(x= as.character(hasPool), fill = as.character(hasYard)))
pl <- pl + geom_bar(stat="count")+ xlab("Has Pool") + ylab("Count")+ guides(fill=guide_legend(title="Has Yard"))
pl <- pl  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
pl

#chi squared
chiTest<-chisq.test(table$hasPool, table$hasYard, correct=FALSE)
chiTest



#city part range & has pool
bpRangePrice<-ggplot(data = table, aes(x=as.character(hasPool), y=cityPartRange)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1', 'deeppink'))+ylab('City Part Range')+xlab('Has Pool')+  
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
bpRangePrice

#anova
av2 <- aov(table$cityPartRange~table$hasPool)
summary(av2)


#anova
av3 <- aov(table$category~table$price)
summary(av3)

#anova
av4 <- aov(table$category~table$cityPartRange)
summary(av4)




#pool & luxury
pl <- ggplot(data = table,aes(x= as.character(hasPool), fill = as.character(hasYard)))
pl <- pl + geom_bar(stat="count")+ xlab("Has Pool") + ylab("Count")+ guides(fill=guide_legend(title="Luxury"))
pl <- pl  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
pl



#chi squared
chiTest1<-chisq.test(table$hasPool, table$category, correct=FALSE)
chiTest1

#chi squared
chiTest2<-chisq.test(table$isNewBuilt, table$category, correct=FALSE)
chiTest2

#chi squared
chiTest<-chisq.test(table$hasPool, table$hasYard, correct=FALSE)
chiTest



fit <- lm(table$price ~ table$squareMeters)
summary(fit)



#squareMeters
hist1<-ggplot(table, aes(x=squareMeters, y =(..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  scale_y_continuous(labels = scales::comma)+
  labs(y = "Relative Frequency", x = "squareMeters")
hist1

bp1<-ggplot(data = table, aes(x=as.character(category), y=squareMeters)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="squareMeters", labels = scales::comma)+
  ylab('squareMeters')+xlab('Category')
bp1



#numberOfRooms
hist2<-ggplot(table, aes(x=numberOfRooms, y =(..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  labs(y = "Relative Frequency", x = "numberOfRooms")
hist2

bp2<-ggplot(data = table, aes(x=as.character(category), y=numberOfRooms)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="numberOfRooms", labels = scales::comma)+
  ylab('numberOfRooms')+xlab('Category')
bp2



#hasYard
hist3<-ggplot(table, aes(x=hasYard, y =(..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue", binwidth = 0.3)+
  scale_x_continuous(breaks = seq(0, 1))+
  labs(y = "Relative Frequency", x = "hasYard")
hist3

bp3 <- ggplot(data = table,aes(x= as.character(hasYard), fill = as.character(category)))
bp3 <- bp3 + geom_bar(stat="count")+ xlab("Has Yard") + ylab("Count")+ guides(fill=guide_legend(title="Category"))
bp3 <- bp3  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
bp3


#hasPool
hist4<-ggplot(table, aes(x=hasPool,y =(..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue", binwidth = 0.3)+
  labs(y = "Relative Frequency", x = "hasPool")+
  scale_x_continuous(breaks = seq(0, 1))
hist4

bp4 <- ggplot(data = table,aes(x= as.character(hasPool), fill = as.character(category)))
bp4 <- bp4 + geom_bar(stat="count")+ xlab("Has Pool") + ylab("Count")+ guides(fill=guide_legend(title="Category"))
bp4 <- bp4  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
bp4




#floors
hist5<-ggplot(table, aes(x=floors , y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  labs(y = "Relative Frequency", x = "floors")
hist5

bp5<-ggplot(data = table, aes(x=as.character(category), y=floors)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="floors", labels = scales::comma)+
  ylab('floors')+xlab('Category')
bp5



#cityCode
hist6<-ggplot(table, aes(x=cityCode, y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  labs(y = "Relative Frequency", x = "cityCode")
hist6

bp6<-ggplot(data = table, aes(x=as.character(category), y=cityCode)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="cityCode", labels = scales::comma)+
  ylab('cityCode')+xlab('Category')
bp6


#cityPartRange 
hist7<-ggplot(table, aes(x=cityPartRange , y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
      scale_x_continuous(breaks = seq(0, 10))+  labs(y = "Relative Frequency", x = "cityPartRange")
hist7

bp7<-ggplot(data = table, aes(x=as.character(category), y=cityPartRange )) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="cityPartRange ", labels = scales::comma)+
  ylab('cityPartRange ')+xlab('Category')
bp7


#numPrevOwners 
hist8<-ggplot(table, aes(x=numPrevOwners , y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  scale_x_continuous(breaks = seq(0, 10))+labs(y = "Relative Frequency")
hist8

bp8<-ggplot(data = table, aes(x=as.character(category), y=numPrevOwners )) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="numPrevOwners ", labels = scales::comma)+
  ylab('numPrevOwners ')+xlab('Category')
bp8



#made 
hist9<-ggplot(table, aes(x=made , y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  labs(y = "Relative Frequency")
hist9

bp9<-ggplot(data = table, aes(x=as.character(category), y=made )) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="made ", labels = scales::comma)+
  ylab('made ')+xlab('Category')
bp9



#isNewBuilt
hist10<-ggplot(table, aes(x=isNewBuilt)) +  geom_histogram(color="black", fill="lightblue", binwidth = 0.3)+
  scale_x_continuous(breaks = seq(0, 1))
hist10

bp10 <- ggplot(data = table,aes(x= as.character(isNewBuilt), fill = as.character(category)))
bp10 <- bp10 + geom_bar(stat="count")+ xlab("Is New Built") + ylab("Count")+ guides(fill=guide_legend(title="Category"))
bp10 <- bp10  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
bp10





#isNewBuilt
hist100<-ggplot(table, aes(x=isNewBuilt)) +  geom_histogram(color="black", fill="lightblue", binwidth = 0.3)+
  scale_x_continuous(breaks = seq(0, 1))
hist100

bp100 <- ggplot(data = table,aes(x= as.character(category), fill = as.character(isNewBuilt)))
bp100 <- bp100 + geom_bar(stat="count")+ xlab("Category") + ylab("Count")+ guides(fill=guide_legend(title="Is New Built"))
bp100 <- bp100  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
bp100




#hasStormProtector
hist11<-ggplot(table, aes(x=hasStormProtector)) +  geom_histogram(color="black", fill="lightblue", binwidth = 0.3)+
  scale_x_continuous(breaks = seq(0, 1))
hist11

bp11 <- ggplot(data = table,aes(x= as.character(hasStormProtector), fill = as.character(category)))
bp11 <- bp11 + geom_bar(stat="count")+ xlab("Has Storm Protector") + ylab("Count")+ guides(fill=guide_legend(title="Category"))
bp11 <- bp11  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
bp11


#Basement  
hist12<-ggplot(table, aes(x=basement, y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  labs(y = "Relative Frequency")
hist12

bp12<-ggplot(data = table, aes(x=as.character(category), y=basement)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="Basement  ", labels = scales::comma)+
  ylab('Basement')+xlab('Category')
bp12


#Attic  
hist13<-ggplot(table, aes(x=attic, y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  labs(y = "Relative Frequency")
hist13

bp13<-ggplot(data = table, aes(x=as.character(category), y=attic)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="Attic  ", labels = scales::comma)+
  ylab('Attic')+xlab('Category')
bp13


#Garage  
hist14<-ggplot(table, aes(x=garage, y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  labs(y = "Relative Frequency")
hist14

bp14<-ggplot(data = table, aes(x=as.character(category), y=garage)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="Garage  ", labels = scales::comma)+
  ylab('Garage')+xlab('Category')
bp14


#hasStorageRoom 
hist15<-ggplot(table, aes(x=hasStorageRoom, y = (..count..)/sum(..count..) )) +  geom_histogram(color="black", fill="lightblue", binwidth = 0.3)+
  scale_x_continuous(breaks = seq(0, 1))+  labs(y = "Relative Frequency")
hist15

bp15 <- ggplot(data = table,aes(x= as.character(hasStorageRoom ), fill = as.character(category)))
bp15 <- bp15 + geom_bar(stat="count")+ xlab("Has Storage Room ") + ylab("Count")+ guides(fill=guide_legend(title="Category"))
bp15 <- bp15  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
bp15


#hasGuestRoom   
hist16<-ggplot(table, aes(x=hasGuestRoom , y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
  scale_x_continuous(breaks = seq(0, 10))+  labs(y = "Relative Frequency")
hist16

bp16<-ggplot(data = table, aes(x=as.character(category), y=hasGuestRoom )) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="Has Guest Room   ", labels = scales::comma)+
  ylab('Has Guest Room ')+xlab('Category')
bp16


#Price  
hist17<-ggplot(table, aes(x=price, y = (..count..)/sum(..count..))) +  geom_histogram(color="black", fill="lightblue")+
    labs(y = "Relative Frequency") 
hist17

bp17<-ggplot(data = table, aes(x=as.character(category), y=price)) +
  geom_boxplot(fill=c('steelblue', 'firebrick1'))+
  scale_y_continuous(name="Price  ", labels = scales::comma)+
  ylab('Price')+xlab('Category')
bp17



#category 
hist18<-ggplot(table, aes(x=category, y = (..count..)/sum(..count..))) +  geom_bar(color="black", fill="lightblue")+
  labs(y = "Relative Frequency") 
hist18



table$category=factor(table$category)



disc1 <- ggplot(table, aes(x = squareMeters, y = category, fill = group)) + 
  geom_bar(stat = "identity")
disc1

plot(table$category~table$squareMeters)+
  ylab('Category ')+xlab('s')
plot(table$category~table$numberOfRooms)
plot(table$category~table$floors)
plot(table$category~table$cityCode)
plot(table$category~table$numPrevOwners)
plot(table$category~table$made)
plot(table$category~table$cityPartRange)
plot(table$category~table$basement)
plot(table$category~table$attic)
plot(table$category~table$garage)
plot(table$category~table$price)




M<-cor(table[3:20], use="pairwise.complete.obs")
head(round(M,2))
#Visualize the correlation matrix

# method = "circle""
corrplot(M, method = "circle")

