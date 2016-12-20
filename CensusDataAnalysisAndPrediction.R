##DATA ANALYSIS



##Loading Census 2011 dataset into RStudio
cens11<-read.csv("Final_Census_2011_Part2.csv")

##Making changes in the dataset in order to analyse the data and depict the various factors graphically

## 1  BAR graph for POPULATION of all districts using table abc and abc1
    ##Loading TOT_P,TOT_M and TOT_F from cens11 to abc table

abc<-cens11[,2:6]
    ##Melting the loaded data abc into new table abc1

abc1<-melt(abc,id.vars=c("cens11.Name"))

ggplot(abc1,aes(cens11.Name,value))+
geom_bar(aes(fill=variable),stat="identity",position="dodge")


## 2  pie chart for population of all districts LITERACY RATE (ABC2)

    ##New table created abc2 which includes LIT_P,LIT_M and LIT_F

bp<- ggplot(abc2, aes(x="", y=P_LIT, fill=Name))+
geom_bar(width=1,stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie

    ## pie chart for all districts literacy (MALE) (ABC2)
bp1 <- ggplot(abc2, aes(x="", y=M_LIT, fill=Name))+
geom_bar(width = 1, stat = "identity")
pie1<- bp1 + coord_polar("y",start=0)
pie1

    ## pie chart for districts literacy (FEMALE) (ABC2)
bp2 <- ggplot(abc2, aes(x="", y=F_LIT, fill=Name))+
geom_bar(width = 1, stat = "identity")
pie2<- bp2 + coord_polar("y",start=0)
pie2

##3.  line graph for EMPLOYMENT(Types of Workers Mainworkers,MarginalWOrkers) by creating new table abc3

    ##Melting table abc3 to obtain abc4
abc4<- melt(abc3, id="Name")

    ##Creating Line Graph
lp<-ggplot(data=abc4,aes(x=Name, y=value, colour=variable))+geom_line(aes(group=variable))
lp

##4.SEX RATIO no of males wrt to 1000 females

abc5<-cens11[,2:8]

##Adding new column SEX_RAT and calculating no of males wrt 1000 females

abc5$SEX_RAT<- (abc5$TOT_F/abc5$TOT_M)*1000

##Rounding off to zero decimal places

abc5$SEX_RAT <- round(abc5$SEX_RAT, 0)

##Plotting SEX RATIO using bar graphs

sr<-ggplot(data=abc5, aes(x=Name, y=SEX_RAT,fill=Name)) +
geom_bar(stat="identity")
sr

##5. LITERACY RATIO

##Adding new column LIT_RAT and calculating no of males wrt 1000 females

abc5$LIT_RAT<- (abc5$P_LIT/abc5$TOT_P)*100

##Rounding off to zero decimal places

abc5$LIT_RAT <- round(abc5$LIT_RAT, 2)

##Plotting Literacy Ratio using bar graphs
lr<-ggplot(data=abc5, aes(x=Name, y=LIT_RAT,fill=Name)) +
  geom_bar(stat="identity")
lr






##Loading 2001 Census dataset 
    cens01<-read.csv("Final_Census_2001_Part2.csv")

##Making changes in the dataset in order to analyse the data and depict the various factors graphically


## 1  BAR graph for POPULATION of all districts using table def and def1
##Loading TOT_P,TOT_M and TOT_F from cens01 to def table

def<-cens01[,2:5]

##Melting the loaded data def into new table def1

def1<-melt(def,id.vars=c("NAME"))

ggplot(def1,aes(NAME,value))+
  geom_bar(aes(fill=variable),stat="identity",position="dodge")


## 2  pie chart for population of all districts LITERACY RATE(DEF2)

##New table created def2 which includes LIT_P,LIT_M and LIT_F
def2<-cens01[,2:8]

##pie chart for whole population district wise (DEF2)
bp<- ggplot(def2, aes(x="", y=P_LIT, fill=NAME))+
  geom_bar(width=1,stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie

## pie chart for all districts literacy (MALE) (DEF2)
bp1 <- ggplot(def2, aes(x="", y=M_LIT, fill=NAME))+
  geom_bar(width = 1, stat = "identity")
pie1<- bp1 + coord_polar("y",start=0)
pie1

## pie chart for districts literacy (FEMALE) (DEF2)
bp2 <- ggplot(def2, aes(x="", y=F_LIT, fill=NAME))+
  geom_bar(width = 1, stat = "identity")
pie2<- bp2 + coord_polar("y",start=0)
pie2


##3.  line graph for EMPLOYMENT(Types of Workers,Main Workers,Marginal WOrkers) by creating new table abc3

##Creating new table def3
def3<-cens01[,2:17]

##Melting table def3 to obtain def4
def4<- melt(def3, id="NAME")

##Creating Line Graph
lp<-ggplot(data=def4,aes(x=NAME, y=value, colour=variable))+geom_line(aes(group=variable))
lp

##4. SEX RATIO 
def5<-cens01[,2:8]

##Adding new column SEX_RAT and calculating no of males wrt 1000 females

def5$SEX_RAT<- (def5$TOT_F/def5$TOT_M)*1000

##Rounding off to zero decimal places

def5$SEX_RAT <- round(def5$SEX_RAT, 0)

##Plotting SEX RATIO using bar graphs

sr<-ggplot(data=def5, aes(x=NAME, y=SEX_RAT,fill=NAME)) +
  geom_bar(stat="identity")
sr

##5. LITERACY RATIO

##Adding new column LIT_RAT and calculating no of males wrt 1000 females

def5$LIT_RAT<- (def5$P_LIT/def5$TOT_P)*100

##Rounding off to zero decimal places

def5$LIT_RAT <- round(def5$LIT_RAT, 2)

##Plotting Literacy Ratio using bar graphs
lr<-ggplot(data=def5, aes(x=NAME, y=LIT_RAT,fill=NAME)) +
  geom_bar(stat="identity")
lr

### Combine Graphs for 2001 and 2011

##1. Population for 2001 and 2011 (bar graphs)

##Melting the table 
com1<-melt(com,id.vars=c("cens11.Name"))

##Plotting the melted data on bar graph
ggplot(com1,aes(cens11.Name,value))+ 
  geom_bar(aes(fill=variable),stat="identity",position="dodge")


## 2  Combined line graph for Literates population(Total,Male and Female) of all districts LITERACY RATE (COM2)

##TOTAL POPULATION
##New table created com2 which includes P_LIT,M_LIT,F_LIT(2011) and P_LIT.1,M_LIT.1,F_LIT.1(2001)
com2<-abc2
com2[,5:7]<-def2[,2:4]

##Melting table com2
com3<-melt(com2,id.vars=c("Name"))

##Creating Line Graph
lp<-ggplot(data=com3,aes(x=Name, y=value, colour=variable))+geom_line(aes(group=variable))
lp


##3.  for EMPLOYMENT(Types of Workers,Main Workers,Marginal WOrkers) by creating new table abc3

##Creating new table com4
com4<-abc3
com4[,4:5]<-def3[,2:3]

##Melting table com4 to obtain com5
com5<- melt(com4, id="Name")


##Plotting bar graphs for MAINWORK_P,MARGWORK_P(2011) and MAINWORK_P.1,MARGWORK_P.1(2001) 
ggplot(com5,aes(Name,value))+
geom_bar(aes(fill=variable),stat="identity",position="dodge")


##4. SEX RATIO for year 2011 and 2001
com6[,4:5]<-def5[6:7]


##Plotting SEX RATIO for 2011 and 2001 using line graph
##Melting table com6 to obtain com8
com8<- melt(com6, id="Name")


##Plotting bar graphs for SEX_RATIO(2011) and (2001) 
lp<-ggplot(data=com8,aes(x=Name, y=value, colour=variable))+geom_line(aes(group=variable))
lp

##5.LITERACY RATe for year 2011 and 2001


##Plotting Literacy Rate for 2011 and 2001 using bar graph
##Melting table com6 to obtain com8
com9<- melt(com7, id="Name")

##Plotting Literacy Rate using bar graph
ggplot(com9,aes(Name,value))+
  geom_bar(aes(fill=variable),stat="identity",position="dodge")
















##PREDICTION FOR THE YEAR 2021


##Predicting Population from Literacy

lm(abc$TOT_P ~ abc$P_LIT)               #Using linear regression
lm.out=lm(abc$TOT_P ~ abc$P_LIT)        #Storing the output in lm.out
lm.out
summary(lm.out)                         #Gives a lot more info
options(show.signif.stars=F)            #Turn Significance stars off
anova(lm.out)                           #Shows an ANOVA table
plot(abc$TOT_P ~ abc$P_LIT, main="Population Prediction plot")       #plot graph
abline(lm.out, col="red")               #plot regression line
par(mfrow=c(2,2))                       #divide plot window into 4
plot(lm.out)                            #plot graphs
lm.out$fitted[1]                        #verify (not needed)
lm.out$residuals[1]
attach(abc22)                           #attach data frame
Population.lm=lm(TOT_P ~ P_LIT)         #regression
newdata = data.frame(P_LIT=17800000)    #value for which prediction is to be done
predict(Population.lm, newdata, interval="predict")         #predicted value


##Predicting Literacy from Population
lm(abc$P_LIT ~ abc$TOT_P)               #Using linear regression
lm.out=lm(abc$P_LIT ~ abc$TOT_P)        #Storing the output in lm.out
lm.out
summary(lm.out)                         #Gives a lot more info
options(show.signif.stars=F)            #Turn Significance stars off
anova(lm.out)                           #Shows an ANOVA table
plot(abc$P_LIT ~ abc$TOT_P, main="Literacy Prediction plot")       #plot graph
abline(lm.out, col="blue")               #plot regression line
par(mfrow=c(2,2))                       #divide plot window into 4
plot(lm.out)                            #plot graphs
lm.out$fitted[1]                        #verify (not needed)
lm.out$residuals[1]
attach(abc22)                           #attach data frame
Population.lm=lm(P_LIT ~ TOT_P)         #regression
newdata = data.frame(TOT_P=23500000)    #value for which prediction is to be done
predict(Population.lm, newdata, interval="predict")         #predicted value

##Predicting Sex ratio from population
lm(abc22s$SEX_RAT ~ abc22s$TOT_P)               #Using linear regression
lm.out=lm(abc22s$SEX_RAT ~ abc22s$TOT_P)        #Storing the output in lm.out
lm.out
summary(lm.out)                         #Gives a lot more info
options(show.signif.stars=F)            #Turn Significance stars off
anova(lm.out)                           #Shows an ANOVA table
plot(abc22s$SEX_RAT ~ abc22s$TOT_P, main="Sex Ratio Prediction plot")       #plot graph
abline(lm.out, col="green")               #plot regression line
par(mfrow=c(2,2))                       #divide plot window into 4
plot(lm.out)                            #plot graphs
lm.out$fitted[1]                        #verify (not needed)
lm.out$residuals[1]
attach(abc22s)                           #attach data frame
Population.lm=lm(SEX_RAT ~ TOT_P)         #regression
newdata = data.frame(TOT_P=23500000)    #value for which prediction is to be done
predict(Population.lm, newdata, interval="predict")         #predicted value

##Predicting Literacy Rate from Population
lm(abc22s$LIT_RAT ~ abc22s$P_LIT)               #Using linear regression
lm.out=lm(abc22s$LIT_RAT ~ abc22s$P_LIT)        #Storing the output in lm.out
lm.out
summary(lm.out)                         #Gives a lot more info
options(show.signif.stars=F)            #Turn Significance stars off
anova(lm.out)                           #Shows an ANOVA table
plot(abc22s$LIT_RAT ~ abc22s$P_LIT, main="Literacy Rate Prediction plot")       #plot graph
abline(lm.out, col="yellow")               #plot regression line
par(mfrow=c(2,2))                       #divide plot window into 4
plot(lm.out)                            #plot graphs
lm.out$fitted[1]                        #verify (not needed)
lm.out$residuals[1] 
attach(abc22s)                           #attach data frame
Population.lm=lm(LIT_RAT ~ P_LIT)         #regression
newdata = data.frame(P_LIT=19000000)    #value for which prediction is to be done
predict(Population.lm, newdata, interval="predict")         #predicted value


## Predicting MAINWORKERS from total Population
lm(ab$MAINWORK_P ~ ab$TOT_P)               #Using linear regression
lm.out=lm(ab$MAINWORK_P ~ ab$TOT_P)        #Storing the output in lm.out
lm.out
summary(lm.out)                         #Gives a lot more info
options(show.signif.stars=F)            #Turn Significance stars off
anova(lm.out)                           #Shows an ANOVA table
plot(ab$MAINWORK_P ~ ab$TOT_P, main="Main Worker Prediction plot")       #plot graph
abline(lm.out, col="yellow")               #plot regression line
par(mfrow=c(2,2))                       #divide plot window into 4
plot(lm.out)                            #plot graphs
lm.out$fitted[1]                        #verify (not needed)
lm.out$residuals[1] 
attach(ab)                           #attach data frame
Population.lm=lm(MAINWORK_P ~ TOT_P)         #regression
newdata = data.frame(TOT_P=23000000)    #value for which prediction is to be done
predict(Population.lm, newdata, interval="predict")         #predicted value


## Predicting MARGWORKERS from total Population
lm(ab$MARGWORK_P ~ ab$TOT_P)               #Using linear regression
lm.out=lm(ab$MARGWORK_P ~ ab$TOT_P)        #Storing the output in lm.out
lm.out
summary(lm.out)                         #Gives a lot more info
options(show.signif.stars=F)            #Turn Significance stars off
anova(lm.out)                           #Shows an ANOVA table
plot(ab$MARGWORK_P ~ ab$TOT_P, main="Main Worker Prediction plot")       #plot graph
abline(lm.out, col="yellow")               #plot regression line
par(mfrow=c(2,2))                       #divide plot window into 4
plot(lm.out)                            #plot graphs
lm.out$fitted[1]                        #verify (not needed)
lm.out$residuals[1] 
attach(ab)                           #attach data frame
Population.lm=lm(MARGWORK_P ~ TOT_P)         #regression
newdata = data.frame(TOT_P=23000000)    #value for which prediction is to be done
predict(Population.lm, newdata, interval="predict")         #predicted value
