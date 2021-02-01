
#Que 1

#The team wants to analyze each variable of the data collected
#through data summarization to get a basic understanding of the
#dataset and to prepare for further analysis.

library(readxl)
data<-read_xlsx("C:/Users/Harshada/data/1555058318_internet_dataset.xlsx")
View(data)
head(data)
str(data)
summary(data)
is.na(data)
#data<-na.omit(data)

#Factorize the attribute Continentss

data$Continent=factor(data$Continent,
                      levels = c("AF","AS","EU","N.America","OC","SA"),
                      labels = c(0,1,2,3,4,5))
data

#Factorize the attribute Sourcegroup

factor(data$Sourcegroup)
levels(data$Sourcegroup)
table(data$Sourcegroup)
data$Sourcegroup=factor(data$Sourcegroup,
                        levels = c("(direct)","google","public.tableausoftware.com","t.co","visualisingdata.com","facebook","Others","reddit.com","tableausoftware.com"),
                        labels = c(0,1,2,3,4,5,6,7,8))

data


#scaling of attribute Timeingpage

data$Timeinpage=scale(data$Timeinpage)
View(data)


#To validate model using chi squared test

chisq.test(data$Uniquepageviews,data$Visits) 

factor(data$Exits)

chisq.test(data$Exits,data$Continent)
chisq.test(data$Exits,data$Sourcegroup)
chisq.test(data$Exits,data$Timeinpage)
chisq.test(data$Exits,data$Uniquepageviews)
chisq.test(data$Exits,data$Visits)
chisq.test(data$Exits,data$Bounces)

#bounces min=0,max=30
#exit min=0 max=36
#From the result of summarized dataset, it is observed that the numerical data includes 
#information related to the maximum, minimum, and mean data. 
#The categorical data like continent includes the data of the number of times the category has been 
#repeated in the dataset. We can see that there is a maximum value of 30 bounces for the website.
#This site was accessed maximum number of times by visitors from North A


#ques2
#As mentioned earlier,a unique page view represents the number of sessions during which that
#page was viewed one or more times. A visit counts all instances, no matter 
#how many times the same visitor may have been to your site. So the team needs
#to know whether the unique page view value depends on visits.



library(ggplot2)
ggplot(data,aes(x=Bounces,y=Visits))+geom_point(color="red",shape=3)

library(caTools)
set.seed(123)

train=data[1:80,]
test=data[1:100,]

model=lm(Bounces~.,train)
summary(model)

factor(data$Continent)
cor(data$Uniquepageviews,data$Visits)

anov<-aov(Uniquepageviews~Visits,data = data)
summary(anov)


#We can conclude from results that Visits variable has significant impact on 
#uniquepage views.so team can conclude that uniquepage values depends on Visits.



#Que3

#Find out the probable factors from the dataset, which could affect the exits.
#Exit Page Analysis is usually required to get an idea about why a user leaves 
#the website for a session and moves on to another one. 
#Please keep in mind that exits should not be confused with bounces.

anoo<-aov(Exits~.,data = data)
summary(anoo)

newModel=lm(Exits~Bounces+Sourcegroup+Timeinpage+Visits,train)
summary(newModel)

predExits=predict(newModel,test)
View(predExits)
predExits=round(predExits)
View(predExits)
factor(predExits)

final_data=cbind(test,predExits)
View(final_data)

#Plot of affects of Bounces and timeingpage on Exits 

ggplot(data,aes(x=Bounces+Timeinpage,y=Exits))+geom_point(color="red",shape=3)

#From the result of ANOVA given here, we can see that source.group, bounces,
#and unique.pageviews have more significance. Visits have comparatively less significance.
#Hence we can say that exit from the site is affected by the factors of source group,
#bounces, and unique.pageviews.  



#Que 4
#Every site wants to increase the time on page for a visitor. This increases
#the chances of the visitor understanding the site content better and hence 
# there are more chances of a transaction taking place. Find the variables
#which possibly have an effect on the time on page.

anooo<-aov(Timeinpage~.,data = data)
summary(anooo)
library(psych)
describe(data)

summary(data)


#find corelation between variables

#one way annova of bounces
aov1<-aov(Timeinpage~Bounces,data = data)
summary(aov1)
print(model.tables(aov1,"means"),digits = 3)

#one way annova of Continent
aov2<-aov(Timeinpage~Continent,data = data)
summary(aov2)
print(model.tables(aov2,"means"),digits = 3)


#Two way annova

aov3<-aov(Timeinpage~Bounces*Continent,data = data)
summary(aov3)
print(model.tables(aov3,"means"),digits = 3)


#Corelation
library(ggplot2)
library(GGally)

cor(data$Timeinpage,data$Bounces)
cor(data[,5:6])


#Corelation coefficient is between -1 to 1 hence graph shows strong and weak
#corelation between variables.

ggcorr(data,label=TRUE,label_alpha=TRUE)


#Strong corelation

qplot(Visits,Timeinpage,data = data,geom = c("point","smooth"),method="lm",alpha=I(1/5),se=FALSE)


#Weak corelation

qplot(Exits,Timeinpage,data = data,geom = c("point","smooth"),alpha=I(1/5))

#All together

ggpairs(data,columns = c("Timeinpage","Bounces","Exits"),upper = list(continuous=wrap("cor",size=10)),lower = list(continuous="smooth"))

#only source group is not affecting the time in page views rest all are 
#significantly afecting the timein page views




#Que5
#A high bounce rate is a cause of alarm for websites which depend on visitor engagement. Help the team in determining 
#the factors that are impacting the bounce.


data$Bounces=data$Bounces*0.01
rmm<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = data,family = "binomial")
summary(rmm)

#As can be inferred from the result shown, the BouncesNew, Unique.Pageviews and visits are the variables that
#impact the target variable bounces
#it has greater significance. 
