
#find a csv of transaction of grocery site
url='https://cdn.rawgit.com/aviram2308/vp-mkt-bskt-anlsis/5a0c4269/groceries.csv'


#import the following package for association rule
library(arules)


#making a item matrix df
df=read.transactions(url,sep=',') 



df

head(df)

inspect(head(df))

str(df)

# checking the density and other details of df
summary(df)


inspect(head(df))

#items of first 20 transactions
inspect(head(df,20))


inspect(df[1:20])

inspect(df[1:50,1:20])

#frequency of first 20 items alphabetically 
itemFrequency(df[,1:10])


itemFrequency(df[,1:30])

#plot of 5 most frequent items
itemFrequencyPlot(df,topN=5)


itemFrequencyPlot(df,topN=10)

itemFrequencyPlot(df,topN=25)

itemFrequencyPlot(df,topN=25,type="absolute")

#forming  Association rules using apriori algorithm
rules1=apriori(df)


summary(rules1)

#default parameters of apriori function was too stringent
#so lowering the min_supp and min_conf
rules2=apriori(df, parameter=list(support=0.007, confidence=.25, minlen=2))


summary(rules2)

inspect(rules2[1:20])

#trying maxlen as 3,4,5
rules3=apriori(df, parameter=list(support=0.007, confidence=.25, minlen=2, maxlen=5))


#picking the top 15 rules giving the highest lift
Lift.rules=inspect(sort(rules2, by='lift')[1:15])


#creating a vector itf containing the frequency of items
itf=itemFrequency(df)


class(itf)

str(itf)

length(itf)

head(itf)

#sorting itf as per frequency in descending order
itf=sort(itf, decreasing=T)


head(itf)

#15 most frequent items and their frequency
head(itf,15)


#15 least  frequent items and their frequency
tail(itf,15)


# creating rules for 5 most frequent items, because they have a good chance of being bought

#rules4 contain whole milk in its rhs
rules4<-apriori(data=df, parameter=list(support=0.02, confidence=.25), 
               appearance = list(default="lhs",rhs=c('whole milk')),
               control = list(verbose=F))
summary(rules4)
inspect(rules4)


#rules5 contain other vegetables in its rhs
rules5<-apriori(data=df, parameter=list(support=0.02, confidence=.25), 
               appearance = list(default="lhs",rhs=c('other vegetables')),
               control = list(verbose=F))
summary(rules5)
inspect(rules5)


#rules6 contain rolls/buns in its rhs
rules6<-apriori(data=df, parameter=list(support=0.02, confidence=.25), 
               appearance = list(default="lhs",rhs=c('rolls/buns')),
               control = list(verbose=F))
summary(rules6)
inspect(rules6)


#rules7 contain soda in its rhs
rules7<-apriori(data=df, parameter=list(support=0.02, confidence=.25), 
               appearance = list(default="lhs",rhs='soda'),
               control = list(verbose=F))
summary(rules7)
inspect(rules7)


#rules8 contain yogurt in its rhs
rules8<-apriori(data=df, parameter=list(support=0.02, confidence=.25), 
               appearance = list(default="lhs",rhs='yogurt'),
               control = list(verbose=F))
summary(rules8)
inspect(rules8)


#using Lift.rules and rules 4 to 8 we can make a item recommendation system for the site
