# ### LIBRARIES #####
library(ggplot2)
library(dplyr)

# Make your dream team
# Analyse which Club or National Team has the best-rated players
# Analyse the ages of the footballer and Peak ages for the career.
# Analyse the top 20 Valuable Clubs and top 10 countries Producing these World class footballers.
# Build a Model to Explain the Overall Ratings based on various explainatory Variables
# Check for interaction terms to improve the accuracy and drop unnecesssary Variables if necessary.

###################


###################
setwd("C://Sem2/SDM/R/fifa19/") # Set your path here.
raw = read.csv("data.csv",stringsAsFactors = FALSE)
View(raw)
## We observe that there are a number of rows with NA Values.
## Remove NA Columns from the Dataset. 
clean = na.omit(raw)

## Convert Wages and Value to Numeric Type by using regular expressions
convertnum <- function(sal) 
{
  sal <- as.character(sal)
  sal <- gsub("(â,¬)","", sal)
  fin = sal
  K_sal <- grep("K", sal)
  fin[K_sal] <- as.numeric(gsub("K","", fin[K_sal]))*1000
  
  M_sal <- grep("M", sal)
  fin[M_sal] <- as.numeric(gsub("M","", fin[M_sal])) *1000000
  
  return(fin)
}

clean$Wage = as.numeric(convertnum(clean$Wage)) ## Wages are in Thousands
clean$Value = as.numeric(convertnum(clean$Value)) ## Value is in Millions
clean$Release.Clause = as.numeric(convertnum(clean$Release.Clause))
View(clean) ## grep and gsub perform replacement of the first and all matches respectively.

## Some Plots
hist(clean$Overall, xlab = "Overall Rating",main = "Overall Ratings",)
dim(clean)
hist(clean$Value, xlab = "Net Worth ",main = "Net Worth of Footballers")

hist(log(clean$Value),xlab = "log(Net Worth) ",main = "Net Worth of Footballers LOG TRANSFORM")


# Distribution with respect to Age

g_age = ggplot(data = clean, aes(Age))
g_age + geom_histogram(col="orange",binwidth = 5 ,aes(fill = ..count..)) + ggtitle("Distribution based on Age")
hist(log(clean$Value),main = "Histogram of Log(Value)", xlab = "Log VALUE", ylab = "Frequency")

### Top 20 Valuable clubs

group_clubs = group_by(clean, Club)
club_value = summarise(group_clubs, Net_Worth = sum(Value))
top_20_valuable_clubs = top_n(club_value, 20, Net_Worth)
top_20_valuable_clubs$Club =  as.factor(top_20_valuable_clubs$Club)
ggplot(top_20_valuable_clubs, aes(x = Club, y = Net_Worth)) + geom_bar(stat = "identity", aes(fill=Net_Worth)) + coord_flip() + ggtitle("Top 20 valuable clubs")

### Top 10 Countries producing net worth Players

group_clubs_nation = group_by(clean,Nationality)
nation_value = summarise(group_clubs_nation, Nat_Value = sum(Value))
top_10_Nations = top_n(nation_value,10,Nat_Value)
top_10_Nations$Nationality = as.factor(top_10_Nations$Nationality)
ggplot(top_10_Nations, aes(x = Nationality, y = Nat_Value)) + geom_bar(stat = "identity", aes(fill=Nat_Value)) + coord_flip()  + ggtitle("Top 10 Nations")


### Positions of Players - Is position a vital trait of a Player?
unique(clean$Position)

pos_pl = as.factor(clean$Position)
levels(pos_pl) = list(GK  = c("GK",""), 
                  DEF = c("LWB", "LB", "CB", "RB", "RWB","LDM","RCB","LCB"), 
                  MID = c("LW","LM","CDM","RCM","CM","LCM","CAM","LAM","RDM","RAM","RM","RW"), 
                  FWD = c("CF", "ST","RF","LF","RS","LS"))
clean = mutate(clean,Position = pos_pl)
head(clean)

### Age and Position 

g_age +   geom_density(aes(fill = clean$Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Distribution based on Age and Position")

## Getting Correlations
cor(x = clean$Age,y = clean$Overall)
cor(x= clean$Age,y = clean$Potential)
max(clean$Value)
boxplot(log(clean$Value))


### Studying Using Overall Rating



## First we create a base model by adding the intutive factors like : Value, Age, Nationality.

m1 = lm(Overall~Value+Age+Nationality,data = clean)
summary(m1)
## We observe here that Value Age and Nationality help explain 63% of Variance in the data. 


## We start with our first Hypothesis :
## H0 : We start by hypothesizing for Player attributes such as Agility. Null Hypothesis is that Agility is not of significance to Overall Rating. 
## HA : Agility is of significance.

m2 = lm(Overall~Value+Age+Nationality+Agility,data = clean)#summary(m2)#m## We see that a unit increase in agiity increases the overall rating by 0.075 Units. 
## Also since the P value is significant we do not ignore this. We will keep this in the model.

## 2nd Hypothesis. 
## Hypothesis : Is composure/Reaction a trait that makes a better soccer player ?

m3 = lm(Overall~Value+Age+Nationality+Composure+Reactions+Agility,data = clean)
summary(m3)a
## An abrubt increase in the adjusted R squared is indicative of the fact that Raction and Composure 
## are both vital to Overall Rating. Or we can say that a unit increase in Composure and Reaction
## increases the Overall Rating by 0.1 and 0.36 respectively. 

## 3rd Hypothesis. 
## Is there an interaction between Reaction and Value.

m4 = lm(Overall~Value+Age+Nationality+Composure+Reactions+Agility+I(Reactions*Value),data = clean)
summary(m4)

## We observe that the the interaction term suggests that the net worth of a player and his 
## Reactions are jointly contribulitng to the overall rating. We observe that after adding this variable the
## effect of Agility goes down marginally. Is it okay to remove it from the model ? Let us try !

m5 = lm(Overall~Value+Age+Composure+Reactions+I(Reactions*Value),data = clean)
summary(m5)

## In order to make the model more Parsimonious I hypothesised on dropping for Nationality. 

## Some more plots to check OLS Assumptions

hist(m5$residuals)
qqnorm(m5$residuals) 
norm = rnorm(1000)
ks.test(norm,m5$fitted)oohi




