# HW5
John Weng  
October 15, 2015  


```r
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
Country.GDP <- read.csv("https://raw.githubusercontent.com/jcweng/stat6306introdatascience/master/Country%20GDP.csv")
Country <- read.csv(url("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv "))

MergeData1 <- merge(x = Country.GDP, y=Country, by = "CountryCode", all.x=TRUE)
nrow(MergeData1)
```

```
## [1] 190
```

The data set include all Country.GDP observations, and there are a total of 190 observations.

```r
View(MergeData1 ) 
nrow(Country)-nrow(Country.GDP)
```

```
## [1] 44
```
There are observations 44 excluded

**Question 2** 

```r
MergeData1 <-MergeData1[order(-MergeData1$Ranking),]
```

ascending order by GDP rank (so United States is last)

```r
tail(MergeData1[seq(1,5)], n=5)
```

```
##     CountryCode Ranking       Country  Economy                   Long.Name
## 58          FRA       5        France  2612878             French Republic
## 45          DEU       4       Germany  3428131 Federal Republic of Germany
## 87          JPN       3         Japan  5959718                       Japan
## 34          CHN       2         China  8227103  People's Republic of China
## 179         USA       1 United States 16244600    United States of America
```

**Question 3** 

```r
attach(MergeData1)
```

```
## The following object is masked _by_ .GlobalEnv:
## 
##     Country
```

```r
m1 <- MergeData1[ which(MergeData1$Income.Group=="High income: OECD"),]
m2 <- MergeData1[ which(MergeData1$Income.Group=="High income: nonOECD"),]

m1<-m1[order(m1$Ranking),] 
m2<-m2[order(m2$Ranking),] 
round(mean(m1$Ranking))
```

```
## [1] 33
```

```r
nrow(m1)
```

```
## [1] 30
```

```r
nrow(m2)
```

```
## [1] 23
```

there is a total of 30 "High income: OECD", with mean GDP rank of 33:

```r
round(mean(m1$Ranking))
```

```
## [1] 33
```
there is a total of 23 "High income: nonOECD", with mean GDP rank of 92:

```r
round(mean(m2$Ranking))
```

```
## [1] 92
```

**Question 4** 

```r
MergeData1$GDP.G <-cut(MergeData1$Ranking,c(-Inf,seq(38,152,by=38),Inf), labels = seq(1,5))
attach(MergeData1)
```

```
## The following object is masked _by_ .GlobalEnv:
## 
##     Country
## 
## The following objects are masked from MergeData1 (pos = 3):
## 
##     Alternative.conversion.factor,
##     Balance.of.Payments.Manual.in.use, Country, CountryCode,
##     Currency.Unit, Economy, External.debt.Reporting.status,
##     Government.Accounting.concept,
##     IMF.data.dissemination.standard, Income.Group,
##     Latest.agricultural.census, Latest.household.survey,
##     Latest.industrial.data, Latest.population.census,
##     Latest.trade.data, Latest.water.withdrawal.data,
##     Lending.category, Long.Name, National.accounts.base.year,
##     National.accounts.reference.year, Other.groups,
##     PPP.survey.year, Ranking, Region, Short.Name,
##     SNA.price.valuation,
##     Source.of.most.recent.Income.and.expenditure.data,
##     Special.Notes, System.of.National.Accounts, System.of.trade,
##     Table.Name, Vital.registration.complete, WB.2.code,
##     X2.alpha.code
```

```r
mytable <- table(GDP.G,Income.Group) # A will be rows, B will be columns 
mytable # print table 
```

```
##      Income.Group
## GDP.G    High income: nonOECD High income: OECD Low income
##     1  0                    4                18          0
##     2  0                    5                10          1
##     3  0                    8                 1          9
##     4  0                    4                 1         16
##     5  0                    2                 0         11
##      Income.Group
## GDP.G Lower middle income Upper middle income
##     1                   5                  11
##     2                  13                   9
##     3                  12                   8
##     4                   8                   8
##     5                  16                   9
```

there are 5 countries that are Lower middle income but among the 38 nations with highest GDP.

**Question 5** 

```r
summary(Lending.category)
```

```
##       Blend  IBRD   IDA  NA's 
##    51    16    62    60     1
```

```r
a <-MergeData1[complete.cases(MergeData1),]

nrow(a)
```

```
## [1] 14
```
If only complete cases are considered on every single catagory, only 14 makes the cut. I assume we are considering those with GDP rank and there are 190 observations. Certainly, if certain variables are allow to have missing value, NA, than we will have more observations.
