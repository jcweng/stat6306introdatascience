## Load packages needed for this project
library(XML)
library(RColorBrewer)
library(maps)
install.packages("RKML", repos = "http://www.omegahat.org/R", type = "source") # R looks for a binary version of the package, which is not available. Instead we have to use the source code for installing this package
library(RKML)

## Create the popDF and infMortDF data sets for this project
factbookDOC = xmlParse("C:/Users/John Weng/Documents/GitHub/stat6306introdatascience/factbook.xml")
factbookRoot = xmlRoot(factbookDOC)
xmlName(factbookRoot)
xmlSize(factbookRoot)
table(names(factbookRoot))
sapply(factbookRoot["category"], function(node) table(names(node)))
sapply(factbookRoot["category"], xmlAttrs)
categoryNodes = factbookRoot["category"]
w = sapply(categoryNodes, xmlGetAttr, "name")=="People and Society"
Ids = sapply(categoryNodes[[ which(w) ]] [ "field" ], xmlGetAttr, "id")
f2091Index = which(Ids == "f2091")
f2091Index # CIA Factbook directions has "field" of 17, we have 18
rankNodes = categoryNodes[[ which(w) ]][ "field" ][[ f2091Index ]]["rank"]
xmlSize(rankNodes) # CIA Factbook direction has 223, we have 224
infMortNum = sapply(rankNodes, xmlGetAttr, "number")
infMortCtry = sapply(rankNodes, xmlGetAttr, "country")
head(infMortNum)
head(infMortCtry)
field2091 = getNodeSet(factbookDOC, "//field[@id='f2091']") # CIA Factbook directions has "factbookDoc", should be "factbookDOC"
xmlAttrs(field2091[[1]])
rankNodes = getNodeSet(factbookDOC, "//field[@id='f2091']/rank")
xmlAttrs(rankNodes[[1]])
infNum = as.numeric(sapply(rankNodes, xmlGetAttr, "number"))
infCtry = sapply(rankNodes, xmlGetAttr, "country")
infMortDF = data.frame(infMort = infNum, ctry = infCtry, stringsAsFactors = FALSE)
rankNodes = getNodeSet(factbookRoot, "//field[@id='f2119']/rank")
popNum = as.numeric(sapply(rankNodes, xmlGetAttr, "number"))
popCtry = sapply(rankNodes, xmlGetAttr, "country")
popDF = data.frame(pop = popNum, ctry = popCtry, stringsAsFactors = FALSE)

## Get the Lattitude and Longitude data frame
latlonDF <- read.csv("http://dev.maxmind.com/static/csv/codes/country_latlon.csv", sep=",")

## Combine the latlonDF, popDF, and infMortDF data frames
head(latlonDF)
head(popDF)
head(infMortDF)
nrow(latlonDF)
IMPop = merge(infMortDF, popDF, by="ctry", all=FALSE)
dim(IMPop)
latlonDF$code=tolower(as.character(latlonDF$iso.3166.country))
ISOCodes <- read.csv("https://raw.githubusercontent.com/jcweng/stat6306introdatascience/master/iso%26cia%20country%20codes.csv")
latlonDFMerge=merge(latlonDF, ISOCodes, by="iso.3166.country", all=FALSE)
latlonDFMerge$ctry=tolower(as.character(latlonDFMerge$ctry))
allCtryData=merge(IMPop, latlonDFMerge, by.x="ctry", all=FALSE)

## Get MapCodes
# sapply(factbookRoot["appendix"], xmlAttrs)
# categoryNodes1 = factbookRoot["appendix"]
# q = sapply(categoryNodes1, xmlGetAttr, "name")=="cross-reference list of country data codes"
# Ids1 = sapply(categoryNodes[[ which(q) ]] [ "field" ], xmlGetAttr, "id")
# f2154Index = which(Ids1 == "f2154")
# f2154Index

## Set the color palette and create the breaks to use for Infant Mortality Rate
display.brewer.all()
cols = brewer.pal(9, "YlOrRd")[c(1,2,4,6,7)]
newInfMort = cut(allCtryData$infMort, breaks = 5)
summary(newInfMort)
newInfMort2 = cut(allCtryData$infMort, breaks = c(0, 37, 50, 65, 80, 150))
summary(newInfMort2)
hist(allCtryData$infMort, breaks=20, main="", xlab="Infant Mortality per 1000 Live Births")
quantile(allCtryData$infMort, probs=seq(0, 1, by=.2))
InfMortDiscrete = cut(allCtryData$infMort, breaks=c(0, 10, 25, 50, 75, 150))

## Create the Map
world = map(database="world", fill=TRUE, col="light grey")
symbols(allCtryData$longitude, allCtryData$latitude, add=TRUE, circles=sqrt(allCtryData$pop)/4000, inches=FALSE, fg=cols[InfMortDiscrete], bg=cols[InfMortDiscrete])
legend(x=-150, y=0, title="Infant Mortality", legend=levels(InfMortDiscrete), fill=cols, cex=.8)
range(allCtryData$pop)
hist(sqrt(allCtryData$pop), breaks=20, xlab="Square-root of Population", main="")
rads=pmax(sqrt(allCtryData$pop)/4000, 1)

