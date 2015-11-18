# DSS Project 2
John Weng  
November 6, 2015  

The purpose of this case study is to explore the basic skill of 'data sciencentist', through online job postings under the search term. The assumption is that we can use the job tags that employers post for data scientists to define what a data scientist does. We hope to arrive data science as a discipline by the most requested skills under search term.

Below are the required R packages needed:


```
## Loading required package: RCurl
## Loading required package: bitops
## Loading required package: XML
## Loading required package: plyr
## Loading required package: wordcloud
## Loading required package: RColorBrewer
```

To begin, we want to get a better idea of the search we are conducting at cybercoder.com. First, txt1 will obtain the webpage code for first search result page. From there we will want to see how many postings, pages, and the URLs for each job sites.

```r
txt1 = getForm("http://www.cybercoders.com/search/", searchterms = "Data Scientist",
              searchlocation = "",  newsearch = "true", sorttype = "")
```
'doc1' is then set to orgniaze the webcode from txt1. 'links' will return the URL links for the the job sites on the first page. Because the maxium  number of job posting per page is twenty, there twenty sites' url in link. 

```r
doc1 = htmlParse(txt1, asText = TRUE)
links = getNodeSet(doc1, "//div[@class = 'job-title']/a/@href")
```

As of the set time below, there are 'job.n' number of search results for "Data scientist"

```r
# number of  jobs postings
Sys.time()
```

```
## [1] "2015-11-10 13:41:06 CST"
```

```r
jobs.n <- as.integer(xpathApply(doc1, "//div[@data-totalresults]", xmlGetAttr, "data-totalresults"))
jobs.n
```

```
## [1] 148
```
'page.n' returns the number of pages used to list all the jobs.

```r
page.n <- as.integer(xpathApply(doc1, "//div[@data-maxpages]", xmlGetAttr, "data-maxpages"))
page.n
```

```
## [1] 8
```

To get to all the job information, url_list is created, for do-loop purposes, to obtain page code for each of the 'page.n' search pages. The 'for' loop created each i in txt[[i]] for each of the search pages.  

```r
i<-c(1:page.n)
url_list <- paste('http://www.cybercoders.com/search/?page=',i,sep='')
url_list <- unlist(url_list)

doc<- vector("list", page.n)
for(i in 1:page.n){
  txt<-getForm(url_list[i], searchterms = '"Data Scientist"',
                    searchlocation = "",  newsearch = "true", sorttype = "")
  doc[[i]] <- htmlParse(txt,asText = TRUE)
}
```

To get each individual search for url

```r
linksk <- vector("list",page.n)
joblinks<- vector("list",page.n)

for (i in 1:page.n) {
  linksk[[i]] <- getNodeSet(doc[[i]], "//div[@class = 'job-title']/a/@href")
  joblinks[[i]] <- getRelativeURL(as.character(linksk[[i]]), "http://www.cybercoders.com/")
}
joblinks[[2]]
```

```
##                             /data-scientist-job-227151 
## "http://www.cybercoders.com/data-scientist-job-227151" 
##                             /data-scientist-job-227159 
## "http://www.cybercoders.com/data-scientist-job-227159" 
##                             /data-scientist-job-227162 
## "http://www.cybercoders.com/data-scientist-job-227162" 
##                             /data-scientist-job-227171 
## "http://www.cybercoders.com/data-scientist-job-227171" 
##                             /data-scientist-job-227174 
## "http://www.cybercoders.com/data-scientist-job-227174" 
##                             /data-scientist-job-227179 
## "http://www.cybercoders.com/data-scientist-job-227179" 
##                             /data-scientist-job-227185 
## "http://www.cybercoders.com/data-scientist-job-227185" 
##                             /data-scientist-job-227196 
## "http://www.cybercoders.com/data-scientist-job-227196" 
##                             /data-scientist-job-229031 
## "http://www.cybercoders.com/data-scientist-job-229031" 
##                             /data-scientist-job-214389 
## "http://www.cybercoders.com/data-scientist-job-214389" 
##                             /data-scientist-job-234497 
## "http://www.cybercoders.com/data-scientist-job-234497" 
##                             /data-scientist-job-232696 
## "http://www.cybercoders.com/data-scientist-job-232696" 
##                             /data-scientist-job-208486 
## "http://www.cybercoders.com/data-scientist-job-208486" 
##                             /data-scientist-job-202521 
## "http://www.cybercoders.com/data-scientist-job-202521" 
##                             /data-scientist-job-233874 
## "http://www.cybercoders.com/data-scientist-job-233874" 
##                             /data-scientist-job-237186 
## "http://www.cybercoders.com/data-scientist-job-237186" 
##                             /data-scientist-job-223543 
## "http://www.cybercoders.com/data-scientist-job-223543" 
##                             /data-scientist-job-238260 
## "http://www.cybercoders.com/data-scientist-job-238260" 
##                             /data-scientist-job-142939 
## "http://www.cybercoders.com/data-scientist-job-142939" 
##                             /data-scientist-job-205453 
## "http://www.cybercoders.com/data-scientist-job-205453"
```

Now that the all general urls are know, the next steps gathering all the tags off of the each job sites would be much easier. Though it is possible to get the tags from each job out by each url, the process would take much computer time. Since the tags are already on the search page, all the tag would be gather straight from each page. The tags from each search page are placed in t.tag, and [[]] marking the page number. Here, t.tag[[2]], is a list of tags from page 2.

```r
t.tag <- vector("list", page.n)
for (i in 1:page.n){
  a<-getNodeSet(doc[[i]],"//span[@class='skill-name']")
  t.tag[[i]]<- unlist(sapply(a, xmlValue))
  }

t.tag[[2]]
```

```
##  [1] "Python"                          "Hadoop"                         
##  [3] "Scala"                           "Linux"                          
##  [5] "Apache Spark"                    "Python"                         
##  [7] "Hadoop"                          "Scala"                          
##  [9] "Linux"                           "Apache Spark"                   
## [11] "Python"                          "Hadoop"                         
## [13] "Scala"                           "Linux"                          
## [15] "Apache Spark"                    "Python"                         
## [17] "Hadoop"                          "Scala"                          
## [19] "Linux"                           "Apache Spark"                   
## [21] "Python"                          "Hadoop"                         
## [23] "Scala"                           "Linux"                          
## [25] "Apache Spark"                    "Python"                         
## [27] "Hadoop"                          "Scala"                          
## [29] "Linux"                           "Apache Spark"                   
## [31] "Python"                          "Hadoop"                         
## [33] "Scala"                           "Linux"                          
## [35] "Apache Spark"                    "Python"                         
## [37] "Hadoop"                          "Scala"                          
## [39] "Linux"                           "Apache Spark"                   
## [41] "Data Mining"                     "ETL Process"                    
## [43] "Predictive Analysis"             "R"                              
## [45] "Hadoop"                          "Machine Learning"               
## [47] "Natural Language Processing"     "Java"                           
## [49] "Hadoop"                          "Mahout"                         
## [51] "Data Scientist"                  "Big Data Statistics"            
## [53] "Geo-Spatial Analyst"             "Predictive Analystics"          
## [55] "Consumer Segmentation Reporting" "Machine Learning"               
## [57] "NLP"                             "Data Mining"                    
## [59] "Agricultural Data"               "Geospatial Data"                
## [61] "Deep Learning"                   "Python/R/Java"                  
## [63] "GIS/PostGIS"                     "Machine Learning"               
## [65] "Hadoop"                          "Python"                         
## [67] "Data Mining"                     "R"                              
## [69] "Python"                          "R"                              
## [71] "SAS"                             "Stata"                          
## [73] "SPSS"                            "Pharmaceutical Industry"        
## [75] "Bioinformatics"                  "CRO Industry"                   
## [77] "Big Data Analysis"               "R programming"                  
## [79] "Python"                          "Excel"                          
## [81] "SQL"                             "SAS"                            
## [83] "R"                               "Python"                         
## [85] "Excel"                           "SQL"                            
## [87] "SAS"                             "R"                              
## [89] "Applied statistics"              "Experimental Design"            
## [91] "Hypothesis Testing"              "Predictive Analytics"           
## [93] "Python/R/SQL"                    "Deep Learning"                  
## [95] "Machine Learning"                "AI"                             
## [97] "Python"                          "C++"
```
Next, this section combines the all the tags from different pages and assign it to atag. Because all the data is a bit large to dispaly, a count of the top 15 most freqent tags are shown below. 


```r
atag<-c()
for (i in 1:page.n){
      atag <- c(atag,t.tag[[i]])
}

at.count <- count(atag) 
head(at.count [order(at.count[,2], decreasing = TRUE),],15)
```

```
##                    x freq
## 152           Python   81
## 112 Machine Learning   71
## 160                R   44
## 86            Hadoop   41
## 53       Data Mining   35
## 189              SQL   30
## 175              SAS   17
## 98              Java   14
## 15      Apache Spark   12
## 110            Linux   12
## 177            Scala   12
## 21          Big Data   11
## 121           Matlab   11
## 199       Statistics    9
## 56      Data Science    8
```

```r
tab.atag <-table(atag)
```

**Visiualization**

The dotchart only display the value with counts above five.
For the world cload, the margin have been adjusted so there won't be any unploted words. Namely, ' Machine learning' often gets rejected for its' high frequency and long text. 

```r
dotchart(sort(tab.atag[(tab.atag)>=5]),main="Skills from Data Scientist Search(count>=3)",)
```

```
## Warning in dotchart(sort(tab.atag[(tab.atag) >= 5]), main = "Skills from
## Data Scientist Search(count>=3)", : 'x' is neither a vector nor a matrix:
## using as.numeric(x)
```

![](DSS_Project_2_files/figure-html/unnamed-chunk-10-1.png) 

```r
wordcloud(names(tab.atag),tab.atag,scale=c(3,.2),min.freq=1)
```

![](DSS_Project_2_files/figure-html/unnamed-chunk-10-2.png) 


**Previous simi-completed**

This last next page only to show that prior to the assignment change another project is already in the works. Some work have been lost due to some confusing with objective of the assignment. I originally did the search on the stackover 

flow but the visionaliztion is pretty disaponting, since most of the gathered words are very common words
**title 1**


```r
require("ggplot2")
```

```
## Loading required package: ggplot2
```

```r
url2 <- "http://careers.stackoverflow.com/jobs/97977/"
cydoc2 <- htmlParse(url2)
```

Job title

```r
las1 <- getNodeSet(cydoc2,"//a[@class = 'title job-link']")
sapply(las1, xmlValue)
```

```
## [1] "(Senior) Data Scientist"
```
Listing the tag

```r
lis2 <- getNodeSet(cydoc2,"//div[@class = 'tags']//a[@class = 'post-tag job-link']")
sapply(lis2, xmlValue)
```

```
## [1] "c++"    "r"      "python" "hadoop" "sql"
```


```r
asWords =
  function(txt, StopWords = readLines("http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop"), stem = FALSE){
    words = unlist(strsplit(txt, '[[:space:]!.,;#:()/"]+'))
    words = words[words != ""]
    if(stem && require(Rlibstemmer))
      words = wordStem(words)
    i = tolower(words) %in% tolower(StopWords)
    words[!i]
  }


cy.Words2 = function(u, stopWords = StopWords, doc = htmlParse(u)){
  words = xpathApply(doc, "//div[@class='description']/ul",
                     function(x ) asWords(xmlValue(x)))
  words[[2]]
}
```

getting skills from "skills & requirements"

```r
a2 <- unlist(cy.Words2(url2))
a3 <- cy.Words2(a2)

kk2 <- as.data.frame(count(a2))
```
Count of the words with more a count of 2, or the list is pretty long

```r
kk2 <-kk2[which(kk2$freq >= 2),]
kk2
```

```
##               x freq
## 4       Ability    3
## 14          Big    2
## 15     business    2
## 23         data    5
## 24         Data    2
## 30       Expert    8
## 34     handling    2
## 38  integration    2
## 40    knowledge    3
## 41    languages    2
## 52 optimization    2
## 58  programming    2
## 60       Proven    2
## 64       regard    4
## 65     required    2
## 68       skills    8
```

```r
kk2$x<- factor(kk2$x, levels = kk2$x[order(kk2$freq)])
```
graphic count 

```r
ggplot(kk2,aes(x=x, y=freq)) + theme_bw() + geom_bar(stat='identity')
```

![](DSS_Project_2_files/figure-html/unnamed-chunk-18-1.png) 
about Gfk 4

```r
las2 <- getNodeSet(cydoc2,"//div[@class = 'description'][3]/p")
sapply(las2, xmlValue)
```

```
## [1] "Having choices is what makes life exciting. Make your choice today and take your future into your own hands. We value skills and talents, and will support your development within our international teams. GfK makes your choice easy as we offer an exciting work environment that brings people together and encourages an entrepreneurial and innovative spirit. As a trusted leader in market research present in over 100 countries, we know what makes consumersâ<U+0080><U+0099> hearts beatâ<U+0080><U+0094>globally and locally. We work on projects for successful companies in virtually every major industry."
```
location 5

```r
las3 <- getNodeSet(cydoc2,"//span[@class = 'location']")
sapply(las3, xmlValue)
```

```
## list()
```
benifts 6

```r
las4 <- getNodeSet(cydoc2,"//div[@class = 'benefits-list']//span[@class = 'benefit-desciption']")
sapply(las4, xmlValue)
```

```
## [1] "Competitive salary"          "Attractive vacation scheme" 
## [3] "Excellent employer benefits" "Collective healthcare plan" 
## [5] "Flexible working hours"      "Travel compensation"        
## [7] "Global Learning Programs"
```
