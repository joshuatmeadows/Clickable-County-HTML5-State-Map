Clickable County HTML5 State Map
================

This is a small tool I created to generate an HTML5 state map with
clickable counties.

The following chunk loads the necessary libraries.

``` r
library(devtools)
```

    ## Loading required package: usethis

``` r
library(geosphere)
library(urbnmapr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Change the variables in the following chunk to customize the map for
your state and urls. Use “@county” in the url to have the name of the
appropriate county inserted.

``` r
my_state_name="West Virginia"
href<-"https://extension.wvu.edu/@county"
```

The following chunk generates the HTML.

``` r
mytr<-function(x) {
  round((-abs(x))*1000)
}
stuff<-urbnmapr::counties%>%filter(state_name ==my_state_name)%>%mutate(County=sub(" County","",county_name))
counties<-unique(stuff$County)
xmax<-stuff$long%>%mytr%>%max
xmin<-stuff$long%>%mytr%>%min
ymax<-stuff$lat%>%mytr%>%max
ymin<-stuff$lat%>%mytr%>%min
width<-xmax-xmin
height<-ymax-ymin
code<- c("<div>","<svg viewBox=\"xmin ymin width height\">"%>%sub(x=.,"xmin",xmin)%>%sub(x=.,"width",width)%>%sub(x=.,"ymin",ymin)%>%sub(x=.,"height",height),"<title>West Virgnia County Map</title>","<desc>WVU Extension has offices in each of West Virginia's 55 counties.</desc>","<g id=\"state-map\">")
temp<-NULL
temp2<-NULL

for (i in 1:length(counties)) {
  
  temp<-stuff%>%filter(County==counties[i])
  center<-centroid(temp%>%select(long,lat)%>%as.matrix)
  ymid<-mytr(center[2])
  xmid<-mytr(center[1])
  county<-counties[i]
  county_name<-temp$county_name[1]
  temp2<-"<a href=\"@href\" class=\"@county\"> <polygon id=\"@county\" style=\"fill:#EAAA00;stroke:white;stroke-width:10\" data-name=\"@county\" points=\""
  for(j in 1:nrow(temp)){
    x<-mytr(temp$long[j])
    y<-mytr(temp$lat[j])
    temp2<-paste(temp2,x)%>%paste(y)
  }
  temp2<-paste(temp2,"\"><title>@county_name</title></polygon></a>",sep="")%>%gsub(x=.,"@href",href)%>%gsub(x=.,"@county_name",county_name)%>%gsub(x=.,"@xmid",xmid)%>%gsub(x=.,"@ymid",ymid)%>%gsub(x=.,"@county",county)
  code<-c(code,temp2)
}
code<-c(code,"</g><g font-size=\"70\" text-anchor=\"middle\" dominant-baseline=\"central\" fill=\"#2C2A29\">")


for (i in 1:length(counties)) {
  
  temp<-stuff%>%filter(County==counties[i])
  center<-centroid(temp%>%select(long,lat)%>%as.matrix)
  ymid<-mytr(center[2])
  xmid<-mytr(center[1])
  county<-counties[i]
  county_name<-temp$county_name[1]
  temp2<-"<a href=\"@href\" class=\"@county\"><text  x=\"@xmid\"  y=\"@ymid\"  fill=\"#2C2A29\">@county<title>@county_name</title></text></a>"%>%gsub(x=.,"@href",href)%>%gsub(x=.,"@county_name",county_name)%>%gsub(x=.,"@xmid",xmid)%>%gsub(x=.,"@ymid",ymid)%>%gsub(x=.,"@county",county)
  code<-c(code,temp2)
}
```

The following chunk is an example of how you can add custom points to
the map.

``` r
#code<-c(code,"<circle id=\"death_star\" fill=\"white\" stroke=\"#002855\" stroke-width=20 cx=\" @x \" cy=\" @y \" r=\"25\"><title>Knapp Hall</title></circle>"%>%gsub(x=.,pattern="@x",replacement=mytr(-79.957138))%>%gsub(x=.,pattern = "@y",replacement=mytr(39.632850)))
#code<-c(code,"<a href=\"https://jacksonsmill.wvu.edu\"><circle id=\"j_m\" fill=\"white\" stroke=\"#002855\" stroke-width=20 cx=\" @x \" cy=\" @y \" r=\"25\"><title>Jackson's Mill</title></circle></a>"%>%gsub(x=.,pattern="@x",replacement=mytr(-80.471051))%>%gsub(x=.,pattern = "@y",replacement=mytr(39.099658)))
#<text id="death_star" fill="white" stroke="#002855" dominant-baseline="central" text-align="center" text-anchor="middle" font-size="75" stroke-width=0 x="-79957" y="-39633">&#9733<title>Knapp Hall</title></text>
```

The last chunk exports your html map.

``` r
code<-c(code,"</g>","</svg>","</div>")
code%>%write.table("example.html",row.names = FALSE,col.names = FALSE,quote = FALSE)
```
