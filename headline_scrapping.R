install.packages("rvest")
install.packages("dplyr")

library(rvest)
library(dplyr)

basic_url='https://search.naver.com/search.naver?ie=utf8&where=news&query=경부고속도로%20졸음운전&sort=0&pd=3&ds=2017.07.09&de=2017.09.28&nso=so:r,p:from20170709to20170928,a:all&start='

urls=NULL

for(x in 0:204) {
  urls[x+1]=paste0(basic_url, x*10+1)
}

urls
titles=NULL

for (url in urls){
  h=read_html(url)
  
    
  titles=c(titles,h%>%html_nodes('.type01')%>%
      html_nodes('a')%>%
      html_attr('title'))
    
    
  
}

titles=unique(titles)
titles

write.csv(titles, 'titles.csv')



