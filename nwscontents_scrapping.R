install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")

library(rvest)
library(dplyr)
library(stringr)

basic_url='https://search.naver.com/search.naver?ie=utf8&where=news&query=경부고속도로%20졸음운전&sort=0&pd=3&ds=2016.01.01&de=2017.09.28&nso=so:r,p:from20160101to20170928,a:all&start='

urls=NULL

for(x in 0:204) {
  urls[x+1]=paste0(basic_url, x*10+1)
}

urls
naver_links=NULL

for (url in urls){
  h=read_html(url)
  
  
  naver_links=c(naver_links,h%>%html_nodes('.type01')%>%
                  html_nodes('.txt_inline')%>%
             html_nodes('a')%>%
             html_attr('href')%>%
               unique())
  
  naver_links=c(naver_links,h%>%html_nodes('.type01')%>%
                  html_nodes('.txt_sinfo')%>%
                  html_nodes('a')%>%
                  html_attr('href')%>%
                  unique())
  
      
  
  naver_links=naver_links[grep('news.naver.com',naver_links)]
  
}

naver_links=unique(naver_links)
naver_links

write.csv(naver_links, 'naver_links.csv')
read.csv('naver_links.csv')
txts=NULL

for(link in naver_links) {
  html=read_html(link)
  temp=repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from='utf-8')
  
  txts=c(txts,temp)
}
txts=unique(txts)

sample=txts[1:100]
sample
write.csv(txts,'naver_contents.csv', row.names=F, na='')
write.csv(sample, 'sample.csv',row.names=F,na="")



