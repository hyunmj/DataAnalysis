install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("scales")
install.packages('rJava')
install.packages('memoise')
isstall.packages("KoNLP")

library(rvest)
library(dplyr)
library(stringr)

library(rJava)
library(memoise)
library(KoNLP)

library(ggplot2)
library(scales)

useNIADic()


title='경부고속도로 졸음운전'
keyword=str_replace_all(title, ' ', '%20')

basic_url='https://search.naver.com/search.naver?ie=utf8&where=news&query=keyword&sort=0&pd=3'
basic_url=str_replace_all(basic_url, 'keyword', keyword)
urls=NULL

presslist=c('연합뉴스', '뉴스1', '뉴시스', '세계일보', '경향신문', 
            '머니투데이', 'SBS 뉴스', 'MBC 뉴스', 'YTN', 'KBS 뉴스', 
            '이데일리', '매일경제', 'JTBC', '연합뉴스TV', '노컷뉴스',
            'TV조선', '채널A', '한겨레', '중앙SUNDAY', '디지털데일리',
            '서울신문', '머니S', '헤럴드경제', '서울경제', '파이낸셜뉴스',
            '한국일보')


sago=NULL
samang=NULL
balsaeng=NULL
daechaek=NULL
balui=NULL
gaejung=NULL
ganghwa=NULL
gaesun=NULL
gaejungan=NULL
gagyul=NULL
jasal=NULL
ganggan=NULL
sanghae=NULL
salhae=NULL
pokhaeng=NULL
narticles=NULL
jinsang=NULL
chotbul=NULL
jejung=NULL
bub=NULL
susaek=NULL
chimmol=NULL

#날짜
start_date=as.Date('2017-07-09')
end_date=as.Date('2017-09-28')

temp=start_date
temp
while (temp!=end_date+1){
  date=as.character(temp,format='%Y.%m.%d')
  str=paste0(basic_url,'&ds=')%>%
    paste0(date)%>%
    paste0('&de=')%>%
    paste0(date)%>%
    paste0('&start=')
  
  basic_url_2=str
  basic_url_2
  
  temp=temp+1
  
  html=read_html(basic_url_2)
  html
  
  num_articles=html%>%
    html_nodes('.section_head')%>%
    html_nodes('.title_desc')%>%
    html_nodes('span')%>%
    html_text()
  
  num_articles=num_articles%>%
    str_replace_all('\\s','')%>%
    str_replace_all('-','')%>%
    str_replace_all('[0-9]+/','')%>%
    str_replace_all('건', '')%>%
    str_replace_all('[:punct:]','')
  
  num_articles=as.numeric(num_articles)
  num_articles
  
  if(length(num_articles)==0) {
    sago=c(sago, 0)
    samang=c(samang, 0)
    daechaek=c(daechaek, 0)
    balui=c(balui, 0)
    balsaeng=c(balsaeng, 0)
    gaejung=c(gaejung,0)
    ganghwa=c(ganghwa,0)
    gaesun=c(gaesun,0)
    gaejungan=c(gaejungan,0)
    gagyul=c(gagyul,0)
    jasal=c(jasal,0)
    ganggan=c(ganggan,0)
    sanghae=c(sanghae,0)
    salhae=c(salhae,0)
    pokhaeng=c(pokhaeng,0)
    jinsang=c(jinsang,0)
    chotbul=c(chotbul,0)
    jejung=c(jejung,0)
    bub=c(bub,0)
    susaek=c(susaek,0)
    chimmol=c(chimmol,0)
    
    narticles=c(narticles,0)
    
    next
  }
  
  num_page=num_articles%/%10
  num_page
  
  urls=NULL
  
  for(x in 0:num_page) {
    urls[x+1]=paste0(basic_url_2, x*10+1)
  }
  
  urls
  naver_links=NULL
  url
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
  
  if(length(naver_links)==0) {
    sago=c(sago, 0)
    samang=c(samang, 0)
    daechaek=c(daechaek, 0)
    balui=c(balui, 0)
    balsaeng=c(balsaeng, 0)
    gaejung=c(gaejung,0)
    ganghwa=c(ganghwa,0)
    gaesun=c(gaesun,0)
    gaejungan=c(gaejungan,0)
    gagyul=c(gagyul,0)
    jasal=c(jasal,0)
    ganggan=c(ganggan,0)
    sanghae=c(sanghae,0)
    salhae=c(salhae,0)
    pokhaeng=c(pokhaeng,0)
    jinsang=c(jinsang,0)
    chotbul=c(chotbul,0)
    jejung=c(jejung,0)
    bub=c(bub,0)
    susaek=c(susaek,0)
    chimmol=c(chimmol,0)
    
    narticles=c(narticles,0)
    
    next
  }
  
  write.csv(naver_links, 'naver_links.csv')
  read.csv('naver_links.csv')
  txts=NULL
  
  for(link in naver_links) {
    ht=read_html(link)
    
    press=ht%>%
      html_nodes('.article_header')%>%
      html_nodes('.press_logo')%>%
      html_nodes('img')%>%
      html_attr('alt')
    
    if(length(press)==0){
      press='no'
    }
    
    if(!is.element(press, presslist)) {
      next
    }
    
    tmp=repair_encoding(html_text(html_nodes(ht,'#articleBodyContents')),from='utf-8')
    
    txts=c(txts,tmp)
  }
  txts=unique(txts)
  narticles=c(narticles, length(txts))
  
  if(length(txts)==0) {
    sago=c(sago, 0)
    samang=c(samang, 0)
    daechaek=c(daechaek, 0)
    balui=c(balui, 0)
    balsaeng=c(balsaeng, 0)
    gaejung=c(gaejung,0)
    ganghwa=c(ganghwa,0)
    gaesun=c(gaesun,0)
    gaejungan=c(gaejungan,0)
    gagyul=c(gagyul,0)
    jasal=c(jasal,0)
    ganggan=c(ganggan,0)
    sanghae=c(sanghae,0)
    salhae=c(salhae,0)
    pokhaeng=c(pokhaeng,0)
    jinsang=C(jinsang,0)
    chotbul=c(chotbul,0)
    jejung=c(jejung,0)
    bub=c(bub,0)
    susaek=c(susaek,0)
    chimmol=c(chimmol,0)
    
    
    
    #narticles=c(narticles,0)
    next
  }
  
  #sample=txts[1:100]
  #sample
  write.csv(txts,'naver_contents.csv', row.names=F, na='')
  #write.csv(sample, 'sample.csv',row.names=F,na="")
  
  
  
  data=read.csv('naver_contents.csv', stringsAsFactors = F)
  data
  
  data=str_replace_all(data[,1], '\n', '')
  data=str_replace_all(data, '\t','')
  data=str_replace_all(data, '[A-Za-z]','')
  data=str_replace_all(data, '[:punct:]',' ')
  data=str_replace_all(data, '오류를 우회하기 위한 함수 추가', '')
  data=str_replace_all(data, '[0-9]', '')
  data=str_replace_all(data, '무단전재 및 재배포 금지','')
  data=str_replace_all(data, '[가-힣]씨', '')
  #data=str_replace_all(data, '졸음운전', '')
  #data=str_replace_all(data, '경부고속도로', '')
  data
  
  nouns=extractNoun(data)
  wordcount=table(unlist(nouns))
  wordcount
  
  df_word=as.data.frame(wordcount,stringsAsFactors = F)
  
  df_word=rename(df_word, word=Var1, freq=Freq)
  df_word=filter(df_word, nchar(word)>=2)
  df_word
  
  if(nrow(subset(df_word, word=='사고'))==0) {
    sago=c(sago, 0)
  }
  else {
    sago=c(sago, subset(df_word, word=='사고')$freq)
  }
  
  if(nrow(subset(df_word, word=='사망'))==0) {
    samang=c(samang, 0)
  }
  else {
    samang=c(samang, subset(df_word, word=='사망')$freq)
  }
  
  if(nrow(subset(df_word, word=='대책'))==0) {
    daechaek=c(daechaek, 0)
  }
  else {
    daechaek=c(daechaek, subset(df_word, word=='대책')$freq)
  }
  
  if(nrow(subset(df_word, word=='발의'))==0) {
    balui=c(balui, 0)
  }
  else {
    balui=c(balui, subset(df_word, word=='발의')$freq)
  }
  
  if(nrow(subset(df_word, word=='발생'))==0) {
    balsaeng=c(balsaeng, 0)
  }
  else {
    balsaeng=c(balsaeng, subset(df_word, word=='발생')$freq)
  }
  
  if(nrow(subset(df_word, word=='개정'))==0) {
    gaejung=c(gaejung, 0)
  }
  else {
    gaejung=c(gaejung, subset(df_word, word=='개정')$freq)
  }
  
  if(nrow(subset(df_word, word=='개정안'))==0) {
    gaejungan=c(gaejungan, 0)
  }
  else {
    gaejungan=c(gaejungan, subset(df_word, word=='개정안')$freq)
  }
  
  if(nrow(subset(df_word, word=='강화'))==0) {
    ganghwa=c(ganghwa, 0)
  }
  else {
    ganghwa=c(ganghwa, subset(df_word, word=='강화')$freq)
  }
  
  if(nrow(subset(df_word, word=='개선'))==0) {
    gaesun=c(gaesun, 0)
  }
  else {
    gaesun=c(gaesun, subset(df_word, word=='개선')$freq)
  }
  
  if(nrow(subset(df_word, word=='가결'))==0) {
    gagyul=c(gagyul, 0)
  }
  else {
    gagyul=c(gagyul, subset(df_word, word=='가결')$freq)
  }
  
  if(nrow(subset(df_word, word=='자살'))==0) {
    jasal=c(jasal, 0)
  }
  else {
    jasal=c(jasal, subset(df_word, word=='자살')$freq)
  }
  
  if(nrow(subset(df_word, word=='강간'))==0) {
    ganggan=c(ganggan, 0)
  }
  else {
    ganggan=c(ganggan, subset(df_word, word=='강간')$freq)
  }
  
  if(nrow(subset(df_word, word=='상해'))==0) {
    sanghae=c(sanghae, 0)
  }
  else {
    sanghae=c(sanghae, subset(df_word, word=='상해')$freq)
  }
  
  if(nrow(subset(df_word, word=='살해'))==0) {
    salhae=c(salhae, 0)
  }
  else {
    salhae=c(salhae, subset(df_word, word=='살해')$freq)
  }
  
  if(nrow(subset(df_word, word=='폭행'))==0) {
    pokhaeng=c(pokhaeng, 0)
  }
  else {
    pokhaeng=c(pokhaeng, subset(df_word, word=='폭행')$freq)
  }
  
  if(nrow(subset(df_word, word=='진상규명'))==0) {
    jinsang=c(jinsang, 0)
  }
  else {
    jinsang=c(jinsang, subset(df_word, word=='진상규명')$freq)
  }
  
  if(nrow(subset(df_word, word=='촛불'))==0) {
    chotbul=c(chotbul, 0)
  }
  else {
    chotbul=c(chotbul, subset(df_word, word=='촛불')$freq)
  }
  
  if(nrow(subset(df_word, word=='제정'))==0) {
    jejung=c(jejung, 0)
  }
  else {
    jejung=c(jejung, subset(df_word, word=='제정')$freq)
  }
  
  if(nrow(subset(df_word, word=='수색'))==0) {
    susaek=c(susaek, 0)
  }
  else {
    susaek=c(susaek, subset(df_word, word=='수색')$freq)
  }
  
  if(nrow(subset(df_word, word=='침몰'))==0) {
    chimmol=c(chimmol, 0)
  }
  else {
    chimmol=c(chimmol, subset(df_word, word=='침몰')$freq)
  }
  
}


a=1:82
a
for ( i in a) {
  x[i]=as.character(start_date+i-1)
}
x

df=data.frame(Dates=seq.Date(start_date, end_date, by=1), narticles, sago, samang, balui, daechaek, balsaeng,gaejung,gaejungan,gaesun,ganghwa,stringsAsFactors = F)
str(df)
str(df)
df

filename=paste0(title,'.csv')
write.csv(df, filename, row.names=F)

a=NULL
b=NULL
c=NULL
d=NULL
e=NULL
c=df$daechaek+df$gaesun+df$ganghwa+df$gaejung+df$gaejungan+df$balui
c

a=df$gaesun+df$daechaek+df$ganghwa
b=df$balui+df$gaejung+df$gaejungan

d=df$samang+chimmol+susaek
e=jinsang+chotbul+jejung
str(df)

ggplot(df, aes(Dates))+ggtitle(title)+
  geom_line(aes(y=narticles, colour='네이버 기사 수'),size=1)+
  geom_line(aes(y=df$sago, colour='사고'),size=1)+
  #geom_line(aes(y=balui, colour='발의'),size=1)+
  #geom_line(aes(y=a, colour='대책+개선+강화'),size=1)+
  #geom_line(aes(y=b, colour='개정+개정안+발의'),size=1)+
  #geom_line(aes(y=e, colour='진상규명+촛불+제정'),size=1)+
  geom_line(aes(y=c, colour='대책+개선+강화+개정+개정안+발의'),size=1)+
  scale_x_date(date_breaks='5 day')+labs(x='날짜', y='단어 등장 횟수')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(axis.title=element_text(face='bold',size=15))+
  theme(legend.text=element_text(size=17, hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.position=c(0.5,0.75))+
  theme(plot.title=element_text(face='bold', hjust=0.5, size=20))




