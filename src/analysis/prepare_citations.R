library(data.table)
library(ggplot2)
library(stringr)
library(bibliometrix)
library(dplyr)

cites[, wos_match:=gsub(':', '',wos_id)]

journals = cites[, list(.N),by=c('journal')]

#library(xlsx)
#write.xlsx(journals, 'journal.xlsx')

journals[, marketing:=grepl('marketing|consumer|retailing|brand|advertising|(product innovation management)|(service research)|(service management)|(service quality)', journal, ignore.case=T)]

setkey(cites, journal)
setkey(journals, journal)
cites[journals, is_marketing:=i.marketing]

cites[, harvard:=grepl('harvard',journal, ignore.case=T)]

agg_cites = cites[, list(N_marketing=uniqueN(citing_id[is_marketing==T]),Ncites=uniqueN(citing_id), n_outside=uniqueN(citing_id[is_marketing==F])),by=c('wos_match')]

agg_cites2 = cites[, list(N_marketing=uniqueN(citing_id[is_marketing==T]),Ncites=uniqueN(citing_id), n_outside=uniqueN(citing_id[is_marketing==F])),by=c('wos_match','year')]


papers=data.table(papers)
setkey(papers,UT)
setkey(agg_cites,wos_match)
setkey(papers,UT)


agg_cites[papers, doi:=i.DI]
agg_cites[, web:=doi%in%coding[web==T]$doi]

setkey(agg_cites, doi)
setkey(coding, doi)

tmp = merge(agg_cites, coding, all.x=T, all.y=F, by = c('doi'))

tmp <- tmp[!is.na(doi)]

tmp[, years:=max(1, 2021-year),by=c('doi')]

empty=rbindlist(lapply(unique(agg_cites2$wos_match), function(x) data.table(wos_match=x, year=2004:2021)))
empty2=merge(empty, agg_cites2, by=c('wos_match','year'), all.x=T)
empty2[is.na(empty2)]<-0

setkey(empty2, wos_match)
setkey(tmp, wos_match)

empty2[tmp, ':=' (web=`i.web.x`, journal=i.journal, vol=i.vol, issue=i.issue, pubyear=i.year)]
empty2 <- empty2[year>=pubyear]
agg_cites3 = data.table(empty2)
agg_cites3[, year_since:=pmax(1, year-pubyear)]



```

```{r,results='asis'}


for (.j in unique(tmp$journal)) {
  tmp[, paste0('web_', .j):=`web.x`*ifelse(journal==.j,1,0)]
}

tmp[, fe:=paste0(journal,'_', vol, '_', issue)]


m1<-lm(log((N_marketing/years)+1)~1+`web.x`+as.factor(fe), data=tmp)
m2<-lm(log((n_outside/years)+1)~1+`web.x`+as.factor(fe), data=tmp)
m3<-lm(log((Ncites/years)+1)~1+`web.x`+as.factor(fe), data=tmp)

m4<-lm(log((N_marketing/years)+1)~1+web_JM + web_JCP + web_JCR + web_JMR + web_MktSci+as.factor(fe), data=tmp)
m5<-lm(log((n_outside/years)+1)~1+web_JM + web_JCP + web_JCR + web_JMR + web_MktSci+as.factor(fe), data=tmp)
m6<-lm(log((Ncites/years)+1)~1+web_JM + web_JCP + web_JCR + web_JMR + web_MktSci+as.factor(fe), data=tmp)


library(stargazer)
stargazer(list(marketing=m1,outside_marketing=m2,total=m3),type='text', keep='web|intercept')
stargazer(list(marketing=m4,outside_marketing=m5,total=m6),type='text', keep='web|intercept')




```


```{r,results='asis'}



for (.j in unique(agg_cites3$journal)) {
  agg_cites3[, paste0('web_', .j):=`web`*ifelse(journal==.j,1,0)]
}


agg_cites3[, fe:=paste0(journal,'_', vol, '_', issue)]

# top papers
top_papers = tmp[Ncites/years>=quantile(Ncites/years, .95)]$wos_match
top_papers = tmp[`web.x`==T][Ncites/years>=quantile(Ncites/years, .99)]$wos_match


m1<-lm(log(N_marketing+1)~-1+ as.factor(year_since)+as.factor(fe), data=agg_cites3, subset=!wos_match%in%top_papers)
m2<-lm(log(n_outside+1)~-1+ as.factor(year_since)+as.factor(fe), data=agg_cites3, subset=!wos_match%in%top_papers)
m3<-lm(log(Ncites+1)~-1+ as.factor(year_since)+as.factor(fe), data=agg_cites3, subset=!wos_match%in%top_papers)

#library(stargazer)
#stargazer(list(marketing=m1,outside_marketing=m2,total=m3),type='text', keep='web|intercept|year')

# extract coefs
ex=data.table(inside=m1$coefficients, outside= m2$coefficients, total=m3$coefficients)
ex[,vars:=names(m1$coefficients)]

ex <- ex[grepl('year_since', vars)]

tmpx=melt(ex, id.vars=c('vars'))
tmpx[, years_since:=as.numeric(gsub('.*[)]','', vars))]
library(tidyverse)
tmpx %>% filter(years_since<=17) %>% filter(variable%in%c('inside','outside')) %>%
  ggplot( aes(x=years_since, y=exp(value)-1, group=variable, color=variable)) +
    geom_line()

sum(agg_cites$N_marketing)/sum(agg_cites$Ncites)

```

```{r}

# by journal

cites_per_year = rbindlist(lapply(unique(agg_cites3$journal), function(jn) {
  m1<-lm(log(N_marketing+1)~-1+ as.factor(year_since)+as.factor(fe), data=agg_cites3,subset=journal==jn)
  #m1<-lm(log(N_marketing+1)~-1+ as.factor(year_since)+as.factor(pubyear), data=agg_cites3,subset=journal==jn)
  m2<-lm(log(n_outside+1)~-1+ as.factor(year_since)+as.factor(fe), data=agg_cites3,subset=journal==jn)
  m3<-lm(log(Ncites+1)~-1+ as.factor(year_since)+as.factor(fe), data=agg_cites3,subset=journal==jn)

  # extract coefs
  ex=data.table(inside=m1$coefficients, outside= m2$coefficients, total=m3$coefficients)
  ex[,vars:=names(m1$coefficients)]
  ex <- ex[grepl('year_since', vars)]

  tmpx=melt(ex, id.vars=c('vars'))
  tmpx[, years_since:=as.numeric(gsub('.*[)]','', vars))]
  
  tmpx[, journal:=jn]
  return(tmpx)
}))

cites_per_year

library(tidyverse)
pl1<-cites_per_year %>% filter(years_since<=17) %>% filter(variable%in%c('inside')) %>%
  ggplot( aes(x=years_since, y=exp(value)-1, group=journal, color=journal)) +
    geom_line() + ggtitle("Inside citations")

pl2<-cites_per_year %>% filter(years_since<=17) %>% filter(variable%in%c('outside')) %>%
  ggplot( aes(x=years_since, y=exp(value)-1, group=journal, color=journal)) +
    geom_line() + ggtitle("Outside citations")

pl3<-cites_per_year %>% filter(years_since<=17) %>% filter(variable%in%c('total')) %>%
  ggplot( aes(x=years_since, y=exp(value)-1, group=journal, color=journal)) +
    geom_line() + ggtitle("Total citations")

gridExtra:::grid.arrange(pl1,pl2, pl3, ncol=2)


    ```
    
### Regression

```{r}

#coding[, fe:=paste0(journal,'_', year)]


for (.j in unique(coding$journal)) {
  coding[, paste0('web_', .j):=web*ifelse(journal==.j,1,0)]
}

m<-lm(log(tcperyear+1)~1 + web_JM + web_JCP + web_JCR + web_JMR + web_MktSci +as.factor(fe), data=coding)
summary(m)
exp(m$coefficients[grepl('web', names(m$coefficients),ignore.case=T)])

length(m$coefficients[!grepl('web', names(m$coefficients),ignore.case=T)])-1




m2<-lm(log(tcperyear+1)~1 + web +as.factor(fe), data=coding)
summary(m2)
exp(m2$coefficients['webTRUE'])
```

## Concentration

```{r}
head(coding$keywords_author)

cnt = tolower(str_trim(unlist(strsplit(coding[web==T]$keywords_author,';'))))
tmp=data.table(keyword=cnt)[, list(.N),by=c('keyword')]

setorder(tmp, N)

```

### Analysis
```{r}
# Overall
clean_data_source <- function(x) {
  x[grepl('googletrends', x, ignore.case=T)]<- 'Google Trends'
x[grepl('imdb', x, ignore.case=T)]<- 'IMDB'
x[grepl('amazon', x, ignore.case=T)]<- 'Amazon'
x[grepl('bn[.]com', x, ignore.case=T)]<- 'Barnes & Nobles'

  x
}
```

```{r}

get_n_keywords = function(x, topx=3) {
 res=gsub('-', ' ', as.character(x))
  
 res= (str_trim(tolower(unlist(strsplit(res, ';')))))
 res=table(res)
 res=rev(res[order(res)])
 return(paste0(names(res)[1:topx], collapse=', '))
}
  
first_platforms = coding[web==T, list(platforms=unique(str_trim(tolower(unlist(strsplit(`scraped data_source`,',')))))),
                          by = c('doi', 'year')]
first_platforms[, first:=year==min(year),by=c('platforms')]
first_platforms_agg = first_platforms[, list(first=any(first)),by=c('doi')]

setkey(first_platforms_agg, doi)
setkey(coding, doi)
coding[first_platforms_agg, first_time:=i.first]

coding[, first_web:=min(year[web==T]),by=c('journal')]

growth = rbindlist(lapply(c('first','last'), function(.half) {
  coding[, filter:=year>=first_web & year<=2012]
  if (.half=='last') coding[, filter:=year>=first_web &year>2012]
  
  gr = coding[filter==T, list(publications = .N, years = uniqueN(year),
                       publications_web = length(which(web==T)),
              first_pub=unique(first_web)),
                by=c('journal')][, half:=.half]
  return(gr)
}))
  
growth[, ':=' (avgoutput=publications/years, avgoutput_web = publications_web/years)]

gr=growth[, list(growth_all = avgoutput[half=='last']/avgoutput[half=='first'],
              growth_web = avgoutput_web[half=='last']/avgoutput_web[half=='first']), by = c('journal')]

setkey(gr, journal)
setkey(coding, journal)
coding[gr, ':=' (growth_all = i.growth_all,
                 growth_web = i.growth_web,
                 growth_index = i.growth_web/i.growth_all)]

  
#top_keywords_first= get_n_keywords(as.character(keywords_author[web==T & year <=2012]))

platform_count = coding[web==T, list(platforms=unique(str_trim(tolower(unlist(strsplit(`scraped data_source`,',')))))),
                          by = c('doi', 'journal')]
top_platforms= platform_count[, list(N=.N),by=c('journal','platforms')]
top_platforms[, marketshare:=(N/sum(N)),by=c('journal')]
top_platforms[, rank:=rank(-marketshare, ties.method='min'),by=c('journal')]

setkey(platform_count, journal, platforms)
setkey(top_platforms, journal, platforms)

platform_count[top_platforms, platform_rank:=i.rank]
platform_count[, ':=' (top3 = any(platform_rank<=3),
                       top5 = any(platform_rank<=5),
                       n_sources = uniqueN(platforms)),by=c('doi')]

setkey(platform_count, doi)
setkey(coding, doi)

coding[platform_count, ':=' (top3_platform = i.top3,
                             top5_platform = i.top5,
                             n_sources=i.n_sources)]



first_platforms[, first:=year==min(year),by=c('platforms')]
first_platforms_agg = first_platforms[, list(first=any(first)),by=c('doi')]
         

aggs <- rbindlist(lapply(c('journal','no'), function(.by) {
  coding[, no:='total']
  
tmp = coding[, list(N=length(which(web==T)), first_web_paper = min(year[web==T]),
                  avg_papers_per_year = length(which(web==T))/(2020-min(year[web==T])+1),
                  growth = mean(growth_web),
                  growth_index = mean(growth_index),
                  share_juniors=round(100 * length(which(author_assistant_professor+author_post_doc+author_phd>0))/length(which(web==T)),0),
                  
                  cites_per_year = median(tcperyear[web==T]),
                  cites_per_year_indexed = median(tcperyear[web==T])/median(tcperyear),
                  
                  share_multiplesource = 100-round(100*length(which(n_sources[web==T]==1))/length(which(web==T)),0), 
                  
                  share_top5 = round(mean(100*top5_platform[web==T]),0),
                 
                  share_worldwide = round(100*length(which(grepl('worldwide', `geography_country (or worldwide)`,ignore.case=T)))/length(which(web==T)),0),
                  share_northamerica = round(100*length(which(grepl('US|canada', `geography_country (or worldwide)`,ignore.case=T)))/length(which(web==T)),0),
                  share_europe = round(100*length(which(grepl('eu|ger|uk|germany|italy|sweden|france|austria|switz|denmark|netherl|poland|luxem|belgium|czech', `geography_country (or worldwide)`, ignore.case=T)))/length(which(web==T)),0),
                  share_asia = round(100*length(which(grepl('china|korea|japan', `geography_country (or worldwide)`, ignore.case=T)))/length(which(web==T)),0),
                  share_other = round(100*length(which(grepl('N[/]A|English[-]speaking', `geography_country (or worldwide)`, ignore.case=T)))/length(which(web==T)),0),
                  
                #  top_keywords_first= get_n_keywords(as.character(keywords_author[web==T & year <=2012])),
                #  top_keywords_late= get_n_keywords(as.character(keywords_author[web==T & year >2012])),
                  
                  
                 
                  share_textual = round(100*length(which(textual==1))/length(which(web==T)),0),
                  share_numeric = round(100*length(which(numeric==1))/length(which(web==T)),0),
                  share_images = round(100 *length(which(images==1))/length(which(web==T)),0),
                  share_video = round(100*length(which(video==1))/length(which(web==T)),0),
                  share_aggregator = round(100 * length(which(is_aggregator))/length(which(web==T)),0),
                  
                  #share_scraped = round(100* length(which(scraped==1&api==0))/length(which(web==T)),0),
                  share_api = round(100*length(which(api==1&scraped==0))/length(which(web==T)),0),
                  #share_scraped_api = length(which(scraped==1&api==1))/length(which(web==T)),
                  
                  share_live = round(100*length(which(live_scraper==T))/length(which(web==T)),0)
                  
                  #share_US_any = length(which(grepl('US', `Geography_Country (or worldwide)`)))/.N
                  ), by = .by]
if (.by=='no') setnames(tmp, 'no', 'journal')
  tmp
}))

journals = c('JM','JMR','JCR','JCP','MktSci')
tmp2 = dcast(melt(aggs, id.vars=c('journal')), variable~journal)
setcolorder(tmp2, c('variable', journals,  'total'))

top_platforms[, text:=paste0(str_to_title(platforms), ' (', N, ')')]
setorder(top_platforms, journal, rank)

top_tmp =top_platforms[, list(Sources=paste0(text[rank<=10],collapse=', ')), by = c('journal')]
top_tmp=top_tmp[match(journals, top_tmp$journal),]

# Notes:
# 
# focal variables of interest
# --> add avg. number of platforms
# add platform concentration (make clean column)
# keyword distrubtion + keyword concentration index
# Hannes checks web of science to compare author juniors
# relative impact (impact of paper, relative to all papers in the journal)
# Highest cited papers, per year + references
# Illustrative papers, highest-impact research, by year


library(knitr)
kable(tmp2,digits=2, caption = 'Web data in marketing research')
# -PhD/Assistant/Postdocs
kable(top_tmp, caption = 'Top 10 Sources by Journal')

# -uniq platforms / number of papers (excluding "outliers")
# - source concentration top 3 share, top 5 share (herfindahl). 
# - names of top 3 sources

```


```{r}

tmp = platform_count[, list(N=uniqueN(doi)),by=c('platforms')]
setorderv(tmp, 'N', order=-1L)


tmp[, text:=paste0(str_to_title(platforms), ' (', N, ')')]
tmp[, rank:=1:.N]

tmp[1:30, c('rank','text'),with=F]

coding[web==T, list(1-mean(api))]

```

```{r}
# JOURNAL TIME GRAPH
timing = coding[web==T, list(.N),by=c('journal', 'year')]


timing %>%
  mutate(journal = factor(journal, levels = c("JCP", "JCR", "MktSci", "JMR", "JM"))) %>%
  ggplot(aes(year, N)) +
  geom_bar(stat = 'identity', aes(fill=journal)) +
  labs(#caption = 
#"Notes: Number of marketing research articles per year from 2004 to 2020. Included journals are Marketing Science 
#(MktSci), Journal of Marketing Research (JMR), Journal of Marketing (JM), Journal of Consumer Research (JCM), 
#Journal of Consumer Psychology (JCP).",
       fill = "Journal",
       x = "Year",
       y = "Number of Articles") +
  scale_fill_manual(values = c("gray30", "grey85", "gray60", "gray40", "gray1")) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.2, face = "italic"),
        axis.title = element_text(face="bold", size = "13"),
        legend.title = element_text(face="bold", size = "13"),
        axis.text.x = element_text(size="8.5")) +
  scale_x_discrete(limits=c(2004:2020)) + 
  guides(fill = guide_legend(reverse=T))
dir.create('../../gen/analysis/output', recursive = T)
ggsave('../../gen/analysis/output/plot.png')

```

