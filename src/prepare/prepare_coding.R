rm(list = ls())

library(knitr)
library(bibliometrix)
library(stringr)
library(data.table)
library(googledrive)
library(readxl)

# Perform bibliographic analysis
  fns <- list.files('../../data/wos_papers', pattern = 'bib', full.names=T)
  
  dir.create('../../gen/prepare/temp', recursive=T)
  bibfile = '../../gen/prepare/temp/joined.bib'
  sink(bibfile, append=F)
  sink()
  
  
  for (fn in fns) {
    print(fn)
    sink(bibfile, append=T)
    cat(paste(readLines(fn),collapse='\n'))
    sink()
  }

  papers <- convert2df(bibfile, dbsource = "wos", format = "bibtex")
  
  # filter for target years
  papers <- papers[papers$PY %in% 2004:2020,]
  
  # fill in missing DOI of article(s)
  papers$DI[grepl("THE NETWORK VALUE OF PRODUCTS", papers$TI, ignore.case=T)] <- '10.1509/jm.11.0400'

  # filter out irrelevant articles
  table(papers$DT)
  nrow(papers)
  papers <- papers[!grepl('retract|biographical|book|correction|editorial|letter', papers$DT, ignore.case=T),]
  nrow(papers)
  
  bib_analysis <- biblioAnalysis(papers, sep = ";")
  
  papers <- data.table(papers)
  
# Merge w/ our own coding
  raw_coding <- data.table(readxl::read_xlsx('../../data/coding/coding.xlsx'))
  raw_coding <- raw_coding[grepl('JB', Coder, ignore.case=T)]
  raw_coding[, DOI:=gsub('\n', '', DOI)]
  raw_coding[, Journal:=NULL]
  
  aggregators <- data.table(readxl::read_xlsx('../../data/coding/coding.xlsx', sheet = 'aggregators'))

# Citations via WOS extended
  cites = fread('../../gen/prepare/temp/wos_citations.csv')
  
  records = cites[, list(N=.N, N_shouldbe =unique(wos_totalrecords)), by = c('wos_id')]
  records[N!=N_shouldbe]
  
  records[N!=N_shouldbe]
  
  # drop two records
  cites <- cites[!wos_id%in%records[N!=N_shouldbe]$wos_id]
  
  # Complete records?
  table(papers$UT%in%gsub('WOS:', 'WOS', cites$wos_id))
  
  # Drop irrelevant cites
  cites <- cites[!grepl('Book review|correction|editorial|film|letter|meeting|abstract|news|reprint|retraction|software|(retracted)', doctype,ignore.case=T)]
  table(cites$doctype)
  
  cites[, wos_match:=gsub(':', '',wos_id)]
  
    
# Load journal coding
  journals <- data.table(readxl::read_xlsx('../../data/coding/journals_master.xlsx', sheet = 'Coding'))
  
  setkey(journals, journal)
  setkey(cites, journal)
  
  cites[journals, ':=' (is_marketing = i.Moussa_Touzani, 
                        is_ft50 = i.FT_50)]
  cites[is.na(is_marketing), is_marketing:=0]
  cites[is.na(is_ft50), is_ft50:=0]
  cites[, is_jm := grepl('^JOURNAL OF MARKETING$', journal)]
  cites[, is_jmr := grepl('^JOURNAL OF MARKETING RESEARCH$', journal)]
  cites[, is_mktsci := grepl('^MARKETING SCIENCE$', journal)]
  cites[, is_jcr := grepl('^JOURNAL OF CONSUMER RESEARCH$', journal)]
  cites[, is_ijrm := grepl('^INTERNATIONAL JOURNAL OF RESEARCH IN MARKETING$', journal)]
  
  
  # expand (i.e., include "empty" years where needed)
  tmp = cites[, list(Ncites_total = uniqueN(citing_id),
                              Ncites_marketing=uniqueN(citing_id[is_marketing==1]),
                              Ncites_ft50 = uniqueN(citing_id[is_ft50==1]),
                              Ncites_notmarketing = uniqueN(citing_id[is_marketing==0]),
                              Ncites_jm = uniqueN(citing_id[is_jm]),
                              Ncites_jmr = uniqueN(citing_id[is_jmr]),
                              Ncites_mktsci = uniqueN(citing_id[is_mktsci]),
                              Ncites_jcr = uniqueN(citing_id[is_jcr]),
                              Ncites_ijrm = uniqueN(citing_id[is_ijrm])),by=c('wos_match', 'year')]
  cites <- cites[year<=2021]
  
  empty_set = papers[, list(year=PY:2021), by = c('UT')]
  tmp=merge(tmp, empty_set, by.x=c('wos_match','year'), by.y=c('UT','year'), all.y = T)
  tmp[is.na(tmp)] = 0
  tmp[, Nyears := .N,by = c('wos_match')]
  tmp <- tmp[!wos_match%in%gsub(':', '', records[N!=N_shouldbe]$wos_id)]
  
  # cumulative metrics
  tmp_cumsum = tmp[, lapply(.SD, cumsum), by = c('wos_match')]
  tmp_cumsum[, year:=seq(from=min(year), length.out=.N),by=c('wos_match')]
  
  setnames(tmp_cumsum, paste0(colnames(tmp_cumsum), '_cum'))
  
  cites_yearly <- merge(tmp, tmp_cumsum, by.x=c('wos_match', 'year'), by.y=c('wos_match_cum', 'year_cum'), all.x=T)
  
  ### COLLAPSED
  cites_sum = cites_yearly[, lapply(.SD, sum), by = c('wos_match'), .SDcols=grep('.*cites.*[^cum]$',colnames(cites_yearly),value=T)]
  cites_avg = cites_yearly[, lapply(.SD, mean), by = c('wos_match'), .SDcols=grep('.*cites.*[^cum]$',colnames(cites_yearly),value=T)]
  setnames(cites_avg, paste0(colnames(cites_avg), '_avg'))
  
  cites_summary <- merge(cites_sum, cites_avg, by.x=c('wos_match'), by.y=c('wos_match_avg'), all.x=T)
  
  # ONE DATA SET - ALL PAPERS PLUS OUR CODING
  tmp = data.table(papers[, c('DI','PY', 'JI', 'DE', 'AB', 'PN', 'VL', 'UT', 'AU', 'TI')])
  tmp[, journal:=as.character(NA)]
  tmp[grepl('Psych', JI,ignore.case=T), journal:='JCP']
  tmp[grepl('Consum[.] Res', JI,ignore.case=T), journal:='JCR']
  tmp[grepl('Mark[.] Res', JI,ignore.case=T), journal:='JMR']
  tmp[grepl('Mark[.]$', JI,ignore.case=T), journal:='JM']
  tmp[grepl('Sci[.]$', JI,ignore.case=T), journal:='MktSci']
  
  setnames(tmp, 'AB', 'abstract_wos')
  setnames(tmp, 'PY', 'year')
  setnames(tmp, 'DE', 'keywords')
  setnames(tmp, 'VL', 'vol')
  setnames(tmp, 'PN', 'issue')
  setnames(tmp, 'UT', 'wos_id')
  setnames(tmp, 'DI', 'doi')
  setnames(tmp, 'AU', 'authors')
  setnames(tmp, 'TI', 'title_wos')
  
  # author count
  tmp[, nauthors := length(unique(unlist(strsplit(authors, ';', fixed=T)))), by = c('wos_id')]
  
  tmp[, JI:=NULL]
  
  # merge w/ citations
  impact <- merge(tmp, cites_summary, by.x='wos_id', by.y='wos_match',all.x=T, all.y=F)
 
  # merge w/ our own coding of web data papers
  coding <- merge(impact, raw_coding, by.x=c('doi'), by.y='DOI', all.x=T)
  colnames(coding) <- tolower(colnames(coding))
  coding[, web:=doi%in%raw_coding$DOI]
  
  coding[!is.na(keywords_self_assigned_missing)&web==T, keywords:=keywords_self_assigned_missing]
  
  coding[, keywords:=tolower(keywords)]
  coding[, keywords:=gsub('[,]',';', keywords)]
  
  # clean
  vars = c('cross_sectional','panel_data', 'live_scraper', 'archival_scraper',
           'geography_country (or worldwide)','textual','numeric','images', 'video',
           'scraped data_source', 'api', 'scraped', "year",
           'author_associate_professor', 'author_full_professor', 'author_practitioner',
           'author_assistant_professor', 'author_post_doc', 
           'author_phd', 'aggregator_usage') #,'other_data')
  
  coding[web==T, scraped:=as.numeric(scraped)]
  coding[web==T, api:=as.numeric(api)]
  
  #tmp = copy(coding)
  for (.v in vars) coding[is.na(get(.v)), (.v):=0]
  
  aggs <- str_trim(aggregators[aggregator==1]$source)
  
  coding[web==T, is_aggregator :=sapply(`scraped data_source`, function(x) {
      tmp = sapply(unlist(strsplit(x, ',')), function(y) tolower(str_trim(y)))
      any(tmp%in%aggs)
        })]
  coding[aggregator_usage==T&web==T, is_aggregator:=T]
  
  dir.create('../../gen/prepare/output/',recursive=T)
  fwrite(coding, '../../gen/prepare/output/coding.csv')
  fwrite(cites_summary, '../../gen/prepare/output/cites_per_paper.csv')
  fwrite(cites_yearly, '../../gen/prepare/output/cites_per_paper_per_year.csv')
  fwrite(papers, '../../gen/prepare/output/papers.csv')
  
  save(coding, cites_summary, cites_yearly, papers, bib_analysis, file= '../../gen/prepare/output/citation_database.RData')
  