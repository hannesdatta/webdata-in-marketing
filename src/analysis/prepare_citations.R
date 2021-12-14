rm(list = ls())

library(knitr)
library(bibliometrix)
library(stringr)
library(data.table)
library(googledrive)
library(readxl)

# Perform bibliographic analysis
  fns <- list.files('../../data/all_papers', pattern = 'bib', full.names=T)
  
  bibfile = '../../gen/analysis/temp/joined.bib'
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
  papers <- papers[!grepl('retract|biographical|book|correction|editorial|letter', papers$DT, ignore.case=T),]
  nrow(papers)
 
  bib_analysis <- biblioAnalysis(papers, sep = ";")
  
# Merge w/ our own coding
  drive_download(as_id('1TU1q_jXhUXsVcyxCr9fYGD9mK6j3he5f'), path = '../../gen/analysis/temp/coding.xlsx', overwrite=T)

  raw_coding <- data.table(readxl::read_xlsx('../../gen/analysis/temp/coding.xlsx'))
  raw_coding <- raw_coding[grepl('JB', Coder, ignore.case=T)]
  raw_coding[, DOI:=gsub('\n', '', DOI)]
  raw_coding[, Journal:=NULL]
  
  # remove docs without DOIs
  impact = data.table(bib_analysis$MostCitedPapers)
  setkey(impact, DOI)
  
  tmp = data.table(papers[, c('DI','PY', 'JI', 'DE')])
  tmp[, journal:=as.character(NA)]
  tmp[grepl('Psych', JI,ignore.case=T), journal:='JCP']
  tmp[grepl('Consum[.] Res', JI,ignore.case=T), journal:='JCR']
  tmp[grepl('Mark[.] Res', JI,ignore.case=T), journal:='JMR']
  tmp[grepl('Mark[.]$', JI,ignore.case=T), journal:='JM']
  tmp[grepl('Sci[.]$', JI,ignore.case=T), journal:='MktSci']
  
  impact[tmp, ':=' (year=i.PY, journal = i.journal, keywords=i.DE)]
  
  # author count
  setkey(tmp, DI)
  papers$id <- 1:nrow(papers)
  authors = data.table(papers)[, list(nauthors = sapply(AU, function(x) length(unique(unlist(strsplit(x, ';', fixed=T)))))), by = c('id', 'DI')]
  authors <- authors[!is.na(DI)]
  setkey(authors, DI)
  impact[authors, nauthors:=i.nauthors]
  
  # merge w/ our own coding of web data papers
  coding <- merge(impact, raw_coding, by.x=c('DOI'), by.y='DOI', all.x=T)
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
           'author_phd') #,'other_data')
  
  coding[web==T, scraped:=as.numeric(scraped)]
  coding[web==T, api:=as.numeric(api)]
  #tmp = copy(coding)
  for (.v in vars) coding[is.na(get(.v)), (.v):=0]
  
  save(coding, papers, bib_analysis, file= '../../gen/analysis/temp/citation_database.RData')
  