rm(list = ls())
library(knitr)
library(bibliometrix)


# Marketing data
# ==============

M <- convert2df("../../data/web_data_papers/savedrecs.bib", dbsource = "wos", format = "bibtex")
# filter to <= 2020 data
M <- M[M$PY<=2020,]
webdata <- M


rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/webdata_papers.html'))


# All publication data
# ====================

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


M <- convert2df(bibfile, dbsource = "wos", format = "bibtex")

M <- M[grepl('2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020', rownames(M), ignore.case=T),]
M <- M[!grepl('retract|biographical|book|correction|editorial|letter', M$DT, ignore.case=T),]

rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/all_papers.html'))

# Remove "web data" papers
M <- M[!M$DI %in% webdata$DI,]

rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/not_webdata_papers.html'))
