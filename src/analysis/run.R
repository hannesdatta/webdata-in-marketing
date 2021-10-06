rm(list = ls())
library(knitr)
library(bibliometrix)


# Marketing data
# ==============

M <- convert2df("../../data/web_data_papers/savedrecs.bib", dbsource = "wos", format = "bibtex")
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
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/all_papers.html'))
