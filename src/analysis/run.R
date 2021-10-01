rm(list = ls())
library(knitr)
library(bibliometrix)

M <- convert2df("../../data/web_data_papers/savedrecs.bib", dbsource = "wos", format = "bibtex")
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/webdata.html'))

if(0) {
  
fns <- list.files('../../data/all_papers', pattern = 'bib', full.names=T)

bibfile = '../../gen/analysis/temp/joined.bib'
sink(bibfile, append=F)
sink()

for (fn in fns) {
  print(fn)
  test<-scan(fn, what='character', sep ='')
  sink(bibfile, append=T)
  cat(paste(test,collapse='\n'))
  sink()
}

Ms <- lapply(fns, function(fn) {
  print(fn)
  convert2df(fn, dbsource = "wos", format = "bibtex")
})

M_big=rbindlist(lapply(Ms, function(x) data.table(x)), fill=T)

fwrite(M_big, file = '../../gen/analysis/temp/wos_new.csv')

M3 = convert2df('../../gen/analysis/temp/wos_new.csv', dbsource = "wos", format = "bibtex")


M_big <- do.call('rbind', lapply(Ms, function(x) x))

}
