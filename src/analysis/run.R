rm(list = ls())
library(knitr)
library(bibliometrix)
library(stringr)
library(data.table)

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


all_papers <- convert2df(bibfile, dbsource = "wos", format = "bibtex")

# filter for target years
all_papers <- all_papers[all_papers$PY %in% 2004:2020,]

# fill in missing DOI of article(s)
all_papers$DI[grepl("THE NETWORK VALUE OF PRODUCTS", all_papers$TI, ignore.case=T)] <- '10.1509/jm.11.0400'

# filter out irrelevant articles
table(all_papers$DT)
all_papers <- all_papers[!grepl('retract|biographical|book|correction|editorial|letter', all_papers$DT, ignore.case=T),]
nrow(all_papers)

# Run analysis
# ====================

# All papers
M <- all_papers
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/all_papers.html'))

# Web-data papers only
web_dois <- data.table(readxl::read_xlsx('../../gen/analysis/temp/coding.xlsx'))
dois <- str_trim(web_dois$DOI)
M <- all_papers[all_papers$DI%in%dois,]
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/webdata_papers.html'))

# Non-web data papers only
M <- all_papers[!all_papers$DI%in%dois,]
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/nonwebdata_papers.html'))
