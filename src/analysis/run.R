rm(list = ls())
library(knitr)
library(bibliometrix)
library(stringr)
library(data.table)


load(file= '../../gen/analysis/temp/citation_database.RData')

# Run analysis
# ====================

# All papers
M <- papers
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/all_papers.html'))

# Web-data papers only
web_dois <- data.table(readxl::read_xlsx('../../gen/analysis/temp/coding.xlsx'))
dois <- str_trim(web_dois$DOI)
M <- papers[papers$DI%in%dois,]
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/webdata_papers.html'))

# Non-web data papers only
M <- papers[!papers$DI%in%dois,]
rmarkdown::render('bib_analysis.Rmd', output_file=paste0('../../gen/analysis/output/nonwebdata_papers.html'))
