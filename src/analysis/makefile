all: ../../gen/analysis/summary.html

# OVERALL BUILD RULES
../../gen/analysis/temp/citation_database.RData: prepare_citations.R
		Rscript prepare_citations.R

../../gen/analysis/output/webdata_papers.html: bib_analysis.Rmd run.R ../../gen/analysis/temp/citation_database.RData
		R --vanilla < run.R
		
../../gen/analysis/summary.html: summary.Rmd ../../gen/analysis/temp/citation_database.RData
		R -e "rmarkdown::render('summary.Rmd', output_file = '../../gen/analysis/summary.html')"