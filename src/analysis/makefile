# Notes: 
# - If run on unix system, use rm instead of del command in clean  
# - Careful with spaces! If use \ to split to multiple lines, cannot have a space after \ 

# OVERALL BUILD RULES
../../gen/analysis/output/webdata.html ../../gen/analysis/output/all_papers.html: bib_analysis.Rmd run.R
		R --vanilla < run.R