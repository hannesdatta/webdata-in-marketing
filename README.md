# An Analysis Into the Use of Web Scraping and APIs in Academic Marketing Research

This repository empirically investigates the use of web data (i.e., collected using web scraping or APIs) in academic marketing research.

## Bibliographic analysis

- Included Journals: Journal of Marketing, Journal of Marketing Research, Journal of Consumer Research, Journal of Consumer Psychology, Marketing Science
- We conduct a bibliographic analysis on two datasets:
  - Papers using web data, as identified in `data/webdata_papers/webdata_papers.xlsx`, and
  - Any paper published in the journal.
- Focal time period: 2004 - 2020

## Dependencies
- [R](https://tilburgsciencehub.com/get/R) 
- R packages: 
	`install.packages("stargazer", "bibliometrix")`
- [GNU Make](https://tilburgsciencehub.com/get/make)

## Running instructions

1. Run the make file in `src/analysis`; alternatively, run `run.R`.
2. The resulting bibliometric analyses are available in `gen/analysis/output`.