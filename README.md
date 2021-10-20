# An Analysis Into the Use of Web Scraping and APIs in Academic Marketing Research

This repository empirically investigates the use of web data (i.e., collected using web scraping or APIs) in academic marketing research.

## Bibliographic analysis

- Included Journals: *Journal of Marketing*, *Journal of Marketing Research*, *Journal of Consumer Research*, *Journal of Consumer Psychology*, *Marketing Science*
- This repository largely consists of the following datasets:
  - Web data papers
    - Papers using web data (`data/web_data_papers/webdata_papers.xlsx`), accompanied by meta characteristics collected by a team of Research Assistants (`data/web_data_papers/coding.xlsx`).
  - All papers in the journals
    - As a benchmark for bibliographic analysis
- The focal time period is 2004 - 2020


## Dependencies
- [R](https://tilburgsciencehub.com/get/R) 
- R packages: 
	`install.packages("stargazer", "bibliometrix")`
- [GNU Make](https://tilburgsciencehub.com/get/make)

## Running instructions

1. Run the make file in `src/analysis`; alternatively, run `run.R`.
2. The resulting bibliometric analyses are available in `gen/analysis/output`.