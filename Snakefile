import os

os.makedirs("docs", exist_ok=True)
os.makedirs("data", exist_ok=True)

rule all:
	input:
		"docs/index.html",
		"data/blanket2.csv"

rule render:
	input:
		"scripts/scrape_temperature.Rmd"
	output:
		"docs/index.html",
		"data/blanket2.csv"
	shell:
		"""
		cd scripts
		Rscript -e "rmarkdown::render('scrape_temperature.Rmd', output_dir='../docs', output_file='index.html')"
		"""
