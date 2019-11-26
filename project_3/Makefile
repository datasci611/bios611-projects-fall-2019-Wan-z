script/proj3_wrangling.py: script/proj3_wrangling.ipynb
	jupyter nbconvert --to python script/proj3_wrangling.ipynb

pull-pandas:
	docker pull amancevice/pandas:0.25.2-slim && touch $@


CLIENTS.csv: pull-pandas \
							script/proj3_wrangling.py
	docker run -it \
		--rm -v $(shell pwd):/workdir \
		-w /workdir \
		amancevice/pandas:0.25.2-slim \
			python script/proj3_wrangling.py \
			script/CLIENTS.csv \
			

CASE.csv: pull-pandas \
							script/proj3_wrangling.py
	docker run -it \
		--rm -v $(shell pwd):/workdir \
		-w /workdir \
		amancevice/pandas:0.25.2-slim \
			python script/proj3_wrangling.py \
			script/CASE.csv \

number.csv: pull-pandas \
							script/proj3_wrangling.py	
	docker run -it \
		--rm -v $(shell pwd):/workdir \
		-w /workdir \
		amancevice/pandas:0.25.2-slim \
			python script/proj3_wrangling.py \
			script/number.csv \

results/report.html: results/report.Rmd \
			CLIENTS.csv \
			CASE.csv \
			number.csv
	docker run -it --rm \
	-v $(shell pwd):/workdir \
	-w /workdir rocker/tidyverse \
	Rscript -e "library(rmarkdown); rmarkdown::render('results/report.Rmd', 'html_document')"
