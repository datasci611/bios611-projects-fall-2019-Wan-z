data-wrangling: Dockerfile.data
	docker build -t data-wrangling . -f Dockerfile.data

CLIENTS.csv: data-wrangling
	docker run --rm -v ${PWD}:/data data-wrangling

CASE.csv: data-wrangling
	docker run --rm -v ${PWD}:/data data-wrangling

number.csv: data-wrangling
	docker run --rm -v ${PWD}:/data data-wrangling

report-maker: Dockerfile
	docker build -t report-maker . -f Dockerfile

report.html : report-maker
	docker run --rm -v $PWD:/report report-maker

