FROM rocker/verse:latest

# Set a user and the working directory
USER rstudio
WORKDIR /report

# Set the container to run `Rscript --vanilla ` by default
ENTRYPOINT ["/usr/local/bin/Rscript", "--vanilla"]

# Set the `example_report.R data.csv` as the default script to run with ENTRYPOINT's Rscript
CMD ["report.R", "../data/CLIENTS.csv", "../data/CASE.csv", "../data/number.csv"]