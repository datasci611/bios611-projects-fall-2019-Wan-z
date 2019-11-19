FROM rocker/verse:latest

# Set a user and the working directory
USER rstudio
WORKDIR /report

# Set the container to run `Rscript --vanilla ` by default
ENTRYPOINT ["/usr/local/bin/Rscript", "--vanilla"]

CMD ["report.R", "CLIENTS.csv", "CASE.csv", "number.csv"]