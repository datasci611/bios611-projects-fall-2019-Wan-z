## Project 3 Analysis on UMD Data

Wan Zhang 730341932

Project 3 is a complete analysis on datasets provided by Urban Ministries of Durham, adopting Python, R, Make and Docker. To run the project in proj_3 directory, 

```
make results/report.html -f Makefile
```

And the final report is lying in the results/ directory.

#### Background

The datasets are provided by Urban Ministries of Durham, demonstrating the information of clients they provided support to from 2014 by now. [Urban Ministries of Durham](https://umdurham.org/who-we-are.html "Markdown") is a non-profit organization working to end homelessness and meet the emergency needs of very poor and hungry neighbors through its programs. Unlike the date for project 1, we can only the the information of clients instead of UMD services. The purpose of this project is analyzing the data with R and python to provide some visualized result to assist UMD to provide support more effectively.

#### Data Description

The datasets are provided by Urban Ministries of Durham, demonstrating the information of clients they provided support to from 2014 by now. The original data contains tens of data sets and this project mainly use CLIENT_191102.tsv, DISABILITY_ENTRY_191102.tsv, data/DISABILITY_EXIT_191102.tsv, ENTRY_EXIT_191102.tsv, HEALTH_INS_ENTRY_191102.tsv, HEALTH_INS_EXIT_191102.tsv, INCOME_ENTRY_191102.tsv, INCOME_EXIT_191102.tsv.

#### Questions to Answer

* How many times the clients come to UMD? How long they stay? 

Summary on their information (distribution of their disability status, health insurance and income, age). How these variables related to the frequency they come to UMD and how long they stayed?

* What Happened Before the Clients Entry & After They Exit?

Analysis on the destination after clients leave and homeless status before they entry, as well as how it been influenced by variables of prior living situation and domestic violence.

* How the Amount of Clients and Support Cases Change over Years

Group the data by year and figure out the distribution of number of clients and cases.