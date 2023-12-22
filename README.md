# Clinical Trial Dashboard

Objective: create interactive dashboard in R Shiny to keep track of clinical trial progress

Specifics: 
- screening and enrollment: keep track of patient screening / enrollment counts, summarize exclusion reasons 
- treatment assignment: randomly assign patient to certain treatment plan + visualize current treatment distribution
- assessment times: calculate assessment times relative to treatment start; warn user of upcoming assessment by highlighting cell red
- results: summarize key biomarkers (C-reactive protein), treatment response stats by treatment class

Context: 
- this dashboard was created during my time as a statistician at the University of Michigan School of Medicine
- NOTE: all data in this version is synthetic to avoid any issues with data confidentiality

Credits: 
- this project was heavily inspired by Jen Thompson (https://jenthompson.me/2018/02/09/flexdashboards-monitoring/); in particular, the screening & enrollment tab is very similar to hers !

Screenshots: 

_Tab 1: Screening and Enrollment_

![Tab 1: Screening and Enrollment](/images/snr.png)

_Tab 2: Treatment Assignment_

![Tab 2: Treatment Assignment](/images/treatment.png)

_Tab 3: Assessment Times_

![Tab 3: Assessment Times](/images/assessment.png)

_Tab 4: Results_

![Tab 4: Outcomes](/images/results.png)


