# COVID-19: Unemployment and Income Supports
Data and Codes blog post: "COVID, unemployment and income supports", Rebecca Hasdell Alice Milivinti and David Rehkopf.

## Data Sources: Census Current Population Survey (CPS)

Data come from the Current Population Survey, which have been download from: https://cps.ipums.org/cps/index.shtml (registration required).

The codebooks for the extraction are the files:

1. [Core_2020_Extr_Codebook.xml](https://github.com/alice1020/COVID-unemployment-and-income-supports/blob/master/Core_2020_Extr_Codebook.xml) for the core set of questions for monthly samples from 01-2019 to 04-2020.
Variables selected: YEAR, SERIAL, MONTH, HWTFINL, CPSID, ASECFLAG, PERNUM, WTFINL, CPSIDP, EMPSTAT, LABFORCE, OCC, CLASSWKR, UHRSWORKT,  UHRSWORK1, UHRSWORK2, AHRSWORKT, AHRSWORK1, AHRSWORK2, EARNWT, HOURWAGE, PAIDHOUR, EARNWEEK, UHRSWORKORG, WKSWORKORG.
  
2. [ASEC_2019_Extr_Codebook.xml](https://github.com/alice1020/COVID-unemployment-and-income-supports/blob/master/ASEC_2019_Extr_Codebook.xml) for the 2019 Annual Social and Economic Supplement (ASEC) of 03-2019.
Among all the variables we selected: YEAR, SERIAL, MONTH, CPSID, ASECFLAG, ASECWTH, PERNUM, CPSIDP, ASECWT, OCCLY, CLASSWLY, WORKLY, FTOTVAL, INCTOT, INCWAGE, INCLONGJ, OINCBUS, OINCFARM, OINCWAGE, EITCRED.
  
The Current Population Survey has monthly data for a core set of questions, which include employment status, occupation, geographical information, etc. Extensive data about socio-economic variables, such as earnings, Earned Income Tax Credit (EITC), health insurance, etc. are collected once a year (March) by the Social and Economic Supplement (ASEC) module.
CPS is a survey which has basically been used for the analysis of cross-sectional data. It has a sampling scheme defined as 4-8-4. One household stays in the sample for max. 16 months (4+8+4). The household is interviewed during each of the first 4 months after entering the survey, followed by 8 without interview. The household is then interviewed again for 4 consecutive month, one year (4+8) after its first wave in order to match the seasonality.

The following figure visualizes the longitudinal structure of the survey. Our data ample cover the time span from January 2019 to April 2020. Each line represent an individual longitudinal record. Wave March 2019 is my master wave since it is the most recent information about EICT and earnings we have. Gray represents when the individual is out of the CPS's sample.


![Sequence Analysis](https://aliceindataland.rbind.io/img/COVID-19-EITC/Sequence_Analysis.png)
**Fig. 1:** Sequence Analysis of the Employment Status from January 2019 to April 2020.

![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Legend.png)

![Employment Status Frequencies](https://aliceindataland.rbind.io/img/COVID-19-EITC/Sequence_Analysis_Density.png)
**Fig. 2:** Sequence Analysis of the Employment Status Frequencies from January 2019 to April 2020 .


## Statistical Exploration
We are interested in analyzing the effects of COVID-19 on the employment status with a focus on the Earned Income Tax Credit (EITC). 

### Individual Level Longitudinal Analysis
At an individual/household level the sample size for individuals and households observed both in March 2019 (ASEC wave) and April 2020  corresponds to 19,013 individuals and 10,456 households. Among those a sub-sample of 4,700 individuals grouped in 3,984 households are part of the labour force. Visually these are mirrored in the three waves at the top of of Fig. 1. At the end, ~9.3% of the households in the sub-sample have received EITC.

|   Level    | # Obs. 03-19 & 04-20 | In the Labor Force | with EITC > 0 |
|------------|----------------------|--------------------|---------------|
| Individual |        19,013        |        4,700       |       370     |         
| Household  |        10,456        |        3,984       |       363     |


![](https://aliceindataland.rbind.io/img/COVID-19-EITC/EITC_Emp_Sub_W.png)
**Fig. 3:** EITC 2019 and Employment status in April 2020


Codes for this part are available in the file: [Disaggregated_Analysis.R](https://github.com/alice1020/COVID-unemployment-and-income-supports/blob/master/Disaggregated_analysis.R)


### Aggregate Occupational Level Analysis

We analyze the CPS data by aggregating over the occupational categories. Our cross-sectional unit becomes the occupational category. The data structure becomes the following


| Occupation |   Time  | % Unempl | Ave. Earn | Ave. EITC | 
|------------|---------|--------- |-----------|-----------|
|      1     | 2020-02 |          |           |           |
|      1     | 2020-03 |          |           |           |
|      1     | 2020-04 |          |           |           |
|      2     | 2020-02 |          |           |           |
|      2     | 2020-03 |          |           |           |
|      2     | 2020-04 |          |           |           |
|      .     | 2020-02 |          |           |           |
|      .     | 2020-03 |          |           |           |
|      .     | 2020-04 |          |           |           |
|      N     | 2020-02 |          |           |           |
|      N     | 2020-03 |          |           |           |
|      N     | 2020-04 |          |           |           |


### Employment vs Unemployment


![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Empl_Occ.png)
**Fig. 4:** % Difference in employment between February and April 2020 by occupational category.



![](https://aliceindataland.rbind.io/img/COVID-19-EITC/W_Dist_Rout.png)
**Fig. 5:** Wage Distribution by Occupation.



![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Empl_Rout.png)
**Fig 6:** % Difference in employment between February and April 2020 for by wage quartile. 


### Hours Worked


![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Hrs_Occ.png)
**Fig. 7:** Difference in hours usually worked per week at the main job between February and April 2020 by occupation (unemployed with hrs = 0 are included).



![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Hrs_Occ_Emp.png)
**Fig. 8:** Difference in hours usually worked per week at the main job between February and April 2020 by occupation among employed only.

### Weekly Earnings

<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Earn_Occ_Emp.png){}
**Fig. 9:** Difference in weekly earnings at the main job between February and April 2020 by occupation among employed only.
</center>


### EITC by Occupational Category


![](https://aliceindataland.rbind.io/img/COVID-19-EITC/EITC_Occ.png)
**Fig. 10:** Average EITC perceived in 2019 vs % Difference in employment between February and April 2020 by occupational category.


Codes for this part are available in the file:
[Aggregated_Analysis.R](https://github.com/alice1020/COVID-unemployment-and-income-supports/blob/master/Aggregated_Analysis.R)

  
## Reference
Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles and J. Robert Warren. Integrated Public Use Microdata Series, Current Population Survey: Version 7.0 [dataset]. Minneapolis, MN: IPUMS, 2020. https://doi.org/10.18128/D030.V7.0

