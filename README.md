# COVID-19: Unemployment and Income Supports
Data and Codes blog post COVID, unemployment and income supports


## Data Sources: Census Current Population Survey (CPS)

Data of the Current Population Survey has been download from: https://cps.ipums.org/cps/index.shtml (registration required).

The codebooks for the extraction are the files:

1. CPS_19-01_20-04_Extr_Codebook.xml for the core set of questions for monthly samples from 01-2019 to 04-2020.

2. ASEC_2019_Extr_Codebook.xml for the 2019 Annual Social and Economic Supplement (ASEC) of 03-2019.

The Current Population Survey has monthly data for a core set of questions, which include employment status, occupation, geographical information, etc. Extensive data about socio-economic varibales, such as earnings, Earned Income Tax Credit (EITC), health insurance, etc. are colleceted once a year (March) by the Social and Economic Supplement (ASEC) module.
CPS is a survey which has basically been used for the analysis of cross-sectional data. It has a sampling scheme defined as 4-8-4. One household stays in the sample for max. 16 months (4+8+4). The household is interviewed during each of the first 4 months after entering the survey, followed by 8 without interview. The household is then interviewd again for 4 consequitive month, one year (4+8) after its first wave in order to match the seasonality.

The following figure visualizes the longitudinal structure of the survey. Our data ample cover the time span from January 2019 to April 2020. Each line represent an individual longitudinal record. Wave March 2019 is my master wave since it is the most recent information about EICT and earnigns we have. Gray represents when the individual is out of the CPS's sample.

<center>
![Sequence Analysis](https://aliceindataland.rbind.io/img/COVID-19-EITC/Sequence_Analysis.png){}
**Fig. 1:** Sequence Analysis of the Employment Status from January 2019 to April 2020.
</center>

<center>
![Employment Status Legend](https://aliceindataland.rbind.io/img/COVID-19-EITC/Legend.png){}
</center>

<center>
![Employment Status Frequencies](https://aliceindataland.rbind.io/img/COVID-19-EITC/Sequence_Analysis_Density.png){}
**Fig. 2:** Sequence Analysis of the Employment Status Frequencies from January 2019 to April 2020 .
</center>


## Statistical Exploration

### Individual Level Logitudinal Analysis

We are interested in analysing the effects of COVID-19 on the employment status with a focus on the Earned Income Tax Credit (EITC). If we want to procede at an individual/household level our sample size for individuals and households obseved both in March 2019 (ASEC wave) and April 2020. It corresponds to 19,013 individuals and 10,456 households, which is visually mirrored in the three waves at the top of of Fig. 1. Among those a sub-sample of 4,700 individuals grouped in 3,984 households are part of the labour force. At the end, ~9.3% of the households in the sub-sample have received EITC.

|   Level    | # Obs. 03-19 & 04-20 | In the Labor Force | with EITC > 0 |
|------------|----------------------|--------------------|---------------|
| Individual |        19,013        |        4,700       |       370     |         
| Household  |        10,456        |        3,984       |       363     |

<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/EITC_Emp_Sub.png){}
**Fig. 3:** EITC 2019 and Employment status in April 2020
</center>

Codes available in the file: Disaggregated_analysis.R

### Aggregate Occupational Level Analysis

We analaze the CPS data by aggregating over the occupational categories. Our cross-sectional unit becomes the occupational category. The data structure becomes the following


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


### Extensive Margin: Employment vs Unemployment
<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Empl_Occ.png){}
**Fig. 4:** % Difference in employment between February and April 2020 by occupational category.
</center>

<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/W_Dist_Rout.png){}
**Fig. 5:** Wage Distribution by Occupation.
</center>

<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Empl_Rout.png){}
**Fig 6:** % Difference in employment between February and April 2020 for by wage quartile. 
</center>

### Intensive Margin: Hours Worked

<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Hrs_Occ.png){}
**Fig. 7:** Difference in hours usually worked per week at all jobs between February and April 2020 by occupation (unemployed with hrs = 0 are included).
</center>

<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/Hrs_Occ_Emp.png){}
**Fig. 8:** Difference in hours usually worked per week at all jobs between February and April 2020 by occupation among employed only.
</center>

### EITC by Occupational Category

<center>
![](https://aliceindataland.rbind.io/img/COVID-19-EITC/EITC_Occ.png){}
**Fig. 9:** Average EITC percevied in 2019 vs % Difference in employment between February and April 2020 by occupational category.
</center>

Codes available in the file: Visualizations.R

  


