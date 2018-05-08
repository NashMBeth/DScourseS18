# Religious Freedom, Immigration, and Economic Flourishing
#### A Guide to Replicate Results
To replicate results, run the file FinalProject_Nash.R available in the same repository as this file.
Software used:
R Studio
Packages needed:
1. lmtest
2. sandwich
3. ggplott2

  - This project is an empirical analysis of prominent theories of the intersection of religious freedom, economic outcomes, and immigration.
  - The first multivariate OLS model tests the relationship between the Government Restrictions Index, taken from Pew Research's 2015 Global Restrictions Report, and majority religion, total immigration levels, Muslim immigration levels, GDP, Population, Unemployment, Exports, and % of Population in Rural Living Areas. Economic variables were taken from the World Bank database.
  - The second OLS model is the same model as the first, but GDP is the dependent variable and the 2015 GRI is an explanatory variable.
  - Breusch-Pagan tests for heteroskedasticity were conducted and resolved by using robust standard errors.
  - Some multicollinearity is necessary in this model due to the dummy variables for Muslim, Hindu, Buddhist, and Jewish majority religion. To prevent perfect collinearity, Christian-majority nations are the largest subgroup for this classification.

# Features of the Data

  - Mean imputation was conducted in Excel for missing observations. Net exports was missing the most observations, with 25 missing values. The rest were all below 15.
  - Data was MNAR--small island countries were missing the most data.
  - Countries were dropped from the dataset that were missing the GRI. After this, 190 countries remained.
 
# Getting the Data
 - The GRI data was entered into Excel manually from Appendix C of Pew's Global Restrictions Report. 
 - Majority religion data was tabulated by creating columns for Muslim, Hindu, Buddhist, and Jewish majority. A zero was entered into the column if its majority religion did not match the religion for the corresponding column.
 - Immigration data was painstakingly sorted through by going to the UN Migration Report, downloading the CSV, then summing all Muslim-majority country inflows for each country by hand and labelling these "Muslim immigration." For total immigration, all inflows from all countries were summed for each host country.
 - Economic data was easily downloaded and merged into an Excel sheet from the World Bank.
 
# The Analysis
- I regressed the GRI on majority religion, total immigration, Muslim immigration, GDP, population, % of population in rural areas, and unemployment
- I then performed the same analysis but with GDP as the dependent variable and the GRI as an independent variable
- Using the lmtest package in R, I tested each for heteroskedasticity
- Heterskedasticity was a problem for the first regression that was fixed by using robust standard errors with the sandwich package in R.
- I then used ggplot2 to make visualizations of the data
# A Word of Caution
- Gathering this data was a process that took a couple weeks. I had to do additional research just to figure out how I could safely classify each country's majority religion.





















