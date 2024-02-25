/*
   STATA Coding Sample

   Mohammadmehdi Sharifkazemi
   
   Last modified 02/18/2024
*/


// Description: This Do-file was created as part of a problem set for Advanced Development Economics (SIPA INAF U8145), a higher-level course at Columbia University's School of International and Public Affairs. 


/* 
	Key STATA commands used:
	
	- For loops with regressions and local memory
	- Matrices constructed with counter variables for regressions
	- Complex twoway scatter graphs
	- Generate variable, label variable
	- Sort, rank, and list for min/max values
*/


clear
// If needed to set directory: 
// cd ""


// Get data ready
use ps1data_1.dta


// 1.2. Create a new variable for adult literacy, and then plot its relationship with infant mortality across two graphs. The second graph should include a regression line. 

gen ad_lit = 100-ad_illit 


// scatterplot of infant mortality vs. adult literacy
twoway scatter inf_mort ad_lit, msymbol(oh) title("infant mortality vs. adult literacy") ytitle("infant mortality (per 1000 births)") xtitle("adult literacy (%)") scheme(s1mono) xscale(titlegap(2)) yscale(titlegap(2))


// scatterplot of infant mortality vs. adult literacy with regression line

reg inf_mort ad_lit

predict yhat

graph twoway scatter inf_mort yhat ad_lit, msymbol(oh i) connect(i l) sort title("infant mortality vs. adult literacy, w/ regression line") ytitle("infant mortality (per 1000 births)") xtitle("adult literacy (%)") scheme(s1mono) xscale(titlegap(2)) yscale(titlegap(2))




// 1.3. Create two new variables, gdp_usd and gdp_id, equal to GDP/capita in current U.S. dollars using the exchange rate conversion and GDP/capita in current international dollars using the PPP conversion. (Hint: to convert from local currency to US dollars or international dollars, you divide by the exchange rate or PPP exchange rate, respectively.)

gen gdp_usd = (gdp_lcu / pop) / xr

label var gdp_usd  "GDP/capita in USD XR"


gen gdp_id = (gdp_lcu / pop) / ppp

label var gdp_id  "GDP/capita in Int'l PPP"

// 1.4. Make a scatterplot with GDP/capita (exchange rate conversion) on the x-axis and GDP per capita (PPP conversion) on the y-axis. Save a pdf of your graph.

graph twoway scatter gdp_id gdp_usd, title("Countries with GDP/capita conversion types") ytitle("GDP per capita (PPP conversion)") xtitle("GDP/capita (exchange rate conversion)") scheme(s1mono) xscale(titlegap(2)) yscale(titlegap(2))


graph export graph1_41.pdf, replace

// 1.5. Make a scatterplot with log(GDP/capita) (exchange rate conversion) on the x-axis and log(GDP per capita (PPP conversion)) on the y-axis. Save a pdf of it. Which graph is easier to interpret in your opinion, the one in 1.4 or the one in 1.5? Why do you say so?

gen log_gdp_usd = log(gdp_usd)

label var log_gdp_usd  "Log of GDP/capita in USD XR"

gen log_gdp_id = log(gdp_id)

label var log_gdp_id  "Log of GDP/capita in Int'l PPP"

graph twoway scatter log_gdp_id log_gdp_usd, msymbol(oh i) connect(i l) sort title("Countries with GDP/capita conversion types") ytitle("GDP per capita (PPP conversion)") xtitle("GDP/capita (exchange rate conversion)") scheme(s1mono) xscale(titlegap(2)) yscale(titlegap(2))
  
graph export graph1_51.pdf, replace

// ANSWER TO 1.5: The log graph is simpler to interpret and to see the differences between methodologies, with countries typically being undervalued in GDP/capita for USD relative to PPP. This is more clearly seen in a log chart that normalises for the various GDP/capita values that can be observed, enabling easier distinction and little clustering. 

// 1.6. Modify the graph command for 1.5 in two ways:
//(a) Have it draw a line indicating where the points would be if GDP/capita (exchange rate conversion) were exactly equal to GDP/capita (PPP conversion.) This is what we would ordinarily refer to as a 45-degree line, if  the scales on the x and y-axes were symmetric. (Hint: You can just include the x-axis variable as an additional yaxis variable, i.e. "graph twoway scatter lgdp_id lgdp_usd lgdp_usd, msymbol(oh) connect(i l) …" where lgdp_id and lgdp_usd refer to log GDP/capita with PPP conversion and XR conversion respectively.

graph twoway scatter log_gdp_id log_gdp_usd log_gdp_usd, title("Countries with GDP/capita conversion types") ytitle("GDP per capita (PPP conversion)") xtitle("GDP/capita (exchange rate conversion)") 

graph export graph1_61.pdf, replace
  
// (b) Modify the graph command so that it uses the variable cty_cod1 (country code) as the plotting symbol. (Hint: put "msymbol(i i) mlabel(cty_cod1) mlabsize(tiny) mlabposition(0)" in the graph command.

graph twoway scatter log_gdp_id log_gdp_usd log_gdp_usd, msymbol(i i) mlabel(cty_cod1) mlabsize(tiny) mlabposition(0) title("Countries with GDP/capita conversion types") ytitle("GDP per capita (PPP conversion)") xtitle("GDP/capita (exchange rate conversion)")


graph export graph1_62.pdf, replace

// 1.7. List five countries for which GDP/capita (PPP conversion) is particularly large relative to GDP/capita (exchange rate conversion). List five countries for which GDP/capita (PPP conversion) is particularly small relative to GDP/capita (XR conversion).

// ANSWER TO 1.7: 

// PPP large relative to XR conversion for country IDs 14 (Belarus), 39 (Czechia), 125 (Sierra Leone), 139 (Tanzania), 155 (Yemen)

list country if cty_cod1 == 125 | cty_cod1 == 139 | cty_cod1 == 155 | cty_cod1 == 14 | cty_cod1 == 39

// XR conversion large relative to PPP for country IDs 34 (Congo), 77 (Japan), 85 (Latvia), 109 (Norway), 136 (Switzerland)

list country if cty_cod1 == 34 | cty_cod1 == 77 | cty_cod1 == 136 | cty_cod1 == 109 | cty_cod1 == 85

// 1.8. Based on your graph from 1.6, what generalization can you make about the relationship between how rich a  country is and the relative magnitudes of GDP/capita (PPP conversion) and GDP/capita (XR conversion)? What is an economic reason for this pattern?

// ANSWER TO 1.8: The richer a country is, the relative magnitudes of GDP/capita in PPP and XR align more closely and have smaller relative differences. This is likely because richer countries might have higher cost of services within their countries, driven by higher wages from the higher productivity economies, and thus their currency's purchasing power for services does not have a large difference with purchasing power for tradable goods. 

*** Question 2: Comparing GDP/capita with other development indicators

// 2.1. Infant mortality is a good indicator of the extent to which basic needs are being met. Using the same data  as above, make a scatterplot with log(GDP/capita) (PPP conversion) on the x-axis and infant mortality (under 5  yrs.) per 1,000 live births on the y-axis

label var inf_mort "Infant mortality (under 5  yrs.)"

graph twoway scatter inf_mort log_gdp_id, title("Infant mortality and log GDP/capita")

graph export graph2_11.pdf, replace

// 2.2. Add a regression line (of infant mortality on log(GDP/capita), PPP conversion) to the data. Save a pdf.  Compare your graph to Ray figure 2.9

reg inf_mort log_gdp_id //regress infant mortality on log_gdp_id;

predict yhat_inf_lgid //create new variable representing predicted values of inf_mort, given ad_lit

graph twoway scatter inf_mort yhat_inf_lgid log_gdp_id, msymbol(oh i) connect(i l) sort title("Infant mortality and log GDP/capita")

graph export graph2_21.pdf, replace

// 2.3. Make a graph similar to the one for part 2.2 using the adult literacy rate in place of infant mortality on the y-axis. Save a pdf

reg ad_lit log_gdp_id //regress adult literacy on log_gdp_id;

predict yhat_adlit_lgid //create new variable representing predicted values of inf_mort, given ad_lit

graph twoway scatter ad_lit yhat_adlit_lgid log_gdp_id, msymbol(oh i) connect(i l) sort title("Adult literacy and log GDP/capita")

graph export graph2_31.pdf, replace

// 2.4. Write a short paragraph (i.e. 2-3 sentences) using your graphs from part 2.2 and 2.3 as evidence in support of the assertion by Robert Lucas in the quote at the beginning of Chapter 2 of Ray (see also Ray's discussion on  p. 9) that average per capita income is a reasonable way to measure economic development. Next write a short paragraph using your graphs as evidence against Lucas' argument. Finally, write a sentence or two expressing your own view about whether GDP/capita is a reasonable measure of development

// ANSWER TO 2.5: 2.2 demonstrates a negative association between GDP/capita and infant mortlity, whilst adult literacy has a positive association with GDP/capita. These two findings support the notion that increasing GDP/capita improves development as both infant mortality and adult literacy are core components of human capabilities, the improvement of which is tied to an increasing GDP/capita. That being said, we have plotted a linear regression line whilst the rate of returns of GDP/capita for these measures of development seem to be diminishing. Therefore, GDP/capita cannot measure economic development past a certain point. My own opinion is similar to the counter argument: there are significant drivers of development which seem to be associated with GDP/capita but this relationship breaks down with increasing GDP/capita. Moreover, there are many instances of strong public services tackling infant mortality and adult literacy without subsequently raising GDP/capita - therefore, this association is not necessarily indicative of a causal relationship between GDP/capita and key measures of development. 


// Question 3: Comparing the Human Development Index with GDP/capita

clear 

use ps1data_2.dta

// 3.1. Create a new variable, hdi, representing the Human Development Index (HDI) for each of the 167  countries. Refer to the notes from lecture and to the technical notes to the Human Development Report 2016  here: http://hdr.undp.org/sites/default/files/hdr2016_technical_notes.pdf. Use the goalposts reported in the  Technical Notes for the 2016 report. (Note that your HDI will not match exactly the HDI published in the  report, since the data you are using is from 2008, not 2016. Note also that you should first re-define any  variables that are above the maximum goalpost or below the minimum one. The Stata syntax for value above the maximum is "replace varname = max_goalpost if varname>max_goalpost & varname~=."

// Scoping the tasks at hand-hand:

// 1. Construct indices for life expectancy, log GNI per capita, mean years schooling, expected years schooling

// 2. Take arithmetic average of schooling indices, and make new index using this average. 

// 3. Take geometric average of life expectancy, log GNI/capita, and combined education index:. 


// 1. CONSTRUCT INDICES

/*
Dimension                   Indicator                              Minimum   Maximum
Health                 Life expectancy (years)                       20        85
Education            Expected years of schooling (years)              0        18
                     Mean years of schooling (years)                  0        15
Standard of living   Gross national income per capita (2011 PPP $)   100     75,000
*/

local life_exp_min = 20
local life_exp_max = 85
local exp_sch_min = 0
local exp_sch_max = 18
local mean_sch_min = 0
local mean_sch_max = 15
local gni_min = log(100)
local gni_max = log(75000)


// Education - EXPECTED

replace exp_sch_child = `exp_sch_max' if exp_sch_child > `exp_sch_max' & exp_sch_child ~= .
replace exp_sch_child = `exp_sch_min' if exp_sch_child < `exp_sch_min' & exp_sch_child ~= .

// Education - ACTUAL

replace sch_adult = `mean_sch_max' if sch_adult > `mean_sch_max' & sch_adult ~= .
replace sch_adult = `mean_sch_min' if sch_adult < `mean_sch_min' & sch_adult ~= .

// GNI
gen log_gni_pc = log(gnipc)
replace log_gni_pc = `gni_max' if log_gni_pc > `gni_max' & log_gni_pc ~= .
replace log_gni_pc = `gni_min' if log_gni_pc < `gni_min' & log_gni_pc ~= .

// Life Expectancy

replace life_exp = `life_exp_max' if life_exp > `life_exp_max' & life_exp ~= .
replace life_exp = `life_exp_min' if life_exp < `life_exp_min' & life_exp ~= .


// 2. ARITHMETIC AVERAGE OF INDICES

// Education - FINAL
gen educ = (exp_sch_child + sch_adult) / 2

gen h_educ = ((sch_adult - `mean_sch_min') / (`mean_sch_max' - `mean_sch_min') + (exp_sch_child - `exp_sch_min') / (`exp_sch_max' - `exp_sch_min')) / 2

gen h_gnipc = (log_gni_pc - `gni_min') / (`gni_max' - `gni_min')

gen h_life = (life_exp - `life_exp_min') / (`life_exp_max' - `life_exp_min')


// 3. HDI - GEOMETRIC AVERAGE

gen hdi = ((h_life)^(1/3))*((h_gnipc)^(1/3))*((h_educ)^(1/3))


// 3.2. Calculate the rank of each country according to the HDI in descending order of "human development" – i.e. the country with the highest HDI is 1, the second-highest is 2, etc. Calculate the rank of each country  according to GDP/capita (PPP conversion) – again, richest country is 1, second richest is 2, etc. The command  is "egen hdi_rank = rank(-hdi)"; the negative sign ensures that the rank will be in descending order. (Note: you should use GDP/capita, not GNI/capita, to calculate the GDP/capita rank.)

egen hdi_rank = rank(-hdi), unique

egen gdppc_rank = rank(-gdppc), unique 

// 3.3. List five countries with HDI ranks that are particularly poor (i.e. corresponding to low levels of human development) relative to their GDP/capita ranks, and list their values of the HDI. List five countries with HDI ranks that are particularly good (i.e. correponding to high levels of human development) relative to their GDP/capita ranks, and list their values of the HDI. List five countries that have poor ranks for both HDI and GDP/capita, and list their values of the HDI.



gen rank_diff = gdppc_rank - hdi_rank

// Countries with low HDI relative to GDP/capita

gsort -rank_diff

list country hdi in 1/5

/*
     |     country        hdi |
     |------------------------|
  1. | Timor-Leste   .5297477 |
  2. | New Zealand   .8911425 |
  3. |       Tonga   .6847103 |
  4. |     Georgia   .7064959 |
  5. |     Ukraine   .7205315 |
*/

// Countries with high HDI relative to GDP/capita

gsort rank_diff

list country hdi in 1/5

/*
     |           country        hdi |
     |------------------------------|
  1. | Equatorial Guinea    .535369 |
  2. |            Angola    .403403 |
  3. |             Gabon   .6454702 |
  4. |          Botswana   .6283521 |
  5. |            Kuwait   .7767124 |
*/

// Total rank

gen total_rank = hdi_rank + gdppc_rank
gsort -total_rank
list country hdi in 1/5

/*
     |                            country        hdi |
     |-----------------------------------------------|
  1. |                           Zimbabwe   .2382953 |
  2. | Congo (Democratic Republic of the)   .2814336 |
  3. |                            Burundi   .3214741 |
  4. |                              Niger   .2854061 |
  5. |                      Guinea-Bissau   .3275769
*/


// 3.4. What generalizations can you draw about these three groups: countries with poor HDI relative to  GDP/capita, countries with good HDI relative to GDP/capita, and countries with both poor HDI and low  GDP/capita? (Note: no single right answer here. Hint: a useful exercise is to calculate the difference in HDI  rank vs. GDP/capita rank for each country, subtracting one from the other. Which countries tend to have large  positive differences? Which countries tend to have large negative differences?)

// ANSWER TO 3.4: Countries with higher HDI:GDPPC likely exhibit greater development metrics such as life expectancy and education, whilst having lower average income (GDPPC). These nations can be expected to have a higher and/or more effective investment in public services that distributes key development benefits to the population, whilst economic activity may not be productive enough to generate high income/GDPPC in relative terms. For the converse, countries with a lower HDI:GDPPC enjoy a higher 'average' income level (as defined by GDPPC) but these benefits are not translating to greater development indicators for a number of reasons. These could be due to poorer government provisions or an inequitable distribution of the income which is generated. For example, Ukraine and Georgia benefit from raw resource exports but their economies are characterised by high corruption which limits the redistribution of the generated income. 


*** 4. O Convergence, Where Art Thou?

// 4.1. Get the data. In this part of the problem set, you will be using the data from the Penn World Table, version 10.0, which is in ps1data_3.dta on Courseworks in the Files > Problem Sets > ps1 folder. For more information on this dataset, go to https://www.rug.nl/ggdc/productivity/pwt/?lang=en. This file contains real GDP/capita (PPP conversion) in constant 2017 international dollars over the period 1960-2019 for 109 countries for which complete data are available.


// 4.2. Read through the code in ps1starterprogram.do under the "*** Question 4.3" heading to understand what it is doing. This code calculates average growth rates for the 109 countries in the sample for the period 1960-1999.
clear
use ps1data_3.dta
sort country_code year // *put in ascending order by country code

* make variable to hold income in 1960;
gen rgdp_cap_1960_tmp = rgdp_cap if year==1960
bysort country_code: egen rgdp_cap_1960 = max(rgdp_cap_1960_tmp)
drop rgdp_cap_1960_tmp

* convert rgdp_cap to log form, so we can use a linear regression to calculate growth rate
gen lrgdp_cap = log(rgdp_cap)

local i=1 // *define counter variable;

matrix gr19601999=J(109, 1, 0) // *define matrix to hold coefficient estimates;



while `i'<=109 { 
  reg lrgdp_cap year if country_code==`i' & year>=1960 & year<=1999 
  matrix gr19601999[`i',1]=_b[year]
  local i=`i'+1
}


sort country_code year
by country_code: keep if _n==1

* convert gr19601999 matrix into a variable, which will be called gr196019991 (the 1 is not important)
svmat gr19601999, name(gr19601999)

*in this case, variable represents average growth rate from 1960-1999;
rename gr196019991 gr19601999


// 4.3. Make a scatterplot with real GDP/capita (PPP) in 1960 (not in logs) on the x-axis, and the average growth rate over the period 1960-1999 on the y-axis. (Your graph should resemble figure 3.10 in Ray; the two graphs will not be identical, though, since they cover different periods.) Save a pdf

twoway (scatter gr19601999 rgdp_cap_1960) (lfit gr19601999 rgdp_cap_1960), title("Average Growth Rate vs. Real GDP per Capita in 1960") xtitle("Real GDP per Capita in 1960") ytitle("Average Growth Rate (1960-1999)") graphregion(color(white))
graph export graph4_31.pdf, replace


// 4.4. Make a scatterplot similar to the one in Question 4.3, but with real GDP/capita (PPP) in 2000 (not in logs) on the x-axis, and the average growth rate over the period 2000-2019 on the y-axis. Save a pdf.
clear
use ps1data_3.dta

// Generate real GDP per capita in 2000 for each country
gen rgdp_cap_2000 = .
replace rgdp_cap_2000 = rgdp_cap if year == 2000
bysort country_code: egen max_rgdp_cap_2000 = max(rgdp_cap_2000)

// Generate the log of real GDP per capita
gen lrgdp_cap = log(rgdp_cap)

// Initialize a variable to store average growth rates and a local macro for looping
local i = 1
matrix gr20002019=J(109, 1, 0)

// Loop through each country code to calculate the average growth rate from 2000 to 2019
while `i' <= 109 {
    quietly reg lrgdp_cap year if country_code == `i' & year >= 2000 & year <= 2019
	matrix gr20002019[`i',1]=_b[year]
    local i = `i' + 1
}

sort country_code year
by country_code: keep if _n==1
svmat gr20002019, name(gr20002019)
rename gr200020191 gr20002019

// Create and save the scatterplot with real GDP per capita in 2000 on the x-axis and the average growth rate from 2000 to 2019 on the y-axis
twoway (scatter gr20002019 max_rgdp_cap_2000) (lfit gr20002019 max_rgdp_cap_2000), title("Average Growth Rate vs. Real GDP per Capita in 2000") xtitle("Real GDP per Capita in 2000") ytitle("Average Growth Rate (2000-2019)")
graph export graph4_41.pdf, as(pdf) replace

// 4.5. Another way of evaluating whether there was absolute convergence in the world economy is to run a regression of growth rates on initial income levels. Usually log initial income (using the natural logarithm, ln) is used as the right-hand-side variable. The syntax for running a regression of y on x in Stata is: "reg y x". (This includes an  intercept, which you should also do in this problem.) Run regressions of (a) the growth rate from 1960-1999 on log  income in 1960, and (b) the growth rate from 2000-2019 on log income in 2000. Cut and paste the Stata output into your write-up. (You do not need to make a nice-looking table.) Recall from your statistics classes that a regression coefficient is typically considered statistically significant if the t-statistic is above 1.96 and the p-value is below 0.05 (which are equivalent statements in this context).

clear
use ps1data_3.dta

// Generate real GDP per capita in 1960 and 2000 for each country
gen rgdp_cap_1960 = .
replace rgdp_cap_1960 = rgdp_cap if year == 1960
bysort country_code: egen max_rgdp_cap_1960 = max(rgdp_cap_1960)

gen rgdp_cap_2000 = .
replace rgdp_cap_2000 = rgdp_cap if year == 2000
bysort country_code: egen max_rgdp_cap_2000 = max(rgdp_cap_2000)

// Generate the log of real GDP per capita
gen lrgdp_cap = log(rgdp_cap)

// Calculate average growth rates for 1960-1999 and 2000-2019
gen gr19601999 = .
gen gr20002019 = .

local i = 1
while `i' <= 109 {
 quietly reg lrgdp_cap year if country_code == `i' & year >= 1960 & year <= 1999
 replace gr19601999 = _b[year] if country_code == `i'
 
 quietly reg lrgdp_cap year if country_code == `i' & year >= 2000 & year <= 2019
 replace gr20002019 = _b[year] if country_code == `i'
 
 local i = `i' + 1
}

// Create log of initial income levels for 1960 and 2000
gen lrgdp_cap_1960 = log(max_rgdp_cap_1960)
gen lrgdp_cap_2000 = log(max_rgdp_cap_2000)

// Run regressions
reg gr19601999 lrgdp_cap_1960
reg gr20002019 lrgdp_cap_2000

/*

. reg gr19601999 lrgdp_cap_1960

      Source |       SS           df       MS      Number of obs   =     6,540
-------------+----------------------------------   F(1, 6538)      =     14.41
       Model |  .007372276         1  .007372276   Prob > F        =    0.0001
    Residual |  3.34392805     6,538   .00051146   R-squared       =    0.0022
-------------+----------------------------------   Adj R-squared   =    0.0020
       Total |  3.35130033     6,539   .00051251   Root MSE        =    .02262

--------------------------------------------------------------------------------
    gr19601999 | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
---------------+----------------------------------------------------------------
lrgdp_cap_1960 |   .0011356   .0002991     3.80   0.000     .0005493     .001722
         _cons |   .0090703   .0024148     3.76   0.000     .0043365    .0138041
--------------------------------------------------------------------------------

. reg gr20002019 lrgdp_cap_2000

      Source |       SS           df       MS      Number of obs   =     6,540
-------------+----------------------------------   F(1, 6538)      =    636.43
       Model |  .253494246         1  .253494246   Prob > F        =    0.0000
    Residual |  2.60411409     6,538  .000398304   R-squared       =    0.0887
-------------+----------------------------------   Adj R-squared   =    0.0886
       Total |  2.85760834     6,539   .00043701   Root MSE        =    .01996

--------------------------------------------------------------------------------
    gr20002019 | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
---------------+----------------------------------------------------------------
lrgdp_cap_2000 |  -.0047924     .00019   -25.23   0.000    -.0051648     -.00442
         _cons |   .0706322   .0016964    41.64   0.000     .0673066    .0739577
--------------------------------------------------------------------------------

*/

// 4.6. How do you interpret your graphs from Questions 4.3-4.4 and your regressions from Question 4.5? Is there evidence of absolute convergence in the world economy from 1960-1999? Is there evidence of absolute convergence from 2000-2019? Explain in a few sentences. 

// ANSWER TO 4.6: First interpreting the regressions, we have a level-log so we can see that for a 1% increase in the GDPPC in 1960 the growth rate increased by 0.001% on average. For the latter period, however, a 1% increase in GDPPC in 2000 was associated with an decrease of 0.005% in the growth rate on average. This implies that  convergence occured more in the latter period than the earlier period. Both results are statistically significant at the 5% level. These results are supported by charts in 4.3-4.4 which show a positive correlation between starting GDPPC and growth in 1960-99 but a negative correlation in 2000-2019. That being said, the correlation is not very strong and there seems to be very high variance for poorer countries - and we are only including one variable so we are likely suffering from omitted variable bias. Therefore, we cannot take the results of the regression at face-value to be representative of all countries and thus not an indication of absolute convergence (which requiers all developing countries catching up with developed countries in terms of per capita output) in either period. There are many countries on the chart in 4.4 (the more optimistic period for the convergence argument) which started off with a low GDPPC but experienced negative or stagnant growth during those 20 years. Therefore, with many countries still lagging in growth rate despite their starting points, we do not have sufficient evidence to conclude there is absolute convergence in the world economy as per the Solow model definition but there may be conditional convergance. 
