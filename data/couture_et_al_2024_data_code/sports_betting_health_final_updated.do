/* ============================================================================
Name: 			sports_betting_health_final_updated.do
Description: 	Analyzes the impact of legalization on mental health

Inputs: 		BRFSS 2017-2020
Outputs:		Tables and Figures for the paper
				
Last updated:		June 4, 2024
============================================================================ */

* Preamble *
clear all
file close _all
set more off
capture log close
set maxvar 10000
set scheme plotplainblind

** Set Directory **
cap cd "G:\My Drive\Research\Sports Betting"
cap cd "F:\My Drive\Research\Sports Betting"

** Globals **
gl figures "Health Figures"
gl tables  "Health Tables"


*********************************************************************************
*********************************************************************************
	use "brfss17_20", clear

	**Drop Nevada**
	drop if _state==32
	**Drop COVID Period**
	drop if iyear>2020
	drop if iyear==2020&imonth>2

	**Code Variables**
	replace menthlth=0 if menthlth==88
	replace menthlth=. if menthlth>30

	replace physhlth=0 if physhlth==88
	replace physhlth=. if physhlth>30

	replace genh=. if genh>5
	replace genh=6-genh

	gen white=_imprace==1
	replace white=. if _imprace==9
	gen black=_imprace==2
	replace black=. if _imprace==9
	gen hisp=_imprace==5
	replace hisp=. if _imprace==9
	gen asian=_imprace==3
	replace asian=. if _imprace==9
	gen other=(_imprace==4|_imprace==6)
	replace other=. if _imprace==9
	replace income2=. if income2>8

	gen female=sex==2
	replace female=. if sex==.

	gen male=sex==1
	replace male=. if sex==.


	replace employ=. if employ==9
	gen self_emp=employ==2
	gen unemp=employ==3|employ==4
	gen homemaker=employ==5
	gen student=employ==6
	gen retired=employ==7
	gen disabled=employ==8
	replace self_emp=. if employ==.
	replace unemp=. if employ==.
	replace homemaker=. if employ==.
	replace student=. if employ==.
	replace retired=. if employ==.
	replace disabled=. if employ==.

	gen married=marital==1
	replace married=. if marital==.
	replace married=. if marital==9
	gen divorced=marital==2
	gen widowed=marital==3
	gen separated=marital==4
	gen partner=marital==6

	gen somehs=educa==3
	gen hs=educa==4
	gen somecoll=educa==5
	gen coll=educa==6
	replace somehs=. if educa==9|educa==.
	replace hs=. if educa==9|educa==.
	replace somecoll=. if educa==9|educa==.
	replace coll=. if educa==9|educa==.

	*gen monthyear=(iyear-2017)*12+imonth

	gen pos_menth=menth>0&menth<=30
	replace pos_menth=. if menth==.

	gen pos_physh=physh>0&physh<=30
	replace pos_physh=. if physh==.

	replace children=0 if children==88
	replace children=. if children>20
	gen have_child=children>0&children<=20
	replace have_child=. if children==.

	***Coding for Sports Betting States**
	gen bet_legal=0
	replace bet_legal=1 if _state==34&(iyear>=2019|(iyear==2018&imonth>=8))
	replace bet_legal=1 if _state==54&(iyear>=2019|(iyear==2018&imonth==12&iday>=27))

	replace bet_legal=1 if _state==42&(iyear>=2020|(iyear==2019&imonth>=6)|(iyear==2019&imonth==5&iday>=28))
	replace bet_legal=1 if _state==19&(iyear>=2020|(iyear==2019&imonth>=9)|(iyear==2019&imonth==8&iday>=15))
	replace bet_legal=1 if _state==44&(iyear>=2020|(iyear==2019&imonth>=10)|(iyear==2019&imonth==9&iday>=4))
	replace bet_legal=1 if _state==18&(iyear>=2020|(iyear==2019&imonth>=11)|(iyear==2019&imonth==10&iday>=3))
	replace bet_legal=1 if _state==41&(iyear>=2020|(iyear==2019&imonth>=11)|(iyear==2019&imonth==10&iday>=16))
	replace bet_legal=1 if _state==33&(iyear>=2020|(iyear==2019&imonth==12&iday>=30))

	replace bet_legal=1 if _state==8&(iyear>=2021|(iyear==2020&imonth>=6)|(iyear==2020&imonth==5&iday>=1))
	replace bet_legal=1 if _state==11&(iyear>=2021|(iyear==2020&imonth>=6)|(iyear==2020&imonth==5&iday>=28))
	replace bet_legal=1 if _state==17&(iyear>=2021|(iyear==2020&imonth>=7)|(iyear==2020&imonth==6&iday>=18))
	replace bet_legal=1 if _state==47&(iyear>=2021|(iyear==2020&imonth>=12)|(iyear==2020&imonth==11&iday>=1))

	replace bet_legal=1 if _state==51&(iyear>=2022|(iyear==2021&imonth>=2)|(iyear==2021&imonth==1&iday>=21))
	replace bet_legal=1 if _state==26&(iyear>=2022|(iyear==2021&imonth>=2)|(iyear==2021&imonth==1&iday>=22))
	replace bet_legal=1 if _state==56&(iyear>=2022|(iyear==2021&imonth>=10)|(iyear==2021&imonth==9&iday>=1))
	replace bet_legal=1 if _state==4&(iyear>=2022|(iyear==2021&imonth>=10)|(iyear==2021&imonth==9&iday>=9))
	replace bet_legal=1 if _state==9&(iyear>=2022|(iyear==2021&imonth>=11)|(iyear==2021&imonth==10&iday>=19))

	replace bet_legal=1 if _state==36&(iyear>=2023|(iyear==2022&imonth>=2)|(iyear==2022&imonth==1&iday>=8))
	replace bet_legal=1 if _state==22&(iyear>=2023|(iyear==2022&imonth>=2)|(iyear==2022&imonth==1&iday>=28))
	replace bet_legal=1 if _state==5&(iyear>=2023|(iyear==2022&imonth>=4)|(iyear==2022&imonth==3&iday>=5))
	replace bet_legal=1 if _state==20&(iyear>=2023|(iyear==2022&imonth>=10)|(iyear==2022&imonth==9&iday>=8))
	replace bet_legal=1 if _state==24&(iyear>=2023|(iyear==2022&imonth>=12)|(iyear==2022&imonth==11&iday>=23))

	replace bet_legal=1 if _state==39&(iyear>=2024|(iyear==2023&imonth>=2)|(iyear==2022&imonth==1&iday>=1))
	
	
	gen monthyear = ym(iyear, imonth)
	format monthyear %tm

	
	* Keeps only under 55 (18-54) and people that are not currently students
	drop if _AGEG5YR > 7 | student == 1
	
	* Labels
	la var bet_legal "Legal"	
	la var pos_menth "Indicator for More than 0 Days of Bad MH"
	
	* Define Controls (education, marital status, employment, race, income, time and state FEs)
	global controls "somehs hs somecoll coll married divorced widowed separated partner self_emp unemp homemaker retired disabled white asian black hisp income"
	global FEs "i.monthyear i._state i._AGEG5YR"
	
	

** Loop through the two outcomes **
local outcomes "pos_menth menthlth"

	foreach l of local outcomes {
		reghdfe `l' bet_legal $controls, cluster(_state) absorb($FEs)
			estadd local gender "Full" , replace
			eststo `l'_1	
			
		reghdfe `l' bet_legal $controls if male == 1, cluster(_state) absorb($FEs)
			estadd local gender "Male" , replace
			eststo `l'_2
			
		reghdfe `l' bet_legal $controls if female == 1, cluster(_state) absorb($FEs)
			estadd local gender "Female" , replace
			eststo `l'_3
			
						#delimit  ;
					esttab `l'_1 `l'_2 `l'_3 using "${tables}/full_sample_`l'", 
							b(2) se booktabs star(* .1 ** .05 *** .01) nomtitles brackets replace label style(tex) gaps nonotes
							keep(bet_legal) 
							prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{2pt}}l*{8} 
							{>{\centering\arraybackslash}m{2cm}}@{}}  \midrule") 
							stats(gender  N N_clust, 
							labels("Sample" "Observations" "Clusters") 
							fmt(%9.0fc %9.0fc));
						#delimit cr	
						
		
	}
	
	forvalues j = 1(1)7 {
		gen bet_legal_`j' = bet_legal
		replace bet_legal_`j' = 0 if bet_legal_`j' == 1 & _AGEG5YR != `j'
		
	}
	
	* Bin the 40+
	egen bet_legal_alt_5 = rowmax(bet_legal_5 - bet_legal_7)
	
	local outcomes "pos_menth menthlth"
	* Make figures and tables *
	foreach l of local outcomes {
	
	****	
	* This gives us the overall effect. If we add in bet_legal and omit a bin then it will give us the relative effects *
	***
		*Males
		reghdfe `l' bet_legal_1 bet_legal_2 bet_legal_3 bet_legal_4 bet_legal_5 bet_legal_6 bet_legal_7 $controls if male == 1, cluster(_state) absorb($FEs)
			coefplot, keep(bet_legal*) vertical yline(0) xtitle("Age Group") ///
			xlabel(, angle(horizontal)) ciopts(recast(rcap)) omitted ///
			coeflabels(bet_legal_1 = "18-24"                         ///
               bet_legal_2 = "25-29"                         ///
               bet_legal_3 = "30-34" ///
			   bet_legal_4 = "35-39" ///
			   bet_legal_5 = "40-44" ///
			   bet_legal_6 = "45-49" ///
			   bet_legal_7 = "50-54")	
			gr export "${figures}/male_agebins_`l'.png", replace
			
		reghdfe `l' bet_legal_1 bet_legal_2 bet_legal_3 bet_legal_4 bet_legal_alt_5 $controls if male == 1, cluster(_state) absorb($FEs)
			coefplot, keep(bet_legal*) vertical yline(0) xtitle("Age Group") ///
			xlabel(, angle(horizontal)) ciopts(recast(rcap)) omitted ///
			coeflabels(bet_legal_1 = "18-24"                         ///
               bet_legal_2 = "25-29"                         ///
               bet_legal_3 = "30-34" ///
			   bet_legal_4 = "35-39" ///
			   bet_legal_alt_5 = "40-54")	
			gr export "${figures}/male_agebins_grouped_`l'.png", replace
			
		*Females
		reghdfe `l' bet_legal_1 bet_legal_2 bet_legal_3 bet_legal_4 bet_legal_5 bet_legal_6 bet_legal_7 $controls if female == 1, cluster(_state) absorb($FEs)
			coefplot, keep(bet_legal*) vertical yline(0) xtitle("Age Group") ///
			xlabel(, angle(horizontal)) ciopts(recast(rcap)) omitted ///
			coeflabels(bet_legal_1 = "18-24"                         ///
               bet_legal_2 = "25-29"                         ///
               bet_legal_3 = "30-34" ///
			   bet_legal_4 = "35-39" ///
			   bet_legal_5 = "40-44" ///
			   bet_legal_6 = "45-49" ///
			   bet_legal_7 = "50-54")	
			gr export "${figures}/female_agebins_`l'.png", replace
			
		reghdfe `l' bet_legal_1 bet_legal_2 bet_legal_3 bet_legal_4 bet_legal_alt_5 $controls if female == 1, cluster(_state) absorb($FEs)
			coefplot, keep(bet_legal*) vertical yline(0) xtitle("Age Group") ///
			xlabel(, angle(horizontal)) ciopts(recast(rcap)) omitted ///
			coeflabels(bet_legal_1 = "18-24"                         ///
               bet_legal_2 = "25-29"                         ///
               bet_legal_3 = "30-34" ///
			   bet_legal_4 = "35-39" ///
			   bet_legal_alt_5 = "40-54")	
			gr export "${figures}/female_agebins_grouped_`l'.png", replace
	
	}
	
	
* Table with the 30-34 married vs not married
** Loop through the two outcomes **
local outcomes "pos_menth menthlth"

	foreach l of local outcomes {
		reghdfe `l' bet_legal $controls if male == 1 & _AGEG5YR == 3, cluster(_state) absorb($FEs)
			estadd local sample "Male: 30-34" , replace
			estadd local marital "Any"
			eststo `l'_1	
			
		reghdfe `l' bet_legal $controls if male == 1 & _AGEG5YR == 3 & (married == 1 | partner == 1), cluster(_state) absorb($FEs)
			estadd local sample "Male: 30-34" , replace
			estadd local marital "Married or Partner"
			eststo `l'_2
			
		reghdfe `l' bet_legal $controls if male == 1 & _AGEG5YR == 3 & married == 0 & partner == 0, cluster(_state) absorb($FEs)
			estadd local sample "Male: 30-34" , replace
			estadd local marital "Not Married or Partner"
			eststo `l'_3
			
						#delimit  ;
					esttab `l'_1 `l'_2 `l'_3 using "${tables}/male_30_34_marital_`l'", 
							b(2) se booktabs star(* .1 ** .05 *** .01) nomtitles brackets replace label style(tex) gaps nonotes
							keep(bet_legal) 
							prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{2pt}}l*{8} 
							{>{\centering\arraybackslash}m{2cm}}@{}}  \midrule") 
							stats(sample marital N N_clust, 
							labels("Sample" "Marital Status" "Observations" "Clusters") 
							fmt(%9.0fc %9.0fc));
						#delimit cr	
						
		
	}
	
	
* CSDiD Figures for males 18-24 and 30-34
* Grab the first month
	bys _state: egen temp_first_monthyear = min(monthyear) if bet_legal == 1
	bys _state: egen first_monthyear = min(temp_first_monthyear) 
	
	gen legal_month = first_monthyear
	replace legal_month = 0 if legal_month == .

	local outcomes "pos_menth menthlth"
	* Make figures and tables *
	foreach l of local outcomes {				
		csdid `l' if _AGEG5==1& male == 1, time(monthyear) gvar(legal_month) long2 method(reg)
		estat event, window(-12 4)
		csdid_plot
		gr export "${figures}/male_18_24_nocont_`l'.png", replace 
		
		csdid `l' if _AGEG5==3& male == 1, time(monthyear) gvar(legal_month) long2 method(reg)
		estat event, window(-12 4)
		csdid_plot
		gr export "${figures}/male_30_34_nocont_`l'.png", replace 
		
		csdid `l' $controls if _AGEG5==1& male == 1, time(monthyear) gvar(legal_month) long2 method(reg)
		estat event, window(-12 4)
		csdid_plot
		gr export "${figures}/male_18_24_cont_`l'.png", replace 
		
		csdid `l' $controls if _AGEG5==3& male == 1, time(monthyear) gvar(legal_month) long2 method(reg)
		estat event, window(-12 4)
		csdid_plot
		gr export "${figures}/male_30_34_cont_`l'.png", replace 
}
