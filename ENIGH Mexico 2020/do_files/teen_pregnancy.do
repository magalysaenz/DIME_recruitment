*Magaly Saenz
*Sample Code 
*World Bank DIME Recruitment Drive - Oct 2022

*-------------------------------------------------------------------------------
*0. Path:
*-------------------------------------------------------------------------------

* Personal User
if (inlist("${suser}", "magalysaenz") {
	global rootdir "~/Dropbox/DIME recruitment"
}

glo projectdir "$rootdir/ENIGH Mexico 2020"
glo rawdatadir "$projectdir/data_raw"
glo procdata "$projectdir/proc_data"
glo code "$projectdir/do_files"
cd  "$projectdir/output"

*-------------------------------------------------------------------------------
*1. CSV to DTA
*-------------------------------------------------------------------------------
/*import delimited "$rawdatadir/poblacion.csv", clear 
save "$rawdatadir/poblacion.dta", replace 
*/

*-------------------------------------------------------------------------------
*2. Teen Pregnancy on Schooling and Health Outcomes 
*-------------------------------------------------------------------------------
 *load data
use "$rawdatadir/poblacion.dta", clear 
rename ïfolioviv id_hh 
format id_hh %15.0f

*identifying sibiling in the same household
replace madre_id="" if madre_id=="&"
destring madre_id, replace
duplicates report id_hh madre_id parentesco
duplicates tag id_hh madre_id parentesco, gen(mom_tag)
egen mean_mom=mean(madre_id), by(id_hh parentesco mom_tag)

*identifying the age of mother for each kid
gen age_mom_t=.
foreach n in 1 2 3 4 {
egen age_mom`n'_aux=max(edad) if numren==`n', by(id_hh)
egen age_mom`n'=mean(age_mom`n'_aux), by(id_hh)
replace age_mom_t=age_mom`n' if madre_id==`n'
drop age_mom`n'_aux age_mom`n'
}

*keeping those who we observe their mom
keep if age_mom_t!=. 

*Sibiling group ID 
egen sib_group=group(id_hh age_mom_t)

* Age of mom when gave birth to each kid
gen age_mom_birth=age_mom_t-edad

/*dropping group of sibilings if the mother was more than 55 at the birth of one of the sibiling. 
this drops .5% of the sibiling groups. */
egen max_age_mom_birth=max(age_mom_birth), by(sib_group)
drop if max_age_mom_birth>=56 

* Identifying the age of the youngest sibiling 
egen min_age_sib=min(edad), by(sib_group)

* Teen birth indicator
gen teen_mom=age_mom_birth<=19

*-------------------------------------------------------------------------------
*2.1 Schooling 
*-------------------------------------------------------------------------------
*years of schooling
destring nivelaprob gradoaprob, replace
recode nivelaprob (0/2=0) (3=6) (5/6=9) (7=12) (8/9=17), gen(educ_level)
gen schooling=educ_level+gradoaprob

*controls for regression
gen age_sq=edad^2
gen female=sexo==2

* Regression specifications
* Limiting the sample to sibiling groups where the youngest is at least 10 years old, imposing a higher than normal bound on school age
local spec1 teen_mom if min_age_sib>=10, noabsorb vce(robust)
local spec2 edad age_sq female teen_mom if min_age_sib>=10, noabsorb vce(robust)
local spec3 edad age_sq female teen_mom if min_age_sib>=10, absorb(i.sib_group) vce(robust)

*Estimations and storing results
foreach s in 1 2 3 {
reghdfe schooling `spec`s''
foreach x in b se {
local `x'`s'_teen=string(_`x'[teen_mom], "%15.3fc")
}
}

*Table export 
texdoc init "teen_pregnancy_schooling", force replace
tex \begin{tabular}{lccc}\hline
tex & \multicolumn{3}{c}{Years of Schooling} \\ \hline
tex & (1)  & (2) & (3) \\  \hline
tex Teen Pregnancy & `b1_teen' & `b2_teen' & `b3_teen'  \\
tex  & (`se1_teen') & (`se2_teen') & (`se3_teen')  \\ \hline
tex Age & No & Yes & Yes  \\
tex Age Sq & No & Yes & Yes  \\
tex Female & No & Yes & Yes  \\
tex Sibiling Group FE & No & No & Yes \\ \hline
tex \end{tabular}
texdoc close  

*graph plotting different in schooling by mother teen pregnancy status conditional on age
preserve 
collapse (mean) schooling, by(edad teen_mom)
*reshape format
reshape wide schooling, i(edad) j(teen_mom)

*graph
twoway (connected schooling0 edad if edad>=18 & edad<=45 , mcolor(navy) msymbol(smsquare_hollow) lcolor(navy) lpattern(dash)) ///
 (connected schooling1 edad if edad>=18 & edad<=45, mcolor(green) msymbol(smcircle_hollow) lcolor(green) lpattern(dash_dot)), ///
 ytitle("Years of Schooling") xtitle(Age in 2020) xlabel(20(5)45)  scale(1.2) legend(order(1 "Mother was >19 when gave birth" 2 "Mother was <=19 when gave birth") position(6))
graph export "schooling_dif.pdf", replace 
restore

*-------------------------------------------------------------------------------
*2. Birth Disabilities 
*-------------------------------------------------------------------------------

*cleaning
foreach v in disc_camin disc_ver disc_brazo disc_apren disc_oir disc_vest disc_habla disc_acti {
replace `v'="" if `v'=="&"
destring `v', replace
}

*dummies for birth disabilities
gen dis_walk=disc_camin==3
gen dis_see=disc_ver==3
gen dis_hear=disc_oir==3
gen dis_speech=disc_habla==3
gen dis_act=disc_acti==3
egen dis_any=rowmax(dis_walk dis_see dis_hear dis_speech dis_act)

* Estimation and storing coefficients as variables
local a=1
foreach v in walk see hear speech act any {
reghdfe dis_`v' female teen_mom, absorb(i.sib_group) vce(robust)
foreach x in b se {
gen `x'`a'=string(_`x'[teen_mom], "%15.3fc")
}
local a=`a'+1
}

*keeping just the coeff
gen n=_n
keep if n==1
keep b1 b2 b3 b4 b5 b6 se1 se2 se3 se4 se5 se6 n
*reshape format
reshape long b se, i(n) j(var)
destring b se, replace
*constructing confidence interval
gen lower=b-(1.96*se)
gen upper=b+(1.96*se)

*reference line
gen value=0 

*graph
graph twoway (rcap upper lower var, fcolor(blue) lwidth(0.3) lcolor(blue)) ///
(scatter b var , mcolor(blue) msize(small) lcolor(blue)) ///
(line value  var, lcolor(red) lwidth(medthin)), ///
legend(off) yline(0, lwidth(medthin) lcolor(red)) xlabel(1 "Walking" 2 "Vision" 3 "Hearing" 4 "Speech" 5 "Activities" 6 "Any", angle(90)) ///
ytitle(Point Estimate) xtitle(Disabilities at Birth)  ///
scale(1.2)
graph export "disabilities_coef.pdf", replace 
