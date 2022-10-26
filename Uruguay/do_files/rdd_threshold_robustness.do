*-------------------------------------------------------------------------------
*0. Path:
*-------------------------------------------------------------------------------

	glo rootdir "~/Dropbox/DIME recruitment"
	glo projectdir "$rootdir/Uruguay"
	cd "$projectdir/output"
	glo rawdatadir "$projectdir/data"
	glo datadir "$projectdir/proc_data"

*-------------------------------------------------------------------------------
*0. RDD robustness to different threshold selections:
*-------------------------------------------------------------------------------
	
foreach P in 1 2 3 {
foreach y in log_count_pos_total log_tot_sales log_count_trans  average_pos_firm total_unique_pos  {
clear all 
use "$datadir\card_transaction_weekly.dta", clear
gen sample2015=(year<=2015 | (year==2016 & month<=6))
keep if sample2015==1

*running variable
replace week_since_reform=week_since_reform-53
gen distt=week_since_reform 

* Post-reform
gen post=(distt>=0)

*bandwith
local bw1 "45 45"
local bw2 "45 30"
local bw3 "45 15"
local bw4 "30 45"

local bw5 "30 30"
local bw6 "30 15"
local bw7 "15 45"
local bw8 "15 30"

local bw9 "15 15"

*Run regression and save results
gen running=(distt-0)
forvalues I=1/9 {
rdrobust `y' running, c(0) p(`P') h(`bw`I'') covs(month)
gen mean_`y'_`P'd`I'=e(tau_cl)
gen se_`y'_`P'd`I'=e(se_tau_cl)
gen bleft_`y'_`P'd`I'=e(h_l)
gen bright_`y'_`P'd`I'=e(h_r)
gen upper_`y'_`P'd`I'=mean_`y'_`P'd`I'+(se_`y'_`P'd`I'*1.960)
gen lower_`y'_`P'd`I'=mean_`y'_`P'd`I'-(se_`y'_`P'd`I'*1.960)
*gen bw_`y'_`P'd`I'=`BW'
}

*Optimal
rdrobust `y' running, c(0) p(`P')  covs(month)
gen mean_`y'_`P'd10=e(tau_cl)
gen se_`y'_`P'd10=e(se_tau_cl)
gen bleft_`y'_`P'd10=e(h_l)
gen bright_`y'_`P'd10=e(h_r)
gen upper_`y'_`P'd10=mean_`y'_`P'd10+(se_`y'_`P'd10*1.960)
gen lower_`y'_`P'd10=mean_`y'_`P'd10-(se_`y'_`P'd10*1.960)



keep if week==1 


keep mean_* upper_* lower_* week distt bleft_* bright_*


reshape long mean_`y'_`P'd upper_`y'_`P'd lower_`y'_`P'd bleft_`y'_`P'd bright_`y'_`P'd, i(week) j(bw)

replace bleft_`y'_`P'd=floor(bleft_`y'_`P'd)
replace bright_`y'_`P'd=floor(bright_`y'_`P'd)

gen value=0
sum upper_`y'_`P'd

*graph parameters
glo rmax=r(max)
glo rmax: di %3.0f ${rmax}
gen gap=0.2*${rmax}
sum gap
glo gap = r(mean)
glo gap: di %3.0f ${gap}

sum bleft_`y'_`P'd if bw==10
glo bl_`y'_`P'=r(mean)
glo bl_`y'_`P': di %3.0f ${bl_`y'_`P'}

sum bright_`y'_`P'd if bw==10
glo br_`y'_`P'=r(mean)
glo br_`y'_`P': di %3.0f ${br_`y'_`P'}

local l_log_count_pos_total="-.15(0.05).15"
local l_log_count_trans="-.3(.1).3"
local l_log_tot_sales="-.3(0.1).3" 


graph twoway (rcap upper_`y'_`P'd lower_`y'_`P'd bw if bw<=9, fcolor(blue) lwidth(0.3) lcolor(blue)) ///
(rcap upper_`y'_`P'd lower_`y'_`P'd bw if bw==10, fcolor(orange) lwidth(0.5) lcolor(orange)) ///
(scatter mean_`y'_`P'd bw if bw<=9, mcolor(blue) msize(medium) lcolor(blue)) ///
(scatter mean_`y'_`P'd bw if bw==10, mcolor(orange) msize(medlarge) msymbol(triangle) lcolor(orange)) ///
(line value  bw, lcolor(red) lwidth(medthin)), ///
ylabel(`l_`y'') xlabel(1 "45, 45" 2 "45,30" 3 "45,15" 4 "30,45" 5 "30,30" 6 "30,15" 7 "15,45" 8 "15,30" ///
9 "15 15" 10 "Optimal ${bl_`y'_`P'},${br_`y'_`P'}", labsize(small) angle(forty_five)) graphregion(fcolor(white)) legend(off) yline(0, lwidth(medthin) lcolor(red)) ytitle(Point Estimate, size(medium)) ///
xtitle(Bandwidth (Weeks to reform), size(medium)) 
graph export "robust_`y'_`P'_d_w_0815.pdf", replace 
}
}





