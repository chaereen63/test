
*Given in order presented in manuscript*

*****************************************************
*Attrition analyses*
*****************************************************

use scales_datafile_rev.dta, clear


*determining how many couples have no post-hurricane data

egen c_nmis_post=rmiss(hrelsat5 hrelsat6 hrelsat7 wrelsat5 wrelsat6 wrelsat7)

tab c_nmis_post

c_nmis_post |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |        114       49.35       49.35
          1 |         13        5.63       54.98
          2 |          9        3.90       58.87
          3 |         14        6.06       64.94
          4 |         12        5.19       70.13
          5 |          5        2.16       72.29
          6 |         64       27.71      100.00
------------+-----------------------------------
      Total |        231      100.00

*c_nmis_post = 6 are couples who have no post-hurricane data (64 couples, 28% of sample)
*reported in text

gen missing = .
replace missing = 1 if (c_nmis_post == 6)
replace missing = 0 if (c_nmis_post == 5) 
replace missing = 0 if (c_nmis_post == 4)
replace missing = 0 if (c_nmis_post == 3)
replace missing = 0 if (c_nmis_post == 2)
replace missing = 0 if (c_nmis_post == 1)
replace missing = 0 if (c_nmis_post == 0) 

label define missing 0 "has post-hurricane data" 1 "no post-hurricane data" 

label values missing missing  

label var c_nmis_post "Number of post-hurricane data points missing (out of 6)"

tab missing

*Do couples with no post-hurricane data differ on initial rel sat?
*reported in text

ttest hrelsat1, by (missing)
*dont differ for husband rel sat, t(229) = .7895, p = .4306

ttest wrelsat1, by (missing)
*do sig differ for wife rel sat, t(229) = 2.4176, p = .0164

ttest hps3, by (missing)
*ns t(159) = -.1845, p = .8539

ttest wps3, by (missing)
*ns t(160) = .3863, p = .6998

ttest hsupp3, by (missing)
*ns t(159) = .8513, p = .3959

ttest wsupp3, by (missing)
*ns t(162) = .9964, p = .3205

save, replace

******************************************************
/*Table S1. Descriptive statistics of items comprising 
the hurricane exposure index*/
******************************************************

use scales_datafile_rev.dta, clear

/*requires estout package available to install using 
 ssc install estout */

eststo: estpost summ H5_BEF1 H5_Bef2r H5_DUR1 H5_DUR4 H5_DUR5 H5_DUR6 ///
H5_DUR8 H5_DUR11 H5_DUR12r H5_DUR14 H5_DUR15_a H5_DUR15_b H5_DUR15_c ///
H5_DUR15_d H5_DUR15_e H5_DUR15_f H5_DUR16r H5_DUR17r 

eststo: estpost summ W5_BEF1 W5_Bef2r W5_DUR1 W5_DUR4 W5_DUR5 W5_DUR6 ///
W5_DUR8 W5_DUR11 W5_DUR12r W5_DUR14 W5_DUR15_a W5_DUR15_b W5_DUR15_c ///
W5_DUR15_d W5_DUR15_e W5_DUR15_f W5_DUR16r W5_DUR17r


esttab using exposureindex_table.rtf, cells (mean(fmt(2))) ///
	title (Table S1. Descriptive statistics of items comprising the hurricane exposure index) ///
	mtitle("Husbands" "Wives") label nonum noobs replace 

eststo clear

/*Values are the mean of dichotomous 0/1 var. For the version presented in the 
manuscript they were multiplied by 100 to convert to the percentage of people who endorsed.*/



**************************************************
*Table 1. Descriptive statistics of all variables*
**************************************************

use scales_datafile_rev.dta, clear

/*requires estout package available to install using 
 ssc install estout */

eststo: estpost summ hrelsat1 hrelsat2 hrelsat3 hrelsat5 hrelsat6 hrelsat7 hexposure hps3 hsupp3 ///
wrelsat1 wrelsat2 wrelsat3 wrelsat5 wrelsat6 wrelsat7 wexposure wps3 wsupp3  

esttab using descriptives_table.rtf, cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2))") wide parentheses ///
	title (Table 1. Descriptive statistics of all variables) ///
	label nonum noobs replace 
	
eststo clear

*alphas added manually to table

	

***************************
***Main effects analysis***
***************************

use scales_datafile_v3long_rev.dta, clear

*creating the pre-hurricane slope (preH), post-hurricane slope (postH), and jump from pre to post (jump) vars to use in piecewise regression
mkspline wpreH 0 wpostH = time
generate wjump = 1
replace  wjump = 0 if time < 0

mkspline hpreH 0 hpostH = time
generate hjump = 1
replace  hjump = 0 if time < 0

gen HusbXhpreH = Husb*hpreH
gen HusbXhpostH = Husb*hpostH
gen HusbXhjump = Husb*hjump

gen WifeXwpreH = Wife*wpreH
gen WifeXwpostH = Wife*wpostH
gen WifeXwjump = Wife*wjump


*executing the dual-interecept piecewise regression
*parameters reported in text and on Figure 1

mixed relsat 1.Husb c.HusbXhpreH c.HusbXhjump c.HusbXhpostH ///
			 1.Wife c.WifeXwpreH c.WifeXwjump c.WifeXwpostH , nocons || ///
CSID: Husb Wife HusbXhpreH HusbXhjump HusbXhpostH WifeXwpreH WifeXwjump WifeXwpostH , nocons var ml
eststo m1


*testing whether pre- and post-hurricane slopes significantly differ
*given in text

test HusbXhpostH == HusbXhpreH
*ns, chi2=3.08, p = .0795

test WifeXwpostH == WifeXwpreH
*ns, chi2=1.22, p = .2688

*testing whether husband and wife parameters are sig different
*given in text

test HusbXhpreH == WifeXwpreH
*sig, chi2=5.93, p=.0149

test HusbXhjump == WifeXwjump
*ns, chi2=.34, p = .5612

test HusbXhpostH == WifeXwpostH
*ns, chi2=.93, p = .3339



**Graphing

*getting the marginal values at the beginning and end of each spline 
*(time = -2.5 and 0 with jump = 0 for pre-hurricane)
*(time = 0 and 1.75 with jump = 1 for post-hurricane)

margins, at(HusbXhpreH=-2.5 HusbXhpostH=0 HusbXhjump=0 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=1 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0) ///
at(HusbXhpreH=0 HusbXhpostH=1.75 HusbXhjump=1 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0) ///
at(WifeXwpreH=-2.5 WifeXwpostH=0 WifeXwjump=0 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 ) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 ) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=1 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 ) ///
at(WifeXwpreH=0 WifeXwpostH=1.75 WifeXwjump=1 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 ) vsquish

*store the adjusted means in a matrix named yhat
matrix yhat_hw = r(b)'

*store the LL and UL of 95% CI in matrices named ll and ul
mat r=r(table)
matrix ll_hw=r["ll",....]' 

mat r=r(table)
matrix ul_hw=r["ul",....]' 


*store levels of time in a matrix named time
matrix time_hw = (-2.5\0\0\1.75\-2.5\0\0\1.75)

*store spouse indicator (1=h 2=w) in a matrix named spouse
matrix spouse_hw = (1\1\1\1\2\2\2\2)

*save matrices to current dataset
svmat yhat_hw
svmat time_hw
svmat spouse_hw
svmat ll_hw  
svmat ul_hw

*checking that values to be graphed look correct
list yhat_hw1 time_hw1 spouse_hw1 ll_hw1 ul_hw1 in 1/8, sep(2)

*export this data to be used to create graph in R
outsheet time_hw1 spouse_hw1 yhat_hw1 ll_hw1 ul_hw1 using "C:\Users\Hannah\Box\Projects in the works\Hurricane Harvey\Analyses\Graphs\main_effects.csv" , comma replace


*generating graph
*used just for quick visualizing, not used in ms - graph in ms was created in R
graph twoway ///
(line yhat_hw1 time_hw1 if spouse_hw1==1, lcolor(red)) ///
(line ll_hw1 time_hw1 if spouse_hw1==1, lpattern (shortdash) lcolor(blue)) ///
(line ul_hw1 time_hw1 if spouse_hw1==1, lpattern (shortdash) lcolor(blue)) ///
(line yhat_hw1 time_hw1 if spouse_hw1==2, lcolor(blue)) ///
(line ll_hw1 time_hw1 if spouse_hw1==2, lpattern (shortdash) lcolor(red)) ///
(line ul_hw1 time_hw1 if spouse_hw1==2, lpattern (shortdash) lcolor(red)), ///
legend(label(1 "Wives") label(4 "Husbands")) ///
title(Dual-Intercept Piecewise Regression) xtitle(Time (years)) ytitle(Relationship Satisfaction) ///
xlabel(-2.5(1)1.75) ylabel(0(10)51) xline(0) ///
legend(size(small) rowgap(0) order (1 4) cols(2) pos(6)) 


*generating table with results of piecewise regression for husbands and wives to save
esttab m1 using Table_piecewise_coefs.rtf, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2)) se(fmt(2))") wide parentheses ///
title (Results of Main Effects Analysis) ///
varlabels(_cons Constant) nocons noobs nonum nobase replace

*outputting results to excel to allow easy calculation of effect size r to report in ms
esttab m1 using Table_piecewise_coefs.csv, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2)) z(fmt(2))") wide plain ///
title (Results of Main Effects Analysis) ///
varlabels(_cons Constant) nocons noobs nonum nobase replace


***************************
*****Testing Predictors****
***************************

**********************************************
*Testing hurricane exposure as a L2 predictor*
**********************************************

use scales_datafile_v3long_rev.dta, clear

gen Hhurr_expo = Husb*hurr_expo
gen Whurr_expo = Wife*hurr_expo

*adding hurricane exposure var as a predictor at L2
mixed relsat 1.Husb c.Hhurr_expo##(c.HusbXhpreH 1.HusbXhjump c.HusbXhpostH) ///
1.Wife c.Whurr_expo##(c.WifeXwpreH 1.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: Husb Wife Hhurr_expo Whurr_expo ///
HusbXhpreH HusbXhjump HusbXhpostH WifeXwpreH WifeXwjump WifeXwpostH , nocons var ml
eststo m2
*all interaction terms are nonsig


*generating Table with results of Piecewise regression with hurricane exposure predictor
esttab m2 using hurrexpo_predictor_rev.rtf, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2))") wide parentheses ///
title (Results of Piecewise Multilevel Model with Hurricane Exposure as a Predictor) ///
nocons noobs nonum nobase replace 


****************************************************
*Testing chronic stress as a L2 predictor*
****************************************************

use scales_datafile_v3long_rev.dta , clear


gen Wifeps = Wife*ps
gen Husbps = Husb*ps



mixed relsat 1.Husb c.Husbps##(c.HusbXhpreH 1.HusbXhjump c.HusbXhpostH) ///
1.Wife c.Wifeps##(c.WifeXwpreH 1.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: Husb Wife Husbps Wifeps ///
HusbXhpreH HusbXhjump HusbXhpostH WifeXwpreH WifeXwjump WifeXwpostH , nocons var ml


*dropping non-sig random effects
mixed relsat 1.Husb c.Husbps##(c.HusbXhpreH 1.HusbXhjump c.HusbXhpostH) ///
1.Wife c.Wifeps##(c.WifeXwpreH 1.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: Husb Wife Husbps Wifeps WifeXwpreH , nocons var ml
eststo m3
*all interaction terms are nonsig


*generating Table with results of Piecewise regression with hurricane exposure predictor
esttab m3 using stress_predictor_rev.rtf, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2))") wide parentheses ///
title (Results of Piecewise Multilevel Model with Chronic Stress as a Predictor) ///
nocons noobs nonum nobase replace 


****************************************************
*Testing social support as a time-varying predictor*
****************************************************

use scales_datafile_v3long_rev.dta , clear


gen Wifesupp = Wife*ps
gen Husbsupp = Husb*ps



mixed relsat 1.Husb c.Husbsupp##(c.HusbXhpreH 1.HusbXhjump c.HusbXhpostH) ///
1.Wife c.Wifesupp##(c.WifeXwpreH 1.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: Husb Wife Husbsupp Wifesupp ///
HusbXhpreH HusbXhjump HusbXhpostH WifeXwpreH WifeXwjump WifeXwpostH , nocons var ml


*dropping non-sig random effects
mixed relsat 1.Husb c.Husbsupp##(c.HusbXhpreH 1.HusbXhjump c.HusbXhpostH) ///
1.Wife c.Wifesupp##(c.WifeXwpreH 1.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: Husb Wife Husbsupp Wifesupp ///
WifeXwpreH , nocons var ml
eststo m4
*all interaction terms are nonsig


*generating Table with results of Piecewise regression with hurricane exposure predictor
esttab m4 using support_predictor_rev.rtf, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2))") wide parentheses ///
title (Results of Piecewise Multilevel Model with Social Support as a Predictor) ///
nocons noobs nonum nobase replace 



***************************************************************
*Testing baseline relationship satisfaction as a L2 predictor*
***************************************************************

use scales_datafile_v3long_rev.dta, clear

gen WifeXwint = Wife*relsat_int
gen HusbXhint = Husb*relsat_int


mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhpreH HusbXhjump HusbXhpostH WifeXwpreH WifeXwjump WifeXwpostH , nocons var ml
*HusbXpreH and WifeXwpreH random effects are not sig, so dropping them

mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhjump HusbXhpostH WifeXwjump WifeXwpostH , nocons var ml
eststo m5

*generating Table with results of Piecewise regression with hurricane exposure predictor
esttab m5 using t1relsat_predictor_rev.rtf, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2))") wide parentheses ///
title (Results of Piecewise Multilevel Model with Baseline Relationship Satisfaction as a Predictor) ///
nocons noobs nonum nobase replace 

*outputting results to excel to allow easy calculation of effect size r to report in ms
esttab m5 using t1relsat_predictor_rev.csv, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2)) z(fmt(2))") wide plain ///
title (Results of Main Effects Analysis) ///
varlabels(_cons Constant) nocons noobs nonum nobase replace

*******GRAPHING*******

*Husband Graph*

*Determining -1SD, mean, and +1SD of intercept values to use for graphing
*H rel sat T1 mean = 43.12, SD = 7.926 (generated from scales_datafile.sav)
*-1SD = 35.194, mean = 43.12, +1SD = 51.046

*getting the marginal values at the beginning and end of each spline for each of the 3 values of the moderator
*(time = -2.5 and 0 with jump = 0 for pre-hurricane)
*(time = 0 and 1.75 with jump = 1 for post-hurricane)
*-1SD of HusbXhint = 35, mean of HusbXhint = 43, +1SD of HusbXhint = 51
margins, at(HusbXhpreH=-2.5 HusbXhpostH=0 HusbXhjump=0 HusbXhint=35 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=-2.5 HusbXhpostH=0 HusbXhjump=0 HusbXhint=43 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=-2.5 HusbXhpostH=0 HusbXhjump=0 HusbXhint=51 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=35 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=43 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=51 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=1 HusbXhint=35 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=1 HusbXhint=43 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=1 HusbXhint=51 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=1.75 HusbXhjump=1 HusbXhint=35 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=1.75 HusbXhjump=1 HusbXhint=43 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) ///
at(HusbXhpreH=0 HusbXhpostH=1.75 HusbXhjump=1 HusbXhint=51 WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=0) 


 
*store the marginal values in a matrix named yhat
matrix yhat_hh = r(b)'

*store the LL and UL of 95% CI in matrices named ll and ul
mat r=r(table)
matrix ll_hh=r["ll",....]' 

mat r=r(table)
matrix ul_hh=r["ul",....]' 

*store levels of time in a matrix named time
matrix time_hh = (-2.5\-2.5\-2.5\0\0\0\0\0\0\1.75\1.75\1.75)

*store levels of group in a matrix named group
matrix group_hh = (1\2\3\1\2\3\1\2\3\1\2\3)

*save matrices to current dataset
svmat yhat_hh
svmat time_hh
svmat group_hh
svmat ll_hh     
svmat ul_hh

*checking that values to be graphed look correct
list time_hh1 group_hh1 yhat_hh1 ll_hh1 ul_hh1 in 1/12, sep(3)


*export this data to be used to create graph in R
outsheet time_hh1 group_hh1 yhat_hh1 ll_hh1 ul_hh1 using "C:\Users\Hannah\Box\Projects in the works\Hurricane Harvey\Analyses\Graphs\husb_t1relsat_predictor.csv" , comma replace


*generating graph (not used in ms)
graph twoway ///
(line yhat_hh1 time_hh1 if group_hh1==1, lcolor(red)) ///
(line ll_hh1 time_hh1 if group_hh1==1, lpattern (shortdash) lcolor(red)) ///
(line ul_hh1 time_hh1 if group_hh1==1, lpattern (shortdash) lcolor(red)) ///
(line yhat_hh1 time_hh1 if group_hh1==2, lcolor(blue)) ///
(line ll_hh1 time_hh1 if group_hh1==2, lpattern (shortdash) lcolor(blue)) ///
(line ul_hh1 time_hh1 if group_hh1==2, lpattern (shortdash) lcolor(blue)) ///
(line yhat_hh1 time_hh1 if group_hh1==3, lcolor(green)) ///
(line ll_hh1 time_hh1 if group_hh1==3, lpattern (shortdash) lcolor(green)) ///
(line ul_hh1 time_hh1 if group_hh1==3, lpattern (shortdash) lcolor(green)), ///
xlabel(-2.5(1)1.75) ylabel(0(10)50) xline(0)  ///
legend(label(1 "-1SD Initial Relationship Satisfaction") ///
label(4 "Mean Initial Relationship Satisfaction") ///
label(7 "+1SD Initial Relationship Satisfaction")) ///
title(Husbands) xtitle(Time (years)) ytitle(Relationship Satisfaction) ///
legend(size(small) rowgap(0) order (7 4 1) cols(1) pos(6) holes(2 3 5 6 8 9)) 


saving(H_dual_intmod, replace)

graph export H_dual_intmod.png, replace


	
*Wife Graph*

*Determining -1SD, mean, and +1SD of intercept values to use for graphing
*W rel sat T1 mean = 42.32, SD = 8.837 (generated from scales_datafile.sav)
*-1SD = 33.483, mean = 42.32, +1SD = 51.157

*getting the marginal values at the beginning and end of each spline for each of the 3 values of the moderator
*(time = -2.5 and 0 with jump = 0 for pre-hurricane)
*(time = 0 and 1.75 with jump = 1 for post-hurricane)
*-1SD of wrelsat_int = 33, mean of wrelsat_int = 42, +1SD of wrelsat_int = 51
margins, at(WifeXwpreH=-2.5 WifeXwpostH=0 WifeXwjump=0 WifeXwint=33 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=-2.5 WifeXwpostH=0 WifeXwjump=0 WifeXwint=42 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=-2.5 WifeXwpostH=0 WifeXwjump=0 WifeXwint=51 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=33 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=42 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=0 WifeXwint=51 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=1 WifeXwint=33 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=1 WifeXwint=42 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=0 WifeXwjump=1 WifeXwint=51 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=1.75 WifeXwjump=1 WifeXwint=33 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=1.75 WifeXwjump=1 WifeXwint=42 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) ///
at(WifeXwpreH=0 WifeXwpostH=1.75 WifeXwjump=1 WifeXwint=51 HusbXhpreH=0 HusbXhpostH=0 HusbXhjump=0 HusbXhint=0) 


  
*store the marginal values in a matrix named yhat
matrix yhat_ww = r(b)'

*store the LL and UL of 95% CI in matrices named ll and ul
mat r=r(table)
matrix ll_ww=r["ll",....]' 

mat r=r(table)
matrix ul_ww=r["ul",....]' 

*store levels of time in a matrix named time
matrix time_ww = (-2.5\-2.5\-2.5\0\0\0\0\0\0\1.75\1.75\1.75)

*store levels of group in a matrix named group
matrix group_ww = (1\2\3\1\2\3\1\2\3\1\2\3)

*save matrices to current dataset
svmat yhat_ww
svmat time_ww
svmat group_ww
svmat ll_ww     
svmat ul_ww

*checking that values to be graphed look correct
list time_ww1 group_ww1 yhat_ww1 ll_ww1 ul_ww1 in 1/12, sep(3)


*export this data to be used to create graph in R
outsheet time_ww1 group_ww1 yhat_ww1 ll_ww1 ul_ww1 using "C:\Users\Hannah\Box\Projects in the works\Hurricane Harvey\Analyses\Graphs\wife_t1relsat_predictor.csv" , comma replace


*generating graph (not used in ms)
graph twoway ///
(line yhat_ww1 time_ww1 if group_ww1==1, lcolor(red)) ///
(line ll_ww1 time_ww1 if group_ww1==1, lpattern (shortdash) lcolor(red)) ///
(line ul_ww1 time_ww1 if group_ww1==1, lpattern (shortdash) lcolor(red)) ///
(line yhat_ww1 time_ww1 if group_ww1==2, lcolor(blue)) ///
(line ll_ww1 time_ww1 if group_ww1==2, lpattern (shortdash) lcolor(blue)) ///
(line ul_ww1 time_ww1 if group_ww1==2, lpattern (shortdash) lcolor(blue)) ///
(line yhat_ww1 time_ww1 if group_ww1==3, lcolor(green)) ///
(line ll_ww1 time_ww1 if group_ww1==3, lpattern (shortdash) lcolor(green)) ///
(line ul_ww1 time_ww1 if group_ww1==3, lpattern (shortdash) lcolor(green)), ///
xlabel(-2.5(1)1.75) ylabel(0(10)52) xline(0)  ///
legend(label(1 "-1SD Initial Relationship Satisfaction") ///
label(4 "Mean Initial Relationship Satisfaction") ///
label(7 "+1SD Initial Relationship Satisfaction")) ///
title(Wives) xtitle(Time (years)) ytitle(Relationship Satisfaction) ///
legend(size(small) rowgap(0) order (7 4 1) cols(1) pos(6) holes(2 3 5 6 8 9)) 

saving(W_dual_intmod, replace)

graph export W_dual_intmod.png, replace



***Generating simple slopes of rel sat intercept moderator
*have to run the regression before each margins command in order to save them with post command and have esttab output them in table


mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhjump HusbXhpostH WifeXwjump WifeXwpostH , nocons var ml

margins, dydx(HusbXhpreH) at(HusbXhint=(35 43 51)) post
eststo m6

mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhjump HusbXhpostH WifeXwjump WifeXwpostH , nocons var ml

margins, dydx(HusbXhjump) at(HusbXhint=(35 43 51)) post
eststo m7

mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhjump HusbXhpostH WifeXwjump WifeXwpostH , nocons var ml

margins, dydx(HusbXhpostH) at(HusbXhint=(35 43 51)) post
eststo m8

mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhjump HusbXhpostH WifeXwjump WifeXwpostH , nocons var ml

margins, dydx(WifeXwpreH) at(WifeXwint=(33 42 51)) post
eststo m9

mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhjump HusbXhpostH WifeXwjump WifeXwpostH , nocons var ml

margins, dydx(WifeXwjump) at(WifeXwint=(33 42 51)) post
eststo m10

mixed relsat c.HusbXhint##(c.HusbXhpreH c.HusbXhjump c.HusbXhpostH) ///
c.WifeXwint##(c.WifeXwpreH c.WifeXwjump c.WifeXwpostH) , nocons || ///
CSID: HusbXhjump HusbXhpostH WifeXwjump WifeXwpostH , nocons var ml

margins, dydx(WifeXwpostH) at(WifeXwint=(33 42 51)) post
eststo m11


*Generating values from simple slopes analyses of T1 rel sat as L2 predictor
*outputting to excel with plain text to allow for calculating effect size r to add to the table
esttab m6 m7 m8 m9 m10 m11 using t1relsat_simpleslopes_rev.csv, cells("b(fmt(2)) p(fmt(3)) ci(fmt(2)) z(fmt(2))") wide plain ///
title (Table X. Simple Slopes by T1 Relationship Satisfaction) ///
varlabels(1._at "-1SD T1 Relationship Satisfaction" 2._at "Mean T1 Relationship Satisfaction" 3._at "+1SD T1 Relationship Satisfaction") addnotes ("Note. N = 231 Couples") label ///
mtitle ("Husbands Pre-Hurricane Slope" "Husbands Jump from Pre- to Post-Hurricane" ///
 "Husbands Post-Hurricane Slope" "Wives Pre-Hurricane Slope" ///
 "Wives Jump from Pre- to Post-Hurricane" "Wives Post-Hurricane Slope") nocons noobs nonum nobase replace 
