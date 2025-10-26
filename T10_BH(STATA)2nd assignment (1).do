//T10_BH
//Data storage, as per user's computer storage location, subject to change
use "C:\Users\91801\Downloads\36151-0002-Data (3).dta"
//excluding the state of Bihar from dataset
keep if STATEID == 10
//replacing zero for missing values
mvencode CO1X CO2X CO3X CO5X CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO13X CO14X CO15 CO16 CO17 CO19 CO20, mv(0) override
//generating required variables
gen fdexp = CO1X+CO2X+CO3X+CO5X+CO6X+CO7X+CO8X+CO9X+CO10X+CO11X+CO12X+CO13X+CO14X+CO15+CO16+CO17+CO19+CO20
gen fdshare = fdexp/COTOTAL
gen lncototal = log(COTOTAL)
gen sqlncototal = lncototal^2
//Simple regression with lncototal
reg fdshare lncototal//MODEL-1A
//Multiple regression with quadratic term for lncototal
reg fdshare lncototal sqlncototal//MODEL-1B
//finding turning point in the graph
twoway scatter fdshare lncototal || qfit fdshare lncototal, ytitle(Food Consumption Share) xtitle(log of COTOTAL)
graph export "C:\Users\91801\Downloads\2ND INT GRAPH-1.png", as(png) replace
twoway scatter fdshare sqlncototal|| qfit fdshare sqlncototal, ytitle(Food Consumption Share) xtitle(square log of COTOTAL)
graph export "C:\Users\91801\Downloads\2nd INT GRAPH-2.png", as(png) replace
//generating new variables for persons in the household based on different gender and age
gen NADULTM_n= NADULTM- NELDERM
gen NADULTF_n= NADULTF- NELDERF
gen n=NADULTM_n+NADULTF_n+NCHILDM+NCHILDF+NTEENM+NTEENF+NELDERM+NELDERF
keep if n== NPERSONS
gen n1=NCHILDF/NPERSONS
gen n2=NCHILDM/NPERSONS
gen n3=NTEENF/NPERSONS
gen n4=NTEENM/NPERSONS
gen n5=NADULTF_n/NPERSONS
gen n6=NADULTM_n/NPERSONS
gen n7=NELDERF/NPERSONS
gen n8=NELDERM/NPERSONS
//Multiple regression for previous model with added variables for persons in the household
reg fdshare lncototal sqlncototal NPERSONS n1 n2 n3 n4 n5 n6 n7 n8//MODEL-2A
reg fdshare lncototal sqlncototal NPERSONS n1 n2 n3 n4 n5 n6 n7//MODEL-2B
twoway scatter fdshare lncototal || qfit fdshare lncototal, ytitle(Food Consumption Share) xtitle(log of COTOTAL)
graph export "C:\Users\91801\Downloads\2ND INT GRAPH-3.png", as(png) replace
tab URBAN2011
//Simple regression with dummy variables for base = URBAN
reg fdshare ib1.URBAN2011//MODEL-3A
//Simple regression with dummy variables for base = RURAL
reg fdshare i.URBAN2011//MODEL-3B
//Multiple regression with dummy variables for base = URBAN
reg fdshare lncototal sqlncototal ib1.URBAN2011//MODEL-3C
//Multiple regression with dummy variables for base = RURAL
reg fdshare lncototal sqlncototal  i.URBAN2011//MODEL-3D
tab GROUPS, gen(grp)
//Simple regression considering GROUPS as a single independent variable
reg fdshare GROUPS//MODEL-4A
//Simple regression considering GROUPS as dummy variable with base group-7
reg fdshare ib7.GROUPS//MODEL-4B
//Two-tail test for average food share comparison between base groups and group-1
test 1.GROUPS == 0
//One-tail test for coefficient comparison between group-5 and group-6
test 5.GROUPS - 6.GROUPS = 0
local sign_for_b= sign(_b[5.GROUPS]-_b[6.GROUPS])
display "H_0: Beta_5_Adivasi <= Beta_6_Muslim p-value = " ttail(r(df_r),`sign_for_b'*sqrt(r(F)))
//Question-5
reg fdshare lncototal if lncototal<10.125
reg fdshare lncototal if lncototal>=10.125
gen lncototal_3=lncototal-10.125
gen lncototal_4=lncototal-10.125
replace lncototal_3=0 if lncototal>=10.125
replace lncototal_4=0 if lncototal<10.125
reg fdshare lncototal_3 if lncototal<10.125
reg fdshare lncototal_4 if lncototal>=10.125
generate dum3 = 1
replace  dum3 = 0 if lncototal>=10.125
generate dum4 = 1
replace  dum4 = 0 if lncototal<10.125
reg fdshare lncototal_3 lncototal_4 dum3 dum4, nocon//Piecewise regression
//Question 6
reg fdshare lncototal sqlncototal
scalar rss1=e(rss)
reg fdshare lncototal sqlncototal if URBAN2011==0
scalar rss2=e(rss)
scalar no1=e(N)
reg fdshare lncototal sqlncototal if URBAN2011==1
scalar no2=e(N)
scalar rss3=e(rss)
scalar k=3
scalar chowtest=(((rss1 - rss2 -rss3 )/(k))/(((rss2+rss3)/(no1+no2-2*k))))//Chow Test
display chowtest
di Ftail(k, no1+no2-2*k, chowtest)
//Question 6 next part
//Instead of two models for two different regions using one model with dummy variables
gen lnx=lncototal*URBAN2011
gen lnx2=sqlncototal*URBAN2011
reg fdshare lncototal sqlncototal  lnx lnx2 URBAN2011
test lnx lnx2 URBAN2011
