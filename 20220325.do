*******All control city FE
*******************2SLS*************
*****************the most significant*************************
gen sample1=cond(age>=45&age<=70,1,0)
gen sample3=cond(age>=45&age<=70,1,0)
gen sample4=cond(age>=45&age<=70&male==1,1,0)
gen sample5=cond(age>=45&age<=70&male==0,1,0)


/*&&
preserve
keep if lag_working==1&nopaypension~=1&cadre~=1&Nc>=10
eststo clear 
eststo:xi:ivreg2 working (housepw1_grw =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid    lngdp   hospital_beds      chess exercise chess  if sample7==1,r cluster(cityid) first
sum hhexp_per if e(sample)==1
sum age  male  urban married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget iv71_p dlag_finrev_p   lngdp  popudensi hospital_beds   migrapercent   chess exercise chess if e(sample)==1
esttab using 04582.csv,replace   cells(b(star fmt(%9.3f)) p(par))  r2(3)   star(# 0.15 * 0.1 ** 0.05 *** 0.01) stats( N mean widstat idp jp,fmt( %9.0f %9.4f %9.3f %9.3f %9.4f %9.4f)) 
!C:\Users\CC\04582.csv
*/
preserve
keep if lag_working==1&Nc>=10&nopaypension~=1&cadre~=1  
eststo clear 
forvalue i=3/5 { 
eststo:xi:ivreg2 working          (housepw1_grw =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw  hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess   if sample`i'==1,r cluster(cityid) first
          matrix a=e(first)
          replace a1=round(a[3,1],0.01)
		 * replace a2=round(a[6,1],0.001)
		 * estadd local pvalue=round(a2,0.001)
          estadd local Fstat=round(a1,0.01)
		  sum working if e(sample)
		  estadd r(mean),replace
}
forvalue i=3/5 { 
eststo:xi:ivreg2 lnworkhourall_we (housepw1_grw =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw  hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid  lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess  if sample`i'==1,r cluster(cityid) first
          matrix a=e(first)
          replace a1=round(a[3,1],0.01)
		 * replace a2=round(a[6,1],0.001)
		 * estadd local pvalue=round(a2,0.001)
          estadd local Fstat=round(a1,0.01)
		  sum lnworkhourall_we if e(sample)
		  estadd r(mean),replace
}
esttab using 04581.csv,replace   cells(b(star fmt(%9.3f)) se(par))  r2(3)   star(# 0.15 * 0.1 ** 0.05 *** 0.01) stats( N mean widstat idp jp,fmt( %9.0f %9.4f %9.3f %9.3f %9.4f %9.4f)) 
!C:\Users\CC\04581.csv
beep

*********************************first stage
preserve
keep if lag_working==1&Nc>=10&nopaypension~=1&cadre~=1  
eststo clear  
forvalue i=3/5 {
xi:ivreg2 working         (housepw1_grw =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess   if sample`i'==1,r cluster(cityid) first
eststo:xi:reg  housepw1_grw               iv71_p dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess   if e(sample)==1,r cluster(cityid) 
          test   iv71_p
          estadd local Fstat=round(r(F),0.01)
		  sum housepw1_grw if e(sample)
		  estadd r(mean),replace
}
forvalue i=3/5 {
xi:ivreg2 lnworkhourall_we (housepw1_grw =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess   if sample`i'==1,r cluster(cityid) first
eststo:xi:reg  housepw1_grw                iv71_p dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess   if e(sample)==1,r cluster(cityid) 
          test   iv71_p
          estadd local Fstat=round(r(F),0.01)
		  sum housepw1_grw if e(sample)
		  estadd r(mean),replace
}
esttab using 0458.csv,replace   cells(b(star fmt(%9.3f)) se(par))  r2(3)   star(# 0.15 * 0.1 ** 0.05 *** 0.01) stats( N mean Fstat idp jp,fmt( %9.0f %9.4f %9.3f %9.3f %9.4f %9.4f)) 
!C:\Users\CC\0458.csv

*********OLS
preserve
keep if lag_working==1&Nc>=10&nopaypension~=1&cadre~=1  
eststo clear
forvalue i=3/5   {
eststo:xi:reg working          housepw1_grw  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess  if sample`i'==1,r cluster(cityid)
}
forvalue i=3/5   {
eststo:xi:reg lnworkhourall_we housepw1_grw  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  population landarea hospital_beds migrapercent   association  exercise  chess  if sample`i'==1,r cluster(cityid)
}
esttab using 0458.csv,replace   cells(b(star fmt(%9.3f)) se(par))  r2(3)   star(# 0.15 * 0.1 ** 0.05 *** 0.01) stats( N mean Fstat idp jp,fmt( %9.0f %9.4f %9.3f %9.3f %9.4f %9.4f)) 
!C:\Users\CC\0458.csv


***communtiy level home value
****2sls
preserve
keep if lag_working==1&Nc>=10&nopaypension~=1&cadre~=1  
eststo clear 
forvalue i=3/5 { 
eststo:xi:ivreg2 working          (houseprc_gr =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess  if sample`i'==1,r cluster(cityid) first
          matrix a=e(first)
          replace a1=round(a[3,1],0.01)
		 * replace a2=round(a[6,1],0.001)
		 * estadd local pvalue=round(a2,0.001)
          estadd local Fstat=round(a1,0.01)
		  sum working if e(sample)
		  estadd r(mean),replace
}

forvalue i=3/5 { 
eststo:xi:ivreg2 lnworkhourall_we (houseprc_gr =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess  if sample`i'==1,r cluster(cityid) first
          matrix a=e(first)
          replace a1=round(a[3,1],0.01)
		 * replace a2=round(a[6,1],0.001)
		 * estadd local pvalue=round(a2,0.001)
          estadd local Fstat=round(a1,0.01)
		  sum lnworkhourall_we if e(sample)
		  estadd r(mean),replace
}
esttab using 04582.csv,replace   cells(b(star fmt(%9.3f)) se(par))  r2(3)   star(# 0.15 * 0.1 ** 0.05 *** 0.01) stats( N mean widstat Fstat arf idp jp,fmt( %9.0f %9.4f %9.3f %9.3f %9.4f %9.4f)) 
!C:\Users\CC\04582.csv
*********************************first stage
preserve
keep if lag_working==1&Nc>=10&nopaypension~=1&cadre~=1  
eststo clear  
forvalue i=3/5 {
xi:ivreg2 working          (houseprc_gr =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess   if sample`i'==1,r cluster(cityid) first
eststo:xi:reg  houseprc_gr                iv71_p dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess  if e(sample)==1,r cluster(cityid) 
          test   iv71_p
          estadd local Fstat=round(r(F),0.01)
		  sum houseprc_gr if e(sample)
		  estadd r(mean),replace
}
forvalue i=3/5 {
xi:ivreg2 lnworkhourall_we (houseprc_gr =iv71_p) dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess if sample`i'==1,r cluster(cityid) first
eststo:xi:reg  houseprc_gr                iv71_p dlag_finrev_p  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess  if e(sample)==1,r cluster(cityid) 
          test   iv71_p
          estadd local Fstat=round(r(F),0.01)
		  sum houseprc_gr if e(sample)
		  estadd r(mean),replace
}
esttab using 04581.csv,replace   cells(b(star fmt(%9.3f)) se(par))  r2(3)   star(# 0.15 * 0.1 ** 0.05 *** 0.01) stats( N mean Fstat idp jp,fmt( %9.0f %9.4f %9.3f %9.3f %9.4f %9.4f)) 
!C:\Users\CC\04581.csv

*********OLS
preserve
keep if lag_working==1&Nc>=10&nopaypension~=1&cadre~=1  
eststo clear
forvalue i=3/5   {
eststo:xi:reg working          houseprc_gr  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess if sample`i'==1,r cluster(cityid)
}
forvalue i=3/5   {
eststo:xi:reg lnworkhourall_we houseprc_gr  age age2 male  urbanhk married edu ln_hhincw hhsize childnum childnummissing poorhlh nopension penget i.wave i.cityid lngdp_per lnbgt_expend_per  popudensi landarea hospital_beds migrapercent   association  exercise  chess if sample`i'==1,r cluster(cityid)
}
esttab using 0458.csv,replace   cells(b(star fmt(%9.3f)) se(par))  r2(3)   star(# 0.15 * 0.1 ** 0.05 *** 0.01) stats( N mean Fstat idp jp,fmt( %9.0f %9.4f %9.3f %9.3f %9.4f %9.4f)) 
!C:\Users\CC\0458.csv

