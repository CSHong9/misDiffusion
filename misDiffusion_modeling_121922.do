** Project: The Diffusion of Online Vaccine Misinformation: Fake News Publishers and the Paradox of Embeddedness
** Purpose: Modeling
** Code written by Chen-Shuo Hong
** All rights reserved
** Last revision: 12/19/2022

clear
pwd

cd "C:\Analyses"

capture log close
log using "modeling_logfile.smcl", replace

/**** BROADCASTING ANALYSIS ****/
*unit of analysis: site-day
use sitepanel_upd, clear

*outcome: number of posts per day
rename v1 panel_id
rename v3 date
encode location, gen(nloc)
encode revenue, gen(nrev)
encode traffic, gen(ntraf)

*recode country
gen country = 5
replace country = 1 if nloc == 18
replace country = 2 if nloc == 15
replace country = 3 if nloc == 17
replace country = 4 if nloc == 12

lab def country 1 "Unknown" 2 "USA" 3 "United Kingdom" 4 "Russia" 5 "Others"
lab val country country

*recode revenue
gen rev = 6
replace rev = 1 if nrev == 2 
replace rev = 2 if nrev == 3
replace rev = 3 if nrev == 1 | nrev == 4
replace rev = 4 if nrev == 5 | nrev == 8 | nrev == 9
replace rev = 5 if nrev == 7

lab def rev 1 "Unknown" 2 "Online advertising" 3 "Donation" 4 "Fees/Sales/Sponsor" 5 "Organization support" 6 "Mixed"
lab val rev rev

/**Model 1**/
nbreg post_fill lag_logmeanfw, vce(cluster time_id) nolog
estat ic
estimates store m1, title(Model 1)
/**Model 2**/
nbreg post_fill lag_logmeanfw lag_trans_local, vce(cluster time_id) nolog
estat ic
estimates store m2, title(Model 2)
/**Model 3**/
nbreg post_fill lag_logmeanfw lag_trans_local lag_trans_localsq, vce(cluster time_id) nolog
estat ic
estimates store m3, title(Model 3)
/**Model 4**/
nbreg post_fill lag_logmeanfw lag_trans_local lag_trans_localsq lag_isoego lag_audcen ib2.country ib2.rev ib3.ntraf, vce(cluster time_id) nolog
estat ic
estimates store m4, title(Model 4)
/**Model 5**/
nbreg post_fill lag_logmeanfw lag_isoego lag_trans_local lag_trans_localsq lag_audcen ib2.country ib2.rev ib3.ntraf lag_propsen, vce(cluster time_id) nolog
estat ic
estimates store m5, title(Model 5)

estadd fitstat : * 
esttab m1 m2 m3 m4 m5 using table2_20221122.rtf, b se scalars(r2_mf r2_mfadj aic0 aic_n bic0 bic_p statabic) mtitles 

*Graph Settings
grstyle clear
set scheme s2color
grstyle init
grstyle set plain, box
grstyle color background white
grstyle set color Set1
grstyle yesno draw_major_hgrid yes
grstyle yesno draw_major_ygrid yes
grstyle color major_grid gs8
grstyle linepattern major_grid dot
grstyle set legend 4, box inside
grstyle color ci_area gs12%50

*predictive margins
nbreg post_fill lag_logmeanfw c.lag_trans_local##c.lag_trans_local, vce(cluster time_id) nolog
margins, at(lag_trans_local=(0(0.01)1)) vsquish
marginsplot, recast(line) plot1opts(lcolor(gs8)) ciopt(color(black%20)) recastci(rarea) xtitle("Weighted Closure") ytitle("Effects on Number of Posts")

/**** Sensitivity Analysis ****/
*Fixed Effect Models
/** Conditional **/
xtset site_id time_id
xtnbreg post_fill lag_logmeanfw lag_trans_local lag_trans_localsq lag_isoego lag_audcen lag_propsen i.time_id, fe nolog
estimates store m6, title(Model 6a)
/** Unconditional **/
nbreg post_fill lag_logmeanfw lag_trans_local lag_trans_localsq lag_isoego lag_audcen lag_propsen i.time_id i.site_id, nolog vce(robust)
estimates store m7, title(Model 6b)
esttab m6 m7 using tableA3_20221124.rtf, b se scalars(r2_mf r2_mfadj aic0 aic_n bic0 bic_p statabic) mtitles 

/**** PERSON-TO-PERSON CONTAGION ****/
*unit of analysis: message
use messagepanel_upd, clear

*outcome: number of retweets + likes per tweet
rename v1 panel_id
encode location, gen(nloc)
encode revenue, gen(nrev)
encode traffic, gen(ntraf)

*recode country
gen country = 5
replace country = 1 if nloc == 18
replace country = 2 if nloc == 15
replace country = 3 if nloc == 17
replace country = 4 if nloc == 12

lab def country 1 "Unknown" 2 "USA" 3 "United Kingdom" 4 "Russia" 5 "Others"
lab val country country

*recode revenue
gen rev = 6
replace rev = 1 if nrev == 2 
replace rev = 2 if nrev == 3
replace rev = 3 if nrev == 1 | nrev == 4
replace rev = 4 if nrev == 5 | nrev == 8 | nrev == 9
replace rev = 5 if nrev == 7

lab def rev 1 "Unknown" 2 "Online advertising" 3 "Donation" 4 "Fees/Sales/Sponsor" 5 "Organization support" 6 "Mixed"
lab val rev rev

*recode sensitive contents
gen sen = 0
replace sen = 1 if possibly_sensitive == "True"

egen stdfollower = std(followers_count)

tostring user_id, gen(user_id2)
tostring time_id, gen(time_id2)
gen usti = user_id2+time_id2

/**Model 1**/
menbreg cgn lag_trans_local || user_id:
estimates store m1, title(Model 1)
estat ic
/**Model 2**/
menbreg cgn lag_trans_local lag_trans_localsq || user_id:
estimates store m2, title(Model 2)
estat ic
/**Model 3**/
menbreg cgn lag_trans_local lag_trans_localsq logfollower || time_id: || usti:
estimates store m3, title(Model 3)
estat ic
/**Model 4**/
menbreg cgn lag_trans_local lag_trans_localsq logfollower lag_isoego ib2.country ib2.rev ib3.ntraf || user_id:
estimates store m4, title(Model 4)
estat ic
/**Model 5**/
/* bot score? */
menbreg cgn lag_trans_local lag_trans_localsq logfollower lag_isoego ib2.country ib2.rev ib3.ntraf sen || user_id:
estimates store m5, title(Model 5)
estat ic

*estadd fitstat : * 
esttab m1 m2 m3 m4 m5 /*using example_message_upd.rtf*/ using table3_20221122.rtf, b se scalars(r2_mf r2_mfadj aic0 aic_n bic0 bic_p statabic) mtitles 

*predictive margins
menbreg cgn c.lag_trans_local##c.lag_trans_local logfollower || time_id: || usti:
margins, at(lag_trans_local=(0(0.01)1) logfollower = 2) atmean vsquish
marginsplot, recast(line) plot1opts(lcolor(gs8)) ciopt(color(black%20)) recastci(rarea) xtitle("Weighted Closure") ytitle("Effects on Number of Retweets/Likes")

/**** Sensitivity Analysis ****/
*Fixed Effect Models
/** Conditional **/
xtset user_id
xtnbreg cgn lag_trans_local lag_trans_localsq logfollower sen lag_isoego ib2.country ib2.rev ib3.ntraf, fe nolog vce()
estimates store m6, title(Model 6a)
estat ic
*esttab m6 using tableA3b_20221124.rtf, b se scalars(r2_mf r2_mfadj aic0 aic_n bic0 bic_p statabic) mtitles 
/** Unconditional **/
set emptycells drop 
xi: nbreg cgn lag_trans_local lag_trans_localsq logfollower lag_isoego sen i.user_id, nolog
*estimates store m7, title(Model 6b)
**because the dummies exceed maxvar in STATA, using R to estimate uncoditional models
save messagepanel_upd_R, replace
**below is the R script
library(haven)
library(fixest)
library(tidyverse)
library(MASS)
data <- read_dta("messagepanel_upd_R.dta")
m7 <- fenegbin(cgn ~ lag_trans_local + 
               lag_trans_localsq + logfollower + lag_isoego +
               sen  + as.factor(country) + as.factor(rev) + as.factor(ntraf) 
               |  as.factor(user_id2), data = data, nthreads = 8, vcov = 'hc1')
summary(m7)

log close

