

********* Positive Teacher Feedback and Student Self-efficacy **********
*******************  A Potential Outcomes Approach  ********************
***************************  Using PISA 2018  **************************


** Header
cls //Clears STATA results window
clear all //Clears everything that has been stored
macro drop _all //All previously defined macros are dropped 

** Set up
global root_path "xxxx" //your working directory
global source_path "$root_path/source"
global data_path "$root_path/data"


*******************************  Variables Preparation  *******************************

** Notes on weighting variables: final student weight (w_fstuwt) and balanced repeated weights w_fsturwt1-80
/* w_fstuwt w_fsturwt1 w_fsturwt2 w_fsturwt3 w_fsturwt4 w_fsturwt5 w_fsturwt6 w_fsturwt7 w_fsturwt8 w_fsturwt9 w_fsturwt10 w_fsturwt11 w_fsturwt12 w_fsturwt13 w_fsturwt14 w_fsturwt15 w_fsturwt16 w_fsturwt17 w_fsturwt18 w_fsturwt19 w_fsturwt20 w_fsturwt21 w_fsturwt22 w_fsturwt23 w_fsturwt24 w_fsturwt25 w_fsturwt26 w_fsturwt27 w_fsturwt28 w_fsturwt29 w_fsturwt30 w_fsturwt31 w_fsturwt32 w_fsturwt33 w_fsturwt34 w_fsturwt35 w_fsturwt36 w_fsturwt37 w_fsturwt38 w_fsturwt39 w_fsturwt40 w_fsturwt41 w_fsturwt42 w_fsturwt43 w_fsturwt44 w_fsturwt45 w_fsturwt46 w_fsturwt47 w_fsturwt48 w_fsturwt49 w_fsturwt50 w_fsturwt51 w_fsturwt52 w_fsturwt53 w_fsturwt54 w_fsturwt55 w_fsturwt56 w_fsturwt57 w_fsturwt58 w_fsturwt59 w_fsturwt60 w_fsturwt61 w_fsturwt62 w_fsturwt63 w_fsturwt64 w_fsturwt65 w_fsturwt66 w_fsturwt67 w_fsturwt68 w_fsturwt69 w_fsturwt70 w_fsturwt71 w_fsturwt72 w_fsturwt73 w_fsturwt74 w_fsturwt75 w_fsturwt76 w_fsturwt77 w_fsturwt78 w_fsturwt79 w_fsturwt80 **

*/

use "$data_path/USA_causal.dta", clear // from USA sample of student questionnaire data and cognitive item data

** Outcome variable: Reading self efficacy

/* 
ST161Q01HA = I am a good reader.
ST161Q02HA = I am able to understand difficult texts
ST161Q03HA = I read fluently.
*/

// create composite variable for reading self-efficacy
ssc install mdesc
mdesc ST161Q01HA ST161Q02HA ST161Q03HA // 115(2.42)  125(2.63)  156(3.28)
alpha ST161Q01HA ST161Q02HA ST161Q03HA //0.85
generate efficacy = (ST161Q01HA + ST161Q02HA + ST161Q03HA) / 3 if ST161Q01HA!=. & ST161Q02HA!=. & ST161Q03HA!=.
mdesc efficacy //185 (3.89)


** Causal variable: Positive teacher feedback

//rename ST104Q02NA potafeed // The teacher gives me feedback on my strengths in this subject (test language)." 1= Never or almost never, 2=Some lessons, 3=Many lessons, and 4= Every lesson or almost every lesson.

des potafeed 


** Control variables

/*  
reading achievement: pv@read
female: female=1, male=0; 
ESCS: SES index; 
JOYREAD: Enjoyment of reading; 
COMPETE: Competitiveness
wellbeing: ST016Q01NA: Overall, how satisfied are you with your life as a whole these days?; 
parecoura: ST123Q04NA
peerlation: 
	ST034Q01TA: I feel like an outsider (or left out of things) at school.
	ST034Q02TA: I make friends easily at school. 
	ST034Q04TA: I feel awkward and out of place in my school.
	ST034Q06TA: I feel lonely at school.
	
stutearela: ST097Q01TA: Students don't listen to what the teacher says.

gen female = 0
replace female = 1 if ST004D01T == 1
replace female = . if ST004D01T == .

*/

// create composite variable for peer relationships
generate ST034Q02TA_r = 5 - ST034Q02TA if ST034Q02TA != . //reverse code ST034Q02TA
alpha ST034Q01TA ST034Q02TA_r ST034Q04TA ST034Q06TA //0.82
generate peerlation = (ST034Q01TA + ST034Q02TA_r + ST034Q04TA + ST034Q06TA) / 4 if ST034Q01TA!=. & ST034Q02TA_r!=. & ST034Q04TA!=. & ST034Q06TA!=.
mdesc peerlation //259 (5.44)




*******************************  Descriptive results  *******************************

/* test if missingness is related to independent variables */

gen missing = (efficacy==. | potafeed==. | female==. | ESCS==. | pv2read==. | COMPETE==. | JOYREAD==. | wellbeing ==. | peerlation==. | parecoura==.| stutearela==.)

foreach convar in efficacy potafeed ESCS pv2read JOYREAD COMPETE wellbeing peerlation parecoura stutearela {
	display  "[`convar']"
	ttest `convar', by(missing)
}

foreach catvar in female {
	display  "[`catvar']"
	tab `catvar' missing, chi row
}

* download required packages for pisa data analysis: repest
ssc install repest

// descriptive results of all variables
repest PISA, est(means efficacy potafeed female ESCS pv@read JOYREAD COMPETE wellbeing peerlation parecoura stutearela)


//Offer a naive estimate of the treatment effect (i.e., an unadjusted mean difference or its analog if your outcome variable is not interval scaled). Discuss whether the ATE or the ATT is your target parameter, with reference to the graph you have drawn.

* define treatment (those with higher teacher positive feedback) and control groups ((those with lower teacher positive feedback))

/*gen highpf = 0
replace highpf = 1 if potafeed == 3 | potafeed == 4
replace highpf = . if potafeed == .
*/

* naive estimate
repest PISA, est(means efficacy) by(highpf) // raw difference = 3.011024-2.812136=0.199

repest PISA, est(means pv@read female ESCS JOYREAD COMPETE wellbeing peerlation parecoura stutearela) by(highpf) 




*******************************************************************************
*** set up macros for specifications
*******************************************************************************

global outcome efficacy // reading self-efficacy
global treatment highpf // higher teacher positive feedback
global confound female ESCS JOYREAD COMPETE wellbeing peerlation parecoura stutearela


*******************************************************************************
*** regresssion estimators
*******************************************************************************
cd "xxxx" //your working directory

repest PISA, estimate(stata: reg $outcome $treatment $confound pv@read) by(cnt) results(add(r2))

  
***********************************
* Logistic regression with plausible values
***********************************

// Define survey design
svyset [pweight=w_fstuwt], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)

// Define plausible values
global plausible_values pv1read pv2read pv3read pv4read pv5read pv6read pv7read pv8read pv9read pv10read

// Create empty lists to store coefficients and variances
local i = 1
foreach pv of global plausible_values {
    // Estimate propensity scores with survey weights
    svy: logistic $treatment female ESCS `pv' JOYREAD COMPETE wellbeing peerlation parecoura stutearela
    
    // Store coefficients and variances in matrices
    matrix b_pscore_`i' = e(b)
    matrix V_pscore_`i' = vecdiag(e(V))'  // Store variances as a row vector
    
    // Predict propensity scores
    predict estpscore_`i'
    
    local i = `i' + 1
}

// Number of plausible values
local num_pv = 10

// Get the number of coefficients
matrix define temp_b = b_pscore_1
local num_coeffs = colsof(temp_b)

// Initialize matrices to combine results
matrix b_pscore_mean = J(1, `num_coeffs', 0)
matrix Vw_pscore = J(`num_coeffs', 1, 0)  // Within-imputation variances as a column vector

// Calculate mean coefficients and within-imputation variance
forvalues j = 1/`num_pv' {
    matrix b_pscore_mean = b_pscore_mean + b_pscore_`j'
    matrix Vw_pscore = Vw_pscore + V_pscore_`j'
}

matrix b_pscore_mean = b_pscore_mean / `num_pv'
matrix Vw_pscore = Vw_pscore / `num_pv'

// Calculate between-imputation variance
matrix Bb_pscore = J(`num_coeffs', `num_coeffs', 0)
forvalues j = 1/`num_pv' {
    matrix diff = b_pscore_`j' - b_pscore_mean
    matrix Bb_pscore = Bb_pscore + (diff' * diff)
}
matrix Bb_pscore = Bb_pscore / (`num_pv' - 1)

// Total variance
matrix Vt_pscore = J(`num_coeffs', 1, 0)
forvalues k = 1/`num_coeffs' {
    matrix Vt_pscore[`k', 1] = Vw_pscore[`k', 1] + (1 + 1/`num_pv') * Bb_pscore[`k', `k']
}

// Display combined results
matrix list b_pscore_mean
matrix list Vt_pscore


// Calculate Standard Errors
matrix std_errors = J(10, 1, 0)
forvalues i = 1/10 {
    matrix std_errors[`i', 1] = sqrt(Vt_pscore[`i', 1])
}
matrix list std_errors

// Calculate t-statistics
forvalues i = 1/10 {
    local b = b_pscore_mean[1, `i']
    local v = Vt_pscore[`i', 1]
    local se = sqrt(`v')
    local t = `b' / `se'
    di "Variable `i': b = `b', se = `se', t = `t'"
}

matrix t_stats = J(1, 10, 0)
forvalues i = 1/10 {
    local b = b_pscore_mean[1, `i']
    local v = Vt_pscore[`i', 1]
    local se = sqrt(`v')
    local t = `b' / `se'
    matrix t_stats[1, `i'] = `t'
}
matrix list t_stats

// Calculate p-values (using normal distribution for approximation)
matrix p_values = J(1, 10, 0)
forvalues i = 1/10 {
    matrix p_values[1, `i'] = 2 * (1 - normal(abs(t_stats[1, `i'])))
}
matrix list p_values


// Calculate mean estpscore
generate estpscore_mean = 0
forvalues j = 1/10 {
    replace estpscore_mean = estpscore_mean + estpscore_`j'
}
replace estpscore_mean = estpscore_mean / 10

// Calculate overall statistics of estpscore_mean
summarize estpscore_mean, detail



***********************************
* Matching with plausible values with variance accounting for within-imputation and between-imputation variance
***********************************

// Define survey design
svyset [pweight=w_fstuwt], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)

// Define plausible values
global plausible_values pv1read pv2read pv3read pv4read pv5read pv6read pv7read pv8read pv9read pv10read

// Initialize matrices to store results
foreach est in nn_ate nn_att nn_atc kernel_ate kernel_att kernel_atc {
    matrix `est' = J(2, 10, .)
}

// Loop through plausible values
local i = 1
foreach pv of global plausible_values {
    // Estimate propensity scores
    svy: logistic $treatment $confound `pv'
    predict pscore_`i', pr

    // Nearest neighbor matching
    kmatch ps $treatment $confound `pv' ($outcome), pscore(pscore_`i') nn(1) svy ate att atc
    di "Nearest Neighbor Matching - Plausible Value `i':"
    matrix nn_results = r(table)
    local nn_ate = nn_results[1,1]
    local nn_att = nn_results[1,2]
    local nn_atc = nn_results[1,3]
    local nn_ate_se = nn_results[2,1]
    local nn_att_se = nn_results[2,2]
    local nn_atc_se = nn_results[2,3]
    di "ATE: `nn_ate' (SE: `nn_ate_se')"
    di "ATT: `nn_att' (SE: `nn_att_se')"
    di "ATC: `nn_atc' (SE: `nn_atc_se')"
    matrix nn_ate[1, `i'] = `nn_ate'
    matrix nn_ate[2, `i'] = `nn_ate_se'
    matrix nn_att[1, `i'] = `nn_att'
    matrix nn_att[2, `i'] = `nn_att_se'
    matrix nn_atc[1, `i'] = `nn_atc'
    matrix nn_atc[2, `i'] = `nn_atc_se'

    // Kernel matching
    kmatch ps $treatment $confound `pv' ($outcome), pscore(pscore_`i') svy ate att atc kernel(epan)
    di "Kernel Matching - Plausible Value `i':"
    matrix kernel_results = r(table)
    local kernel_ate = kernel_results[1,1]
    local kernel_att = kernel_results[1,2]
    local kernel_atc = kernel_results[1,3]
    local kernel_ate_se = kernel_results[2,1]
    local kernel_att_se = kernel_results[2,2]
    local kernel_atc_se = kernel_results[2,3]
    di "ATE: `kernel_ate' (SE: `kernel_ate_se')"
    di "ATT: `kernel_att' (SE: `kernel_att_se')"
    di "ATC: `kernel_atc' (SE: `kernel_atc_se')"
    matrix kernel_ate[1, `i'] = `kernel_ate'
    matrix kernel_ate[2, `i'] = `kernel_ate_se'
    matrix kernel_att[1, `i'] = `kernel_att'
    matrix kernel_att[2, `i'] = `kernel_att_se'
    matrix kernel_atc[1, `i'] = `kernel_atc'
    matrix kernel_atc[2, `i'] = `kernel_atc_se'

    local i = `i' + 1
}

// Display matrices
foreach est in nn_ate nn_att nn_atc kernel_ate kernel_att kernel_atc {
    di "Matrix `est':"
    matrix list `est'
    di ""
}


// Define a program to calculate statistics
program define calc_stats, rclass
    args matrix_name
    
    // Step 1: Calculate mean
    mata: st_matrix("`matrix_name'_row1", st_matrix("`matrix_name'")[1,.])
    matrix `matrix_name'_mean = J(1, 1, 0)
    foreach i of numlist 1/10 {
        matrix `matrix_name'_mean = `matrix_name'_mean + `matrix_name'_row1[1,`i']
    }
    matrix `matrix_name'_mean = `matrix_name'_mean / 10
    scalar mean_`matrix_name' = `matrix_name'_mean[1,1]

    // Step 2: Calculate between-imputation variance
    matrix `matrix_name'_var = J(1, 1, 0)
    foreach i of numlist 1/10 {
        matrix `matrix_name'_var = `matrix_name'_var + (`matrix_name'_row1[1,`i'] - mean_`matrix_name')^2
    }
    matrix `matrix_name'_var = `matrix_name'_var / 9
    scalar between_var_`matrix_name' = `matrix_name'_var[1,1]

    // Step 3: Calculate within-imputation variance
    mata: st_matrix("`matrix_name'_row2", st_matrix("`matrix_name'")[2,.])
    matrix `matrix_name'_within_var = J(1, 1, 0)
    foreach i of numlist 1/10 {
        matrix `matrix_name'_within_var = `matrix_name'_within_var + `matrix_name'_row2[1,`i']^2
    }
    matrix `matrix_name'_within_var = `matrix_name'_within_var / 10
    scalar within_var_`matrix_name' = `matrix_name'_within_var[1,1]

    // Step 4: Calculate total variance
    scalar total_var_`matrix_name' = within_var_`matrix_name' + (1 + 1/10) * between_var_`matrix_name'

    // Calculate standard error
    scalar se_`matrix_name' = sqrt(total_var_`matrix_name')

    // Calculate degrees of freedom
    scalar r_`matrix_name' = (between_var_`matrix_name' * (1 + 1/10)) / within_var_`matrix_name'
    scalar df_`matrix_name' = (((10 + 1) / (3 * 10)) * ((1 / r_`matrix_name') ^ 2)) + (10 - 1)

    // Calculate t-statistic and p-value
    scalar t_`matrix_name' = mean_`matrix_name' / se_`matrix_name'
    scalar p_`matrix_name' = 2 * ttail(df_`matrix_name', abs(t_`matrix_name'))

    // Return results
    return scalar mean = mean_`matrix_name'
    return scalar se = se_`matrix_name'
    return scalar t = t_`matrix_name'
    return scalar df = df_`matrix_name'
    return scalar p = p_`matrix_name'
end

// Calculate and display results for all estimates
foreach est in nn_ate nn_att nn_atc kernel_ate kernel_att kernel_atc {
    calc_stats `est'
    di "`est' estimate: " r(mean)
    di "`est' standard error: " r(se)
    di "`est' t-statistic: " r(t)
    di "`est' degrees of freedom: " r(df)
    di "`est' p-value: " r(p)
    di ""
}




*******************************************************************************
*** weighting with plausible values (including all covariates) with variance accounting for within-imputation and between-imputation variance
*******************************************************************************
drop estpscore_1 estpscore_2 estpscore_3 estpscore_4 estpscore_5 estpscore_6 estpscore_7 estpscore_8 estpscore_9 estpscore_10
// Get the list of all covariates
local covariates $confound `pv'

// Count the number of covariates
local num_covariates: word count `covariates'

// Initialize matrices to store results
matrix att_results = J(10, 4 * (2 + `num_covariates'), .)  // 10 rows for PVs, 4 columns (coef, SE, t, p) for each variable (including intercept and treatment)
matrix atc_results = J(10, 4 * (2 + `num_covariates'), .)

// Loop through plausible values
forvalues i = 1/10 {
    local pv = "pv`i'read"
    
    // Step 1: Estimate Propensity Scores
    // drop estpscore_`i' w_att_`i' w_atc_`i' // if needed
    svyset [pweight=w_fstuwt], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
    quietly svy: logistic $treatment $confound `pv'
    predict estpscore_`i'
    
    // Step 2: Generate IPW Weights for ATT and ATC
    gen w_att_`i' = estpscore_`i' * w_fstuwt / (1 - estpscore_`i')
    replace w_att_`i' = w_fstuwt if $treatment == 1
    gen w_atc_`i' = (1 - estpscore_`i') * w_fstuwt / estpscore_`i'
    replace w_atc_`i' = w_fstuwt if $treatment == 0
    
    // Step 3: Estimate Treatment Effects
    // ATT
    svyset [pweight=w_att_`i'], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
    quietly svy: regress $outcome $treatment $confound `pv'
    
    local col = 1
    foreach var in _cons $treatment `covariates' {
        matrix att_results[`i', `col'] = _b[`var']
        matrix att_results[`i', `col'+1] = _se[`var']
        matrix att_results[`i', `col'+2] = _b[`var'] / _se[`var']
        matrix att_results[`i', `col'+3] = 2 * ttail(e(df_r), abs(_b[`var'] / _se[`var']))
        local col = `col' + 4
    }
    
    // ATC
    svyset [pweight=w_atc_`i'], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
    quietly svy: regress $outcome $treatment $confound `pv'
    
    local col = 1
    foreach var in _cons $treatment `covariates' {
        matrix atc_results[`i', `col'] = _b[`var']
        matrix atc_results[`i', `col'+1] = _se[`var']
        matrix atc_results[`i', `col'+2] = _b[`var'] / _se[`var']
        matrix atc_results[`i', `col'+3] = 2 * ttail(e(df_r), abs(_b[`var'] / _se[`var']))
        local col = `col' + 4
    }
}

// Calculate combined estimates using Rubin's rules
foreach est in att atc {
    di "Results for `est':"
    di "---------------"
    
    local col = 1
    foreach var in _cons $treatment `covariates' {
        // Calculate mean (Q-bar)
        mata: st_numscalar("mean_`est'_`var'", mean(st_matrix("`est'_results")[.,`col']))
        
        // Calculate between-imputation variance (B)
        mata: st_numscalar("between_var_`est'_`var'", variance(st_matrix("`est'_results")[.,`col']))
        
        // Calculate within-imputation variance (W-bar)
        mata: st_numscalar("within_var_`est'_`var'", mean(st_matrix("`est'_results")[.,`col'+1]:^2))
        
        // Calculate total variance (T)
        scalar total_var_`est'_`var' = within_var_`est'_`var' + (1 + 1/10) * between_var_`est'_`var'
        
        // Calculate standard error
        scalar se_`est'_`var' = sqrt(total_var_`est'_`var')
        
        // Calculate degrees of freedom
        scalar r_`est'_`var' = (between_var_`est'_`var' * (1 + 1/10)) / within_var_`est'_`var'
        scalar df_`est'_`var' = (((10 + 1) / (3 * 10)) * ((1 / r_`est'_`var') ^ 2)) + (10 - 1)
        
        // Calculate t-statistic and p-value
        scalar t_`est'_`var' = mean_`est'_`var' / se_`est'_`var'
        scalar p_`est'_`var' = 2 * ttail(df_`est'_`var', abs(t_`est'_`var'))
        
        // Display results
        di "`var' estimate: " mean_`est'_`var'
        di "`var' standard error: " se_`est'_`var'
        di "`var' t-statistic: " t_`est'_`var'
        di "`var' degrees of freedom: " df_`est'_`var'
        di "`var' p-value: " p_`est'_`var'
        di ""
        
        local col = `col' + 4
    }
}


*******************************************************************************
*** weighting with plausible values and common support restriction
*******************************************************************************
drop estpscore_1 estpscore_2 estpscore_3 estpscore_4 estpscore_5 estpscore_6 estpscore_7 estpscore_8 estpscore_9 estpscore_10

drop w_att_1 w_att_2 w_att_3 w_att_4 w_att_5 w_att_6 w_att_7 w_att_8 w_att_9 w_att_10
drop w_atc_1 w_atc_2 w_atc_3 w_atc_4 w_atc_5 w_atc_6 w_atc_7 w_atc_8 w_atc_9 w_atc_10

// Get the list of all covariates
local covariates $confound

// Count the number of covariates
local num_covariates: word count `covariates'

// Initialize matrices to store results
matrix att_results = J(10, 4 * (2 + `num_covariates' + 1), .)  // +1 for the plausible value
matrix atc_results = J(10, 4 * (2 + `num_covariates' + 1), .)

// Loop through plausible values
forvalues i = 1/10 {
    local pv = "pv`i'read"
    
    // Step 1: Estimate Propensity Scores
    capture drop estpscore_`i' w_att_`i' w_atc_`i' in_support_`i'
    svyset [pweight=w_fstuwt], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
    quietly svy: logistic $treatment $confound `pv'
    predict estpscore_`i', pr
    
    // Step 2: Determine common support region
    sum estpscore_`i' if $treatment == 1
    local min_treat = r(min)
    local max_treat = r(max)
    sum estpscore_`i' if $treatment == 0
    local min_control = r(min)
    local max_control = r(max)
    
    local min_common = max(`min_treat', `min_control')
    local max_common = min(`max_treat', `max_control')
    
    // Generate indicator for common support
    gen in_support_`i' = (estpscore_`i' >= `min_common' & estpscore_`i' <= `max_common')
    
    // Step 3: Generate IPW Weights for ATT and ATC (only for observations in common support)
    gen w_att_`i' = estpscore_`i' * w_fstuwt / (1 - estpscore_`i') if in_support_`i' == 1
    replace w_att_`i' = w_fstuwt if $treatment == 1 & in_support_`i' == 1
    gen w_atc_`i' = (1 - estpscore_`i') * w_fstuwt / estpscore_`i' if in_support_`i' == 1
    replace w_atc_`i' = w_fstuwt if $treatment == 0 & in_support_`i' == 1
    
    // Step 4: Estimate Treatment Effects
    // ATT
    svyset [pweight=w_att_`i'], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
    quietly svy: regress $outcome $treatment $confound `pv' if in_support_`i' == 1
    
    local col = 1
    foreach var in _cons $treatment `covariates' `pv' {
        matrix att_results[`i', `col'] = _b[`var']
        matrix att_results[`i', `col'+1] = _se[`var']
        matrix att_results[`i', `col'+2] = _b[`var'] / _se[`var']
        matrix att_results[`i', `col'+3] = 2 * ttail(e(df_r), abs(_b[`var'] / _se[`var']))
        local col = `col' + 4
    }
    
    // ATC
    svyset [pweight=w_atc_`i'], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
    quietly svy: regress $outcome $treatment $confound `pv' if in_support_`i' == 1
    
    local col = 1
    foreach var in _cons $treatment `covariates' `pv' {
        matrix atc_results[`i', `col'] = _b[`var']
        matrix atc_results[`i', `col'+1] = _se[`var']
        matrix atc_results[`i', `col'+2] = _b[`var'] / _se[`var']
        matrix atc_results[`i', `col'+3] = 2 * ttail(e(df_r), abs(_b[`var'] / _se[`var']))
        local col = `col' + 4
    }
    
    // Display number of observations in common support
    count if in_support_`i' == 1
    di "Plausible value `i': Number of observations in common support = " r(N)
}

// Calculate combined estimates using Rubin's rules
foreach est in att atc {
    di "Results for `est' with common support restriction:"
    di "------------------------------------------------"
    
    local col = 1
    foreach var in _cons $treatment `covariates' `pv' {
        // Calculate mean (Q-bar)
        mata: st_numscalar("mean_`est'_`var'", mean(st_matrix("`est'_results")[.,`col']))
        
        // Calculate between-imputation variance (B)
        mata: st_numscalar("between_var_`est'_`var'", variance(st_matrix("`est'_results")[.,`col']))
        
        // Calculate within-imputation variance (W-bar)
        mata: st_numscalar("within_var_`est'_`var'", mean(st_matrix("`est'_results")[.,`col'+1]:^2))
        
        // Calculate total variance (T)
        scalar total_var_`est'_`var' = within_var_`est'_`var' + (1 + 1/10) * between_var_`est'_`var'
        
        // Calculate standard error
        scalar se_`est'_`var' = sqrt(total_var_`est'_`var')
        
        // Calculate degrees of freedom
        scalar r_`est'_`var' = (between_var_`est'_`var' * (1 + 1/10)) / within_var_`est'_`var'
        scalar df_`est'_`var' = (((10 + 1) / (3 * 10)) * ((1 / r_`est'_`var') ^ 2)) + (10 - 1)
        
        // Calculate t-statistic and p-value
        scalar t_`est'_`var' = mean_`est'_`var' / se_`est'_`var'
        scalar p_`est'_`var' = 2 * ttail(df_`est'_`var', abs(t_`est'_`var'))
        
        // Display results
        di "`var' estimate: " mean_`est'_`var'
        di "`var' standard error: " se_`est'_`var'
        di "`var' t-statistic: " t_`est'_`var'
        di "`var' degrees of freedom: " df_`est'_`var'
        di "`var' p-value: " p_`est'_`var'
        di ""
        
        local col = `col' + 4
    }
}

/* Balance Diagnostics */
// Balance diagnostics for ATT weights
svyset [pweight=w_att_2], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
svy: mean female ESCS pv2read JOYREAD COMPETE wellbeing peerlation parecoura stutearela, over(highpf)

// Balance diagnostics for ATC weights
svyset [pweight=w_atc_2], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
svy: mean female ESCS pv2read JOYREAD COMPETE wellbeing peerlation parecoura stutearela, over(highpf)

// Balance diagnostics before inverse probability weighting
svyset [pweight=w_fstuwt], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)
svy: mean female ESCS pv2read JOYREAD COMPETE wellbeing peerlation parecoura stutearela, over(highpf)


************************************************************************
/*  Visualization of balance results before and after weighting without plausible values*/
************************************************************************

	************  Start of Kernel Density Plot 	************ 

cd "xxxx" //your working directory
* Estimate Propensity Scores with Final Student Weights and Balanced Repeated Weights *
// Define survey design
svyset [pweight=w_fstuwt], brrweight(w_fsturwt1-w_fsturwt80) vce(brr)

// Estimate propensity scores with survey weights (pweight & BRR weights)
svy: logistic $treatment $confound pv2read
predict estpscore

sum estpscore

* Generate IPW Weights for ATT and ATC *
//weights ATT (with sampling weight pweight)
gen w_att=estpscore*w_fstuwt/(1-estpscore)
replace w_att=w_fstuwt if $treatment==1

//weights ATC (with sampling weight pweight)
gen w_atc=(1-estpscore)*w_fstuwt/estpscore
replace w_atc=w_fstuwt if $treatment==0

// Verify weights
sum w_att w_atc

* Define the list of covariates *
local covariates ESCS pv2read JOYREAD COMPETE wellbeing peerlation parecoura stutearela female

* Loop over each covariate *
foreach var of local covariates {
    * Kernel density plot before weighting
    twoway (kdensity `var' if highpf == 0, lcolor(blue)) (kdensity `var' if highpf == 1, lcolor(red)), ///
        title("Before Weighting") legend(off) name(density_before_`var', replace)
    
    * Kernel density plot after ATT weighting
    twoway (kdensity `var' [w = w_att] if highpf == 0, lcolor(blue)) (kdensity `var' [w = w_att] if highpf == 1, lcolor(red)), ///
        title("After ATT Weighting") legend(off) name(density_att_`var', replace)
    
    * Kernel density plot after ATC weighting
    twoway (kdensity `var' [w = w_atc] if highpf == 0, lcolor(blue)) (kdensity `var' [w = w_atc] if highpf == 1, lcolor(red)), ///
        title("After ATC Weighting") legend(off) name(density_atc_`var', replace)
    
    * Combine density plots with aspect ratio
    graph combine density_before_`var' density_att_`var' density_atc_`var', ///
        col(3) title("Density Plots for `var'") name(combined_density_`var', replace)
    
    * Save each combined plot as a PNG file
    graph export "combined_density_`var'.png", replace
}


	************  End of Kernel Density Plot 	************ 




