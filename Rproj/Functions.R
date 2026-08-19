# Functions Associated with Meta-Analysis ---- 


## Log-Transformation and Standard Error Calculation 

calc_log_se <- function(df, ee = ee, upper_bound = upper_bound, lower_bound = lower_bound, n = sample_size ){ 
  df |> 
  mutate(
    log_ee = log10(ee),
    se = ( (log10(upper_bound) - log10(lower_bound)) / (2 * qt(0.975, df = n - 1)))
  )
}