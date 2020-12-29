source('libraries.R')

## convenience function (like unix command of similar name)
## run expression, print time, return result
ps <- function(x) print(system.time(x))

## set names of vector to match contents
## useful for lapply
make.names <- function(x) { names(x) <- x; x}

## compute quantiles
## return as list (with mean)
make.stats <- function(x, probs=c(0.25, 0.5, 0.75)) {
    y <- na.omit(x)
    quants=quantile(y, probs)
    ret <- as.list(quants)
    names(ret) <- paste0('p', probs)
    ret <- within(ret, {
        mean <- mean(y)
        sd <- sd(y)
    })
    ret
}

## Shift epidemic week to integer week of season
## Season starts at week 
mk.shift <- function(x, start=40) if_else(x<start,
    ## new calendar year: move to end
    x+(53-start),
    ## old calendar year: move to front
    x-start
)

# convert  the list of profile likelihood results into a dataframe
list_to_tibble <- function(res_list) {
  map_df(1:length(res_list), function(x) {
    res_list[[x]]$param_hat %>%
      t() %>%
      as_tibble() %>% 
      bind_cols(nll = res_list[[x]]$value) %>% 
      bind_cols(convergence = res_list[[x]]$convergence) %>% 
      bind_cols(n_iter = res_list[[x]]$n_iter)
  }) -> res_df
  
  return(res_df)
}


# calculate AIC
calculate_aic <- function(loglik, npar) {
  return(2*npar-2*loglik)
}

# Lrt test and p-value
lrt_test <-  function(full_mod_obj, nested_mod_obj) {
  # browser()
  chisq_stat <- -2*(nested_mod_obj$logLik-full_mod_obj$logLik)
  
  test_df <- (full_mod_obj$npar - nested_mod_obj$npar)
  
  p_value <- pchisq(chisq_stat, df = test_df, lower.tail = FALSE)
  
  return(list(chisq_test_stat = chisq_stat,
              df = test_df,
              p_value = p_value))
  
}



# functions for parameteric boostrap

# Replicate a vector of parameters into pxn dimension matrix 
param_replicate_matrix <- function(param_v, n = 1000) {
  t(t(param_v)) %*% rep(1, n)
}   

# Simulate from the observation process
sim_obs_model <- function(po_obj, params, times, 
                          nsim, 
                          long_form = FALSE, ...) {
  # if (nsim < 2) {
  #   stop("nsim is needed to be atleast 2, its a format thing")
  # }
  
  #browser()
  # Calculate the solution to the system of diff. eq. 
  po_obj %>% 
    trajectory(params = params, ...) -> soln 
  
  # Simulate using the observation process
  po_obj %>%   
    rmeasure(x = soln, times = times, 
             params = param_replicate_matrix(params, n = nsim)) -> obs_process_array 
  
  
  
  # Dress the result:
  # Self note: this step coerces the array into a matrix
  do.call(rbind, lapply(1:nsim, function(x) {t(obs_process_array[,x,])})) %>% 
    as.data.frame() %>% 
    transmute(total_a = total_a, 
              total_b = total_b, 
              time = rep(times, times = nsim), 
              `.id` = rep(1:nsim, each = length(times))) %>% 
    select(time, `.id`, total_a, total_b) %>% 
    mutate_at(.vars = vars(starts_with("total_")), 
              .funs = function(x) {ifelse(.$time < 2.751540, NA, x)}) -> result_df
  
  # If the long form is required for plotting purposes
  if(long_form == TRUE) {
    
    result_df %>% 
      gather(key = "FluType", value = "SimObs", -c(time, `.id`)) %>% 
      return()
    
  } else { 
    return(result_df)
  }
}

give_quantile_list <- function(quantiles = c(0.025, 0.975), denominator = 100) {
  # browser()
  purrr::map(quantiles, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = map_chr(quantiles, ~paste0(.x*denominator, "%")))
}
