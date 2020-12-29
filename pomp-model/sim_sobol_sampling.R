
# 
# 
# sobol_wrapper <-function(nseq=10,
#                          R0_A=c(1.2,7),
#                          gamma_A=c(365/10.,365./2.),
#                          w_A=c(1./5., 1./5),
#                          rho=c(0.005, 0.002),
#                          psi=c(0.00001,0.001),
#                          sigmaSE=c(0.0001, 0.001),
#                          amplitude=c(0.01,0.8),
#                          tpeak=c(0.01,0.3), 
#                          pop=c(6.7e6,6.7e6)){
#   sobolDesign(lower=c(R0_A=R0_A[1], gamma_A=gamma_A[1], w_A=w_A[1], rho=rho[1],
#                       sigmaSE=sigmaSE[1], psi=psi[1], amplitude=amplitude[1], 
#                       tpeak=tpeak[1], pop=pop[1]),
#               upper=c(R0_A=R0_A[2], gamma_A=gamma_A[2], w_A=w_A[2], rho=rho[2],
#                       sigmaSE=sigmaSE[2], psi=psi[2], amplitude=amplitude[2], 
#                       tpeak=tpeak[2], pop=pop[2]),
#               nseq=nseq) -> sb
#        
#   a <- as.data.frame(t(apply(sb, FUN=SIRS_endemic_equilibrium, MARGIN=1)))
#   sb <- cbind(a,sb)
#   
#   return(sb)     
#   }                          
#   

# 
# sobol_wrapper2 <-function(nseq=10,
#                          R0_A=c(1.2,7),
#                          gamma_A=c(365/10.,365./2.),
#                          w_A=c(1./5., 1./5),
#                          eta_A=c(365./1., 365./1.),
#                          R0_B=c(1.2,7),
#                          gamma_B=c(365/10.,365./2.),
#                          w_B=c(1./5., 1./5),
#                          eta_B=c(365./1., 365./1.),
#                          c_AB=c(0,1),
#                          c_BA=c(0,1),
#                          rho=c(0.0002, 0.001),
#                          psi=c(0.00001,0.001),
#                          sigmaSE=c(0.0001, 0.001),
#                          amplitude=c(0.01,0.1),
#                          tpeak=c(0.01,0.2), 
#                          pop=c(6.7e6,6.7e6))
# {
#   sobolDesign(lower=c(R0_A=R0_A[1], gamma_A=gamma_A[1], w_A=w_A[1], eta_A=eta_A[1],
#                       R0_B=R0_B[1], gamma_B=gamma_B[1], w_B=w_B[1], eta_B=eta_B[1],
#                       c_AB=c_AB[1], c_BA=c_BA[1], rho=rho[1],
#                       sigmaSE=sigmaSE[1], psi=psi[1], amplitude=amplitude[1], 
#                       tpeak=tpeak[1], pop=pop[1]),
#               upper=c(R0_A=R0_A[2], gamma_A=gamma_A[2], w_A=w_A[2], eta_A=eta_A[2],
#                       R0_B=R0_B[2], gamma_B=gamma_B[2], w_B=w_B[2], eta_B=eta_B[2],
#                       c_AB=c_AB[2], c_BA=c_BA[2], rho=rho[1],
#                       sigmaSE=sigmaSE[2], psi=psi[2], amplitude=amplitude[2], 
#                       tpeak=tpeak[2], pop=pop[2]),
#               nseq=nseq) -> sb
#   
#   a <- as.data.frame(t(apply(sb, FUN=SIRS2_independent_endemic_equilibrium,
#                              MARGIN=1)))
#   sb <- cbind(a,sb)
#   
#   return(sb)     
# }                          



set_up_rw <- function(fix_pars=c("pop","gamma_A", "w_A", "eta_A",
                                 "gamma_B", "w_B", "eta_B"),
                      profile_par_name=NULL,
                      par_names=model_params, sd=0.01){
  new2 <- list()
  new2[par_names] <- sd
  new2[fix_pars] <- 0
  new2[profile_par_name] <- 0
  
  return(do.call(rw.sd,new2))
}


# todo: Move default parameters to sim_data_and_params
sobol_wrapper3 <-function(nseq=10,
                          profile_value=NULL,
                          equil_function=SIRS2_independent_endemic_equilibrium)
{
  
  profile_range <- lapply(profile_value, function(x) c(x,x))
    
  default_sobol_ranges %>%
    list_modify(!!!profile_range) -> par_ranges
  
  lower_bound <- lapply(par_ranges, `[[`, 1)
  upper_bound <- lapply(par_ranges, `[[`, 2)
  
  sobolDesign(lower=unlist(lower_bound),
              upper=unlist(upper_bound),
              nseq=nseq) -> sb
  
  a <- as.data.frame(t(apply(sb, FUN=equil_function,
                             MARGIN=1)))
  sb <- cbind(a,sb)
  
  return(sb)     
}        

# TODO: fix hardcoding of parameters
get_sobol_guesses <- function(nseq, profile_par=NULL, run_indices=NULL){
  
  if (!is.null(profile_par)){
    if (!is.null(run_indices)){
      i <- rownames(profile_par)[run_indices]
    }
    else{
      i <- 1:dim(profile_par)[1]
    }
    
    temp_func <- function(x){
      sobol_wrapper3(nseq = nseq,
                     profile_value = setNames(list(profile_par[x,]),
                                              colnames(profile_par)))
    }
    guesses <- do.call("rbind",lapply(i, 
                                      FUN=temp_func))
  }
  else{
    guesses <- sobol_wrapper3(nseq = nseq)
  }
  return(guesses)
}
