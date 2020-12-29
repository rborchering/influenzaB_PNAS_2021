## depends on pomp-model/0load.R

params_design_30 <- expand.grid(chi = seq(1e-10, 1, length = 500), 
                                rel = seq(0.5, 2, length = 500), 
                                R0 = 2, 
                                phi = 40)

# Add the response to the axes grid
params_design_30 %<>% bind_cols(res_df)

# plotting 
#.ylab <- expression(paste("Cross-protection (",chi,")"))
.ylab <- expression(paste("          Cross-protection (",chi,")"))
.xlab <- expression(paste(R[0]^B," / ", R[0]^A))

min_br_diff <- min(params_design_30$p_AB_diff)
max_br_diff <- max(params_design_30$p_AB_diff)

min_br_ratio <- min(params_design_30$mp_AB_ratio)
max_br_ratio <- max(params_design_30$mp_AB_ratio)

av <- mean(c(min(params_design_30$mp_AB_ratio), max(params_design_30$mp_AB_ratio)))

# annotation layer data 

x_seg_data <- tibble(xc = c(1, 1, 0.6, 1.5), 
                     xendc = xc, 
                     yc = rep(0, times = 4), 
                     yendc = rep(1, times = 4))

y_seg_data <- tibble(xc = rep(0.5, times = 4), 
                     xendc = rep(2, times = 4), 
                     yc = c(0.25, 0.25, 0.25, 0.85), 
                     yendc = yc)

point_data <- tibble(xc = c(1, 1, 0.6, 1.5), 
                     yc = c(0.25, 0.85, 0.25, 0.85), 
                     xlc = xc + 0.08, 
                     ylc = yc - 0.08, 
                     labs = c("b", "c", "a", "d"))

margin_dim <- c(3,3,-2,3)


## shared between panels

gg.annotations <- list(
  with(x_seg_data, 
    annotate("segment", x = xc, xend = xendc, y = yc, yend = yendc, linetype = "dotdash")
  ),
  with(y_seg_data, 
      annotate("segment", x = xc, xend = xendc, y = yc, yend = yendc, linetype = "dotdash")
  ),
  with(point_data, 
      annotate("point", x = xc, y = yc, shape = 20, size = 3)
  ),
  with(point_data, 
      annotate("text", x = xlc, y = ylc, size = lab.size.small, label = labs, parse = TRUE)
  )
)


## main text figure
( 
  params_design_30 %>% 
  ggplot(aes(x = rel, y = chi, fill = mp_AB_ratio)) +
  geom_tile() +
  gg.annotations +
  labs(y = .ylab,
       x = .xlab,
       fill = "Peak case \nratio (A/B)") +
  scale_fill_gradientn(colours = c("blue", "white", "red"), 
                       values = rescale(c(min_br_ratio, 1, max_br_ratio)), 
                       limits = c(min_br_ratio, max_br_ratio)) +
  guides(fill = guide_colourbar(barheight = 1,
                                ticks.colour = "black",
                                frame.colour = "black")) +
  gg.scale.noexpand +
  gg.theme +
  theme(
    ## share xaxis title between plots?
    axis.title.y=element_text(hjust=0),
    axis.ticks.y.right = element_blank(),
    legend.justification = 'left',
    legend.margin=margin(-0.5,3,0.5,0.5, unit='lines'),
    legend.position = "bottom",
    legend.key = element_rect(colour = "black", size = 1.1),
    #axis.title.y = element_text(size = 9)
  ) 
) -> p_cr_30


## main text figure: 
## like above, only different
(
  p_cr_30
  + aes(fill=p_AB_diff)
  + scale_fill_gradientn(
    colours = c("red", "white", "blue"), 
    values = rescale(c(min_br_diff, 0, max_br_diff)), 
    ## why these values??
    #breaks = c(-1.9, 2.5, 7.5)
    limits = c(min_br_diff, max_br_diff)
  ) +
    scale_y_continuous(expand=c(0,0))+
  labs(
    y = '  ', #.ylab, 
    x = '',
    #x = .xlab,
    fill = "Peak week \ndifference (A-B)"
  ) +
  ## only update differences
  theme(
    legend.position = "top"
    #axis.title.y = element_text(size = 9)
  ) 
) -> p_wd_30

# specify length of burn in for simulations (in years)
t_start <- 0 
# produce the simulation of compartment for three seasons

# add fake data to simulate three seasons 
data.frame(time = seq(0, 2.75, by = 1/52), 
           total_a = NA, 
           total_b = NA) %>% 
  create_SIRS2_pomp_model(time_start_sim=t_start) -> pomp_sirs

# functions to generate trajectory based on various parmeter values

param_values <- data.frame(R0 = 2.0, chi = c(0.25, 0.85, 0.25, 0.85), phi = 365/30,  
                           rel = c(1, 1, 0.6, 1.5))

simulate_tss <- function(params, give_everything = FALSE, show_progress = TRUE,...) {
  # browser()
  if(show_progress == TRUE) {
    pb$tick()$print()  
  } else {
    print("Progress of the job will not be displayed!")
  }
  
  guess1_params <- c(R0_A=unname(params[,"R0"]), gamma_A=365./3.4, w_A=1./4.,
                     R0_B=unname(params[,"rel"])*unname(params[,"R0"]), gamma_B=365./2.5, w_B=1./4.,
                     phi_A=unname(params[,"phi"]), phi_B=unname(params[,"phi"]),
                     chi_BA=unname(params[,"chi"]), chi_AB=unname(params[,"chi"]), 
                     eta_A=365., eta_B=365., rho_A = 0.015, rho_B = 0.007, 
                     sigmaSE=0.0001, psi=0.00001, 
                     amplitude_A=0.3354482, amplitude_B=0.4348537, 
                     tpeak_A=0.1118114, tpeak_B = 0.1374526,
                     pop=6.7e6)
  
  guess1_params <- unlist(guess1_params) 
  
  guess1_ic <- SIRS2_independent_endemic_equilibrium(guess1_params)
  guess1_all <- c(guess1_params,guess1_ic)
  
  # browser()
  
  pomp_sirs %>%
    trajectory(params=guess1_all, t0=-750, format="d", method = "ode45") %>% 
    slice(2:n()) %>% 
    mutate(mp_A = max(K_A), 
           mp_B = max(K_B), 
           mp_AB_ratio = mp_A/mp_B, 
           pw_A = time[which(K_A == max(K_A))], 
           pw_B = time[which(K_B == max(K_B))], 
           p_AB_diff = (pw_A - pw_B)*52, 
           pop = guess1_all["pop"], 
           I = I_A + I_B + I_AB + I_BA, 
           I_A_tot = I_A + I_AB, 
           I_B_tot = I_B + I_BA,   
           C = C_A + C_B, 
           R = R_A + R_B + R_AB, 
           I_A_prop = (I_A + I_AB)/I, 
           I_B_prop = (I_B + I_BA)/I, 
           C_A_prop = C_A/C, 
           C_B_prop = C_B/C) -> everything 
  
  everything %>% 
    slice(n()) %>% 
    select(mp_AB_ratio, p_AB_diff) -> test_sim
  
  if(give_everything == TRUE) {
    print("All the Compartments are produced")
    return(everything)
  } else {
    return(test_sim)  
  }
  
} 



#function to loop over values 
multi_simulate_tss <- function(counter, params_mat, ...) {
  simulate_tss(params = params_mat[counter,], ...)
}


c_facet_data <- map_df(1:4, multi_simulate_tss, params = param_values, 
                       give_everything = TRUE, show_progress = FALSE)


# data for facet specific labels for panel B
label_data <- tibble(
    anno_labs = c("b", "c", "a", "d"),  
    xlc = rep(2019.5, times = 4), 
    ylc = rep(2.75e3, times = 4),
    facet_label = c("b", "c", "a", "d")
)


c_facet_data %>% 
  mutate(
    facet_label = rep(c("b", "c", "a", "d"), each = length(seq(0, 2.75-1/52, by = 1/52))),
    Inc_A_tot = ((K_A)/pop)*1e5, 
    Inc_B_tot = ((K_B)/pop)*1e5
  ) %>% 
  select(Inc_A_tot, Inc_B_tot, time, facet_label) %>% 
  gather(key = "Compartment", value = "Count", -c(time, facet_label)) %>% 
  ggplot(aes(x = time+2017, y = Count, colour = Compartment, fill = Compartment)) +
  geom_area(position = position_dodge(width = 0), alpha = 0.5) +
  labs(x = "Time (weeks)", 
       y = "Cases per 100,000") +
  scale_fill_manual(name = "", 
                    values = c("red", "blue"), 
                    labels = c("Type A", "Type B")) +
  scale_colour_manual(name = "", 
                      values = c("red", "blue"), 
                      labels = c("Type A", "Type B")) +
  scale_x_continuous(expand=c(0.0,0)) +
  scale_y_continuous(expand=c(0.0,0), labels = scales::scientific) +
  facet_wrap(.~ facet_label, scales = "fixed", ncol = 1) +
  #gg.annotate_facet_labels +
  geom_text(data = label_data, 
        aes(x = xlc, y = ylc, label = anno_labs), 
        nudge_x=0.1, nudge_y=-100,
        inherit.aes = FALSE, size = lab.size.small
  ) +
  gg.theme +
  theme(
        legend.position = c(0.30, 0.98),
        legend.background = element_blank(),
        legend.key.height = unit(0.3, "lines"),
        strip.background = element_blank(), 
        strip.text = element_blank(),
        panel.spacing=unit(0.8,'lines')) -> c_grid_plot

## egregious copy-pasta starts here

##############################################################################################################
######################################## Supplementary figure ################################################
##############################################################################################################

params_design_30 <- expand.grid(chi = seq(1e-10, 1, length = 500), 
                                rel = seq(0.5, 2, length = 500), 
                                R0 = 2, 
                                phi = 40)

# Add the response to the axes grid
params_design_30 %<>% bind_cols(res_df_2)

# plotting 
.ylab <- expression(paste("        Cross-protection (",chi,")"))
.xlab <- expression(paste(R[0]^B," / ", R[0]^A))

min_br_diff <- min(params_design_30$p_AB_diff)
max_br_diff <- max(params_design_30$p_AB_diff)

min_br_ratio <- min(params_design_30$mp_AB_ratio)
max_br_ratio <- max(params_design_30$mp_AB_ratio)

av <- mean(c(min(params_design_30$mp_AB_ratio), max(params_design_30$mp_AB_ratio)))

# annotation layer data 

x_seg_data <- tibble(xc = c(1, 1, 0.6, 1.5), 
                     xendc = xc, 
                     yc = rep(0, times = 4), 
                     yendc = rep(1, times = 4))

y_seg_data <- tibble(xc = rep(0.5, times = 4), 
                     xendc = rep(2, times = 4), 
                     yc = c(0.25, 0.25, 0.25, 0.85), 
                     yendc = yc)

point_data <- tibble(xc = c(1, 1, 0.6, 1.5), 
                     yc = c(0.25, 0.85, 0.25, 0.85), 
                     xlc = xc + 0.08, 
                     ylc = yc - 0.08, 
                     labs = c("b", "c", "a", "d"))

margin_dim <- c(3,3,-2,3)


## shared between panels

gg.annotations <- list(
  with(x_seg_data, 
       annotate("segment", x = xc, xend = xendc, y = yc, yend = yendc, linetype = "dotdash")
  ),
  with(y_seg_data, 
       annotate("segment", x = xc, xend = xendc, y = yc, yend = yendc, linetype = "dotdash")
  ),
  with(point_data, 
       annotate("point", x = xc, y = yc, shape = 20, size = 3)
  ),
  with(point_data, 
       annotate("text", x = xlc, y = ylc, size = lab.size.small, label = labs, parse = TRUE)
  )
)


## main text figure
( 
  params_design_30 %>% 
    ggplot(aes(x = rel, y = chi, fill = mp_AB_ratio)) +
    geom_tile() +
    gg.annotations +
    labs(y = .ylab,
         x = .xlab,
         fill = "Peak case \nratio (A/B)") +
    scale_fill_gradientn(colours = c("white", "red"), 
                         values = rescale(c(min(min_br_ratio, 1), max_br_ratio)), 
                         limits = c(min(min_br_ratio, 1), max_br_ratio)) +
    guides(fill = guide_colourbar(barheight = 1,
                                  ticks.colour = "black",
                                  frame.colour = "black")) +
    gg.scale.noexpand +
    gg.theme +
    theme(
      axis.title.y=element_text(hjust=0),
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      legend.justification = 'left',
      legend.margin=margin(0.5,2,0.5,0, unit='lines'),
      legend.position = "bottom",
      legend.key = element_rect(colour = "black", size = 1.1),
      #axis.title.y = element_text(size = 9)
    ) 
) -> p_cr_30_2


## main text figure: 
## like above, only different
(
  p_cr_30_2
  + aes(fill=p_AB_diff)
  + scale_fill_gradientn(colours = c("red", "white", "blue"), 
                         values = rescale(c(min_br_diff, 0, max_br_diff)),
                         limits = c(min_br_diff, max_br_diff)) +
    labs(
      y = '', 
      x = '',
      #x = .xlab,
      fill = "Peak week \ndifference  (A-B)"
    ) +
    theme(
      legend.position = "top",
      legend.key = element_rect(colour = "black", size = 1.1),
      #axis.title.y = element_text(size = 9)
    ) 
) -> p_wd_30_2

# specify length of burn in for simulations (in years)
t_start <- 0 
# produce the simulation of compartment for three seasons

# add fake data to simulate three seasons 
data.frame(time = seq(0, 2.75, by = 1/52), 
           total_a = NA, 
           total_b = NA) %>% 
  create_SIRS2_pomp_model(time_start_sim=t_start) -> pomp_sirs

# functions to generate trajectory based on various parmeter values

param_values <- data.frame(R0 = 2.0, chi = c(0.25, 0.85, 0.25, 0.85), phi = 365/30,  
                           rel = c(1, 1, 0.6, 1.5))

simulate_tss_2 <- function(params, give_everything = FALSE, show_progress = TRUE,...) {
  # browser()
  if(show_progress == TRUE) {
    pb$tick()$print()  
  } else {
    print("Progress of the job will not be displayed!")
  }
  
  guess1_params <- c(R0_A=unname(params[,"R0"]), gamma_A=365./3.4, w_A=1./4.,
                     R0_B=unname(params[,"rel"])*unname(params[,"R0"]), gamma_B=365./2.5, w_B=1./10.,
                     phi_A=unname(params[,"phi"]), phi_B=unname(params[,"phi"]),
                     chi_BA=unname(params[,"chi"]), chi_AB=unname(params[,"chi"]), 
                     eta_A=365., eta_B=365., rho_A = 0.015, rho_B = 0.007, 
                     sigmaSE=0.0001, psi=0.00001, 
                     amplitude_A=0.3354482, amplitude_B=0.4348537, 
                     tpeak_A=0.1118114, tpeak_B = 0.1374526,
                     pop=6.7e6)
  
  guess1_params <- unlist(guess1_params) 
  
  guess1_ic <- SIRS2_independent_endemic_equilibrium(guess1_params)
  guess1_all <- c(guess1_params,guess1_ic)
  
  # browser()
  
  pomp_sirs %>%
    trajectory(params=guess1_all, t0=-750, format="d", method = "ode45") %>% 
    slice(2:n()) %>% 
    mutate(mp_A = max(K_A), 
           mp_B = max(K_B), 
           mp_AB_ratio = mp_A/mp_B, 
           pw_A = time[which(K_A == max(K_A))], 
           pw_B = time[which(K_B == max(K_B))], 
           p_AB_diff = (pw_A - pw_B)*52, 
           pop = guess1_all["pop"], 
           I = I_A + I_B + I_AB + I_BA, 
           I_A_tot = I_A + I_AB, 
           I_B_tot = I_B + I_BA,   
           C = C_A + C_B, 
           R = R_A + R_B + R_AB, 
           I_A_prop = (I_A + I_AB)/I, 
           I_B_prop = (I_B + I_BA)/I, 
           C_A_prop = C_A/C, 
           C_B_prop = C_B/C) -> everything 
  
  everything %>% 
    slice(n()) %>% 
    select(mp_AB_ratio, p_AB_diff) -> test_sim
  
  if(give_everything == TRUE) {
    print("All the Compartments are produced")
    return(everything)
  } else {
    return(test_sim)  
  }
  
} 



#function to loop over values 
multi_simulate_tss_2 <- function(counter, params_mat, ...) {
  simulate_tss_2(params = params_mat[counter,], ...)
}


c_facet_data_2 <- map_df(1:4, multi_simulate_tss_2, params = param_values, 
                       give_everything = TRUE, show_progress = FALSE)


# data for facet specific labels for panel B
label_data <- tibble(anno_labs = c("b", "c", "a", "d"),
                     anno_labs_ph = c("", "", "", ""),
                     xlc = rep(2019.5, times = 4), 
                     ylc_ph = rep(2.0e3, times = 4),
                     ylc = ylc_ph-0.1e3, 
                     facet_label = c("b", "c", "a", "d"))


c_facet_data_2 %>% 
  mutate(facet_label = rep(c("b", "c", "a", "d"), 
                           each = length(seq(0, 2.75-1/52, by = 1/52))),
         Inc_A_tot = ((K_A)/pop)*1e5, 
         Inc_B_tot = ((K_B)/pop)*1e5) %>% 
  select(Inc_A_tot, Inc_B_tot, time, facet_label) %>% 
  gather(key = "Compartment", value = "Count", -c(time, facet_label)) %>% 
  ggplot(aes(x = time+2017, y = Count, colour = Compartment, fill = Compartment)) +
  geom_area(position = position_dodge(width = 0), alpha = 0.5) +
  labs(x = "Time (weeks)", 
       y = "Cases per 100,000") +
  scale_fill_manual(name = "", 
                    values = c("red", "blue"), 
                    labels = c("Type A", "Type B")) +
  scale_colour_manual(name = "", 
                      values = c("red", "blue"), 
                      labels = c("Type A", "Type B")) +
  scale_x_continuous(expand=c(0,0)) + #.01
  scale_y_continuous(expand=c(0,0), labels = scales::scientific) + #.05
  facet_wrap(.~ facet_label, scales = "fixed", ncol = 1) +
  #gg.annotate_facet_labels +
  geom_text(data = label_data, aes(x = xlc, y = ylc_ph, label = anno_labs_ph), 
            inherit.aes = FALSE, size = 3.5) +
  geom_text(data = label_data, aes(x = xlc, y = ylc, label = anno_labs), 
            inherit.aes = FALSE, size = 3.5) +
  gg.theme +
  theme(
    legend.position = c(0.40, 0.995),
    legend.background = element_blank(),
    legend.key.height = unit(0.3, "lines"),
    strip.background = element_blank(), 
    strip.text = element_blank(),
    panel.spacing=unit(1,'lines')) -> c_grid_plot_2














