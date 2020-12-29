if(FALSE){
  source("../src/libraries.R")
  source("./setup.R")
  
  
  source("../pomp-model/0make.R", chdir = TRUE)


  source("../pomp-model/sim_sirs_simulation_study_2.R", chdir = TRUE)  
}  



susc_max_1 <- plt_data %>% 
  summarise(y_lim_up = max(Susceptible, na.rm = TRUE)) %>% 
  unlist()

susc_max_2 <- plt_data_2 %>% 
  summarise(y_lim_up = max(Susceptible, na.rm = TRUE)) %>% 
  unlist()

susc_max_2 > susc_max_1


# make comparison to see which Reff axis is greater

Reff_max_1 <- plt_data %>% 
  summarise(y_lim_up = max(Reff, na.rm = TRUE)) %>% 
  unlist()

Reff_max_2 <- plt_data_2 %>% 
  summarise(y_lim_up = max(Reff, na.rm = TRUE)) %>% 
  unlist()

Reff_max_2 > Reff_max_1


# ratio of the two infectious times series 

Inf_max_1 <- plt_data %>% 
  summarise(Inf_max = max(Infectious, na.rm = TRUE)) %>% unlist()

Inf_max_2 <- plt_data_2 %>% 
  summarise(Inf_max = max(Infectious, na.rm = TRUE)) %>% unlist()

Inf_r <- Inf_max_1/Inf_max_2

# NOTE :: plt_data_2 will be used to define axis limits and ratios of the secondary axis 
# for panels A and C 


# Extend the upper limit of the plot
text_data <- plt_data_2 %>% 
  summarise(y_lim_up = 1.15*max(Susceptible, na.rm = TRUE)) 

# Define breaks for the axes
break_matrx <- plt_data_2 %>%
  select(Infectious, Susceptible) %>% 
  gather(key = "Comp", value = "Count") %>% 
  group_by(Comp) %>% 
  summarise(b0 = max(Count, na.rm = TRUE), b1 = max(Count, na.rm = TRUE)*(3/4), 
            b2 = max(Count, na.rm = TRUE)*(1/2), b3 = max(Count, na.rm = TRUE)*(1/4), 
            b4 = 0) %>% 
  select(-Comp) %>% 
  as.matrix()

break_matrx_l <- plt_data %>%
  select(Infectious, Susceptible) %>% 
  gather(key = "Comp", value = "Count") %>% 
  group_by(Comp) %>% 
  summarise(b0 = max(Count, na.rm = TRUE), b1 = max(Count, na.rm = TRUE)*(3/4), 
            b2 = max(Count, na.rm = TRUE)*(1/2), b3 = max(Count, na.rm = TRUE)*(1/4), 
            b4 = 0) %>% 
  select(-Comp) %>% 
  as.matrix()


# NOTE :: plt_data will be used to define axis limits and breaks for panels B and D
text_data_2 <- plt_data %>% 
  summarise(y_lim_up = 1.05*max(Reff, na.rm = TRUE))

break_matrx_2 <- plt_data %>%
  summarise(b0 = max(Reff, na.rm = TRUE), b1 = max(Reff, na.rm = TRUE)*(3/4), 
            b2 = max(Reff, na.rm = TRUE)*(1/2), b3 = max(Reff, na.rm = TRUE)*(1/4), 
            b4 = 0) %>% 
  as.matrix()


# annotation data for the rectangle - right panel 
anno_text_r_data <- data.frame(x = c(2018.50, 2019.30, 2020.18),   
                               y = 0.645, 
                               label = c("bold(R[0]^B==1.56)", 
                                         "bold(R[0]^B==0.50)", 
                                         "bold(R[0]^B==1.56)")) 


anno_text_l_data <- data.frame(x = c(2019.32, 2020.12),   
                               y = 0.645, 
                               label = c("bold(R[0]^{B(low)}==1.56)", 
                                         "bold(R[0]^{B(high)}==2.41)")) 



## shared layout for tall panels (top row)
.gg.short <- theme(
    legend.position = "none", 
    #legend.background = element_blank(),
    #axis.title = element_text(size = 13),
)
 

## and short panels (bottom row)
.gg.tall <- theme(
    axis.text.x = element_blank(),
    axis.title.y.right = element_text(hjust=1),
    legend.position = 'top',
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.4, "cm"), 
    legend.box = "horizontal",
    legend.key.width = unit(0.8, "line"),
    #axis.title = element_text(size = 13), 
    legend.text.align = 0
) 

.gg.guide <- guides(
    color = guide_legend(order = 1, nrow = 2),
    linetype = guide_legend(order = 2, ncol = 1)
)

# plt_1: Plot the two compartments - 
plt_1 <- plt_data %>% 
  ggplot(aes(x = time)) +
  annotate("rect", xmin = 2019.750, xmax = Inf, 
           ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.2) +
  geom_line(aes(y = Susceptible, color = which, linetype = "dotted"), size = 0.8) +
  geom_line(aes(y = Infectious*10.7, color = which, linetype = "solid"), size = 0.8) +
  labs(x = "", 
       y = "Susceptible") +
  scale_color_manual(name = "", values = c("red", "#00C9E4", "#3B74FF", "blue"), 
                     label = c(parse(text = "Type~A"), parse(text = "Type~B~(R[0]^low)"), 
                               parse(text = "Type~B~(R[0]^high)"), 
                               expression(Type~B~(R[0]^{low %->% high})))) +
  scale_linetype_manual(name = "", labels = c("Susceptible", "Infectious"), 
                        values = c("dotted", "solid")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = break_matrx[2,], 
                     limits = c(0, text_data$y_lim_up),
                     labels = scales::percent_format(accuracy = 1),
                     sec.axis = sec_axis(~./10.7, name = "", 
                                         labels = scales::percent_format(accuracy = 0.1), 
                                         breaks = c(break_matrx_l[1,]))) +
  geom_text(data = text_data, aes(x = 2017.5, y = y_lim_up,
                                  label = ""), inherit.aes = FALSE) +
  geom_text(data = anno_text_l_data, aes(x = x, y = y, label = label), 
            parse = TRUE, size = 3.2) +
  gg.theme + .gg.tall + .gg.guide 
  # theme(legend.position = c(0.35, 0.28)) 

# plt2: R effective

plt_2 <- plt_data %>% 
  ggplot(aes(x = time)) +
  annotate("rect", xmin = 2019.750, xmax = Inf, 
           ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.2) +
  geom_line(aes(y = Reff, colour = which), size = 0.8) +
  scale_color_manual(name = "", values = c("red", "#00C9E4", "#3B74FF", "blue"), 
                     label = c(parse(text = "Type~A"), parse(text = "Type~B~(R[0]^low)"), 
                               parse(text = "Type~B~(R[0]^high)"), 
                               expression(Type~B~(R[0]^{low %->% high})))) +
  labs(x = "Time (weeks)", y = expression(R[eff])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = c(0.2, 0.6, 1.0), 
                     limits = c(0.2, text_data_2$y_lim_up)) +
  geom_text(data = text_data_2, aes(x = 2017.5, y = y_lim_up,
                                    label = ""), inherit.aes = FALSE) +
  ## add theme at end
  gg.theme + .gg.short

plt_3 <- plt_data_2 %>% 
  filter(time < 2020.502) %>% 
  ggplot(aes(x = time)) +
  #annotate("rect", xmin = 2019.750, xmax = Inf, 
  #         ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.2) +
  annotate("rect", xmin=2018.769, xmax=2019.750, 
           ymin = -Inf , 
           ymax = Inf, alpha=0.2, fill="grey20") +
  geom_line(aes(y = Susceptible, colour = FluType, linetype = "dotted"), size = 0.8) +
  geom_line(aes(y = Infectious*11.085, colour = FluType, linetype = "solid"),  size = 0.8) +
  labs(x = "", 
       y = "") +
  scale_color_manual(name = "", values = c("red", "#8E2DE2"), 
                     labels = c("Type A", "Type B")) +
  scale_linetype_manual(name = "", labels = c("Susceptible", "Infectious"), 
                        values = c("dotted", "solid")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = break_matrx[2,], 
                     limits = c(0, text_data$y_lim_up),
                     labels = scales::percent_format(accuracy = 1),
                     sec.axis = sec_axis(~./11.085, name = "  Infectious", 
                                         labels = scales::percent_format(accuracy = 0.1),
                                         breaks = c(0.0, 0.007, 0.014, 0.021))) +
  geom_text(data = text_data, aes(x = 2017.5, y = y_lim_up,
                                  label = ""),  inherit.aes = FALSE) +
  geom_text(data = anno_text_r_data, aes(x = x, y = y, label = label), 
            parse = TRUE, size = 3.2) +
  gg.theme + .gg.tall + .gg.guide 
  # theme(legend.position = c(0.25, 0.28))

plt_4 <- plt_data_2 %>% 
  filter(time < 2020.502) %>% 
  ggplot(aes(x = time)) +
  #annotate("rect", xmin = 2019.750, xmax = Inf, 
  #         ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.2) +
  annotate("rect", xmin=2018.769, xmax=2019.750, 
           ymin = -Inf , 
           ymax = Inf, alpha=0.2, fill="grey20") +
  geom_line(aes(y = Reff, colour = FluType), size = 0.8) +
  scale_color_manual(name = "", values = c("red", "#8E2DE2"), 
                     label = c("Type A", "Type B")) +
  labs(x = "Time (weeks)", y = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = c(0.2, 0.6, 1.0), 
                     limits = c(0.2, text_data_2$y_lim_up)) +
  ## what does this do? add no text? - Produces extra white space above the R0 labels
  geom_text(data = text_data_2, aes(x = 2017.5, y = y_lim_up,
                                    label = ""), inherit.aes = FALSE) +
  ## add theme at end
  gg.theme + .gg.short
