Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src", "DHS")
VarDir <- file.path(SrcDir, "1_variables_scripts")


source(file.path(VarDir, "generic_functions", "DHS_fun.R"))

SMC_doses <- read_csv(file.path(DataDir, "SMC_2018_doses_distri_LGA.csv")) #%>% filter(State == "Borno")
head(SMC_doses)

SMC_doses[is.na(SMC_doses)] <- 0

SMC_target <-SMC_doses %>%  pivot_longer(cols = starts_with("U5_target"), names_to = "cycle", values_to = "values") %>%
                  mutate(cycle = ifelse(cycle == "U5_target_cy_one", 1, ifelse(cycle ==  "U5_target_cy_two", 2,
                                                      ifelse(cycle== "U5_target_cy_three",3,
                                                             ifelse(cycle == "U5_target_cy_four", 4, NA)))),
                         values_new = values/1000) %>%
                    dplyr::select(LGA = LGAs, cycle, values_new)
head(SMC_target)
# 
plot <- ggplot(SMC_target, aes(x = cycle, y = values_new)) +
          geom_line(aes(color = LGA)) +
       ylab("LGA SMC U5 Target Population") +
        xlab("SMC round") +
         theme(legend.position = "none")
table(SMC_target$LGA)
# 
# png("results/SMC_coverage/U5_target.png", units="px", width=800, height=800, res=150)
# plot(plot, col = adjustcolor(alpha = 0.5))
# dev.off()


# SMC coverage for all other LGAs 

SMC_r_prop <- SMC_doses %>% filter(State !="Borno") %>%  mutate(U5_target_max = pmax(U5_target_cy_one, U5_target_cy_two, U5_target_cy_three,
                                                        U5_target_cy_four)) %>%
                              mutate_at(vars(num_reach_cy_one, num_reach_cy_two, num_reach_cy_three,
                                             num_reach_cy_four), '/', quote(U5_target_max)) %>%
                dplyr::select(LGAs,prop_cy_one = num_reach_cy_one, prop_cy_two = num_reach_cy_two,
                              prop_cy_three = num_reach_cy_three, prop_cy_four = num_reach_cy_four)
head(SMC_r_prop)

# next we compute for Borno adjusting for overestimate and underestimate of target
# 
# SMC_r_prop_2 <- SMC_doses %>% #filter(State =="Borno") %>%
#   mutate(U5_target_cy_one = ifelse(LGAs == "Bama",U5_target_cy_two,
#                                    ifelse(LGAs == "Dikwa", U5_target_cy_two,
#                                           ifelse(LGAs == "Mobbar", U5_target_cy_two, U5_target_cy_one)))) #%>%
# mutate(prop_cy_one = num_reach_cy_one/U5_target_cy_one,
#        prop_cy_two = num_reach_cy_two/U5_target_cy_two,
#                 prop_cy_three = num_reach_cy_three/U5_target_cy_three,
#        prop_cy_four = num_reach_cy_four/U5_target_cy_four) %>%
# dplyr::select(LGAs, prop_cy_one,prop_cy_two,prop_cy_three, prop_cy_four)


# # no Borno adjustment
# SMC_r_prop_3 <- SMC_doses %>% 
#   mutate(prop_cy_one = num_reach_cy_one/U5_target_cy_one, 
#          prop_cy_two = num_reach_cy_two/U5_target_cy_two,
#          prop_cy_three = num_reach_cy_three/U5_target_cy_three, 
#          prop_cy_four = num_reach_cy_four/U5_target_cy_four) %>% 
#   dplyr::select(LGAs, prop_cy_one,prop_cy_two,prop_cy_three, prop_cy_four)
# 
# 
# # we join the computation for Borno and other LGAs 
# 
# SMC_c_NM <- rbind(SMC_r_prop, SMC_r_prop_2)
# 
# SMC_prop_pl <- SMC_r_prop_3 %>% pivot_longer(-LGAs, names_to = "cycle", values_to = "values") %>% 
#              mutate(cycle = ifelse(cycle == "prop_cy_one", 1, ifelse(cycle ==  "prop_cy_two", 2,
#                                                                ifelse(cycle== "prop_cy_three",3,
#                                                                       ifelse(cycle == "prop_cy_four", 4, NA)))))
# head(SMC_prop_pl)
# 
# 
# p <- ggplot(SMC_prop_pl, aes(x=as.character(cycle), y=values, fill = as.character(cycle))) + 
#    geom_violin(trim = FALSE, alpha = 0.5) +
#     scale_fill_brewer(palette = "Accent") +
#    geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
#   theme(legend.position = "none") + 
#   xlab("SMC round") + 
#   ylab("SMC receipt by LGA (NMCP) without Borno adjustment")
# 
# png("results/SMC_coverage/NMCP_doses_distributed_without_Borno_adjustment.png", units="px", width=800, height=800, res=150)
# plot(p, col = adjustcolor(alpha = 0.5))
# dev.off()
# 
# 
# summary(SMC_prop_pl$values)
# 
# SMC_age <- SMC_c_NM %>% mutate_at(vars(three_11_re_cyc_one, twelve_59_cyc_one), '/', quote(U5_target_cy_one)) %>% 
#                    mutate_at(vars(three_11_re_cyc_two, twelve_59_cyc_two), '/', quote(U5_target_cy_two)) %>% 
#                          mutate_at(vars(three_11_re_cyc_three, twelve_59_cyc_three), '/', quote(U5_target_cy_three)) %>% 
#                                 mutate_at(vars(three_11_re_cyc_four, twelve_59_cyc_four), '/', quote(U5_target_cy_four)) %>% 
#           dplyr::select(LGAs, three_11_re_cyc_one,  twelve_59_cyc_one, three_11_re_cyc_two,  twelve_59_cyc_two, 
#                  three_11_re_cyc_three, twelve_59_cyc_three, three_11_re_cyc_four, twelve_59_cyc_four)
# 
# SMC_age_2 <- SMC_age %>% pivot_longer(-LGAs, names_to = "cycle", values_to = "values") %>% 
#   mutate(group = ifelse(grepl('^three', cycle), '3 - 11 months',  ifelse(grepl('^twelv', cycle), '12 - 59 months', NA))) %>% 
#        mutate(cycle = ifelse(grepl('one$', cycle), '1', ifelse(grepl('two$', cycle), '2',
#                                                           ifelse(grepl('three$', cycle), '3',
#                                                                  ifelse(grepl('four$', cycle), '4', NA)))))
# head(SMC_age_2)
# 
# dodge <- position_dodge(width = 0.5)
# p <- ggplot(SMC_age_2, aes(x=cycle, y=values, fill = group, width = 1)) + 
#   geom_violin(trim = FALSE, position = dodge, alpha = 0.5) +
#   scale_fill_brewer(palette = "Accent") +
#   geom_dotplot(binaxis='y', stackdir = "center", dotsize=0.5, position = dodge) +
#   xlab("SMC round") + 
#   ylab("Proportion of U5 that received treatment by LGA (NMCP)") + 
#   labs(fill = "Age in months") +
#   guides(fill = guide_legend(reverse=TRUE))+
#   theme(legend.position="top") #+
#  # theme_bw() +
#    #theme(panel.border = element_blank(), panel.grid.major.x = element_blank(),
#          #panel.grid.major.y = element_line(size=.1, color="black"),
#           #panel.grid.minor = element_blank(), axis.line = element_blank())
# 
# png("results/SMC_coverage/NMCP_doses_distributed_by_age_Borno_adjusted.png", units="px", width=800, height=800, res=150)
# plot(p, col = adjustcolor(alpha = 0.5))
# dev.off()



# we read in the MC data and plot 

# MC <- read_csv(file.path(DataDir,  "MC_SMC_coverage_by_cycle.csv")) %>%
#   dplyr::filter(grepl('EoR cycle 4|EoC coverage surveys', Data_source)) %>%
#   mutate(cycle_2 = str_sub(cycle, 4, 12), cycle_2 = ifelse(Data_source == 'EoR cycle 4', "cycle 4", cycle_2))

# 
# p <- ggplot(MC, aes(x=as.character(cycle_2), y=coverage, fill = as.character(cycle_2))) + 
#   geom_violin(trim = FALSE, alpha = 0.5) +
#   scale_fill_brewer(palette = "Accent") +
#   geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
#   theme(legend.position = "none") + 
#   xlab("SMC round") + 
#   ylab("Proportion receiving SMC by State (MC)")
# 
# png("results/SMC_coverage/MC_all_cycles.png", units="px", width=800, height=800, res=150)
# plot(p, col = adjustcolor(alpha = 0.5))
# dev.off()

MC <- read_csv(file.path(DataDir,  "MC_SMC_coverage_by_cycle.csv")) %>% 
  dplyr::filter(grepl('SPAQ treatments provided to ineligible children aged 5 and above', describe)) %>% 
  dplyr::filter(grepl('EoC', cycle)) %>%  mutate(cycle_2 = str_sub(Data_source, 5, 12))

# p <- ggplot(MC, aes(x = adm, y = coverage)) + 
#   geom_bar(aes(color = cycle_2, fill=cycle_2), stat = "identity", position = position_dodge(0.8), width = 0.7) +
#   scale_color_manual("SMC round", values = c("#00AFBB", "#E7B800", "#FC4E07"))+
#   scale_fill_manual("SMC round", values = c("#00AFBB", "#E7B800", "#FC4E07")) +
#   xlab("State") + 
#   ylab("Proportion >=5 receving SMC by State (MC)")
# 
# png("results/SMC_coverage/MC_SMC_five_over.png", units="px", width=800, height=800, res=150)
# plot(p, col = adjustcolor(alpha = 0.5))
# dev.off()
 
# adjustment for cycle 1 - 3

dose_in <- SMC_r_prop_2 %>% dplyr::select(State, LGAs, U5_target_cy_one, U5_target_cy_two, U5_target_cy_three, U5_target_cy_four) %>% 
                        pivot_longer(cols = starts_with("U5_target"),
                                     names_to = c("target"), values_to = "values") %>% 
  mutate(cycle = ifelse(target == "U5_target_cy_one",
                        "cycle 1", ifelse(target == "U5_target_cy_two",
                                          "cycle 2", ifelse(target == "U5_target_cy_three",
                                                            "cycle 3", 
                                                            ifelse(target == "U5_target_cy_four", "cycle 4", NA)))))
  
reach <- SMC_r_prop_2 %>% dplyr::select(State, LGAs, num_reach_cy_one, num_reach_cy_two, num_reach_cy_three, 
                                        num_reach_cy_four) %>% 
  pivot_longer(cols = starts_with("num_reac"),
               names_to = c("num_reach"), values_to = "values_num") %>% 
  mutate(cycle = ifelse(num_reach == "num_reach_cy_one",
                        "cycle 1", ifelse(num_reach == "num_reach_cy_two",
                                          "cycle 2", ifelse(num_reach == "num_reach_cy_three",
                                                            "cycle 3",
                                                            ifelse(num_reach == "num_reach_cy_four",
                                                                   "cycle 4", NA)))))

df <- left_join(dose_in, reach, by=c("LGAs", "cycle")) %>% 
     mutate(State.x = trimws(State.x))  #%>%  filter(State.x != "Borno")

colnames(df)[1] <- "State"


# Make MC smaller 

MC_2 <- MC %>%  dplyr::select(State = adm, Data_source, coverage) %>% 
  mutate(cycle = trimws(str_sub(Data_source, 5, 12)), State = trimws(State)) 


# joining both datasets 

df_2 <- left_join(df, MC_2, by = c("State", "cycle")) %>%  mutate(new_values_num= values_num * (1 - coverage),
                                                                  new_values_num = ifelse(is.na(new_values_num), 
                                                                                          values_num, new_values_num))#,
                                                                 # SMC_adm_cov = new_values_num/values) #%>% 
  #mutate(SMC_adm_cov = ifelse(State == "Borno", values_num/values, SMC_adm_cov))

# doses distributed for cycle 4

colnames(df_2)[2] <- "LGA"

summary(df_2$SMC_adm_cov)

# we read in the nigeria LGA pop data and use as denominator 

LGA_pop <- read.csv("bin/nigeria_LGA_pop.csv") %>% filter(LGA %in%df_2$LGA) # before fixing this, I need to include cycle 4 in this 

# which LGAs are not matching?
df_3 <- left_join(df_2, LGA_pop, by = "LGA") %>% mutate(cov_ng_pop = new_values_num/geopode.pop.0.4)


# plots 

p <- ggplot(df_3, aes(x=as.character(cycle), y=cov_ng_pop, fill = as.character(cycle))) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  scale_fill_brewer(palette = "Accent") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  theme(legend.position = "none") +
  xlab("SMC round") +
  ylab("SMC by LGA (LGA 0-4 population as denominator)")

png("results/SMC_coverage/four_states_LGA_U5_pop_denom.png", units="px", width=800, height=800, res=150)
plot(p, col = adjustcolor(alpha = 0.5))
dev.off()
                                                                