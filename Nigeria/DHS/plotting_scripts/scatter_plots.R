#pfpr comparison by year 
u5_pfpr <- read.csv("bin/projection/U5_PfPR.csv")
u5_pfpr$PfPR.U5

u5_pfpr_df <- u5_pfpr %>% group_by(year, LGA, Run_Number) %>% 
            summarise(mean(PfPR.U5)) %>% filter(year == '2021'| year=='2024')

u5_pfpr_df_v2 <-u5_pfpr_df %>% 
       pivot_wider(names_from = year, values_from = `mean(PfPR.U5)`)

u5_pfpr_df$LGA
library(ggplot2)
library(ggrepel)

u5_pfpr_df_v2$`2024`

g <- ggplot(u5_pfpr_df_v2, aes(x = `2021` , y = `2024`, label = LGA)) + 
  geom_point() +
  geom_smooth(method='lm')
  

g <- ggplot(u5_pfpr_df_v2, aes(x = `2021` , y = `2024`, color = "green", label = LGA)) + 
    geom_point() +
  geom_text(aes(label=ifelse(`mean(PfPR.U5)`>0.55,as.character(LGA),'')),hjust=0.5,vjust=0.5)

ggsave("results/archetype_sim_input/Intervention_files_LGA/scatter2024vs2021.pdf")


g_v2 <- ggplot(u5_pfpr_df, aes(x= year, y = `mean(PfPR.U5)`)) + 
  geom_point(color = "blue", size = 3)

### geom_label_repel
g_v2+ 
  geom_label_repel(aes(label = LGA),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()


### geom_text_repel
# only label players with PTS > 25 or < 18
# align text vertically with nudge_y and allow the labels to 
# move horizontally with direction = "x"
ggplot(u5_pfpr_df, aes(x= year, y = `mean(PfPR.U5)`, label = LGA)) + 
  geom_point(color = dplyr::case_when(u5_pfpr_df$`mean(PfPR.U5)` > 0.5 ~ "#1b9e77", 
                                      TRUE ~ "#7570b3"), 
             size = 3, alpha = 0.8) +
  geom_text_repel(data          = subset(u5_pfpr_df, `mean(PfPR.U5)` > 0.5),
                  nudge_y       = 1.2 - subset(u5_pfpr_df, `mean(PfPR.U5)` > 0.5)$`mean(PfPR.U5)`,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey50",
                  direction     = "x") +
  scale_x_continuous(expand = expand_scale(mult = c(0.2, .2))) +
  scale_y_continuous(expand = expand_scale(mult = c(0.1, .1))) +
  theme_classic(base_size = 16)

#urban vs rural area comparison 
u5_pfpr_ur <- read.csv("results/urbanvsrural/2018_micropfpr_state.csv")
head(u5_pfpr_ur)

nig_LGA_pop <- read.csv("bin/nigeria_LGA_pop.csv") %>% 
                group_by(State) %>% 
                summarise(sum(geopode.pop))
summary(nig_LGA_pop$`sum(geopode.pop)`)

nig_urban_areas <- read.csv("bin/NGA_Urban_Areas_new.csv") %>% dplyr::select(State = adm1, pop) 

nig_urban_areas[is.na(nig_urban_areas)] <- 0

nig_urban_areas <- nig_urban_areas%>% 
                    group_by(State) %>% 
                    summarise(sum(pop))%>% 
                    dplyr::select(State, urban_prop = `sum(pop)`)

u5_pfpr_ur_2  <- u5_pfpr_ur %>% left_join (nig_LGA_pop) %>% 
  dplyr::select(urban =X1.urban.,rural =X2.rural., population = `sum(geopode.pop)`, State) %>% 
  mutate( `population size` = ifelse(population >=1000000 & population <=5000000, "1 000 000 - 5 000 000",
                              ifelse(population > 5000000 & population <= 10000000, "> 5 000 000 - 10 000 000",
                              ifelse(population > 10000000, "> 10 000 000 ", NA))))
head(u5_pfpr_ur_2)

u5_pfpr_ur_2$`population size` <- factor(u5_pfpr_ur_3$`population size`, 
                                          levels = unique(u5_pfpr_ur_3$`population size`[order(u5_pfpr_ur_3$population)]))
levels(u5_pfpr_ur_2$`population size`)

u5_pfpr_ur_3 <- u5_pfpr_ur_2 %>% left_join(nig_urban_areas) %>% 
                mutate(urban_prop_new = urban_prop/population, `% urban population` = ifelse(is.na(urban_prop_new), "no data",
                      ifelse(urban_prop_new < 0.2, "< 0.2", ifelse(urban_prop_new >=0.2 & urban_prop_new <= 0.4,  "0.2 - 0.4",
                      ifelse(urban_prop_new > 0.5, "> 0.5", NA)))))
head(u5_pfpr_ur_3)


u5_pfpr_ur_3$`% urban population` <- factor(u5_pfpr_ur_3$`% urban population`, 
                                          levels = unique(u5_pfpr_ur_3$`% urban population`[order(u5_pfpr_ur_3$urban_prop)]))

levels(u5_pfpr_ur_3$`% urban population`)

g <- ggplot(data = u5_pfpr_ur_3, mapping = aes(x = rural ,y = urban)) + 
  geom_point(mapping = aes(size = `population size`, fill = `% urban population`), color = "black", pch = 21)+
  geom_abline(intercept = 0, slope = 1)+
  geom_hline(yintercept = 0.01)+
  ggtitle("Urban PfPR vs Rural PfPR in Nigerian States")+ 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("results/urbanvsrural/scatterurbanvsrural.pdf")


