df <- left_join(NGAfiles[[20]],key_list[[7]]) %>%  filter(v025 == "1")

table(df$v025)
var_label(df$v001)
length(unique(df$v001))

