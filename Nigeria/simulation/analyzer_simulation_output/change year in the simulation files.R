filepath <- "C:/Users/ido0493/Documents/shiny_app/shiny_nigeria"


cm <- read.csv(file.path(filepath, "HS_placeholder.csv"))

year <-c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030)
scenario <- ("Scenario 1")

changeyear_rbind<- function(df){
 data<- list()
   for (i in 1:11) {
     df$year <- year[i]
     df$scenario <- scenario[i]
     data[[i]]<- df
   }
do.call(rbind, data)
}

fin_df <- changeyear_rbind(cm)

write.csv(fin_df, file.path(filepath, "HS_placeholder_2020_2030.csv"))
