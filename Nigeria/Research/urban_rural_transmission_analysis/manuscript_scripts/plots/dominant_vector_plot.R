file.reader <- function(filename){
  read.csv(file.path(DataDir, filename),
           header = T, sep = ',')
}

dominant_vector_0m <- file.reader("dominant_vector_0m_DHS_10_15_18.csv")
dominant_vector_1000m <- file.reader("dominant_vector_1000m_DHS_10_15_18.csv")
dominant_vector_2000m <- file.reader("dominant_vector_2000m_DHS_10_15_18.csv")
dominant_vector_3000m <- file.reader("dominant_vector_3000m_DHS_10_15_18.csv")

sum(is.na(dominant_vector_0m$X2010_Dominant_Vector_Species_Global_5k_NGA))
sum(is.na(dominant_vector_1000m$X2010_Dominant_Vector_Species_Global_5k_NGA))
sum(is.na(dominant_vector_2000m$X2010_Dominant_Vector_Species_Global_5k_NGA))
sum(is.na(dominant_vector_3000m$X2010_Dominant_Vector_Species_Global_5k_NGA))

#Seperated plots

density_fun<- function(variable, label){ggplot(dominant_vector_0m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}


elev_0m_plot <- density_fun(dominant_vector_0m$X2010_Dominant_Vector_Species_Global_5k_NGA, "dominant_vector 0m buffer")
elev_0m_plot


density_fun<- function(variable, label){ggplot(dominant_vector_1000m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}

elev_1000m_plot <- density_fun(dominant_vector_1000m$X2010_Dominant_Vector_Species_Global_5k_NGA, "dominant_vector 1000m buffer")
elev_1000m_plot

density_fun<- function(variable, label){ggplot(dominant_vector_2000m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}
elev_2000m_plot <- density_fun(dominant_vector_2000m$X2010_Dominant_Vector_Species_Global_5k_NGA, "dominant_vector 2000m buffer")
elev_2000m_plot

density_fun<- function(variable, label){ggplot(dominant_vector_3000m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}
elev_3000m_plot <- density_fun(dominant_vector_3000m$X2010_Dominant_Vector_Species_Global_5k_NGA, "dominant_vector 3000m buffer")
elev_3000m_plot



plot_list <- list(elev_0m_plot, elev_1000m_plot, elev_2000m_plot, elev_3000m_plot) 

variables <- ggarrange(plotlist=plot_list, nrow =1, ncol=4)
ggsave(paste0(ResultDir, '/', Sys.Date(),  'dominant_vector_hist_urban.pdf'), variables, width=13, height=7.5)
variables

