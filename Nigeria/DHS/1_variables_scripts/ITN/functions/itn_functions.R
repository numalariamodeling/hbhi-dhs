#recoder itn
recode_itn <- function(data) {
  data %>% mutate(hh_itn = ifelse(hml12 == 9, NA,ifelse(hml12 ==1 | hml12 ==2, 1, 0)))
}

survey.month.fun <- function(data) {
  data%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% #dhs pr 2010
    dplyr::rename(v001 = hv001) %>%
    mutate(YYYY = (floor((hv008 - 1)/12)+1900))%>%
    mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
    mutate(time2 = str_c(MM, YYYY, sep = '-'))
}