chomage2020 <-read.csv("FD_csv_EEC20.csv",sep = ";")
install.packages("esquisse")

library(esquisse)
##Mettre en français 
set_i18n("fr")
esquisser(chomage2020)
