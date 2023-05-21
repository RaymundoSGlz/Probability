library(tidyverse)
head(esoph)
esoph
agg_df <- aggregate(ncases ~ tobgp, esoph, sum)
all_cases <- sum(esoph$ncases)
agg_df$pct <- with(agg_df, ave(ncases, tobgp, FUN=sum) / sum(ncases))


agg1_df <- aggregate(ncontrols ~ tobgp, esoph, sum)
all_controls <- sum(esoph$ncontrols)
agg1_df$pct <- with(agg1_df, ave(ncontrols, tobgp, FUN=sum) / sum(ncontrols))

porc_hig <- aggregate(ncases ~ alcgp, esoph, sum)
porc_hig$pct <- with(porc_hig, ave(ncases, alcgp, FUN = sum)/sum(ncases))

porc_hig1 <- aggregate(ncontrols ~ alcgp, esoph, sum)
porc_hig1$pct <- with(porc_hig1, ave(ncontrols, alcgp, FUN = sum)/sum(ncontrols))

porc_f <- aggregate(ncases ~ tobgp, esoph, sum)
porc_f$pct <- with(porc_f, ave(ncases, tobgp, FUN = sum)/sum(ncases))

porc_f1 <- aggregate(ncontrols ~ tobgp, esoph, sum)
porc_f1$pct <- with(porc_f1, ave(ncontrols, tobgp, FUN = sum)/sum(ncontrols))

f<- esoph %>% filter(alcgp=="120+") %>% 
  summarize(ncontrols=sum(ncontrols)) %>% pull(ncontrols)
alcont<- round(f/all_controls, 4)
alcont

g<- esoph %>% filter(tobgp=="30+") %>% 
  summarize(ncontrols= sum(ncontrols)) %>% pull(ncontrols)
tobcont<-round(g/all_controls, 4)
tobcont

h<- esoph %>% filter(alcgp=="120+" & tobgp=="30+") %>% 
  summarize(ncontrols = sum(ncontrols)) %>% pull(ncontrols)
bothcont<-round(h/all_controls, 5)
bothcont

x<- esoph %>% filter(alcgp=="120+") %>% 
  summarize(ncases=sum(ncases)) %>% pull(ncases)
alcp<- x/all_cases
alcp
