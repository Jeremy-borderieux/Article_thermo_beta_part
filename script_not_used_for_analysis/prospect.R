library(data.table)
library(broman)
library(stringr)
library(ggplot2)
library(ggspatial)
library(sf)
library(foreach)
library(doParallel)
library(raster)
library(car)
library(scales)
library(showtext)
library(ggpubr)
library(cowplot)
library(lubridate)



RS_explo<-NFI_flora[,.(N=.N,year=unique(campaign)),by=idp]
RS_explo<-NFI_flora_selected_plot[,.(N=.N,year=unique(campaign)),by=idp]

RS_explo[,mean(N),by=year>2012]

ggplot(RS_explo,aes(x=year,y=(N)))+theme_bw()+geom_jitter(size=0.25)+geom_smooth(method="lm")
ggplot(RS_explo,aes(x=as.character(year),y=log(N)))+theme_bw()+geom_boxplot()+geom_smooth(method="lm")


list_sp_contrib[,delta_occ_n:=delta_occ/sum(occurrence_total),by=species_name ]
list_sp_contrib[,occurrence_total_n:=occurrence_total/sum(occurrence_total),by=species_name ]

list_sp_contrib<-merge(list_sp_contrib,ser_variable_summary[,c("ser","mean_tmoy")])


ggplot(list_sp_contrib,aes(x=occurrence_total,y=delta_occ))+theme_bw()+geom_point()+geom_smooth()+scale_x_continuous(trans="log")

ggplot(list_sp_contrib,aes(x=occurrence_past,y=occurrence_recent))+theme_bw()+geom_point()+geom_smooth()+scale_x_continuous(limits = c(0,250))+ylim(c(0,250))
ggplot(list_sp_contrib[greco=="J",],aes(x=occurrence_past,y=occurrence_recent,color=sp_relative_topt))+theme_bw()+geom_point()+geom_smooth()+scale_x_continuous(limits = c(0,100))+ylim(c(0,100))


ggplot(list_sp_contrib,aes(x=occurrence_total_n,y=delta_occ_n))+theme_bw()+geom_point()+geom_smooth()

lm_sp_delta<-lm(occurrence_recent~occurrence_past+occurrence_past:topt_climplant+occurrence_past:mean_tmoy+topt_climplant:occurrence_past:mean_tmoy,data=list_sp_contrib[occurrence_past<300,])

summary(lm_sp_delta)
plot(lm_sp_delta)


plot_model(lm_sp_delta,type="pred",terms=c("occurrence_past","topt_climplant","mean_tmoy"))+theme_bw()




#### Supplementary figures ####

name_8_comp_beta<-colnames(contrib_therm_beta_ser_8)[(ncol(contrib_therm_beta_ser_8)-11):(ncol(contrib_therm_beta_ser_8)-4) ]
name_8_comp_topt<-colnames(contrib_therm_beta_ser_8)[(ncol(contrib_therm_beta_ser_8)-19):(ncol(contrib_therm_beta_ser_8)-12) ]

melt_beta<-melt(contrib_therm_beta_ser_8,id.vars="ser")[variable%in%c(name_8_comp_beta,"sp_delta_beta"),]
melt_topt<-melt(contrib_therm_beta_ser_8,id.vars="ser")[variable%in%c(name_8_comp_topt,"sp_thermo"),]

ggplot(melt_beta,aes(x=variable,y=as.numeric(value),fill=variable))+theme_bw()+theme(axis.text.x = element_text(angle=20,vjust = 1, hjust=1))+
  geom_hline(yintercept = 0,lty=2)+geom_jitter(shape=21,alpha=0.6,color="white")+geom_boxplot(alpha=0.6,outlier.shape = NA)#+scale_discrete_manual(aesthetics = c("colour", "fill"),values=c("lightcyan4","cadetblue","indianred3","tomato","gold3"),breaks=c("contrib_beta_colder_less", "contrib_beta_colder_more", "contrib_beta_warmer_less" ,"contrib_beta_warmer_more","delta_beta"))

ggplot(melt_topt,aes(x=variable,y=as.numeric(value),fill=variable))+theme_bw()+theme(axis.text.x = element_text(angle=20,vjust = 1, hjust=1))+
  geom_hline(yintercept = 0,lty=2)+geom_jitter(shape=21,alpha=0.6,color="white")+geom_boxplot(alpha=0.6,outlier.shape = NA)#+scale_discrete_manual(aesthetics = c("colour", "fill"),values=c("lightcyan4","cadetblue","indianred3","tomato","gold3"),breaks=c("contrib_beta_colder_less", "contrib_beta_colder_more", "contrib_beta_warmer_less" ,"contrib_beta_warmer_more","delta_beta"))





whole_france_year<-foreach(year=2015:2021,.combine = rbind)%do%{
  get_contrib_one_ser(unique(pairs_ser$ser),year=year,table_flora,n_comp = 4)[[1]] # pairs_ser$ser   # [greco=="H",]
 
  
}
whole_france_year$year<-2015:2021



whole_greco_year<-foreach(year=rep(2015:2021,10), greco_1=sort(rep(LETTERS[1:10],7)),.combine = rbind)%do%{
  cat(paste0(year,greco_1," -> "))
  get_contrib_one_ser(unique(pairs_ser[greco==greco_1,]$ser),year=year,table_flora,n_comp = 4)[[1]] # pairs_ser$ser   # [greco=="H",]
  
  
}


whole_greco_year$year<-rep(2015:2021,10)
whole_greco_year$greco<-sort(rep(LETTERS[1:10],7))

whole_greco_year[,mean(sp_thermo*10),by=.(year,greco)]


NFI_pairs_info[,mean(tmoy_v2b_9015_13,na.rm=T),by=campaign]

fig_sppl<-ggplot(whole_france_year,aes(x=year,y=sp_thermo*10,color="Thermophilization"))+theme_bw()+
  geom_line(lwd=1)+
  geom_line(mapping = aes(y=topt_ext*10,color="Extinction contribution"),lwd=1)+
  geom_line(mapping = aes(y=topt_col*10,color="Colonization contribution"),lwd=1)+
  geom_point(size=2)+
  geom_point(mapping = aes(y=topt_ext*10,color="Extinction contribution"),size=2)+
  geom_point(mapping = aes(y=topt_col*10,color="Colonization contribution"),size=2)+
  geom_hline(yintercept = 0,lty=2)+
  scale_x_continuous(breaks=c(2015,2016,2017,2018,2019,2020,2021))+
  scale_color_manual(values=c("peru","salmon4","gold3"))+
  labs(y="Thermophilization °C/10years",x="Year of the recent plots",color="  ")+
  theme(legend.position = c(0.20,0.75))

fig_sppl

ggsave(file.path("Figures_results","Supplementary_thermo_fig.png"),dpi=300,fig_sppl,scale=1,width = 180,height = 120,unit="mm")

fig_sppl_2<-ggplot(whole_greco_year,aes(x=year,y=sp_thermo*10,color=greco))+theme_bw()+
  geom_line(lwd=1)+
 # geom_line(mapping = aes(y=topt_ext*10,color="Extinction contribution"),lwd=1)+
#  geom_line(mapping = aes(y=topt_col*10,color="Colonization contribution"),lwd=1)+
  geom_point(size=2)+
 # geom_point(mapping = aes(y=topt_ext*10,color="Extinction contribution"),size=2)+
 # geom_point(mapping = aes(y=topt_col*10,color="Colonization contribution"),size=2)+
  geom_hline(yintercept = 0,lty=2)+
  scale_x_continuous(breaks=c(2015,2016,2017,2018,2019,2020,2021))+
  #scale_color_manual(values=c("peru","salmon4","gold3"))+
  labs(y="Thermophilization °C/10years",x="Year of the recent plots",color="  ")+
  theme(legend.position = c(0.20,0.75))

fig_sppl_2



order_sp_contrib<-list_sp_contrib[greco=="J",.(sum(raw_contrib),unique(topt_climplant)),by=species_name][order(-V1),][1:20,]
order_sp_contrib<-list_sp_contrib[greco=="J",.(sum(raw_contrib),unique(topt_climplant)),by=species_name][order(-abs(V1)),][1:20,]
order_sp_contrib
0.6^4

order_sp_contrib<-list_sp_contrib[greco=="H",.(sum(raw_contrib),unique(topt_climplant)),by=species_name][order(-abs(V1)),][1:20,]
order_sp_contrib

list_sp_contrib[greco=="J" & species_name == "Brachypodium pinnatum",]
list_sp_contrib[greco=="J" & grepl("Brachypodium",species_name),]

list_sp_contrib[greco=="J" & species_name == "Festuca ovina",]
list_sp_contrib[greco=="J" & grepl("Festuca",species_name),]

list_sp_contrib[greco=="J" & species_name == "Hieracium murorum",]
list_sp_contrib[greco=="J" & grepl("Hieracium",species_name),]

list_sp_contrib[greco=="J" & species_name == "Galium mollugo",]
list_sp_contrib[greco=="J" & grepl("Galium",species_name),]

list_sp_contrib[greco=="J" & species_name == "Lonicera xylosteum",]
list_sp_contrib[greco=="J" & grepl("Lonicera",species_name),]

list_sp_contrib[greco=="J" & species_name == "Juniperus communis",]
list_sp_contrib[greco=="J" & grepl("Juniperus",species_name),]

list_sp_contrib[greco=="J" & species_name == "Ruscus aculeatus",]
list_sp_contrib[greco=="J" & grepl("Ruscus",species_name),]

list_sp_contrib[greco=="J" & species_name == "Rubus ulmifolius",]
list_sp_contrib[greco=="J" & grepl("Rubus",species_name),]

list_sp_contrib[greco=="J",sum(raw_contrib),by=ser]
list_sp_contrib[greco=="J" & ! species_name %in% order_sp_contrib$species_name[1:10],sum(raw_contrib),by=ser]

out_j<-foreach(pouet=unique(pairs_ser[greco=="J"]$ser),.combine = rbind)%do%{
  get_contrib_one_ser(pouet,table_flora[,!colnames(table_flora)%in% order_sp_contrib$species_name[1:1]],n_comp = 4)[[1]] 
  #get_contrib_one_ser(pouet,table_flora,n_comp = 4)[[1]] 
  
  
}

summary(out_j$sp_thermo)
summary(out_j$topt_ext)



hist(NFI_pairs_info$tmoy_v2b_9015_13,breaks=c(-5:20))

hist(NFI_climate$tmoy_v2b_6186_13,breaks=c(-5:20))

NFI_climate_2<-merge(NFI_climate,NFI_plot_info[,c("idp","greco")],by="idp")

NFI_climate_2[,greco_name:=switchv(EXPR=greco, 
                                                A="North West",
                                                B="Center North",
                                                C="Grand East",
                                                D="Vosges",
                                                E="Jura",
                                                `F`="South West",
                                                G="Central massif",
                                                H="Alpes",
                                                I="Pyrenees",
                                                J="Mediterranean","bug")]
NFI_pairs_info[,greco_name:=switchv(EXPR=greco, 
                                   A="North West",
                                   B="Center North",
                                   C="Grand East",
                                   D="Vosges",
                                   E="Jura",
                                   `F`="South West",
                                   G="Central massif",
                                   H="Alpes",
                                   I="Pyrenees",
                                   J="Mediterranean")]



histo_climate_repre<-ggplot(NFI_climate_2[greco_name!="bug",],aes(x=tmoy_v2b_6186_13,fill="French forests"))+
  theme_bw()+
  geom_density(alpha=0.5)+
  geom_density(alpha=0.5,data=NFI_pairs_info,mapping=aes(fill="Paired dataset"))+
  facet_wrap(~greco_name,scales ="free_x")+
  labs(fill="Dataset",x="Mean annual temperature °C","Density distribution")+
  scale_fill_manual(values=c("darkolivegreen3","grey80"))+
  theme(legend.position = c(0.75,0.2))

ggsave(file.path("Figures_results","Climate_repre.png"),plot=histo_climate_repre,dpi=300,scale=1,width = 180,height = 140,unit="mm")


ggplot(whole_france_year,aes(x=year,y=sp_delta_beta))+theme_bw()+
  geom_line(color="olivedrab")+
  geom_line(mapping = aes(y=beta_ext),color="salmon4")+
  geom_line(mapping = aes(y=beta_col),color="peru")+
  geom_hline(yintercept = 0,lty=2)

ggplot(whole_france_year,aes(x=year,y=sp_thermo))+theme_bw()+
  geom_line(color="gold3")+
  geom_line(mapping = aes(y=contrib_topt_colder_col),color="cadetblue")+
  geom_line(mapping = aes(y=contrib_topt_warmer_col),color="tomato")+
  geom_line(mapping = aes(y=contrib_topt_colder_ext),color="lightcyan4")+
  geom_line(mapping = aes(y=contrib_topt_warmer_ext),color="indianred3")+
  geom_hline(yintercept = 0,lty=2)


ggplot(whole_france_year,aes(x=year,y=sp_delta_beta))+theme_bw()+
  geom_line(color="olivedrab")+
  geom_line(mapping = aes(y=contrib_beta_colder_col),color="cadetblue")+
  geom_line(mapping = aes(y=contrib_beta_warmer_col),color="tomato")+
  geom_line(mapping = aes(y=contrib_beta_colder_ext),color="lightcyan4")+
  geom_line(mapping = aes(y=contrib_beta_warmer_ext),color="indianred3")+
  geom_hline(yintercept = 0,lty=2)




#### thermo  ####

thermo_plot_level<-NFI_plot_info[!is.na(period),mean(cit_ecoplant_picq),by=.(ser,period)][order(ser)]
thermo_plot_level<-thermo_plot_level[,.(thermo_plot=diff(V1)),by=ser]
contrib_therm_beta_ser_2<-merge(contrib_therm_beta_ser,thermo_plot_level,by="ser")

ggplot(contrib_therm_beta_ser_2,aes(x=sp_thermo,y=thermo_plot))+theme_bw()+geom_point()+geom_smooth(method = "lm")

mean(contrib_therm_beta_ser_2[ser!="F22",]$sp_thermo)
mean(contrib_therm_beta_ser_2[ser!="F22",]$thermo_plot)
with(contrib_therm_beta_ser_2[ser!="F22",],weighted.mean(sp_thermo,n_plot))
with(contrib_therm_beta_ser_2[ser!="F22",],weighted.mean(thermo_plot,n_plot))

ggplot(melt(contrib_therm_beta_ser_2,measure.vars = c("thermo_plot","sp_thermo")),
       aes(x=mean_tmoy,y=value,color=variable))+theme_bw()+geom_point(size=1)+geom_smooth(method='lm')+
  scale_discrete_manual(aesthetics = c("colour"),values=c("indianred3","lightcyan4"),breaks=c("sp_thermo","thermo_plot"))+
  geom_hline(yintercept = 0,lty=2)+facet_wrap(~variable)





#### stashed survey explo & test ####



# NFI_pairs_habitat<-NFI_habitat[idp%in%c(pairs_ser$idp1,pairs_ser$idp2),]
# NFI_pairs_habitat[,campaign:=floor(idp/100000)+2005]
# NFI_pairs_habitat[,period:=ifelse(campaign%in%c(2005,2006,2007,2008,2009,2010,2011),"past",ifelse(campaign%in%c(2011,2012,2013,2014),NA,"recent"))]
# 
# table_habitat<-NFI_pairs_habitat[,.(compte=.N),by=.(cb_ifn,period)][order(cb_ifn),]
# table_habitat[!is.na(period),N:=(compte/sum(compte))*100,by=period]




dim(table_flora[,colnames(table_flora)%in%sp_indicator_value$lb_nom_final])


id_ref<-rbind(data.table(idp=pairs_ser$idp2,pair_id_ser=pairs_ser$pair_id_ser,period="past",ser=pairs_ser$ser),
              data.table(idp=pairs_ser$idp1,pair_id_ser=pairs_ser$pair_id_ser,period="recent",ser=pairs_ser$ser))

flora_of_selected_plots<-flora_of_selected_plots[order(idp),]
flora_of_selected_plots<-merge(flora_of_selected_plots,id_ref,by="idp",all.x=T)

ban_small_oc<-flora_of_selected_plots[,.N,by=.(period,species_name)]
ban_small_oc<-dcast(ban_small_oc,N + species_name~period ,fill=0,fun.aggregate = max)
ban_small_oc<-ban_small_oc[,.(N=sum(N),past=sum(past),recent=sum(recent)),by=species_name]
ban_small_oc[,has_climplant:=species_name%in%sp_indicator_value[!is.na(YearMeanMean),lb_nom_final]]
View(ban_small_oc[has_climplant==T,])

selected_species<-ban_small_oc[N>=10,species_name]
flora_of_pairs<-flora_of_pairs[species_name%in%ban_small_oc[N>=10,species_name],]

ban_small_oc_ser<-flora_of_pairs[,.N,by=.(species_name,ser)]
flora_of_pairs<-merge(flora_of_pairs,ban_small_oc_ser,by=c("species_name","ser"))
flora_of_pairs<-flora_of_pairs[N>2,]



table_past<-table_flora[rownames(table_flora)%in% as.character(pairs_ser$idp2),]
table_recent<-table_flora[rownames(table_flora)%in% as.character(pairs_ser$idp1),]


table_one_ser_example<-create_comparaison_table(table_past[rownames(table_past)%in% as.character(pairs_ser[ser=="G22",]$idp2),],
                                                table_recent[rownames(table_recent)%in% as.character(pairs_ser[ser=="G22",]$idp1),],ser="G22")

table_one_ser_example<-create_comparaison_table(table_past[rownames(table_past)%in% as.character(pairs_ser[ser=="F21",]$idp2),],
                                                table_recent[rownames(table_recent)%in% as.character(pairs_ser[ser=="F21",]$idp1),],ser="G22")



table(floor(pairs_ser$idp1/100000)+2005,pairs_ser$campaign_dif)

u2<-get_contrib_one_ser("C20",2019,table_flora,n_comp = 8,rarefy = F,random_topt = F)

u2[[1]]

u2<-get_contrib_one_ser("C20",table_flora,n_comp = 8,rarefy = F,random_topt = F,topt_name="microYearMean")
u2<-get_contrib_one_ser("C20",table_flora,n_comp = 4)


multiple_thermo_error<-run_contrib_multiple_time(500,"B32",table_flora,n_comp = 4,rarefy = F,random_topt = "error",average = F)

summary(multiple_thermo_error)

multiple_thermo_error[,c("topt_ext","topt_col","sp_thermo")]

ggplot(multiple_thermo_error)+geom_violin(mapping = aes(x="topt_ext",y=topt_ext))+geom_violin(mapping = aes(x="topt_col",y=topt_col))+geom_violin(mapping = aes(x="sp_thermo",y=sp_thermo))
ggplot(multiple_thermo_error,aes(x=topt_ext,y=topt_col))+geom_point()+geom_smooth(method = "lm")

contrib_therm_beta_ser[ser=="B32",]

whole_france<-get_contrib_one_ser(unique(pairs_ser$ser),year=NULL,table_flora,n_comp = 4)


summary(whole_france[[1]])
View(whole_france[[2]])

test_boot<-run_contrib_multiple_time(n=50,ser_select="F21",table_flora,n_comp=4,topt_name="topt_climplant",
                                     occ_hyp = 1,random_topt = T,rarefy=F)

#### big stash ####
if(n_comp==12)comparison_surv_one_ser_beta<-comparison_surv_one_ser_beta[forest_species!="NC",]
if(n_comp==12)decompose_thermo_beta<-comparison_surv_one_ser_beta[,.(N_past=sum(occurrence_past),N_recent=sum(occurrence_recent)/(sp_tot_recent),contrib_topt=sum(raw_contrib,na.rm=T),contrib_beta=sum(delta_beta)),by=.(sp_relative_occ,sp_relative_topt,forest_species)][order(sp_relative_topt,sp_relative_occ,forest_species)]

if(n_comp==12){
  
  decompose_beta<-bet_test[,.(N_past=sum(N_past),N_recent=sum(N_recent),contrib_topt=sum(n_contrib_topt)),by=.(sp_relative_topt,sp_relative_occ,forest_species )][sp_relative_occ!="stable",]
  
  decompose_beta<-decompose_beta[order(sp_relative_occ,sp_relative_topt,forest_species),]
  # new version: replace less by extirpation
  intermediate_beta<-comparison_surv_one_ser_beta[,.(contrib_extirp=sum(extirpation),contrib_colo=sum(colonisation)),by=.(sp_relative_topt,forest_species)]#[sp_relative_occ!="stable",]
  intermediate_beta<-intermediate_beta[order(sp_relative_topt,forest_species),]
  decompose_beta$contrib_beta<-c(intermediate_beta$contrib_extirp,intermediate_beta$contrib_colo)
  
  results_8<-dcast.data.table(decompose_beta, .~sp_relative_topt+sp_relative_occ+forest_species,value.var = c("contrib_topt", "contrib_beta"))
  
  decompose_beta_n<-bet_test[,.(N_past=sum(N_past),N=sum(N),contrib_topt=sum(n_contrib_topt)),by=.(sp_relative_topt,forest_species )]
  decompose_beta_n<-decompose_beta_n[order(sp_relative_topt,forest_species),]
  count_n<-dcast.data.table(decompose_beta_n, .~sp_relative_topt+forest_species,value.var = c("N"))
  
  results_8$.<-NULL
  count_n$.<-NULL
  
}


#### data  checking/result ####

hist(list_sp_contrib$delta_beta,nc=20)
sp_check<-list_sp_contrib[sp_relative_occ=="col" & occurrence_past==0  ,]
sp_check<-list_sp_contrib[sp_relative_occ=="col" & occurrence_past==0 & sp_relative_topt=="colder" ,]
sp_check<-list_sp_contrib[sp_relative_occ=="col" & occurrence_past==0 & sp_relative_topt=="warmer" ,]

sp_check<-list_sp_contrib[sp_relative_occ=="ext" & occurrence_past==1 ,]



sp_check_order<-sp_check[,.(.N,delta_beta=sum(delta_beta),occ=sum(occurrence_recent)),by=species_name][order(-N),]
sum(sp_check_order$N)
View(sp_check_order)
sp_check_order_2<-list_sp_contrib[species_name%in%sp_check_order$species_name,.(.N,delta_beta=sum(delta_beta),occ=sum(occurrence_recent)),by=species_name][order(-N),]

sp_check_order<-merge(sp_check_order,sp_check_order_2,by="species_name")
View(sp_check_order)
"Daucus carota"
nrow(list_sp_contrib)
list_sp_contrib[,.(sum(occurrence_past<11),sum(occurrence_recent<11)),]

View(list_sp_contrib[sp_relative_occ=="col" & sp_relative_beta=="Heterogenize",])


#### bootstrap 8 comps ####
n_cores_to_use<-28
clust_contrib<-makeCluster(n_cores_to_use)
registerDoParallel(clust_contrib)

rbind_with_fill <- function(...) rbind(..., fill = TRUE)

## supplementary analysis : bootstrap with random thermal optima assigned to each species
bootstrap_random_topt_8<-foreach(sertest=unique(pairs_ser$ser),
                               .combine = rbind,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr","foreach"))%dopar%{
                                 res<-run_contrib_multiple_time(200,sertest,table_flora,8,"topt_climplant",random_topt=T)
                                 cat(paste0(sertest," -> "))
                                 return(res)}




## supplementary analysis : bootstrap with rarefied occurrences for the time span of the ecoregions with the most occurrences
clust_contrib<-makeCluster(n_cores_to_use)
registerDoParallel(clust_contrib)
bootstrap_same_n_occurrence_r_topt_8<-foreach(sertest=unique(pairs_ser$ser),
                                     .combine = rbind,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr","foreach"))%dopar%{
                                       res<-run_contrib_multiple_time(200,sertest,table_flora,8,"topt_climplant",random_topt=T,rarefy=T)
                                       cat(paste0(sertest," -> "))
                                       return(res)}

clust_contrib<-makeCluster(n_cores_to_use)
registerDoParallel(clust_contrib)
bootstrap_same_n_occurrence_8<-foreach(sertest=unique(pairs_ser$ser),
                                              .combine = rbind,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr","foreach"))%dopar%{
                                                res<-run_contrib_multiple_time(200,sertest,table_flora,8,"topt_climplant",random_topt=F,rarefy=T)
                                                cat(paste0(sertest," -> "))
                                                return(res)}


stopCluster(clust_contrib)


bootstrap_same_n_occurrence_8_bug<-foreach(sertest=bug,
                                       .combine = rbind,.multicombine = T,.packages = c("data.table","stringr","foreach"))%do%{
                                         res<-run_contrib_multiple_time(5,sertest,table_flora,4,"topt_climplant",random_topt=F,rarefy=F)
                                         cat(paste0(sertest," -> "))
                                         return(res)}

table_flora[table_flora==2]<-1
get_contrib_one_ser("C30",table_flora,4,"topt_climplant",random_topt=F,rarefy=T)



saveRDS(bootstrap_random_topt_8,"//tsclient/C/Users/borderieux/Documents/calculus_computation/bootstrap_random_topt.RData")
saveRDS(bootstrap_same_n_occurrence_8,"//tsclient/C/Users/borderieux/Documents/calculus_computation/bootstrap_same_n_occurence.RData")



unique(table_flora)
table_flora[table_flora==4]

name_8_comp_beta<-colnames(contrib_therm_beta_ser_8)[(ncol(contrib_therm_beta_ser_8)-11):(ncol(contrib_therm_beta_ser_8)-4) ]
name_8_comp_topt<-colnames(contrib_therm_beta_ser_8)[(ncol(contrib_therm_beta_ser_8)-19):(ncol(contrib_therm_beta_ser_8)-12) ]

melt_beta<-melt(contrib_therm_beta_ser_8,id.vars="ser")[variable%in%c(name_8_comp_beta,"sp_delta_beta"),]
melt_beta_boot<-melt(bootstrap_random_topt_8,id.vars="ser")[variable%in%c(name_8_comp_beta,"sp_delta_beta"),]
melt_beta_boot_2<-melt(bootstrap_same_n_occurrence_r_topt_8,id.vars="ser")[variable%in%c(name_8_comp_beta,"sp_delta_beta"),]


melt_topt<-melt(contrib_therm_beta_ser_8,id.vars="ser")[variable%in%c(name_8_comp_topt,"sp_thermo"),]


ggplot(melt_beta,aes(x=variable,y=as.numeric(value),fill=variable))+theme_bw()+theme(axis.text.x = element_text(angle=20,vjust = 1, hjust=1))+
  geom_hline(yintercept = 0,lty=2)+geom_jitter(shape=21,alpha=0.6,color="white")+
  geom_boxplot(data=melt_beta_boot_2,alpha=0.4,outlier.shape = NA,fill="grey")+
  geom_boxplot(alpha=0.6,outlier.shape = NA)



ggplot(melt_topt,aes(x=variable,y=as.numeric(value),fill=variable))+theme_bw()+theme(axis.text.x = element_text(angle=20,vjust = 1, hjust=1))+
  geom_hline(yintercept = 0,lty=2)+geom_jitter(shape=21,alpha=0.6,color="white")+geom_boxplot(alpha=0.6,outlier.shape = NA)#+scale_discrete_manual(aesthetics = c("colour", "fill"),values=c("lightcyan4","cadetblue","indianred3","tomato","gold3"),breaks=c("contrib_beta_colder_less", "contrib_beta_colder_more", "contrib_beta_warmer_less" ,"contrib_beta_warmer_more","delta_beta"))

test_signif_random("contrib_beta_colder_ext_Homogenize",wilcox.test,
                   data_true = contrib_therm_beta_ser_8,
                   data_sim = bootstrap_random_topt_8)


test_signif_random("contrib_beta_colder_ext_Heterogenize",wilcox.test,
                   data_true = contrib_therm_beta_ser_8,
                   data_sim = bootstrap_same_n_occurrence_r_topt_8)

test_signif_random("contrib_beta_warmer_ext_Heterogenize",wilcox.test,
                   data_true = contrib_therm_beta_ser_8,
                   data_sim = bootstrap_same_n_occurrence_r_topt_8)

for ( i in c("sp_delta_beta" ,name_8_comp_beta)){
  print(i)
  test_signif_random(i,wilcox.test,
                     data_true = contrib_therm_beta_ser_8,
                     data_sim = bootstrap_same_n_occurrence_r_topt_8)
  
  
}

wilcox.test(contrib_therm_beta_ser_8$contrib_topt_colder_col_Heterogenize,-contrib_therm_beta_ser_8$contrib_topt_colder_ext_Homogenize)
mean(contrib_therm_beta_ser_8$contrib_topt_colder_col_Heterogenize-contrib_therm_beta_ser_8$contrib_topt_colder_ext_Homogenize)

wilcox.test(bootstrap_random_topt_8$contrib_topt_colder_col_Heterogenize,-bootstrap_random_topt_8$contrib_topt_colder_ext_Homogenize)
mean(bootstrap_random_topt_8$contrib_topt_colder_col_Heterogenize-bootstrap_random_topt_8$contrib_topt_colder_ext_Homogenize)



contrib_therm_beta_ser_8[,mean(contrib_beta_colder_col_Heterogenize+contrib_beta_colder_col_Homogenize)]
contrib_therm_beta_ser_8[,mean(contrib_beta_colder_ext_Heterogenize+contrib_beta_colder_ext_Homogenize)]

contrib_therm_beta_ser_8[,mean(contrib_beta_colder_col_Heterogenize)]
contrib_therm_beta_ser_8[,mean(contrib_beta_colder_col_Homogenize)]
contrib_therm_beta_ser_8[,mean(contrib_beta_colder_ext_Heterogenize)]
contrib_therm_beta_ser_8[,mean(contrib_beta_colder_ext_Homogenize)]



contrib_therm_beta_ser_8<-merge(contrib_therm_beta_ser_8,ser_variable_summary,by="ser")

ggplot(contrib_therm_beta_ser_8,aes(x=log(n_pair),y=contrib_beta_colder_col))+theme_bw()+geom_point()+geom_smooth()

ggplot(contrib_therm_beta_ser_8,aes(x=log(n_pair),y=contrib_beta_colder_ext_Heterogenize))+theme_bw()+geom_point()+geom_smooth()

c("contrib_beta_colder_ext","contrib_beta_warmer_ext" ,"contrib_beta_colder_col" ,"contrib_beta_warmer_col")
c("contrib_beta_colder_ext","contrib_beta_warmer_ext" ,"contrib_beta_colder_col" ,"contrib_beta_warmer_col")

labs_8<-name_8_comp_beta



labs_8<-c("Rare cold\nspecies","Common cold\nspecies","Common cold\nspecies","Rare cold\nspecies",
          "Rare warm\nspecies","Common warm\nspecies","Common warm\nspecies","Rare warm\nspecies")



dt_color_8<-data.table(variable=name_8_comp_beta,
                       color=c("cadetblue","cadetblue","lightcyan4","lightcyan4","tomato","tomato","indianred3","indianred3"),
                       label=labs_8)
dt_color_8_topt<-data.table(variable=name_8_comp_topt,
                       color=c("cadetblue","cadetblue","lightcyan4","lightcyan4","tomato","tomato","indianred3","indianred3"),
                       label=labs_8)


dt_label_color_reference<-data.table(variable=c("sp_thermo","sp_delta_beta",c("topt_ext","topt_col"),c("contrib_topt_colder_ext","contrib_topt_warmer_ext" ,"contrib_topt_colder_col" ,"contrib_topt_warmer_col"),
                                                c("beta_ext","beta_col"),c("contrib_beta_colder_ext","contrib_beta_warmer_ext" ,"contrib_beta_colder_col" ,"contrib_beta_warmer_col")),
                                     color=c("gold3","olivedrab",c("salmon4","peru"),c("lightcyan4","indianred3","cadetblue","tomato"),c("salmon4","peru"),c("lightcyan4","indianred3","cadetblue","tomato")),
                                     label=c("Thermophilization","\U0394\U03B2-diversity","Extinction","Colonization","Cold-adapted species","Warm-adapted species","Cold-adapted species","Warm-adapted species",
                                             "Extinction","Colonization","Cold-adapted species","Warm-adapted species","Cold-adapted species","Warm-adapted species"))

dt_label_color_reference<-rbind(dt_label_color_reference,dt_color_8)

dt_label_color_reference<-rbind(dt_label_color_reference,dt_color_8_topt)

oui<-create_one_box(dt=contrib_therm_beta_ser_8,dt_sim=bootstrap_random_topt_8,response = "beta",lims = c(-4,3.8),var=name_8_comp_beta[c(3,4,7,8,2,1,6,5)])

oui_topt<-create_one_box(dt=contrib_therm_beta_ser_8,dt_sim=bootstrap_random_topt_8,response = "thermo",lims = thermo_lims,var=name_8_comp_topt[c(3,4,7,8,2,1,6,5)])
oui<-create_one_box(dt=bootstrap_same_n_occurrence_8,dt_sim=bootstrap_random_topt_8,response = "beta",lims = c(-4,3.8),var=name_8_comp_beta[c(3,4,7,8,2,1,6,5)])

oui<-oui+theme(plot.margin=margin(3,3,3,3))

oui


export<-ggarrange(oui_topt,oui,nrow=2,labels = c("a)","b)"),align = "v")
export<-export+theme(plot.margin=margin(3,3,3,3))

ggsave("Figure_8_comp.png",oui,units ="mm",width = 180,height =90)
ggsave("Figure_8_comp.png",export,units ="mm",width = 180,height =90,scale=1.25)
ggsave("Figure_8_comp.svg",export,units ="mm",width = 180,height =90,scale=1.25)

oui


contrib_therm_beta_ser_8[!ser%in%bootstrap_same_n_occurrence_8$ser,ser]

bug<-c("C30", "H22" ,"H30" ,"I22")


#### plot fct####


##function that create a row of the main figure (Fig.3) if given the corresonding components
create_one_box<-function(var, # vectors of one or several components
                         response="thermo", # thermo or beta
                         lims=c(-0.35,0.6), # the Y axis limit
                         dt=contrib_therm_beta_ser, # the dataset
                         dt_sim=bootstrap_random_topt){ # the dataset of a null model for significance testing. here delta beta is not tested against a null model but 0
  var_save<-var
  first_var<-var[1]
  list_plot<-list()
  for(i in var){
    ## from wide to long data.table
    data<-suppressWarnings(melt(dt,id.vars="ser",verbose =F)[variable%in%i,])
    data_simu<-suppressWarnings(melt(dt_sim,id.vars="ser",verbose =F)[variable%in%i,])
    
    ## mean valu of the component
    mean_val<-round(mean(as.numeric(data$value),na.rm=T),2)
    mean_val_simu<-ifelse(response=="beta" && length(var)<7,0,round(mean(as.numeric(data_simu$value),na.rm=T),2))
    
    ##we use the wilcox.test to h0= no difference between the null model (thermo) or 0 (delta beta)
    p_val<-if(response=="beta" && length(var)<7) wilcox.test(as.numeric(data$value))$p.value else wilcox.test(as.numeric(data$value), as.numeric(data_simu$value))$p.value
    signif_star <- NA                            
    signif_star[p_val < 0.05] <- "*"
    signif_star[p_val < 0.01] <- "**"
    signif_star[p_val < 0.001] <- "***"
    signif_star[is.na(signif_star)] <- "n.s."
    
    # data$subtitle<-subtitle
    #sub<-subtitle
    res<-ggplot(data,aes(x=variable,y=as.numeric(value),fill=variable))+
      geom_hline(yintercept = mean_val_simu,lty=2)+
      geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable),show.legend = F)+
      geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
      theme_bw()+
      labs(x="",y=if(length(var)==2) if(response=="thermo")"Thermophilization °C/ 10years" else "\U0394\U03B2-diversity" else " ")+
      facet_wrap(~variable,labeller = facet_labeller)+
      coord_cartesian(ylim =lims)+
      annotate("text", x=0.45, y=lims[2]-0.2*lims[2],hjust=0, label=paste0("µ = ",mean_val," ",signif_star),size=3.5, color = "grey15")+
      theme(strip.text.x = element_text(size = 10,face="bold"))
    
    
    # change the number of signif numbers in the Y axis
    add_signif<-function(x){
      x<-as.character(x)
      x<-ifelse(response=="beta" & x!="0",paste0(x,".0"),x)
      # if(response=="beta" & x!=0) x<-paste0(x,".0")
      x
    }
    res<-res+
      scale_y_continuous(labels =add_signif)+
      scale_discrete_manual(aesthetics = c("colour", "fill"),
                            values=dt_label_color_reference$color,breaks=dt_label_color_reference$variable)
    
    # adjustement for several rows with different numbers of plot to match smoothly, and adjust the size of the boxplot
    res<-res +theme(axis.line.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
                    plot.margin =  margin(0,0,0,0),
                    panel.grid.major.x=element_blank(),
                    strip.background =element_rect(fill=if(response=="thermo")"#EAE397" else "#C5D49A"),
                    strip.text = element_text(margin = margin(0.1,0,0.1,0, "cm")))#+scale_x_discrete(expand = expansion(0.05,0.75))
    
    if(i != first_var)res<-res+theme(axis.title.y =  element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank())
    if(length(var)==1)res<-res+scale_x_discrete(expand = expansion(0,0.75))
    
    
    list_plot[[i]]<-res+theme(plot.background = element_rect( fill = NA,colour = NA,linewidth = 1))
  }
  ## adapt the first plot to the size of the other
  w<-if(length(var)==2) c(1.137,1)else if(length(var)==4)c(1.27,1,1,1) else if(length(var)==8)c(1.45,rep(1,7)) else 1
  
  return(ggarrange(plotlist=list_plot,ncol=length(var),align = "h",widths = w))# we arrange the several plots created into one row
  
}



#### habitat ####

NFI_habitat<-data.table(read.table(unz(file.path("Data","Data_NFI_2005_2021","export_dataifn_2005_2021.zip"),"HABITAT.csv"),sep=";",header=T))
colnames(NFI_habitat)<-tolower(colnames(NFI_habitat));colnames(NFI_habitat)[1]<-"campaign";NFI_habitat[,x:=NULL]

NFI_habitat<-merge(NFI_habitat,NFI_pairs_info[,c("idp","period")],by="idp")
sort(table(NFI_habitat$campaign))
sort(table(NFI_habitat$hic))
hab_interest<-c( "9190"  ,  "9170"  ,  "9140"  ,  "91F0"   , "9420"  ,  "9410" ,  "9180","91E0"  ,  "9150"   , "9110"  ,  "9160"   , "9130" )

NFI_habitat_sum<-NFI_habitat[hic%in%hab_interest,.(n_hab=.N),by=.(campaign,hic)]
NFI_habitat_sum[,topt_plot:=sum(n_hab), by=campaign]
NFI_habitat_sum[,prop_hab:=(n_hab)/topt_plot, by=campaign]


ggplot(NFI_habitat_sum,aes(x=as.character(campaign),y=prop_hab,fill=hic))+
  theme_bw()+
  geom_col()

#### fig 2 wit hgreco####
list_sp_contrib[sp_relative_occ=="col",weighted.mean(topt_climplant,abs(delta_occ))]
list_sp_contrib[sp_relative_occ=="ext",weighted.mean(topt_climplant,abs(delta_occ))]

summed_sp_occ<-list_sp_contrib[,.(delta_occ=sum(delta_occ),
                                  past_occ=sum(occurrence_past),
                                  recent_occ=sum(occurrence_recent),
                                  occ=sum(occurrence_total),
                                  topt_climplant=unique(topt_climplant)),by=.(greco,species_name,sp_relative_topt)]

summed_sp_occ<-list_sp_contrib[,.(delta_occ=sum(delta_occ),
                                  past_occ=sum(occurrence_past),
                                  recent_occ=sum(occurrence_recent),
                                  occ=sum(occurrence_total),
                                  topt_climplant=unique(topt_climplant)),by=.(greco,species_name,sp_relative_topt)]

summed_sp_occ<-summed_sp_occ[delta_occ!=0,]

summed_sp_occ[,sp_relative_occ:=ifelse(delta_occ >0,"col","ext")]

summed_sp_occ[,sum(delta_occ),by=sp_relative_occ]
n_plot_greco<-ser_variable_summary[,.(tot_plot=sum(n_plot)),by=greco]

summed_sp_occ<-merge(summed_sp_occ,n_plot_greco)

summed_sp_occ[,greco_name:=switchv(EXPR=greco, 
                                   A="North West",
                                   B="Center North",
                                   C="Grand East",
                                   D="Vosges",
                                   E="Jura",
                                   `F`="South West",
                                   G="Massif Central",
                                   H="Alpes",
                                   I="Pyrenees",
                                   J="Mediterranean","bug")]


label_plot<-summed_sp_occ[,sum(delta_occ)/unique(tot_plot),by=greco_name]
label_plot[,x:= 12.5]
label_plot[,y:= 0.43]
label_plot[,lab:= paste0("Δα-div: ",signif(V1,2))]

summed_sp_occ[,n_delta:=sum(delta_occ)/(tot_plot)]

ggplot(summed_sp_occ,aes(x=topt_climplant,y=delta_occ))+geom_point()+geom_smooth()

ggplot(summed_sp_occ,aes(x=sp_relative_topt,y=n_delta))+geom_jitter()+geom_smooth()+geom_boxplot()


summed_sp_occ[,cut_topt:=cut(topt_climplant,seq(1,17,1))]

summed_sp_occ[,sum(delta_occ),by=sp_relative_topt]
summed_sp_occ[,sum(delta_occ)/sum(occ),by=cut_topt]
summed_sp_occ[,sum(delta_occ)/sum(occ)]

toplot<-summed_sp_occ[,.(prop=(sum(delta_occ)/unique(sum(past_occ))), sum=sum(past_occ)),by=.(cut_topt)]
toplot[,lab:=as.character(signif(sum/1000,6))]
toplot[,lab:=str_replace_all(lab,"\\."," ")]
toplot[,lab:=ifelse(cut_topt%in% c("(4,5]","(10,11]","(13,14]"),paste0(lab,0),lab)] # manual work, careful



summed_sp_occ[,.(prop=(sum(delta_occ)/unique(sum(past_occ))), sum=sum(past_occ))]





sum(toplot$prop)

export_discuss<-ggplot(toplot[sum>300,],aes(x= as.numeric(cut_topt),y=prop*100))+
  geom_smooth(method = "lm",se=F,lty=2,color="cadetblue3" )+
  geom_point()+
  geom_hline(yintercept=0,lty=1,color="grey25")+
  geom_hline(yintercept=-0.01827827*100,lty=2,color="grey25")+
  geom_text(aes(label=lab),nudge_y = 0.015*100)+
  theme_bw()+
  scale_x_continuous(breaks = 4:13,labels =toplot[sum>300,]$cut_topt[order(as.numeric(toplot[sum>300,]$cut_topt))])+
  scale_y_continuous()+
  labs(x="Classes of thermal optimum °C",y="Occurrences change relative\nto past occurrences (%)")
 

export_discuss

ggsave(file.path("export_discuss.png"),export_discuss,dpi=250,scale = 1,width = 160,height = 80,units = "mm",)



out_fig_2<-ggplot(summed_sp_occ[sp_relative_occ!="stable",],aes(x=topt_climplant,weight=abs(delta_occ)/tot_plot,fill=sp_relative_occ,lty=sp_relative_occ))+
  geom_histogram(alpha=0.5,position = "identity",breaks=seq(1,16,by=1),color="grey20")+
  scale_fill_manual(breaks = c("col","ext"),values=c("lightcyan4","peru"),label =c("Gain in occurrences","Decline in occurrences"))+
  scale_linetype_manual(breaks = c("col","ext"),values=c(2,1),label = c("Gain in occurrences","Decline in occurrences"))+
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16))+
  geom_text(aes(label=lab,y=y,x=x),data=label_plot,inherit.aes=F,size=3)+
  labs(y="Absolute change of occurrence per plots",x="Species thermal optimum °C",fill="Dynamic",linetype="Dynamic")+
  theme_bw(base_size = 12)+
  theme(legend.background = element_blank(),legend.spacing.y = unit(0.2, 'cm'),legend.position=c(0.75,0.15))  +
  ## important additional element
  guides(fill = guide_legend(byrow = TRUE))+
  facet_wrap(~greco_name)


showtext:: showtext_auto(enable = F)
ggsave(file.path("Figures_results","Discussion_hist_greco.png"),out_fig_2,dpi=280,scale = 1,width = 180,height = 130,units = "mm",)
