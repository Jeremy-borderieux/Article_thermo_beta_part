# fiugre en français 


##this data.table contains the label used for the different variable, as well as their corresponding color scheme
dt_label_color_reference<-data.table(variable=c("sp_thermo","sp_delta_beta",c("topt_ext","topt_col"),c("contrib_topt_colder_ext","contrib_topt_warmer_ext" ,"contrib_topt_colder_col" ,"contrib_topt_warmer_col"),
                                                c("beta_ext","beta_col"),c("contrib_beta_colder_ext","contrib_beta_warmer_ext" ,"contrib_beta_colder_col" ,"contrib_beta_warmer_col")),
                                     color=c("gold3","olivedrab",c("salmon4","peru"),c("lightcyan4","indianred3","cadetblue","tomato"),c("salmon4","peru"),c("lightcyan4","indianred3","cadetblue","tomato")),
                                     label=c("Thermophilisation","\U0394\U03B2-diversité","Extinction","Colonisation","Cold-adapted species","Warm-adapted species","Cold-adapted species","Warm-adapted species",
                                             "Extinction","Colonization","Cold-adapted species","Warm-adapted species","Cold-adapted species","Warm-adapted species"))


color_ref_main_fig<-data.table(breaks=c("sp_thermo","sp_delta_beta","ext","col",
                                        "contrib_colder_ext","contrib_warmer_ext","contrib_colder_col","contrib_warmer_col"),
                               color=dt_label_color_reference$color[1:8],
                               #label=c("Variable","Variable","Extinction","Colonization","Ext. Cold-adapted","Ext. Warm-adapted","Col. Cold-adapted","Col. Warm-adapted"))
                               label=c("Total","Total","Extinction","Colonisation","Extinction esp.\n climat froid","Extinction esp.\n climat chaud","Colonisation esp.\n climat froid","Colonisation esp.\n climat chaud"))

thermo_var<-c("sp_thermo","topt_ext","topt_col","contrib_topt_colder_ext" ,"contrib_topt_warmer_ext"  ,"contrib_topt_colder_col"  ,"contrib_topt_warmer_col"  )


contrib_therm_beta_ser_melt<-melt(contrib_therm_beta_ser,measure.vars=colnames(contrib_therm_beta_ser)[c(2:9,14:17,33,34)])
contrib_therm_beta_ser_melt[,variable:=as.character(variable)]
contrib_therm_beta_ser_melt[,type:=ifelse(grepl("beta",variable),"2_beta","1_thermo")]
contrib_therm_beta_ser_melt[,variable_2:=str_remove_all(variable,"topt_")]
contrib_therm_beta_ser_melt[,variable_2:=str_remove_all(variable_2,"beta_")]
contrib_therm_beta_ser_melt[,variable_3:=ifelse(variable_2%in% c("sp_thermo","sp_delta_beta"),"Total",variable_2)]



bootstrap_random_topt_melt<-melt(bootstrap_random_topt,measure.vars=colnames(contrib_therm_beta_ser)[c(2:9,14:17,33,34)])
bootstrap_random_topt_melt[,value_random_topt:=value]
bootstrap_random_topt_melt[variable%in%thermo_var,value_random_topt:=value_random_topt]

contrib_therm_beta_ser_melt<-cbind(contrib_therm_beta_ser_melt,bootstrap_random_topt_melt[,"value_random_topt"])

table_test<-contrib_therm_beta_ser_melt[,.(mean_val=mean(value),
                                           mean_val_random=mean(value_random_topt)*10,
                                           p_val=ifelse(!variable%in%thermo_var,
                                                        wilcox.test(value)$p.value,
                                                        wilcox.test(value,(value_random_topt))$p.value )),by=variable]



label_facets<-c(`1_thermo`="Thermophilisation",`2_beta`="\U0394\U03B2-diversité")
#label_facets<-c(`1_thermo`="- - - - - 0 - - - - -0 - - - - - - 0 - - - - - 0 - - - - -",`2_beta`="- - - - - 0 - - - - -0 - - - - - - 0 - - - - - 0 - - - - -")



table_test[,signif_star:="n.s."]
table_test[p_val < 0.05,"signif_star"] <- "*"
table_test[p_val < 0.01,"signif_star"] <- "**"
table_test[p_val < 0.001,"signif_star"] <- "***"

table_test[,type:=ifelse(grepl("beta",variable),"2_beta","1_thermo")]
table_test[,label:=paste0("µ = ",round(mean_val*ifelse(type=="1_thermo",10,1),2)," ",signif_star)]
table_test[,variable_2:=str_remove_all(variable,"topt_")]
table_test[,variable_2:=str_remove_all(variable_2,"beta_")]
table_test[,variable_3:=ifelse(variable_2%in% c("sp_thermo","sp_delta_beta"),"Total",variable_2)]
table_test[,x:=ifelse(type=="1_thermo",-0.45,1.93)]
table_test[variable%in%c("contrib_topt_colder_col","contrib_topt_warmer_ext"),x:=0.19]
table_test[variable%in%c("beta_col","contrib_beta_warmer_col","contrib_beta_colder_col"),x:=-2.5]

contrib_therm_beta_ser_melt

contrib_therm_beta_ser[,ecoregion:= ifelse(greco%in% c("J"),"Medit",ifelse(greco%in% c("D","G","E","H","I"),"Mountain","Lowland")),]


plot_thermo<-ggplot(contrib_therm_beta_ser_melt[type=="1_thermo"],aes(x=value*10,y=variable_3,fill=variable_2))+
  geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable_2),show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
  geom_text(data=table_test[type=="1_thermo",],aes(x=x,y=variable_3,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(4.5,6.5),lwd=0.5)+
  geom_point(data=table_test[type=="1_thermo",],aes(x= mean_val_random, y= variable_3),pch=21,fill="white",color="grey20",size=3)+
  scale_fill_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_color_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_y_discrete(limits=c("Total",color_ref_main_fig$breaks[-c(1,2)])[7:1] ,label= color_ref_main_fig$label[8:2] )+
  theme(strip.text.x = element_text(size = 10,face="bold"),plot.margin = margin(r=0,l=4),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#EAE397",linewidth = 0.5))+
  labs(x="Thermophilisation °C/10ans",y="Contributions au total")


plot_beta<-ggplot(contrib_therm_beta_ser_melt[type=="2_beta"],aes(x=value,y=variable_3,fill=variable_2))+
  geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable_2),show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
  geom_text(data=table_test[type=="2_beta",],aes(x=x,y=variable_3,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(4.5,6.5),lwd=0.5)+
  coord_cartesian(xlim=c(-3,4))+
  scale_fill_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_color_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_y_discrete(limits=c("Total",color_ref_main_fig$breaks[-c(1,2)])[7:1] ,label= rep("",7) )+
  theme(strip.text.x = element_text(size = 10,face="bold"),axis.ticks = element_blank(),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#C5D49A",linewidth = 0.5))+
  labs(x="\U0394\U03B2-diversité",y=" ")


out_main_fig<-ggarrange(plotlist = list(plot_thermo,plot_beta+theme(plot.background = element_rect(fill=NA,color=NA))),labels = c("a)","b)"),align = "h",widths = c(1.25,1))

showtext:: showtext_auto(enable = FALSE) 
ggsave(file.path("Figures_results_fr","main_figure_reworked.jpg"),out_main_fig,width =180,height =120,units = "mm",scale=0.95,dpi=400)
showtext:: showtext_auto(enable = TRUE)



#### fig sampling ####



## we create a map of forest ecoregions with the number of pairs inside, open street map as a background
breaks=c(0,50,100,200,300,400,2000)
ser_value_sf$cut_n_duet<-cut(ser_value_sf$n_pair,breaks=breaks)

ser_of_interest<-"G42"
ser_of_interes_sf<-ser_value_sf[ser_value_sf$ser==ser_of_interest,]
#ext_ser<-bbox(as_Spatial(ser_of_interes_sf))
NFI_pairs_info_interest_sf<-st_as_sf( NFI_pairs_info[ser==ser_of_interest,] ,coords=c("xl","yl"),crs=st_crs(2154))

ser_value_sf$ecoregion<-ifelse(ser_value_sf$greco%in% c("J"),"Mediterranean",ifelse(ser_value_sf$greco%in% c("D","G","E","H","I"),"Mountain","Lowland"))
large_ecoregion_sf<-aggregate(ser_value_sf,by=list(ser_value_sf$ecoregion),FUN=function(x)x[1])


map_all_ser_france<-ggplot(ser_value_sf)+
  annotation_map_tile(type="cartolight",zoomin = 0)+
  annotation_north_arrow(location = "tl",height = unit(0.8, "cm"),width = unit(0.8, "cm"))+
  annotation_scale(location = "bl", text_cex = 0.75)+
  geom_sf(aes(fill=(cut_n_duet)),color="grey60",show.legend = T)+
  geom_sf(data=large_ecoregion_sf,fill=NA,color="grey25",linewidth=0.5)+
  geom_sf(data=ser_of_interes_sf,linewidth=0.5,color="darkred",fill=NA)+
  theme_bw()+
  theme(legend.position = "bottom",legend.direction = "vertical")+
  guides(fill=guide_legend(nrow=2,order=c(2)))+
  
  labs(pattern="Ecoregion type")+
  scale_fill_manual(values=colorRampPalette(c("ivory","peachpuff3","olivedrab"))(7),na.value = "grey65",label=c("15:50","51:100","101:200","201:300","301:400",">400","Exclue"),name="Nombre de paire de placettes")



## we load a 150m * 150m resolution forest map to vizualize one forest ecoregions
forest_cover_25m<-raster(file.path("Data","Spatial_Data","Forest_cover_one_ecoreg_G42_150m.tif"))
forest_cover_25m_dt<-data.table(xyFromCell(forest_cover_25m,1:ncell(forest_cover_25m)))
forest_cover_25m_dt$cover_f<-getValues(forest_cover_25m)

## we create the single ecoregion map with NFI plots
one_ser_plots<-ggplot(ser_of_interes_sf)+
  geom_sf(fill="cornsilk",color="grey25",linewidth=0.75)+
  geom_raster(data=forest_cover_25m_dt[cover_f>0.5,],aes(x=x,y=y),fill="#B4BF89",show.legend = F)+
  geom_sf(fill="NA",color="grey25",linewidth=0.75)+
  geom_sf(data=NFI_pairs_info_interest_sf,aes(fill=period),color="grey10",size=1.65,alpha=0.75,shape=21)+
  scale_fill_manual(values =  c("darkorange","deepskyblue3"),breaks=c("past","recent"),label=c("Placettes anciennes (2005-2011)","Placettes récentes (2015-2021)"))+
  guides(fill=guide_legend(nrow=2,title.position = "top",override.aes = list(size=3.5)))+
  theme_grey()+
  labs(x="",y="",fill="Périodes des placettes IFN")+
  theme_void()+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())



##we combine the two maps
export_map_fig_1<-ggarrange(map_all_ser_france,
                            one_ser_plots,
                            ncol=2,align = "v",labels = c("(a)","(b)"),widths=c(1,0.8) )+theme(panel.background=element_rect(fill="white",color="white"))


showtext:: showtext_auto(enable = FALSE)
ggsave(file.path("Figures_results_fr","export_map_ext_fig_1.png"),device = "png",type="cairo",dpi=300,export_map_fig_1,scale=1,width = 180,height = 120,unit="mm")

rm(map_all_ser_france,one_ser_plots,export_map_fig_1)



#### fig 2 ####


list_sp_contrib[sp_relative_occ=="col",weighted.mean(topt_climplant,abs(delta_occ))]
list_sp_contrib[sp_relative_occ=="ext",weighted.mean(topt_climplant,abs(delta_occ))]

summed_sp_occ<-list_sp_contrib[,.(delta_occ=sum(delta_occ),
                                  past_occ=sum(occurrence_past),
                                  recent_occ=sum(occurrence_recent),
                                  occ=sum(occurrence_total),
                                  topt_climplant=unique(topt_climplant)),by=species_name]
summed_sp_occ<-summed_sp_occ[delta_occ!=0,]

summed_sp_occ[,sp_relative_occ:=ifelse(delta_occ >0,"col","ext")]

summed_sp_occ[,sum(delta_occ),by=sp_relative_occ]


summa<-summed_sp_occ[,sum(delta_occ),by=species_name]
sum(summa$V1>0)
sum(summa$V1<0)
sum(summa$V1==0)

label_fig_2<-c(paste0("Gains\nµopt. thermique:",signif(summed_sp_occ[sp_relative_occ=="col",weighted.mean(topt_climplant,abs(delta_occ))],3) ,"°C"),
               paste0("Pertes\nµopt. thermique:",signif(summed_sp_occ[sp_relative_occ=="ext",weighted.mean(topt_climplant,abs(delta_occ))],3) ,"°C" ))


out_fig_2<-ggplot(summed_sp_occ[sp_relative_occ!="stable",],aes(x=topt_climplant,weight=(delta_occ),fill=sp_relative_occ,lty=sp_relative_occ))+
  geom_histogram(alpha=0.5,position = "identity",breaks=seq(1,16,by=1),color="grey20")+
  scale_fill_manual(breaks = c("col","ext"),values=c("lightcyan4","peru"),label =label_fig_2)+
  scale_linetype_manual(breaks = c("col","ext"),values=c(2,1),label = label_fig_2)+
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16))+
  #facet_wrap(~greco)+
  labs(y="Changement d'occurrences",x="Optimum thermique de l'espèce",fill="Dynamique",linetype="Dynamique")+
  theme_bw(base_size = 12)+
  theme(legend.background = element_blank(),legend.position = c(0.8,0.25),legend.spacing.y = unit(0.2, 'cm'))  +
  ## important additional element
  guides(fill = guide_legend(byrow = TRUE))


showtext:: showtext_auto(enable = F)
ggsave(file.path("Figures_results_fr","fig_hist_main_reworked.png"),out_fig_2,dpi=400,scale = 1.8,width = 80,height = 70,units = "mm",)
ggsave(file.path("Figures_results_fr","fig_hist_main_reworked_2.png"),out_fig_2,dpi=400,scale = 1.8,width = 80,height = 40,units = "mm",)


