if(T){
  chla_filter=read_excel(paste0(path,"KOSMOS_Kiel_spring_2024_Chlorophyll_a_25_03_2024_JT.xlsx"),sheet = "Main table",na=c("",NA))
  chla_filter$Day=factor(chla_filter$Day,c(1:3,seq(5,33,2)))
  chlcal=chla_filter


  if(T){
  concatenated_data=full_join(concatenated_data,chlcal)

  colnr=ncol(concatenated_data)
  tocalibrate=concatenated_data[concatenated_data$Set=="All cells" & concatenated_data$Mesocosm!="Fjord",c(1:11,(colnr-15):colnr)]

  tocalibratelarge=tocalibrate[tocalibrate$Settings=="large",]
  tocalibratesmall=tocalibrate[tocalibrate$Settings=="small",]

  lmlarge=lm(ChlaProxyRaw~`Chlorophyll a (µg/L)`,tocalibratelarge)
  lmsmall=lm(ChlaProxyRaw~`Chlorophyll a (µg/L)`,tocalibratesmall)

  #y=mx+t
  #x=(y-t)/m
  concatenated_data$ChlaProxyCalibrated[concatenated_data$Settings=="large"]=(concatenated_data$ChlaProxyRaw[concatenated_data$Settings=="large"]-lmlarge$coefficients[[1]])/lmlarge$coefficients[[2]]
  concatenated_data$ChlaProxyCalibrated[concatenated_data$Settings=="small"]=(concatenated_data$ChlaProxyRaw[concatenated_data$Settings=="small"]-lmsmall$coefficients[[1]])/lmsmall$coefficients[[2]]

  rm(colnr,tocalibrate,tocalibratelarge,tocalibratesmall,lmlarge,lmsmall)
  }

  # QC
  if(T){
  #
  # KOSMOStimeplot(chla_filter,"Chlorophyll a (µg/L)")
  # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="small" & concatenated_data$Set=="All cells",],"ChlaProxyRaw")
  # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="large" & concatenated_data$Set=="All cells",],"ChlaProxyRaw")
  # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="small" & concatenated_data$Set=="All cells",],"ChlaProxyCalibrated")
  # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="large" & concatenated_data$Set=="All cells",],"ChlaProxyCalibrated")
  #
  # plot(ChlaProxy~`Chlorophyll a (µg/L)`,tocalibratelarge)
  # abline(lmlarge)
  # plot(ChlaProxy~`Chlorophyll a (µg/L)`,tocalibratesmall)
  # abline(lmsmall)
  #
  # library(ggplot2)
  # ggplot(tocalibratelarge,aes(`Chlorophyll a (µg/L)`,ChlaProxy,col=Day))+
  #   geom_point()+
  #   labs(title="large",col="Day")
  # ggplot(tocalibratesmall,aes(`Chlorophyll a (µg/L)`,ChlaProxy,col=Day))+
  #   geom_point()+
  #   labs(title="small",col="Day")
  #
  # ggplot(tocalibratesmall,aes(`Chlorophyll a (µg/L)`,ChlaProxy,col=`% of total`))+
  #   geom_point()+
  #   labs(title="small",col="Day")
  # ggplot(tocalibratesmall,aes(`% of total`,ChlaProxy,col=Day))+
  #   geom_point()+
  #   labs(title="small",col="Day")
  #
  # smcomp=concatenated_data[concatenated_data$Set=="All cells",c("Day","Mesocosm","Settings","ChlaProxy")]
  # library(tidyr)
  # smcomplong=pivot_wider(smcomp,names_from="Settings",values_from="ChlaProxy")
  #
  # ggplot(smcomplong,aes(small,large,col=Day))+
  #   geom_point()+
  #   labs(title="Correlation of ChlaProxy between large and small setting",col="Day")
  #
  # plot(large~small,smcomplong)
  # abline(lm(large~small,smcomplong))
  #
  #
  # # tocalibratelarge=tocalibratelarge[!(tocalibratelarge$Day %in% c(1,2,3)),]
  # #
  # # tmp=tocalibratelarge[tocalibratelarge$Day %in% c(11),]
  # # points(tmp$`Chlorophyll a (µg/L)`,tmp$ChlaProxy,col="blue")
  # #
  # # #plot(tocalibratelarge$`Chlorophyll a (µg/L)`+1,tocalibratelarge$ChlaProxy,log="xy")
  # # #plot(tocalibratelarge$`Chlorophyll a (µg/L)`,tocalibratelarge$ChlaProxy)
  # # points(tocalibratelarge$`Chlorophyll a (µg/L)`,tocalibratelarge$ChlaProxy,col="blue")
  # # soos=lm(tocalibratelarge$ChlaProxy~tocalibratelarge$`Chlorophyll a (µg/L)`)
  # # abline(soos)
  # # # die fragwürdigen
  # # frage=tocalibratelarge[tocalibratelarge$`Chlorophyll a (µg/L)`>3.75 & tocalibratelarge$`Chlorophyll a (µg/L)`<5 & tocalibratelarge$ChlaProxy<10000,]
  # # points(frage$`Chlorophyll a (µg/L)`,frage$ChlaProxy,col="red")
  #
  #
  #
  #
  # #als Kosmosplot
  # library(KOSMOSplotR)
  # meem=concatenated_data[concatenated_data$Settings=="large" & concatenated_data$Set=="All cells",]
  # meem$chladivider=meem$ChlaProxy/meem$`Chlorophyll a (µg/L)`
  # KOSMOStimeplot(meem,"chladivider",ylimit=c(1000,7000))
  #
  # test2=concatenated_data[,c("Filename","Set","Count","Concentration [n/µl]","Mean FL Red Total","ChlaProxy")]

  }
}
