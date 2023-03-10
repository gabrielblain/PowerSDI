#' Reference
#'
#' @param ref
#'A csv file with the variables required for calculating the SPI and SPEI.
#'The number of variables depends on the potential evapotranspiration method selected by the user.
#'Variables required for both Hargreaves & Samani and FAO-56 Penman-Monteith methods:
#'1st Column (Head=YEAR): The year of each daily data
#'2nd Column (Head=MM): The month of each daily data (1 to 12)
#'3rd Column (Head=DD): The day of each daily data (1 to 28, 29, 30, or, 31)
#'4th Column (Head=tmed): Daily average air temperature at 2 meters above the ground (ºC)
#'5th Column (Head=tmax): Daily maximum air temperature at 2 meters above the ground (ºC)
#'6th Column (Head=tmin): Daily minimum air temperature at 2 meters above the ground (ºC)
#'7th Column (Head=Ra): Daily top of the atmosphere (extraterrestrial) radiation (MJ/m^2/day)
#'For the Hargreaves & Samani method
#'8th Column (Head=Rain): Daily rainfall amounts (mm)
#'Tip: Use the file referenceHS.csv as example.
#'For the FAO-56 Penman-Monteith method
#'8th Column (Head=Rs): Daily global horizontal irradiance (MJ/m^2/day)
#'9th Column (Head=W): Daily average wind speed at 2 meters above the ground (m/s)
#'10th Column (Head=RH): Daily average relative humidity at 2 meters above the ground (%)
#'11th Column (Head=Rain): Daily rainfall amounts (mm)
#'Tip: Use the file referencePM.csv as example.

#' @return
#'Scatter plots of Rainfall and potential evapotranspiration accumulated at the 1-quart.month time scale.
#'A csv file (ReferenceSDI.csv) with: Rain, Potential evapotranspiration,
#'The difference between rainfall and potential evapotranspiration,
#'SPI and SPEI calculated at the times scales selected by the user.
#'The user may also select the evapotranspiration estimation method:
#'Hargreaves & Samani or FAO-56 Penman-Monteith method

#'
#' @export
#' @import lmom
Reference=function(ref){
  if (!require(lmom)) install.packages('lmom')
  library(lmom)
  N.TS=as.numeric(readline(prompt="How many time scales?;N.TS="))
  if(N.TS<1 || N.TS>96 || all.equal(N.TS, as.integer(N.TS))!=TRUE){
    print("N.TS must be an interger  value ranging between 1 and 96")} else {
      warning("Make sure that you have a period of at least 30 years of continuous records, ok?")
      TS=matrix(NA,N.TS,1)
      TS[1,1]=as.numeric(readline(prompt="Please, specify the first time scale "))
      if (all.equal(TS[1,1], as.integer(TS[1,1]))!=TRUE || is.na(TS[1,1])==TRUE){
        message("TS must be an interger value ranging between 1 and 96.
                 Please, choose another TS")} else{
                   if(N.TS>1){
                     for (ts in 2:N.TS){
                       TS[ts,1]=as.numeric(readline(prompt="Please, specify the next time scale "))
                       if (all.equal(TS[ts,1], as.integer(TS[ts,1]))!=TRUE  || is.na(TS[ts,1])==TRUE){
                         stop("TS must be an interger value ranging between 1 and 96.
            Please, choose another TS and press esc")}
                     }}
distribution=menu(c("If GEV, type 1", "If GLO, type 2"),
title="Generalized Extreme Value (GEV) or Generalized Logistic (GLO) to calculate the SPEI?")
                   n.tot=length(ref[,1])
                   end.year=ref$YEAR[n.tot]
                   end.month=ref$MM[n.tot]
                   end.day=ref$DD[n.tot]
                   start.year=ref$YEAR[1]
                   start.month=ref$MM[1]
                   start.day=ref$DD[1]
                   if (start.day <= 7){start.week=1}
                   if (start.day>7 & start.day<=14){start.week=2}
                   if (start.day>14 & start.day<=22){start.week=3}
                   if (start.day>22){start.week=4}

                   question=menu(c("If Hargreaves & Samani type 1 ", "If FAO-56 Penman-Monteith type 2"), title="Please, select the potential evapotranspiration method")
                   if (question==1){
                     tmed=ref$tmed;tmax=ref$tmax;tmin=ref$tmin;Ra=ref$Ra;Rain=ref$Rain
                     ####Hargreaves & Samani
                     ETP.harg.daily=0.0023*(Ra*0.4081633)*(tmax-tmin)^0.5*(tmed+17.8)
                     message("Please wait. If N.TS is large, it will take a while (about 2 minutes for each TS).")
                     ref=cbind(ref,ETP.harg.daily)
                     if(end.day<=7){end.week=1}else{if(end.day<=14){end.week=2}else{if(end.day<=21){end.week=3}else{end.week=4}}}
                     n.years=1+(end.year-1991);total.nweeks=48*n.years
                     a=1;b=2;c=3;d=4;
                     data.week=matrix(NA,total.nweeks,5)
                     for (year in start.year:end.year){
                       gc()
                       for (month in 1:12){
                         gc()
                         data.week1=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD<=7),8:9])
                         data.week2=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD>7 & ref$DD<=14),8:9])
                         data.week3=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD>14 & ref$DD<=21),8:9])
                         data.week4=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD>21),8:9])

                         data.week[a,]=c(year,month,1,data.week1)
                         data.week[b,]=c(year,month,2,data.week2)
                         data.week[c,]=c(year,month,3,data.week3)
                         data.week[d,]=c(year,month,4,data.week4)
                         a=a+4;b=b+4;c=c+4;d=d+4
                       }}}
                   if (question==2){
                     tmed=ref$tmed;tmax=ref$tmax;tmin=ref$tmin;Ra=ref$Ra
                     Rs=ref$Rs; W=ref$W; RH=ref$RH
                     Rain=ref$Rain
                     ###FAO-56 Penman-Monteith
                     es=0.6108*exp((17.27*tmed)/(tmed+273.3))
                     ea=(RH*es)/100
                     slope.pressure=(4098*es)/((tmed+237.3)^2)
                     Q0.ajust=0.75*Ra
                     Rn=(1-0.2)*Rs-(1.35*(Rs/Q0.ajust)-0.35)*(0.35-(0.14*sqrt(ea)))*(5.67*10^-8)*(((tmed^4)+(tmin^4))/2)
                     ETP.pm.daily=(0.408*slope.pressure*(Rn-0.8)+0.063*(900/(tmed+273))*W*(es-ea))/(slope.pressure+0.063*(1+0.34*W))
                     message("Please wait. If N.TS is large, it will take a while (about 2 minutes for each TS).")
                     ref=cbind(ref,ETP.pm.daily)
                     n.tot=length(ref[,1])
                     end.year=ref$YEAR[n.tot]
                     end.month=ref$MM[n.tot]
                     end.day=ref$DD[n.tot]
                     if(end.day<=7){end.week=1}else{if(end.day<=14){end.week=2}else{if(end.day<=21){end.week=3}else{end.week=4}}}
                     n.years=1+(end.year-1991);total.nweeks=48*n.years
                     a=1;b=2;c=3;d=4;
                     data.week=matrix(NA,total.nweeks,5)
                     for (year in 1991:end.year){
                       gc()
                       for (month in 1:12){
                         gc()
                         data.week1=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD<=7),11:12])
                         data.week2=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD>7 & ref$DD<=14),11:12])
                         data.week3=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD>14 & ref$DD<=21),11:12])
                         data.week4=colSums(ref[which(ref$YEAR==year &
                                                        ref$MM==month &
                                                        ref$DD>21),11:12])

                         data.week[a,]=c(year,month,1,data.week1)
                         data.week[b,]=c(year,month,2,data.week2)
                         data.week[c,]=c(year,month,3,data.week3)
                         data.week[d,]=c(year,month,4,data.week4)
                         a=a+4;b=b+4;c=c+4;d=d+4
                       }}}
                   rows=which(data.week[,1]==end.year & data.week[,2]>end.month)
                   n.rows=length(rows)
                   if(n.rows>0){data.week=data.week[-c(rows),]}
                   rows=which(data.week[,1]==end.year & data.week[,2]==end.month & data.week[,3]>end.week)
                   n.rows=length(rows)
                   if(n.rows>0){data.week=data.week[-c(rows),]}
                   data.week=suppressWarnings(cbind(data.week,rep(1:48,n.years)))
                   first.row=which(data.week[,1]==start.year & data.week[,2]==start.month & data.week[,3]==start.week)
                   if(first.row>1){
                     data.week=data.week[-(1:(first.row-1)),]
                   }
                   if(start.day!=7 || start.day!=14 || start.day!=22 || start.day!=30){
                     data.week=data.week[c(-1),]
                   }
                   ########Visual analysis.
                   plot.rain=menu(c("If yes, type 1 ", "If no, type 2"), title="Show the scatter plot of the rainfall data?")
                   if (plot.rain==1){
                     plot(data.week[,4],xlab="1-quart.month time scale", ylab="Rainfall (mm)")
                     message("if you detected suspicious data, they may be removed from the input file.")}
                   plot.ep=menu(c("If yes, type 1 ", "If no, type 2"), title="Show the scatter plot of the evapotranspiration?")
                   if (plot.ep==1){
                     plot(data.week[,5],xlab="1-quart.month time scale", ylab="Potential Evapotranspiration (mm)")
                     message("if you detected suspicious data, they should be removed from the input file.")}
                   n=length(data.week[,1])
                   for(ts in 1:N.TS){
                     TS1=TS[ts,1]
                     data.at.timescale=matrix(NA,(n-(TS1-1)),5)
                     end.point=n-(TS1-1)
                     ########Parameter fitting:Rainfall
                     if(TS1>1){
                       point=1;a=1;b=TS1;c=1
                       data.at.timescale[c,]=c(data.week[b,1:2],data.week[b,6],colSums(data.week[a:b,4:5]))
                       point=point+1;a=a+1;b=b+1;c=c+1
                       while (point<=end.point){
                         data.at.timescale[c,]=c(data.week[b,1:2],data.week[b,6],colSums(data.week[a:b,4:5]))
                         point=point+1;a=a+1;b=b+1;c=c+1
                       }}else{
                         data.at.timescale=cbind(data.week[,1:2],data.week[,6],data.week[,4:5])
                       }
                     data.at.timescale=cbind(data.at.timescale,(data.at.timescale[,4]-data.at.timescale[,5]))
                     parameters=matrix(NA,48,7)
                     for (i in 1:48){
                       rain=data.at.timescale[which(data.at.timescale[,3]==i),4];rain.nozero=rain[rain>0];n.rain=length(rain)
                       n.nonzero=length(rain.nozero); n.z=n.rain-n.nonzero; probzero=(n.z+1)/(2*(n.rain+1))
                       parameters[i,1:4]=c(i,pelgam(samlmu(rain.nozero)),probzero)
                       pep=data.at.timescale[which(data.at.timescale[,3]==i),6]
                       if (distribution==1){
                         parameters[i,5:7]=c(pelgev(samlmu(pep)))}else{parameters[i,5:7]=c(pelglo(samlmu(pep)))}
                     }
                     colnames(parameters)=c("lastweek","alfa.gam","beta.gam","probzero.rain","loc.gev","sc.gev","sh.gev")
                     n.weeks=length(data.at.timescale[,1]);pos=1;SDI=matrix(NA,n.weeks,2)
                     while (pos<=n.weeks){
                       i=data.at.timescale[pos,3]
                       prob=parameters[i,4]+(1-parameters[i,4])*cdfgam(data.at.timescale[pos,4],c(parameters[i,2],parameters[i,3]))
                       if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
                       SDI[pos,1]=qnorm(prob, mean = 0, sd = 1)
                       if (distribution==1){
                         prob=cdfgev(data.at.timescale[pos,6],c(parameters[i,5],parameters[i,6],parameters[i,7]))}else{
                           prob=cdfglo(data.at.timescale[pos,6],c(parameters[i,5],parameters[i,6],parameters[i,7]))}
                       if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
                       SDI[pos,2]=qnorm(prob, mean = 0, sd = 1)
                       pos=pos+1
                     }
                     categories=matrix(NA,n.weeks,2)
                     for(i in 1:n.weeks){
                       if(SDI[i,1]<=-2.0 & !is.na(SDI[i,1])){categories[i,1]="ext.dry"} else{
                         if(SDI[i,1]<=-1.5 & !is.na(SDI[i,1])){categories[i,1]="sev.dry"} else{
                           if(SDI[i,1]<=-1.0 & !is.na(SDI[i,1])){categories[i,1]="mod.dry"} else{
                             if(SDI[i,1]<=1.0 & !is.na(SDI[i,1])) {categories[i,1]="Normal"}  else{
                               if(SDI[i,1]<=1.5 & !is.na(SDI[i,1])) {categories[i,1]="mod.wet"} else{
                                 if(SDI[i,1]<=2.0 & !is.na(SDI[i,1])) {categories[i,1]="sev.wet"} else{
                                   if(SDI[i,1]>2.0 & !is.na(SDI[i,1]))  {categories[i,1]="ext.wet"}}}}}}}
                       if(SDI[i,2]<=-2.0 & !is.na(SDI[i,2])){categories[i,2]="ext.dry"} else{
                         if(SDI[i,2]<=-1.5 & !is.na(SDI[i,2])){categories[i,2]="sev.dry"} else{
                           if(SDI[i,2]<=-1.0 & !is.na(SDI[i,2])){categories[i,2]="mod.dry"} else{
                             if(SDI[i,2]<=1.0 & !is.na(SDI[i,2])) {categories[i,2]="Normal"}  else{
                               if(SDI[i,2]<=1.5 & !is.na(SDI[i,2])) {categories[i,2]="mod.wet"} else{
                                 if(SDI[i,2]<=2.0 & !is.na(SDI[i,2])) {categories[i,2]="sev.wet"} else{
                                   if(SDI[i,2]>2.0 & !is.na(SDI[i,2]))  {categories[i,2]="ext.wet"}}}}}}}
                     }
                     SDI=cbind(data.at.timescale,SDI)
                     SDI.final=data.frame(SDI,categories)
                     colnames(SDI.final)=c("Year","Month","quart.month","Rain","EP","P-EP",
                                           "SPI","SPEI","Categ.SPI","Categ.SPEI")
                     if (end.month==1 || end.month==3 || end.month==5 ||
                         end.month==7 || end.month==8 || end.month==10 || end.month==12){
                       if(end.day<7 || end.day>7 &  end.day<14 ||
                          end.day>14 &  end.day<22 || end.day>22 &  end.day<31)
                       {message ("The latest quart.month period is not complete")
                         SDI.final=SDI.final[-c(n.weeks),]}}
                     if (end.month==4 || end.month==6 || end.month==9 || end.month==11){
                       if(end.day<7 || end.day>7 &  end.day<14 ||
                          end.day>14 &  end.day<22 || end.day>22 &  end.day<30)
                       {message ("The latest quart.month period is not complete")
                         SDI.final=SDI.final[-c(n.weeks),]}}
                     if (end.month==2){
                       if(end.day<7 || end.day>7 &  end.day<14 ||
                          end.day>14 &  end.day<22 || end.day>22 &  end.day<28)
                       {message ("The latest quart.month period is not complete")
                         SDI.final=SDI.final[-c(n.weeks),]}}

                     csv_fname = "ReferenceSDI.csv"
                     suppressWarnings(write.table(SDI.final, file = csv_fname, sep = ",",
                                                  append = TRUE, quote = FALSE,
                                                  col.names = TRUE, row.names = FALSE))
                   }
                 }}
  rm(list = ls())}
