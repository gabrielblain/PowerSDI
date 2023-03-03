#' OperatSDI
#'
#' @param lonlat
#' A 2-column matrix.
#' The first column is the longitude in decinal degrees: (+) Estern hemisphere (-) Western hemisphere.
#' The second column is the latitude in decinal degrees: (+) Northern hemisphere (-) Southern hemisphere.
#' @param start.date
#' Date at each the calculation must star (format; “DD-MM-YYYY").
#' @param end.date
#' Date at each the calculation must end (format; “DD-MM-YYYY").
#' @param DistPar
#' A csv file provided by the Scient.R function with the distributions parameters required for calculating the indices.
#' @param TS
#' #'Time scale on the "quart.month" basis (integer values between 1 and 96).
#' @return
#' A csv file with Rainfall amounts, potential evapotranspiration amounts (EP; Hargreaves & Samani or FAO-56 Penman-Monteith),
#' differences between rainfall and EP, the NASA-SPI and NASA_SPEI,
#' and the SPI/SPEI classification (dry/wet categories) corresponding to each indices estimates.
#' @export
#' @import lmom nasapower
OperatSDI=function(lonlat, start.date, end.date, DistPar, TS){
  question.par=menu(c("If Yes type 1 ", "If No type 2"), title="Do you have the parameters for all locals?")
  if (question.par==2){message("You must first run function ScientSDI.R")}
  if (question.par==1){
    if (!require(nasapower)) install.packages('nasapower')
    if (!require(lmom)) install.packages('lmom')
    library(nasapower)
    library(lmom)
    N.locals=length(lonlat[,1])
    if(is.na(as.Date(start.date, "%d-%m-%Y"))==TRUE || is.na(as.Date(end.date, "%d-%m-%Y"))==TRUE || TS<1 || TS>96 || all.equal(TS, as.integer(TS))!=TRUE ){
      warning("Date format should be DD-MM-YYYY, TS must be an interger value ranging between 1 and 96")}
    else {
      end.date.user=as.Date(end.date, "%d-%m-%Y")
      start.date.user=as.Date(start.date, "%d-%m-%Y")
      mim.date.fit=end.date.user-start.date.user
      start.user.day=as.numeric(format(start.date.user, format = "%d"))
      end.user.day=as.numeric(format(end.date.user, format = "%d"))
      start.year=as.numeric(format(start.date.user, format = "%Y"))
      start.month=as.numeric(format(start.date.user, format = "%m"))
      if (mim.date.fit<7)
      {message("Time difference between end.date and start.date must be equal to or longer than 7 days")}else{
        if (start.user.day <= 7){dif=start.user.day-1;start.week=1}
        if (start.user.day>7 & start.user.day<=14){dif=start.user.day-8;start.week=2}
        if (start.user.day>14 & start.user.day<=22){dif=start.user.day-15;start.week=3}
        if (start.user.day>22){dif=start.user.day-23;start.week=4}
        start.date.protocal=start.date.user-dif
        start.date.protocal=format(start.date.protocal,"%d-%m-%Y")
        question=menu(c("If Hargreaves type 1 ", "If Penman type 2"), title="Please, select the potential evapotranspiration method")
        message("Calculating. If the number of locals are large, it might take a while.")
        local=1
        for (local in 1:N.locals){
          lon=lonlat[local,1]; lat=lonlat[local,2]
          if (question==1){
            sse_i=as.data.frame(get_power( community = "ag", lonlat = c(lon, lat),
                                           dates = c(start.date.protocal, end.date), temporal_api = "daily",
                                           pars = c("T2M","T2M_MAX","T2M_MIN","TOA_SW_DWN","PRECTOTCORR")))
            ####Hargreaves & Samani
            ETP.harg.daily=0.0023*(sse_i$TOA_SW_DWN*0.4081633)*(sse_i$T2M_MAX-sse_i$T2M_MIN)^0.5*(sse_i$T2M+17.8) #por dia
            sse_i=cbind(sse_i,ETP.harg.daily)
            n.tot=length(sse_i[,1])
            final.year=sse_i$YEAR[n.tot]
            final.month=sse_i$MM[n.tot]
            final.day=sse_i$DD[n.tot]
            if(final.day<=7){final.week=1}else{if(final.day<=14){final.week=2}else{if(final.day<=21){final.week=3}else{final.week=4}}}
            n.years=1+(final.year-start.year);total.nweeks=48*n.years
            a=1;b=2;c=3;d=4;
            data.week=matrix(NA,total.nweeks,7)
            year=start.year
            for (year in start.year:final.year){
              gc();month=1
              for (month in 1:12){
                gc()
                data.week1=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD<=7),12:13])
                data.week2=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD>7 & sse_i$DD<=14),12:13])
                data.week3=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD>14 & sse_i$DD<=21),12:13])
                data.week4=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD>21),12:13])

                data.week[a,]=c(lon,lat,year,month,1,data.week1)
                data.week[b,]=c(lon,lat,year,month,2,data.week2)
                data.week[c,]=c(lon,lat,year,month,3,data.week3)
                data.week[d,]=c(lon,lat,year,month,4,data.week4)
                a=a+4;b=b+4;c=c+4;d=d+4
              }}}
          if (question==2){

            sse_i=as.data.frame(get_power( community = "ag", lonlat = c(lon, lat),
                                           dates = c(start.date.protocal, end.date), temporal_api = "daily",
                                           pars = c("T2M","T2M_MAX","T2M_MIN","TOA_SW_DWN",
                                                    "ALLSKY_SFC_SW_DWN","WS2M","RH2M","PRECTOTCORR")))
            ###FAO-56 Penman-Monteith
            es=0.6108*exp((17.27*sse_i$T2M)/(sse_i$T2M+273.3))
            ea=(sse_i$RH2M*es)/100
            slope.pressure=(4098*es)/((sse_i$T2M+237.3)^2)
            Q0.ajust=0.75*sse_i$TOA_SW_DWN
            Rn=(1-0.2)*sse_i$ALLSKY_SFC_SW_DWN-(1.35*(sse_i$ALLSKY_SFC_SW_DWN/Q0.ajust)-0.35)*(0.35-(0.14*sqrt(ea)))*(5.67*10^-8)*(((sse_i$T2M^4)+(sse_i$T2M_MIN^4))/2)
            ETP.pm.daily=(0.408*slope.pressure*(Rn-0.8)+0.063*(900/(sse_i$T2M+273))*sse_i$WS2M*(es-ea))/(slope.pressure+0.063*(1+0.34*sse_i$WS2M))
            sse_i=cbind(sse_i,ETP.pm.daily)
            n.tot=length(sse_i[,1])
            final.year=sse_i$YEAR[n.tot]
            final.month=sse_i$MM[n.tot]
            final.day=sse_i$DD[n.tot]
            if(final.day<=7){final.week=1}else{if(final.day<=14){final.week=2}else{if(final.day<=21){final.week=3}else{final.week=4}}}
            n.years=1+(final.year-start.year);total.nweeks=48*n.years
            a=1;b=2;c=3;d=4;
            data.week=matrix(NA,total.nweeks,7)
            year=start.year
            for (year in start.year:final.year){
              gc();month=1
              for (month in 1:12){
                gc()
                data.week1=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD<=7),15:16])
                data.week2=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD>7 & sse_i$DD<=14),15:16])
                data.week3=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD>14 & sse_i$DD<=21),15:16])
                data.week4=colSums(sse_i[which(sse_i$YEAR==year &
                                                 sse_i$MM==month &
                                                 sse_i$DD>21),15:16])

                data.week[a,]=c(lon,lat,year,month,1,data.week1)
                data.week[b,]=c(lon,lat,year,month,2,data.week2)
                data.week[c,]=c(lon,lat,year,month,3,data.week3)
                data.week[d,]=c(lon,lat,year,month,4,data.week4)
                a=a+4;b=b+4;c=c+4;d=d+4
              }}}
          rows=which(data.week[,3]==final.year & data.week[,4]>final.month)
          n.rows=length(rows)
          if(n.rows>0){data.week=data.week[-c(rows),]}
          rows=which(data.week[,3]==final.year & data.week[,4]==final.month & data.week[,5]>final.week)
          n.rows=length(rows)
          if(n.rows>0){data.week=data.week[-c(rows),]}
          data.week=suppressWarnings(cbind(data.week,rep(1:48,n.years)))
          first.row=which(data.week[,3]==start.year & data.week[,4]==start.month & data.week[,5]==start.week)
          if(first.row>1){
            data.week=data.week[-(1:(first.row-1)),]
          }
          n=length(data.week[,1])
          data.at.timescale=matrix(NA,(n-(TS-1)),7)
          final.point=n-(TS-1)
          ########
          if(TS>1){
            point=1;a=1;b=TS;c=1
            data.at.timescale[c,]=c(data.week[b,1:4],data.week[b,8],colSums(data.week[a:b,6:7]))
            point=point+1;a=a+1;b=b+1;c=c+1
            while (point<=final.point){
              data.at.timescale[c,]=c(data.week[b,1:4],data.week[b,8],colSums(data.week[a:b,6:7]))
              point=point+1;a=a+1;b=b+1;c=c+1
            }}else{
              data.at.timescale=cbind(data.week[,1:4],data.week[,8],data.week[,6:7])
            }
          data.at.timescale=cbind(data.at.timescale,(data.at.timescale[,6]-data.at.timescale[,7]))
          n.weeks=length(data.at.timescale[,1]);pos=1;SDI=matrix(NA,n.weeks,2)
          parameters=as.data.frame(DistPar[which(DistPar$lon==lon & DistPar$lat==lat),])
          if(length(parameters[,1])==0){message("It seems that you don't have the gamma or GEV parameters for this local.
                                      Run Scient.R function.")}else{
                                        pos=1
                                        while (pos<=n.weeks){
                                          i=data.at.timescale[pos,5];par=as.numeric(parameters[i,])
                                          prob=(par[6]+(1-par[6]))*cdfgam(data.at.timescale[pos,6],c(par[4],par[5]))
                                          if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
                                          SDI[pos,1]=qnorm(prob, mean = 0, sd = 1)
                                          if (question==1){
                                            prob=cdfgev(data.at.timescale[pos,8],c(par[7],par[8],par[9]))}
                                          if (question==2){
                                            prob=cdfgev(data.at.timescale[pos,8],c(par[10],par[11],par[12]))}
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
                                        colnames(SDI.final)=c("Lon","Lat","Year","Month","quart.month","Rain","EP","P-EP",
                                                              "SPI","SPEI","Categ.SPI","Categ.SPEI")
                                        csv_fname = "OperationalSDI.csv"
                                        suppressWarnings(write.table(SDI.final, file = csv_fname, sep = ",",
                                                                     append = TRUE, quote = FALSE,
                                                                     col.names = TRUE, row.names = FALSE))}}}
      if (final.month==1 || final.month==3 || final.month==5 ||
          final.month==7 || final.month==8 || final.month==10 ||
          final.month==12){
        if(end.user.day==7 || end.user.day==14 || end.user.day==22 || end.user.day==31)
        {message ("Done")} else
        {message ("The latest quart.month period is not complete. Showing it as preliminary results.")}}

      if (final.month==4 || final.month==6 || final.month==9 || final.month==11){
        if(end.user.day==7 || end.user.day==14 || end.user.day==22 || end.user.day==30)
        {message ("Done")} else
        {message ("The latest quart.month period is not complete. Showing it as preliminary results.")}}
      if (final.month==2){ if(end.user.day==7 || end.user.day==14 ||
                              end.user.day==22 || end.user.day==28 || end.user.day==29)
      {message ("Done")} else
      {message ("The latest quart.month period is not complete. Showing it as preliminary results.")}}

      if(is.na(sum(SDI.final[,10]))==TRUE){message("Check the NASAPOWER data, it might have missing records")}
      rm(list = ls())}}}
