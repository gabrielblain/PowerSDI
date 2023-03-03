#' ScientSDI
#'
#' @param lon
#' longitude in decinal degrees: (+) Estern Hemispher (-) Western Hemisphere.
#' @param lat
#' latitude in decinal degrees: (+) Northern hemispher (-) Southern Hemisphere.
#' @param start.date
#' date at which the indices estimates should start. Format: DD-MM-YYYY".
#' @param end.date
#' date at which the indices estimates should end. Format: DD-MM-YYYY".
#' @param TS
#'Time scale on the "quart.month" basis (integer values between 1 and 96).
#' @return
# 'Scatter plots of Rainfall and potential evapotranspiration accumulated at the 1-quart.month time scale.
#' Four csv files with data calculated at the time scale selected by the user.
#' DistPar.csv: The parameters of the distributions (gamma and GEV) used to calculate the indices.
#' Goodness.csv: The Lilliefors and Anderson-Darling tests goodness-of-fit tests.
#' Normality.csv:  The outcomes of the two normality checking procedures (Wu et al., 2007 and Stagge et., 2015)
#' ScientSDI.csv: The NASA-SPI, NASA-SPEI.HS and NASA-SPEI.PM.
#' This function also presents other data obtained from the NASAPOWER project, that is:
#' Rainfall amounts (Rain).
#' Potential evapotranspitations values estimated through the Hargreaves & Samani method (EPHS).
#' Potential evapotranspitations values estimated through the FAO-56 Penman-Monteith method (EPPM).
#' The difference between rainfall and potential evapotranspiration (P-EPHS and P-EPPM).
#' @export
#' @import lmom nasapower
ScientSDI=function(lon,lat,start.date, end.date,TS){
  if (!require(nasapower)) install.packages('nasapower')
  if (!require(lmom)) install.packages('lmom')
  library(nasapower)
  library(lmom)
  if(is.na(as.Date(end.date, "%d-%m-%Y"))==TRUE || is.na(as.Date(start.date, "%d-%m-%Y"))==TRUE
     || TS<1 || TS>96 || all.equal(TS, as.integer(TS))!=TRUE){
    message("Recall Date format should be DD-MM-YYYY and TS must be an interger  value ranging between 1 and 96")} else {
      end.date.user=as.Date(end.date, "%d-%m-%Y")
      start.date.user=as.Date(start.date, "%d-%m-%Y")
      mim.date.fit=as.numeric((end.date.user-start.date.user)/365.25)
      if (mim.date.fit<10){message ("Please, select a longer period between start.date and end.date.")} else{
        if (mim.date.fit<29){message ("Caution: Ideally, the SPI and SPEI require at least a 30-year period for their calculation.
Consider selecting a longer period between start.date and end.date?")}
        end.date.user=as.Date(end.date, "%d-%m-%Y")
        start.date.user=as.Date(start.date, "%d-%m-%Y")
        mim.date.fit=end.date.user-start.date.user
        start.user.day=as.numeric(format(start.date.user, format = "%d"))
        end.user.day=as.numeric(format(end.date.user, format = "%d"))
        end.user.month=as.numeric(format(end.date.user, format = "%m"))
        start.year=as.numeric(format(start.date.user, format = "%Y"))
        start.month=as.numeric(format(start.date.user, format = "%m"))
        if (start.user.day <= 7){dif=start.user.day-1;start.week=1}
        if (start.user.day>7 & start.user.day<=14){dif=start.user.day-8;start.week=2}
        if (start.user.day>14 & start.user.day<=22){dif=start.user.day-15;start.week=3}
        if (start.user.day>22){dif=start.user.day-23;start.week=4}
        start.date.protocal=start.date.user-dif
        start.date.protocal=format(start.date.protocal,"%d-%m-%Y")

        message("Just a sec. Downloading NASA POWER Data.")
        sse_i=as.data.frame(get_power( community = "ag", lonlat = c(lon, lat),
                                       dates = c(start.date.protocal, end.date), temporal_api = "daily",
                                       pars = c("T2M","T2M_MAX","T2M_MIN","TOA_SW_DWN",
                                                "ALLSKY_SFC_SW_DWN","WS2M","RH2M","PRECTOTCORR")))
        message("Calculating the agrometeorological parameters. Just a little patience.")
        ####   Hargreaves&Samani
        ETP.harg.daily=0.0023*(sse_i$TOA_SW_DWN*0.4081633)*(sse_i$T2M_MAX-sse_i$T2M_MIN)^0.5*(sse_i$T2M+17.8) #por dia
        ###    Penman- Monteith-FAO
        es=0.6108*exp((17.27*sse_i$T2M)/(sse_i$T2M+273.3))
        ea=(sse_i$RH2M*es)/100
        slope.pressure=(4098*es)/((sse_i$T2M+237.3)^2)
        Q0.ajust=0.75*sse_i$TOA_SW_DWN
        Rn=(1-0.2)*sse_i$ALLSKY_SFC_SW_DWN-(1.35*(sse_i$ALLSKY_SFC_SW_DWN/Q0.ajust)-0.35)*(0.35-(0.14*sqrt(ea)))*(5.67*10^-8)*(((sse_i$T2M^4)+(sse_i$T2M_MIN^4))/2)
        ETP.pm.daily=(0.408*slope.pressure*(Rn-0.8)+0.063*(900/(sse_i$T2M+273))*sse_i$WS2M*(es-ea))/(slope.pressure+0.063*(1+0.34*sse_i$WS2M))
        sse_i=cbind(sse_i,ETP.harg.daily,ETP.pm.daily)
        n.tot=length(sse_i[,1])
        final.year=sse_i$YEAR[n.tot]
        final.month=sse_i$MM[n.tot]
        final.day=sse_i$DD[n.tot]
        if(final.day<=7){final.week=1}else{if(final.day<=14){final.week=2}else{if(final.day<=21){final.week=3}else{final.week=4}}}
        n.years=1+(final.year-1991);total.nweeks=48*n.years
        a=1;b=2;c=3;d=4;
        data.week=matrix(NA,total.nweeks,8)
        year=start.year
        for (year in start.year:final.year){
          gc();month=1
          for (month in 1:12){
            gc()
            data.week1=colSums(sse_i[which(sse_i$YEAR==year &
                                             sse_i$MM==month &
                                             sse_i$DD<=7),15:17])
            data.week2=colSums(sse_i[which(sse_i$YEAR==year &
                                             sse_i$MM==month &
                                             sse_i$DD>7 & sse_i$DD<=14),15:17])
            data.week3=colSums(sse_i[which(sse_i$YEAR==year &
                                             sse_i$MM==month &
                                             sse_i$DD>14 & sse_i$DD<=21),15:17])
            data.week4=colSums(sse_i[which(sse_i$YEAR==year &
                                             sse_i$MM==month &
                                             sse_i$DD>21),15:17])

            data.week[a,]=c(lon,lat,year,month,1,data.week1)
            data.week[b,]=c(lon,lat,year,month,2,data.week2)
            data.week[c,]=c(lon,lat,year,month,3,data.week3)
            data.week[d,]=c(lon,lat,year,month,4,data.week4)
            a=a+4;b=b+4;c=c+4;d=d+4
          }}
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
        ########removing suspicions data
        #####Rainfall
        plot(data.week[,6],xlab="1-quart.month time scale", ylab="Rainfall (mm)")
        question=menu(c("If yes (both upper and lower tails), type 1",
                        "If no, type 2",
                        "If only from the upper tail, type 3",
                        "If only from the lower tail, type 4"),
                      title="Are there suspicions weekly rainfall data to be removed?
              Remember that the function Accuracy.R may provide information for this question.")
        if (question==1){
          Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
          Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
          upremov=which(data.week[,6]>Uplim);lowremov=which(data.week[,6]<Lowlim)
          if (length(upremov)>0){data.week=data.week[-c(upremov),]; message("removed rowns:"); print(upremov)}
          if (length(lowremov)>0){data.week=data.week[-c(lowremov),]; message("removed rowns:"); print(lowremov)}}
        if (question==3){
          Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
          upremov=which(data.week[,6]>Uplim)
          if (length(upremov)>0){data.week=data.week[-c(upremov),]; message("removed rowns:"); print(upremov)}}
        if (question==4){
          Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
          lowremov=which(data.week[,6]<Lowlim)
          if (length(lowremov)>0){data.week=data.week[-c(lowremov),]; message("removed rowns:"); print(lowremov)}}
        #####Potential Evapotranspiration (Hargreaves & Samani)
        plot(data.week[,7],xlab="1-quart.month time scale", ylab="EP (Hargreaves; mm)")
        question=menu(c("If yes (both upper and lower tails), type 1",
                        "If no, type 2",
                        "If only from the upper tail, type 3",
                        "If only from the lower tail, type 4"),
                      title="Are there suspicions weekly potential evapotranspiration data to be removed?
              Remember that the function Accuracy.R may provide information for this question.")
        if (question==1){
          Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
          Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
          upremov=which(data.week[,7]>Uplim);lowremov=which(data.week[,7]<Lowlim)
          if (length(upremov)>0){data.week=data.week[-c(upremov),]; message("removed rowns:"); print(upremov)}
          if (length(lowremov)>0){data.week=data.week[-c(lowremov),]; message("removed rowns:"); print(lowremov)}}
        if (question==3){
          Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
          upremov=which(data.week[,7]>Uplim)
          if (length(upremov)>0){data.week=data.week[-c(upremov),]; message("removed rowns:"); print(upremov)}}
        if (question==4){
          Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
          lowremov=which(data.week[,7]<Lowlim)
          if (length(lowremov)>0){data.week=data.week[-c(lowremov),]; message("removed rowns:"); print(lowremov)}}
        #####Potential Evapotranspiration (FAO-56 Penman-Monteith)
        plot(data.week[,8],xlab="1-quart.month time scale", ylab="EP (FAO-56/PM; mm)")
        question=menu(c("If yes (both upper and lower tails), type 1",
                        "If no, type 2",
                        "If only from the upper tail, type 3",
                        "If only from the lower tail, type 4"),
                      title="Are there suspicions weekly potential evapotranspiration data to be removed?
              Remember that the function Accuracy.R may provide information for this question.")
        if (question==1){
          Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
          Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
          upremov=which(data.week[,8]>Uplim);lowremov=which(data.week[,8]<Lowlim)
          if (length(upremov)>0){data.week=data.week[-c(upremov),]; message("removed rowns:"); print(upremov)}
          if (length(lowremov)>0){data.week=data.week[-c(lowremov),]; message("removed rowns:"); print(lowremov)}}
        if (question==3){
          Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
          upremov=which(data.week[,8]>Uplim)
          if (length(upremov)>0){data.week=data.week[-c(upremov),]; message("removed rowns:"); print(upremov)}}
        if (question==4){
          Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
          lowremov=which(data.week[,8]<Lowlim)
          if (length(lowremov)>0){data.week=data.week[-c(lowremov),]; message("removed rowns:"); print(lowremov)}}
        #########
        n=length(data.week[,1])
        data.at.timescale=matrix(NA,(n-(TS-1)),6)
        final.point=n-(TS-1)
        ########Parameter fitting:Rainfall
        if(TS>1){
          point=1;a=1;b=TS;c=1
          data.at.timescale[c,]=c(data.week[b,3:4],data.week[b,9],colSums(data.week[a:b,6:8]))
          point=point+1;a=a+1;b=b+1;c=c+1
          while (point<=final.point){
            data.at.timescale[c,]=c(data.week[b,3:4],data.week[b,9],colSums(data.week[a:b,6:8]))
            point=point+1;a=a+1;b=b+1;c=c+1
          }}else{
            data.at.timescale=cbind(data.week[,3:4],data.week[,9],data.week[,6:8])
          }
        data.at.timescale=cbind(data.at.timescale,(data.at.timescale[,4]-data.at.timescale[,5]),(data.at.timescale[,4]-data.at.timescale[,6]))
        parameters=matrix(NA,48,10)
        complete=menu(c("If yes, type 1",
                        "If no, type 2"),
                      title="Do you want to calculate the goodness-of-fit and normality-checking tests?")
        if (complete==1){
          question.sig=menu(c("If 5% type 1 ", "If 10% type 2"), title="Please, select a significance level")
          if (question.sig==1){sig.level=0.95}else{sig.level=0.9}
          message("OK. This might take a while.")
          Goodness=matrix(NA,48,12)
          for (i in 1:48){
            rain=data.at.timescale[which(data.at.timescale[,3]==i),4];rain.nozero=rain[rain>0];n.rain=length(rain)
            n.nonzero=length(rain.nozero); n.z=n.rain-n.nonzero; probzero=(n.z+1)/(2*(n.rain+1))
            soma.rain=matrix(NA,n.nonzero,1)
            parameters[i,1:4]=c(i,pelgam(samlmu(rain.nozero)),probzero)
            prob.rain=sort(cdfgam(rain.nozero,c(parameters[i,2],parameters[i,3])))
            prob.rain[prob.rain<0.001351]=0.001351;prob.rain[prob.rain>0.998649]=0.998649
            prob.emp=sort(rank(rain.nozero,na.last = NA,ties.method=c("first")))/n.nonzero
            Goodness[i,1]=max(abs(prob.emp-prob.rain))
            petp.harg=data.at.timescale[which(data.at.timescale[,3]==i),7]
            petp.pm=data.at.timescale[which(data.at.timescale[,3]==i),8]
            parameters[i,5:10]=c(pelgev(samlmu(petp.harg)),pelgev(samlmu(petp.pm)))
            prob.harg=sort(cdfgev(petp.harg,c(parameters[i,5],parameters[i,6],parameters[i,7])))
            prob.harg[prob.harg<0.001351]=0.001351;prob.harg[prob.harg>0.998649]=0.998649
            n.harg=length(petp.harg)
            soma.harg=matrix(NA,n.harg,1)
            prob.emp=sort(rank(petp.harg,na.last = NA,ties.method=c("first")))/n.harg
            Goodness[i,3]=max(abs(prob.emp-prob.harg))
            prob.pm=sort(cdfgev(petp.pm,c(parameters[i,8],parameters[i,9],parameters[i,10])))
            prob.pm[prob.pm<0.001351]=0.001351;prob.pm[prob.pm>0.998649]=0.998649
            n.pm=length(petp.pm)
            soma.pm=matrix(NA,n.pm,1)
            prob.emp=sort(rank(petp.pm,na.last = NA,ties.method=c("first")))/n.pm
            Goodness[i,5]=max(abs(prob.emp-prob.pm))
            for (ad in 1:n.nonzero){
              soma.rain[ad,1]=((2*ad)-1)*((log(prob.rain[ad]))+log(1-prob.rain[n.nonzero+1-ad]))
            }
            Goodness[i,7]=-n.nonzero-((1/n.nonzero)*sum(soma.rain,na.rm = TRUE))
            for (ad in 1:n.harg){
              soma.harg[ad,1]=((2*ad)-1)*((log(prob.harg[ad]))+log(1-prob.harg[n.harg+1-ad]))
            }
            Goodness[i,9]=-n.harg-((1/n.harg)*sum(soma.harg,na.rm = TRUE))
            for (ad in 1:n.pm){
              soma.pm[ad,1]=((2*ad)-1)*((log(prob.pm[ad]))+log(1-prob.pm[n.pm+1-ad]))
            }
            Goodness[i,11]=-n.pm-((1/n.pm)*sum(soma.pm,na.rm = TRUE))

            ####Critical values
            null.dist=matrix(NA,2000,6)
            for (j in 1:2000){
              x=sort(quagam(runif(n.nonzero), c(parameters[i,2],parameters[i,3])))
              prob.synt=cdfgam(x,pelgam(samlmu(x)))
              prob.synt[prob.synt<0.001351]=0.001351;prob.synt[prob.synt>0.998649]=0.998649
              prob.emp=sort(rank(x))/n.nonzero
              null.dist[j,1]=max(abs(prob.emp-prob.synt))
              for (ad in 1:n.nonzero){
                soma.rain[ad,1]=((2*ad)-1)*((log(prob.synt[ad]))+log(1-prob.synt[n.nonzero+1-ad]))
              }
              null.dist[j,4]=-n.nonzero-((1/n.nonzero)*sum(soma.rain,na.rm = TRUE))
              y=sort(quagev(runif(n.harg), c(parameters[i,5],parameters[i,6],parameters[i,7])))
              prob.synt=cdfgev(y,pelgev(samlmu(y)))
              prob.synt[prob.synt<0.001351]=0.001351;prob.synt[prob.synt>0.998649]=0.998649
              prob.emp=sort(rank(y))/n.harg
              null.dist[j,2]=max(abs(prob.emp-prob.synt))
              for (ad in 1:n.harg){
                soma.harg[ad,1]=((2*ad)-1)*((log(prob.synt[ad]))+log(1-prob.synt[n.harg+1-ad]))
              }
              null.dist[j,5]=-n.harg-((1/n.harg)*sum(soma.harg,na.rm = TRUE))
              z=sort(quagev(runif(n.pm), c(parameters[i,8],parameters[i,9],parameters[i,10])))
              prob.synt=cdfgev(z,pelgev(samlmu(z)))
              prob.synt[prob.synt<0.001351]=0.001351;prob.synt[prob.synt>0.998649]=0.998649
              prob.emp=sort(rank(z))/n.pm
              null.dist[j,3]=max(abs(prob.emp-prob.synt))
              for (ad in 1:n.pm){
                soma.pm[ad,1]=((2*ad)-1)*((log(prob.synt[ad]))+log(1-prob.synt[n.pm+1-ad]))
              }
              null.dist[j,6]=-n.pm-((1/n.pm)*sum(soma.pm,na.rm = TRUE))
            }
            Goodness[i,2]=quantile(null.dist[,1],sig.level)
            Goodness[i,4]=quantile(null.dist[,2],sig.level)
            Goodness[i,6]=quantile(null.dist[,3],sig.level)
            Goodness[i,8]=quantile(null.dist[,4],sig.level)
            Goodness[i,10]=quantile(null.dist[,5],sig.level)
            Goodness[i,12]=quantile(null.dist[,6],sig.level)
          }
          parameters=cbind(lon,lat,parameters)
          colnames(parameters)=c("lon","lat","quart.month","alfa.rain","beta.rain",
                                 "probzero.rain","loc.harg","sc.harg","sh.harg","loc.pm","sc.pm","sh.pm")
          colnames(Goodness)=c("Lili.Rain","Crit","Lili.PEPHarg","Crit","Lili.PEPPM","Crit",
                               "AD.Rain","Crit","AD.PEPHarg","Crit","AD.PEPPM","Crit")
          ########Parameter fitting:P-EP
          n.weeks=length(data.at.timescale[,1]);pos=1;SDI=matrix(NA,n.weeks,3)
          while (pos<=n.weeks){
            i=data.at.timescale[pos,3]
            prob=parameters[i,6]+(1-parameters[i,6])*cdfgam(data.at.timescale[pos,4],c(parameters[i,4],parameters[i,5]))
            if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
            SDI[pos,1]=qnorm(prob, mean = 0, sd = 1)
            prob=cdfgev(data.at.timescale[pos,7],c(parameters[i,7],parameters[i,8],parameters[i,9]))
            if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
            SDI[pos,2]=qnorm(prob, mean = 0, sd = 1)
            prob=cdfgev(data.at.timescale[pos,8],c(parameters[i,10],parameters[i,11],parameters[i,12]))
            if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
            SDI[pos,3]=qnorm(prob, mean = 0, sd = 1)
            pos=pos+1
          }
          categories=matrix(NA,n.weeks,3)
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
            if(SDI[i,3]<=-2.0 & !is.na(SDI[i,3])){categories[i,3]="ext.dry"} else{
              if(SDI[i,3]<=-1.5 & !is.na(SDI[i,3])){categories[i,3]="sev.dry"} else{
                if(SDI[i,3]<=-1.0 & !is.na(SDI[i,3])){categories[i,3]="mod.dry"} else{
                  if(SDI[i,3]<=1.0 & !is.na(SDI[i,3])) {categories[i,3]="Normal"}  else{
                    if(SDI[i,3]<=1.5 & !is.na(SDI[i,3])) {categories[i,3]="mod.wet"} else{
                      if(SDI[i,3]<=2.0 & !is.na(SDI[i,3])) {categories[i,3]="sev.wet"} else{
                        if(SDI[i,3]>2.0 & !is.na(SDI[i,3]))  {categories[i,3]="ext.wet"}}}}}}}
          }
          SDI=cbind(data.at.timescale,SDI)
          #####Normality checking procedures
          Norn.check=matrix(NA,48,15)
          for (j in 1:48){
            SDI.week=as.matrix(SDI[which(SDI[,3]==j),9:11])
            w=shapiro.test(SDI.week[,1])
            Norn.check[j,1:3]=c(w$statistic, w$p.value, abs(median((SDI.week[,1]),na.rm=TRUE)))
            w=shapiro.test(SDI.week[,2])
            Norn.check[j,4:6]=c(w$statistic, w$p.value, abs(median((SDI.week[,2]),na.rm=TRUE)))
            w=shapiro.test(SDI.week[,3])
            Norn.check[j,7:9]=c(w$statistic, w$p.value, abs(median((SDI.week[,3]),na.rm=TRUE)))
            ######As proposed in Wu et al. (2007)
            if(Norn.check[j,1]<0.960 && Norn.check[j,2]<0.10 && Norn.check[j,3]>0.05){Norn.check[j,10]="NoNorn"}else{Norn.check[j,10]="Nornal"}
            if(Norn.check[j,4]<0.960 && Norn.check[j,5]<0.10 && Norn.check[j,6]>0.05){Norn.check[j,11]="NoNorn"}else{Norn.check[j,11]="Nornal"}
            if(Norn.check[j,7]<0.960 && Norn.check[j,8]<0.10 && Norn.check[j,9]>0.05){Norn.check[j,12]="NoNorn"}else{Norn.check[j,12]="Nornal"}
            ######As proposed in Stagge et al. (2015)
            if(Norn.check[j,2]<0.10){Norn.check[j,13]="NoNorn"}else{Norn.check[j,13]="Nornal"}
            if(Norn.check[j,5]<0.10){Norn.check[j,14]="NoNorn"}else{Norn.check[j,14]="Nornal"}
            if(Norn.check[j,8]<0.10){Norn.check[j,15]="NoNorn"}else{Norn.check[j,15]="Nornal"}
          }
          ##########
          colnames(Norn.check)=c("SPI.Shap","SPI.Shap.p","SPI.AbsMed",
                                 "SPEI.Harg.Shap","SPEI.Harg.Shap.p","SPEI.Harg.AbsMed",
                                 "SPEI.PM.Shap","SPEI.PM.Shap.p","SPEI.PM.AbsMed",
                                 "SPI.testI","SPEI.Harg.testI","SPEI.PM.testI",
                                 "SPI.testII","SPEI.Harg.testII","SPEI.PM.testII")
          SDI.final=data.frame(SDI,categories)
          colnames(SDI.final)=c("Year","Month","quart.month","Rain","EP.Harg","EP.PM","P-EP.Harg","P-EP.PM",
                                "SPI","SPEI.Harg","SPEI.PM","Categ.SPI","Categ.SPEI.Harg","Categ.SPEI.PM")
          if (end.user.month==1 || end.user.month==3 || end.user.month==5 ||
              end.user.month==7 || end.user.month==8 || end.user.month==10 || end.user.month==12){
            if(end.user.day<7 || end.user.day>7 &  end.user.day<14 ||
               end.user.day>14 &  end.user.day<22 || end.user.day>22 &  end.user.day<31)
            {message ("The latest quart.month period is not complete")
              SDI.final=SDI.final[-c(n.weeks),]}}
          if (end.user.month==4 || end.user.month==6 || end.user.month==9 || end.user.month==11){
            if(end.user.day<7 || end.user.day>7 &  end.user.day<14 ||
               end.user.day>14 &  end.user.day<22 || end.user.day>22 &  end.user.day<30)
            {message ("The latest quart.month period is not complete")
              SDI.final=SDI.final[-c(n.weeks),]}}
          if (end.user.month==2){
            if(end.user.day<7 || end.user.day>7 &  end.user.day<14 ||
               end.user.day>14 &  end.user.day<22 || end.user.day>22 &  end.user.day<28)
            {message ("The latest quart.month period is not complete")
              SDI.final=SDI.final[-c(n.weeks),]}}
          whichTS=paste("TS is",as.character(TS))
          row.names(SDI.final[1,])=whichTS
          csv_fname = "ScientSDI.csv"
          suppressWarnings(write.table(SDI.final, file = csv_fname, sep = ",",
                                       append = TRUE, quote = FALSE,
                                       col.names = TRUE, row.names = FALSE))
          csv_fname = "DistPar.csv"
          suppressWarnings(write.table(parameters, file = csv_fname, sep = ",",
                                       append = TRUE, quote = FALSE,
                                       col.names = TRUE, row.names = FALSE))
          csv_fname = "Goodness.csv"
          suppressWarnings(write.table(Goodness, file = csv_fname, sep = ",",
                                       append = TRUE, quote = FALSE,
                                       col.names = TRUE, row.names = FALSE))
          csv_fname = "Normality.csv"
          suppressWarnings(write.table(Norn.check, file = csv_fname, sep = ",",
                                       append = TRUE, quote = FALSE,
                                       col.names = TRUE, row.names = FALSE))

          message("Done. See, it didn't take so long")
          if(is.na(sum(SDI.final[,4]))==TRUE){message("please, check the NASAPOWER data it might have missing records")}
        }
        if (complete==2){
          for (i in 1:48){
            rain=data.at.timescale[which(data.at.timescale[,3]==i),4];rain.nozero=rain[rain>0];n.rain=length(rain)
            n.nonzero=length(rain.nozero); n.z=n.rain-n.nonzero; probzero=(n.z+1)/(2*(n.rain+1))
            parameters[i,1:4]=c(i,pelgam(samlmu(rain.nozero)),probzero)
            petp.harg=data.at.timescale[which(data.at.timescale[,3]==i),7]
            petp.pm=data.at.timescale[which(data.at.timescale[,3]==i),8]
            parameters[i,5:10]=c(pelgev(samlmu(petp.harg)),pelgev(samlmu(petp.pm)))
          }
          parameters=cbind(lon,lat,parameters)
          colnames(parameters)=c("lon","lat","quart.month","alfa.rain","beta.rain",
                                 "probzero.rain","loc.harg","sc.harg","sh.harg","loc.pm","sc.pm","sh.pm")
          n.weeks=length(data.at.timescale[,1]);pos=1;SDI=matrix(NA,n.weeks,3)
          while (pos<=n.weeks){
            i=data.at.timescale[pos,3]
            prob=parameters[i,6]+(1-parameters[i,6])*cdfgam(data.at.timescale[pos,4],c(parameters[i,4],parameters[i,5]))
            if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
            SDI[pos,1]=qnorm(prob, mean = 0, sd = 1)
            prob=cdfgev(data.at.timescale[pos,7],c(parameters[i,7],parameters[i,8],parameters[i,9]))
            if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
            SDI[pos,2]=qnorm(prob, mean = 0, sd = 1)
            prob=cdfgev(data.at.timescale[pos,8],c(parameters[i,10],parameters[i,11],parameters[i,12]))
            if (is.na(prob)==FALSE & prob<0.001351){prob=0.001351};if (is.na(prob)==FALSE & prob>0.998649){prob=0.998649}
            SDI[pos,3]=qnorm(prob, mean = 0, sd = 1)
            pos=pos+1
          }
          categories=matrix(NA,n.weeks,3)
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
            if(SDI[i,3]<=-2.0 & !is.na(SDI[i,3])){categories[i,3]="ext.dry"} else{
              if(SDI[i,3]<=-1.5 & !is.na(SDI[i,3])){categories[i,3]="sev.dry"} else{
                if(SDI[i,3]<=-1.0 & !is.na(SDI[i,3])){categories[i,3]="mod.dry"} else{
                  if(SDI[i,3]<=1.0 & !is.na(SDI[i,3])) {categories[i,3]="Normal"}  else{
                    if(SDI[i,3]<=1.5 & !is.na(SDI[i,3])) {categories[i,3]="mod.wet"} else{
                      if(SDI[i,3]<=2.0 & !is.na(SDI[i,3])) {categories[i,3]="sev.wet"} else{
                        if(SDI[i,3]>2.0 & !is.na(SDI[i,3]))  {categories[i,3]="ext.wet"}}}}}}}
          }
          SDI=cbind(data.at.timescale,SDI)
          SDI.final=data.frame(SDI,categories)
          colnames(SDI.final)=c("Year","Month","quart.month","Rain","EP.Harg","EP.PM","P-EP.Harg","P-EP.PM",
                                "SPI","SPEI.Harg","SPEI.PM","Categ.SPI","Categ.SPEI.Harg","Categ.SPEI.PM")
          if (end.user.month==1 || end.user.month==3 || end.user.month==5 ||
              end.user.month==7 || end.user.month==8 || end.user.month==10 || end.user.month==12){
            if(end.user.day<7 || end.user.day>7 &  end.user.day<14 ||
               end.user.day>14 &  end.user.day<22 || end.user.day>22 &  end.user.day<31)
            {message ("The latest quart.month period is not complete")
              SDI.final=SDI.final[-c(n.weeks),]}}
          if (end.user.month==4 || end.user.month==6 || end.user.month==9 || end.user.month==11){
            if(end.user.day<7 || end.user.day>7 &  end.user.day<14 ||
               end.user.day>14 &  end.user.day<22 || end.user.day>22 &  end.user.day<30)
            {message ("The latest quart.month period is not complete")
              SDI.final=SDI.final[-c(n.weeks),]}}
          if (end.user.month==2){
            if(end.user.day<7 || end.user.day>7 &  end.user.day<14 ||
               end.user.day>14 &  end.user.day<22 || end.user.day>22 &  end.user.day<28)
            {message ("The latest quart.month period is not complete")
              SDI.final=SDI.final[-c(n.weeks),]}}
          whichTS=paste("TS is",as.character(TS))
          row.names(SDI.final[1,])=whichTS
          csv_fname = "ScientSDI.csv"
          suppressWarnings(write.table(SDI.final, file = csv_fname, sep = ",",
                                       append = TRUE, quote = FALSE,
                                       col.names = TRUE, row.names = FALSE))
          csv_fname = "DistPar.csv"
          suppressWarnings(write.table(parameters, file = csv_fname, sep = ",",
                                       append = TRUE, quote = FALSE,
                                       col.names = TRUE, row.names = FALSE))
          message("Done. See, it didn't take so long")
        }
      }}
  rm(list = ls())}
