#' Accuracy
#'
#' @param obs_est
#' A 2-column csv file.
#' The 1st column is the reference/observed data.
#' The 2nd column is the estimated/predicted data
#' @return
#'Absolute mean error (AME)
#'Square root of the mean squared error (RMSE)
#'Willmott's indices of agreemenet: original (d)
#'Modified (dmod) and refined (dref)
#'Pearson determination coefficient (R2).
#'All measures may have their corresponding confidence intervals (CIinf:CIsup) calculated.

#' @export
Accuracy=function(obs_est){
  obs_est=na.omit(obs_est)
  o=(obs_est[,1])
  p=obs_est[,2]
  plot(o,p)
  title("",xlab="Reference", ylab="Estimated")
  question=menu(c("If yes (both upper and lower tails), type 1",
                  "If no, type 2",
                  "If only from the upper tail, type 3",
                  "If only from the lower tail, type 4"),
                title="Are there suspicions values to be removed from the estimated/esticted data?")
  if (question==1){
    Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
    Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
    upremov=which(p>Uplim);lowremov=which(p<Lowlim)
    if (length(upremov)>0){ obs_est=obs_est[-c(upremov),]}
    if (length(lowremov)>0){obs_est=obs_est[-c(lowremov),]}}
  if (question==3){
    Uplim=as.numeric(readline(prompt="Insert a upper limit. Values larger than this threshold will be removed: "))
    upremov=which(p>Uplim)
    if (length(upremov)>0){ obs_est=obs_est[-c(upremov),]}}
  if (question==4){
    Lowlim=as.numeric(readline(prompt="Insert a lower limit. Values smaller than this threshold will be removed: "))
    lowremov=which(p<Lowlim)
    if (length(lowremov)>0){obs_est=obs_est[-c(lowremov),]}}
  rm(o);rm(p)
  o=obs_est[,1]
  p=obs_est[,2]
  N=length(o);N1=length(p)
  if(N!=N1){
    print("Observed/reference and estimated/estimated data must have the same length")}else{
      if(any(is.na(o))==TRUE || (any(is.na(p)==TRUE))){print("Missing data is not allowed. Please, check your input file")}else{
        databoot=matrix(NA,N,2)
        Nboots=10000
        dorigboot=matrix(NA,Nboots,1)
        drefboot=matrix(NA,Nboots,1)
        dmodboot=matrix(NA,Nboots,1)
        MAEboot=matrix(NA,Nboots,1)
        RMSEboot=matrix(NA,Nboots,1)
        RQuadboot=matrix(NA,Nboots,1)

        message("OK")
        # MAE
        MAE=sum((abs(p-o)))/N
        # RMSE
        RMSE=sqrt(sum(((p-o)^2))/N)
        # Original Willmott index
        Num.orig=sum((o-p)^2)
        Den.orig=sum((abs(p-mean(o))+abs(o-mean(o)))^2)
        dorig=1-(Num.orig/Den.orig)
        # Modified Willmott index
        Num.mod=sum(abs(o-p));
        Den.mod=sum(abs(p-mean(o))+abs(o-mean(o)))
        dmod=1-(Num.mod/Den.mod)
        # Refined Willmott's index
        if (abs(sum(p-o))<=2*sum(abs(o-mean(o)))){
          dref=1-((sum(abs(p-o)))/(2*sum(abs(o-mean(o)))))} else
          {(dref=((2*sum(abs(o-mean(o))))/sum(abs(p-o)))-1)}
        #Rquad
        RQuad=cor(o,p,method="pearson")
        complete=menu(c("If yes, type 1",
                        "If no, type 2"),
                      title="Do you want to calculate confidence intervals for the accuracy measures?")
        if (complete==1){
          question=menu(c("If 5% type 1 ", "If 10% type 2"), title="Please, select a significance level")
          if (question==1){sig.level=0.05}else{sig.level=0.10}
          message("Just a sec")
          # Bootstraping
          sig.level1=sig.level/2
          for (i in 1:Nboots){
            databoot=obs_est[sample(nrow(obs_est),replace=TRUE),]
            oboot=as.matrix(databoot[,1])
            pboot=as.matrix(databoot[,2])
            # MAE
            MAEboot[i,1]=sum((abs(pboot-oboot)))/N
            RMSEboot[i,1]=sqrt(sum(((pboot-oboot)^2))/N)
            # Original Willmott index
            Numboot=sum((oboot-pboot)^2)
            Denboot=sum((abs(pboot-mean(oboot))+abs(oboot-mean(oboot)))^2)
            dorigboot[i,1]=1-((Numboot)/Denboot)
            # Modified Willmott index
            Numboot.mod=sum(abs(oboot-pboot));
            Denboot.mod=sum(abs(pboot-mean(oboot))+abs(oboot-mean(oboot)))
            dmodboot[i,1]=1-(Numboot.mod/Denboot.mod)
            # Refined Willmott index
            if (abs(sum(pboot-oboot))<=2*sum(abs(oboot-mean(oboot)))){
              drefboot[i,1]=1-((sum(abs(pboot-oboot)))/(2*sum(abs(oboot-mean(oboot)))))} else
              {(drefboot[i,1]=((2*sum(abs(oboot-mean(oboot))))/sum(abs(pboot-oboot)))-1)}
            #Rquad
            RQuadboot=cor(oboot,pboot,method="pearson")}
          #Defining confidence intervals
          MAE_CIinf=quantile(MAEboot, probs=sig.level1, na.rm=T)
          MAE_CIsup=quantile(MAEboot, probs=(1-sig.level1), na.rm=T)
          RMSE_CIinf=quantile(RMSEboot, probs=sig.level1, na.rm=T)
          RMSE_CIsup=quantile(RMSEboot, probs=(1-sig.level1), na.rm=T)
          dorig_CIinf=quantile(dorigboot, probs=sig.level1, na.rm=T)
          dorig_CIsup=quantile(dorigboot, probs=(1-sig.level1), na.rm=T)
          dmod_CIinf=quantile(dmodboot, probs=sig.level1, na.rm=T)
          dmod_CIsup=quantile(dmodboot, probs=(1-sig.level1), na.rm=T)
          dref_CIinf=quantile(drefboot, probs=sig.level1, na.rm=T)
          dref_CIsup=quantile(drefboot, probs=(1-sig.level1), na.rm=T)
          RQuad_CIinf=quantile(RQuadboot, probs=sig.level1, na.rm=T)
          RQuad_CIsup=quantile(RQuadboot, probs=(1-sig.level1), na.rm=T)
          ModelAcuracy=cbind(MAE_CIinf,MAE,MAE_CIsup,RMSE_CIinf,RMSE,RMSE_CIsup,
                             dorig_CIinf,dorig,dorig_CIsup,
                             dmod_CIinf,dmod,dmod_CIsup,dref_CIinf,dref,dref_CIsup,
                             RQuad_CIinf,RQuad,RQuad_CIsup)
          colnames(ModelAcuracy)=c("MAECIinf","MAE","MAECIsup", "RMSECIinf","RMSE","RMSECIsup",
                                   "dorig_CIinf","dorig","dorigCIsup",
                                   "dmodCIinf","dmod","dmodCIsup","dref_CIinf","dref","dref_CIsup",
                                   "RQuad_CIinf","RQuad","RQuad_CIsup")
          row.names(ModelAcuracy)=c("")
          print(ModelAcuracy)}else{
            ModelAcuracy=cbind(MAE,RMSE,dorig,dmod,dref,RQuad)
            colnames(ModelAcuracy)=c("MAE","RMSE","dorig","dmod","dref","RQuad")
            row.names(ModelAcuracy)=c("")
            print(ModelAcuracy)
          }
        csv_fname = "ModelAcuracy.csv"
        suppressWarnings(write.table(ModelAcuracy, file = csv_fname, sep = ",",
                                     append = TRUE, quote = FALSE,
                                     col.names = TRUE, row.names = FALSE))
      }}
  plot(o,p)
  title("",xlab="Reference", ylab="Estimated")
  rm(list = ls())}
