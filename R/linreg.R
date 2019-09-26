#' Linreg 
#' 
#' Given \code{formula} and data source \code{data}, find the best fitted linear regression model.
#' 
#' @param formula, a suggested linear regression model
#' @param data, a data source used to train the model
#' @return a list of key results from regression, including Coefficients,FittedValues,Residuals,FreedomDegree,VarianceResiduals,StdErrorResiduals,VarianceCoefficients,StdError,Tcoefficients,Possibility,X,Y,Formula,Data,VariableNames
#' @examples
#' data(iris)
#' a<-linreg$new(formula=Sepal.Length~Sepal.Width,data=iris)
#' a$print()          
#' a$plot()

linreg<-setRefClass("linreg",
  fields=list(formula="formula",
              data="data.frame",
              qrde="logical",
              guo="list"),
  
  
  methods=list(
    
    #Predefined methods
    initialize=function(formula="formula",data="data.frame",qrde="logical") {
       if(!class(formula)=="formula") stop("The formula should be a formula object!")
       if(!is.data.frame(data)) stop("The data should be a data frame object!")
      
       
       library(ggplot2)
       library(png)
       library(grid)
       ##install.packages("gridExtra")
     
       va=all.vars(formula)          #15.Variables y,x1,x2...
       x=model.matrix(formula,data)  #11.Model.Matrix
       y=data[[va[1]]]               #12.DataY
       tem=as.character(formula)
       gs=paste(tem[2],tem[1],tem[3],sep="") #13.Formula in character
       shu=as.data.frame(cbind(x,y))  ## 14. Trainning Data prepared
       QR=qr(x)                      #QR decomposition
       
       if(qrde==FALSE) co=solve(t(x)%*%x,t(x)%*%y) 
       else co=solve.qr(QR,y)          #1.Estimated Coefficients
       
       fi=x%*%co                                  #2.Fitted values
       re=y-fi                                    #3.Residuals
       df=length(y)-(length(all.vars(formula))-1) #4.Freedom degree
       red=t(re)%*%re/df                          #5.Variance of residuals
       resd=sqrt(red)                             #6.StStandard Variance of residuals
              
       if(qrde==FALSE) cod=rep(red,length(co))*diag(solve(t(x)%*%x))
       else cod=rep(red,length(co))*diag(chol2inv(qr.R(QR)))      #7.Variance of coefficients
       
       cosd=sqrt(cod)              #8.Standard Variance of coefficients 
       cot=co/sqrt(cod)           #9.t-values of coefficients
       tpv=1-pt(cot,df)            #10.Possibility of testing wrong
       
       guo<<-list(Coefficients=co,FittedValues=fi,Residuals=re,FreedomDegree=df,VarianceResiduals=red,StdErrorResiduals=resd,VarianceCoefficients=cod,StdError=cosd,Tcoefficients=cot,Possibility=tpv,X=x,Y=y,Formula=gs,Data=shu,VariableNames=va)
       return (guo)},
  
      
    #Required methods
    print=function() {
       te<-names(guo$Coefficients)
       cat(paste("linreg(formula=",guo$Formula,",data=iris)",sep=""))
       cat("\n")
       for(i in 1:length(te)) cat(sprintf("%*s",20,te[i]))
       cat("\n")
       for(i in 1:length(te)) cat(sprintf("%*s",20,guo$Coefficients[i]))},
    
    plot=function(){
      if(length(guo$VariableNames)>2) stop("Too many independent variables!")
      
      #get_png<-function(filename) grid::rasterGrob(png::readPNG(filename),interpolate=TRUE)
      g<-readPNG("liu.png")
      tu<-rasterGrob(g,interpolate=TRUE)
      t <- grid::roundrectGrob()  ##Draw a frame
      
      g1<-ggplot(guo$Data)+
         geom_point(aes(x=guo$FittedValues,y=guo$Residuals),shape=1)+
         xlab(paste("Fitted values\n",guo$Formula,sep=""))+
         ylab("Residuals")+
         ggtitle("Residuals VS Fitted")+
         theme(plot.title = element_text(hjust = 0.5))+
         stat_smooth(aes(x=guo$FittedValues,y=guo$Residuals),method="lm",col="red")+
         annotation_custom(tu,xmin=5.49,xmax=5.67,ymin=-2.6,ymax=-2)+
         coord_cartesian(clip="off")+
         theme(plot.background=element_rect(fill="#00b9e7",colour="#00b9e7"),plot.margin=margin(5,5,30,5))
      
      rest<-sqrt(abs(guo$Residuals/sd(guo$Residuals)))
      g2<-ggplot()+
        geom_point(aes(x=guo$FittedValues,y=rest),shape=1)+
        xlab(paste("Fitted values\n",guo$Formula,sep=""))+
        ylab(expression(sqrt(abs(StandardizedResiduals))))+
        ggtitle("Scale-Location")+
        theme(plot.title = element_text(hjust = 0.5))+
        stat_smooth(aes(x=guo$FittedValues,y=rest),method = "lm", col = "red")+
        annotation_custom(tu,xmin=5.5,xmax=5.65,ymin=-0.35,ymax=0.05)+
        coord_cartesian(clip="off")+
        theme(plot.background = element_rect(fill="#00b9e7",colour="#00b9e7"),plot.margin=margin(5,5,30,5))
      
        ##readline(prompt="Press any key to continue")
        list(Residuals_VS_Fitted=g1,Scale_Location=g2)},
    
    resid=function() return(as.vector(guo$Residuals)),
    pred=function() return(as.vector(guo$FittedValues)),
    coef=function() {te<-as.vector(guo$Coefficients)
                     ge<-dimnames(guo$Coefficients)[[1]]
                     names(te)<-ge
                     return(te)},
    
    summary=function(){
      colna<-c("Coefficients","StdError","Tcoefficients","Possibility")
      cat("Summary:\n");cat(sprintf("%*s",20,""))
      for(i in 1:4) cat(sprintf("%*s",20,colna[i]))
      cat("\n")
      for(i in 1:length(guo$Coefficients)) 
       {cat(sprintf("%*s",20,dimnames(guo$Coefficients)[[1]][i]))
        for(j in 1:4){
          cat(sprintf("%*s",20,guo[[colna[j]]][i]))
        }
        cat("\n")}
      cat(paste("\nThe standard error of residuals is ",guo$StdErrorResiduals[1],sep=""))
      cat(paste("\nThe freedom degree is ",guo$FreedomDegree[1],sep=""))}
  ))
