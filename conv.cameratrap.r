####Source code for data conversion tool for camera-trapping data

####Tabulate number of events and duration of camera in action by arbitrary interval from tables of detection and occasion
#trange=c(start date, end date)
#if trange=NULL, tabulate all the input

#interval:interval for tabulation
#interval="hours" : an hour
#="days" : a day
#="months" : a month
#="years" : a year
#="fyears" : a fiscal year in Japan (April to March)
#If interval is set to a number, tabulates by a interval of it in second

#units:Unit of output time 
#units=c("secs", "mins", "hours","days", "weeks")


table.cam<-function(detection, occasion, trange=NULL, interval="months",units="mins"){
	#converting input time into class POSIXct
	starttime<-ymd2ct(occasion$startDate,occasion$startTime)
	endtime<-ymd2ct(occasion$endDate,occasion$endTime)
	noccasion<-nrow(occasion)
	#start and end time of tabulation
	if(is.null(trange)){
		mintime<-min(starttime)
		maxtime<-max(endtime)
	}else{
		mintime<-as.POSIXct(trange[[1]])
		maxtime<-as.POSIXct(trange[[2]])
	}

	#generating start and end of each interval
	if(is.numeric(interval)){
		int<-seq(mintime,maxtime,interval)
		if(max(int)!=maxtime){
			int<-c(int,as.POSIXct(maxtime))
		}
	}else if(interval=="hours"){
		int<-seq.hours(mintime,maxtime,plus=T)
	}else if(interval=="days"){
		int<-seq.days(mintime,maxtime,plus=T)
	}else if(interval=="months"){
		int<-seq.mons(mintime,maxtime,plus=T)
	}else if(interval=="years"){
		int<-seq.yrs(mintime,maxtime,plus=T)
	}else if(interval=="fyears"){
		int<-seq.fyrs(mintime,maxtime,plus=T)
	}else{
		stop("Invalid interval!")
	}
	
	#tabulate duration of camera in action
	int1<-int[-length(int)]
	int2<-int[-c(1)]
	nint<-length(int1)
	effort0<-NULL
	
	for(i in 1:nint){
		temp<-intoverlap(int1[i],int2[i],starttime,endtime,units=units)
		mergemat<-data.frame(institutionCode=occasion$institutionCode,collectionCode=occasion$collectionCode,locationID=occasion$locationID,
						begin=rep(int1[i],noccasion),end=rep(int2[i],noccasion),time=temp)
		effort0<-rbind(effort0,mergemat)
	}
	effortagg<-aggregate(list(time=effort0[,c("time")]),effort0[,c("institutionCode","collectionCode","locationID","begin","end")],sum)
	effortagg<-effortagg[order(effortagg$institutionCode,effortagg$collectionCode,effortagg$locationID,effortagg$begin),]

	#tabulate number of events within each intervals
	objlist<-unique(detection$object)
	nobj<-length(objlist)
	eventtime<-vec2ct(detection$y,detection$m,detection$d,detection$hm)
	eventtag<-inttag(eventtime,int1,int2)
	detectiontag<-cbind(eventtag,detection,nevents=rep(1,nrow(eventtag)))
	detectionsp<-reshape(detectiontag,timevar="object",idvar=colnames(detectiontag)[!is.element(colnames(detectiontag),c("object","individualCount","nevents"))],direction="wide")
	detectionsp2<-detectionsp[!is.na(detectionsp$begin),]
	eventagg<-aggregate(detectionsp2[,grep("(^individualCount\\.|^nevents\\.)",colnames(detectionsp2))],detectionsp2[,c("begin","end","institutionCode","collectionCode","locationID")],sum,na.rm=T)
	tempmat<-data.frame(matrix(0,nrow=nrow(effortagg),ncol=length(grep("(^individualCount\\.|^nevents\\.)",colnames(eventagg)))))
	rownames(tempmat)<-apply(effortagg[,c("institutionCode","collectionCode","locationID","begin","end")],1,paste,collapse="_")
	colnames(tempmat)<-colnames(eventagg)[grep("(^individualCount\\.|^nevents\\.)",colnames(eventagg))]
	pointer<-apply(eventagg[,c("institutionCode","collectionCode","locationID","begin","end")],1,paste,collapse="_")
	is.point<-is.element(pointer,rownames(tempmat))
	tempmat[pointer[is.point],]<-eventagg[is.point,colnames(tempmat)]

	res<-cbind(effortagg,tempmat)
	return(res)
}


####Supplementary R functions
#Converting characters of date (ymd="yyyy/mm/dd") and time (hms="hh:mm:ss") 
#at arbitrary time zone (tz) into class "POSIXlt"
ymd2lt<-function(ymd,hms="00:00:00",tz="JST"){
	cymd<-as.character(ymd)
	chms<-as.character(hms)
	chms<-ifelse(is.na(chms),rep("00:00:00",length(chms)),chms)
	chms<-ifelse(chms=="",rep("00:00:00",length(chms)),chms)
	timechar<-paste(cymd,chms,tz,sep=" ")
	res<-as.POSIXlt(timechar)
	return(res)
}

#Converting characters of date (ymd="yyyy/mm/dd") and time (hms="hh:mm:ss") 
#at arbitrary time zone (tz) into class "POSIXct"
ymd2ct<-function(ymd,hms="00:00:00",tz="JST"){
	cymd<-as.character(ymd)
	chms<-as.character(hms)
	chms<-ifelse(is.na(chms),rep("00:00:00",length(chms)),chms)
	chms<-ifelse(chms=="",rep("00:00:00",length(chms)),chms)
	timechar<-paste(cymd,chms,tz,sep=" ")
	res<-as.POSIXct(timechar)
	return(res)
}

#Converting integer value of year (yrs), month (mons), date (days) and time (time="hh:mm:ss") 
#at arbitrary time zone (tz) into class "POSIXlt"
vec2lt<-function(yrs,mons=0,days=0,time="00:00:00",tz="JST"){
	timechar<-paste(paste(as.character(yrs),as.character(mons),as.character(days),sep="/"),as.character(time),tz,sep=" ")
	res<-as.POSIXlt(timechar)
	return(res)
}

#Converting integer value of year (yrs), month (mons), date (days) and time (time="hh:mm:ss") 
#at arbitrary time zone (tz) into class "POSIXct"
vec2ct<-function(yrs,mons=0,days=0,time="00:00:00",tz="JST"){
	timechar<-paste(paste(as.character(yrs),as.character(mons),as.character(days),sep="/"),as.character(time),tz,sep=" ")
	res<-as.POSIXct(timechar)
	return(res)
}




#extract year(s) from Date-Time class ("POSIXlt" and "POSIXct")
yrs<-function(x){
	res<-as.numeric(substr(as.character(x),1,4))
	return(res)
}

#extract month(s) from Date-Time class ("POSIXlt" and "POSIXct")
mons<-function(x){
	res<-as.numeric(substr(as.character(x),6,7))
	return(res)
}

#extract day(s) from Date-Time class ("POSIXlt" and "POSIXct")
days<-function(x){
	res<-as.numeric(substr(as.character(x),9,10))
	return(res)
}

#extract hour(s) from Date-Time class ("POSIXlt" and "POSIXct")
hours<-function(x){
	res<-as.numeric(substr(as.character(x),12,13))
	return(res)
}

#extract minute(s) from Date-Time class ("POSIXlt" and "POSIXct")
mins<-function(x){
	res<-as.numeric(substr(as.character(x),15,16))
	return(res)
}

#extract second(s) from Date-Time class ("POSIXlt" and "POSIXct")
secs<-function(x){
	res<-as.numeric(substr(as.character(x),18,19))
	return(res)
}


#add years to Date-Time class ("POSIXct")
sumyear<-function(x,y){
	xyrs<-yrs(x)
	xmons<-mons(x)
	xdays<-days(x)
	xhours<-hours(x)
	xmins<-mins(x)
	xsecs<-secs(x)

	xyrs<-xyrs+y
	res<-as.POSIXct(paste(paste(xyrs,xmons,xdays,sep="/"),paste(xhours,xmins,xsecs,sep=":")))
	return(res)
}

#Generating sequencial values of Date-Time class ("POSIXct") by an hour interval
seq.hours<-function(start,end,plus=F){
	start.hours<-as.POSIXct(cut(start,"hours"))
	if(plus==T){
		end.hours<-as.POSIXct(cut(as.POSIXct(cut(end,"hours"))+3650,"hours"))
	}else{
		end.hours<-as.POSIXct(cut(end,"hours"))
	}
	res<-start.hours
	i<-1
	repeat{
		temp<-res[i]+3600
		temp2<-as.POSIXct(cut(temp,"hours"))
		if(end.hours-temp2<0){
			break
		}
		res<-c(res,temp2)
		i<-i+1
	}
	return(res)
}

#Generating sequencial values of Date-Time class ("POSIXct") by a day interval
seq.days<-function(start,end,plus=F){
	start.days<-as.POSIXct(cut(start,"days"))
	if(plus==T){
		end.days<-as.POSIXct(cut(as.POSIXct(cut(end,"days"))+3600*25,"days"))
	}else{
		end.days<-as.POSIXct(cut(end,"days"))
	}
	res<-start.days
	i<-1
	repeat{
		temp<-res[i]+3600*24
		temp2<-as.POSIXct(cut(temp,"days"))
		if(end.days-temp2<0){
			break
		}
		res<-c(res,temp2)
		i<-i+1
	}
	return(res)
}

#Generating sequencial values of Date-Time class ("POSIXct") by a month interval
seq.mons<-function(start,end,plus=F){
	start.mons<-as.POSIXct(cut(start,"months"))
	if(plus==T){
		end.mons<-as.POSIXct(cut(as.POSIXct(cut(end,"months"))+3600*24*32,"months"))
	}else{
		end.mons<-as.POSIXct(cut(end,"months"))
	}
	res<-start.mons
	i<-1
	repeat{
		temp<-res[i]+3600*24*32
		temp2<-as.POSIXct(cut(temp,"months"))
		if(end.mons-temp2<0){
			break
		}
		res<-c(res,temp2)
		i<-i+1
	}
	return(res)
}

#Generating sequencial values of Date-Time class ("POSIXct") by a year interval
seq.yrs<-function(start,end,plus=F){
	start.yrs<-as.POSIXct(cut(start,"years"))
	if(plus==T){
		end.yrs<-sumyear(as.POSIXct(cut(end,"years")),1)
	}else{
		end.yrs<-as.POSIXct(cut(end,"years"))
	}
	res<-start.yrs
	i<-1
	repeat{
		temp<-res[i]+3600*24*370
		temp2<-as.POSIXct(cut(temp,"years"))
		if(end.yrs-temp2<0){
			break
		}
		res<-c(res,temp2)
		i<-i+1
	}
	return(res)
}



#Generating sequencial values of Date-Time class ("POSIXct") by a interval of fiscal year in Japan (April to March)
seq.fyrs<-function(start,end,plus=F){
	if(mons(start)<4){
		start.fyrs<-as.POSIXct(cut(as.POSIXct(cut(start,"years"))+3600*24*100-3600*24*365,"months"))
	}else{
		start.fyrs<-as.POSIXct(cut(as.POSIXct(cut(start,"years"))+3600*24*100,"months"))
	}

	if((mons(end)<4)&(plus==F)){
		end.fyrs<-as.POSIXct(cut(as.POSIXct(cut(end,"years"))+3600*24*100-3600*24*365,"months"))
	}else if((mons(end)>=4)&(plus==T)){
		end.fyrs<-as.POSIXct(cut(as.POSIXct(cut(end,"years"))+3600*24*100+3600*24*365,"months"))
	}else{
		end.fyrs<-as.POSIXct(cut(as.POSIXct(cut(end,"years"))+3600*24*100,"months"))
	}

	res<-start.fyrs
	i<-1
	repeat{
		temp<-res[i]+3600*24*370
		temp2<-as.POSIXct(cut(temp,"months"))
		if(end.fyrs-temp2<0){
			break
		}
		res<-c(res,temp2)
		i<-i+1
	}
	return(res)
}

#Calculating overlapping time length between two time intervals
intoverlap<-function(start1,end1,start2,end2,units="mins"){
	startct1<-as.POSIXct(start1)
	startct2<-as.POSIXct(start2)
	endct1<-as.POSIXct(end1)
	endct2<-as.POSIXct(end2)
	ostart<-pmax(startct1,startct2)
	oend<-pmin(endct1,endct2)
	res<-as.numeric(difftime(oend,ostart,units=units))
	res<-ifelse(res<0,0,res)
	return(res)
}

#Counting number of events within time interval(s)
intcount<-function(time,event,num,start,end){
	startct<-as.POSIXct(start)
	endct<-as.POSIXct(end)
	timect<-as.POSIXct(time)
	isint<-(timect>=startct)&(timect<endct)
	nevent<-table(event[isint])
	nind<-tapply(num[isint],list(event[isint]),sum)
	nind<-ifelse(is.na(nind),0,nind)
	res<-list(nevent=nevent,nind=nind)
	return(res)
}

#Matching events with overlapping time intervals 
inttag<-function(time,start,end){
	startct<-as.POSIXct(start)
	endct<-as.POSIXct(end)
	timect<-as.POSIXct(time)
	ntime<-length(time)
	nint<-length(start)
	res<-data.frame(eventtime=timect,begin=as.POSIXct(rep(NA,ntime)),end=as.POSIXct(rep(NA,ntime)))
	for(i in 1:nint){
		isint<-(timect>=startct[i])&(timect<endct[i])
		res$begin[isint]<-startct[i]
		res$end[isint]<-endct[i]
	}
	return(res)
}
