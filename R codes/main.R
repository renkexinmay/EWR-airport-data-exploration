#Part 1 import GDP data
TMI = read.csv("advisory.csv")
focus = which(TMI$AdvisoryType == "GDP"| TMI$AdvisoryType == "GDP CNX")
GDP = TMI[focus,]
focus = which(GDP$ControlElement == "EWR/ZNY")
EWR_GDP = GDP[focus,]
write.csv(x = EWR_GDP, file="EWR_GDP.csv")

# Part 2 Change timezone for GDP times (GMT->NY)
EWR_GDP$SendDate.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$SendDate.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Derived.BgnDate.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$Derived.BgnDate.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Derived.EndDate.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$Derived.EndDate.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Eff.Bgn.Date.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$Eff.Bgn.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Eff.End.Date.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$Eff.End.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$GDP.Bgn.Date.Time.UTC= as.POSIXct(strptime(as.character(EWR_GDP$GDP.Bgn.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$GDP.End.Date.Time.UTC= as.POSIXct(strptime(as.character(EWR_GDP$GDP.End.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$GDPX.Bgn.Date.Time.UTC= as.POSIXct(strptime(as.character(EWR_GDP$GDPX.Bgn.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$GDPX.End.Date.Time.UTC= as.POSIXct(strptime(as.character(EWR_GDP$GDPX.End.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$RootAdvisoryDate.UTC=as.POSIXct(strptime(as.character(EWR_GDP$RootAdvisoryDate.UTC),"%Y-%m-%d",tz="GMT"))

attr(EWR_GDP$SendDate.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Derived.BgnDate.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Derived.EndDate.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Eff.Bgn.Date.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Eff.End.Date.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$GDP.Bgn.Date.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$GDP.End.Date.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$GDPX.Bgn.Date.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$GDPX.End.Date.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$RootAdvisoryDate.UTC, "tzone") <- "America/New_York"

write.csv(x = EWR_GDP, file="EWR_GDP2.csv")

# Part 3 Duration variables
N=nrow(EWR_GDP)

# duration of each single advisory
EWR_GDP$Duration_Advisory = difftime(EWR_GDP$Derived.EndDate.Time.UTC, EWR_GDP$Derived.BgnDate.Time.UTC, units = "hours")  

# duration of the GDP plan when a new advisory is issued
EWR_GDP$RootBgnTime= EWR_GDP$Derived.BgnDate.Time.UTC
for (i in 1:N){
  if (EWR_GDP$Is.RootAdvisory[i]== "Yes") {
    j=i
    root= as.numeric(EWR_GDP$RootAdvisoryNumber[j])
    roottime= EWR_GDP$RootAdvisoryDate.UTC[j]
  }
  EWR_GDP$RootBgnTime[as.numeric(EWR_GDP$RootAdvisoryNumber)== root  & EWR_GDP$RootAdvisoryDate.UTC== roottime]= EWR_GDP$Derived.BgnDate.Time.UTC[j]
}

EWR_GDP$Duration_Initiative[as.character(EWR_GDP$AdvisoryType)== "GDP"]= difftime(EWR_GDP$Derived.EndDate.Time.UTC[as.character(EWR_GDP$AdvisoryType)== "GDP"], EWR_GDP$RootBgnTime[as.character(EWR_GDP$AdvisoryType)== "GDP"], units = "hours")
EWR_GDP$Duration_Initiative[as.character(EWR_GDP$AdvisoryType)== "GDP CNX"]= difftime(EWR_GDP$Derived.BgnDate.Time.UTC[as.character(EWR_GDP$AdvisoryType)== "GDP CNX"], EWR_GDP$RootBgnTime[as.character(EWR_GDP$AdvisoryType)== "GDP CNX"], units = "hours")

# Actual duration of the GDP plan when no more new advisory will be issued
for (i in 1:N){
  if (EWR_GDP$Is.RootAdvisory[i]== "Yes") {
    root= as.numeric(EWR_GDP$RootAdvisoryNumber[i])
    roottime= EWR_GDP$RootAdvisoryDate.UTC[i]
  }
  last = max(which(as.numeric(EWR_GDP$RootAdvisoryNumber)== root  & EWR_GDP$RootAdvisoryDate.UTC== roottime))
  EWR_GDP$Duration_Actual[as.numeric(EWR_GDP$RootAdvisoryNumber)== root  & EWR_GDP$RootAdvisoryDate.UTC== roottime]=EWR_GDP$Duration_Initiative[last]
}

write.csv(x = EWR_GDP, file="EWR_GDP3.csv")

# Part 4 Number of Modifications of GDP plan including & excluding GDP CNX
for (i in 1:N){
  if (EWR_GDP$Is.RootAdvisory[i]== "Yes") {
    root= as.numeric(EWR_GDP$RootAdvisoryNumber[i])
    roottime= EWR_GDP$RootAdvisoryDate.UTC[i]
  }
  EWR_GDP$Number_Revisions_NoCNX[as.numeric(EWR_GDP$RootAdvisoryNumber)== root  & EWR_GDP$RootAdvisoryDate.UTC== roottime] = nrow(EWR_GDP[which(as.character(EWR_GDP$AdvisoryType)== "GDP"& as.numeric(EWR_GDP$RootAdvisoryNumber)== root  & EWR_GDP$RootAdvisoryDate.UTC== roottime),])-1
  EWR_GDP$Number_TotModif_incldCNX[as.numeric(EWR_GDP$RootAdvisoryNumber)== root  & EWR_GDP$RootAdvisoryDate.UTC== roottime] = nrow(EWR_GDP[which(as.numeric(EWR_GDP$RootAdvisoryNumber)== root  & EWR_GDP$RootAdvisoryDate.UTC== roottime),])-1
}

write.csv(x = EWR_GDP, file="EWR_GDP4.csv")

# Part 5 Early Cancel, Lead Time
#early cancel time
for (i in 1:N){
  if (as.character(EWR_GDP$AdvisoryType[i])== "GDP CNX") {
    beforeCNX=max(which(as.character(EWR_GDP$AdvisoryType)== "GDP" & as.numeric(EWR_GDP$RootAdvisoryNumber)== as.numeric(EWR_GDP$RootAdvisoryNumber[i])  & EWR_GDP$RootAdvisoryDate.UTC== EWR_GDP$RootAdvisoryDate.UTC[i]))
    
    EWR_GDP$EarlyCancelTime[as.numeric(EWR_GDP$RootAdvisoryNumber)== as.numeric(EWR_GDP$RootAdvisoryNumber[i])  & EWR_GDP$RootAdvisoryDate.UTC== EWR_GDP$RootAdvisoryDate.UTC[i]]= EWR_GDP$Duration_Initiative[beforeCNX]-EWR_GDP$Duration_Initiative[i]
  }
}

#lead time
EWR_GDP$LeadTime = difftime(EWR_GDP$Derived.BgnDate.Time.UTC, EWR_GDP$SendDate.Time.UTC, units = "hours")

write.csv(x = EWR_GDP, file="EWR_GDP5.csv")

# Part 6 time of the day, day of the week, month-day of the year
#send time of the day
EWR_GDP$SendTime= format(EWR_GDP$SendDate.Time.UTC, "%H:%M")
#begin time of the day
EWR_GDP$BgnTime= format(EWR_GDP$Derived.BgnDate.Time.UTC, "%H:%M")
#end time of the day
EWR_GDP$EndTime= format(EWR_GDP$Derived.EndDate.Time.UTC, "%H:%M")
#GDP month-day of the year
EWR_GDP$BgnDay=format(EWR_GDP$Derived.BgnDate.Time.UTC, "%m-%d")
#GDP day of the week
EWR_GDP$BgnWeekday=weekdays(EWR_GDP$Derived.BgnDate.Time.UTC, abbreviate = FALSE)

write.csv(x = EWR_GDP, file="EWR_GDP6.csv")

#Part 7 match TAF data (hourly)

TAF4 = read.csv("TAF-20140831-nonduplicated-24h-RNSNIC.csv",  as.is=TRUE)
TAF4$issue.time= as.POSIXct(TAF4$issue.time, tz =  "America/New_York")
TAF4$start.time= as.POSIXct(TAF4$start.time, tz =  "America/New_York")
TAF4$end.time= as.POSIXct(TAF4$end.time, tz =  "America/New_York")

EWR_GDP$IMC <- ""
EWR_GDP$MCIC <- ""
EWR_GDP$Visibility <- ""
EWR_GDP$Ceiling <- ""
EWR_GDP$TS <- ""
EWR_GDP$CW0422<- ""
EWR_GDP$CW1129<- ""
EWR_GDP$PC<- ""
EWR_GDP$RNSNIC<- ""

for ( i in 1:N) {
  
  TAFGDP1=TAF4[TAF4$issue.time < EWR_GDP$SendDate.Time.UTC[i] & TAF4$end.time > EWR_GDP$Derived.BgnDate.Time.UTC[i] & TAF4$start.time< EWR_GDP$Derived.EndDate.Time.UTC[i],]
  
  H = ceiling(as.numeric(EWR_GDP$Duration_Advisory[i]))
  
  for (h in 1:H) {
    
    TAFGDP2=TAFGDP1[TAFGDP1$start.time<EWR_GDP$Derived.BgnDate.Time.UTC[i]+h*60*60 & TAFGDP1$end.time> EWR_GDP$Derived.BgnDate.Time.UTC[i]+(h-1)*60*60,]
    
    TAFGDP3 = TAFGDP2[order(TAFGDP2$issue.time,decreasing = TRUE)[1],]
    
    EWR_GDP$IMC[i] = paste(EWR_GDP$IMC[i], TAFGDP3$IMC, sep = " ")
    EWR_GDP$MCIC[i] = paste(EWR_GDP$MCIC[i], TAFGDP3$MCIC, sep = " ")
    EWR_GDP$Visibility[i] = paste(EWR_GDP$Visibility[i], TAFGDP3$Visibility, sep = " ")
    EWR_GDP$Ceiling[i] = paste(EWR_GDP$Ceiling[i], TAFGDP3$Ceiling, sep = " ")
    EWR_GDP$TS[i] = paste(EWR_GDP$TS[i], TAFGDP3$TS, sep = " ")
    EWR_GDP$CW0422[i] = paste(EWR_GDP$CW0422[i], TAFGDP3$CW0422, sep = " ")
    EWR_GDP$CW1129[i] = paste(EWR_GDP$CW1129[i], TAFGDP3$CW1129, sep = " ")
    EWR_GDP$PC[i] = paste(EWR_GDP$PC[i], TAFGDP3$PC, sep = " ")
    EWR_GDP$RNSNIC[i] = paste(EWR_GDP$RNSNIC[i], TAFGDP3$RNSNIC, sep = " ")
  }
}
write.csv(x = EWR_GDP, file="EWR_GDP7【TAF】.csv")

#Part 8 Convert program rate to hourly vector
EWR_GDP$ProgramRate = as.character(EWR_GDP$ProgramRate)
EWR_GDP$ProgramRateVct= EWR_GDP$ProgramRate

for (i in 1:N) {
  if (EWR_GDP$AdvisoryType[i] == "GDP" & !grepl("/",EWR_GDP$ProgramRate[i])) {
    DurationRoundUp = ceiling(as.numeric(EWR_GDP$Duration_Advisory[i]))
    
    EWR_GDP$ProgramRateVct[i]=paste(replicate(DurationRoundUp, EWR_GDP$ProgramRate[i]), collapse = "/")
  }
}

write.csv(x = EWR_GDP, file="EWR_GDP8.csv")
#Part 9 Type of Dep Scope 
EWR_GDP$Dep.Scope = as.character(EWR_GDP$Dep.Scope)
EWR_GDP$DepScopeType = "ARTCC"
EWR_GDP$DepScopeType[grepl("0", EWR_GDP$Dep.Scope) | grepl("5", EWR_GDP$Dep.Scope)]= "Radius"
EWR_GDP$DepScopeType[grepl("-", EWR_GDP$Dep.Scope)]= "-"

write.csv(x = EWR_GDP, file="EWR_GDP9.csv")

# Part 10 preprocess IF 

EWRIF = read.csv("EWR_IF.csv")
EWRIF$ARR_YYYY = as.character(substr(EWRIF$ARR_YYYYMM,1,4))
EWRIF$ARR_MM = as.character(substr(EWRIF$ARR_YYYYMM,5,6))

EWRIF$SchIn = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), as.character (EWRIF$SCHINTM)))
EWRIF$SchIn = strptime(EWRIF$SchIn, "%Y %m %d %H:%M", tz ="America/New_York")

#flight plan time?
EWRIF$EDCTONTM1=as.character(EWRIF$EDCTONTM)
EWRIF$EDCTOn = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), EWRIF$EDCTONTM1))
EWRIF$EDCTOn = strptime(EWRIF$EDCTOn, "%Y %m %d %H:%M", tz ="America/New_York")

DEPARP = read.csv("DEPARP.csv")
DEPARP$ARTCC=as.character(DEPARP$ARTCC)
DEPARP$Country=as.character(DEPARP$Country)
DEPARP$Mile=as.character(DEPARP$Mile)

DEPARP$LocationID=as.character(DEPARP$LocationID)
EWRIF$DEP_LOCID=as.character(EWRIF$DEP_LOCID)

for (i in 1:nrow(DEPARP)) {
  EWRIF$ARTCC[EWRIF$DEP_LOCID %in% DEPARP$LocationID[i]] = DEPARP$ARTCC [i]
  EWRIF$Country[EWRIF$DEP_LOCID %in% DEPARP$LocationID[i]] = DEPARP$Country[i]
  EWRIF$Mile[EWRIF$DEP_LOCID %in% DEPARP$LocationID[i]] = DEPARP$Mile[i]
}

write.csv(x = EWRIF, file="EWRIF-fixed.csv")

# Part 11 Match IF and TMI (suggest to run this part separately with following parts)
EWR_GDP$Exempt.Dep.Facilities = as.character(EWR_GDP$Exempt.Dep.Facilities)
EWR_GDP$AdvisoryType =as.character(EWR_GDP$AdvisoryType)
EWR_GDP1=EWR_GDP[difftime(as.Date(EWR_GDP$AdvisoryDate.UTC), as.Date('2014-09-01 '))<0,]
EWR_GDP1$Dep.Scope = as.character(EWR_GDP1$Dep.Scope)

EWR_GDP1$SchArrSchIn = 0
EWR_GDP1$ImpArrSchIn = 0

EWRIF$Mile = as.numeric(EWRIF$Mile)
EWRIF$DEP_LOCID=as.character(EWRIF$DEP_LOCID)

IFCA= EWRIF[EWRIF$Country == "CA",]
IFUS= EWRIF[EWRIF$Country == "US",]
IFINT= EWRIF[EWRIF$Country == "INT",]

for (i in 1:nrow(EWR_GDP1)) {
  if (EWR_GDP1$AdvisoryType[i] == "GDP") {
    #scheduled arrivals
    EWR_GDP1$SchArrSchIn[i] = length(which( EWRIF$SchIn >=EWR_GDP1$Derived.BgnDate.Time.UTC[i] & EWRIF$SchIn <= EWR_GDP1$Derived.EndDate.Time.UTC[i]))
    
    #EWR_GDP1$SchArrEDCT[i] = nrow(EWRIF[EWRIF$EDCTOn >EWR_GDP1$Derived.BgnDate.Time.UTC[i] & EWRIF$EDCTOn < EWR_GDP1$Derived.EndDate.Time.UTC[i],])
    
    #Impacted arrivals
    ##CA flights
    ###Spatial
    rows<-sapply(IFCA$DEP_LOCID, function(x) grepl(x, EWR_GDP1$Canadian.Dep.Arpts.Included[i]))
    IFGDPCA = IFCA[rows,]
    ###Temporal
    CA = length(which(IFGDPCA$SchIn>=EWR_GDP1$Derived.BgnDate.Time.UTC[i] & IFGDPCA$SchIn <=EWR_GDP1$Derived.EndDate.Time.UTC[i]))
    #CA1 = nrow(IFGDPCA[IFGDPCA$EDCTOn>EWR_GDP1$Derived.BgnDate.Time.UTC[i] & IFGDPCA$EDCTOn < EWR_GDP1$Derived.EndDate.Time.UTC[i],])
    ##US flights
    ### tick out exempted flights
    if (EWR_GDP1$Exempt.Dep.Facilities[i]!= "-") { 
      rows<-sapply(IFUS$DEP_LOCID, function(x) !grepl(x, EWR_GDP1$Exempt.Dep.Facilities[i])) 
      IFUS1 = IFUS[rows,]
      rows<- sapply(IFUS1$ARTCC, function(x) !grepl(x, EWR_GDP1$Exempt.Dep.Facilities[i]) )
      IFGDPUS = IFUS1[rows,]
    } else{
      IFGDPUS= IFUS
    }
    ### spatial scope
    if (!grepl("ALL",EWR_GDP1$Dep.Scope[i]) ){
      if(EWR_GDP1$DepScopeType[i] == "ARTCC")
      {
        IFGDPUS1=IFGDPUS[sapply(IFGDPUS$ARTCC, function(x) grepl(x, EWR_GDP1$Dep.Scope[i] )),]
      }
      if(EWR_GDP1$DepScopeType[i] == "Radius")
      {
        IFGDPUS1 = IFGDPUS[IFGDPUS$Mile <= as.numeric(EWR_GDP1$Dep.Scope[i]),]
      }
    }else {
      IFGDPUS1= IFGDPUS}
    ### temporal scope
    US= length(which(IFGDPUS1$SchIn>=EWR_GDP1$Derived.BgnDate.Time.UTC[i] &  IFGDPUS1$SchIn<=EWR_GDP1$Derived.EndDate.Time.UTC[i]))
    #US1= nrow(IFGDPUS1[IFGDPUS1$EDCTOn >EWR_GDP1$Derived.BgnDate.Time.UTC[i] &  IFGDPUS1$EDCTOn <EWR_GDP1$Derived.EndDate.Time.UTC[i],])
    
    ##Int Flights
    #INT= length(which(IFINT$SchIn>=EWR_GDP1$Derived.BgnDate.Time.UTC[i] &  #IFINT$SchIn<=EWR_GDP1$Derived.EndDate.Time.UTC[i]))
    #INT1= nrow(IFINT[IFINT$EDCTOn >EWR_GDP1$Derived.BgnDate.Time.UTC[i] &  #IFINT$EDCTOn<EWR_GDP1$Derived.EndDate.Time.UTC[i],])
    
    ##Flight No.
    EWR_GDP1$ImpArrSchIn[i] = as.numeric(CA)+ as.numeric(US) 
    #EWR_GDP1$ImpArrEDCT[i] = as.numeric(CA1)+ as.numeric(US1) 
  } 
}

write.csv(x = EWR_GDP1, file="EWR_GDP11.csv")



若exampt不是-，建立IF子集，删掉airport name/artcc在exampt中grepl的
建立子集1，CA航班；US航班子集2；INT航班为子集3
对于子集1，判断机场在不在canadian之内，统计航班数
对于子集2，建立子集2-1， 删掉所有非-的exampt中包含的航班
判断dep scope类型
若是radius，比较该航班出发机场的Dist_mile和as.numeric(Dep.Scope)。+1
若是artcc，比较该航班出发机场的artcc是否在as.character(Dep.Scope)中grepl +1
对于子集3 ，统计航班数

# Part 12 change format of some variable, preparing for dimensionality reduction
# Open “9.RDat”

EWR_GDP_IF = read.csv("EWR_GDP11【IF】.csv")
EWR_GDP_IF$SchArrSchIn =as.numeric(as.character(EWR_GDP_IF$SchArrSchIn))
EWR_GDP_IF$ImpArrSchIn= as.numeric(as.character(EWR_GDP_IF$ImpArrSchIn))


EWR_GDP1=EWR_GDP[difftime(as.Date(EWR_GDP$AdvisoryDate.UTC), as.Date('2014-09-01 '))<0,]
EWR_GDP1$SchArrSchIn = EWR_GDP_IF$SchArrSchIn
EWR_GDP1$ImpArrSchIn = EWR_GDP_IF$ImpArrSchIn

##generate month
library(lubridate)
EWR_GDP1$Month = month(EWR_GDP1$Derived.BgnDate.Time.UTC)
EWR_GDP1$Month=as.numeric(as.character(EWR_GDP1$Month))

##generate time of the day
time1 = hm("0,0")
time2 = hm("6,0")
time3 = hm("9,0")
time4 = hm("12,0")
time5 = hm("15,0")
time6 = hm("18,0")
time7 = hm("21,0")
time8 = hm("24,0")

EWR_GDP1$SendTime=hm(EWR_GDP1$SendTime)
EWR_GDP1$BgnTime=hm(EWR_GDP1$BgnTime)
EWR_GDP1$EndTime=hm(EWR_GDP1$EndTime)

EWR_GDP1$SendTimePeriod[EWR_GDP1$SendTime<time2& EWR_GDP1$SendTime>=time1] = 6 #"0-6"
EWR_GDP1$SendTimePeriod[EWR_GDP1$SendTime<time3& EWR_GDP1$SendTime>=time2] =9 #"6-9"
EWR_GDP1$SendTimePeriod[EWR_GDP1$SendTime<time4& EWR_GDP1$SendTime>=time3] =12 #"9-12"
EWR_GDP1$SendTimePeriod[EWR_GDP1$SendTime<time5& EWR_GDP1$SendTime>=time4] =15 #"12-15"
EWR_GDP1$SendTimePeriod[EWR_GDP1$SendTime<time6& EWR_GDP1$SendTime>=time5] =18 #"15-18"
EWR_GDP1$SendTimePeriod[EWR_GDP1$SendTime<time7& EWR_GDP1$SendTime>=time6] =21 #"18-21"
EWR_GDP1$SendTimePeriod[EWR_GDP1$SendTime<time8& EWR_GDP1$SendTime>=time7] =24 #"21-24"

EWR_GDP1$BgnTimePeriod[EWR_GDP1$BgnTime<time2& EWR_GDP1$BgnTime>=time1] =6 #"0-6"
EWR_GDP1$BgnTimePeriod[EWR_GDP1$BgnTime<time3& EWR_GDP1$BgnTime>=time2] =9 #"6-9"
EWR_GDP1$BgnTimePeriod[EWR_GDP1$BgnTime<time4& EWR_GDP1$BgnTime>=time3] =12 #"9-12"
EWR_GDP1$BgnTimePeriod[EWR_GDP1$BgnTime<time5& EWR_GDP1$BgnTime>=time4] =15 #"12-15"
EWR_GDP1$BgnTimePeriod[EWR_GDP1$BgnTime<time6& EWR_GDP1$BgnTime>=time5] =18 #"15-18"
EWR_GDP1$BgnTimePeriod[EWR_GDP1$BgnTime<time7& EWR_GDP1$BgnTime>=time6] =21 #"18-21"
EWR_GDP1$BgnTimePeriod[EWR_GDP1$BgnTime<time8& EWR_GDP1$BgnTime>=time7] =24 #"21-24"

EWR_GDP1$EndTimePeriod[EWR_GDP1$EndTime<time2& EWR_GDP1$EndTime>=time1] =6 #"0-6"
EWR_GDP1$EndTimePeriod[EWR_GDP1$EndTime<time3& EWR_GDP1$EndTime>=time2] =9 #"6-9"
EWR_GDP1$EndTimePeriod[EWR_GDP1$EndTime<time4& EWR_GDP1$EndTime>=time3] =12 #"9-12"
EWR_GDP1$EndTimePeriod[EWR_GDP1$EndTime<time5& EWR_GDP1$EndTime>=time4] =15 #"12-15"
EWR_GDP1$EndTimePeriod[EWR_GDP1$EndTime<time6& EWR_GDP1$EndTime>=time5] =18 #"15-18"
EWR_GDP1$EndTimePeriod[EWR_GDP1$EndTime<time7& EWR_GDP1$EndTime>=time6] =21 #"18-21"
EWR_GDP1$EndTimePeriod[EWR_GDP1$EndTime<time8& EWR_GDP1$EndTime>=time7] =24 #"21-24"

EWR_GDP1$SendTimePeriod= as.numeric(EWR_GDP1$SendTimePeriod)
EWR_GDP1$BgnTimePeriod= as.numeric(EWR_GDP1$BgnTimePeriod)
EWR_GDP1$EndTimePeriod= as.numeric(EWR_GDP1$EndTimePeriod)

##generate numeric weekday
EWR_GDP1$BgnWeekday=as.character(EWR_GDP1$BgnWeekday)
EWR_GDP1$WeekdayNo[EWR_GDP1$BgnWeekday =="Monday"] = 1
EWR_GDP1$WeekdayNo[EWR_GDP1$BgnWeekday =="Tuesday"] = 2
EWR_GDP1$WeekdayNo[EWR_GDP1$BgnWeekday =="Wednesday"] = 3
EWR_GDP1$WeekdayNo[EWR_GDP1$BgnWeekday =="Thursday"] = 4
EWR_GDP1$WeekdayNo[EWR_GDP1$BgnWeekday =="Friday"] = 5
EWR_GDP1$WeekdayNo[EWR_GDP1$BgnWeekday =="Saturday"] = 6
EWR_GDP1$WeekdayNo[EWR_GDP1$BgnWeekday =="Sunday"] = 7
EWR_GDP1$WeekdayNo = as.numeric(EWR_GDP1$WeekdayNo)

##change others to numeric
EWR_GDP1$Duration_Advisory= as.numeric(EWR_GDP1$Duration_Advisory)
EWR_GDP1$Duration_Actual= as.numeric(EWR_GDP1$Duration_Actual)
EWR_GDP1$Number_Revisions_NoCNX= as.numeric(EWR_GDP1$Number_Revisions_NoCNX)
EWR_GDP1$Number_TotModif_incldCNX= as.numeric(EWR_GDP1$Number_TotModif_incldCNX)
EWR_GDP1$LeadTime= as.numeric(EWR_GDP1$LeadTime)

write.csv(x = EWR_GDP1, file="EWR_GDP12.csv")

#Part 13 match METAR data (hourly)

METAR4 = read.csv("METAR-20140831-nonduplicated-RNSNIC.csv",  as.is=TRUE)
METAR4$start.time= as.POSIXct(METAR4$start.time, tz =  "America/New_York")
METAR4$end.time= as.POSIXct(METAR4$end.time, tz =  "America/New_York")

EWR_GDP1$MCIC_obs <- ""
EWR_GDP1$Vis_obs <- ""
EWR_GDP1$Ceiling_obs <- ""
EWR_GDP1$TS_obs <- ""
EWR_GDP1$CW0422_obs <- ""
EWR_GDP1$CW1129_obs <- ""
EWR_GDP1$PC_obs <- ""
EWR_GDP1$RNSNIC_obs <- ""

for ( i in 1:nrow(EWR_GDP1)) {
  
  METARGDP1=METAR4[METAR4$end.time > EWR_GDP1$Derived.BgnDate.Time.UTC[i] & METAR4$start.time< EWR_GDP1$Derived.EndDate.Time.UTC[i],]
  
  H = ceiling(as.numeric(EWR_GDP1$Duration_Advisory[i]))
  
  for (h in 1:H) {
    
    METARGDP2=METARGDP1[METARGDP1$start.time<EWR_GDP1$Derived.BgnDate.Time.UTC[i]+h*60*60 & METARGDP1$end.time> EWR_GDP1$Derived.BgnDate.Time.UTC[i]+(h-1)*60*60,]
    
    METARGDP3 = METARGDP2[order(METARGDP2$MCIC, METARGDP2$TS, METARGDP2$CW0422, METARGDP2$CW1129, METARGDP2$RNSNIC, METARGDP2$PC ,decreasing = TRUE)[1],]
    
    EWR_GDP1$MCIC_obs[i] = paste(EWR_GDP1$MCIC_obs[i], METARGDP3$MCIC, sep = " ")
    EWR_GDP1$Vis_obs[i] = paste(EWR_GDP1$Vis_obs[i], METARGDP3$Visibility, sep = " ")
    EWR_GDP1$Ceiling_obs[i] = paste(EWR_GDP1$Vis_obs[i], METARGDP3$Ceiling, sep = " ")
    EWR_GDP1$TS_obs[i] = paste(EWR_GDP1$TS_obs[i], METARGDP3$TS, sep = " ")
    EWR_GDP1$CW0422_obs[i] = paste(EWR_GDP1$CW0422_obs[i], METARGDP3$CW0422, sep = " ")
    EWR_GDP1$CW1129_obs[i] = paste(EWR_GDP1$CW1129_obs[i], METARGDP3$CW1129, sep = " ")
    EWR_GDP1$PC_obs[i] = paste(EWR_GDP1$PC_obs[i], METARGDP3$PC, sep = " ")
    EWR_GDP1$RNSNIC_obs[i] = paste(EWR_GDP1$RNSNIC_obs[i], METARGDP3$RNSNIC, sep = " ")
    
  }
}
write.csv(x = EWR_GDP1, file="EWR_GDP13.csv")

# Part 14 generate root no and mod No. 

#add V&C TAF
temp <-read.csv("EWR_GDP7【TAF】.csv")
temp=temp[difftime(as.Date(temp$AdvisoryDate.UTC), as.Date('2014-09-01 '))<0,]
temp$Visibility<- as.character(temp$Visibility)
temp$Ceiling<- as.character(temp$Ceiling)
EWR_GDP1 = cbind(EWR_GDP1, Visibility = temp$Visibility, Ceiling = temp$Ceiling)
EWR_GDP1$Visibility = as.character(EWR_GDP1$Visibility)
EWR_GDP1$Ceiling = as.character(EWR_GDP1$Ceiling)

##label every root GDP
EWR_GDP1$RootAdvisoryDate.UTC = as.character(EWR_GDP1$RootAdvisoryDate.UTC)
EWR_GDP1$Is.RootAdvisory = as.character(EWR_GDP1$Is.RootAdvisory)
Root = EWR_GDP1[EWR_GDP1$Is.RootAdvisory == "Yes",]
Root$RootNumber= c(1: nrow(Root))

##label root GDP no for every GDP 
for (i in 1:nrow(Root)) {
  rows = which(Root$RootAdvisoryDate.UTC[i] ==EWR_GDP1$RootAdvisoryDate.UTC & Root$RootAdvisoryNumber[i]==EWR_GDP1$RootAdvisoryNumber)
  for (j in 1:length(rows)) {
    EWR_GDP1$RootNumber[rows[j]] <- Root$RootNumber[i]
  }
}

##label mods for every GDP plan
EWR_GDP2= EWR_GDP1[order(EWR_GDP1$RootNumber, decreasing = FALSE),]
EWR_GDP2$ModNumber= 0
ModNo = 0
for (i in 2:nrow(EWR_GDP2)) {
  RootNo = EWR_GDP2$RootNumber[i-1]
  if (EWR_GDP2$RootNumber[i] == RootNo) {
    ModNo = ModNo+1
    EWR_GDP2$ModNumber[i] = ModNo
  } else {
    ModNo = 0}
}

##order data
EWR_GDP3= EWR_GDP2[order(EWR_GDP2$RootNumber, EWR_GDP2$ModNumber, decreasing = FALSE),]
write.csv(x = EWR_GDP3, file="EWR_GDP14.csv")


# Part 15 convert to hourly GDPs 

EWR_GDP3$ActAdvisoryDuration= EWR_GDP3$Duration_Advisory
EWR_GDP3$ActAdvisoryEndTime= EWR_GDP3$Derived.EndDate.Time.UTC
for (i in 1:(nrow(EWR_GDP3)-1)) {
  if(EWR_GDP3$RootNumber[i]== EWR_GDP3$RootNumber[i+1] & EWR_GDP3$Derived.BgnDate.Time.UTC[i+1] < EWR_GDP3$Derived.EndDate.Time.UTC[i]) {
    EWR_GDP3$ActAdvisoryEndTime[i] = EWR_GDP3$Derived.BgnDate.Time.UTC[i+1]
    EWR_GDP3$ActAdvisoryDuration[i] = difftime(EWR_GDP3$ActAdvisoryEndTime[i],EWR_GDP3$Derived.BgnDate.Time.UTC[i],units = "hours")
  } 
}

##delete first blank in the weather data
MCIC = substring(EWR_GDP3$MCIC,2, nchar(EWR_GDP3$MCIC))
Vis = substring(EWR_GDP3$Visibility,2, nchar(EWR_GDP3$Visibility))
Ceiling = substring(EWR_GDP3$Ceiling,2, nchar(EWR_GDP3$Ceiling))
TS = substring(EWR_GDP3$TS,2, nchar(EWR_GDP3$TS))
CW0422 = substring(EWR_GDP3$CW0422,2, nchar(EWR_GDP3$CW0422))
CW1129 = substring(EWR_GDP3$CW1129,2, nchar(EWR_GDP3$CW1129))
PC = substring(EWR_GDP3$PC,2, nchar(EWR_GDP3$PC))
RNSNIC= substring(EWR_GDP3$RNSNIC,2, nchar(EWR_GDP3$RNSNIC))

MCIC_obs = substring(EWR_GDP3$MCIC_obs,2, nchar(EWR_GDP3$MCIC_obs))
Vis_obs = substring(EWR_GDP3$Vis_obs,2, nchar(EWR_GDP3$Vis_obs))
Ceiling_obs = substring(EWR_GDP3$Ceiling_obs,2, nchar(EWR_GDP3$Ceiling_obs))
TS_obs = substring(EWR_GDP3$TS_obs,2, nchar(EWR_GDP3$TS_obs))
CW0422_obs = substring(EWR_GDP3$CW0422_obs,2, nchar(EWR_GDP3$CW0422_obs))
CW1129_obs = substring(EWR_GDP3$CW1129_obs,2, nchar(EWR_GDP3$CW1129_obs))
PC_obs = substring(EWR_GDP3$PC_obs,2, nchar(EWR_GDP3$PC_obs))
RNSNIC_obs = substring(EWR_GDP3$RNSNIC_obs,2, nchar(EWR_GDP3$RNSNIC_obs))

c = ncol(EWR_GDP3)
HrGDP <- EWR_GDP3[0,]
rowno = 0

for ( i in 1:nrow(EWR_GDP3)) {
  H = ceiling(as.numeric(EWR_GDP3$ActAdvisoryDuration[i]))
  
  MCIC1 = as.numeric(unlist(strsplit(as.character(MCIC[i]), split =" ")))
  Vis1 = as.numeric(unlist(strsplit(as.character(Vis[i]), split =" ")))
  Ceiling1 = as.numeric(unlist(strsplit(as.character(Ceiling[i]), split =" ")))
  TS1 = as.numeric(unlist(strsplit(as.character(TS[i]), split =" ")))
  CW04221 = as.numeric(unlist(strsplit(as.character(CW0422[i]), split =" ")))
  CW11291 = as.numeric(unlist(strsplit(as.character(CW1129[i]), split =" ")))
  PC1 = as.numeric(unlist(strsplit(as.character(PC[i]), split =" ")))
  RNSNIC1 = as.numeric(unlist(strsplit(as.character(RNSNIC[i]), split =" ")))
  
  PR1 = as.numeric(unlist(strsplit(as.character(EWR_GDP3$ProgramRateVct [i]), split ="/")))
  
  MCIC1_obs = as.numeric(unlist(strsplit(as.character(MCIC_obs[i]), split =" ")))
  Vis1_obs = as.numeric(unlist(strsplit(as.character(Vis_obs [i]), split =" ")))
  Ceiling1_obs = as.numeric(unlist(strsplit(as.character(Ceiling_obs [i]), split =" ")))
  TS1_obs = as.numeric(unlist(strsplit(as.character(TS_obs[i]), split =" ")))
  CW04221_obs = as.numeric(unlist(strsplit(as.character(CW0422_obs[i]), split =" ")))
  CW11291_obs = as.numeric(unlist(strsplit(as.character(CW1129_obs[i]), split =" ")))
  PC1_obs = as.numeric(unlist(strsplit(as.character(PC_obs[i]), split =" ")))
  RNSNIC1_obs = as.numeric(unlist(strsplit(as.character(RNSNIC_obs[i]), split =" ")))
  if(H>0) {
    for (h in 1:H) {
      rowno =rowno+1
      HrGDP[rowno,1:c] =EWR_GDP3[i,]
      
      HrGDP$HourNo[rowno] = h
      
      HrGDP$SchArrSchIn[rowno] = EWR_GDP3$SchArrSchIn[i]
      HrGDP$ImpArrSchIn[rowno] = EWR_GDP3$ImpArrSchIn[i]
      
      HrGDP$BgnTime[rowno] = as.character(EWR_GDP3$Derived.BgnDate.Time.UTC[i]+ (h-1)*60*60)
      if(h==H) {
        HrGDP$EndTime[rowno] <- as.character(EWR_GDP3$ActAdvisoryEndTime[i])
      } else {
        HrGDP$EndTime[rowno] = as.character(EWR_GDP3$Derived.BgnDate.Time.UTC[i]+h*60*60)
      }
      
      HrGDP$MCIC_hr[rowno] = MCIC1[h]
      HrGDP$Vis_hr[rowno] = Vis1[h]
      HrGDP$Ceiling_hr[rowno] = Ceiling1[h]
      HrGDP$TS_hr[rowno] = TS1[h]
      HrGDP$CW0422_hr[rowno] = CW04221[h]
      HrGDP$CW1129_hr[rowno] = CW11291[h]
      HrGDP$PC_hr[rowno] = PC1[h]
      HrGDP$RNSNIC_hr[rowno] = RNSNIC1[h]
      
      HrGDP$PR_hr[rowno] = PR1[h]
      
      HrGDP$MCIC_hr_obs[rowno] = MCIC1_obs[h]
      HrGDP$Vis_hr_obs [rowno] = Vis1_obs [h]
      HrGDP$Ceiling_hr_obs [rowno] = Ceiling1_obs [h]
      HrGDP$TS_hr_obs[rowno] = TS1_obs[h]
      HrGDP$CW0422_hr_obs[rowno] = CW04221_obs[h]
      HrGDP$CW1129_hr_obs[rowno] = CW11291_obs[h]
      HrGDP$PC_hr_obs[rowno] = PC1_obs[h]
      HrGDP$RNSNIC_hr_obs[rowno] = RNSNIC1_obs[h]
    }
  }
}

Focus = which(HrGDP$AdvisoryType == "GDP CNX" & HrGDP$HourNo != 1)
HrGDP = HrGDP[-Focus,]

HrGDP$EarlyCancelTime[is.na(HrGDP$EarlyCancelTime)] = 0
HrGDP[HrGDP$AdvisoryType == "GDP CNX", c("Duration_Advisory", "PR_hr")] = 0

HrGDP$BgnTime = parse_date_time(HrGDP$BgnTime, guess_formats(HrGDP$BgnTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")), tz="America/New_York")
HrGDP$EndTime = parse_date_time(HrGDP$EndTime, guess_formats(HrGDP$EndTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")), tz="America/New_York")

HrGDP$StartMin= difftime(HrGDP$BgnTime, HrGDP$RootBgnTime, units = "mins")
HrGDP$EndTime[HrGDP$AdvisoryType == "GDP CNX"]= HrGDP$BgnTime[HrGDP$AdvisoryType == "GDP CNX"]
HrGDP$EndMin= difftime(HrGDP$EndTime, HrGDP$RootBgnTime, units = "mins")

write.csv(x = HrGDP, file="EWR_GDP15.csv")

#Part 16 GDP CNX is a red point not a line
for (j in 1:nrow(HrGDP)) {
  if(HrGDP$AdvisoryType[j] == "GDP CNX") {
    HrGDP$PR_hr[j] = HrGDP$PR_hr[j-1]
    HrGDP$SchArrSchIn[j] = HrGDP$SchArrSchIn[j-1]
    HrGDP$ImpArrSchIn[j] = HrGDP$ImpArrSchIn[j-1]
  }
}
## label advisory type
HrGDP$GDPCNX=2
HrGDP$GDPCNX[HrGDP$AdvisoryType == "GDP"]=1


# strong wind
HrGDP$CW0422_hr=as.numeric(HrGDP$CW0422_hr)
HrGDP$CW1129_hr=as.numeric(HrGDP$CW1129_hr)
HrGDP$CW0422_hr_obs =as.numeric(HrGDP$CW0422_hr_obs)
HrGDP$CW1129_hr_obs =as.numeric(HrGDP$CW1129_hr_obs)

HrGDP$CW0422_Str=0
HrGDP$CW1129_Str=0
HrGDP$CW0422_Str[HrGDP$CW0422_hr>=25]=1
HrGDP$CW1129_Str[HrGDP$CW1129_hr>=25]=1

HrGDP$CW0422_obs_Str=0
HrGDP$CW1129_obs_Str=0
HrGDP$CW0422_obs_Str[HrGDP$CW0422_hr_obs >=25]=1
HrGDP$CW1129_obs_Str[HrGDP$CW1129_hr_obs>=25]=1

write.csv(x = HrGDP, file="EWR_GDP16.csv")

## test whether a GDP is faulted
I = nrow(HrGDP)-1
HrGDP$Broken=0
for (i in 1:I) {
  if(HrGDP$RootNumber[i]== HrGDP$RootNumber[i+1] & HrGDP$StartMin[i+1] > HrGDP$EndMin[i]) {
    HrGDP$Broken[i+1] = 1
  } 
}
write.csv(x = HrGDP, file="EWR_GDP15-1.csv")


#Part 17横向数据库


HrGDP1= HrGDP

#HrGDP1[] = lapply(HrGDP1, function(x) as.character(x))
#HrGDP2 = HrGDP1[complete.cases(HrGDP1[,97:101]),]

HrGDP1$RootNumber=as.numeric(HrGDP1$RootNumber)
HrGDP1$AdvisoryType=as.character(HrGDP1$AdvisoryType)
HrGDP1$StartMin = as.numeric(HrGDP1$StartMin)
HrGDP1$EndMin = as.numeric(HrGDP1$EndMin)

##删掉最长的GDP： 89,436
#HrGDP1=HrGDP1[-which(HrGDP1$RootNumber == 89),]
#EWR_GDP3= EWR_GDP3[-which(EWR_GDP3$RootNumber == 89),]

##是否以EARLY CNX结束
#EWR_GDP3$AdvisoryType=as.character(EWR_GDP3$AdvisoryType)
#EWR_GDP3$RootAdvisoryNumber=as.numeric(as.character(EWR_GDP3$RootAdvisoryNumber))
#EWR_GDP3$RootAdvisoryDate.UTC= EWR_GDP3$RootAdvisoryDate.UTC

#EWR_GDP3$GDPCNX=0
#dat1= EWR_GDP3[EWR_GDP3$AdvisoryType == "GDP CNX",]
#for (i in 1:nrow(EWR_GDP3) ) {
#dat2=dat1[dat1$RootAdvisoryNumber==EWR_GDP3$RootAdvisoryNumber[i],]
#if (length(which(dat2$RootAdvisoryDate.UTC ==EWR_GDP3$RootAdvisoryDate.UTC[i]))>0) {
#EWR_GDP3$GDPCNX[i]=1
#}
#}

#EWR_GDP3$EarlyCancel=0
#EWR_GDP3$EarlyCancel[EWR_GDP3$EarlyCancelTime >0]=1

##创建root GDP
EWR_GDP3$Is.RootAdvisory=as.character(EWR_GDP3$Is.RootAdvisory)
root = EWR_GDP3[EWR_GDP3$Is.RootAdvisory=="Yes",]

#删掉含有NA的GDP
HrGDP1$CW0422=as.character(HrGDP1$CW0422)
HrGDP1$CW1129=as.character(HrGDP1$CW1129)
HrGDP1$MCIC=as.character(HrGDP1$MCIC)
HrGDP1$TS=as.character(HrGDP1$TS)
HrGDP1$PC=as.character(HrGDP1$PC)

focus=which(grepl("NA", HrGDP1$CW0422))
if(length(focus)>0) {
  NANo=HrGDP1$RootNumber[focus]
  HrGDP1 = HrGDP1[-focus,]
  for(j in 1:length(NANo)) {
    focus1 = which(root$RootNumber == NANo[j])
    if(length(focus1)>0) {
      root = root[-focus1,]
    }
  }
}
focus=which(grepl("NA", HrGDP1$CW1129))
if(length(focus)>0) {
  NANo=HrGDP1$RootNumber[focus]
  HrGDP1 = HrGDP1[-focus,]
  for(j in 1:length(NANo)) {
    focus1 = which(root$RootNumber == NANo[j])
    if(length(focus1)>0) {
      root = root[-focus1,]
    }
  }
}
focus=which(grepl("NA", HrGDP1$MCIC))
if(length(focus)>0) {
  NANo=HrGDP1$RootNumber[focus]
  HrGDP1 = HrGDP1[-focus,]
  for(j in 1:length(NANo)) {
    focus1 = which(root$RootNumber == NANo[j])
    if(length(focus1)>0) {
      root = root[-focus1,]
    }
  }
}
focus=which(grepl("NA", HrGDP1$PC))
if(length(focus)>0) {
  NANo=HrGDP1$RootNumber[focus]
  HrGDP1 = HrGDP1[-focus,]
  for(j in 1:length(NANo)) {
    focus1 = which(root$RootNumber == NANo[j])
    if(length(focus1)>0) {
      root = root[-focus1,]
    }
  }
}
focus=which(grepl("NA", HrGDP1$TS))
if(length(focus)>0) {
  NANo=HrGDP1$RootNumber[focus]
  HrGDP1 = HrGDP1[-focus,]
  for(j in 1:length(NANo)) {
    focus1 = which(root$RootNumber == NANo[j])
    if(length(focus1)>0) {
      root = root[-focus1,]
    }
  }
}
write.csv(x = HrGDP1, file="T.csv")

#17-t saved
##删掉时长不连贯的GDP
HrGDP2= HrGDP1
root1=root

J= nrow(HrGDP2)-1
HrGDP2$DiffSE=0
for( j in 1:J) {
  if(HrGDP2$RootNumber[j]== HrGDP2$RootNumber[j+1]){
    HrGDP2$DiffSE[j] = HrGDP2$EndMin[j]- HrGDP2$StartMin[j+1]
  }
}
focus=which(HrGDP2$DiffSE!=0)
NANo=HrGDP2$RootNumber[focus]
for(j in 1:length(NANo)) {
  focus1 = which(HrGDP2$RootNumber == NANo[j])
  if(length(focus1)>0) {
    HrGDP2 = HrGDP2[-focus1,]
  }
  focus2= which(root1$RootNumber == NANo[j])
  if(length(focus2)>0) {
    root1 = root1[-focus2,]
  }
}

###删掉时间异常的GDP
focus=which(HrGDP2$EndMin<0 | HrGDP2$StartMin<0)
NANo=HrGDP2$RootNumber[focus]
for(j in 1:length(NANo)) {
  focus1 = which(HrGDP2$RootNumber == NANo[j])
  if(length(focus1)>0) {
    HrGDP2 = HrGDP2[-focus1,]
  }
  focus2= which(root1$RootNumber == NANo[j])
  if(length(focus2)>0) {
    root1 = root1[-focus2,]
  }
}


write.csv(x = HrGDP2, file="T1.csv")

#17-2 saved

maxtime=max(HrGDP2$EndMin)
MaxInterval=15
NumNewcol= floor(maxtime/MaxInterval)+1 #66
root2 = root1
##TS_hr
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "TS")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$TS_hr, decreasing = TRUE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$TS_hr[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}

##PC_hr
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "PC")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$PC_hr, decreasing = TRUE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$PC_hr[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}


##CW0422_hr
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "CW0422")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$CW0422_hr, decreasing = TRUE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$CW0422_hr[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}

##CW1129_hr
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "CW1129")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$CW1129_hr, decreasing = TRUE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$CW1129_hr[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}

##Ceiling_hr
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "Ceiling")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$Ceiling_hr, decreasing = FALSE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$Ceiling_hr[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}
##Vis_hr
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "Vis")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$Vis_hr, decreasing = FALSE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$Vis_hr[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}

##PR_hr

###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "PR")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$PR_hr, decreasing = FALSE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$PR_hr[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}

##SchArrSchIn
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "SchArrSchIn")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$SchArrSchIn, decreasing = TRUE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$SchArrSchIn[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}


##ImpArrSchIn
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "ImpArrSchIn")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$ImpArrSchIn, decreasing = TRUE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$ImpArrSchIn[1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}



##Duration
###列名
OldTotCol=ncol(root2)
NewBgnCol=ncol(root2)+1
NewTotCol= OldTotCol + NumNewcol #192 = 126 +66
for(j in NewBgnCol:NewTotCol){
  root2[,j]<-NA
  IntNum= j- OldTotCol
  colnames(root2)[j]<-paste(as.character(IntNum), "PlanDuration")
}
###赋值
maxrt= max(HrGDP2$RootNumber)
for(k in 1:maxrt) {
  sub1 = HrGDP2[HrGDP2$RootNumber == k,]
  if(nrow(sub1)>0){
    MaxEndTm = max(sub1$EndMin)
    Int = MaxEndTm/(NumNewcol-1)
    for(i in 1:NumNewcol){
      x= (i-1)*Int
      y=i*Int
      sub2=sub1[which( y>=sub1$StartMin & x< sub1$EndMin),] #GDP advisory时间与15间隔时间有交集，取最大SchArr
      if(nrow(sub2)>0) {
        sub2= sub2[order(sub2$Duration_Initiative, decreasing = TRUE),]
        root2[root2$RootNumber == k, OldTotCol +i]=sub2$Duration_Initiative [1]
      }else{
        root2[root2$RootNumber == k, OldTotCol +i]= "/"
      }}}}

#17-3-f saved(更换了变量顺序)

write.csv(x = root2, file="EWR_GDP17-3-f.csv")

EWR_GDP4 = EWR_GDP3[EWR_GDP3$RootNumber %in% HrGDP2$RootNumber,]
#18-1 saved 

#Part 18 Evaluation
## get GD, TD, PD, AD from IF
EWRIF = read.csv( "EWRIF-fixed.csv")

EWR_GDP4$Exempt.Dep.Facilities = as.character(EWR_GDP4$Exempt.Dep.Facilities)
EWR_GDP4$AdvisoryType =as.character(EWR_GDP4$AdvisoryType)
EWR_GDP4$Dep.Scope = as.character(EWR_GDP4$Dep.Scope)

EWRIF$Mile = as.numeric(EWRIF$Mile)
EWRIF$DEP_LOCID=as.character(EWRIF$DEP_LOCID)
EWRIF$Country=as.character(EWRIF$Country)

##Ground and G+Air delay
EWRIF$FpIn = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), as.character (EWRIF$FPINTM)))
EWRIF$FpIn = strptime(EWRIF$FpIn, "%Y %m %d %H:%M", tz ="America/New_York")

EWRIF$SchIn = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), as.character (EWRIF$SCHINTM)))
EWRIF$SchIn = strptime(EWRIF$SchIn, "%Y %m %d %H:%M", tz ="America/New_York")

EWRIF$DLAP = as.numeric(EWRIF$DLASCHARR) - as.numeric(EWRIF$DLAFPARR)
EWRIF$DLAP[EWRIF$DLAP<0] = 0

IFCA= EWRIF[EWRIF$Country == "CA",]
IFUS= EWRIF[EWRIF$Country == "US",]
IFINT= EWRIF[EWRIF$Country == "INT",]

for (i in 1:nrow(EWR_GDP4)) {
  if (EWR_GDP4$AdvisoryType[i] == "GDP") {
    
    ##CA flights
    ###Spatial
    rows<-sapply(IFCA$DEP_LOCID, function(x) grepl(x, EWR_GDP4$Canadian.Dep.Arpts.Included[i]))
    IFGDPCA = IFCA[rows,]
    ###Temporal
    if(EWR_GDP4$RootNumber[i+1] == EWR_GDP4$RootNumber[i] ) {
      mint = min(EWR_GDP4$Derived.BgnDate.Time.UTC[i+1], EWR_GDP4$Derived.EndDate.Time.UTC[i]) 
      row <-which(IFGDPCA$SchIn>=EWR_GDP4$Derived.BgnDate.Time.UTC[i] & IFGDPCA$SchIn <=mint)} else {
        row <-which(IFGDPCA$SchIn>=EWR_GDP4$Derived.BgnDate.Time.UTC[i] & IFGDPCA$SchIn <= EWR_GDP4$Derived.EndDate.Time.UTC[i])
      }
    CA = IFGDPCA[row,]
    CAGD = sum(CA$DLASCHOFF)
    CATD = sum(CA$DLASCHARR)
    CADLAA=sum(CA$DLASCHARR)
    
    ##US flights
    ### tick out exempted flights
    if (EWR_GDP4$Exempt.Dep.Facilities[i]!= "-") { 
      rows<-sapply(IFUS$DEP_LOCID, function(x) !grepl(x, EWR_GDP4$Exempt.Dep.Facilities[i])) 
      IFUS1 = IFUS[rows,]
      rows<- sapply(IFUS1$ARTCC, function(x) !grepl(x, EWR_GDP4$Exempt.Dep.Facilities[i]) )
      IFGDPUS = IFUS1[rows,]
    } else{
      IFGDPUS= IFUS
    }
    ### spatial scope
    if (!grepl("ALL",EWR_GDP4$Dep.Scope[i]) ){
      if(EWR_GDP4$DepScopeType[i] == "ARTCC")
      {
        IFGDPUS1=IFGDPUS[sapply(IFGDPUS$ARTCC, function(x) grepl(x, EWR_GDP4$Dep.Scope[i] )),]
      }
      if(EWR_GDP4$DepScopeType[i] == "Radius")
      {
        IFGDPUS1 = IFGDPUS[IFGDPUS$Mile <= as.numeric(EWR_GDP4$Dep.Scope[i]),]
      }
    }else {
      IFGDPUS1= IFGDPUS}
    ### temporal scope
    if(EWR_GDP4$RootNumber[i+1] == EWR_GDP4$RootNumber[i] ) {
      mint = min(EWR_GDP4$Derived.BgnDate.Time.UTC[i+1], EWR_GDP4$Derived.EndDate.Time.UTC[i]) 
      row <-which(IFGDPUS1$SchIn>=EWR_GDP4$Derived.BgnDate.Time.UTC[i] &  IFGDPUS1$SchIn<=mint)} else {
        row <-which(IFGDPUS1$SchIn>=EWR_GDP4$Derived.BgnDate.Time.UTC[i] &  IFGDPUS1$SchIn<=EWR_GDP4$Derived.EndDate.Time.UTC[i])
      }
    
    US = IFGDPUS1[row,]
    USGD = sum(US$DLASCHOFF)
    USTD = sum(US$DLASCHARR)
    USDLAA=sum(US$DLASCHARR)
    
    ##Total Delay.
    EWR_GDP4$GD[i] = as.numeric(CAGD)+ as.numeric(USGD) 
    EWR_GDP4$TD[i] = as.numeric(CATD)+ as.numeric(USTD) 
    ##Total Delay.
    EWR_GDP4$DLAA[i] = as.numeric(CADLAA)+ as.numeric(USDLAA) 
    
  } 
}

write.csv(x = EWR_GDP4, file="EWR_GDP11-2.csv")
#18-if saved

##Eff
for( j in root2$RootNumber) {
  x= EWR_GDP4[EWR_GDP4$RootNumber ==j & EWR_GDP4$AdvisoryType =="GDP" ,]
  root2$GD[root2$RootNumber ==j] = sum(x$GD)
  root2$TD[root2$RootNumber ==j] = sum(x$TD)
  root2$Eff[root2$RootNumber ==j] = root2$GD[root2$RootNumber ==j]/ root2$TD[root2$RootNumber ==j]
}
write.csv(x = root2, file="EWR_GDP18-t.csv")

for (i in 1:nrow(root2)) {
  
  ##CA flights
  ###Spatial
  rows<-sapply(IFCA$DEP_LOCID, function(x) grepl(x, root2$Canadian.Dep.Arpts.Included[i]))
  IFGDPCA = IFCA[rows,]
  ###Temporal
  row <-which(IFGDPCA$SchIn>=root2$Derived.BgnDate.Time.UTC[i] & IFGDPCA$SchIn <= root2$Derived.EndDate.Time.UTC[i])
  CA = IFGDPCA[row,]
  CADLAP= sum(CA$DLAP)
  
  ##US flights
  ### tick out exempted flights
  if (root2$Exempt.Dep.Facilities[i]!= "-") { 
    rows<-sapply(IFUS$DEP_LOCID, function(x) !grepl(x, root2$Exempt.Dep.Facilities[i])) 
    IFUS1 = IFUS[rows,]
    rows<- sapply(IFUS1$ARTCC, function(x) !grepl(x, root2$Exempt.Dep.Facilities[i]) )
    IFGDPUS = IFUS1[rows,]
  } else{
    IFGDPUS= IFUS
  }
  ### spatial scope
  if (!grepl("ALL",root2$Dep.Scope[i]) ){
    if(root2$DepScopeType[i] == "ARTCC")
    {
      IFGDPUS1=IFGDPUS[sapply(IFGDPUS$ARTCC, function(x) grepl(x, root2$Dep.Scope[i] )),]
    }
    if(root2$DepScopeType[i] == "Radius")
    {
      IFGDPUS1 = IFGDPUS[IFGDPUS$Mile <= as.numeric(root2$Dep.Scope[i]),]
    }
  }else {
    IFGDPUS1= IFGDPUS}
  ### temporal scope
  row <-which(IFGDPUS1$SchIn>=root2$Derived.BgnDate.Time.UTC[i] &  IFGDPUS1$SchIn<=root2$Derived.EndDate.Time.UTC[i])
  USDLAP= sum(US$DLAP)
  
  ##Total Delay.
  root2$DLAP[i] = as.numeric(CADLAP)+ as.numeric(USDLAP) 
  
}

write.csv(x = root2, file="EWR_GDP11-3.csv")



##Predictability

for( j in root2$RootNumber) {
  x= EWR_GDP4[EWR_GDP4$RootNumber ==j & EWR_GDP4$AdvisoryType =="GDP" ,]
  A = sum(x$DLAA)
  root2$DLAA[root2$RootNumber ==j] = A
}

for(j in 1:nrow(root2)) {
  root2$Pred[j] = min(root2$DLAA[j], root2$DLAP[j])/ max(root2$DLAA[j], root2$DLAP[j])
}


write.csv(x = root2, file="EWR_GDP18-3.csv")


#18-f saved 

##Actual Arrivals
EWRIF$ARR_YYYY = as.character(substr(EWRIF$ARR_YYYYMM,1,4))
EWRIF$ARR_MM = as.character(substr(EWRIF$ARR_YYYYMM,5,6))

EWRIF$ActIn = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), as.character (EWRIF$ACTINTM)))
EWRIF$ActIn = strptime(EWRIF$ActIn, "%Y %m %d %H:%M", tz ="America/New_York")
for(i in 1:nrow(EWR_GDP4)) {
  if(EWR_GDP4$RootNumber[i+1] == EWR_GDP4$RootNumber[i] ) {
    mint = min(EWR_GDP4$Derived.BgnDate.Time.UTC[i+1], EWR_GDP4$Derived.EndDate.Time.UTC[i]) 
    EWR_GDP4$ActArr[i] = length(which(EWRIF$ActIn >=EWR_GDP4$Derived.BgnDate.Time.UTC[i] & EWRIF$ActIn <=mint)) } else {
      EWR_GDP4$ActArr[i] = length(which(EWRIF$ActIn >=EWR_GDP4$Derived.BgnDate.Time.UTC[i] & EWRIF$ActIn <= EWR_GDP4$Derived.EndDate.Time.UTC[i]))
    }
}
write.csv(x = EWR_GDP4, file="EWR_GDP18-4.csv")


##Capacity Utilization
#open 18-1
for ( j in 1:nrow(EWR_GDP4)) {
  if(!grepl("-", EWR_GDP4$ProgramRate[j])) {
    PR = as.numeric(unlist(strsplit(as.character(EWR_GDP4$ProgramRate[j]),split = "/")))
    EWR_GDP4$AvePR[j]= mean(sum(PR))
  }
}
EWR_GDP4$TotCap= EWR_GDP4$AvePR * EWR_GDP4$ActAdvisoryDuration

for( j in root2$RootNumber) {
  x= EWR_GDP4[EWR_GDP4$RootNumber ==j & EWR_GDP4$AdvisoryType =="GDP" ,]
  ActArr= sum(x$ActArr)
  TotCap = sum(x$TotCap)
  root2$CU[root2$RootNumber ==j] = ActArr/TotCap
}
write.csv(x = root2, file="EWR_GDP18-CU.csv")

#18-real saved
#Use this one


#Open 18
#Paste label to Hr and Advisory data
rootevl =read.csv("EWR_GDP18-CU.csv")
rootevl$Label = allweather12[,598]
write.csv(x = rootevl, file="EWR_GDP20-r.csv")

rootevl$Label[rootevl$Label==6] =3
for(j in rootevl$RootNumber) {
  HrGDP2$Label[HrGDP2$RootNumber ==j] = rootevl$Label[rootevl$RootNumber == j]
}
write.csv(x = HrGDP2, file="EWR_GDP20-Hr.csv")

for(j in rootevl$RootNumber) {
  EWR_GDP3$Label[EWR_GDP3$RootNumber ==j] = rootevl$Label[rootevl$RootNumber == j]
}
write.csv(x = EWR_GDP3, file="EWR_GDP20-adv.csv")

#Calc average for each lable
StatAna = data.frame(matrix(vector(), 12, 9,
                            dimnames=list(c(), c("TSAve","PCAve","CW0422Ave", "CW1129Ave", "CeilingAve", "VisAve","PRAve","ScopeAve","DurationAve"))),
                     stringsAsFactors=F)
##Mean
HrGDP3 = HrGDP2[!is.na(HrGDP2$Label),]
for( j in 1:12) {
  x = HrGDP3[HrGDP3$Label == j,]
  StatAna$TSAve[j] = mean(x$TS_hr)
  StatAna$PCAve[j] = mean(x$PC_hr)
  StatAna$CW0422Ave[j] = mean(x$CW0422_hr)
  StatAna$CW1129Ave[j] = mean(x$CW1129_hr)
  StatAna$VisAve[j] = mean(x$Vis_hr)
  StatAna$CeilingAve[j] = mean(x$CW0422_hr)
  StatAna$PRAve[j] = mean(x$PR_hr)
}

EWR_GDP4= EWR_GDP3[!is.na(EWR_GDP3$Label),]
for( j in 1:12) {
  x = EWR_GDP4[EWR_GDP4$Label == j,]
  StatAna$ScopeAve[j] = mean(x$ImpArrSchIn)
  StatAna$DurationAve[j] = mean(x$Duration_Initiative)
}

##Var
library(matrixStats)
STAT<- allweather12_P
N = ncol(allweather12_P)+1
for(j in 1:9) {
  j1=(j-1)*66+1
  j2=j1+64
  stat = as.matrix(allweather12_P[,j1:j2])
  STAT[,j] = rowVars(stat)
  colnames(STAT)[j] =paste("var-",j)
}

for( j in 1:12) {
  x = STAT[STAT$PM12 == j,]
  StatAna$TSVar[j] = mean(x$var-1)
  StatAna$PCVar[j] = mean(x$var-2)
  StatAna$CW0422Var[j] = mean(x$var-3)
  StatAna$CW1129Var[j] = mean(x$var-4)
  StatAna$VisVar[j] = mean(x$var-5)
  StatAna$CeilingVar[j] = mean(x$var-6)
}

colnames(STAT)[1:6] = c("TSVar","PCVar","CW0422Var", "CW1129Var", "CeilingVar", "VisVar")
StatAna= cbind(StatAna, STAT[.1:6] )


