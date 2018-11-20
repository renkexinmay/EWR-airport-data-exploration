rm(list=ls())

#import TMI and IF files
TMI = read.csv("advisory.csv")
EWR_GDP = read.csv("GDP_EWR.csv", stringsAsFactors = FALSE)

#Pick out GDPsEWR for EWR airport
focus = which(TMI$AdvisoryType == "GDP"| TMI$AdvisoryType == "GDP CNX")
GDP = TMI[focus,]
focus = which(GDP$ControlElement == "EWR/ZNY")
EWR_GDP = GDP[focus,]

#1st part ends ->EWR_GDP.csv

#Change the timezone of some GDP time varaibles into New York time
EWR_GDP$SendDate.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$SendDate.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Derived.BgnDate.Time.UTC= as.POSIXct(strptime(as.character(EWR_GDP$Derived.BgnDate.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Derived.EndDate.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$Derived.EndDate.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Eff.Bgn.Date.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$Eff.Bgn.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))
EWR_GDP$Eff.End.Date.Time.UTC = as.POSIXct(strptime(as.character(EWR_GDP$Eff.End.Date.Time.UTC),"%Y-%m-%d %H:%M",tz="GMT"))

attr(EWR_GDP$SendDate.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Derived.BgnDate.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Derived.EndDate.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Eff.Bgn.Date.Time.UTC, "tzone") <- "America/New_York"
attr(EWR_GDP$Eff.End.Date.Time.UTC, "tzone") <- "America/New_York"

#Combine year, month, date and time for IF Schedule-In Time

write.csv(x = EWR_GDP, file="2nd.csv")
#2nd parts end

#Convert Departure Scope to Airports
DEPARP= read.csv("DEPARP.csv")
#Initialization
EWR_GDP$Dep.Scope = as.character(EWR_GDP$Dep.Scope)
DEPARP$ARTCC = as.character(DEPARP$ARTCC)
DEPARP$ARTCCARP = as.character(DEPARP$ARTCCARP)
EWR_GDP$ARP = ""
#ARTCC to airports
EWR_GDP$ARP[grepl("ZAB", EWR_GDP$Dep.Scope)] = as.character(paste(as.character(EWR_GDP$ARP[grepl("ZAB", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZAB", DEPARP$ARTCC)]))
EWR_GDP$ARP[grepl("ZAN", EWR_GDP$Dep.Scope)] = as.character(paste(as.character(EWR_GDP$ARP[grepl("ZAN", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZAN", DEPARP$ARTCC)]))
EWR_GDP$ARP[grepl("ZAU", EWR_GDP$Dep.Scope)] = as.character(paste(as.character(EWR_GDP$ARP[grepl("ZAU", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZAU", DEPARP$ARTCC)]))
EWR_GDP$ARP[grepl("ZBW", EWR_GDP$Dep.Scope)] = as.character(paste(as.character(EWR_GDP$ARP[grepl("ZBW", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZBW", DEPARP$ARTCC)]))
EWR_GDP$ARP[grepl("ZDC", EWR_GDP$Dep.Scope)] = as.character(paste(as.character(EWR_GDP$ARP[grepl("ZDC", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZDC", DEPARP$ARTCC)]))
EWR_GDP$ARP[grepl("ZDV", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZDV", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[DEPARP$ARTCC=="ZDV"])
EWR_GDP$ARP[grepl("ZFW", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZFW", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZFW", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZHU", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZHU", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZHU", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZID", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZID", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZID", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZJX", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZJX", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZJX", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZKC", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZKC", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZKC", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZLA", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZLA", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZLA", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZLC", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZLC", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZLC", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZMA", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZMA", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZMA", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZME", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZME", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZME", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZMP", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZMP", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZMP", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZNY", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZNY", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZNY", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZOA", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZOA", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZOA", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZOB", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZOB", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZOB", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZSE", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZSE", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZSE", DEPARP$ARTCC)])
EWR_GDP$ARP[grepl("ZTL", EWR_GDP$Dep.Scope)] = paste(as.character(EWR_GDP$ARP[grepl("ZTL", EWR_GDP$Dep.Scope)]), DEPARP$ARTCCARP[grepl("ZTL", DEPARP$ARTCC)])

EWR_GDP$ARP[grepl("ZTL", EWR_GDP$Dep.Scope) & grepl("ZAB", EWR_GDP$Dep.Scope) & grepl("ZSE", EWR_GDP$Dep.Scope) & grepl("ZFW", EWR_GDP$Dep.Scope) & grepl("ZKC", EWR_GDP$Dep.Scope) & grepl("ZME", EWR_GDP$Dep.Scope) & grepl("ZOA", EWR_GDP$Dep.Scope) & grepl("ZLC", EWR_GDP$Dep.Scope) & grepl("ZLA", EWR_GDP$Dep.Scope) & grepl("ZAU", EWR_GDP$Dep.Scope) & grepl("ZMP", EWR_GDP$Dep.Scope) & grepl("ZDV", EWR_GDP$Dep.Scope) & grepl("ZID", EWR_GDP$Dep.Scope) & grepl("ZMA", EWR_GDP$Dep.Scope) & grepl("ZHU", EWR_GDP$Dep.Scope) & grepl("ZJX", EWR_GDP$Dep.Scope) & grepl("ZBW", EWR_GDP$Dep.Scope) & grepl("ZOB", EWR_GDP$Dep.Scope) & grepl("ZDC", EWR_GDP$Dep.Scope) & grepl("ZNY", EWR_GDP$Dep.Scope) ] = "ALL"
#Distance to Aiports
EWR_GDP$DepScope = as.numeric(EWR_GDP$Dep.Scope)
EWR_GDP$DepScope[is.na(EWR_GDP$DepScope)] = 0
EWR_GDP$ARP = as.character(EWR_GDP$ARP)
DEPARP$DIST = as.numeric(DEPARP$DIST)
DEPARP$AirportFAA = as.character(DEPARP$AirportFAA)
#Next step needs 15 mins
for (j in 1:nrow(DEPARP)) {
  EWR_GDP$ARP[EWR_GDP$DepScope >= DEPARP$DIST[j] & !grepl(DEPARP$ARTCC_ARP[j],EWR_GDP$Exempt.Dep.Facilities)] = paste(EWR_GDP$ARP[EWR_GDP$DepScope >= DEPARP$DIST[j] & !grepl(DEPARP$ARTCC_ARP[j],EWR_GDP$Exempt.Dep.Facilities)], DEPARP$AirportFAA[j])
}
#Canadian Flights
EWR_GDP$Canadian.Dep.Arpts.Included = gsub("-","",  EWR_GDP$Canadian.Dep.Arpts.Included)
EWR_GDP$Canadian.Dep.Arpts.Included = gsub("NONE","",  EWR_GDP$Canadian.Dep.Arpts.Included)
EWR_GDP$ARP = paste(EWR_GDP$ARP, EWR_GDP$Canadian.Dep.Arpts.Included)

EWR_GDP$ARP1= EWR_GDP$ARP
EWR_GDP$ARP1[grepl("ALL", EWR_GDP$Dep.Scope)] = "ALL"
EWR_GDP$ARP1[grepl("ALL" , EWR_GDP$Dep.Scope)& grepl("CZY_AP", EWR_GDP$Dep.Scope)] = "ALL + CZY_AP"
# CZY_AP = CYHZ CYOW CYUL CYYZ CYTZ CYQB

write.csv(x = EWR_GDP, file="3rd.csv")
#3rd  part ends

#Minus Exempt Zones
#EWR_GDP = read.csv("z.csv", stringsAsFactors=FALSE)
EWR_GDP$ARP_EX = EWR_GDP$ARP1
for (i in 1: nrow(EWR_GDP)){
  if (grepl(EWR_GDP$Exempt.Dep.Facilities[i], EWR_GDP$ARP1[i])) {
    EWR_GDP$ARP_EX[i] = gsub(EWR_GDP$Exempt.Dep.Facilities[i], "", EWR_GDP$ARP1[i])
  }
}
EWR_GDP$Exempt.Dep.Facilities1 = paste("except", EWR_GDP$Exempt.Dep.Facilities)
EWR_GDP$Exempt.Dep.Facilities1[grepl("-",EWR_GDP$Exempt.Dep.Facilities ) ] = ""
EWR_GDP$ARP_FIN = EWR_GDP$ARP_EX
for (i in 1: nrow(EWR_GDP)){
  if (grepl("except", EWR_GDP$Exempt.Dep.Facilities1[i]) & grepl("ALL", EWR_GDP$ARP_EX[i])) {
    EWR_GDP$ARP_FIN[i] = paste(EWR_GDP$ARP_EX[i], EWR_GDP$Exempt.Dep.Facilities1[i])
  }
}
EWR_GDP$Dep.Airport= EWR_GDP$ARP_FIN
EWR_GDP$Dep.Airport[grepl("ALL", EWR_GDP$ARP_FIN)] = gsub("ALL", "ALM ABQ E51 BIF DUG ZUN BXK CVS CGZ CNM CVN WSD P08 P52 DHT DMA DGL ELP PRC FLG FST GUP E63 GEU SVC GNT HMN IKR LRU LVS LAM LUF AVW FFZ OLS PEQ A39 PHX AZA MZJ AMA ROW SAD SAF ZSY SDX SOW SRR FHU TCS TUS TCC E25 INW CHI DLL Y72 OEB CID BMI CMI PWK MDW ORD RFD CWI 1CS MSN DKB C91 DBQ DPA 57C EKI FLD FWA 7A4 C16 10C GYY MKE GRR C02 IKK GUS VYS IOW RAC AZO ENW IGQ MWC LOT 24C MQB CGX MGC BUU MKG NBU PIA C65 C47 NPZ LAF MLI JOT 93C 06C SBM SMD SBN LWA BRL JVL BEH C89 LNR ANQ BIV DNV VOK ALO UGN UES OSH SLK ALB XZK AUG BGR ME5 BAF HYA BVY BID ASH ZTY BDL BTV K27 CAR HCC CON DXR IZG EPM MPV FIT GFL FOK BOS RME GON BHB HFD ZRT VSF HUL BDR ITH RKD LKP BED LWM LEB LEW ISP NY9 MHT 1B9 GHG MVY MSS MLT MVL ACK EWB ZVE UUU SFZ WFK PQI OWD OGS ONH MGJ FMH PSM PBG 2B2 PYM PWM PVC OQU 06N 4B8 RUT SFM 5B2 DAW ZSF ZTF SWF N69 SCH SYR TAN PVD HVN UCA OXC ART HPN WST CEF GTB IJD ORH OAJ ALX WAS ADW ACY ZRA BWI OQN BUY WWD CHO NKT EWN MQI PTB DOV W13 MRB ESN ECG EKN FAY FAF FFA FBG LWB HGR ISO NEL LFI ANP JYO LYH MNZ MTN MIV GAI SOP LBT ZRZ ILG PHF ORF NGU PNE NTU NHK ZBP ZFV PHL APG PGV POB NYG RDU RCZ RIC ROA RWI DCA SBY GSB SHD VAY 7N7 GED FME ZRD WAL IAD ZWU ZWI ILM  AIA ASE SPF WBU BKF FCS CNY APA CDR CYS E91 COS A50 CEZ DEN DRO EGE EGA RCA FNL FMN RIL GCC GJT GUC UT3 LHX LAA LAR MCK MTJ CPR LBF PGA PUB RAP RWL GLD BJC ALS SAA SNY SBS STK TEX BFF HDN ABI ADS LTS ADM GKY BAD CDS TKI RBD DFW DAL MKO DYS GGG AFW FTW DUC FSI INJ HBR BWD LAW HOB LBB GVT MLC LXY MAF MWL MLU SJT SPS SHV ELD BKD TXK TIK CNW TYR ACT OKC INK ARA AEX ALI AAP LFK RKP AUS BTR BMT DRI BRO ATT CLC CRP NGP COT CFD DWH DRT TPL CLL EFD ESF UVA IAH GTU GPT HBG PIB HDO HLR BIX ERV NQI SKF LFT LCH NEW LRD DLF CXO MSY BFM MOB NBG OZA PSX POE S46 RND GRK SAT GLS BPT SGR HRL VCT IWS HOU LEX TZR LOU FFT LUK CVG MGY MIE EVV 1OH FTK IND UMP DAY SME AOH LOZ SDF BLF OXD PKB BMG OSU OWB PBX CMH 4I7 BKW RID LCK SGH HUF HTS VNW ILN FFO CRW AAF X04 99N BFT BFT CEW BQK 70J NZC CDK CHS CLW CAE CTY CGC UDG DAB BGE 54J DTS DHN DQH VPS ORL FDW 55J XFL FLO GNV GGE CRE TMA HHH HXD SVN HRT OCF JKA JAX NIP 09J JES LCQ SSI VAD MGR LRO ZPH MYR 60J DWS MCO SFB OMN PFN ECP NPA PNS 40J JYL 2J9 3J1 6J4 SAV SSC ABY UST SGJ 24J TLH VDF TPA X39 TVI PAM VLD NSE CDN SPI BEC COU DEC DDC MKC 1H2 EMP FOE GCK GBN HYS HUT JEF OJC JLN MCI EOK IRK STL LWC AIZ LBL MHK FRI IAB JCI EWK TOP PNC UIN RVS STJ K83 SLN BLV 2H0 FLV SUS SGF CPS TUL END TBN EGT SZL ICT MWA AVX UDD BYS L35 BLH BUR BXS BLD POC SDM CXL CWT 49X NID INS EDW NJK NZJ RIR RBK FUL L06 SEE JGC GCN GCW 1G4 HSH IPL IYK HHR TRM SNA IGM HII IFP LPC LGB LAX RIV LAS CLD BFL NKX MHV LSV NZY VGT L52 ONT OXR PSP PMD NTD RNM REI RAL SBD SAN OLT SBP SBA SMX SMO VCV SGU TNX NXP VNY VBG WHP YUM TOA BTM 6S0 BIL BOI BMC BCE PUC CDC CTB GDV 4U9 DTA EKO ELY EVW FBR MLS SUN BZN FCA GTF HVR 36U HLN HIF IDA JAC SMN LWT OLF LGU TWF MLD MYL LVM MSO MUO U76 PIH PVU RIF RIW RKS SLC SHR SDY ISN VEL ENV GGW WRL WYS COD SPG X21 BOW BCT PGD OBE TNT X01 AVO MTH RBN FXE FLL GIF HST IMM TMB OCA EYW NQX ISM X07 LAL MCF MRK MLB COI MIA MPB APF HWO OPF FMY PHK LNA PBI COF PMP SEF SRQ F57 X26 X49 RSW TIX FRP PIE X59 VRB SUA LIT BYH PAH HRO BWG BKG HOP CGI 23M CKV CBM FYV FSM GTR GWO PBF JAN JBR MEI LRF MKL HOT MEM NMM GLH NQA BNA MSL XNA OLV POF RBM SIK UTM TUP ABR ISW APN IKV ATW GRB BDE BJI BIS BRD BKX GRI CWA YKN CVX TVC CIU EAU HIB MIC ESC DSM DVL DIK SUE DRM DLH EGV LYU 3D2 OMA INL FCM IMT 25D FOD 8M8 GDW IWD RDR GFK GRM GPZ FAR CMX HON ECA JMS EAR LSE ARV LNK ERY MCD MBL MFI MCW MNM MSP MIB MOT Y51 OFF OSC PLN PMB PIR PNM RHI RST HTL STC MQT FSD SUX SPW STE TVF ATY PCZ AUW 6Y8 MWM NYC CDW CXY CTH TSS ELM BGM 0W3 MDT HZL IDL JFK LGA LNS ABE LDJ WRI O03 MMU MUI EWR ZRP ZYP RDG 0P2 70N N53 TEB TTN N87 CZG SCE JRB JRA AVP WBW LHV IPT NXX BBX BAB CCR MER STS CIC NFL FAT HWD TVL NLC LHM LVK MAE MMH OAR MPI MCC MCE OAK MOD NUQ MRY APC NGZ SJC O27 E55 PAO RIU RDD RNO SAC SMF MHR SQL SFO SPZ SCK SUU TKF VIS DWA MYV CAK AKC AGC AOO ARB LBE 3G4 BFP FNT 41N BFD RMY BUF BKL CDI LAN DKK JHW CLE DET CGF DTW DUJ ERI PCW FDY 0G7 FZI FKL GQQ GVQ 7D9 CBE ROC SKY 29D 4G2 CKB PHD 2G2 JST 1G3 04G LPR LNN MFD MBS MGW IAG PTK PIT 4G0 17G 3W2 JXN 9G1 MTC 2G9 PHN TOL 3G3 HLG 0G6 SDC YIP 4G4 YNG OTS ACV AST BLI BYW BFI HQM PWT CLS CVO DHB CEC M94 PDT SKA SFF R49 6S2 FRD FBS MWH GRF TWD 0S9 KLS KEH LKE LMT S30 LWS LPS 38W EUG TCM MMV SLE ONP W04 OLM ESD 55S EAT COE 1RL NOW HIO PDX TTD S40 PUW RNT RDM RCE MFR RSJ SPB SEA PAE OTH GEG TIW PSC ALW WSX NUW CLM WIH YKM AND ANB AVL AHN FFC AUO AGS MLJ WDR 9A5 EKY BHM VPC PYP CLT 47A 4A7 CEU RYY CSG 9A1 CCO SEM CKF DNN DNL PDK MGE 0A9 SBO FZG MRN FTY 49A GSP 6A2 LZU HDI PIM ATL HKY HSV 4A9 19A ACJ DKX LGC LSF GVL CHA 52A 2A0 1A3 MXF TYS HQU MMI MCN EQY MGM 06A AIK GAD JZP GSO 4A4 HUA SVH RMG WRB RKH SES EET INT TOC TRI TCL DBN EUF CTJ", EWR_GDP$Dep.Airport[grepl("ALL", EWR_GDP$ARP_FIN)])
EWR_GDP$Dep.Airport[grepl("CZY_AP", EWR_GDP$ARP_FIN)] = gsub("CZY_AP", " CYHZ CYOW CYUL CYYZ CYTZ CYQB ", EWR_GDP$Dep.Airport[grepl("CZY_AP", EWR_GDP$ARP_FIN)])
EWR_GDP$Dep.Airport[grepl("FLL", EWR_GDP$Exempt.Dep.Facilities1)] = gsub("FLL",  "", EWR_GDP$Dep.Airport[grepl("FLL", EWR_GDP$Exempt.Dep.Facilities1)])
EWR_GDP$Dep.Airport[grepl("except", EWR_GDP$ARP_FIN)] = gsub("except",  "", EWR_GDP$Dep.Airport[grepl("except", EWR_GDP$ARP_FIN)])

write.csv(x = EWR_GDP, file="4th.csv")
#4th  part ends


#Match GDPs with flights (base file: IF)
n_GDP = nrow(EWR_GDP)
#next step takes a long time
for(I in 1:n_GDP) {
  EWRIF$GDPNO[EWRIF$datetime>= EWR_GDP$Derived.BgnDate.Time.UTC[i] & EWRIF$datetime<= EWR_GDP$Derived.EndDate.Time.UTC[i]]=i
}
EWRIF$GDPON[EWRIF$GDPNO != NA]=1
EWRIF$GDPON[is.na(EWRIF$GDPON)] = 0
write.csv(x = EWRIF, file="5th.csv")
#5th  part ends

EWRIF$sub2GDP[mapply(grepl, as.character(EWRIF$DEP_LOCID), EWR_GDP$Dep.Airport[EWRIF$GDPNO])] = 1
EWRIF$sub2GDP[is.na(EWRIF$sub2GDP)] = 0
EWRIF$ROOTGDP[EWRIF$sub2GDP == 1 & grepl("Yes", EWR_GDP$Is.RootAdvisory[EWRIF$GDPNO])] = 1
EWRIF$ROOTGDP [is.na(EWRIF$ROOTGDP)] = 0
EWRIF$GDPNO[is.na(EWRIF$GDPNO)] = 0
EWRIF1 = read.csv("EWRIF1.csv")
EWRIF2 = read.csv("EWRIF2.csv")
EWRIF1$ARR_YYYYMM = as.numeric(EWRIF1$ARR_YYYYMM)
focus = which(EWRIF1$ARR_YYYYMM >= 201001)
EWRIF0 = rbind(EWRIF1[focus,],EWRIF2)
focus = which(grepl("EWR", as.character(EWRIF0$ARR_LOCID)))
EWRIF = EWRIF0[focus,]
write.csv(x=EWRIF, file = "EWR_IF.csv")


EWRIF$ARR_YYYY = as.character(substr(EWRIF$ARR_YYYYMM,1,4))
EWRIF$ARR_MM = as.character(substr(EWRIF$ARR_YYYYMM,5,6))
EWRIF$datetime = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), as.character (EWRIF$SCHINTM)))
EWRIF$datetime = strptime(EWRIF$datetime, "%Y %m %d %H:%M", tz ="America/New_York")

EWR_GDP$NoFltAffected =0
EWR_GDP$IFDepAirport =""

for (i in 1:nrow(EWRIF)) {
  EWR_GDP$NoFltAffected[EWRIF$datetime[i]>= EWR_GDP$Eff.Bgn.Date.Time.UTC & EWRIF$datetime[i]<= EWR_GDP$Eff.End.Date.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )] = EWR_GDP$NoFltAffected[EWRIF$datetime[i]>= EWR_GDP$Eff.Bgn.Date.Time.UTC & EWRIF$datetime[i]<= EWR_GDP$Eff.End.Date.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )] +1
  
}
for (i in 1:nrow(EWRIF)) {
  
  EWR_GDP$IFDepAirport[EWRIF$datetime[i]>= EWR_GDP$Eff.Bgn.Date.Time.UTC & EWRIF$datetime[i]<= EWR_GDP$Eff.End.Date.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )] = paste (EWR_GDP$IFDepAirport[EWRIF$datetime[i]>= EWR_GDP$Eff.Bgn.Date.Time.UTC & EWRIF$datetime[i]<= EWR_GDP$Eff.End.Date.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )], as.character(EWRIF$DEP_LOCID[i]))
  
}

write.csv(x = EWR_GDP, file="6-all.csv")
6

EDCTON
EWRIF$ARR_YYYY = as.character(substr(EWRIF$ARR_YYYYMM,1,4))
EWRIF$ARR_MM = as.character(substr(EWRIF$ARR_YYYYMM,5,6))
EWRIF$EDCTONTM1=as.character(EWRIF$EDCTONTM)
EWRIF$EDCTdatetime = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), EWRIF$EDCTONTM1))
EWRIF$EDCTdatetime = strptime(EWRIF$EDCTdatetime, "%Y %m %d %H:%M", tz ="America/New_York")

EWR_GDP$Demand =0


for (i in 1:nrow(EWRIF)) {
  if(!is.na(EWRIF$EDCTdatetime[i])) {
    EWR_GDP$Demand[EWRIF$EDCTdatetime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$EDCTdatetime[i]<= EWR_GDP$Derived.EndDate.Time.UTC] = EWR_GDP$Demand[EWRIF$EDCTdatetime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$EDCTdatetime[i]<= EWR_GDP$Derived.EndDate.Time.UTC]+1
  }
}
write.csv(x = EWR_GDP, file="6-all-EDCT-derivedtime.csv")


for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniNoFltAffected[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$NoFltAffected[k]
    ROOT$IniIFDepAirport[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IFDepAirport[k]
    
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinNoFltAffected[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$NoFltAffected[k]
    ROOT$FinIFDepAirport[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IFDepAirport[k]
    
  }
}

for (k in 1:nrow(EWR_GDP)) {
  
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinDepScope[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Dep.Scope[k]
    
  }
}

for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniDemand[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Demand[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinDemand[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Demand[k]
  }
}



write.csv(x = ROOT, file="6-ROOT.csv")
6(2)
#6th  part ends

write.csv(x = ROOT, file="6EDCT-ROOT-derived.csv")
6(3)


#ROOT GDP
# Create ROOT file
EWR_GDP$RootAdvisoryNumber = as.numeric(EWR_GDP$RootAdvisoryNumber)
EWR_GDP$AdvisoryNumber = as.numeric(EWR_GDP$AdvisoryNumber)
EWR_GDP$RootAdvisoryDate.UTC = as.POSIXct(strptime(as.character(EWR_GDP$RootAdvisoryDate.UTC),"%Y-%m-%d",tz="GMT"))
ROOT <- data.frame(Date=as.Date(character()),
                   File=character(), 
                   User=character(), 
                   stringsAsFactors=FALSE)
focus = which(grepl("Yes", EWR_GDP$Is.RootAdvisory)) 
ROOT = EWR_GDP[focus,]
# Number of Revisions for each Root GDP
ROOT$NoRev=0
EWR_GDP$ROOTNO=0
ROOT$ROOTNO = c(1:nrow(ROOT))
for ( k in 1: nrow(ROOT)) { 
  EWR_GDP$ROOTNO[ROOT$RootAdvisoryDate.UTC[k] == EWR_GDP$RootAdvisoryDate.UTC & EWR_GDP$RootAdvisoryNumber == ROOT$RootAdvisoryNumber[k]] =k
  ROOT$NoRev[k] = nrow(EWR_GDP[which(EWR_GDP$ROOTNO == k & EWR_GDP$AdvisoryType == "GDP") ,]) - 1
}
write.csv(x = EWR_GDP, file="7th.csv")
#7th  part ends

# Advisory Number of included GDP for each root GDP
ROOT$InclGDPAdvisoryNo =""
ROOT$InclGDPRowNO=""
ROOT$RootAdvisoryNO = ROOT$RootAdvisoryNumber
for (i in 1:nrow(EWR_GDP)) {
  ROOT$InclGDPAdvisoryNo[ROOT$ROOTNO == EWR_GDP$ROOTNO[i] & EWR_GDP$AdvisoryType[i] == "GDP" & EWR_GDP$Is.RootAdvisory[i] == "No"] = paste(ROOT$InclGDPAdvisoryNo[ROOT$ROOTNO == EWR_GDP$ROOTNO[i] & EWR_GDP$AdvisoryType[i] == "GDP" & EWR_GDP$Is.RootAdvisory[i] == "No"], EWR_GDP$AdvisoryNumber[i], sep =",")
  ROOT$InclGDPRowNO[ROOT$ROOTNO == EWR_GDP$ROOTNO[i] & EWR_GDP$AdvisoryType[i] == "GDP" & EWR_GDP$Is.RootAdvisory[i] == "No"] = paste(ROOT$InclGDPRowNO[ROOT$ROOTNO == EWR_GDP$ROOTNO[i] & EWR_GDP$AdvisoryType[i] == "GDP" & EWR_GDP$Is.RootAdvisory[i] == "No"],  rownames(EWR_GDP)[i], sep =",")
}
# Initial start and end time
ROOT$InitialBgnTime = ROOT$Derived.BgnDate.Time.UTC
ROOT$InitialEndTime = ROOT$Derived.EndDate.Time.UTC

# Actual start and end time
ROOT$ActBgnTime = ROOT$Eff.Bgn.Date.Time.UTC
ROOT$ActEndTime= ROOT$Eff.Bgn.Date.Time.UTC

for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$ActEndTime[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Eff.End.Date.Time.UTC[k]
  }
  if (EWR_GDP$AdvisoryType[k] =="GDP CNX") {
    ROOT$ActEndTime[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Eff.Bgn.Date.Time.UTC[k]
  }
}

#Initial Duration (Root) & Actual Duration
ROOT$InitialDurationHrs= difftime( ROOT$InitialEndTime,  ROOT$InitialBgnTime, units = "hours")
ROOT$ActDurationHrs = difftime( ROOT$ActEndTime,  ROOT$ActBgnTime, units = "hours" )
#Diff Initial & Act Duration
ROOT$DiffActAndIniDurationHrs = ROOT$ActDurationHrs- ROOT$InitialDurationHrs
write.csv(x = ROOT, file="8th.csv")
#8th  part ends

#Original Origins(ROOT)
ROOT$OriginalOri = ROOT$ARP_FIN
ROOT$OriginalOriArp = ROOT$Dep.Airport
#Actual Origins
for (i in 1:nrow(EWR_GDP)) {
  if (EWR_GDP$AdvisoryType[i] =="GDP") {
    ROOT$ActOrigin[ROOT$ROOTNO == EWR_GDP$ROOTNO[i]] = EWR_GDP$ARP_FIN[i]
    ROOT$ActOriginArp[ROOT$ROOTNO == EWR_GDP$ROOTNO[i]] = EWR_GDP$Dep.Airport[i]
  }
}
#Root Cause
ROOT$RootCause = ROOT$Impacting.Condition
#Revision Causes
ROOT$RevCause = ""
for (i in 1:nrow(EWR_GDP)) {
  if (EWR_GDP$AdvisoryType[i] =="GDP" & EWR_GDP$Is.RootAdvisory[i] == "No" ) {
    ROOT$RevCause[ROOT$ROOTNO == EWR_GDP$ROOTNO[i]] = paste(ROOT$RevCause[ROOT$ROOTNO == EWR_GDP$ROOTNO[i]], EWR_GDP$Impacting.Condition[i], sep ="," )
  }
}
write.csv(x = ROOT, file="9th.csv")
#9th  part ends

#Initial Average Delay
ROOT$IntlAveDelayMins = as.numeric(as.character(ROOT$Average.Delay))
#Final Average Delay
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinAveDelayMins[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = as.numeric(as.character(EWR_GDP$Average.Delay[k]))
  }
}
#Diff Ave Delay
ROOT$DiffAveDelayMins = ROOT$FinAveDelayMins - ROOT$IntlAveDelayMins
#Initial Max Delay
ROOT$IntlMaxDelayMins = as.numeric(as.character(ROOT$Maximum.Delay))
#Final Max Delay
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinMaxDelayMins[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = as.numeric(as.character(EWR_GDP$Maximum.Delay[k]))
  }
}
#Diff Max Delay
ROOT$DiffMaxDelayMins = ROOT$FinMaxDelayMins - ROOT$IntlMaxDelayMins

write.csv(x = ROOT, file="10th.csv")
#10th  part ends

#Difference between Scope
ROOT$DiffScope[ROOT$InitialScope == ROOT$FinARP ] = "NoChange"
ROOT$DiffScope[grepl("ALL|ZAB", ROOT$InitialScope) & !grepl( "ALL|ZAB", ROOT$FinScope) ] = "ZAB"
ROOT$DiffScope[grepl("ALL|ZSE", ROOT$InitialScope) & !grepl( "ALL|ZSE", ROOT$FinScope) ] = "ZAB"


a = "ZAB ZSE ZFW ZKC ZME ZTL ZOA ZLC ZLA ZAU ZMP ZDV ZID ZMA ZHU ZJX ZBW ZOB ZDC ZNY"
ROOT$DiffScope[grepl("ALL", ROOT$InitialScope) & !grepl( "ALL", ROOT$FinScope) ] = sapply(setdiff(utf8ToInt(a), utf8ToInt(b)), intToUtf8)

#Number of flights affected (initial)
n_GDP = nrow(EWR_GDP)
for ( j in n_GDP:1) { 
  EWRIF$GDPNOforIntAffFlightNo[EWRIF$datetime>= EWR_GDP$Derived.BgnDate.Time.UTC[j] & EWRIF$datetime<= EWR_GDP$Derived.EndDate.Time.UTC[j]]=j
}
EWRIF$sub2GDPforIntAffFlightNo[mapply(grepl, as.character(EWRIF$DEP_LOCID), EWR_GDP$Dep.Airport[EWRIF$GDPNO])] = 1
EWRIF$sub2GDPforIntAffFlightNo[is.na(EWRIF$sub2GDP)] = 0

ROOT$InitalAffFltNo=0
for (k in 1:nrow(ROOT)) {
  if ( EWRIF$GDPNOforIntAffFlightNo[EWRIF$sub2GDPPforIntAffFlightNo==1] ==k) {
    ROOT$InitalAffFltNo[k] = ROOT$InitalAffFltNo[k]+1
  }
}
#Number of flights affected (Actual)
EWR_GDP$AffFltNo = 0
for (k in 1:nrow(EWR_GDP)) {
  if ( EWRIF$GDPNO[EWRIF$sub2GDP==1] ==k) {
    EWR_GDP$AffFltNo[k] = EWR_GDP$AffFltNo[k]+1
  }
}
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinAffFltNo[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$AffFltNo[k]
  }
}

#Initial Ave & max Rate
for (i in 1:nrow(EWR_GDP)) {
  if (grepl("/", EWR_GDP$ProgramRate[i])) {
    EWR_GDP$AveRate[i] = mean(as.numeric(unlist(strsplit(as.character(EWR_GDP$ProgramRate[i]), split ="/"))))
    EWR_GDP$MaxRate[i] = max(as.numeric(unlist(strsplit(as.character(EWR_GDP$ProgramRate[i]), split ="/"))))
  }
  if (!grepl("/", EWR_GDP$ProgramRate[i])) {
    EWR_GDP$AveRate[i] = as.numeric(as.character( EWR_GDP$ProgramRate[i]))
    EWR_GDP$MaxRate[i] = as.numeric(as.character( EWR_GDP$ProgramRate[i]))
  }
}

for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniAveRate[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$AveRate[k]
    ROOT$IniMaxRate[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$MaxRate[k]
  }
}
#Final Ave & Max Rate
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinAveRate[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$AveRate[k]
    ROOT$FinMaxRate[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$MaxRate[k]
  }
}
#difference Initial & Final Ave&Max Rate
ROOT$DiffAveRate = as.numeric(ROOT$FinAveRate) - as.numeric( ROOT$IniAveRate)
ROOT$DiffMaxRate = as.numeric(ROOT$FinMaxRate) - as.numeric( ROOT$IniMaxRate)

write.csv(x = ROOT, file="11th-r.csv")
write.csv(x = ROOT, file="11th-all.csv")
#11th  part ends

#QH AAR
#difference???

#Metar Weather
EWRMETAR=read.csv("EWRMETAR.csv")
EWRMETAR$start.time = as.POSIXct(strptime(as.character(EWRMETAR$start.time),"%Y-%m-%d %H:%M",tz="GMT"))
EWRMETAR$end.time = as.POSIXct(strptime(as.character(EWRMETAR$end.time),"%Y-%m-%d %H:%M",tz="GMT"))

attr(EWRMETAR$start.time, "tzone") <- "America/New_York"
attr(EWRMETAR$end.time, "tzone") <- "America/New_York"

#Combine several related columns
EWRMETAR$Snow = as.numeric(EWRMETAR$SN)+as.numeric(EWRMETAR$SG)
EWRMETAR$SnowYes[EWRMETAR$Snow>=1] = 1
EWRMETAR$SnowYes[EWRMETAR$Snow<1] = 0
EWRMETAR$SnowYes=as.character(EWRMETAR$SnowYes)

EWRMETAR$Ice = as.numeric(EWRMETAR$IC)+as.numeric(EWRMETAR$PL)+as.numeric(EWRMETAR$GR)+as.numeric(EWRMETAR$GS)
EWRMETAR$IceYes[EWRMETAR$Ice>=1] = 1
EWRMETAR$IceYes[EWRMETAR$Ice<1] = 0
EWRMETAR$IceYes=as.character(EWRMETAR$IceYes)

EWRMETAR$Fog = as.numeric(EWRMETAR$BR)+as.numeric(EWRMETAR$FG)+as.numeric(EWRMETAR$FU)
EWRMETAR$FogYes[EWRMETAR$Fog>=1] = 1
EWRMETAR$FogYes[EWRMETAR$Fog<1] = 0
EWRMETAR$FogYes=as.character(EWRMETAR$FogYes)

EWRMETAR$Rain = as.numeric(EWRMETAR$RA)+as.numeric(EWRMETAR$DZ)
EWRMETAR$RainYes[EWRMETAR$Rain>=1] = 1
EWRMETAR$RainYes[EWRMETAR$Rain<1] = 0
EWRMETAR$RainYes=as.character(EWRMETAR$RainYes)

EWRMETAR$Wind.Angle1= as.numeric(as.character(EWRMETAR$Wind.Angle))
EWRMETAR$Crosswind11_29 = round(as.numeric(EWRMETAR$Wind.Speed)*abs(sin((EWRMETAR$Wind.Angle1-110)*(pi/180))), digits = 0)
EWRMETAR$Crosswind4_22 = round(as.numeric(EWRMETAR$Wind.Speed)*abs(sin((EWRMETAR$Wind.Angle1-40)*(pi/180))), digits =0)
write.csv(x = EWRMETAR, file="12th-METAR-new.csv")
#12

#widen 30 min to start and end time

#All records
EWR_GDP$WindSpeedMETAR =""
EWR_GDP$WindAngleMETAR =""
EWR_GDP$VisibilityMETAR =""
EWR_GDP$FogMETAR =""
EWR_GDP$IceMETAR =""
EWR_GDP$CeilingMETAR =""
EWR_GDP$SnowMETAR =""
EWR_GDP$RainMETAR =""
EWR_GDP$ThunderstormMETAR =""
EWR_GDP$Crosswind422METAR = ""
EWR_GDP$Crosswind1129METAR=""

EWRMETAR$Wind.Angle = as.character(EWRMETAR$Wind.Angle)
EWRMETAR$Wind.Speed = as.character(EWRMETAR$Wind.Speed)
EWRMETAR$Crosswind11_29 = as.character(EWRMETAR$Crosswind11_29)
EWRMETAR$Crosswind4_22 = as.character(EWRMETAR$Crosswind4_22)
EWRMETAR$Visibility = as.character(EWRMETAR$Visibility)
EWRMETAR$Ceiling = as.character(EWRMETAR$Ceiling)
EWRMETAR$TS = as.character(EWRMETAR$TS)

for (j in 1:nrow(EWRMETAR)) {
  
  EWR_GDP$WindSpeedMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$WindSpeedMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$Wind.Speed[j])
  
  EWR_GDP$WindAngleMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$WindAngleMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$Wind.Angle [j])
  
  EWR_GDP$VisibilityMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$VisibilityMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$Visibility[j])
  
  EWR_GDP$CeilingMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$CeilingMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$Ceiling[j])
  
  EWR_GDP$SnowMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$SnowMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$SnowYes[j])
  
  EWR_GDP$RainMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$RainMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$RainYes[j])
  
  EWR_GDP$FogMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$FogMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$FogYes[j])
  
  EWR_GDP$IceMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$IceMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$IceYes[j])
  
  EWR_GDP$ThunderstormMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$ThunderstormMETAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$TS[j])
  
  EWR_GDP$Crosswind1129METAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$Crosswind1129METAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$Crosswind11_29[j])
  
  EWR_GDP$Crosswind422METAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= paste(EWR_GDP$Crosswind422METAR[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60], EWRMETAR$Crosswind4_22[j])
}
write.csv(x = EWR_GDP, file="12th-all-new.csv")
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniWindSpeedMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindSpeedMETAR[k]
    ROOT$IniCrosswind1129METAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129METAR[k]
    ROOT$IniCrosswind422METAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422METAR[k]
    ROOT$IniFogMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogMETAR[k]
    ROOT$IniIceMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceMETAR[k]
    ROOT$IniVisibilityMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityMETAR[k]
    ROOT$IniCeilingMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingMETAR[k]
    ROOT$IniRainMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainMETAR[k]
    ROOT$IniSnowMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowMETAR[k]
    ROOT$IniThunderstormMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormMETAR[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinWindSpeedMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindSpeedMETAR[k]
    ROOT$FinCrosswind1129METAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129METAR[k]
    ROOT$FinCrosswind422METAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422METAR[k]
    ROOT$FinFogMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogMETAR[k]
    ROOT$FinIceMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceMETAR[k]
    ROOT$FinVisibilityMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityMETAR[k]
    ROOT$FinCeilingMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingMETAR[k]
    ROOT$FinRainMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainMETAR[k]
    ROOT$FinSnowMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowMETAR[k]
    ROOT$FinThunderstormMETAR[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormMETAR[k]
  }
}
write.csv(x = ROOT, file="12th-r-new.csv")
#12th  part ends

#Worst weather
EWR_GDP$Crosswind1129METARworst = 0
EWR_GDP$Crosswind422METARworst = 0
EWR_GDP$VisibilityMETARworst = 10000
EWR_GDP$CeilingMETARworst = 10000
EWR_GDP$RainMETARworst =0
EWR_GDP$SnowMETARworst =0
EWR_GDP$IceMETARworst =0
EWR_GDP$FogMETARworst=0
EWR_GDP$ThunderstormMETARworst=0

EWRMETAR$Visibility = as.numeric(as.character(EWRMETAR$Visibility))
EWRMETAR$Ceiling= as.numeric(as.character(EWRMETAR$Ceiling))
EWRMETAR$RainYes= as.numeric(as.character(EWRMETAR$RainYes))
EWRMETAR$SnowYes= as.numeric(as.character(EWRMETAR$SnowYes))
EWRMETAR$IceYes= as.numeric(as.character(EWRMETAR$IceYes))
EWRMETAR$FogYes= as.numeric(as.character(EWRMETAR$FogYes))
EWRMETAR$TS= as.numeric(as.character(EWRMETAR$TS))
EWRMETAR$Crosswind11_29= as.numeric(as.character(EWRMETAR$Crosswind11_29))
EWRMETAR$Crosswind4_22= as.numeric(as.character(EWRMETAR$Crosswind4_22))

for (i in 1:nrow(EWR_GDP)) {
  if (grepl(" ", EWR_GDP$VisibilityMETAR[i])) {
    EWR_GDP$VisibilityMETARworst[i] = min(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$VisibilityMETAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$VisibilityMETAR [i])) {
    EWR_GDP$VisibilityMETARworst[i] = as.numeric(as.character( EWR_GDP$VisibilityMETAR[i]))
  }
  
  if (grepl(" ", EWR_GDP$CeilingMETAR[i])) {
    EWR_GDP$CeilingMETARworst[i] = min(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$CeilingMETAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$CeilingMETAR [i])) {
    EWR_GDP$CeilingMETARworst[i] = as.numeric(as.character( EWR_GDP$CeilingMETAR[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$RainMETAR[i])) {
    EWR_GDP$RainMETARworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$RainMETAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$RainMETAR [i])) {
    EWR_GDP$RainMETARworst[i] = as.numeric(as.character( EWR_GDP$RainMETAR[i]))
  }
  
  if (grepl(" ", EWR_GDP$SnowMETAR[i])) {
    EWR_GDP$SnowMETARworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$SnowMETAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$SnowMETAR [i])) {
    EWR_GDP$SnowMETARworst[i] = as.numeric(as.character( EWR_GDP$SnowMETAR[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$IceMETAR[i])) {
    EWR_GDP$IceMETARworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$IceMETAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$IceMETAR [i])) {
    EWR_GDP$IceMETARworst[i] = as.numeric(as.character( EWR_GDP$IceMETAR[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$FogMETAR[i])) {
    EWR_GDP$FogMETARworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$FogMETAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$FogMETAR [i])) {
    EWR_GDP$FogMETARworst[i] = as.numeric(as.character( EWR_GDP$FogMETAR[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$ThunderstormMETAR[i])) {
    EWR_GDP$ThunderstormMETARworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$ThunderstormMETAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$ThunderstormMETAR [i])) {
    EWR_GDP$ThunderstormMETARworst[i] = as.numeric(as.character( EWR_GDP$ThunderstormMETAR[i]))
  }
  
  if (grepl(" ", EWR_GDP$Crosswind1129METAR[i])) {
    EWR_GDP$Crosswind1129METARworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$Crosswind1129METAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$Crosswind1129METAR[i])){
    EWR_GDP$Crosswind1129METARworst[i] = as.numeric(as.character( EWR_GDP$Crosswind1129METAR[i]))
  }
  
  if (grepl(" ", EWR_GDP$Crosswind422METAR[i])) {
    EWR_GDP$Crosswind422METARworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$Crosswind422METAR[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$Crosswind422METAR[i])){
    EWR_GDP$Crosswind422METARworst[i] = as.numeric(as.character( EWR_GDP$Crosswind422METAR[i]))
  }
  
}

write.csv(x = EWR_GDP, file="13th-al-newl.csv")
#12-new

for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniVisibilityMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityMETARworst[k]
    ROOT$IniIceMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceMETARworst[k]
    ROOT$IniFogMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogMETARworst[k]
    ROOT$IniCeilingMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingMETARworst[k]
    ROOT$IniRainMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainMETARworst[k]
    ROOT$IniSnowMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowMETARworst[k]
    ROOT$IniThunderstormMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormMETARworst[k]
    ROOT$IniCrosswind1129METARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129METARworst[k]
    ROOT$IniCrosswind422METARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422METARworst[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinWindAngleMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindAngleMETARworst[k]
    ROOT$FinVisibilityMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityMETARworst[k]
    ROOT$FinCeilingMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingMETARworst[k]
    ROOT$FinRainMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainMETARworst[k]
    ROOT$FinSnowMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowMETARworst[k]
    ROOT$FinThunderstormMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormMETARworst[k]
    ROOT$FinIceMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceMETARworst[k]
    ROOT$FinFogMETARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogMETARworst[k]
    ROOT$FinCrosswind1129METARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129METARworst[k]
    ROOT$FinCrosswind422METARworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422METARworst[k]
  }
}
write.csv(x = ROOT, file="13th-r-new.csv")
#13th  part ends
#12-new


#Average weather 
EWRMETAR$timediff = difftime( EWRMETAR$end.time[j], EWRMETAR$start.time[j], units = "hours")
EWRMETAR$Crosswind1129Xtimediff = as.numeric(as.character(EWRMETAR$Crosswind11_29)) * EWRMETAR$timediff
EWRMETAR$Crosswind422Xtimediff = as.numeric(as.character(EWRMETAR$Crosswind4_22)) * EWRMETAR$timediff
EWRMETAR$VisibilityXtimediff = as.numeric(as.character(EWRMETAR$Visibility)) * EWRMETAR$timediff
EWRMETAR$CeilingXtimediff = as.numeric(as.character(EWRMETAR$Ceiling)) * EWRMETAR$timediff

EWR_GDP$METARtimeLength =0
EWR_GDP$METARCrosswind1129Xtimediff=0
#(other variables should be added later)

for (j in 1:nrow(EWRMETAR)) {
  EWR_GDP$METARtimeLength[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= EWR_GDP$METARtimeLength[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]+ EWRMETAR$timediff[j]
  if (!grepl("NA", as.character(EWRMETAR$Crosswind1129Xtimediff[j]))) {
    EWR_GDP$METARCrosswind1129Xtimediff[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= EWR_GDP$METARCrosswind1129Xtimediff[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]+ as.numeric(EWRMETAR$Crosswind1129Xtimediff[j])
  }
}
EWR_GDP$Crosswind1129METARave=EWR_GDP$METARCrosswind1129Xtimediff/EWR_GDP$METARtimeLength
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniCrosswind1129METARave[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129METARave[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinCrosswind1129METARave[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129METARave[k]
  }
}

#Counts of Rain, Snow 
EWR_GDP$RainMETARcount =0
EWR_GDP$SnowMETARcount=0

for (j in 1:nrow(EWRMETAR)) {
  if (EWRMETAR$RainYes[j]>0) {
    EWR_GDP$RainMETARcount[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= EWR_GDP$RainMETARcount[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60] +1
  }
  if (EWRMETAR$SnowYes[j]>0) {
    EWR_GDP$SnowMETARcount[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60]= EWR_GDP$SnowMETARcount[EWR_GDP$Eff.Bgn.Date.Time.UTC<= EWRMETAR$start.time[j]+30*60 & EWR_GDP$Eff.End.Date.Time.UTC>= EWRMETAR$end.time[j]-30*60] +1
  }
}
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniRainMETARcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainMETARcount[k]
    ROOT$IniSnowMETARcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainMETARcount[k]
    
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinRainMETARcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainMETARcount[k]
    ROOT$FinSnowMETARcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowMETARcount[k]
    
  }
}


write.csv(x = EWR_GDP, file="14th-all-new.csv")
write.csv(x = ROOT, file="14th-r-new.csv")
write.csv(x = EWRMETAR, file="14th-MEATR-new.csv")
#14th  part ends
#14-new

#Taf Weather 
EWRTAF=read.csv("EWRTAF.csv")
EWRTAF$start.time = as.character(paste(as.character(EWRTAF$From.Year), as.character(EWRTAF$From.Month), as.character(EWRTAF$From.Day), as.character (EWRTAF$From.Hour), as.character (EWRTAF$From.Minute)))
EWRTAF$end.time = as.character(paste(as.character(EWRTAF$To.Year), as.character(EWRTAF$To.Month), as.character(EWRTAF$To.Day), as.character (EWRTAF$To.Hour), as.character (EWRTAF$To.Minute)))

EWRTAF$start.time = strptime(EWRTAF$start.time, "%Y %m %d %H %M", tz ="GMT")
EWRTAF$start.time = format(EWRTAF$start.time, format = "%Y-%m-%d %H:%M", tz ="GMT" )
EWRTAF$start.time = as.POSIXct(strptime(as.character(EWRTAF$start.time),"%Y-%m-%d %H:%M",tz="GMT"))

EWRTAF$end.time = strptime(EWRTAF$end.time, "%Y %m %d %H %M", tz ="GMT")
EWRTAF$end.time = format(EWRTAF$end.time, format = "%Y-%m-%d %H:%M", tz ="GMT" )
EWRTAF$end.time = as.POSIXct(strptime(as.character(EWRTAF$end.time),"%Y-%m-%d %H:%M",tz="GMT"))


attr(EWRTAF$start.time, "tzone") <- "America/New_York"
attr(EWRTAF$end.time, "tzone") <- "America/New_York"

#Combine several related columns
EWRTAF$Snow = as.numeric(EWRTAF$SN)+as.numeric(EWRTAF$SG)
EWRTAF$SnowYes[EWRTAF$Snow>=1] = 1
EWRTAF$SnowYes[EWRTAF$Snow<1] = 0
EWRTAF$SnowYes=as.character(EWRTAF$SnowYes)

EWRTAF$Ice = as.numeric(EWRTAF$IC)+as.numeric(EWRTAF$PL)+as.numeric(EWRTAF$GR)+as.numeric(EWRTAF$GS)
EWRTAF$IceYes[EWRTAF$Ice>=1] = 1
EWRTAF$IceYes[EWRTAF$Ice<1] = 0
EWRTAF$IceYes=as.character(EWRTAF$IceYes)

EWRTAF$Fog = as.numeric(EWRTAF$BR)+as.numeric(EWRTAF$FG)+as.numeric(EWRTAF$FU)
EWRTAF$FogYes[EWRTAF$Fog>=1] = 1
EWRTAF$FogYes[EWRTAF$Fog<1] = 0
EWRTAF$FogYes=as.character(EWRTAF$FogYes)

EWRTAF$Rain = as.numeric(EWRTAF$RA)+as.numeric(EWRTAF$DZ)
EWRTAF$RainYes[EWRTAF$Rain>=1] = 1
EWRTAF$RainYes[EWRTAF$Rain<1] = 0
EWRTAF$RainYes=as.character(EWRTAF$RainYes)

EWRTAF$Wind.Angle1= as.numeric(as.character(EWRTAF$Wind.Angle))
EWRTAF$Crosswind11_29 = round(as.numeric(EWRTAF$Wind.Speed)*abs(sin((EWRTAF$Wind.Angle1-110)*(pi/180))), digits =0)
EWRTAF$Crosswind4_22 = round(as.numeric(EWRTAF$Wind.Speed)*abs(sin((EWRTAF$Wind.Angle1-40)*(pi/180))), digits = 0)
write.csv(x = EWRTAF, file="15th-TAF-new.csv")


#widen 30 min to start and end time
#All records
EWR_GDP$WindSpeedTAF =""
EWR_GDP$WindAngleTAF =""
EWR_GDP$VisibilityTAF =""
EWR_GDP$FogTAF =""
EWR_GDP$IceTAF =""
EWR_GDP$CeilingTAF =""
EWR_GDP$SnowTAF =""
EWR_GDP$RainTAF =""
EWR_GDP$ThunderstormTAF =""
EWR_GDP$Crosswind422TAF = ""
EWR_GDP$Crosswind1129TAF=""

EWRTAF$Wind.Angle = as.character(EWRTAF$Wind.Angle)
EWRTAF$Wind.Speed = as.character(EWRTAF$Wind.Speed)
EWRTAF$Crosswind11_29 = as.character(EWRTAF$Crosswind11_29)
EWRTAF$Crosswind4_22 = as.character(EWRTAF$Crosswind4_22)
EWRTAF$Visibility = as.character(EWRTAF$Visibility)
EWRTAF$Ceiling = as.character(EWRTAF$Ceiling)
EWRTAF$TS = as.character(EWRTAF$TS)

for (j in 1:nrow(EWRTAF)) {
  
  EWR_GDP$WindSpeedTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$WindSpeedTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Wind.Speed[j])
  
  EWR_GDP$WindAngleTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$WindAngleTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Wind.Angle [j])
  
  EWR_GDP$VisibilityTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$VisibilityTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Visibility[j])
  
  EWR_GDP$CeilingTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$CeilingTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Ceiling[j])
  
  EWR_GDP$SnowTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$SnowTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$SnowYes[j])
  
  EWR_GDP$RainTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$RainTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$RainYes[j])
  
  EWR_GDP$FogTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$FogTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$FogYes[j])
  
  EWR_GDP$IceTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$IceTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$IceYes[j])
  
  EWR_GDP$ThunderstormTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$ThunderstormTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$TS[j])
  
  EWR_GDP$Crosswind1129TAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$Crosswind1129TAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Crosswind11_29[j])
  
  EWR_GDP$Crosswind422TAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$Crosswind422TAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Crosswind4_22[j])
}
write.csv(x = EWR_GDP, file="15th-all-new.csv")
#15-1

for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniWindSpeedTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindSpeedTAF[k]
    ROOT$IniCrosswind1129TAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129TAF[k]
    ROOT$IniCrosswind422TAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422TAF[k]
    ROOT$IniFogTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogTAF[k]
    ROOT$IniIceTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceTAF[k]
    ROOT$IniVisibilityTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityTAF[k]
    ROOT$IniCeilingTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingTAF[k]
    ROOT$IniRainTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAF[k]
    ROOT$IniSnowTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowTAF[k]
    ROOT$IniThunderstormTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormTAF[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinWindSpeedTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindSpeedTAF[k]
    ROOT$FinCrosswind1129TAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129TAF[k]
    ROOT$FinCrosswind422TAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422TAF[k]
    ROOT$FinFogTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogTAF[k]
    ROOT$FinIceTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceTAF[k]
    ROOT$FinVisibilityTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityTAF[k]
    ROOT$FinCeilingTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingTAF[k]
    ROOT$FinRainTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAF[k]
    ROOT$FinSnowTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowTAF[k]
    ROOT$FinThunderstormTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormTAF[k]
  }
}
write.csv(x = ROOT, file="15th-r-NEW.csv")
#15-2

#Worst weather
EWR_GDP$Crosswind1129TAFworst = 0
EWR_GDP$Crosswind422TAFworst = 0
EWR_GDP$VisibilityTAFworst = 10000
EWR_GDP$CeilingTAFworst = 10000
EWR_GDP$RainTAFworst =0
EWR_GDP$SnowTAFworst =0
EWR_GDP$IceTAFworst =0
EWR_GDP$FogTAFworst=0
EWR_GDP$ThunderstormTAFworst=0

EWRTAF$Visibility = as.numeric(as.character(EWRTAF$Visibility))
EWRTAF$Ceiling= as.numeric(as.character(EWRTAF$Ceiling))
EWRTAF$RainYes= as.numeric(as.character(EWRTAF$RainYes))
EWRTAF$SnowYes= as.numeric(as.character(EWRTAF$SnowYes))
EWRTAF$IceYes= as.numeric(as.character(EWRTAF$IceYes))
EWRTAF$FogYes= as.numeric(as.character(EWRTAF$FogYes))
EWRTAF$TS= as.numeric(as.character(EWRTAF$TS))
EWRTAF$Crosswind11_29= as.numeric(as.character(EWRTAF$Crosswind11_29))
EWRTAF$Crosswind4_22= as.numeric(as.character(EWRTAF$Crosswind4_22))

for (i in 1:nrow(EWR_GDP)) {
  if (grepl(" ", EWR_GDP$VisibilityTAF[i])) {
    EWR_GDP$VisibilityTAFworst[i] = min(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$VisibilityTAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$VisibilityTAF [i])) {
    EWR_GDP$VisibilityTAFworst[i] = as.numeric(as.character( EWR_GDP$VisibilityTAF[i]))
  }
  
  if (grepl(" ", EWR_GDP$CeilingTAF[i])) {
    EWR_GDP$CeilingTAFworst[i] = min(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$CeilingTAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$CeilingTAF [i])) {
    EWR_GDP$CeilingTAFworst[i] = as.numeric(as.character( EWR_GDP$CeilingTAF[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$RainTAF[i])) {
    EWR_GDP$RainTAFworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$RainTAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$RainTAF [i])) {
    EWR_GDP$RainTAFworst[i] = as.numeric(as.character( EWR_GDP$RainTAF[i]))
  }
  
  if (grepl(" ", EWR_GDP$SnowTAF[i])) {
    EWR_GDP$SnowTAFworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$SnowTAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$SnowTAF [i])) {
    EWR_GDP$SnowTAFworst[i] = as.numeric(as.character( EWR_GDP$SnowTAF[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$IceTAF[i])) {
    EWR_GDP$IceTAFworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$IceTAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$IceTAF [i])) {
    EWR_GDP$IceTAFworst[i] = as.numeric(as.character( EWR_GDP$IceTAF[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$FogTAF[i])) {
    EWR_GDP$FogTAFworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$FogTAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$FogTAF [i])) {
    EWR_GDP$FogTAFworst[i] = as.numeric(as.character( EWR_GDP$FogTAF[i]))
  }
  
  
  if (grepl(" ", EWR_GDP$ThunderstormTAF[i])) {
    EWR_GDP$ThunderstormTAFworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$ThunderstormTAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$ThunderstormTAF [i])) {
    EWR_GDP$ThunderstormTAFworst[i] = as.numeric(as.character( EWR_GDP$ThunderstormTAF[i]))
  }
  
  if (grepl(" ", EWR_GDP$Crosswind1129TAF[i])) {
    EWR_GDP$Crosswind1129TAFworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$Crosswind1129TAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$Crosswind1129TAF[i])){
    EWR_GDP$Crosswind1129TAFworst[i] = as.numeric(as.character( EWR_GDP$Crosswind1129TAF[i]))
  }
  
  if (grepl(" ", EWR_GDP$Crosswind422TAF[i])) {
    EWR_GDP$Crosswind422TAFworst[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$Crosswind422TAF[i]), split =" ")))))
  }
  if (!grepl(" ", EWR_GDP$Crosswind422TAF[i])){
    EWR_GDP$Crosswind422TAFworst[i] = as.numeric(as.character( EWR_GDP$Crosswind422TAF[i]))
  }
  
}

write.csv(x = EWR_GDP, file="16th-all.csv")	
write.csv(x = EWRTAF, file="16th-TAF.csv")
# 16- 1
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniVisibilityTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityTAFworst[k]
    ROOT$IniIceTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceTAFworst[k]
    ROOT$IniFogTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogTAFworst[k]
    ROOT$IniCeilingTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingTAFworst[k]
    ROOT$IniRainTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAFworst[k]
    ROOT$IniSnowTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowTAFworst[k]
    ROOT$IniThunderstormTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormTAFworst[k]
    ROOT$IniCrosswind1129TAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129TAFworst[k]
    ROOT$IniCrosswind422TAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422TAFworst[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinWindAngleTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindAngleTAFworst[k]
    ROOT$FinVisibilityTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityTAFworst[k]
    ROOT$FinCeilingTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingTAFworst[k]
    ROOT$FinRainTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAFworst[k]
    ROOT$FinSnowTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowTAFworst[k]
    ROOT$FinThunderstormTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormTAFworst[k]
    ROOT$FinIceTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$IceTAFworst[k]
    ROOT$FinFogTAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$FogTAFworst[k]
    ROOT$FinCrosswind1129TAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind1129TAFworst[k]
    ROOT$FinCrosswind422TAFworst[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$Crosswind422TAFworst[k]
  }
}
write.csv(x = ROOT, file="16th-r.csv")
#16th -2
#Average weather 

#Counts of Rain, Snow (Rain only)
EWR_GDP$RainTAFcount =0
EWR_GDP$SnowTAFcount=0

for (j in 1:nrow(EWRTAF)) {
  if (EWRTAF$RainYes[j]>0) {
    EWR_GDP$RainTAFcount[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= EWR_GDP$RainTAFcount[EWR_GDP$Derived.BgnDate.Time.UTC <= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC >= EWRTAF$end.time[j]-30*60] +1
  }
  if (EWRTAF$SnowYes[j]>0) {
    EWR_GDP$SnowTAFcount[EWR_GDP$Derived.BgnDate.Time.UTC <= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC >= EWRTAF$end.time[j]-30*60]= EWR_GDP$SnowTAFcount[EWR_GDP$Derived.BgnDate.Time.UTC <= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC >= EWRTAF$end.time[j]-30*60] +1
  }
}
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniRainTAFcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAFcount[k]
    ROOT$IniSnowTAFcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAFcount[k]
    
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinRainTAFcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAFcount[k]
    ROOT$FinSnowTAFcount[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowTAFcount[k]
    
  }
}


write.csv(x = EWR_GDP, file="17th-all.csv")
write.csv(x = ROOT, file="17th-r.csv")
write.csv(x = EWRTAF, file="17th-TAF.csv")
#17th  part ends

EWR_GDP

EWR_GDP$WindAngleTAF =""
EWR_GDP$WindSpeedTAF =""
EWR_GDP$VisibilityTAF =""
EWR_GDP$CeilingTAF =""
EWR_GDP$SnowTAF =""
EWR_GDP$RainTAF =""
EWR_GDP$ThunderstormTAF =""

EWRTAF$Wind.Angle = as.character(EWRTAF$Wind.Angle)
EWRTAF$Wind.Speed = as.character(EWRTAF$Wind.Speed)
EWRTAF$Visibility = as.character(EWRTAF$Visibility)
EWRTAF$Ceiling = as.character(EWRTAF$Ceiling)
EWRTAF$TS = as.character(EWRTAF$TS)

#widen 30 min to start and end time
#Use Called time since TAF is forcast
for (j in 1:nrow(EWRTAF)) {
  EWR_GDP$WindSpeedTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$WindSpeedTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Wind.Speed[j])
}
for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniWindSpeedTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindSpeedTAF[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinWindSpeedTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindSpeedTAF[k]
  }
}
#Combine several snow/rain-related columns
EWRTAF$Snow = as.numeric(EWRTAF$SN)+as.numeric(EWRTAF$SG)+as.numeric(EWRTAF$IC)+as.numeric(EWRTAF$PL)+as.numeric(EWRTAF$GR)+as.numeric(EWRTAF$GS)+as.numeric(EWRTAF$TS)+as.numeric(EWRTAF$UP)
EWRTAF$SnowYes[EWRTAF$Snow>=1] = 1
EWRTAF$SnowYes[EWRTAF$Snow<1] = 0
EWRTAF$SnowYes=as.character(EWRTAF$SnowYes)
EWRTAF$Rain = as.numeric(EWRTAF$RA)+as.numeric(EWRTAF$DZ)
EWRTAF$RainYes[EWRTAF$Rain>=1] = 1
EWRTAF$RainYes[EWRTAF$Rain<1] = 0
EWRTAF$RainYes=as.character(EWRTAF$RainYes)

for (j in 1:nrow(EWRTAF)) {
  EWR_GDP$WindAngleTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$WindAngleTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Wind.Angle[j])
  
  EWR_GDP$VisibilityTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$VisibilityTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Visibility[j])
  
  EWR_GDP$CeilingTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$CeilingTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$Ceiling[j])
  
  EWR_GDP$SnowTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$SnowTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$SnowYes[j])
  
  EWR_GDP$RainTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$RainTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$RainYes[j])
  
  EWR_GDP$ThunderstormTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60]= paste(EWR_GDP$ThunderstormTAF[EWR_GDP$Derived.BgnDate.Time.UTC<= EWRTAF$start.time[j]+30*60 & EWR_GDP$Derived.EndDate.Time.UTC>= EWRTAF$end.time[j]-30*60], EWRTAF$TS[j])
}
write.csv(x = EWR_GDP, file="14th-all.csv")

for (k in 1:nrow(EWR_GDP)) {
  if  (EWR_GDP$AdvisoryType[k] =="GDP" & EWR_GDP$Is.RootAdvisory[k] =="Yes"  ) {
    ROOT$IniWindAngleTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindAngleTAF[k]
    ROOT$IniVisibilityTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityTAF[k]
    ROOT$IniCeilingTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingTAF[k]
    ROOT$IniRainTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAF[k]
    ROOT$IniSnowTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowTAF[k]
    ROOT$IniThunderstormTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormTAF[k]
  }
  if  (EWR_GDP$AdvisoryType[k] =="GDP") {
    ROOT$FinWindAngleTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$WindAngleTAF[k]
    ROOT$FinVisibilityTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$VisibilityTAF[k]
    ROOT$FinCeilingTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$CeilingTAF[k]
    ROOT$FinRainTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$RainTAF[k]
    ROOT$FinSnowTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$SnowTAF[k]
    ROOT$FinThunderstormTAF[EWR_GDP$ROOTNO[k] == ROOT$ROOTNO] = EWR_GDP$ThunderstormTAF[k]
  }
}

write.csv(x = ROOT, file="14th-r.csv")
#14th  part ends






#initial total flights: SCHINTM vs. initial GDP time

#IF dataset
EWRIF1 = read.csv("EWRIF1.csv")
EWRIF2 = read.csv("EWRIF2.csv")
EWRIF1$ARR_YYYYMM = as.numeric(EWRIF1$ARR_YYYYMM)
focus = which(EWRIF1$ARR_YYYYMM >= 201001)
EWRIF0 = rbind(EWRIF1[focus,],EWRIF2)
focus = which(grepl("EWR", as.character(EWRIF0$ARR_LOCID)))
EWRIF = EWRIF0[focus,]
write.csv(x=EWRIF, file = "EWR_IF.csv")

EWRIF$ARR_YYYY = as.character(substr(EWRIF$ARR_YYYYMM,1,4))
EWRIF$ARR_MM = as.character(substr(EWRIF$ARR_YYYYMM,5,6))
EWRIF$schintime = as.character(paste(EWRIF$ARR_YYYY, EWRIF$ARR_MM, as.character(EWRIF$ARR_DAY), as.character (EWRIF$SCHINTM)))
EWRIF$schintime = strptime(EWRIF$schintime, "%Y %m %d %H:%M", tz ="America/New_York")

EWRIF$DEP_LOCID = as.character(EWRIF$DEP_LOCID)

#Total flights
EWR_GDP$totFlt = 0
for (i in 1:nrow(EWRIF)) {
  EWR_GDP$totFlt[EWRIF$schintime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$schintime[i]<= EWR_GDP$Derived.EndDate.Time.UTC] = EWR_GDP$totFlt[EWRIF$schintime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$schintime[i]<= EWR_GDP$Derived.EndDate.Time.UTC] + 1
}

#Affected Flights
EWR_GDP$AffectedFlt = 0

for (i in 1:nrow(EWRIF)) {
  EWR_GDP$AffectedFlt[EWRIF$schintime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$schintime[i]<= EWR_GDP$Derived.EndDate.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )] = EWR_GDP$AffectedFlt[EWRIF$schintime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$schintime[i]<= EWR_GDP$Derived.EndDate.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )] +1
  
  #18th-all
  write.csv(x = EWR_GDP, file="18th-all.csv")
  
  #total delay
  EWRIF$DLASCHARR=as.character(EWRIF$DLASCHARR)
  EWR_GDP$delay= ""
  
  for (i in 1:nrow(EWRIF)) {
    EWR_GDP$delay[EWRIF$schintime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$schintime[i]<= EWR_GDP$Derived.EndDate.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )] = paste (EWR_GDP$delay[EWRIF$schintime[i]>= EWR_GDP$Derived.BgnDate.Time.UTC & EWRIF$schintime[i]<= EWR_GDP$Derived.EndDate.Time.UTC & grepl(as.character(EWRIF$DEP_LOCID[i]), as.character(EWR_GDP$Dep.Airport) )], EWRIF$DLASCHARR
                                                                                                                                                                                                                        [i])
    EWR_GDP$delayMax=0
    EWR_GDP$delayAve=0
    
    for (i in 1:nrow(EWR_GDP)) {
      if (grepl(" ", EWR_GDP$delay[i])) {
        EWR_GDP$delayMax[i] = max(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$EWR_GDP$delay[i]), split =" ")))))
        
        EWR_GDP$delayAve[i]= mean(na.omit(as.numeric(unlist(strsplit(as.character(EWR_GDP$EWR_GDP$delay[i]), split =" ")))))
      }
      
      if (!grepl(" ", EWR_GDP$EWR_GDP$delay[i])) {
        EWR_GDP$delayAve[i] = as.numeric(as.character(EWR_GDP$delay[i]))
      }
    }
    
    #19th-all
    write.csv(x = EWR_GDP, file="19th-all.csv")
    
    
    #Duration
    
    #No Modification
    ROOT$NoMod=0
    EWR_GDP$ROOTNO=0
    ROOT$ROOTNO = c(1:nrow(ROOT))
    for ( k in 1: nrow(ROOT)) { 
      EWR_GDP$ROOTNO[ROOT$RootAdvisoryDate.UTC[k] == EWR_GDP$RootAdvisoryDate.UTC & EWR_GDP$RootAdvisoryNumber == ROOT$RootAdvisoryNumber[k]] =k
      ROOT$NoMod[k] = nrow(EWR_GDP[which(EWR_GDP$ROOTNO == k & EWR_GDP$AdvisoryCategory == "GDP") ,]) - 1
    }
