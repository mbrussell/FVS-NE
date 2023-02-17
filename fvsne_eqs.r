
DBH=seq(1,20,b=1)
SPP=c("RM","SM","RS","BF")
tree=as.data.frame(merge(SPP,DBH))
names(tree)[1]='SPP'; names(tree)[2]='DBH'

#Species function
FVSNE.SPP=function(SPP){
     if(SPP=='BF'){HTDBH_EQ='C';Dbw=0.1;SPPGR=1; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='TA'){HTDBH_EQ='C';Dbw=0.1;SPPGR=2; BAMAX=200; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='WS'){HTDBH_EQ='C';Dbw=0.2;SPPGR=3; BAMAX=190; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='RS'){HTDBH_EQ='C';Dbw=0.2;SPPGR=4; BAMAX=260; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='NS'){HTDBH_EQ='C';Dbw=0.2;SPPGR=4; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Krajicek'}
else if(SPP=='BS'){HTDBH_EQ='C';Dbw=0.2;SPPGR=4; BAMAX=180; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='PI'){HTDBH_EQ='C';Dbw=0.2;SPPGR=4; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='RN'){HTDBH_EQ='C';Dbw=0.1;SPPGR=5; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='WP'){HTDBH_EQ='C';Dbw=0.4;SPPGR=6; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='LP'){HTDBH_EQ='W';Dbw=0.5;SPPGR=7; BAMAX=210; LCW_EQ='Bechtold'; MCW_EQ='Smith'}
else if(SPP=='VP'){HTDBH_EQ='W';Dbw=0.5;SPPGR=8; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='WC'){HTDBH_EQ='W';Dbw=0.1;SPPGR=9; BAMAX=200; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='AW'){HTDBH_EQ='W';Dbw=0.1;SPPGR=9; BAMAX=200; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='RC'){HTDBH_EQ='W';Dbw=0.5;SPPGR=9; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='OC'){HTDBH_EQ='W';Dbw=0.5;SPPGR=9; BAMAX=200; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='EH'){HTDBH_EQ='C';Dbw=0.1;SPPGR=10; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='HM'){HTDBH_EQ='W';Dbw=0.1;SPPGR=10; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='OP'){HTDBH_EQ='W';Dbw=0.5;SPPGR=11; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='JP'){HTDBH_EQ='W';Dbw=0.1;SPPGR=11; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='SP'){HTDBH_EQ='W';Dbw=0.4;SPPGR=11; BAMAX=210; LCW_EQ='Bechtold'; MCW_EQ='Smith'}
else if(SPP=='TM'){HTDBH_EQ='W';Dbw=0.5;SPPGR=11; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PP'){HTDBH_EQ='C';Dbw=0.5;SPPGR=11; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PD'){HTDBH_EQ='W';Dbw=0.5;SPPGR=11; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SC'){HTDBH_EQ='W';Dbw=0.5;SPPGR=11; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='OS'){HTDBH_EQ='W';Dbw=0.3;SPPGR=11; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='RM'){HTDBH_EQ='W';Dbw=0.2;SPPGR=12; BAMAX=190; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='SM'){HTDBH_EQ='W';Dbw=0.2;SPPGR=13; BAMAX=190; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='BM'){HTDBH_EQ='C';Dbw=0.2;SPPGR=13; BAMAX=190; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='SV'){HTDBH_EQ='C';Dbw=0.2;SPPGR=13; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='YB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=14; BAMAX=200; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=14; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='RB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=14; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=15; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='GB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=15; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='HI'){HTDBH_EQ='W';Dbw=0.3;SPPGR=16; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='PH'){HTDBH_EQ='W';Dbw=0.3;SPPGR=16; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SL'){HTDBH_EQ='W';Dbw=0.3;SPPGR=16; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='SH'){HTDBH_EQ='W';Dbw=0.3;SPPGR=16; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='MH'){HTDBH_EQ='W';Dbw=0.3;SPPGR=16; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='AB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=17; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='AS'){HTDBH_EQ='W';Dbw=0.2;SPPGR=18; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='WA'){HTDBH_EQ='W';Dbw=0.2;SPPGR=18; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BA'){HTDBH_EQ='W';Dbw=0.2;SPPGR=18; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='GA'){HTDBH_EQ='W';Dbw=0.2;SPPGR=18; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='PA'){HTDBH_EQ='W';Dbw=0.2;SPPGR=18; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='YP'){HTDBH_EQ='C';Dbw=0.2;SPPGR=19; BAMAX=180; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SU'){HTDBH_EQ='W';Dbw=0.2;SPPGR=19; BAMAX=140; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='CT'){HTDBH_EQ='C';Dbw=0.2;SPPGR=19; BAMAX=180; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='QA'){HTDBH_EQ='W';Dbw=0.3;SPPGR=20; BAMAX=130; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='BP'){HTDBH_EQ='W';Dbw=0.2;SPPGR=20; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='EC'){HTDBH_EQ='W';Dbw=0.1;SPPGR=20; BAMAX=130; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='BT'){HTDBH_EQ='W';Dbw=0.2;SPPGR=20; BAMAX=130; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PY'){HTDBH_EQ='W';Dbw=0.2;SPPGR=20; BAMAX=130; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='BC'){HTDBH_EQ='W';Dbw=0.1;SPPGR=21; BAMAX=200; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='WO'){HTDBH_EQ='W';Dbw=0.2;SPPGR=22; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Krajicek'}
else if(SPP=='BR'){HTDBH_EQ='W';Dbw=0.2;SPPGR=22; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='CK'){HTDBH_EQ='W';Dbw=0.1;SPPGR=22; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PO'){HTDBH_EQ='W';Dbw=0.1;SPPGR=22; BAMAX=130; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='OK'){HTDBH_EQ='W';Dbw=0.2;SPPGR=22; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Krajicek'}
else if(SPP=='SO'){HTDBH_EQ='W';Dbw=0.2;SPPGR=23; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='QI'){HTDBH_EQ='W';Dbw=0.2;SPPGR=23; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='WK'){HTDBH_EQ='W';Dbw=0.1;SPPGR=23; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PN'){HTDBH_EQ='W';Dbw=0.2;SPPGR=23; BAMAX=130; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='CO'){HTDBH_EQ='W';Dbw=0.2;SPPGR=24; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SW'){HTDBH_EQ='W';Dbw=0.1;SPPGR=24; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Krajicek'}
else if(SPP=='SN'){HTDBH_EQ='W';Dbw=0.2;SPPGR=24; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='RO'){HTDBH_EQ='W';Dbw=0.2;SPPGR=25; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='SK'){HTDBH_EQ='W';Dbw=0.1;SPPGR=25; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BO'){HTDBH_EQ='W';Dbw=0.2;SPPGR=26; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Krajicek'}
else if(SPP=='CB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=26; BAMAX=130; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='OH'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BU'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='YY'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='WR'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='HK'){HTDBH_EQ='C';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PS'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='HY'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BN'){HTDBH_EQ='W';Dbw=0.3;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='WN'){HTDBH_EQ='W';Dbw=0.4;SPPGR=27; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='OO'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='MG'){HTDBH_EQ='W';Dbw=0.2;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='MV'){HTDBH_EQ='W';Dbw=0.2;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='AP'){HTDBH_EQ='W';Dbw=0.2;SPPGR=27; BAMAX=150; LCW_EQ='Bragg'; MCW_EQ='Bragg'}
else if(SPP=='WT'){HTDBH_EQ='W';Dbw=0.2;SPPGR=27; BAMAX=140; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BG'){HTDBH_EQ='C';Dbw=0.2;SPPGR=27; BAMAX=140; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SD'){HTDBH_EQ='W';Dbw=0.2;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PW'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SY'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='WL'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=160; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BK'){HTDBH_EQ='C';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BL'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='SS'){HTDBH_EQ='C';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BW'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='WB'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='EL'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='AE'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
else if(SPP=='RL'){HTDBH_EQ='W';Dbw=0.1;SPPGR=27; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='NC'){HTDBH_EQ='W';Dbw=0.1;SPPGR=28; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='BE'){HTDBH_EQ='W';Dbw=0.3;SPPGR=28; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='ST'){HTDBH_EQ='W';Dbw=0.1;SPPGR=28; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='AI'){HTDBH_EQ='W';Dbw=0.1;SPPGR=28; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='SE'){HTDBH_EQ='W';Dbw=0.1;SPPGR=28; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='AH'){HTDBH_EQ='C';Dbw=0.2;SPPGR=28; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='DW'){HTDBH_EQ='W';Dbw=0.1;SPPGR=28; BAMAX=150; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='HT'){HTDBH_EQ='W';Dbw=0.1;SPPGR=28; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='HH'){HTDBH_EQ='W';Dbw=0.2;SPPGR=28; BAMAX=170; LCW_EQ='Bechtold'; MCW_EQ='Bechtold'}
else if(SPP=='PL'){HTDBH_EQ='W';Dbw=0.2;SPPGR=28; BAMAX=150; LCW_EQ='Bragg'; MCW_EQ='Bragg'}
else if(SPP=='PR'){HTDBH_EQ='W';Dbw=0.1;SPPGR=28; BAMAX=150; LCW_EQ='Bragg'; MCW_EQ='Bragg'}
else{HTDBH_EQ='W';Dbw=0.1;SPPGR=1; BAMAX=240; LCW_EQ='Bechtold'; MCW_EQ='Ek'}
  return(c(HTDBH_EQ=HTDBH_EQ,Dbw=Dbw,SPPGR=SPPGR,BAMAX=BAMAX,LCW_EQ=LCW_EQ,MCW_EQ=MCW_EQ))}

aa=sapply(tree$SPP,FVSNE.SPP)

tree$HTDBH_EQ=t(aa)[,1]
tree$Dbw=as.numeric(t(aa)[,2])
tree$SPPGR=as.numeric(t(aa)[,3])
tree$BAMAX=as.numeric(t(aa)[,4])
tree$LCW_EQ=t(aa)[,5]
tree$MCW_EQ=t(aa)[,6]
tree$SDI_MAX=tree$BAMAX/(0.5454154*0.85) #SDI_MAX is maximum stand density index
tree$SPP_SI=58 #SPP_SI is species site index
tree$BAL=10 #BAL is basal area in larger trees
tree$BA=120 #BA is stand basal area 
tree$RELHT=1.1 #RELHT is tree reltive height within the stand 
tree$LAT=45
tree$LONG=-90
tree$ELEV=400
tree$HI=((tree$ELEV-5449)/100)*1.0+(tree$LAT-42.16)*4.0+(-116.39-tree$LONG)*1.25

#Bark ratios (Table 4.2.1 in FVS-NE guide)
FVSNE.BARK=function(SPPGR){
     if(SPPGR==1){BARK=0.9349}
else if(SPPGR==2){BARK=0.9349}
else if(SPPGR==3){BARK=0.956}
else if(SPPGR==4){BARK=0.9324}
else if(SPPGR==5){BARK=0.920}
else if(SPPGR==6){BARK=0.920}
else if(SPPGR==7){BARK=0.890}
else if(SPPGR==8){BARK=0.964}
else if(SPPGR==9){BARK=0.950}
else if(SPPGR==10){BARK=0.934}
else if(SPPGR==11){BARK=0.964}
else if(SPPGR==12){BARK=0.950}
else if(SPPGR==13){BARK=0.920}
else if(SPPGR==14){BARK=0.948}
else if(SPPGR==15){BARK=0.948}
else if(SPPGR==16){BARK=0.900}
else if(SPPGR==17){BARK=0.950}
else if(SPPGR==18){BARK=0.900}
else if(SPPGR==19){BARK=0.900}
else if(SPPGR==20){BARK=0.900}
else if(SPPGR==21){BARK=0.940}
else if(SPPGR==22){BARK=0.910}
else if(SPPGR==23){BARK=0.900}
else if(SPPGR==24){BARK=0.880}
else if(SPPGR==25){BARK=0.900}
else if(SPPGR==26){BARK=0.900}
else if(SPPGR==27){BARK=0.900}
else if(SPPGR==28){BARK=0.900}
else {BARK=0.9349}
    BARK=BARK
    return(BARK=BARK)}

tree$BARK=mapply(FVSNE.BARK,SPPGR=tree$SPPGR)
tree$DIB=tree$BARK*tree$DBH

#Crown ratio (Table 4.3.1 in FVS-NE guide)
FVSNE.CR=function(SPPGR,BA,DBH){
     if(SPPGR==1){b1=5.630; b2=0.0047; b3=3.523; b4=-0.0689}
else if(SPPGR==2){b1=6.000; b2=0.0053; b3=0.431; b4=-0.0012}
else if(SPPGR==3){b1=7.840; b2=0.0057; b3=1.272; b4=-0.1420}
else if(SPPGR==4){b1=5.540; b2=0.0072; b3=4.200; b4=-0.0530}
else if(SPPGR==5){b1=5.540; b2=0.0072; b3=4.200; b4=-0.0530}
else if(SPPGR==6){b1=6.640; b2=0.0135; b3=3.200; b4=-0.0518}
else if(SPPGR==7){b1=5.350; b2=0.0053; b3=1.528; b4=-0.0330}
else if(SPPGR==8){b1=6.790; b2=0.0058; b3=7.590; b4=-0.0103}
else if(SPPGR==9){b1=5.710; b2=0.0077; b3=2.290; b4=-0.2530}
else if(SPPGR==10){b1=5.710; b2=0.0077; b3=2.290; b4=-0.2530}
else if(SPPGR==11){b1=4.350; b2=0.0046; b3=1.820; b4=-0.2740}
else if(SPPGR==12){b1=3.400; b2=0.0066; b3=2.870; b4=-0.4340}
else if(SPPGR==13){b1=4.180; b2=0.0025; b3=1.410; b4=-0.5120}
else if(SPPGR==14){b1=4.180; b2=0.0025; b3=1.410; b4=-0.5120}
else if(SPPGR==15){b1=5.000; b2=0.0066; b3=4.920; b4=-0.0263}
else if(SPPGR==16){b1=4.000; b2=0.0024; b3=-2.830; b4=0.0210}
else if(SPPGR==17){b1=6.210; b2=0.0073; b3=9.990; b4=-0.0100}
else if(SPPGR==18){b1=3.733; b2=0.0040; b3=3.632; b4=-0.0412}
else if(SPPGR==19){b1=3.733; b2=0.0040; b3=3.632; b4=-0.0412}
else if(SPPGR==20){b1=4.500; b2=0.0032; b3=0.795; b4=-0.1050}
else if(SPPGR==21){b1=3.733; b2=0.0040; b3=3.632; b4=-0.0412}
else if(SPPGR==22){b1=4.110; b2=0.0054; b3=1.650; b4=-0.1100}
else if(SPPGR==23){b1=4.000; b2=0.0024; b3=-2.830; b4=0.0210}
else if(SPPGR==24){b1=3.733; b2=0.0040; b3=3.632; b4=-0.0412}
else if(SPPGR==25){b1=5.840; b2=0.0082; b3=3.260; b4=-0.0490}
else if(SPPGR==26){b1=4.200; b2=0.0016; b3=2.760; b4=-0.0250}
else if(SPPGR==27){b1=4.000; b2=0.0024; b3=-2.830; b4=0.0210}
else if(SPPGR==28){b1=4.000; b2=0.0024; b3=-2.830; b4=0.0210}
else {b1=5.630; b2= 0.0047; b3=3.523; b4=-0.0689}
    CR=10*(b1/(1+b2*BA)+(b3*(1-exp(b4*DBH))))
    return(CR=CR)}

tree$CR=mapply(FVSNE.CR,SPPGR=tree$SPPGR,BA=tree$BA,DBH=tree$DBH)

#FVS HT-DBH (Table 4.1.1 in FVS-NE guide)
FVSNE.HTDBH=function(SPP,HTDBH_EQ,DBH,Dbw){
     if(SPP=='BF'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='TA'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='WS'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='RS'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='NS'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='BS'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='PI'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='RN'){ca_a1=266.4562; ca_a2=3.9931; ca_a3=-0.386; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='WP'){ca_a1=2108.8442; ca_a2=5.6595; ca_a3=-0.1856; w_a1=4.609; w_a2=-6.1896}     
else if(SPP=='LP'){ca_a1=243.8606; ca_a2=4.2846; ca_a3=-0.4713; w_a1=4.6897; w_a2=-6.8801}     
else if(SPP=='VP'){ca_a1=926.1803; ca_a2=4.4621; ca_a3=-0.2005; w_a1=4.4718; w_a2=-5.0078}     
else if(SPP=='WC'){ca_a1=2163.9468; ca_a2=6.2688; ca_a3=-0.2161; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='AW'){ca_a1=266.4562; ca_a2=3.9931; ca_a3=-0.386; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='RC'){ca_a1=926.1803; ca_a2=4.4621; ca_a3=-0.2005; w_a1=4.4718; w_a2=-5.0078}     
else if(SPP=='OC'){ca_a1=926.1803; ca_a2=4.4621; ca_a3=-0.2005; w_a1=4.4718; w_a2=-5.0078}     
else if(SPP=='EH'){ca_a1=266.4562; ca_a2=3.9931; ca_a3=-0.386; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='HM'){ca_a1=266.4562; ca_a2=3.9931; ca_a3=-0.386; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='OP'){ca_a1=208.7773; ca_a2=3.7281; ca_a3=-0.4109; w_a1=4.3898; w_a2=-5.7183}     
else if(SPP=='JP'){ca_a1=266.4562; ca_a2=3.9931; ca_a3=-0.386; w_a1=4.5084; w_a2=-6.0116}     
else if(SPP=='SP'){ca_a1=444.0922; ca_a2=4.1188; ca_a3=-0.3062; w_a1=4.6271; w_a2=-6.4095}     
else if(SPP=='TM'){ca_a1=208.7773; ca_a2=3.7281; ca_a3=-0.4109; w_a1=4.3898; w_a2=-5.7183}     
else if(SPP=='PP'){ca_a1=208.7773; ca_a2=3.7281; ca_a3=-0.4109; w_a1=4.3898; w_a2=-5.7183}     
else if(SPP=='PD'){ca_a1=142.7468; ca_a2=3.9726; ca_a3=-0.5871; w_a1=4.5457; w_a2=-6.8}     
else if(SPP=='SC'){ca_a1=142.7468; ca_a2=3.9726; ca_a3=-0.5871; w_a1=4.5457; w_a2=-6.8}     
else if(SPP=='OS'){ca_a1=212.7933; ca_a2=3.4715; ca_a3=-0.3259; w_a1=4.0374; w_a2=-4.2964}     
else if(SPP=='RM'){ca_a1=268.5564; ca_a2=3.1143; ca_a3=-0.2941; w_a1=4.3379; w_a2=-3.8214}     
else if(SPP=='SM'){ca_a1=209.8555; ca_a2=2.9528; ca_a3=-0.3679; w_a1=4.4834; w_a2=-4.5431}     
else if(SPP=='BM'){ca_a1=209.8555; ca_a2=2.9528; ca_a3=-0.3679; w_a1=4.4834; w_a2=-4.5431}     
else if(SPP=='SV'){ca_a1=80.5118; ca_a2=26.9833; ca_a3=-2.022; w_a1=4.5991; w_a2=-6.6706}     
else if(SPP=='YB'){ca_a1=170.5253; ca_a2=2.6883; ca_a3=-0.4008; w_a1=4.4388; w_a2=-4.0872}     
else if(SPP=='SB'){ca_a1=68.9223; ca_a2=43.3383; ca_a3=-2.4445; w_a1=4.4522; w_a2=-4.5758}     
else if(SPP=='RB'){ca_a1=170.5253; ca_a2=2.6883; ca_a3=-0.4008; w_a1=4.4388; w_a2=-4.0872}     
else if(SPP=='PB'){ca_a1=170.5253; ca_a2=2.6883; ca_a3=-0.4008; w_a1=4.4388; w_a2=-4.0872}     
else if(SPP=='GB'){ca_a1=170.5253; ca_a2=2.6883; ca_a3=-0.4008; w_a1=4.4388; w_a2=-4.0872}     
else if(SPP=='HI'){ca_a1=337.6685; ca_a2=3.6273; ca_a3=-0.3208; w_a1=4.5128; w_a2=-4.9918}     
else if(SPP=='PH'){ca_a1=337.6685; ca_a2=3.6273; ca_a3=-0.3208; w_a1=4.5128; w_a2=-4.9918}     
else if(SPP=='SL'){ca_a1=337.6685; ca_a2=3.6273; ca_a3=-0.3208; w_a1=4.5128; w_a2=-4.9918}     
else if(SPP=='SH'){ca_a1=337.6685; ca_a2=3.6273; ca_a3=-0.3208; w_a1=4.5128; w_a2=-4.9918}     
else if(SPP=='MH'){ca_a1=337.6685; ca_a2=3.6273; ca_a3=-0.3208; w_a1=4.5128; w_a2=-4.9918}     
else if(SPP=='AB'){ca_a1=526.1393; ca_a2=3.8923; ca_a3=-0.2259; w_a1=4.4772; w_a2=-4.7206}     
else if(SPP=='AS'){ca_a1=251.4043; ca_a2=3.2692; ca_a3=-0.3591; w_a1=4.4819; w_a2=-4.5314}     
else if(SPP=='WA'){ca_a1=91.3528; ca_a2=6.9961; ca_a3=-1.2294; w_a1=4.5959; w_a2=-6.4497}     
else if(SPP=='BA'){ca_a1=178.9308; ca_a2=4.9286; ca_a3=-0.6378; w_a1=4.6155; w_a2=-6.2945}     
else if(SPP=='GA'){ca_a1=404.9692; ca_a2=3.3902; ca_a3=-0.2551; w_a1=4.6155; w_a2=-6.2945}     
else if(SPP=='PA'){ca_a1=251.4043; ca_a2=3.2692; ca_a3=-0.3591; w_a1=4.4819; w_a2=-4.5314}     
else if(SPP=='YP'){ca_a1=625.7697; ca_a2=3.8732; ca_a3=-0.2335; w_a1=4.6892; w_a2= -4.9605}     
else if(SPP=='SU'){ca_a1=290.9055; ca_a2=3.624; ca_a3=-0.372; w_a1=4.592; w_a2= -5.1719}     
else if(SPP=='CT'){ca_a1=660.1997; ca_a2=3.9208; ca_a3=-0.2112; w_a1=4.6067; w_a2= -5.203}     
else if(SPP=='QA'){ca_a1=337.6685; ca_a2=3.6273; ca_a3=-0.3208; w_a1=4.5128; w_a2= -4.9918}     
else if(SPP=='BP'){ca_a1=91.3528; ca_a2=6.9961; ca_a3=-1.2294; w_a1=4.5959; w_a2= -6.4497}     
else if(SPP=='EC'){ca_a1=190.9797; ca_a2=3.6928; ca_a3=-0.5273; w_a1=4.9396; w_a2=-8.1838}     
else if(SPP=='BT'){ca_a1=91.3528; ca_a2=6.9961; ca_a3=-1.2294; w_a1=4.5959; w_a2=-6.4497}     
else if(SPP=='PY'){ca_a1=91.3528; ca_a2=6.9961; ca_a3=-1.2294; w_a1=4.5959; w_a2=-6.4497}     
else if(SPP=='BC'){ca_a1=364.0248; ca_a2=3.5599; ca_a3=-0.2726; w_a1=4.3286; w_a2=-4.0922}     
else if(SPP=='WO'){ca_a1=170.1331; ca_a2=3.2782; ca_a3=-0.4874; w_a1=4.5463; w_a2=-5.2287}     
else if(SPP=='BR'){ca_a1=196.0565; ca_a2=3.0067; ca_a3=-0.385; w_a1=4.5225; w_a2=-4.9401}     
else if(SPP=='CK'){ca_a1=72.7907; ca_a2=3.6707; ca_a3=-1.0988; w_a1=4.342; w_a2=-5.1193}     
else if(SPP=='PO'){ca_a1=765.2908; ca_a2=4.2238; ca_a3=-0.1897; w_a1=4.2496; w_a2=-4.8061}     
else if(SPP=='OK'){ca_a1=196.0565; ca_a2=3.0067; ca_a3=-0.385; w_a1=4.5225; w_a2=-4.9401}     
else if(SPP=='SO'){ca_a1=196.0565; ca_a2=3.0067; ca_a3=-0.385; w_a1=4.5225; w_a2=-4.9401}     
else if(SPP=='QI'){ca_a1=94.5447; ca_a2=3.4203; ca_a3=-0.8188; w_a1=4.4618; w_a2=-4.8786}     
else if(SPP=='WK'){ca_a1=470.0617; ca_a2=3.7889; ca_a3=-0.2512; w_a1=4.5577; w_a2=-4.9595}     
else if(SPP=='PN'){ca_a1=196.0565; ca_a2=3.0067; ca_a3=-0.385; w_a1=4.5225; w_a2=-4.9401}     
else if(SPP=='CO'){ca_a1=94.5447; ca_a2=3.4203; ca_a3=-0.8188; w_a1=4.4618; w_a2=-4.8786}     
else if(SPP=='SW'){ca_a1=182.6306; ca_a2=3.129; ca_a3=-0.4639; w_a1=4.7342; w_a2=-6.2674}     
else if(SPP=='SN'){ca_a1=281.3413; ca_a2=3.517; ca_a3=-0.3336; w_a1=4.6135; w_a2=-5.7613}     
else if(SPP=='RO'){ca_a1=700.0636; ca_a2=4.1061; ca_a3=-0.2139; w_a1=4.5202; w_a2=-4.8896}     
else if(SPP=='SK'){ca_a1=150.43; ca_a2=3.1327; ca_a3=-0.4993; w_a1=4.5142; w_a2=-5.2205}     
else if(SPP=='BO'){ca_a1=224.7163; ca_a2=3.1165; ca_a3=-0.3598; w_a1=4.4747; w_a2=-4.8698}     
else if(SPP=='CB'){ca_a1=182.6306; ca_a2=3.129; ca_a3=-0.4639; w_a1=4.7342; w_a2=-6.2674}     
else if(SPP=='OH'){ca_a1=109.7324; ca_a2=2.2503; ca_a3=-0.413; w_a1=4.0322; w_a2=-3.0833}     
else if(SPP=='BU'){ca_a1=293.5715; ca_a2=3.5226; ca_a3=-0.3512; w_a1=4.582; w_a2=-5.0903}     
else if(SPP=='YY'){ca_a1=293.5715; ca_a2=3.5226; ca_a3=-0.3512; w_a1=4.582; w_a2=-5.0903}     
else if(SPP=='WR'){ca_a1=170.5253; ca_a2=2.6883; ca_a3=-0.4008; w_a1=4.4388; w_a2=-4.0872}     
else if(SPP=='HK'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='PS'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='HY'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='BN'){ca_a1=285.8798; ca_a2=3.5214; ca_a3=-0.3194; w_a1=4.5018; w_a2=-5.6123}     
else if(SPP=='WN'){ca_a1=93.7104; ca_a2=3.6575; ca_a3=-0.8825; w_a1=4.5018; w_a2=-5.6123}     
else if(SPP=='OO'){ca_a1=109.7324; ca_a2=2.2503; ca_a3=-0.413; w_a1=4.0322; w_a2=-3.0833}     
else if(SPP=='MG'){ca_a1=585.6609; ca_a2=3.4197; ca_a3=-0.1766; w_a1=4.4004; w_a2=-4.7519}     
else if(SPP=='MV'){ca_a1=184.1932; ca_a2=2.8457; ca_a3=-0.3695; w_a1=4.3609; w_a2=-4.1423}     
else if(SPP=='AP'){ca_a1=574.0201; ca_a2=3.8637; ca_a3=-0.1632; w_a1=3.9678; w_a2=-3.251}     
else if(SPP=='WT'){ca_a1=163.9728; ca_a2=2.7682; ca_a3=-0.441; w_a1=4.333; w_a2=-4.5383}     
else if(SPP=='BG'){ca_a1=319.9788; ca_a2=3.6731; ca_a3=-0.3065; w_a1=4.3802; w_a2=-4.7903}     
else if(SPP=='SD'){ca_a1=690.4918; ca_a2=4.1598; ca_a3=-0.1861; w_a1=4.1352; w_a2=-3.745}     
else if(SPP=='PW'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='SY'){ca_a1=644.3568; ca_a2=3.9205; ca_a3=-0.2144; w_a1=4.6355; w_a2=-5.2776}     
else if(SPP=='WL'){ca_a1=190.9797; ca_a2=3.6928; ca_a3=-0.5273; w_a1=4.9396; w_a2=-8.1838}     
else if(SPP=='BK'){ca_a1=880.2845; ca_a2=4.5964; ca_a3=-0.2182; w_a1=4.4299; w_a2=-4.992}     
else if(SPP=='BL'){ca_a1=408.2772; ca_a2=3.8181; ca_a3=-0.2721; w_a1=4.4911; w_a2=-5.7928}     
else if(SPP=='SS'){ca_a1=755.1038; ca_a2=4.395; ca_a3=-0.2178; w_a1=4.3383; w_a2=-4.5018}     
else if(SPP=='BW'){ca_a1=293.5715; ca_a2=3.5226; ca_a3=-0.3512; w_a1=4.582; w_a2=-5.0903}     
else if(SPP=='WB'){ca_a1=293.5715; ca_a2=3.5226; ca_a3=-0.3512; w_a1=4.582; w_a2=-5.0903}     
else if(SPP=='EL'){ca_a1=1005.8067; ca_a2=4.6474; ca_a3=-0.2034; w_a1=4.3744; w_a2=-4.5257}     
else if(SPP=='AE'){ca_a1=418.5942; ca_a2=3.1704; ca_a3=-0.1896; w_a1=4.6008; w_a2=-7.2732}     
else if(SPP=='RL'){ca_a1=1337.5472; ca_a2=4.4895; ca_a3=-0.1475; w_a1=4.6238; w_a2=-7.4847}     
else if(SPP=='NC'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='BE'){ca_a1=285.8798; ca_a2=3.5214; ca_a3=-0.3194; w_a1=4.5018; w_a2=-5.6123}     
else if(SPP=='ST'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='AI'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='SE'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='AH'){ca_a1=109.7324; ca_a2=2.2503; ca_a3=-0.413; w_a1=4.0322; w_a2=-3.0833}     
else if(SPP=='DW'){ca_a1=863.0501; ca_a2=4.3856; ca_a3=-0.1481; w_a1=3.7301; w_a2=-2.7758}     
else if(SPP=='HT'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}     
else if(SPP=='HH'){ca_a1=109.7324; ca_a2=2.2503; ca_a3=-0.413; w_a1=4.0322; w_a2=-3.0833}     
else if(SPP=='PL'){ca_a1=574.0201; ca_a2=3.8637; ca_a3=-0.1632; w_a1=3.9678; w_a2=-3.251}     
else if(SPP=='PR'){ca_a1=484.753; ca_a2=3.9393; ca_a3=-0.26; w_a1=4.4207; w_a2=-5.1435}
else{ca_a1=68.5564; ca_a2=3.1143; ca_a3=-0.2941; w_a1=4.3379; w_a2=-3.8214}
    HT=ifelse(HTDBH_EQ=="C" & DBH < 3.0,(4.5+ca_a1*exp(-1*ca_a2*3**ca_a3)-4.51*(DBH-Dbw)/(3-Dbw))+4.51,
          ifelse(HTDBH_EQ=="C" & DBH >= 3.0,4.5+ca_a1*exp(-1*ca_a2*DBH**ca_a3),
            4.5+exp(w_a1+w_a2/(DBH+1))))
    return(HT=HT)}

tree$HT=mapply(FVSNE.HTDBH,SPP=tree$SPP,HTDBH_EQ=tree$HTDBH_EQ,DBH=tree$DBH,Dbw=tree$Dbw)


#Largest crown width (Table 4.4.1 in FVS-NE guide)
FVSNE.LCW=function(SPP,LCW_EQ,DBH,CR,HI){
   if(SPP=='BF'){a1=0.6564; a2=0.8403; a3=0; a4=0.0792; a5=0}
else if(SPP=='TA'){a1=-0.3276; a2=1.3865; a3=0; a4=0.0517; a5=0}
else if(SPP=='WS'){a1=0.3789; a2=0.8658; a3=0; a4=0.0878; a5=0}
else if(SPP=='RS'){a1=-1.2151; a2=1.6098; a3=-0.0277; a4=0.0674; a5=-0.0474}
else if(SPP=='NS'){a1=1.8336; a2=0.9932; a3=0; a4=0.0431; a5=0.1012}
else if(SPP=='BS'){a1=-0.8566; a2=0.9693; a3=0; a4=0.0573; a5=0}
else if(SPP=='PI'){a1=0.3789; a2=0.8658; a3=0; a4=0.0878; a5=0}
else if(SPP=='RN'){a1=-3.6548; a2=1.9565; a3=-0.0409; a4=0.0577; a5=0}
else if(SPP=='WP'){a1=0.3914; a2=0.9923; a3=0; a4=0.1080; a5=0}
else if(SPP=='LP'){a1=-0.8277; a2=1.3946; a3=0; a4=0.0768; a5=0}
else if(SPP=='VP'){a1=-0.1211; a2=1.2319; a3=0; a4=0.1212; a5=0}
else if(SPP=='WC'){a1=-0.0634; a2=0.7057; a3=0; a4=0.0837; a5=0}
else if(SPP=='AW'){a1=-0.0634; a2=0.7057; a3=0; a4=0.0837; a5=0}
else if(SPP=='RC'){a1=1.2359; a2=1.2962; a3=0; a4=0.0545; a5=0}
else if(SPP=='OC'){a1=1.2359; a2=1.2962; a3=0; a4=0.0545; a5=0}
else if(SPP=='EH'){a1=6.1924; a2=1.4491; a3=-0.0178; a4=0; a5=0.0341}
else if(SPP=='HM'){a1=6.1924; a2=1.4491; a3=-0.0178; a4=0; a5=0.0341}
else if(SPP=='OP'){a1=0.3914; a2=0.9923; a3=0; a4=0.1080; a5=0}
else if(SPP=='JP'){a1=0.7478; a2=0.8712; a3=0; a4=0.0913; a5=0}
else if(SPP=='SP'){a1=-2.2564; a2=1.3004; a3=0; a4=0.1031; a5=-0.0562}
else if(SPP=='TM'){a1=-0.9442; a2=1.4531; a3=0; a4=0.0543; a5=-0.1144}
else if(SPP=='PP'){a1=-0.9442; a2=1.4531; a3=0; a4=0.0543; a5=-0.1144}
else if(SPP=='PD'){a1=-8.7711; a2=3.7252; a3=-0.1063; a4=0; a5=0}
else if(SPP=='SC'){a1=3.5522; a2=0.6742; a3=0; a4=0.0985; a5=0}
else if(SPP=='OS'){a1=1.2359; a2=1.2962; a3=0; a4=0.0545; a5=0}
else if(SPP=='RM'){a1=2.7563; a2=1.4212; a3=-0.0143; a4=0.0993; a5=-0.0276}
else if(SPP=='SM'){a1=4.9399; a2=1.0727; a3=0; a4=0.1096; a5=-0.0493}
else if(SPP=='BM'){a1=4.9399; a2=1.0727; a3=0; a4=0.1096; a5=-0.0493}
else if(SPP=='SV'){a1=3.3576; a2=1.1312; a3=0; a4=0.1011; a5=-0.1730}
else if(SPP=='YB'){a1=-1.1151; a2=2.2888; a3=-0.0493; a4=0.0985; a5=-0.0396}
else if(SPP=='SB'){a1=4.6725; a2=1.2968; a3=0; a4=0.0787; a5=0}
else if(SPP=='RB'){a1=11.6634; a2=1.0028; a3=0; a4=0; a5=0}
else if(SPP=='PB'){a1=2.8399; a2=1.2398; a3=0; a4=0.0855; a5=-0.0282}
else if(SPP=='GB'){a1=2.8399; a2=1.2398; a3=0; a4=0.0855; a5=-0.0282}
else if(SPP=='HI'){a1=4.5453; a2=1.3721; a3=0; a4=0.0430; a5=0}
else if(SPP=='PH'){a1=3.9234; a2=1.5220; a3=0; a4=0.0405; a5=0}
else if(SPP=='SL'){a1=4.5453; a2=1.3721; a3=0; a4=0.0430; a5=0}
else if(SPP=='SH'){a1=4.5453; a2=1.3721; a3=0; a4=0.0430; a5=0}
else if(SPP=='MH'){a1=1.5838; a2=1.6318; a3=0; a4=0.0721; a5=0}
else if(SPP=='AB'){a1=3.9361; a2=1.1500; a3=0; a4=0.1237; a5=-0.0691}
else if(SPP=='AS'){a1=2.9672; a2=1.3066; a3=0; a4=0.0585; a5=0}
else if(SPP=='WA'){a1=1.7625; a2=1.3413; a3=0; a4=0.0957; a5=0}
else if(SPP=='BA'){a1=5.2824; a2=1.1184; a3=0; a4=0; a5=0}
else if(SPP=='GA'){a1=2.9672; a2=1.3066; a3=0; a4=0.0585; a5=0}
else if(SPP=='PA'){a1=1.7625; a2=1.3413; a3=0; a4=0.0957; a5=0}
else if(SPP=='YP'){a1=3.3543; a2=1.1627; a3=0; a4=0.0857; a5=0}
else if(SPP=='SU'){a1=1.8853; a2=1.1625; a3=0; a4=0.0656; a5=-0.0300}
else if(SPP=='CT'){a1=4.1711; a2=1.6275; a3=0; a4=0; a5=0}
else if(SPP=='QA'){a1=0.7315; a2=1.3180; a3=0; a4=0.0966; a5=0}
else if(SPP=='BP'){a1=6.2498; a2=0.8655; a3=0; a4=0; a5=0}
else if(SPP=='EC'){a1=3.4375; a2=1.4092; a3=0; a4=0; a5=0}
else if(SPP=='BT'){a1=0.6847; a2=1.1050; a3=0; a4=0.1420; a5=-0.0265}
else if(SPP=='PY'){a1=3.4375; a2=1.4092; a3=0; a4=0; a5=0}
else if(SPP=='BC'){a1=3.0237; a2=1.1119; a3=0; a4=0.1112; a5=-0.0493}
else if(SPP=='WO'){a1=3.2375; a2=1.5234; a3=0; a4=0.0455; a5=-0.0324}
else if(SPP=='BR'){a1=1.7827; a2=1.6549; a3=0; a4=0.0343; a5=0}
else if(SPP=='CK'){a1=0.5189; a2=1.4134; a3=0; a4=0.1365; a5=-0.0806}
else if(SPP=='PO'){a1=1.6125; a2=1.6669; a3=0; a4=0.0536; a5=0}	
else if(SPP=='OK'){a1=3.2375; a2=1.5234; a3=0; a4=0.0455; a5=-0.0324}
else if(SPP=='SO'){a1=0.5656; a2=1.6766; a3=0; a4=0.0739; a5=0}
else if(SPP=='QI'){a1=9.8187; a2=1.1343; a3=0; a4=0; a5=0}
else if(SPP=='WK'){a1=1.6349; a2=1.5443; a3=0; a4=0.0637; a5=-0.0764}
else if(SPP=='NP'){a1=4.8935; a2=1.6069; a3=0; a4=0; a5=0}
else if(SPP=='CO'){a1=2.1480; a2=1.6928; a3=-0.0176; a4=0.0569; a5=0}
else if(SPP=='SW'){a1=3.2375; a2=1.5234; a3=0; a4=0.0455; a5=-0.0324}
else if(SPP=='SN'){a1=2.1480; a2=1.6928; a3=-0.0176; a4=0.0569; a5=0}
else if(SPP=='RO'){a1=2.8908; a2=1.4077; a3=0; a4=0.0643; a5=0}
else if(SPP=='SK'){a1=2.1517; a2=1.6064; a3=0; a4=0.0609; a5=0}
else if(SPP=='BO'){a1=2.8974; a2=1.3697; a3=0; a4=0.0671; a5=0}
else if(SPP=='CB'){a1=2.1517; a2=1.6064; a3=0; a4=0.0609; a5=0}
else if(SPP=='OH'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='BU'){a1=4.5453; a2=1.3721; a3=0; a4=0.0430; a5=0}
else if(SPP=='YY'){a1=4.5453; a2=1.3721; a3=0; a4=0.0430; a5=0}
else if(SPP=='WR'){a1=11.6634; a2=1.0028; a3=0; a4=0; a5=0}
else if(SPP=='HK'){a1=7.1043; a2=1.3041; a3=0; a4=0.0456; a5=0}
else if(SPP=='PS'){a1=3.5393; a2=1.3939; a3=0; a4=0.0625; a5=0}
else if(SPP=='HY'){a1=4.5803; a2=1.0747; a3=0; a4=0.0661; a5=0}
else if(SPP=='BN'){a1=3.6031; a2=1.1472; a3=0; a4=0.1224; a5=0}
else if(SPP=='WN'){a1=3.6031; a2=1.1472; a3=0; a4=0.1224; a5=0}
else if(SPP=='OO'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='MG'){a1=8.2119; a2=0.9708; a3=0; a4=0; a5=0}
else if(SPP=='MV'){a1=8.2119; a2=0.9708; a3=0; a4=0; a5=0}
else if(SPP=='AP'){a1=4.1027; a2=1.3960; a3=1.0775; a4=0; a5=0}
else if(SPP=='WT'){a1=5.3409; a2=0.7499; a3=0; a4=0.1047; a5=0}
else if(SPP=='BG'){a1=5.5037; a2=1.0567; a3=0; a4=0.0880; a5=0.0610}
else if(SPP=='SD'){a1=7.9750; a2=0.8303; a3=0; a4=0.0423; a5=-0.0706}
else if(SPP=='PW'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='SY'){a1=-1.3973; a2=1.3756; a3=0; a4=0.1835; a5=0}
else if(SPP=='WL'){a1=1.6477; a2=1.3672; a3=0; a4=0.0846; a5=0}
else if(SPP=='BK'){a1=3.0012; a2=0.8165; a3=0; a4=0.1395; a5=0}
else if(SPP=='BL'){a1=1.7296; a2=2.0732; a3=0; a4=0.0590; a5=-0.0869}
else if(SPP=='SS'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='BW'){a1=1.6871; a2=1.2110; a3=0; a4=0.1194; a5=-0.0264}
else if(SPP=='WB'){a1=1.6871; a2=1.2110; a3=0; a4=0.1194; a5=-0.0264}
else if(SPP=='EL'){a1=1.7296; a2=2.0732; a3=0; a4=0.0590; a5=-0.0869}
else if(SPP=='AE'){a1=1.7296; a2=2.0732; a3=0; a4=0.0590; a5=-0.0869}
else if(SPP=='RL'){a1=9.0023; a2=1.3933; a3=0; a4=0; a5=-0.0785}
else if(SPP=='NC'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='BE'){a1=6.4741; a2=1.0778; a3=0; a4=0.0719; a5=-0.0637}
else if(SPP=='ST'){a1=6.4741; a2=1.0778; a3=0; a4=0.0719; a5=-0.0637}
else if(SPP=='AI'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='SE'){a1=6.9814; a2=1.6032; a3=0; a4=0; a5=0}
else if(SPP=='AH'){a1=0.9219; a2=1.6303; a3=0; a4=0.1150; a5=-0.1113}
else if(SPP=='DW'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='HT'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='HH'){a1=7.8084; a2=0.8129; a3=0; a4=0.0941; a5=-0.0817}
else if(SPP=='PL'){a1=4.1027; a2=1.396; a3=1.0775; a4=0; a5=0}
else if(SPP=='PR'){a1=4.1027; a2=1.396; a3=1.0775; a4=0; a5=0}
else{a1=0.6564; a2=0.8403; a3=0; a4=0.0792; a5=0}  
      LCW=ifelse(LCW_EQ=="Bechtold" & DBH < 5.0,a1+(a2*5.0)+(a3*5.0**2)+(a4*CR)+(a5*HI)*(DBH/5.0),
          ifelse(LCW_EQ=="Bechtold" & DBH >= 5.0,a1+(a2*DBH)+(a3*DBH**2)+(a4*CR)+(a5*HI),
            a1 + (a2 * DBH**a3)))
    return(LCW=LCW)}

tree$LCW=mapply(FVSNE.LCW,SPP=tree$SPP,LCW_EQ=tree$LCW_EQ,DBH=tree$DBH,CR=tree$CR,HI=tree$HI)


#Maximum crown width (Table 4.4.2 in FVS-NE guide)
FVSNE.MCW=function(SPP,MCW_EQ,DBH,CR,HI){
   if(SPP=='BF'){a1=0.3270; a2=5.1160; a3=0.5035; a4=0; a5=0}
else if(SPP=='TA'){a1=2.2050; a2=3.4750; a3=0.7506; a4=0; a5=0}
else if(SPP=='WS'){a1=3.5940; a2=1.9630; a3=0.8820; a4=0; a5=0}
else if(SPP=='RS'){a1=-1.2151; a2=1.6098; a3=-0.0277; a4=0.0674; a5=-0.0474}
else if(SPP=='NS'){a1=5.0570; a2=1.1313; a3=0; a4=0; a5=0}
else if(SPP=='BS'){a1=3.6550; a2=1.3980; a3=1.0000; a4=0; a5=0}
else if(SPP=='PI'){a1=3.5940; a2=1.9630; a3=0.8820; a4=0; a5=0}
else if(SPP=='RN'){a1=4.2330; a2=1.4620; a3=1.0000; a4=0; a5=0}
else if(SPP=='WP'){a1=1.6200; a2=3.1970; a3=0.7981; a4=0; a5=0}
else if(SPP=='LP'){a1=0.7380; a2=0.2450; a3=0.0008; a4=0; a5=0}
else if(SPP=='VP'){a1=-0.1211; a2=1.2319; a3=0; a4=0.1212; a5=0}
else if(SPP=='WC'){a1=-0.0634; a2=0.7057; a3=0; a4=0.0837; a5=0}
else if(SPP=='AW'){a1=-0.0634; a2=0.7057; a3=0; a4=0.0837; a5=0}
else if(SPP=='RC'){a1=1.2359; a2=1.2962; a3=0; a4=0.0545; a5=0}
else if(SPP=='OC'){a1=1.2359; a2=1.2962; a3=0; a4=0.0545; a5=0}
else if(SPP=='EH'){a1=6.1924; a2=1.4491; a3=-0.0178; a4=0; a5=0.0341}
else if(SPP=='HM'){a1=6.1924; a2=1.4491; a3=-0.0178; a4=0; a5=0.0341}
else if(SPP=='OP'){a1=1.6200; a2=3.1970; a3=0.7981; a4=0; a5=0}
else if(SPP=='JP'){a1=0.2990; a2=5.6440; a3=0.6036; a4=0; a5=0}
else if(SPP=='SP'){a1=0.5830; a2=0.2450; a3=0.0009; a4=0; a5=0}
else if(SPP=='TM'){a1=-0.9442; a2=1.4531; a3=0; a4=0.0543; a5=-0.1144}
else if(SPP=='PP'){a1=-0.9442; a2=1.4531; a3=0; a4=0.0543; a5=-0.1144}
else if(SPP=='PD'){a1=-8.7711; a2=3.7252; a3=-0.1063; a4=0; a5=0}
else if(SPP=='SC'){a1=3.5522; a2=0.6742; a3=0; a4=0.0985; a5=0}
else if(SPP=='OS'){a1=1.2359; a2=1.2962; a3=0; a4=0.0545; a5=0}
else if(SPP=='RM'){a1=0; a2=4.7760; a3=0.7656; a4=0; a5=0}
else if(SPP=='SM'){a1=0.8680; a2=4.1500; a3=0.7514; a4=0; a5=0}
else if(SPP=='BM'){a1=0.8680; a2=4.1500; a3=0.7514; a4=0; a5=0}
else if(SPP=='SV'){a1=3.3576; a2=1.1312; a3=0; a4=0.1011; a5=-0.1730}
else if(SPP=='YB'){a1=-1.1151; a2=2.2888; a3=-0.0493; a4=0.0985; a5=-0.0396}
else if(SPP=='SB'){a1=4.6725; a2=1.2968; a3=0; a4=0.0787; a5=0}
else if(SPP=='RB'){a1=11.6634; a2=1.0028; a3=0; a4=0; a5=0}
else if(SPP=='PB'){a1=3.6390; a2=1.9530; a3=1.0000; a4=0; a5=0}
else if(SPP=='GB'){a1=3.6390; a2=1.9530; a3=1.0000; a4=0; a5=0}
else if(SPP=='HI'){a1=2.3600; a2=3.5480; a3=0.7986; a4=0; a5=0}
else if(SPP=='PH'){a1=3.9234; a2=1.5220; a3=0; a4=0.0405;a5=	0}
else if(SPP=='SL'){a1=2.3600; a2=3.5480; a3=0.7986; a4=0; a5=0}
else if(SPP=='SH'){a1=2.3600; a2=3.5480; a3=0.7986; a4=0; a5=0}
else if(SPP=='MH'){a1=1.5838; a2=1.6318; a3=0; a4=0.0721; a5=0}
else if(SPP=='AB'){a1=3.9361; a2=1.1500; a3=0; a4=0.1237; a5=-0.0691}
else if(SPP=='AS'){a1=0; a2=4.7550; a3=0.7381; a4=0; a5=0}
else if(SPP=='WA'){a1=1.7625; a2=1.3413; a3=0; a4=0.0957; a5=0}
else if(SPP=='BA'){a1=5.2824; a2=1.1184; a3=0; a4=0; a5=0}
else if(SPP=='GA'){a1=0; a2=4.7550; a3=0.7381; a4=0; a5=0}
else if(SPP=='PA'){a1=1.7625; a2=1.3413; a3=0; a4=0.0957; a5=0}
else if(SPP=='YP'){a1=3.3543; a2=1.1627; a3=0; a4=0.0857; a5=0}
else if(SPP=='SU'){a1=1.8853; a2=1.1625; a3=0; a4=0.0656; a5=-0.0300}
else if(SPP=='CT'){a1=4.1711; a2=1.6275; a3=0; a4=0; a5=0}
else if(SPP=='QA'){a1=4.2030; a2=2.1290; a3=1.0000; a4=0; a5=0}
else if(SPP=='BP'){a1=6.2498; a2=0.8655; a3=0; a4=0; a5=0}
else if(SPP=='EC'){a1=2.9340; a2=2.5380; a3=0.8617; a4=0; a5=0}
else if(SPP=='BT'){a1=0.6847; a2=1.1050; a3=0; a4=0.1420; a5=-0.0265}
else if(SPP=='PY'){a1=2.9340; a2=2.5380; a3=0.8617; a4=0; a5=0}
else if(SPP=='BC'){a1=0.6210; a2=7.0590; a3=0.5441; a4=0; a5=0}
else if(SPP=='WO'){a1=1.8000; a2=1.8830; a3=0; a4=0; a5=0}
else if(SPP=='BR'){a1=0.9420; a2=3.5390; a3=0; a4=0; a5=0}
else if(SPP=='CK'){a1=0.5189; a2=1.4134; a3=0; a4=0.1365; a5=-0.0806}
else if(SPP=='PO'){a1=1.6125; a2=1.6669; a3=0; a4=0.0536; a5=0}
else if(SPP=='OK'){a1=1.8000; a2=1.8830; a3=0; a4=0; a5=0}
else if(SPP=='SO'){a1=0.5656; a2=1.6766; a3=0; a4=0.0739; a5=0}
else if(SPP=='QI'){a1=9.8187; a2=1.1343; a3=0; a4=0; a5=0}
else if(SPP=='WK'){a1=1.6349; a2=1.5443; a3=0; a4=0.0637; a5=-0.0764}
else if(SPP=='NP'){a1=4.8935; a2=1.6069; a3=0; a4=0; a5=0}
else if(SPP=='CO'){a1=2.1480; a2=1.6928; a3=-0.0176; a4=0.0569; a5=0}
else if(SPP=='SW'){a1=1.8000; a2=1.8830; a3=0; a4=0; a5=0}
else if(SPP=='SN'){a1=2.1480; a2=1.6928; a3=-0.0176; a4=0.0569; a5=0}
else if(SPP=='RO'){a1=2.8500; a2=3.7820; a3=0.7968; a4=0; a5=0}
else if(SPP=='SK'){a1=2.1517; a2=1.6064; a3=0; a4=0.0609; a5=0}
else if(SPP=='BO'){a1=4.5100; a2=1.6700; a3=0; a4=0; a5=0}
else if(SPP=='CB'){a1=2.1517; a2=1.6064; a3=0; a4=0.0609; a5=0}
else if(SPP=='OH'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='BU'){a1=2.3600; a2=3.5480; a3=0.7986; a4=0; a5=0}
else if(SPP=='YY'){a1=2.3600; a2=3.5480; a3=0.7986; a4=0; a5=0}
else if(SPP=='WR'){a1=11.6634; a2=1.0028; a3=0; a4=0; a5=0}
else if(SPP=='HK'){a1=7.1043; a2=1.3041; a3=0; a4=0.0456; a5=0}
else if(SPP=='PS'){a1=3.5393; a2=1.3939; a3=0; a4=0.0625; a5=0}
else if(SPP=='HY'){a1=4.5803; a2=1.0747; a3=0; a4=0.0661; a5=0}
else if(SPP=='BN'){a1=3.6031; a2=1.1472; a3=0; a4=0.1224; a5=0}
else if(SPP=='WN'){a1=3.6031; a2=1.1472; a3=0; a4=0.1224; a5=0}
else if(SPP=='OO'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='MG'){a1=8.2119; a2=0.9708; a3=0; a4=0; a5=0}
else if(SPP=='MV'){a1=8.2119; a2=0.9708; a3=0; a4=0; a5=0}
else if(SPP=='AP'){a1=4.1027; a2=1.3960; a3=1.0775; a4=0; a5=0}
else if(SPP=='WT'){a1=5.3409; a2=0.7499; a3=0; a4=0.1047; a5=0}
else if(SPP=='BG'){a1=5.5037; a2=1.0567; a3=0; a4=0.0880; a5=0.0610}
else if(SPP=='SD'){a1=7.9750; a2=0.8303; a3=0; a4=0.0423; a5=-0.0706}
else if(SPP=='PW'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='SY'){a1=-1.3973; a2=1.3756; a3=0; a4=0.1835; a5=0}
else if(SPP=='WL'){a1=1.6477; a2=1.3672; a3=0; a4=0.0846; a5=0}
else if(SPP=='BK'){a1=3.0012; a2=0.8165; a3=0; a4=0.1395; a5=0}
else if(SPP=='BL'){a1=2.8290; a2=3.4560; a3=0.8575; a4=0; a5=0}
else if(SPP=='SS'){a1=4.6311; a2=1.0108; a3=0; a4=0.0564; a5=0}
else if(SPP=='BW'){a1=1.6871; a2=1.2110; a3=0; a4=0.1194; a5=-0.0264}
else if(SPP=='WB'){a1=1.6871; a2=1.2110; a3=0; a4=0.1194; a5=-0.0264}
else if(SPP=='EL'){a1=2.8290; a2=3.4560; a3=0.8575; a4=0; a5=0}
else if(SPP=='AE'){a1=2.8290; a2=3.4560; a3=0.8575; a4=0; a5=0}
else if(SPP=='RL'){a1=9.0023; a2=1.3933; a3=0; a4=0; a5=-0.0785}
else if(SPP=='NC'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='BE'){a1=6.4741; a2=1.0778; a3=0; a4=0.0719; a5=-0.0637}
else if(SPP=='ST'){a1=6.4741; a2=1.0778; a3=0; a4=0.0719; a5=-0.0637}
else if(SPP=='AI'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='SE'){a1=6.9814; a2=1.6032; a3=0; a4=0; a5=0}
else if(SPP=='AH'){a1=0.9219; a2=1.6303; a3=0; a4=0.1150; a5=-0.1113}
else if(SPP=='DW'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='HT'){a1=2.9646; a2=1.9917; a3=0; a4=0.0707; a5=0}
else if(SPP=='HH'){a1=7.8084; a2=0.8129; a3=0; a4=0.0941; a5=-0.0817}
else if(SPP=='PL'){a1=4.1027; a2=1.3960; a3=1.0775; a4=0; a5=0}
else if(SPP=='PR'){a1=4.1027; a2=1.3960; a3=1.0775; a4=0; a5=0}
else{a1=0.3270; a2=5.1160; a3=0.5035; a4=0; a5=0}
      MCW=ifelse(MCW_EQ=="Bechtold" & DBH < 3.0,a1+(a2*3.0)+(a3*3.0**2)+(a4*CR)+(a5*HI)*(DBH/3.0),
          ifelse(MCW_EQ=="Bechtold" & DBH >= 3.0,a1+(a2*DBH)+(a3*DBH**2)+(a4*CR)+(a5*HI),
              ifelse(MCW_EQ=="Ek" & DBH < 3.0,a1+(a2*DBH**a3)*(DBH/3.0),
              ifelse(MCW_EQ=="Ek" & DBH >= 3.0,a1+(a2*DBH**a3),
                ifelse(MCW_EQ=="Krajicek" & DBH < 3.0,(a1 + (a2 * DBH))*(DBH/3.0),
                ifelse(MCW_EQ=="Krajicek" & DBH >= 3.0,(a1 + (a2 * DBH)),
                  ifelse(MCW_EQ=="Smith" & DBH < 3.0,(a1+(a2*DBH*2.54)+(a3*(DBH*2.54)**2)*3.28084)*(DBH/3.0),
                  ifelse(MCW_EQ=="Smith",(a1+(a2*DBH*2.54)+(a3*(DBH*2.54)**2)*3.28084),
                    a1 + (a2 * DBH**a3)))))))))
    return(MCW=MCW)}

tree$MCW=mapply(FVSNE.MCW,SPP=tree$SPP,MCW_EQ=tree$MCW_EQ,DBH=tree$DBH,CR=tree$CR,HI=tree$HI)


#Large-tree potential basal area growth (Table 4.7.1.1 in FVS-NE guide)
FVSNE.POTBAG=function(SPPGR,DBH,SPP_SI){
     if(SPPGR==1){b1=0.0008829; b2=0.0602785; b3=0.012785}
else if(SPPGR==2){b1=0.0009933; b2=0.0816995; b3=0.018831}
else if(SPPGR==3){b1=0.0008721; b2=0.057865; b3=0.013427}
else if(SPPGR==4){b1=0.0008236; b2=0.0549439; b3=0.011942}
else if(SPPGR==5){b1=0.0009252; b2=0.1134195; b3=0.0173}
else if(SPPGR==6){b1=0.0011303; b2=0.0934796; b3=0.015496}
else if(SPPGR==7){b1=0.0009252; b2=0.1134195; b3=0.0173}
else if(SPPGR==8){b1=0.0006634; b2=0.108347; b3=0.016835}
else if(SPPGR==9){b1=0.000905; b2=0.0517297; b3=0.012329}
else if(SPPGR==10){b1=0.0008737; b2=0.0940538; b3=0.009149}
else if(SPPGR==11){b1=0.0006634; b2=0.108347; b3=0.016835}
else if(SPPGR==12){b1=0.0007906; b2=0.0651982; b3=0.016191}
else if(SPPGR==13){b1=0.0007439; b2=0.0706905; b3=0.01624}
else if(SPPGR==14){b1=0.0006668; b2=0.0768212; b3=0.019046}
else if(SPPGR==15){b1=0.0009766; b2=0.0832328; b3=0.023978}
else if(SPPGR==16){b1=0.0007993; b2=0.0779654; b3=0.015963}
else if(SPPGR==17){b1=0.0006911; b2=0.0730441; b3=0.013029}
else if(SPPGR==18){b1=0.0008992; b2=0.0925395; b3=0.015004}
else if(SPPGR==19){b1=0.0008815; b2=0.1419212; b3=0.019904}
else if(SPPGR==20){b1=0.0011885; b2=0.092005; b3=0.016877}
else if(SPPGR==21){b1=0.0007929; b2=0.1568904; b3=0.016537}
else if(SPPGR==22){b1=0.0007417; b2=0.0867535; b3=0.014235}
else if(SPPGR==23){b1=0.0008769; b2=0.0866621; b3=0.01856}
else if(SPPGR==24){b1=0.0008238; b2=0.079066; b3=0.013762}
else if(SPPGR==25){b1=0.000892; b2=0.0979702; b3=0.018024}
else if(SPPGR==26){b1=0.000855; b2=0.0957964; b3=0.020843}
else if(SPPGR==27){b1=0.0009567; b2=0.1038458; b3=0.020653}
else if(SPPGR==28){b1=0.0003604; b2=0.0328767; b3=0.01162}
else {b1=0.0008829; b2=0.0602785; b3=0.012785}
    POTBAG=(b1*SPP_SI*(1-exp(-b2*DBH)))*0.7
    return(POTBAG=POTBAG)}

tree$POTBAG=mapply(FVSNE.POTBAG,SPPGR=tree$SPPGR,DBH=tree$DBH,SPP_SI=tree$SPP_SI)


#Large-tree growth modifier (Table 4.7.1.1 in FVS-NE guide)
FVSNE.GMOD=function(SPPGR,BAL){
     if(SPPGR==1){b1=0.0008829; b2=0.0602785; b3=0.012785}
else if(SPPGR==2){b1=0.0009933; b2=0.0816995; b3=0.018831}
else if(SPPGR==3){b1=0.0008721; b2=0.057865; b3=0.013427}
else if(SPPGR==4){b1=0.0008236; b2=0.0549439; b3=0.011942}
else if(SPPGR==5){b1=0.0009252; b2=0.1134195; b3=0.0173}
else if(SPPGR==6){b1=0.0011303; b2=0.0934796; b3=0.015496}
else if(SPPGR==7){b1=0.0009252; b2=0.1134195; b3=0.0173}
else if(SPPGR==8){b1=0.0006634; b2=0.108347; b3=0.016835}
else if(SPPGR==9){b1=0.000905; b2=0.0517297; b3=0.012329}
else if(SPPGR==10){b1=0.0008737; b2=0.0940538; b3=0.009149}
else if(SPPGR==11){b1=0.0006634; b2=0.108347; b3=0.016835}
else if(SPPGR==12){b1=0.0007906; b2=0.0651982; b3=0.016191}
else if(SPPGR==13){b1=0.0007439; b2=0.0706905; b3=0.01624}
else if(SPPGR==14){b1=0.0006668; b2=0.0768212; b3=0.019046}
else if(SPPGR==15){b1=0.0009766; b2=0.0832328; b3=0.023978}
else if(SPPGR==16){b1=0.0007993; b2=0.0779654; b3=0.015963}
else if(SPPGR==17){b1=0.0006911; b2=0.0730441; b3=0.013029}
else if(SPPGR==18){b1=0.0008992; b2=0.0925395; b3=0.015004}
else if(SPPGR==19){b1=0.0008815; b2=0.1419212; b3=0.019904}
else if(SPPGR==20){b1=0.0011885; b2=0.092005; b3=0.016877}
else if(SPPGR==21){b1=0.0007929; b2=0.1568904; b3=0.016537}
else if(SPPGR==22){b1=0.0007417; b2=0.0867535; b3=0.014235}
else if(SPPGR==23){b1=0.0008769; b2=0.0866621; b3=0.01856}
else if(SPPGR==24){b1=0.0008238; b2=0.079066; b3=0.013762}
else if(SPPGR==25){b1=0.000892; b2=0.0979702; b3=0.018024}
else if(SPPGR==26){b1=0.000855; b2=0.0957964; b3=0.020843}
else if(SPPGR==27){b1=0.0009567; b2=0.1038458; b3=0.020653}
else if(SPPGR==28){b1=0.0003604; b2=0.0328767; b3=0.01162}
else {b1=0.0008829; b2=0.0602785; b3=0.012785}
    GMOD=exp(-b3*(BAL))
    return(GMOD=GMOD)}

tree$GMOD=mapply(FVSNE.GMOD,SPPGR=tree$SPPGR,BAL=tree$BAL)
tree$GMOD=ifelse(tree$GMOD<=0.5,0.5,tree$GMOD)
tree$ABAG=tree$POTBAG*tree$GMOD
tree$DBHinc=((((0.00545415*DBH**2)+tree$ABAG)/0.00545415)**0.5-DBH)
tree$DBH2=tree$DBH+tree$DBHinc


#Carmean coefficients for calculating potential height increment (Table 4.7.2.1 in FVS-NE guide)
FVSNE.HTinc_pot=function(SPP,SPP_SI,HT){
     if(SPP=='BF'){b1=2.0770; b2=0.9303; b3=-0.0285; b4=2.8937; b5=-0.1414; b6=0}     
else if(SPP=='TA'){b1=1.1151; b2=1.0000; b3=-0.0504; b4=1.3076; b5=0.0009; b6=0}     
else if(SPP=='WS'){b1=1.3342; b2=1.0008; b3=-0.0401; b4=1.8068; b5=0.0248; b6=0}     
else if(SPP=='RS'){b1=1.3307; b2=1.0442; b3=-0.0496; b4=3.5829; b5=0.0945; b6=0}     
else if(SPP=='NS'){b1=8.6744; b2=0.9986; b3=-0.0031; b4=0.8904; b5=-0.0006; b6=0}     
else if(SPP=='BS'){b1=1.7620; b2=1.0000; b3=-0.0201; b4=1.2307; b5=0.0000; b6=0}     
else if(SPP=='PI'){b1=1.3307; b2=1.0442; b3=-0.0496; b4=3.5829; b5=0.0945; b6=0}     
else if(SPP=='RN'){b1=1.8900; b2=1.0000; b3=-0.0198; b4=1.3892; b5=0.0000; b6=0}     
else if(SPP=='WP'){b1=3.2425; b2=0.7980; b3=-0.0435; b4=52.0549; b5=-0.7064; b6=0}     
else if(SPP=='LP'){b1=1.1421; b2=1.0042; b3=-0.0374; b4=0.7632; b5=0.0358; b6=0}     
else if(SPP=='VP'){b1=0.7716; b2=1.1087; b3=-0.0348; b4=0.1099; b5=0.5274; b6=0}     
else if(SPP=='WC'){b1=1.9730; b2=1.0000; b3=-0.0154; b4=1.0895; b5=0.0000; b6=0}     
else if(SPP=='AW'){b1=1.5341; b2=1.0013; b3=-0.0208; b4=0.9986; b5=-0.0012; b6=0}     
else if(SPP=='RC'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='OC'){b1=1.9730; b2=1.0000; b3=-0.0154; b4=1.0895; b5=0.0000; b6=0}     
else if(SPP=='EH'){b1=2.1493; b2=0.9979; b3=-0.0175; b4=1.4086; b5=-0.0008; b6=0}     
else if(SPP=='HM'){b1=2.1493; b2=0.9979; b3=-0.0175; b4=1.4086; b5=-0.0008; b6=0}     
else if(SPP=='OP'){b1=1.8900; b2=1.0000; b3=-0.0198; b4=1.3892; b5=0.0000; b6=0}     
else if(SPP=='JP'){b1=1.6330; b2=1.0000; b3=-0.0223; b4=1.2419; b5=0.0000; b6=0}     
else if(SPP=='SP'){b1=1.4232; b2=0.9989; b3=-0.0285; b4=1.2156; b5=0.0088; b6=0}     
else if(SPP=='TM'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='PP'){b1=0.7716; b2=1.1087; b3=-0.0348; b4=0.1099; b5=0.5274; b6=0}     
else if(SPP=='PD'){b1=1.1266; b2=1.0051; b3=-0.0367; b4=0.6780; b5=0.0404; b6=0}     
else if(SPP=='SC'){b1=1.2096; b2=1.0027; b3=-0.0671; b4=1.2282; b5=0.0335; b6=0}     
else if(SPP=='OS'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='RM'){b1=2.9435; b2=0.9132; b3=-0.0141; b4=1.6580; b5=-0.1095; b6=0}     
else if(SPP=='SM'){b1=3.3721; b2=0.8407; b3=-0.0150; b4=2.6208; b5=-0.2661; b6=0}     
else if(SPP=='BM'){b1=3.3721; b2=0.8407; b3=-0.0150; b4=2.6208; b5=-0.2661; b6=0}   
else if(SPP=='SV'){b1=1.0645; b2=0.9918; b3=-0.0812; b4=1.5754; b5=-0.0272; b6=0}     
else if(SPP=='YB'){b1=2.2835; b2=0.9794; b3=-0.0054; b4=0.5819; b5=-0.0281; b6=0}     
else if(SPP=='SB'){b1=2.2835; b2=0.9794; b3=-0.0054; b4=0.5819; b5=-0.0281; b6=0}     
else if(SPP=='RB'){b1=2.2835; b2=0.9794; b3=-0.0054; b4=0.5819; b5=-0.0281; b6=0}     
else if(SPP=='PB'){b1=1.7902; b2=0.9522; b3=-0.0173; b4=1.1668; b5=-0.1206; b6=0}     
else if(SPP=='GB'){b1=2.2835; b2=0.9794; b3=-0.0054; b4=0.5819; b5=-0.0281; b6=0}     
else if(SPP=='HI'){b1=1.8326; b2=1.0015; b3=-0.0207; b4=1.4080; b5=-0.0005; b6=0}     
else if(SPP=='PH'){b1=1.8326; b2=1.0015; b3=-0.0207; b4=1.4080; b5=-0.0005; b6=0}     
else if(SPP=='SL'){b1=1.8326; b2=1.0015; b3=-0.0207; b4=1.4080; b5=-0.0005; b6=0}     
else if(SPP=='SH'){b1=1.8326; b2=1.0015; b3=-0.0207; b4=1.4080; b5=-0.0005; b6=0}     
else if(SPP=='MH'){b1=1.8326; b2=1.0015; b3=-0.0207; b4=1.4080; b5=-0.0005; b6=0}     
else if(SPP=='AB'){b1=29.7300; b2=0.3631; b3=-0.0127; b4=16.7616; b5=-0.6804; b6=0}     
else if(SPP=='AS'){b1=1.5768; b2=0.9978; b3=-0.0156; b4=0.6705; b5=0.0182; b6=0}     
else if(SPP=='WA'){b1=1.5768; b2=0.9978; b3=-0.0156; b4=0.6705; b5=0.0182; b6=0}     
else if(SPP=='BA'){b1=4.2286; b2=0.7857; b3=-0.0178; b4=4.6219; b5=-0.3591; b6=0}     
else if(SPP=='GA'){b1=1.6505; b2=0.9096; b3=-0.0644; b4=125.7045; b5=-0.8908; b6=0}     
else if(SPP=='PA'){b1=1.6505; b2=0.9096; b3=-0.0644; b4=125.7045; b5=-0.8908; b6=0}     
else if(SPP=='YP'){b1=1.2941; b2=0.9892; b3=-0.0315; b4=1.0481; b5=-0.0368; b6=0}     
else if(SPP=='SU'){b1=1.5932; b2=1.0124; b3=-0.0122; b4=0.6245; b5=0.0130; b6=0}     
else if(SPP=='CT'){b1=1.2941; b2=0.9892; b3=-0.0315; b4=1.0481; b5=-0.0368; b6=0}     
else if(SPP=='QA'){b1=5.2188; b2=0.6855; b3=-0.0301; b4=50.0071; b5=-0.8695; b6=0}     
else if(SPP=='BP'){b1=1.2941; b2=0.9892; b3=-0.0315; b4=1.0481; b5=-0.0368; b6=0}     
else if(SPP=='EC'){b1=1.3615; b2=0.9813; b3=-0.0675; b4=1.5494; b5=-0.0767; b6=0}     
else if(SPP=='BT'){b1=5.2188; b2=0.6855; b3=-0.0301; b4=50.0071; b5=-0.8695; b6=0}     
else if(SPP=='PY'){b1=1.2834; b2=0.9571; b3=-0.0680; b4=100.0000; b5=-0.9223; b6=0}     
else if(SPP=='BC'){b1=7.1846; b2=0.6781; b3=-0.0222; b4=13.9186; b5=-0.5268; b6=0}     
else if(SPP=='WO'){b1=4.5598; b2=0.8136; b3=-0.0132; b4=2.2410; b5=-0.1880; b6=0}     
else if(SPP=='BR'){b1=2.1037; b2=0.9140; b3=-0.0275; b4=3.7962; b5=-0.2530; b6=0}     
else if(SPP=='CK'){b1=2.1037; b2=0.9140; b3=-0.0275; b4=3.7962; b5=-0.2530; b6=0}     
else if(SPP=='PO'){b1=2.1037; b2=0.9140; b3=-0.0275; b4=3.7962; b5=-0.2530; b6=0}     
else if(SPP=='OK'){b1=2.1037; b2=0.9140; b3=-0.0275; b4=3.7962; b5=-0.2530; b6=0}     
else if(SPP=='SO'){b1=1.6763; b2=0.9837; b3=-0.0220; b4=0.9949; b5=0.0240; b6=0}     
else if(SPP=='QI'){b1=2.1037; b2=0.9140; b3=-0.0275; b4=3.7962; b5=-0.2530; b6=0}     
else if(SPP=='WK'){b1=1.3466; b2=0.9590; b3=-0.0574; b4=8.9538; b5=-0.3454; b6=0}     
else if(SPP=='PN'){b1=2.1037; b2=0.9140; b3=-0.0275; b4=3.7962; b5=-0.2530; b6=0}     
else if(SPP=='CO'){b1=1.9044; b2=0.9752; b3=-0.0162; b4=0.9262; b5=0.0000; b6=0}     
else if(SPP=='SW'){b1=1.3466; b2=0.9590; b3=-0.0574; b4=8.9538; b5=-0.3454; b6=0}     
else if(SPP=='SN'){b1=1.3466; b2=0.9590; b3=-0.0574; b4=8.9538; b5=-0.3454; b6=0}     
else if(SPP=='RO'){b1=0.4737; b2=1.2905; b3=-0.0236; b4=0.0979; b5=0.6121; b6=0}     
else if(SPP=='SK'){b1=1.2866; b2=0.9962; b3=-0.0355; b4=1.4485; b5=-0.0316; b6=0}     
else if(SPP=='BO'){b1=2.9989; b2=0.8435; b3=-0.0200; b4=3.4635; b5=-0.3020; b6=0}     
else if(SPP=='CB'){b1=1.0945; b2=0.9938; b3=-0.0755; b4=2.5601; b5=0.0114; b6=0}     
else if(SPP=='OH'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='BU'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='YY'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='WR'){b1=2.2835; b2=0.9794; b3=-0.0054; b4=0.5819; b5=-0.0281; b6=0}     
else if(SPP=='HK'){b1=1.5932; b2=1.0124; b3=-0.0122; b4=0.6245; b5=0.0130; b6=0}     
else if(SPP=='PS'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='HY'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='BN'){b1=1.2898; b2=0.9982; b3=-0.0289; b4=0.8546; b5=0.0171; b6=0}     
else if(SPP=='WN'){b1=1.2898; b2=0.9982; b3=-0.0289; b4=0.8546; b5=0.0171; b6=0}     
else if(SPP=='OO'){b1=0.9680; b2=1.0301; b3=-0.0468; b4=0.1639; b5=0.4127; b6=0}     
else if(SPP=='MG'){b1=1.3213; b2=0.9995; b3=-0.0254; b4=0.8549; b5=-0.0016; b6=0}     
else if(SPP=='MV'){b1=1.3213; b2=0.9995; b3=-0.0254; b4=0.8549; b5=-0.0016; b6=0}     
else if(SPP=='AP'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='WT'){b1=1.2721; b2=0.9995; b3=-0.0256; b4=0.7447; b5=-0.0019; b6=0}     
else if(SPP=='BG'){b1=1.3213; b2=0.9995; b3=-0.0254; b4=0.8549; b5=-0.0016; b6=0}     
else if(SPP=='SD'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='PW'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='SY'){b1=1.2941; b2=0.9892; b3=-0.0315; b4=1.0481; b5=-0.0368; b6=0}     
else if(SPP=='WL'){b1=2.1037; b2=0.9140; b3=-0.0275; b4=3.; b5= -0.2530; b6=0}     
else if(SPP=='BK'){b1=0.9680; b2=1.0301; b3=-0.0468; b4=0.1639; b5=0.4127; b6=0}     
else if(SPP=='BL'){b1=0.9680; b2=1.0301; b3=-0.0468; b4=0.1639; b5=0.4127; b6=0}     
else if(SPP=='SS'){b1=0.9680; b2=1.0301; b3=-0.0468; b4=0.1639; b5=0.4127; b6=0}     
else if(SPP=='BW'){b1=4.7633; b2=0.7576; b3=-0.0194; b4=6.5110; b5=-0.4156; b6=0}     
else if(SPP=='WB'){b1=4.7633; b2=0.7576; b3=-0.0194; b4=6.5110; b5=-0.4156; b6=0}     
else if(SPP=='EL'){b1=6.4362; b2=0.6827; b3=-0.0194; b4=10.9767; b5=-0.5477; b6=0}     
else if(SPP=='AE'){b1=6.4362; b2=0.6827; b3=-0.0194; b4=10.9767; b5=-0.5477; b6=0}     
else if(SPP=='RL'){b1=6.4362; b2=0.6827; b3=-0.0194; b4=10.9767; b5=-0.5477; b6=0}     
else if(SPP=='NC'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='BE'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='ST'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='AI'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='SE'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='AH'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='DW'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='HT'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='HH'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='PL'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else if(SPP=='PR'){b1=0.9276; b2=1.0591; b3=-0.0424; b4=0.3529; b5=0.3114; b6=0}     
else {b1=2.0770; b2=0.9303; b3=-0.0285; b4=2.8937; b5=-0.1414; b6=0}
  age_pred= round((1/b3)*(log(1-(((HT)-b6)/(b1*(SPP_SI)**b2))**(1/(b4*(SPP_SI)**b5)))),0)
  age_pred_plus10=age_pred+10;
  HT_pred_plus10= (b6+b1*SPP_SI**b2)*(1-exp(b3*age_pred_plus10))**(b4*SPP_SI**b5);
  HTinc_pot=HT_pred_plus10-HT;
    return(HTinc_pot=HTinc_pot)}

tree$HTinc_pot=mapply(FVSNE.HTinc_pot,SPP=tree$SPP,SPP_SI=tree$SPP_SI,HT=tree$HT)
tree$HTinc_mod=(1-((1-tree$GMOD)*(1-tree$RELHT)))*0.8;
tree$HTinc=(tree$HTinc_pot*tree$HTinc_mod)/10;

