setwd("D:/tesi2/dati")
studenti <- read.csv("studenti_2_marzo.csv", row.names=1)

voti_semestre <- read.csv("2021-02-17_10-35-13_polimi_valutazione.csv", sep = ';', dec = '.')
voti_singoli <- read.csv("2021-02-17_10-35-13_polimi_registro_valutazioni_flat.csv", sep = ';', dec = '.')
voti_singoli$Valutazione = (voti_singoli$Valutazione)/10
names(voti_singoli)[1] = "Oid"
voti_media_sett <- read.csv("2021-02-17_10-35-13_polimi_registro_valutazioni_settimana.csv", sep = ';', dec = '.')
names(voti_media_sett)[1] = "Oid"

#select only Math and 2018/2019

#keep only note, assenze, ritardi complessivi (non conto settimanale) e primo quadrimestre (21^ sett)
studenti <- studenti[,-c(seq(268,349),seq(394,475),seq(22,41),seq(43,61),seq(63,82),seq(84,102),seq(104,123),seq(125,143),seq(145,164),seq(166,184),seq(186,205),seq(207,225))]
#media ita e ing; no_ita e no_ing; assenze; note merito,impegno, disciplinari, ritardi (tengo 21 e 41)

voti_semestre$DataScrutinio <- as.Date(voti_semestre$DataScrutinio)
voti_semestre1718 <- voti_semestre[which(voti_semestre$DataScrutinio<='2018-08-31'),]
voti_semestre <- voti_semestre[which(voti_semestre$DataScrutinio>'2018-08-31'),]
voti_singoli$DataValutazione <- as.Date(voti_singoli$DataValutazione, "%d/%m/%Y")
voti_singoli1718 <- voti_singoli[which(voti_singoli$DataValutazione<='2018-08-31'),]
voti_singoli <- voti_singoli[which(voti_singoli$DataValutazione>'2018-08-31'),]
voti_media_sett1718 <- voti_media_sett[which((voti_media_sett$Anno==2018 & voti_media_sett$Settimana<=22) | voti_media_sett$Anno==2017),]
voti_media_sett <- voti_media_sett[which((voti_media_sett$Anno==2018 & voti_media_sett$Settimana>22) | voti_media_sett$Anno==2019),]

voti_semestre <- voti_semestre[voti_semestre$Insegnamento %in% c("MATEMATICA"),]
voti_media_sett <- voti_media_sett[voti_media_sett$Insegnamento %in% c("MATEMATICA"),]
voti_singoli <- voti_singoli[voti_singoli$Insegnamento %in% c("MATEMATICA"),]
voti_singoli1718 <- voti_singoli1718[voti_singoli1718$Insegnamento %in% c("MATEMATICA"),]

#add first semester grade to studenti dataset

voti_primo_quadrimestre = voti_semestre[which(voti_semestre$Periodo=="Secondo Periodo 2018-19" | voti_semestre$Periodo=="Primo Quadrimestre 2018-19"),c(1,4)]
names(voti_primo_quadrimestre)[2]="mate_primo_quad"

new_studenti = merge(studenti,voti_primo_quadrimestre,by="Oid")


#add variance of each student's  math grades (updated at 21^ sett and 41^ sett e anno precedente)
student_oid=unique(new_studenti$Oid)
varianze <- data.frame(
  Oid = numeric(0),
  varianza1718 = numeric(0),
  varianza21 = numeric(0),
  varianza41 = numeric(0)
)
for (Oid in student_oid) {
  voti_studente=voti_singoli[which(voti_singoli$Oid==Oid),7]
  voti_studente_primo_quad=voti_singoli[which(voti_singoli$Oid==Oid & voti_singoli$DataValutazione<'2019-01-17'),7]
  voti_studente1718=voti_singoli1718[which(voti_singoli1718$Oid==Oid),7]
  varianza1718=var(voti_studente1718)
  varianza41=var(voti_studente)
  varianza21=var(voti_studente_primo_quad)
  riga <- cbind(Oid, varianza1718, varianza41, varianza21)
  varianze <- rbind(varianze,riga)
}
varianze$varianza21[which(is.na(varianze$varianza21))]=0 #students with only one grade in the first semester (rows 55 and 244)

new_studenti=merge(new_studenti,varianze,by="Oid")
new_studenti$varianza1718=as.numeric(new_studenti$varianza1718)
new_studenti$varianza21=as.numeric(new_studenti$varianza21)
new_studenti$varianza41=as.numeric(new_studenti$varianza41)

#preprocessing

perc_doc_ruolo = new_studenti$DocentiRuolo/new_studenti$Docenti
new_studenti=cbind(new_studenti,perc_doc_ruolo)

which(new_studenti$DataNascita==2)
new_studenti[which(new_studenti$DataNascita==2),3]=1

Cittadinanza_bin=rep(1,dim(new_studenti)[1])
Cittadinanza_bin[which(new_studenti$Cittadinanza!="ITALIA")]=0
new_studenti=cbind(new_studenti,Cittadinanza_bin)

StatusFinale_bin_prec=rep(0,dim(new_studenti)[1])
StatusFinale_bin_prec[which(new_studenti$StatusFinale_prec=="P1")]=1
new_studenti=cbind(new_studenti,StatusFinale_bin_prec)

StatusFinale_bin=rep(0,dim(new_studenti)[1])
StatusFinale_bin[which(new_studenti$StatusFinale.x=="P1")]=1
new_studenti=cbind(new_studenti,StatusFinale_bin)

#delete the only student with Matematica_finale==3
which(new_studenti$Matematica_finale==3)
new_studenti <- new_studenti[which(new_studenti$Matematica_finale!=3),]

#rischio
new_studenti$Matematica_finale=as.numeric(new_studenti$Matematica_finale)
rischio=rep(1,dim(new_studenti)[1]) #at risk
rischio[which(new_studenti$Matematica_finale>5 & new_studenti$Matematica_finale<8)]=2 #medium
rischio[which(new_studenti$Matematica_finale>7)]=3 #no risk
new_studenti=cbind(new_studenti,rischio)

new_studenti$Classe[which(new_studenti$Settore=="Liceo Classico Paritario")] = sub("^", "C", new_studenti$Classe[which(new_studenti$Settore=="Liceo Classico Paritario")] )
new_studenti$Classe[which(new_studenti$Settore=="Liceo Internazionale per l'Intercultura")] = sub("^", "I", new_studenti$Classe[which(new_studenti$Settore=="Liceo Internazionale per l'Intercultura")] )
new_studenti$Classe[which(new_studenti$Settore=="Liceo Scientifico Paritario")] = sub("^", "S", new_studenti$Classe[which(new_studenti$Settore=="Liceo Scientifico Paritario")] )
new_studenti$Classe[which(new_studenti$Settore=="Liceo Scienze Umane Paritario")] = sub("^", "SU", new_studenti$Classe[which(new_studenti$Settore=="Liceo Scienze Umane Paritario")] )

assenze_perc21=new_studenti$assenze21/105 #5days x 21 weeks
assenze_perc41=new_studenti$assenze41/205 #5days x 41 weeks
new_studenti=cbind(new_studenti,assenze_perc21)
new_studenti=cbind(new_studenti,assenze_perc41)

write.table(new_studenti, "new_studenti.csv", sep = ";", dec = ",", row.names=FALSE)
