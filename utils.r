# https://raporty.pse.pl/
# Ceny energii
# https://raporty.pse.pl/index.html?report=CRB-ROZL&state=Funkcjonowanie%20RB,Raporty%20dobowe%20z%20funkcjonowania%20RB,Podstawowe%20wska%C5%BAniki%20cenowe%20i%20kosztowe&date=2024-11-30&type=table&search=&

#Opis API:
#https://api.raporty.pse.pl/

library(httr)
library(rjson)
library(data.table)

#------------------------------------------------------------------------------------- 
#Rozliczeniowe ceny energi
#Składowa raportu:
#- Ceny i niezbilansowanie na RB
#Pozostałe składowe raportu:
#- {crb-prog}
#- {sk-d}
#- {cor-prog}
#- {cor-rozl}
#- {en-rozl}
#Mapowanie
#Doba: doba
#OREB: udtczas_oreb
#CEN: cen_rozl
#CKOEB: ckoeb_rozl
#CEBsr: ceb_sr_rozl
#CEBpp: ceb_pp_rozl
#Doba handlowa: business_date
#Data publikacji: source_datetime

get_CENY_ROZL <- function(date){

    ret <- data.frame()

    if(!dir.exists("work"))
      dir.create("work")

    if(!dir.exists("data"))
      dir.create("data")

    
    try({
      if(!file.exists(file.path("data",paste0("crb-rozlp_",date,".csv")))){
   
        url <- paste0("https://api.raporty.pse.pl/api/crb-rozl?$filter=business_date%20eq%20'", date, "'")
        response <- GET(url)
        if (status_code(response) == 200) {
            dir.create("work", showWarnings = FALSE) 
            writeBin(content(response, "raw"), paste0("work/", date, ".json"))
            cat("Plik został zapisany pomyślnie!\n")
        } else {
            cat("Błąd podczas pobierania danych. Status HTTP:", status_code(response), "\n")
        }
        
        options(warn=-1)
        d <- readLines(con=paste0("work/",date,".json"))
        options(warn=0)
        d <- do.call("rbind",lapply(fromJSON(d)[[1]],function(x){as.data.frame(x)}))
        unlink(paste0("work/",date,".json"))
        d$timestamp <- as.character(as.POSIXct(paste0(d$doba," ",
                                    substr(d$udtczas_oreb,1,5),":00"),tz="GMT"))
        d <- d[c("timestamp","cen_rozl")]
        d$timestamp <- as.character(d$timestamp)
        d$timestamp[nchar(d$timestamp)<19] <- paste0(
          d$timestamp[nchar(d$timestamp)<19]," 00:00:00")
        ret <- d 

        write.table(ret,file=file.path("data",paste0("crb-rozlp_",date,".csv")),
                    sep=";",dec=".",row.names=F)

      }else{
        ret <- read.table(file=file.path("data",paste0("crb-rozlp_",date,".csv")),
                          sep=";",dec=".",header=T)
      }

    },silent=T)
      
    return(ret)
}
#------------------------------------------------------------------------------------- 
# https://raporty.pse.pl/index.html?report=HIS-WLK-CAL&state=Funkcjonowanie%20KSE,Raporty%20dobowe%20z%20funkcjonowania%20KSE&date=2024-12-18&type=table&search=&
#Raporty dobowe z funkcjonowania KSE - Wielkości podstawowe
#Mapowanie
#Suma generacji jednostek grafikowych w KSE (JGw, JGm, JGz i JGa) [MW]: jg
#Sumaryczna generacja źródeł fotowoltaicznych: pv
#Sumaryczna generacja źródeł wiatrowych: wi
#Suma generacji Jednostek Grafikowych Agregatów JGa (JGa1) [MW]: jga
#Sumaryczna moc ładowania: jgm
#Doba: doba
#Suma generacji Jednostek Grafikowych Magazynów JGm z ZAK=1 (JGm1) [MW]: jgm1
#Suma generacji Jednostek Grafikowych Magazynów JGm z ZAK=2 (JGm2) [MW]: jgm2
#Suma generacji Jednostek Grafikowych Wytwórczych JGw z ZAK=1 (JGw1) [MW]: jgw1
#Suma generacji Jednostek Grafikowych Wytwórczych JGw z ZAK=2 (JGw2) [MW]: jgw2
#Suma generacji Jednostek Grafikowych Źródeł Wiatrowych i Fotowoltaicznych JGz z ZAK=1 (JGz1) [MW]: jgz1
#Suma generacji Jednostek Grafikowych Źródeł Wiatrowych i Fotowoltaicznych JGz z ZAK=2 (JGz2) [MW]: jgz2
#Suma generacji Jednostek Grafikowych Źródeł Wiatrowych i Fotowoltaicznych JGz z ZAK=3 (JGz3) [MW]: jgz3
#Sumaryczna Generacja Jednostek Wytwórczych nie uczestniczących aktywnie w Rynku Bilansującym [MW]: jnwrb
#Krajowe saldo wymiany międzysystemowej - równoległa [MW]: swm_r
#Krajowe saldo wymiany międzysystemowej - nierównoległa [MW]: swm_nr
#Doba (udtczas): udtczas
#ORED Jednostka czasu od-do: udtczas_oreb
#Doba handlowa: business_date
#Data publikacji: source_datetime
#Zapotrzebowanie na moc MW: zapotrzebowanie
#------------------------------------------------------------------------------------- 
get_HIS_WLK_CAL <- function(date){

    ret <- data.frame()

    if(!dir.exists("work"))
      dir.create("work")

    if(!dir.exists("data"))
      dir.create("data")

    
    try({
      if(!file.exists(file.path("data",paste0("his-wlk-cal_",date,".csv")))){
   
        url <- paste0("https://api.raporty.pse.pl/api/his-wlk-cal?$filter=business_date%20eq%20'", date, "'")
        response <- GET(url)
        if (status_code(response) == 200) {
            dir.create("work", showWarnings = FALSE) 
            writeBin(content(response, "raw"), paste0("work/", date, ".json"))
            cat("Plik został zapisany pomyślnie!\n")
        } else {
            cat("Błąd podczas pobierania danych. Status HTTP:", status_code(response), "\n")
        }
        
        options(warn=-1)
        d <- readLines(con=paste0("work/",date,".json"))

        options(warn=0)
        d <- as.data.frame(do.call("rbind",lapply(fromJSON(d)[[1]],function(x){as.data.frame(t(x))})))
        d <- d[c("udtczas","zapotrzebowanie","pv","wi","jgm")]
        colnames(d) <- c("timestamp","zapotrzebowanie_na_moc","pv","wiatr","sumatyczna_moc_ladowania_magazynow_energii")
        d$timestamp <- paste0(d$timestamp,":00")
        unlink(paste0("work/",date,".json"))
        d$timestamp[nchar(d$timestamp)<19] <- paste0(
          d$timestamp[nchar(d$timestamp)<19]," 00:00:00")
        ret <- d 
        for(j in 2:ncol(ret)){
            ret[,j] <- as.numeric(ret[,j])
        } 


        write.table(ret,file=file.path("data",paste0("his-wlk-cal_",date,".csv")),
                    sep=";",dec=".",row.names=F)

      }else{
        ret <- read.table(file=file.path("data",paste0("his-wlk-cal_",date,".csv")),
                          sep=";",dec=".",header=T)
      }


    },silent=T)
    
    return(ret)
}
#------------------------------------------------------------------------------------- 
ret <- get_CENY_ROZL("2024-12-06")
print(head(ret))

# ret <- get_CENY_ROZL(as.character(Sys.Date()-2))
# print(head(ret))

days <- as.character(seq(from=Sys.Date()-4, to=Sys.Date()-2, by="1 day"))
ceny_rozl <- as.data.frame(rbindlist(lapply(days,function(day){
 return(get_CENY_ROZL(day))
})))
print(head(ceny_rozl))
print(tail(ret))
#------------------------------------------------------------------------------------- 
ret <- get_HIS_WLK_CAL("2024-12-06")
print(head(ret))

# ret <- get_HIS_WLK_CAL(as.character(Sys.Date()-2))
# print(head(ret))

days <- as.character(seq(from=Sys.Date()-4, to=Sys.Date()-2, by="1 day"))
his_wlk_cal <- as.data.frame(rbindlist(lapply(days,function(day){
 return(get_HIS_WLK_CAL(day))
})))
print(head(his_wlk_cal))
print(tail(his_wlk_cal))
#------------------------------------------------------------------------------------- 




