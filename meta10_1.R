library(tidyverse)
library(survey)
library(PNADcIBGE)

pnad_ODS <- function(ano_inicial, tri_inicial){
    
    media_maispobres <- function(x){
        quantil_40 <- survey::svyquantile(~VD4020, x, quantiles = .4, na.rm = T, ci = TRUE)
        x <- subset(x, VD4020<=quantil_40$VD4020[1] & Capital=="Município de São Paulo (SP)")
        avg <- survey::svymean(~VD4020, x, na.rm=T, deff=T)
        return(avg)
    }
    
    media_geral <- function(x){
        x <- subset(x, Capital=="Município de São Paulo (SP)")  
        avg <- survey::svymean(~VD4020, x, na.rm=T)
        return(avg)
    }
    
    if(ano_inicial%%1==0 & ano_inicial<2012){stop("Ano deve ser número inteiro maior ou igual a 2012.")}
    if(tri_inicial%%1==0 & (tri_inicial<1 | tri_inicial>4)){stop("Trimestre deve ser número inteiro entre 1 e 4")}
    
    resultado <- data.frame("ano" = NULL, "trimestre" = NULL, "renda.<40" = NULL, "renda.geral"=NULL)
    
    for (i in ano_inicial:lubridate::year(Sys.Date())) {
        
        if(i == ano_inicial){
            for (j in tri_inicial:4) {
                try(
                    x <- PNADcIBGE::get_pnadc(year = i, 
                                              quarter = j, 
                                              vars = c("Capital","VD4020")
                    ),silent = T
                )
                try(resultado <- rbind(resultado, list(i,j,media_maispobres(x),media_geral(x))),silent = T)
                if(exists("x")) rm(x)
            }
        }
        
        else{
            for(j in tri_inicial:4){
                try(
                    x <- PNADcIBGE::get_pnadc(year = i, 
                                              quarter = j, 
                                              vars = c("Capital","VD4020")
                    ),silent = T
                )
                try(resultado <- rbind(resultado, list(i,j,media_maispobres(x),media_geral(x))),silent = T)
                if(exists("x")) rm(x)
            }
        }
    }
    names(resultado) <- c("Ano", "Trimestre", "Média_<40%", "Média_Geral")
    return(resultado)
}
