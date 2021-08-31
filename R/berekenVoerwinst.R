#Jur Eekelder; 31-08-2021
#jur.eekelder@demarke.eu
#Script voor het maken van produceren van Voerwinst
#Idee gebaseerd op Marcel van Ittersum (mm.vanittersum@countus.nl) en Jaap Gielen (jaap.gielen@demarke.eu)


#INPUTS
#path_dataset --> string met path naar mappen met .DMPX files vanuit KLW. .DMPX files per jaar in een aparte submap.


#BENODIGDE FUNCTIES:
#getDataInFolder

opzettenDataBaseVKX <- function(path_dataset){
  
  #Laden libraries
  library(dplyr)  
  library(openxlsx)
  
  #Het laden van benodigde functies die ook op GIT staan.
  library(devtools)
  scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/scriptsalgemeen/main/R/getDataInFolder.R")
  
  
  for(script in scripts_to_source){
    source_url(script)
  }
  
  #Inlezen van dataset
  if(file.exists(path_dataset)){
    data_VKX_input = getDataInFolder(path_dataset)
  } else {
    stop("Path naar dataset bestaat niet!")
  }
  
  #Parameters
  prijs_vet = 293
  prijs_eiwit = 586
  prijs_lactose = 59
  procent_lactose = 4.51
  netto_weidepremie = 1.50
  criterium_weidegang = 720
  
  #Kolommen die we nodig hebben
  kolommen_lijst = c("melk_bedr",
                     "melk_vet",
                     "melk_eiwit",
                     "urenweidenmelkkoeienNA",
                     )
  
  bereken_melkprijs <- function(procent_vet, procent_eiwit, melk_bedr, prijs_vet, prijs_eiwit, prijs_lactose, procent_lactose, netto_weidepremie, uren_weidegang, criterium_weidegang){
    
    #Berekening basis melkprijs
    melkprijs  = procent_vet / 100 * + procent_eiwit / 100 * prijs_eiwit + procent_lactose / 100 * prijs_lactose
    
    #Als weide-uren > criterium, dan de weidepremie toevoegen aan melkprijs
    if(uren_weidegang >= criterium_weidegang){
      melkprijs = melkprijs + netto_weidepremie
    }
    
    return(melkprijs)
    
  }
  
  #Algemene observaties:
  # 
  # Afvoer vee
  # De sterfte was niet opgenomen waardoor afgevoerde dode dieren geld opleverden. In nieuwere versies van de klw is levend en dood afvoeren gescheiden. 
  # Dat is nog niet overal zo dus daarom hier op basis van aannames.
  # Uit cijfers van 2020 van Countus blijkt de sterfte onder melkkoeien 4% en onder kalveren 11%
  # Dit alles moet gecorrigeerd worden op de afvoercijfers. 
  # Voor koeien is het gemiddeld aantal aanwezige aantal koeien keer het sterftepercentage. Dat aantal is van de verkoop koeien afgehaald
  # Bij kalveren is het 11% sterfte van de geboren kalveren. Die zijn onbekend. Daarom aangenomen dat de afgevoerde kalveren + het aanwezige jongvee <1jr het aantal geboren kalveren is.
  # Daar het sterfte percentage van is het aantal gestorven nuka’s. Die zijn van de verkochte kalveren af gehaald. Soms is er geen verkoop van nuka’s ingevoerd. Die staan dan onder JV <1jr.  
  # Het komt dan voor dat er een negatief aantal bij nuka’s staat. Dat is niet erg want nuka’s en JV <1jr hebben dezelfde verkoopprijs. In totaal verkoop vee wordt het gesaldeerd.
  # 

  
}