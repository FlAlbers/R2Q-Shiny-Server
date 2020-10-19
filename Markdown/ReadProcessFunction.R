

# e1= ebene1, e2=ebene2, e3=ebene4, sa= Spalte, welche ausgegeben werden soll

## ---- rpf --------


rpf <- function(e1, e2 = NA, e3 = NA, sa = 7){


  if(is.na(e2)){
    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == as.character(e1))[1,1])
  } else {
    if(is.na(e3)){
      nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == as.character(e1) & datamassnahme$Ebene2 == as.character(e2))[1,1])
    } else {
      nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == as.character(e1) & datamassnahme$Ebene2 == as.character(e2) & datamassnahme$Ebene3 == as.character(e3))[1,1])
    }
  }
  
  if(is.na(datamassnahme[nr,sa])){
     val <- as.character("")
   } else {
     val <- as.character(datamassnahme[nr,sa])
   }

  return(val)
}

## ---- xrpf --------


xrpf <- function(e1, e2 = NA, e3 = NA, sa = 7){
  
  
  if(is.na(e2)){
    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == as.character(e1))[1,1])
  } else {
    if(is.na(e3)){
      nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == as.character(e1) & datamassnahme$Ebene2 == as.character(e2))[1,1])
    } else {
      nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == as.character(e1) & datamassnahme$Ebene2 == as.character(e2) & datamassnahme$Ebene3 == as.character(e3))[1,1])
    }
  }
  
  if(is.na(datamassnahme[nr,sa])){
    val <- as.character("")
  } else {
    val <- as.character(datamassnahme[nr,sa])
  }
  
  if(val=="1"){return("**[x]**")} else {return("**[ ]**")}
}







