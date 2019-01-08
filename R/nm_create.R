nm_create <- function(ret) {
  
    ret$Variability <- NA
    
    ret$TYPE <- gsub("[[:space:]]*", "", ret$TYPE)
    
    unknown_directive <- setdiff(ret$TYPE, c("",paste0("[",c("A","P","R","C",""),"]")))
    for(i in seq_along(unknown_directive)) warning("Unknown directive:", unknown_directive[i])
    
    # P; Lognormal or proportional
    pIdx <- which(ret$TYPE == "[P]")
    
    if(length(pIdx)>0){
      
      pIdx_offdiag <- pIdx[ret[pIdx,"ROW"]!=ret[pIdx,"COL"]]
      
      if(length(pIdx_offdiag)>0){
        warning("Lognormal directive indicated for off-diagonal elements, reporting correlation")
        ret[pIdx_offdiag,"TYPE"] <- "[R]"
        pIdx <- setdiff(pIdx,pIdx_offdiag)
      }
      
      ret$Variability[pIdx] <- "CV %"
      
    }
    
    # A; Additive
    aIdx <- which(ret$TYPE == "[A]")
    
    if(length(aIdx)>0){
      
      aIdx_offdiag <- aIdx[ret[aIdx,"ROW"]!=ret[aIdx,"COL"]]
      if(length(aIdx_offdiag)>0){
        ret[aIdx_offdiag,"TYPE"] <- "[R]"
        aIdx <- setdiff(aIdx,aIdx_offdiag)
      }
      
      ret$Variability[aIdx] <- "SD"
      
    }
    
    
    # R; Give correlation matrix values
    rIdx <- which(ret$TYPE == "[R]")
    
    if(length(rIdx)>0){
      
      ret$Variability[rIdx] <- ifelse(ret$ROW[rIdx]!=ret$COL[rIdx], "r", "SD")
      
    }
    
    
    # C; Give covariance matrix values
    cIdx <- which(ret$TYPE == "[C]")
    
    if(length(cIdx)>0){
      
      ret$Variability[cIdx] <- ifelse(ret$ROW[cIdx]!=ret$COL[cIdx], "Cov", "Var")
      
    }
    
    ret$Variability <- gsub("%", "\\%", ret$Variability, fixed = TRUE)

    for(i in grep("\\br\\b",ret$Variability)){
      t1 <- ret$ROW[i]
      t2 <- ret$COL[i]
      ret$LABEL[i] <- sprintf("Cor_{%s-%s}",
                              ret$LABEL[ret$ROW==t1 & ret$COL==t1],
                              ret$LABEL[ret$ROW==t2 & ret$COL==t2])
    }

  return(ret)
}
