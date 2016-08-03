leftoverCode <- function(){
  
  # load the ACG data and save it into Rdata file
  adgCSVdata <- read.csv(file = "sampleDataACG.csv", header = TRUE, na.strings = F)
  adgCSVdata <- as_data_frame(adgCSVdata)
  for(cntCol in 1:ncol(adgCSVdata)){
    
  }
  
  
  
  
  
  save(adgCSVdata, file = "rawSampleDataACG.Rdata")
  
  adgCSVdata2 <- read_csv(file = "sampleDataACG.csv",col_names = T, 
                          col_types = )
  
  
  
  # some basic statistics
  cat("Dimension of the data set:")
  print( dim(adgCSVdata))
  cat("Column names:")
  print( names(adgCSVdata))
  
  adgCSVdata$ADG.Codes[1]
  adgCSVdata$ADG.Vector[1]
  
  
}

aa <- function(){
  snortyPants <<- "a"
}

bb <- function(){
  snortyPants = wigglyPants = fluffyPants = 1
  aa(); 
  cat(snortyPants)
  1
}




singlePatientReport.old <- function(currPatient){
  
  header_str = c(
    sprintf("Patient ID:%s", currPatient$patient_id[1]), 
    sprintf("Sex: %s, Age:%s", currPatient$sex[1], currPatient$age[1]), 
    "", 
    ""
  )
  
  monthlyReport_list = list()
  for(cntM in 1:nrow(currPatient)){
    monthHeader = sprintf("Month %d", cntM)
    
    # ADG part
    adgHeader = "ADG:"
    adgCodesInText <- currPatient$adg_codes[cntM]
    adgCodesInNumnbers <- as.numeric(unlist(strsplit(adgCodesInText, split = " ")))
    adgDesc = c()
    for(cntADG in seq_along(adgCodesInNumnbers)){
      currADGNum = adgCodesInNumnbers[cntADG]
      adgDesc[cntADG] = sprintf("ADG%d:%s", currADGNum, adgMapKeyToDesc_vec[currADGNum])
    }
    
    # EDC part
    edcHeader = "EDC:"
    edcCodesInText <- currPatient$edc_codes[cntM]
    edcCodesSplit <- unlist(strsplit(edcCodesInText, split = " "))
    edcDesc_idx <- match(edcCodesSplit, edcMapKeyToDesc_df$key)
    edcDescSplit <- edcMapKeyToDesc_df$desc[edcDesc_idx]
    edcDesc = c()
    for(cntEDC in seq_along(edcCodesSplit)){
      currEDCCode = edcCodesSplit[cntEDC]
      currEDCDesc = edcDescSplit[cntEDC]
      if(is.na(currEDCDesc)){
        edcDesc[cntEDC] = sprintf("EDC(%s)", currEDCCode)
      }else{
        edcDesc[cntEDC] = sprintf("EDC(%s):%s", currEDCCode, currEDCDesc)
      }
    }
    
    # hospitalization part
    hospHeader = "Hospitalization:"
    hospDesc = c(
      sprintf("Unplanned: %s", currPatient$unplanned_inpatient_hospitalization_count[cntM]), 
      sprintf("Total: %s", currPatient$inpatient_hospitalization_count[cntM])
    )
    
    # save the complete report for this month
    monthlyReport_list[[cntM]] <- c("", monthHeader, "", adgDesc, "", edcDesc, "", hospHeader, hospDesc)
    
  }
  
  # concatenate all pieces and return the result
  longHorizontalLine <- c("", paste(rep("-", 40), collapse = ""), "")
  result_str <- c(header_str, unlist(monthlyReport_list), longHorizontalLine)
  returnList = list(report = result_str, monthlyReport = monthlyReport_list, header = header_str)
  returnList
}



firstGamesWithKnitr <- function(){
  library(knitr)
  firstTry_df <- data.frame(month = c(1,2,"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"), 
                            adgs = c("snort\n snort", "wag\nwag\nwag", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"), 
                            procedures  = c("planned:1\n unplanned:2", "planned:0\n unplanned:1", 
                                            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  aa <- kable(firstTry_df, format = "html", col.names = names(firstTry_df), 
              caption = "patient1")
  
  zz <- file("all.html", open = "wt")
  sink(zz)
  sink(zz, type = "message")
  try(print(aa))
  ## back to the console
  sink(type = "message")
  sink()
  file.show("all.html")
  
  fileConn<-file("wag.html")
  writeLines(aa, fileConn)
  close(fileConn)
  
  # let us try to work with xtable
  xtbl <- xtable(firstTry_df)
  align(xtbl) <- "|l|l|l|l|"
  
  zz <- file("all.html", open = "wt")
  sink(zz)
  sink(zz, type = "message")
  try(print(xtbl, type = "html", tabular.environment="tabularx", width=1))
  ## back to the console
  sink(type = "message")
  sink()
  file.show("all.html")
  
  
  #kable(x, format, digits = getOption("digits"), row.names = NA, col.names = colnames(x), 
  #      align, caption = NULL, escape = TRUE, ...)
}




aa <- function(){
  # add emergency visits count
  numOfEDvisits <- as.numeric(monthData_df$emergency_visit_count)
  if(numOfEDvisits>0){
    highlights_list[length(highlights_list) + 1] = sprintf("%d ED visits", numOfEDvisits); 
  }
  
  # major procedures
  numOfMajorProc <- as.numeric(monthData_df$major_procedure) 
  if(numOfMajorProc>0){
    highlights_list[length(highlights_list) + 1] = sprintf("%d Major procedures", numOfMajorProc); 
  }
  
  # unplanned hospitalizations
  numOfUnplanned <- as.numeric(monthData_df$unplanned_inpatient_hospitalization_count) 
  if(numOfUnplanned > 0){
    highlights_list[length(highlights_list) + 1] = sprintf("%d unplanned hospitalizations", numOfUnplanned); 
  }
  
  # planned hospitalizations
  numOfPlanned <- as.numeric(monthData_df$all_cause_inpatient_hospitalization_count) - numOfUnplanned
  if(numOfPlanned > 0){
    highlights_list[length(highlights_list) + 1] = sprintf("%d planned hospitalizations", numOfPlanned); 
  }
  
  # hospital dominant morbidity types
  numOfHospDomMorbTypes <- as.numeric(monthData_df$hospital_dominant_morbidity_types)
  if(numOfHospDomMorbTypes > 0){
    highlights_list[length(highlights_list) + 1] = sprintf("%d hosp. dominant morbidity", numOfHospDomMorbTypes); 
  }
  
  # number of outpatient visits
  numOfOPvisits <- as.numeric(monthData_df$outpatient_visit_count)
  if(numOfOPvisits > 0){
    highlights_list[length(highlights_list) + 1] = sprintf("%d outpatient visits", numOfOPvisits); 
  }
  
  # total number of hospital days
  totalHospDays <- as.numeric(monthData_df$inpatient_hospitalization_days)
  if(totalHospDays>0){
    highlights_list[length(highlights_list) + 1] = sprintf("Total %d of inpatient days", totalHospDays); 
  }
  
  # number of chronic conditions
  numOfChronCond <- as.numeric(monthData_df$chronic_condition_count)
  if(numOfChronCond > 0){
    highlights_list[length(highlights_list) + 1] = sprintf("%d chronic conditions", numOfChronCond); 
  }
}


# This auxiliary function returns data frame with the following columns: 
# *dayNum, weekNum, dayOfTheWeek, date
# * beginDate_str, endDate_str - dates of the beginning and the end of the 
# interval of interest given in format "MM-DD-YYYY".
# * 
detalizeCalendarInterval <- function(beginDate_str, endDate_str){
  
  # test the input vars
  assert_that(is.character(beginDate_str), is.character(endDate_str))
  
  # some constants
  inputDateFormat <- "%m-%d-%Y"
  sundayString <- "Sunday"
  mondayString <- "Monday"
  
  # translate data from string to Date object
  
  bgnDate <- as.Date(beginDate_str, inputDateFormat)
  endDate <- as.Date(endDate_str, inputDateFormat)
  
  # get the number of days in the interval
  numOfDaysInInterval <- endDate - bgnDate + 1
  assert_that(numOfDaysInInterval>=0);  
  
  # generate all the dates in the interval
  datesInInterval <- bgnDate + 0:(numOfDaysInInterval-1)
  
  # get day of the week for all these dates
  weekdaysInInterval <- weekdays(datesInInterval)
  
  # number weeks by the number of Sundays so far
  weekNumInInteval <- rep(NA, length(weekdaysInInterval))
  weekNumInInteval[1] = 1; # first day belongs to week 1
  sundayIdxs <- which(weekdaysInInterval == sundayString); # Sunday always marks weeks end
  numOfSundays <- length(sundayIdxs)
  weekNumInInteval[sundayIdxs] <- 1:numOfSundays
  
  # if last day of the interval is not Sunday
  if(is.na(weekNumInInteval[numOfDaysInInterval])){
    weekNumInInteval[numOfDaysInInterval] = numOfSundays + 1;
  }
  
  # now I need to propagate the numbers backward, i.e., week number given to Sunday
  # should be propagated to all days before it until previous Sunday 
  weekNumInInteval <- weekNumInInteval %>% rev %>% na.locf %>% rev
  detalizeCalendarInterval_df <- data.frame(date = datesInInterval, 
                                            weekday = weekdaysInInterval,
                                            weekNum = weekNumInInteval)
    
  # find indexes of beginnings and endings of all complete weeks
  numOfDaysInFirstWeek <- sum(weekNumInInteval == 1)
  numOfDaysInLastWeek <- sum(weekNumInInteval == max(weekNumInInteval))
  uniqueWeekNums <- 1:weekNumInInteval[numOfDaysInInterval]
  # remove the first week if incomplete
  if(numOfDaysInFirstWeek<7){
    uniqueWeekNums <- setdiff(uniqueWeekNums, 1)
  }
  
  # remove the last week if incomplete
  if(numOfDaysInLastWeek<7){
    uniqueWeekNums <- setdiff(uniqueWeekNums, max(uniqueWeekNums))
  }
  
  #
  completeWeeksBeginIdx <- which()
  
}

test.detalizeCalendarInterval <- function(){
  
  
  # first test
  beginDate_str = "01-01-2010" 
  endDate_str = "12-31-2010"
  
  calendarIntervalData_list <- detalizeCalendarInterval(beginDate_str, endDate_str)
}



# create the function that is a loader, i.e., it loads the CSv file, 
# it checks whether all possible columns are covered. 
# uncovered columns are left in the string format and warned about
# the rest of the columns are transformed to the corresponding type
# while any inconsistency also leaves the column untransformed and is warned about
# we also warn about absence of columns. Perhaps the associated warning attribute should be structured. 
aa <- function(){
  
  
  stringCols <- c("patient_id", "diabetes_untreated_rx")
  
  factorCols <- c("sex")
  
  # "NP"  "Rx"  "BTH" "ICD" "TRT"
  conditionCols <- c("bipolar_disorder_condition", "congestive_heart_failure_condition", "depression_condition", 
                     "diabetes_condition", "glaucoma_condition", "human_immunodeficiency_virus_condition",
                     "hypertension_condition", "immuno_suppression_transplant_condition", "ischemic_heart_disease_condition",
                     "osteoporosis_condition", "parkinsons_disease_condition", 
                     "rheumatoid_arthritis_condition", "schizophrenia_condition", "seizure_disorders_condition", 
                     "chronic_obstructive_pulmonary_disease_condition", "chronic_renal_failure_condition", "low_back_pain_condition", 
                     "hypothyroidism_condition", "persistent_asthma_condition", 
                     "bipolar_disorder_condition", "congestive_heart_failure_condition")
  
  colNamesToNumeric_list <- c("age", "pharmacy_cost", "total_cost", "inpatient_hospitalization_count",
                              "inpatient_hospitalization_days", "emergency_visit_count", "outpatient_visit_count", 
                              "all_cause_inpatient_hospitalization_count", "unplanned_inpatient_hospitalization_count", "readmission_30_day_count",
                              "unplanned_readmission_30_day_count", "hospital_dominant_morbidity_types", "chronic_condition_count", 
                              "active_ingredient_count", "major_adg_count", "hospital_dominant_morbidity_types", "specialty_count", 
                              "diabetes_rx_gaps", "diabetes_mpr", "diabetes_csa")  
  
  zeroOneCols <- c("pregnant", "delivered", "dialysis_service", "nursing_service",  "major_procedure", 
                   "cancer_treatment", "psychotherapy_service", "mechanical_ventilation", 
                   "disorders_of_lipid_metabolism_condition")
  nyCols <- c("pregnancy_without_delivery", "frailty_flag", "generalist_seen")
  
  specialFormat <- c("acg_code", "frailty_concepts", "adg_vector", "edc_codes", "rxmg_codes", 
                     "adg_codes")
  
  
  
}


microbenchmarking.Reduce <- function(){
  microbenchmark({
    for(k in 1:ncol(currF)){
      fUpdNumerator <- lambdaTPM*currFnormSquare[,k]; 
      fUpdDenominator <- rep(lambdaTPM, nrow(currF)); 
      for(l in seq_along(patientMat_list)){
        currG <- perPatientCodingVec_list[[l]][[r]] # coding vector of patient l that corresponds to pattern r
        currPatMat <- patientMat_list[[l]]; 
        currPatApproxMat <- patientMatApprox_list[[l]]
        for(j in k:ncol(currPatApproxMat) ){
          # min(c(ncol(currPatApproxMat), k + ncol(currF)-1))
          # some auxiliary expressions named using notations from the paper
          YhatF <- currPatApproxMat[,j]*currFnormSquare[,k]
          XF <- currPatMat[,j]*currFnormSquare[,k]
          Yhatg <- (currPatApproxMat[,j])^(divergBeta-2)*currG[j-k+1]
          fUpdNumerator <- fUpdNumerator + (currPatMat[,j] + YhatF)*Yhatg; 
          fUpdDenominator <- fUpdDenominator + (currPatApproxMat[,j] + XF)*Yhatg
        }
      }
      # now test different combinations of zero values in numerator and denominator
      bothZero_cond = (fUpdNumerator == 0) & (fUpdDenominator == 0)
      newF[bothZero_cond, k] = currF[bothZero_cond, k]
      
      numZeroDenNZ_cond = (fUpdNumerator == 0) & (fUpdDenominator != 0)
      newF[bothZero_cond, k] = 0
      
      numNZ = (fUpdNumerator != 0)
      newF[numNZ,k] <- currF[numNZ,k]*(fUpdNumerator[numNZ]/fUpdDenominator[numNZ])^etaBeta
    }
  },
  {
    for(k in 1:tpmHorizon){
      
      # create an auxiliary matrix with number of columns equal to the number of weeks
      # where all columns are equal to the k-th column of the currently processed TPM
      currFkDuplicateTtimes <- matrix(data = rep(currFnormSquare[,k], patMatHorizon), nrow = numOfFeat, ncol = patMatHorizon)
      fUpdNumerator <- lambdaTPM*currFnormSquare[,k]; 
      fUpdDenominator <- rep(lambdaTPM, numOfFeat); 
      
      for(l in seq_along(patientMat_list)){
        # coding vector of patient l that corresponds to pattern r
        currG <- perPatientCodingVec_list[[l]][[r]] 
        
        # create a matrix where each row is the appropriately shifted currG
        activeColumns <- k:patMatHorizon; 
        shiftedGvec <- c(rep(0,k-1), currG[activeColumns])
        shiftedGmat <- matrix(data = rep(shiftedGvec, numOfFeat), nrow = numOfFeat, ncol = patMatHorizon, byrow = T)
        
        # current patient matrix and its approximation
        currPatMat <- patientMat_list[[l]]; 
        currPatApproxMat <- patientMatApprox_list[[l]]
        
        # update numerator
        updNumMatrix <- (currPatMat + currPatApproxMat*currFkDuplicateTtimes)*(currPatApproxMat)^(divergBeta-2)*shiftedGmat
        fUpdNumerator <- fUpdNumerator + rowSums(updNumMatrix)
        
        # update denominator
        updDenMatrix <- (currPatApproxMat + currPatMat*currFkDuplicateTtimes)*(currPatApproxMat)^(divergBeta-2)*shiftedGmat
        fUpdDenominator <- fUpdDenominator + rowSums(updDenMatrix)
      }
      # now test different combinations of zero values in numerator and denominator
      bothZero_cond = (fUpdNumerator == 0) & (fUpdDenominator == 0)
      newF[bothZero_cond, k] = currF[bothZero_cond, k]
      
      numZeroDenNZ_cond = (fUpdNumerator == 0) & (fUpdDenominator != 0)
      newF[bothZero_cond, k] = 0
      
      numNZ = (fUpdNumerator != 0)
      newF[numNZ,k] <- currF[numNZ,k]*(fUpdNumerator[numNZ]/fUpdDenominator[numNZ])^etaBeta
    }
  }, 
                 times = 10)
}
  

profilingFunc <- function(){
  
  for(cnt in 1:10){
    
    for(k in 1:tpmHorizon){
      
      # create an auxiliary matrix with number of columns equal to the number of weeks
      # where all columns are equal to the k-th column of the currently processed TPM
      currFkDuplicateTtimes <- matrix(data = rep(currFnormSquare[,k], patMatHorizon), nrow = numOfFeat, ncol = patMatHorizon)
      #currFkDuplicateTtimes <- currFnormSquare[,k]
      fUpdNumerator <- lambdaTPM*currFnormSquare[,k]; 
      fUpdDenominator <- rep(lambdaTPM, numOfFeat); 
      
      for(l in seq_along(patientMat_list)){
        # coding vector of patient l that corresponds to pattern r
        currG <- perPatientCodingVec_list[[l]][[r]] 
        
        # create a matrix where each row is the appropriately shifted currG
        activeColumns <- k:patMatHorizon; 
        shiftedGvec <- c(rep(0,k-1), currG[activeColumns])
        #shiftedGmat <- matrix(data = rep(shiftedGvec, numOfFeat), nrow = numOfFeat, ncol = patMatHorizon, byrow = T)
        
        # current patient matrix and its approximation
        currPatMat <- patientMat_list[[l]]; 
        currPatApproxMat <- patientMatApprox_list[[l]]
        #currPatApproxMatToBetaMinusTwo <- (currPatApproxMat)^(divergBeta-2)
        #currYg <- patientMatApproxToBetaMinusTwo_list[[l]]*shiftedGmat
        currYg <- patientMatApproxToBetaMinusTwo_list[[l]]
        #currFYg <- currFkDuplicateTtimes*currYg
        #currPatApproxMatToBetaMinusTwo <- patientMatApproxToBetaMinusTwo_list[[l]]
        
        # update numerator
        updNumMatrix <- (currPatMat + currPatApproxMat*currFkDuplicateTtimes)*currYg
        #updNumMatrix <- currPatMat*currYg + currPatApproxMat*currFYg
        #fUpdNumerator <- fUpdNumerator + rowSums(updNumMatrix)
        fUpdNumerator <- fUpdNumerator + updNumMatrix %*% shiftedGvec
        
        # update denominator
        updDenMatrix <- (currPatApproxMat + currPatMat*currFkDuplicateTtimes)*currYg
        #updDenMatrix <- currPatApproxMat*currYg + currPatMat*currFYg
        #fUpdDenominator <- fUpdDenominator + rowSums(updDenMatrix)
        fUpdDenominator <- fUpdDenominator + updDenMatrix %*% shiftedGvec
      }
      # now test different combinations of zero values in numerator and denominator
      bothZero_cond = (fUpdNumerator == 0) & (fUpdDenominator == 0)
      newF[bothZero_cond, k] = currF[bothZero_cond, k]
      
      numZeroDenNZ_cond = (fUpdNumerator == 0) & (fUpdDenominator != 0)
      newF[bothZero_cond, k] = 0
      
      numNZ = (fUpdNumerator != 0)
      newF[numNZ,k] <- currF[numNZ,k]*(fUpdNumerator[numNZ]/fUpdDenominator[numNZ])^etaBeta
    }
  }
}

benchmarkingWightedRowSums <- function(){
  
  k = 1
  activeColumns <- k:patMatHorizon; 
  shiftedGvec <- c(rep(0,k-1), currG[activeColumns])
  currPatMat <- patientMat_list[[l]]; 
  currPatApproxMat <- patientMatApprox_list[[l]]
  
  
  microbenchmark({
    shiftedGmat <- matrix(data = rep(shiftedGvec, numOfFeat), nrow = numOfFeat, ncol = patMatHorizon, byrow = T)
    currYg <- patientMatApproxToBetaMinusTwo_list[[l]]*shiftedGmat
  }, {
    currYg <- patientMatApproxToBetaMinusTwo_list[[l]]%*%shiftedGvec
  },
  times = 1000)
  
  f1 <- function(x){sum(x*shiftedGvec)}
  
  microbenchmark(
    {rowSums(updDenMatrix*shiftedGmat)},
    {updDenMatrix%*%shiftedGvec}
  ,  
  times = 1000)
  
  
  
  
  
  
  # current patient matrix and its approximation
  
  #currPatApproxMatToBetaMinusTwo <- (currPatApproxMat)^(divergBeta-2)
  
  currFYg <- currFkDuplicateTtimes*currYg
  
   
}
  

profilingReport <- function(){
  
  patientMatApproxToBetaMinusTwo_list <- lapply(patientMatApprox_list, function(x){x^(divergBeta-2)})
  
  library(microbenchmark)
  microbenchmark(profilingFunc(), times = 1)
  
  Rprof(filename = "Snort")
  profilingFunc()
  Rprof()
  summaryRprof("Snort")
  
  RRprofStart(filename = "Snort.out", interval = )
  profilingFunc()
  RRprofStop()
  RRprofReport()
}



#
# We now proceed to make the g_l update process quicker
#


glUpdate.Code0<- function(l){
  newCodingVec_list = list()
  for(r in seq_along(tempPattMat_list)){
    # current normalized pattern
    currFnorm = normTempPattMat_list[[r]]
    
    # current coding vector
    currG <- perPatientCodingVec_list[[l]][[r]]
    newG <- rep(NA, length(currG))
    # for each time step
    for(k in 1:length(currG)){
      gUptNumerator <- 0; 
      gUptDenominator <- lambdaCodingVec; 
      # for each feature
      for(i in 1:nrow(currFnorm)){
        # for each column in TPM within the window
        for(j in k:min(c(k+ncol(currFnorm)-1, length(currG)))){
          gUptNumerator <- gUptNumerator + patientMat_list[[l]][i,j]*(patientMatApprox_list[[l]][i,j])^(divergBeta - 2)*currFnorm[i, j-k+1]
          gUptDenominator <- gUptDenominator + (patientMatApprox_list[[l]][i,j])^(divergBeta - 1)*currFnorm[i, j-k+1]
        }
      }
      if(gUptNumerator == 0 ){
        if(gUptDenominator == 0){
          newG[k] <- currG[k]
        }else{
          newG[k] <- 0
        }
      }else{
        newG[k] <- currG[k]*(gUptNumerator/gUptDenominator)^etaBeta
      }
    }
    newCodingVec_list[[r]] <- newG
  }
  newCodingVec_list; 
}


glUpdate.Code1<- function(l){
  newCodingVec_list = list()
  for(r in seq_along(tempPattMat_list)){
    # current normalized pattern
    currFnorm = normTempPattMat_list[[r]]
    
    # current coding vector
    currG <- perPatientCodingVec_list[[l]][[r]]
    newG <- rep(NA, length(currG))
    # for each time step
    for(k in 1:length(currG)){
      gUptNumerator <- 0; 
      gUptDenominator <- lambdaCodingVec; 
      # for each feature
      for(i in 1:nrow(currFnorm)){
        # for each column in TPM within the window
        for(j in k:min(c(k+ncol(currFnorm)-1, length(currG)))){
          gUptNumerator <- gUptNumerator + patientMat_list[[l]][i,j]*patientMatApproxToBetaMinusTwo_list[[l]][i,j]*currFnorm[i, j-k+1]
          gUptDenominator <- gUptDenominator + patientMatApprox_list[[l]][i,j]*patientMatApproxToBetaMinusTwo_list[[l]][i,j]*currFnorm[i, j-k+1]
        }
      }
      if(gUptNumerator == 0 ){
        if(gUptDenominator == 0){
          newG[k] <- currG[k]
        }else{
          newG[k] <- 0
        }
      }else{
        newG[k] <- currG[k]*(gUptNumerator/gUptDenominator)^etaBeta
      }
    }
    newCodingVec_list[[r]] <- newG
  }
  newCodingVec_list; 
}

glUpdate.Code2<- function(){
  fixedRPerPatientCodingVec <- list()
  for(l in seq_along(patientMat_list)){
    
    currG <- perPatientCodingVec_list[[l]][[r]] 
    
    gUptNumerator = c(); 
    gUptDenominator = c(); 
    
    for(k in 1:patMatHorizon){
      # add sparsity
      if(currG[k] != 0){
        activeIdxInF <- 1:min(c(tpmHorizon, patMatHorizon - k + 1))
        activeIdxInPatMat <- (k-1) + activeIdxInF
        
        # update for the numerator
        tmp <- patientMat_list[[l]][, activeIdxInPatMat]*patientMatApproxToBetaMinusTwo_list[[l]][,activeIdxInPatMat]*currFnorm[, activeIdxInF]
        gUptNumerator[k] <- sum(tmp)
        
        # update for the denominator
        tmp <- patientMatApprox_list[[l]][, activeIdxInPatMat]*patientMatApproxToBetaMinusTwo_list[[l]][,activeIdxInPatMat]*currFnorm[, activeIdxInF]
        gUptDenominator[k] <- sum(tmp) + lambdaCodingVec
      }else{
        gUptNumerator[k] = 1; 
        gUptDenominator[k] = 1; 
      }
    }
    # make sure there are no problems
    cond_bothZero = (gUptNumerator == 0) & (gUptDenominator == 0)
    gUptNumerator[cond_bothZero] = 1; gUptDenominator[cond_bothZero] = 1; 
    

    newG <- currG*(gUptNumerator/gUptDenominator)^etaBeta
    if(any(is.na(newG))){
      stop("Some values of the coding vector equal NA after update")
    }
    fixedRPerPatientCodingVec[[l]] <- newG
  }
  fixedRPerPatientCodingVec
}




benchmark.Different.glUpdateCodes <- function(){
  newPerPatientCodingVec_list <- list()
  l = 1; 
  microbenchmark({newCodingVecList.Code0 <- glUpdate.Code0(l)}
                 ,times = 1)
  
  microbenchmark({newCodingVecList.Code1 <- glUpdate.Code1(l)}
                 ,times = 1)
  expect_equal(newCodingVecList.Code0, newCodingVecList.Code1)
  
  
  # new attempt code
  newCodingVec_list = list()
  for(r in seq_along(tempPattMat_list)){
    
    # init
    fixeRCodingVec_list <- list()
    
    # get the currently considered TPM
    currFnorm = normTempPattMat_list[[r]]; 
    
    # create a list of matrices that are shifted 
    fNormShiftedByK <- list()
    for(k in 1:patMatHorizon){
      numOfLeadZeros <- k-1; 
      activeIdxs <- 1:min(c(tpmHorizon, patMatHorizon - k + 1))
      numOfTailZeros <- max(c(0, patMatHorizon - (k+tpmHorizon) + 1))
      fNormShiftedByK[[k]] <- t(cbind(matrix(0, nrow = numOfFeat, ncol = numOfLeadZeros), currFnorm[, activeIdxs], matrix(0, nrow = numOfFeat, ncol = numOfTailZeros)))
    }
    
    # for a fixed r, calculate all possible shiftings of F
    for(l in seq_along(patientMat_list)){
      for(k in 1:patMatHorizon){
        gUptNumeratorMat <- (patientMat_list[[l]]*patientMatApproxToBetaMinusTwo_list[[l]])%*% fNormShiftedByK[[k]]
        gUptNumerator <- sum(gUptNumeratorMat)
      }
    }
    
    Rprof("Snort")
    glUpdate.Code2()
    Rprof()
    summaryRprof("Snort")
    
    newCodingVec_list[[r]] <- fixeRCodingVec_list
  }
  
}


investigateMysteriousPatientWithLongHospitalization <- function(){
  load("C:/Users/kdiagil1/Dropbox/PostDoc/CPHIT/SampleDataACG/allPatient12monthDataAugmentedWithDiabetesRelatedData.Rdata")
  patientID <- "s103AAAAAABGMSSP"
  outPatientRow <- which(allPatientData_df$patient_id == patientID)
  singlePatientData <- allPatientData_df[outPatientRow,]
  
  source("acgReportingLibrary.R")
  fn_edcKeyToDesc = "edc_labels_klaus.txt"
  edcMapKeyToDesc_df <- edcMapKeyToDescription.readFromFile(fn_edcKeyToDesc)
  fn_rxmgKeyToDesc = "rxmg_labels_klaus.txt"
  rxmgMapKeyToDesc_df <- edcMapKeyToDescription.readFromFile(fn_rxmgKeyToDesc)
  fn_adgMapToDesc = "adg_labels.txt"
  adgMapKeyToDesc_vec <- adgMapping.readFromFile(fn_adgMapToDesc)
  
  
  EDC_list <- patientReport.CodeWordToText(
    codesInText = unlist(singlePatientData$edcInList, use.names = F), 
    mapKeyToDesc_df = edcMapKeyToDesc_df
  ); 
  sprintf(EDC_list[[1]])
  
  
  # RxMG part
  monthlyRxMG_list[cntM] <- patientReport.CodeWordToText(
    codesInText = unlist(currPatient$rxmgInList[cntM]), 
    mapKeyToDesc_df = rxmgMapKeyToDesc_df
  ); 
  
  
  
  
  
  
}




preliminaryAnalysisOfOSCNMFAlgorithmConvergence <- function(){
  
  fn_template  = "intermediateResults/afterIteration_%d.Rdata"
  fnIterationsNums <- 2:101
  
  
  meanGsparsity = c()
  meanFsparsity = c()
  topPerc <- c(0.1, 1, 2, 3, 4, 5, 10)/100
  energyAtTop_F <- matrix(NA, nrow = length(topPerc), ncol = length(fnIterationsNums))
  
  
  for(cntI in seq_along(fnIterationsNums)){
    cntF <- fnIterationsNums[cntI]
    currFN <- sprintf(fn_template, cntF)
    load(currFN)
    
    tpmAsVector <- unlist(intermediateResult_list$tempPattMat_list, use.names = F)
    meanFsparsity[length(meanFsparsity) + 1] = mean(tpmAsVector == 0)
    
    tpmAsVector_norm = tpmAsVector/sum(tpmAsVector); 
    tpmAsVector_norm <- sort(tpmAsVector_norm, decreasing = T)
    cumSumTPM <- cumsum(tpmAsVector_norm)
    topPercIndices = ceiling(length(cumSumTPM) * topPerc)
    energyAtTop_F[,cntI] <- cumSumTPM[topPercIndices]
    
    
    allGasVector <- unlist(intermediateResult_list$perPatientCodingVec_list, use.names = F)
    meanGsparsity[length(meanGsparsity) + 1] = mean(allGasVector == 0)
  }
  
  # consider one of the F-s
  load("trainingOnSampled200Diabetics_after100Iterations.Rdata")
  load(file = "auxiliaryFeatureNamesFile.Rdata")
  
  
  for(cntF in seq_along(trainingTrace$tempPattMat_list)){
    currF <- trainingTrace$tempPattMat_list[[cntF]]
    currF_norm <- currF/sum(currF)
    fractionOfEnergyInFeature <- rowSums(currF_norm)
    names(fractionOfEnergyInFeature) <- featureNames
    sortedFractionOfEnergyInFeature <- sort(fractionOfEnergyInFeature, decreasing = T)
    cumulativeEnergyInTop <- cumsum(sortedFractionOfEnergyInFeature)
    numberOfRowsThatContain99percent <- which(cumulativeEnergyInTop >= 0.99)[1]
  }
  
  
  
}



plottingWindowed <- function(){
  
  if(0){
    # cut out the current window
    currW_list <- allViewObject_list
    for(cnt in seq_along(currW_list)){
      currW_list[[cnt]] <- currW_list[[cnt]] + scaleXObj
    }
    currW_list[["nrow"]] = length(currW_list)
    try({
      print(do.call(grid.arrange, currW_list))
    })
  }
  
  if(0){
    x11()
    cntS = 1
    scaleXObj <- scale_x_date(limits = c(stDate_vec[cntS], endDate_vec[cntS]))
    grid.newpage()
    grid.draw(rbind(ggplotGrob(timingObj + scaleXObj), ggplotGrob(pharmVisObj + scaleXObj), size = "first"))
  }
  
  #allViewObject_list <- lapply(allViewObject_list, function(obj){ggplot_gtable(ggplot_build(obj))})
  
  # fetch the visits visualization object
  #timingObj <- (dxClaimTimeObj_list[[currID]] + ggtitle(titleMsg)) #%>% ggplot_build() %>% ggplot_gtable()
  
  # t2d medication visualization
  #pharmVisObj <- pharmedsObj_list[[currID]] #%>% ggplot_build() %>% ggplot_gtable()
  
  
}



lookingForWeirdSecondaryDiabetes <- function(){
  
  imsS1data.CreatePathsToData()
  
  # load dictionary for all codes, ipData, and the mediserv, pharms, and demographics data
  load(fn_codeToDescription)
  load("ipData_ims_s1_threeYears.Rdata" %>% imsS1data.inSlicesDir())
  
  claimsData_list <- rawClaims.DxTraj.LoadThreeYearsOfTraj(isDebugging = FALSE); 
  mediserv_df <- claimsData_list$mediserv
  demogr_df <- claimsData_list$demographics
  
  # perform serialization of the data
  mediservSerial_df <- mediserv_df %>% select(patient_id:icd_cd_5) %>% gather(key = "num", value = "code", icd_cd_1:icd_cd_5) %>% 
    select(patient_id, code) %>% filter(code != "") %>% arrange(patient_id)
  
  # find all entries that correspond to at least one of the patterns
  accompanyingDiseases = c(hemochromatosis = "2750",
                           cysticFibrosis = "2770",
                           chronicPancreatitis = "5771",
                           pcos = "2564",
                           cushings = "2550",
                           pancreaticCancer = "157", 
                           glucagonoma = "1574", glucagonoma2 = "2117",
                           pancreatectomy = "0525", pancreatectomy2 = "0526")
  patterns = c("249", "250",  accompanyingDiseases)
  ij_list <- rawClaims.fastPrefixMatching(codeVec = mediservSerial_df$code, pttrn = patterns)
  
  # find all codes that correspond to a secondary diabetes
  cond_secondary = ij_list$j == 1; 
  secDiabetes <- rep(FALSE, nrow(mediservSerial_df)); secDiabetes[ij_list$i[cond_secondary]] = TRUE
  
  # find all codes that correspond to 250.xx codes
  cond_250 <- ij_list$j == 2; 
  diabetes <- rep(FALSE, nrow(mediservSerial_df)); diabetes[ij_list$i[cond_250]] = TRUE
  
  # accompanying diseases
  cond_acc = ij_list$j > 3; 
  accDiseases <- rep(FALSE, nrow(mediservSerial_df)); accDiseases[ij_list$i[cond_acc]] = TRUE
  
  # augment with these fields
  mediservSerial_df <- mediservSerial_df %>% mutate(diabetes = diabetes, secDiabetes = secDiabetes, accDiseases = accDiseases)
  perPatientStats_df <- mediservSerial_df %>% group_by(patient_id) %>% summarise(diabetes = any(diabetes), secDiabetes = any(secDiabetes), accDiseases = any(accDiseases))
  
  
  # now to some statistics
  printStats <- function(mediservSerial_df, perPatientStats_df){
    cat("Total number of patients is", nrow(perPatientStats_df), "\n")
    cat("Total number of claims is", nrow(mediservSerial_df), "\n")
    cat("Total number of patients with at least one 250.xx claim is", sum(perPatientStats_df$diabetes == TRUE), "\n")
    cat("Total number of patients with at least one 249.xx claim is", sum(perPatientStats_df$secDiabetes == TRUE), "\n")
    cat("Total number of patients with at least one 249.xx or 250.xx claim is", sum(perPatientStats_df$secDiabetes == TRUE | perPatientStats_df$diabetes == TRUE), "\n")
    cat("Total number of patient with at least one code for accompanying diagnoses is", sum(perPatientStats_df$accDiseases == TRUE), "\n")
    cat("Total number of patient with at least one code for accompanying diagnoses and at least one 249.xx code is", 
        sum(perPatientStats_df$accDiseases == TRUE & perPatientStats_df$secDiabetes == TRUE), "\n")
  }
  printStats(mediservSerial_df, perPatientStats_df)
}





quora <- function(){
  
  library(dplyr)
  library(purrr)
  
  df <- data_frame(str = c("I love my dad", "I love my mom"))
  df$str %>% as.list() %>% lapply(FUN = strsplit, split = " ") %>% unlist(use.names = FALSE) %>% unique() %>% paste0(collapse = " ")
  
  
}


test.seeIfNewSas7datFilesCanBeReadToR <- function(){
  
  library(sas7bdat)
  imsS1data.CreatePathsToData()
  fn1 <- "E:/CPHITdata/CancerPatients/NewIMS/plans1_enroll_2010.sas7bdat"
  
  df <- read.sas7bdat(fn1); 
  
  
  fn2 <- "E:/CPHITdata/CancerPatients/NewIMS/plans1_claims_mediserv_2011.sas7bdat"
  t_st <- Sys.time()
  claims <- read.sas7bdat(fn2); 
  t_end <- Sys.time()
  
  
  
  
}

# this procedure identifies the year of the current slice
# getYearOfMediservData <- function(mediserv_df){
#   numRowsToSubsample <- min(c(1000, nrow(mediserv_df)))
#   year = mediserv_df$from_dt[1:numRowsToSubsample] %>% substr(1,4) %>% unique(); 
#   year <- year[str_length(year) == 4]; 
#   if(length(year) != 1){
#     error("getYearOfMediservData: first 1000 rows of mediserv data frame do not contain valid dates")
#   }
#   year
# }
# currYear <- getYearOfMediservData(mediserv_df) %>% as.numeric()


test.ImportMediservClaimsOfNewIMSdata <- function(){
  
  # import data
  years = c("2010", "2011")
  for(cntY in years){
    base_fn <- sprintf("plans1_claims_mediserv_%s", cntY) %>% imsS1data.newIMS()
    in_fn <- sprintf("%s.tsv", base_fn)
    out_fn <- sprintf("%s.Rdata", base_fn)
    mediserv_df <- rawClaims.mediserv.LoadAsStrings(in_fn)
    save(mediserv_df, file = out_fn)
  }
 
  years = c("2010", "2011")
  load(file = "t2dCohort_newIMSdata.Rdata")
  patIDcohort = t2dPatientsOfRightAgeIDs_list; 
  for(cntY in years){
    in_fn <- sprintf("plans1_claims_mediserv_%s.Rdata", cntY) %>% imsS1data.newIMS()
    out_fn <- sprintf("t2d_plans1_claims_mediserv_%s.Rdata", cntY) %>% imsS1data.newIMS()
    newIMSdata.preprocessMediservFiles(in_fn, out_fn, patIDcohort, cntY)
    cat("Year", cntY, "was processed\n")
  }
  
  
  
  
}





test_density_function <- function(){
  
  df <- data_frame(x = rnorm(1000, 0, 1), y = rnorm(1000,0, 2), z = rnorm(1000, 2, 1.5))
  df.m <- gather(df)
  ggplot(df.m) + geom_freqpoly(aes(x = value, y = ..density.., colour = key))
  
  ggplot(df.m) + geom_density(aes(x = value, colour = key))
  
}





create_visualization_endocrinologist_visits_aligned_by_first_t2dm_hosp <- function(){
  
  # global constants - allow a horizon of a year and set the alignment date to Jan 1st, 2100
  horizon_in_days = 365; 
  uniform_align_date = "2100-01-01" %>% as.Date();
  num_of_time_bins = 4; 
  cut_prior_align_date = 14; 
  
  # which events to use for alignment
  selected_align_dates <- c("hosp", "no_hosp")
  #selected_align_dates <- c("t2dm", "dcsi", "t2dm_and_dcsi", "no_t2dm_or_dcsi_in_discharge", "ed_visit", "no_event")
  #selected_align_dates <- c("t2dm_or_dcsi_in_discharge", "no_t2dm_or_dcsi_in_discharge", "ed_visit", "no_event")
  
  # data shared by all signals
  breaks <- event_time_bin_breaks(horizon_in_days, cut_prior_align_date, uniform_align_date, num_of_time_bins)
  
  # alignment for all signams and subpopulations
  align_data_list <- 
    prepare_alignment_data(horizon_in_days, uniform_align_date, num_of_time_bins, cut_prior_align_dateselected_align_dates, selected_align_dates)
  
  vec_of_event_names <- align_data_list$vec_of_event_names
  list_of_event_dfs <- align_data_list$list_of_event_dfs
  all_alignments_list <- align_data_list$all_alignments_list
  
  # now plot distributions
  cutoff_num_events = 14
  use_color = TRUE
  pdf(file = "experiments_with_visits.pdf", w = 12, h = 8)
  for(cntEvent in seq_along(list_of_event_dfs)){
    create_and_print_event_visualizations(event_df = list_of_event_dfs[[cntEvent]], 
                                          event_name= vec_of_event_names[cntEvent], 
                                          all_alignments_list,
                                          horizon_in_days, uniform_align_date, num_of_time_bins, cut_prior_align_date, 
                                          breaks, use_color)
  }
  dev.off()
  
  
  # now plot distributions
  cutoff_num_events = 14
  use_color = TRUE
  use_density = FALSE
  pdf(file = "timing_of_events.pdf", w = 12, h = 8)
  vec_of_cutoff_num_events <- c(14, 14, 14, 14, 14, 14, 14, 14)
  for(cntEvent in seq_along(list_of_event_dfs)){
    create_and_print_event_timing_distributions(event_df = list_of_event_dfs[[cntEvent]], 
                                                event_name = vec_of_event_names[cntEvent], 
                                                all_alignments_list,
                                                horizon_in_days, uniform_align_date, num_of_time_bins, cut_prior_align_date, 
                                                breaks, cutoff_num_events = vec_of_cutoff_num_events[cntEvent], 
                                                use_color, 
                                                use_density = use_density)
  }
  dev.off()
  
  # divide the population into training and testing sets
  dev_valid_test_in_all_alignments <- lapply(all_alignments_list, function(df){dev_valid_test(df$patient_id)}) %>% transpose() %>% lapply(FUN = unlist, use.names = FALSE) %>%
    lapply(FUN = sort)
  train_test_patient_id <- list(train = c(dev_valid_test_in_all_alignments$dev, dev_valid_test_in_all_alignments$valid), test = dev_valid_test_in_all_alignments$test)
  train_patient_id <- train_test_patient_id$train
  
  # fit model to the data
  uniq_values_vec = 0:4
  list_of_uniq_values <- rep(list(uniq_values_vec), times = length(list_of_event_dfs))
  vec_of_cutoff_num_events <- rep(14, times = length(list_of_event_dfs))
  pdf(file = "event_timing_iid_model.pdf", w = 12, h = 8)
  fitted_models_list <- list()
  for(cntEvent in seq_along(list_of_event_dfs)){
    event_name = vec_of_event_names[cntEvent]
    fitted_models_list[[event_name]] <- 
      fit_iid_binned_model_to_event(train_patient_id = train_patient_id, plot = TRUE, 
                                    uniq_vals = list_of_uniq_values[[cntEvent]],
                                    event_df = list_of_event_dfs[[cntEvent]], 
                                    event_name = event_name, 
                                    all_alignments_list,
                                    horizon_in_days, uniform_align_date, cut_prior_align_date, 
                                    breaks)
  }
  dev.off()
  
  
  # 
  test_patient_id = train_test_patient_id$test
  llr_list <- list()
  for(cntEvent in seq_along(list_of_event_dfs)){
    event_name = vec_of_event_names[cntEvent]
    llr_list[[event_name]] <- 
      calculate_llr_iid_binned_model(test_patient_id = test_patient_id, 
                                     event_df = list_of_event_dfs[[cntEvent]], 
                                     all_alignments_list = all_alignments_list, 
                                     model = fitted_models_list[[event_name]],
                                     horizon_in_days, uniform_align_date, cut_prior_align_date, 
                                     breaks)
  }
  
  # predictive accuracy of each signal separately 
  auc_separate <- rep(NA, length(list_of_event_dfs))
  for(cntEvent in seq_along(list_of_event_dfs)){
    event_name = vec_of_event_names[cntEvent]
    predOut <- llr_list[[event_name]]$llr
    outcome <- ifelse(llr_list[[event_name]]$align_nm == "hosp", 1, 0)
    pred_obj <- prediction( predOut, outcome)
    perf <- performance(pred_obj,"auc")
    auc_separate[cntEvent] = perf@y.values[[1]]
  }
  cumPredOut <- rep(0, length(llr_list[[1]]$llr))
  for(cntEvent in seq_along(list_of_event_dfs)){
    event_name = vec_of_event_names[cntEvent]
    cumPredOut = cumPredOut + llr_list[[event_name]]$llr
  }
  outcome <- ifelse(llr_list[[cntEvent]]$align_nm == "hosp", 1, 0)
  pred_obj <- prediction( cumPredOut, outcome)
  perf <- performance(pred_obj,"auc")
  auc_cumulative = perf@y.values[[1]]
}


gather_statistics_on_frequency_og_EDCs <- function(){
  load("t2dICD9codesOnlyForAllPatients.Rdata")
  t2dICDconcat_df <- t2dICDconcat_df %>% as_data_frame()
}



compare_bayes_factors_calculated_by_different_versions_of_code <- function(){
  debugging_flag = 0
  if(debugging_flag){
    llr_list_old <- list()
    for(cntEvent in seq_along(list_of_event_dfs)){
      event_name = vec_of_event_names[cntEvent]
      llr_list_old[[event_name]] <- 
        calculate_llr_iid_binned_model_old(test_patient_id = test_patient_id, 
                                           event_df = list_of_event_dfs[[cntEvent]], 
                                           all_alignments_list = all_alignments_list, 
                                           model = fitted_models_list[[event_name]],
                                           horizon_in_days, uniform_align_date, cut_prior_align_date, 
                                           breaks)
    }
    
    cumPredOut <- rep(0, length(llr_list_old[[1]]$llr))
    cum_pred_mat <- matrix(0, nrow = length(llr_list_old[[1]]$patient_id), ncol = length(list_of_event_dfs))
    patient_id_df <- data_frame(patient_id = llr_list_old[[1]]$patient_id)
    #cum_pred_mat[,1] = llr_list[[1]]$patient_id
    for(cntEvent in seq_along(list_of_event_dfs)){
      event_name = vec_of_event_names[cntEvent]
      tmp_df <- patient_id_df %>% left_join(llr_list_old[[event_name]] %>% select(patient_id, llr), by = "patient_id")
      cumPredOut = cumPredOut + tmp_df$llr
      cum_pred_mat[,cntEvent] = tmp_df$llr
    }
    
    llr_old_df <- cum_pred_mat %>% as.data.frame() %>% as_data_frame()
    llr_old_df <- cbind(data_frame(patient_id = llr_list_old[[1]]$patient_id), llr_old_df) %>% as_data_frame()
    
    
    cum_pred_mat2 <- matrix(0, nrow = length(llr_list_old[[1]]$patient_id), ncol = 2*length(list_of_event_dfs))
    patient_id_df <- data_frame(patient_id = llr_list_old[[1]]$patient_id)
    for(cntEvent in seq_along(list_of_event_dfs)){
      event_name = vec_of_event_names[cntEvent]
      tmp_df <- patient_id_df %>% left_join(llr_list_old[[event_name]] %>% select(patient_id, llr_num, llr_timing), by = "patient_id")
      cum_pred_mat2[,2*cntEvent-1] = tmp_df$llr_num
      cum_pred_mat2[,2*cntEvent] = tmp_df$llr_timing
    }
    
    llr2_df <- cum_pred_mat2 %>% as.data.frame() %>% as_data_frame()
    llr2_df <- cbind(data_frame(patient_id = llr_list_old[[1]]$patient_id), llr2_df) %>% as_data_frame()
    
    
    # check if the new and the old data produces the same distributions
    for(cntEvent in seq_along(list_of_event_dfs)){
      df <- data_frame(patient_id = llr_list[[cntEvent]]$patient_id, 
                       llr_new = rowSums(llr_list[[cntEvent]] %>% select(-patient_id) %>% select(-align_nm))) %>% 
        left_join(llr_list_old[[cntEvent]] %>% select(patient_id, llr), by = "patient_id")
      expect_equivalent(df$llr_new, df$llr)
    }
  }
}



another_debugging_function <- function(){
  if(debugging_flag){
    
    cols <- grep("Num", names(joined_llr_df))
    selected_col_names <- c("patient_id", names(joined_llr_df)[cols])
    
    dataMat_ACG_LLRnum <- cbind(data_mat_list[[cntM]][cond_t2dm,], alignFeatureMatrix(joined_llr_df[,selected_col_names], patient_id))
    acg_llrnum_fit_list <- trainAndTestModel(dataMat_ACG_LLRnum, "") 
    auc_results_mat[cntRep, 3 + cntM + 2] = acg_counts_llr_fit_list$auc_test
    
    aa <- alignFeatureMatrix(llr_old_df, patient_id) %>% as.data.frame() %>% as_data_frame();  aa <- cbind(patient_id, aa) %>% as_data_frame()
    cc <- alignFeatureMatrix(llr2_df, patient_id) %>% as.data.frame() %>% as_data_frame(); cc <- cbind(patient_id, cc) %>% as_data_frame()
    bb <- alignFeatureMatrix(joined_llr_df %>% select(-align_nm), patient_id) %>% as.data.frame() %>% as_data_frame(); bb <- cbind(patient_id, bb) %>% as_data_frame()
    
    cntEvent = 2
    llr_list_old[[cntEvent]] %>% filter(patient_id == "s103AAAAAAAAEXSV")
    llr_list[[cntEvent]] %>% filter(patient_id == "s103AAAAAAAAEXSV")
    
    cntEvent = 2
    dd <- data_frame(patient_id = patient_id) %>% left_join(llr_list_old[[cntEvent]] %>% select(-align_nm), by = c("patient_id")) %>% left_join(llr_list[[cntEvent]], by = "patient_id") 
    
    dataMat6 = cbind(data_mat_list[[cntM]][cond_t2dm,], alignFeatureMatrix(llr_old_df, patient_id))
    trainAndTestModel(dataMat6, data_mat_names[cntM])$auc_test
    
    dataMat7 = cbind(data_mat_list[[cntM]][cond_t2dm,], alignFeatureMatrix(llr2_df, patient_id))
    trainAndTestModel(dataMat7, data_mat_names[cntM])$auc_test
    
  }
}


create_icd_to_adg_mapping <- function(){
  library(dplyr)
  library(readr)
  load("allCodesToDescriptions.Rdata")
  icd_to_adg_df <- read_csv("icd_to_adg.csv")
  icd_to_adg_df <- icd_to_adg_df %>% select(ICD, code_set, ADG) %>% filter(code_set == "9") %>% rename(key = ICD, desc = ADG) %>% select(-code_set)
  acgLoader.codeToDescription$icd_to_adg = icd_to_adg_df
  
  face_to_face_cpt_df <- read_csv("face2face_cpt_codes.csv")
  acgLoader.codeToDescription$face_to_face_cpt = face_to_face_cpt_df
  save(acgLoader.codeToDescription, file = "allCodesToDescriptions.Rdata")
  
  
}



# testing how to define logistic regression against a continuous outcome
test_log_res_against_continuous_outcome <- function(){
  # dummy input - only first column matters
  X_mat <- data_frame(x1 = c(1,2,3,4,5), 
                      x2 = c(1,5,2,4,0)) %>% as.matrix()
  
  # output as a vector and a matrix of counts
  y_vec = (X_mat[,1]-1)/4
  y_mat = data_frame(class0 = 1-y_vec, 
                     class1 = y_vec) %>% as.matrix()
  y_mat = round(y_mat*10000)
  
  # training and testing a model against the vector
  model <- cv.glmnet(x = X_mat, y = y_mat, family = "binomial", alpha = 1); 
  perf <- predict(model, X_mat, s = model$lambda[which.min(model$cvlo)], type = "response")
  
}



testing_lubridate <- function(){
  
  library(microbenchmark)
  library(lubridate)
  
  microbenchmark(times = 100, 
                 as.Date("2011-05-07"), as_date("2011-05-07"))
  
  
  
  
}

