##############################################################
##                   TGen Robot Program                     ##
##                                                          ##
##                      Jason Clark                         ##
##                   jaclark2@alaska.edu                    ##
##                      2022-02-11                          ##
##############################################################

# Creating a function and storing into robotProgram
robotProgram<-function(sourceFile, 
                       firstSourceWell=character(),
                       firstDestinWell=character(),
                       destinationPlate_prefix=character(),
                       destinationPlate_first=numeric(),
                       outDir,
                       programDate=paste0(unlist(strsplit(as.character(Sys.Date()),split='-')),collapse = '') ){ # uses system date;
  # can change programDate if needed when you call the function to whatever you want it to be
  
  # libraries
  if(!require(tidyverse)){
    install.packages('tidyverse')
  }
  
  # constants
  roboVolume<-50
  roboTool<-'TS_50'
  
  # resources # there has to be a resource folder adjacent to the script or in the working dir for it to work
  sourceWells<-'./resources/Robot_source_well_template.csv'
  destinWells<-'./resources/Robot_destination_well_template.csv'
  
  #create output dir
  if(!dir.exists(paste0(outDir,programDate,'/'))){
    dir.create(paste0(outDir,programDate,'/'))
  }
  
  # Import well templates
  source_wells <- read.csv(sourceWells, header = TRUE)
  destin_wells <- read.csv(destinWells, header = TRUE)
  
  # Import .tsv
  tsv <- (read.table(file = sourceFile, sep = '\t', header = TRUE))
  
  # filter records
  ntc <- tsv %>%
    filter(Sample == "NEC")
  
  cq <- tsv %>%
    filter(Cq != "NaN")
  
  good <- rbind(ntc, cq)
  
  good <- arrange(good, File, Well)
  
  # Parse out second number in File column entries --> Source plate
  # The columns in the source file need to stay in identical order; also, for "File" column, format needs to be e.g., 20211112_980102_PostDnase_AW.rdml
  good$SourcePlate <- matrix( 
    unlist(
      strsplit(
        good$File,split='_'
      )
    ),
    ncol=4, byrow = TRUE)[,2]
  
  # number source plates sequentially for key
  nSourcePlates<-length(unique(good$SourcePlate))
  SourcePlates<-data.frame(SourceRack=1:nSourcePlates,SourcePlate=unique(good$SourcePlate))
  good<-merge(good,SourcePlates,all.x=TRUE)
  # might be worth sorting good by rack, well...just to make sure it is in order.
  
  print(paste('# of source plates: ', nSourcePlates))
  
  # assign wells to goodRemaining  # avoiding overwriting good dataframe
  goodRemaining<-good

  # initializing some new variables that I will use
  progCounter=0 # this is the number of the current program
  firstDestinPlate<-1 # going to start putting things in plate 1
  #make up some destin plates, more than we need, but they wont get used
  destinationPlates<-paste0(destinationPlate_prefix,seq(destinationPlate_first,destinationPlate_first+100,1))
  destinationPlatesRemaining<-destinationPlates
  print(paste('# of Dest. plates: ', length(destinationPlates)))
  # initializing output key and instructions so we can fill them with data in the programming loop
  outputKey<-data.frame(Program=numeric(),Type=character(),Rack=integer(),PlateID=character())
  instructions<-character()
  
  programOutput <- data.frame()
  
  while(nrow(drop_na(goodRemaining))>0){ # will keep looping as long as condition is true; when Source wells (e.g., no rows remain in goodRemaining) run out, loop ends
    # iterate programs
    progCounter<-progCounter+1
    print(paste('Program:',progCounter))
    instructions<-c(instructions,paste('Program:',progCounter))
    # Subset destination to those available
    # this assumes only first plate could be partial
    firstWellint <- destin_wells[destin_wells$Rack==1&destin_wells$Destination==firstDestinWell,] %>% row.names() %>% as.integer()
    destin_avail <- destin_wells[firstWellint:nrow(destin_wells),]
    # nwells with first plate possibly partial
    nDest<-nrow(destin_avail)
    print(paste('First destination plate is:',ifelse(nrow(destin_wells)==nDest,'empty','partial')))
    print(paste('Starting first destination plate at well:',firstDestinWell))
    instructions<-c(instructions,paste('First destination plate is:',ifelse(nrow(destin_wells)==nDest,'empty','partial')))
    instructions<-c(instructions,paste('Starting first destination plate at well:',firstDestinWell))
    # Subset source to those available
    # this assumes only first plate could be partial
    firstSourceWellInt <- source_wells[source_wells$Rack==1&source_wells$Source==firstSourceWell,] %>% row.names() %>% as.integer()
    source_avail <- source_wells[firstSourceWellInt:nrow(source_wells),]
    # nwells with first plate possibly partial
    nSour<-nrow(source_avail)
    print(paste('First source plate is:',ifelse(nrow(source_wells)==nSour,'empty','partial')))
    print(paste('Starting first source plate at well:',firstSourceWell))
    instructions<-c(instructions,paste('First source plate is:',ifelse(nrow(source_wells)==nSour,'empty','partial')),
                    paste('Starting first source plate at well:',firstSourceWell))
    
    # Count source wells, compare to destination wells, decide how many programs are needed, calculate last well used  
    # pick 6 source plates for this program, if fewer than 6, pick remaining
    remainingSourcePlates<-sort(unique(goodRemaining$SourceRack))
    nRemainingSourcePlates<-length(remainingSourcePlates)
    theseSourcePlates<-remainingSourcePlates[1:ifelse(nRemainingSourcePlates>=6,6,nRemainingSourcePlates)]
    sixGood<-goodRemaining[goodRemaining$SourceRack %in% theseSourcePlates,]
    (nThese<-nrow(sixGood))
    nSour;nThese;nDest
    # make a program
    if(nThese>nDest){
      print('overfull destination program, remainder passed to next program')
      instructions<-c(instructions,'all destination plates filled')
      
      # Use only nDest wells from nThese
      # calculate last source well as an integer
      lastSource<-source_avail[nDest,]
      lastSourceWellInt<-as.integer(rownames(source_avail)[nDest])
      # use this for updating goodRemaining
      
      # calculate last dest well
      lastDestin<-destin_avail[nDest,]
      lastDestinWellInt<-as.integer(rownames(destin_avail)[nDest])
      # this may not be needed, as destin should be full
      
      # update
      theseGood<-sixGood[1:nDest,] # which wells will be chosen for this program, e.g., 1-# of destination wells available
      
      # check if it's the last program, define goodNext as remaining wells in goodRemaining
      if((nDest+1)>nrow(goodRemaining)){
        goodNext<-goodRemaining[(nDest+1),]
      }else{
        goodNext<-goodRemaining[(nDest+1):nrow(goodRemaining),]
      }
      nextSourceWell<-source_avail[nDest+1,]$Source
      nextDestinWell<-'A01'
      nextSourcePlate<-goodNext$SourcePlate[1]
    }
    if(nThese<=nDest){
      print('partial/perfect destination program')
      
      # Use all nThese
      # calculate last source well
      lastSource<-source_avail[nThese,]
      lastSourceWellInt<-as.integer(rownames(source_avail)[nThese])
      # use this for updating goodRemaining
      # calculate last dest well
      lastDestin<-destin_avail[nThese,]
      lastDestinWellInt<-as.integer(rownames(destin_avail)[nThese])
      # this may not be needed, as destin should be full
      # update
      # NAs if none available
      theseGood<-sixGood
      # check if done, fill accordingly
      if((nThese+1)>nrow(goodRemaining)){
        goodNext<-goodRemaining[(nThese+1),]
      }else{
        goodNext<-goodRemaining[(nThese+1):nrow(goodRemaining),]
      }
      nextSourceWell<-goodNext[1,]$Well
      nextSourcePlate<-goodNext$SourcePlate[1]
      nextDestinWell<-ifelse(lastDestin$Destination=='H10','A01',destin_avail[nThese+1,]$Destination)
      instructions<-c(instructions,ifelse(lastDestin$Destination=='H10','last destination plate full','last destination plate partial'))
    }
    
    # calculate plates
    print('calculating plates')
    lastSourcePlate<-lastSource$Rack
    lastDestinPlate<-lastDestin$Rack
    theseDestinPlates<-firstDestinPlate:lastDestinPlate
    theseDestinPlateDF<-data.frame(dRack=theseDestinPlates,destPlate=destinationPlatesRemaining[theseDestinPlates])
    nextDestinPlate<-ifelse(nextDestinWell=='A01',destinationPlatesRemaining[lastDestinPlate+1],destinationPlatesRemaining[lastDestinPlate])
    
    # program
    print('making program')
    programDF<-data.frame(program=integer(),TSVsRack=integer(),SourceWell=character(),dRack=integer(),DestinationWell=character(),Volume=integer(),Tool=character(),
                          sourcePlate=character(),destPlate=character(),File=character(),Sample=character(),Cq=numeric())
    nTheseGood<-nrow(theseGood)
    programDF<-rbind(programDF,
                     data.frame(program=rep(progCounter,nTheseGood),
                                TSVsRack=theseGood$SourceRack,
                                SourceWell=theseGood$Well,
                                dRack=destin_avail$Rack[1:nTheseGood],
                                DestinationWell=destin_avail$Destination[1:nTheseGood],
                                Volume=rep(roboVolume,nTheseGood),
                                Tool=rep(roboTool,nTheseGood),
                                sourcePlate=theseGood$SourcePlate,
                                # destPlate=destin_avail$DestinationPlate[1:nTheseGood],
                                File=theseGood$File,
                                Sample=theseGood$Sample,
                                Cq=theseGood$Cq
                     )
    )
    # add destination plate names
    programDF<-merge(programDF,theseDestinPlateDF,all.x=TRUE)
    # renumber source plates 1:6
    theseSracks<-unique(programDF$TSVsRack)
    programDF<-programDF %>% mutate(sRack=as.integer(as.character(factor(TSVsRack,levels=theseSracks,labels=(1:length(theseSracks))))))
    
    # write outputs
    write.csv(programDF, file = paste0(outDir,programDate,'/Program_infoTable_',programDate,"_",progCounter,'.csv'), row.names = FALSE)
    # above table is for troubleshooting. Columns could be reordered using select to make more sense.
    
    programOutput<-rbind(programOutput,programDF) # store program data for later output
    
    thisProgramFile<-paste0(outDir, programDate,'/Program_',programDate,"_",progCounter,'.csv')
    write.csv(programDF %>% 
                select(sRack,SourceWell,dRack,DestinationWell,Volume,Tool) %>% 
                rename(Source=SourceWell,Destination=DestinationWell) , 
              file = thisProgramFile, row.names = FALSE)
    # manually change col names for robo, can't have duplicate names in dataframes
    tempProg<-readLines( thisProgramFile)
    tempProg[1]<- gsub('sRack','Rack',tempProg[1])
    tempProg[1]<- gsub('dRack','Rack',tempProg[1])
    writeLines(tempProg,con=thisProgramFile)
    print(paste0('Program: ',thisProgramFile, ', written'))
    
    # key
    thisSKey <- programDF %>%
      group_by(sRack) %>% 
      slice(1) %>% 
      mutate(Type='Source') %>%
      select(program,Type,sRack,sourcePlate) %>% 
      rename(Program=program,
             Rack=sRack,
             PlateID=sourcePlate)
    thisDKey <- programDF %>% 
      group_by(dRack) %>% 
      slice(1) %>% 
      mutate(Type='Destination') %>%
      select(program,Type,dRack,destPlate) %>% 
      rename(Program=program,
             Rack=dRack,
             PlateID=destPlate)
    outputKey<-rbind(outputKey,thisSKey,thisDKey)
    
    # update wells for next program (e.g., second, third, fourth, fifth, etc., ad finitum)
    print('updating remaining')
    if(nrow(drop_na(goodNext))!=0){ # checks to see if program is done
      if(goodNext$SourcePlate[1]==programDF$sourcePlate[nrow(programDF)]){
        instructions<-c(instructions,paste('move source plate(s) to next program run:',goodNext$SourcePlate[1]))
      }
    }else{
      instructions<-c(instructions,paste('source plates finished'))
    }
    if(programDF$destPlate[nrow(programDF)]!=destin_wells$Rack[nrow(destin_wells)] & 
       programDF$DestinationWell[nrow(programDF)]!=destin_wells$Destination[nrow(destin_wells)]){
      instructions<-c(instructions,paste('last destination plate is partial:',programDF$destPlate[nrow(programDF)])) # if last destination plate is not finished/full
    }
    if(programDF$destPlate[nrow(programDF)]==destin_wells$Rack[nrow(destin_wells)] & 
       programDF$DestinationWell[nrow(programDF)]==destin_wells$Destination[nrow(destin_wells)]){
      instructions<-c(instructions,paste('last destination plate is full:',programDF$destPlate[nrow(programDF)])) # if last destination plate IS finished/full
    }
    goodRemaining<-goodNext # update goodRemaining with goodNext
    
    # break loop if none remaining
    if(nrow(drop_na(goodRemaining))==0){
      print('none remaining')
      break
    } # if we're out of Source wells then while() loop breaks

    print('updating for next prog')
    firstSourceWell  <- nextSourceWell
    # firstSourcePlate <- nextSourcePlate
    firstDestinWell  <- nextDestinWell
    # firstDestinPlate <- nextDestinPlate
    destinationPlatesRemaining <- destinationPlatesRemaining[which(destinationPlatesRemaining == nextDestinPlate):length(destinationPlatesRemaining)]
    print('next program')
  } # if we're not out of Source wells, goes back to beginning of while() loop
  
  write.csv(programOutput %>%
              select(destPlate, DestinationWell, Sample) %>%
              filter(Sample != 'NEC') %>%
              mutate(Sample = gsub('TG', '', Sample)), file = paste0(outDir,programDate,'/',programDate,'_PIMpoint_upload','.csv'), row.names = FALSE)
  
  write.csv(programOutput %>%
              select(Sample, sourcePlate, destPlate, DestinationWell) %>%
              filter(Sample == 'NEC') %>%
              mutate(destPlate = gsub('160-', '', destPlate)) %>%
              mutate(Concat = paste(Sample, sourcePlate, destPlate, sep = "-")) %>%
              select(Concat, destPlate, DestinationWell), file = paste0(outDir,programDate,'/',programDate,'_Additional_controls','.csv'), row.names = FALSE)
  
  write.csv(outputKey, file = paste0(outDir, programDate, '/Key_', programDate,'.txt'), row.names = FALSE)
  # this might be useful, I mainly wrote it as a check on what the programs were doing
  
  writeLines(instructions,paste0(outDir,programDate,'/Instructions_',programDate,'.txt'))
}