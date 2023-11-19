#
#DEPENDENCIES
#

#RJDBC requires the driver files for each database to function:
#       MS-SQL  : ./sqljdbc4.jar
#       Oracle  : ./ojdbc8.jar
#
#upon installing RJDBC you may need to run "R CMD javareconf" command in the 
#terminal as root to add Java support to R.

#suppressMessages(library(RJDBC, quietly=FALSE))   #JDBC maybe DBI: dbConnect
#suppressMessages(library(dplyr, quietly=FALSE))   #appearantly not used
#suppressMessages(library(getPass, quietly=FALSE)) #getPass
#suppressMessages(library(squash, quietly=FALSE))  #hist2

#Based on notes from donExpandLenButREjWeight.r

#
#ANCILLARY FUCNTIONS
#

#' A function primarily for internal use that returns the number of port complex away a given target port is from a hypothetical borrowed port. 
#' @param tar target port
#' @param bor borrowed port
howFar = function(tar, bor){
        #
        map = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
        #
        def = which( map==tar )
        rep = which( map==bor )
        #
        return( abs(def-rep) )
}
howFar = Vectorize(howFar, 'bor')

## Not currently used but it will be nice to remove the squash dependency
#base2DHist = function(df, xbins, ybins, freak=F){
#        # https://www.r-bloggers.com/2014/09/5-ways-to-do-2d-histograms-in-r/
#        # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
#
#        #
#        freq = as.data.frame(table(findInterval(df[,1], xbins),findInterval(df[,2], ybins)))
#        freq[,1] = as.numeric(freq[,1])
#        freq[,2] = as.numeric(freq[,2])
#
#        #
#        freq2D = matrix(0, length(xbins), length(ybins))
#        freq2D[cbind(freq[,1], freq[,2])] = freq[,3]
#
#        #return proportion
#        if( !freq ){
#                return( freq2D/sum(freq2D) )
#        }
#
#        #
#        return( freq2D )
#
#        ## Normal
#        #image(x.bin, y.bin, freq2D, col=r)
#        #
#        ## Log
#        #image(x.bin, y.bin, log(freq2D), col=r)
#}

#
#DATABASE FUNCTIONS
#

#
#
#length_expand.sql FROM CALCOM
#


#' Collect and prepare Calcom data for length expansion.
#'
#' @param year  The year of the expansion given as a four digit integer. Year may 
#'      be given as a vector to prepare data for a multi-year expansion.
#' @param save  A filename.csv to save data to file or a logical. If save=True 
#'      the filename defalts to sprintf('calcom%sSppData%s', year, Sys.time()); 
#'      save=False (Default) does not save.
#' @param fromFile A filename.csv to read data from file or a logical. 
#'      fromFile=True defaults to the most recent sprintf('pacfin%sData*', year) 
#'      file; fromFile=False (Default) reads data from calcom.
#'
#' @return Returns a list of the various objects from calcom needed to compute 
#'      a length expansions for the given years.
getCalcomLenData = function(year, save=F, fromFile=F){
	#year     : the year of the expansion. 
        #save     : a filename.RData to save data to file or a logical.
        #               True defaults to the filename sprintf('calcom%sData%s', year, Sys.time()); False (Default) does not save.
        #fromFile : a filename.rda to read data from file or a logical.
        #               True defaults to the most recent sprintf('calcom%sData*', year) file; False (Default) reads data from calcom.
        #
        #value    : returns a list of all of the various objects in calcom needed to compute an expansion in the given year.
	
        #error if the given year is before the data would exist (also catches two digit years)
        stopifnot( year>=1975 )
        #year should not be fractional
        stopifnot( floor(year)%in%year )
	
	#pull from calcom 
	#NOTE: Still need to figure out how to recreate the exact results from pacfin only

	#
        if( fromFile!=F ){
		#if fromFile=T make the name the most recent date available
                if( fromFile==T ){
                        years = paste(unique(year), collapse='')
                        #get possible files
                        possibleFiles = list.files(path='.', pattern=glob2rx(sprintf('calcom%sLenData*', years)))
                        stopifnot( length(possibleFiles)>0 )
                        #
                        firstPart = sprintf("calcom%sLenData", years)
                        dates = as.POSIXlt(sub(firstPart, "", possibleFiles))
                        fromFile = sprintf("%s%s.rda", firstPart, max(dates))
                }
                #
                writeLines(sprintf("\nReading CALCOM Data From %s...", fromFile))
                #load the list of calcom data
                #load(fromFile)
                out = readRDS(fromFile)
	#otherwise get from database
        }else{	
		#
                flag = T
                while( flag ){
                        #
                        flag = tryCatch({
				
			# Create microsoft sql connection driver and open connection to CALCOM
			# CALCOM is an MS-SQL server on the PSMFC VPN
			# sqljdbc4.jar file is required for creating the microsoft sql driver
			#mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'")
			mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', system.file("drivers/sqljdbc4.jar", package = "calcomExpansions"), identifier.quote="'")
			# CALCOM connection
			writeLines("\nReading CALCOM Length Data From CALCOM Connection...") 
			mCon = RJDBC::dbConnect(mDrv, 'jdbc:sqlserver://sql2016.psmfc.org\\calcom;databaseName=CALCOM', getPass::getPass('CALCOM User: '), getPass::getPass('Password: '))	
			
			#
			tempLen1 = c()
			tempLen2 = c()
			tempLen3 = c()
			tempLen4 = c()
			for(y in year){
				#year%%100: uses modular arithmatic to get last two digits of the year.
				twoDigitYear = y%%100
				
				#Five (four) Files
				
				#sums over qtr
				#NOTE: depends on having completed sppComp expansion in this given year
				tempLen1 = rbind(tempLen1, RJDBC::dbGetQuery(mCon, sprintf('
				                select 
				                        year, 
				                        species, 
				                        live, 
				                        gear_grp, 
				                        port_complex, 
				                        mark_cat, 
				                        source, 
				                        sum(pounds) as LBS
				                
				                from com_lands
				                
				                where %d=year or %d=year
				                
				                group by 
				                        year, 
				                        species, 
				                        live, 
				                        gear_grp, 
				                        port_complex, 
				                        mark_cat, 
				                        source
				                
				                order by
				                        live, 
				                        species, 
				                        mark_cat, 
				                        port_complex, 
				                        gear_grp
				        ', twoDigitYear, y)
				))
				#tempLen1$LBS = as.numeric(tempLen1$LBS)
				
				#2) select * into temp_len_2 from len_samp_vu where lenct1>9 and @expandyr=yr order by live_fish, species, mark_cat, port_complex, gear_grp
				tempLen2 = rbind(tempLen2, RJDBC::dbGetQuery(mCon, sprintf('select * from len_samp_vu where lenct1>9 and %d=yr order by live_fish, species, mark_cat, port_complex, gear_grp', twoDigitYear)) )
				
				#3) select * into temp_len_3 from len_expand_data_vu where substring(sample_no,3,2)=@expandyr
				tempLen3 = rbind(tempLen3, RJDBC::dbGetQuery(mCon, sprintf('select * from len_expand_data_vu where substring(sample_no,3,2)=%d', twoDigitYear)) )
				
				#4) select sample_no, species, flength, age, sex into temp_len_4 from master_fish where substring(sample_no,3,2)=@expandyr
				tempLen4 = rbind(tempLen4, RJDBC::dbGetQuery(mCon, sprintf('
				                select 
				                        sample_no, 
				                        species, 
				                        flength, 
				                        age, 
				                        sex 
				                
				                from master_fish 
				                
				                where substring(sample_no,3,2)=%d
				        ', twoDigitYear)
				))
				#tempLen4$flength[is.na(tempLen4$flength)] = 0
				#tempLen4$age[is.na(tempLen4$age)] = 0
			}
			#
			tempLen1$LBS = as.numeric(tempLen1$LBS)
			#
			tempLen4$flength[is.na(tempLen4$flength)] = 0
                	tempLen4$age[is.na(tempLen4$age)] = 0
	
			#exit loop when this eventually doesn't error
                	flag = F
			}, error=function(err){
                                #
                                print(err)
                                readline("\nDo you have a PSMFC IP address? Join PSMFC VPN.\n(Ctrl-C to Escape -or- Enter to Try Again)")
                                writeLines('')
                                #
                                flag = T
                        })
                }
	
		#glom everything together for output
        	out = list( 
        	        tempLen1 = tempLen1,
        	        tempLen2 = tempLen2,
			tempLen3 = tempLen3,
			tempLen4 = tempLen4
			#gearCodes = gearCodes,
        	        #portCodes = portCodes,
        	        #nmSpCodes = nmSpCodes,
        	        #mcat_list = mcat_list,
        	        #species_codes = species_codes,
        	        #market_categories = market_categories
        	)

        	#
        	if( save!=F ){
        	        years = paste(unique(year), collapse='')
        	        #if save=T make the name the date
        	        if( save==T ){ save=sprintf('calcom%sLenData%s.rda', years, as.POSIXlt(Sys.time())) }
        	        #write the data to file.
        	        saveRDS(out, file=save)
        	}
	}
	
	#
	return( out )
}

#
#EXPANSION FUNCTIONS
#

#' The core expansion function executing an automated length expansion of Don's 
#' Visual Basic code.
#' 
#' @param calcomLenData A list as returned by getCalcomLenData.
#' @param portBorr      A matrix to define the priority of port borrowing. 
#'      Rownames should indicate the port complex code of the actual stratum to 
#'      be filled. The first column contains the first priorty for borrowing 
#'      from, second column contains the second priority, ... etc. Elements 
#'      should be port complex codes as they appear in the samples. Elements not 
#'      given as appearing in the samples will code for Nominal.
#' @param files         A boolean flag to produce verbose error files and/or 
#'      expansion output files such as sppdoc.
#'
#' @return a data.frame reminiscent of the calcom ___ table. 
estLenComp = function(calcomLenData, portBorr=portMatrix1, files=T){ 
	#calcomLenData : a list as returned by getCalcomLenData
        #portBorr   : a matrix to define the priority of port borrowing. 
        #               rownames should indicate the port complex code of the actual stratum to be filled.
        #               the first column contains the first priorty for borrowing from, second column contains the second priority, ... etc. 
        #               elements should be port complex codes as they appear in the samples.
        #               elements not given as appearing in the samples will code for Nominal.
        #files      : a boolean flag to produce verbose error files and/or expansion output files such as exdoc
        #
        #value      : 
		
	#SOME LOCAL CONSTANTS
	
	#
	ctThresh = 0
	totThresh = 2204/4 #1/4 of a metric ton
	#
	notSpp = c(
	        'ALBC', 'URCK', 'RCK8', 'THDS', 'MISC', 'SCLP', 'OCTO', 'OCRB',
	        'SPRW', 'SKAT', 'SHRK', 'PWHT', 'GRDR', 'PRCH', 'UFLT', 'RPRW',
	        'UDAB', 'OFLT', 'UINV', 'PMCK', 'RCK3', 'BONI', 'DCRB', 'GROU',
	        'JMCK', 'MSQD', 'OCRK', 'RCK6', 'RCRB', 'SRMP', 'UMCK', 'RCK4'
	)
	##NOTE: there seems to be a special rule for these gears (not TWL) no port to borrow from north or south AND one of these gears then IGNORE 
	##I think basically we only want to borrow in TWL gear  
	#notGear = c('HKL', 'FPT', 'NET', 'MDT')
	notGear = c('UNK', 'MSC', 'OTH')
	yesSource = c("ACTUAL", "NOMINAL", "BORROW")
	
	#DEAL W/ YEAR

        #
        sppExpYear = as.numeric(unique(calcomLenData$tempLen1$year))
        fishYear = as.numeric(unique(substr(calcomLenData$tempLen3$sample_no, 1, 4)))
        #make sure species expansion years match the fish level years 
        stopifnot( sppExpYear %in% fishYear )
	
	#
	expOut = c()
	for(year in fishYear){
		##NOTE:maybe year loop here?
		#year = fishYear[1]
		#
		twoDigitYear = year%%100
		#
		tempLen1Y = calcomLenData$tempLen1[calcomLenData$tempLen1$year==year,]
		tempLen2Y = calcomLenData$tempLen2[calcomLenData$tempLen2$YR==twoDigitYear,]
		tempLen3Y = calcomLenData$tempLen3[substr(calcomLenData$tempLen3$sample_no, 1, 4)==year,]
		tempLen4Y = calcomLenData$tempLen4[substr(calcomLenData$tempLen4$sample_no, 1, 4)==year,]
		
		#UNPACK CALCOM SPPCOMP DATA

		#remove some gears
		landstrat1 = tempLen1Y[!(tempLen1Y$gear_grp%in%notGear),]
		#don only keep sources that end in "AL" or "OW"; I'm more explicit here
		landstrat1 = landstrat1[landstrat1$source%in%yesSource,]
		colnames(landstrat1) = c('year', 'species', 'live', 'gear', 'port', 'mcat', 'source', 'lbs')

		#
		lenct1 = tempLen2Y
		colnames(lenct1) = c('yr', 'species', 'live', 'gear', 'port', 'mcat', 'lenct')
		
		#
		lenstrat2 = merge(landstrat1[,-1], lenct1[,-1], all.x=T)
		lenstrat2$lenct[is.na(lenstrat2$lenct)] = 0
		#remove rows where the sample count is below 10 where the lbs are also below 10
		lenstrat2 = lenstrat2[!(lenstrat2$lenct<10 & lenstrat2$lbs<10),]	

		#UNPACK LENGTH DATA

		#for every stratum, age, length, and sex
		#identify totLbs, alsLbs, totCt, #NOTE:alsCt, expLands 
		
		#sample statistics of the raw fish
		samphead = tempLen3Y
		samphead = samphead[samphead$mark_cat!=0,]
		colnames(samphead) = c('species', 'gear', 'port', 'mcat', 'totLbs', 'sample_no', 'alsLbs', 'live')

		#raw fish level
		sampfish = tempLen4Y
		sampfish$flength = sampfish$flength/10 

		#
		#NOTE: confirm that we don't want to include 'sex' here VVVVVVVVVVVVVVVV
		totCt = aggregate(sampfish$sample_no, by=sampfish[,c('sample_no', 'species')], FUN=length)
		colnames(totCt) = c('sample_no', 'species', 'totCt')
		samphead = merge(samphead, totCt)
		#
		#average weight
		samphead$avgW = samphead$alsLbs/samphead$totCt
		#average numbers
		samphead$avgN = samphead$totLbs/samphead$avgW

		#LENUSE/LENDOC CODE
		
		#
		nc = sum(!lenstrat2$species%in%notSpp)
		lenuse = data.frame(lenstrat2[!lenstrat2$species%in%notSpp,], borrSpecies=character(nc), borrLive=character(nc), borrGear=character(nc), borrPort=character(nc), borrMcat=character(nc), lenSource=character(nc))
		#actuals
		aWho = lenuse$lenct>ctThresh
		actuals = cbind(lenuse[aWho, c('lenct', 'species', 'live', 'gear', 'port', 'mcat')], rep('A', sum(aWho)))
		lenuse[aWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = actuals[,-1]
		#ignores
		iWho = (lenuse$lenct<=ctThresh & lenuse$lbs<totThresh) | lenuse$gear=='MSC' | lenuse$gear=='OTH'
		lenuse[iWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = rep('I', 6)

		#borrow
		bWho = lenuse$lenSource==''
		lenuse[bWho, 'lenSource'] = 'B'
		for(i in which(bWho)){
		        #
		        sam = actuals[
		                actuals$live==lenuse$live[i]            &
		                actuals$gear==lenuse$gear[i]            &
		                actuals$mcat==lenuse$mcat[i]            &
		                actuals$species==lenuse$species[i]
		        ,]
		        #if no available samples found in adjacent strata, then the strata is nominal
		        if( nrow(sam)==0 ){ lenuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = rep('I', 6); next }
		
		        #
		        borr = rep("I", 6)
		        #port priority for the given missing strata
		        portPriority = portBorr[lenuse$port[i],]
		        fars = unlist(howFar(lenuse$port[i], portPriority))
		        for(d in sort(unique(fars))){
		                # a version of portPriority that is at the radius d
		                pp = portPriority[names(fars)[fars==d]]
		                # a version of sam that is at the radius d
		                ss = sam[sam$port%in%pp,]
		
		                #if no samples at d move to a larger d
		                if( length(ss$lenct)==0 ){ next }
		
		                #borrow from the smallest d with the most samples
		                rowSource = LETTERS[d+1]
		                borr = cbind(ss[ss$lenct==max(ss$lenct), c('species', 'live', 'gear', 'port', 'mcat')], rowSource)
		                #if you've made it this far there borr has the strata with the most samples at the smallest d 
		                break
		        }
		        #
		        lenuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = borr
		}
		colnames(lenuse) = c('species', 'live', 'gear', 'port', 'mcat',  'source', 'expLands', 'sppStratSamCt', 'borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'alsSource')
		#
		lendoc = lenuse[,c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'alsSource', 'alsSource', 'borrGear', 'borrPort', 'sppStratSamCt')]
		colnames(lendoc) = c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'expanded', 'borrowed', 'borrGear', 'borrPort', 'sppStratSamCt')
		lendoc = cbind(year, lendoc)
		#
		lendoc$expanded[lendoc$expanded!="I"] = "Y"
		lendoc$expanded[lendoc$expanded=="I"] = "N"
		#NOTE: this is what don does
		numSource = sapply(lendoc$borrowed, function(l){which(LETTERS==l)})
		lendoc$borrowed = "N"
		lendoc$borrowed[numSource>1 & numSource<9] = "Y"
		lendoc[lendoc$borrowed!="Y", c("borrGear", "borrPort")] = ""
		lendocDon = lendoc
		#NOTE: in sppComp we only include expanded strata, but we include all expanded strata sources (below I do that)
		lendoc = lendoc[lendoc$expanded=="Y",]
		isBorr = lendoc$borrowed=="Y"
		#copy over stuff where no borrowing can happen
		lendoc$yearUse = lendoc$year
		lendoc$sppUse  = lendoc$species
		lendoc$dispUse = lendoc$live
		lendoc$mcatUse = lendoc$mcat
		#
		lendoc$borrGear[!isBorr] = lendoc$gear[!isBorr]
		lendoc$borrPort[!isBorr] = lendoc$port[!isBorr]
		#
		for(i in which(isBorr)){	
			lendoc$sppStratSamCt[i] = max(lendoc[
        	       		lendoc$year==lendoc$yearUse[i]   &
				lendoc$species==lendoc$sppUse[i] &
        	               	lendoc$live==lendoc$dispUse[i]   &
        	               	lendoc$mcat==lendoc$mcatUse[i]   &
        	               	lendoc$gear==lendoc$borrGear[i]  &
        	               	lendoc$port==lendoc$borrPort[i]
        	       ,'sppStratSamCt'])
		}	
		#remove the unneccessary columns, reorder, and then rename
		lendoc = lendoc[,c('year', 'species', 'live', 'mcat', 'gear', 'port', 'yearUse', 'sppUse', 'dispUse', 'mcatUse', 'borrGear', 'borrPort', 'sppStratSamCt')]
		colnames(lendoc) = c('yearAct', 'sppAct', 'dispAct', 'mcatAct', 'gearAct', 'portAct', 'yearUse', 'sppUse', 'dispUse', 'mcatUse', 'gearUse', 'portUse', 'ctUse')

		#
        	if( files ){
			write.csv(lendoc, sprintf("lendoc%s.csv", year), row.names=F, quote=F)
		}
		
		#PREP FOR EXPAND
		
		#species and stratum level; a working object
		samp = lenuse[lenuse$alsSource!='I',] #implements the ignores     
		colnames(samp) = c('actSpecies', 'actLive', 'actGear', 'actPort', 'actMcat',  'source', 'expLands', 'sppStratSamCt', 'species', 'live', 'gear', 'port', 'mcat', 'alsSource')
		#
		sppStratSamLbs = aggregate(samphead$alsLbs, by=samphead[,c('species', 'gear', 'port', 'mcat', 'live')], FUN=sum)
		colnames(sppStratSamLbs) = c('species', 'gear', 'port', 'mcat', 'live', 'sppStratSamLbs')
		samp = merge(samp, sppStratSamLbs)
		#
		#average spp weight in stratum
		samp$sppStratAvgW = samp$sppStratSamLbs/samp$sppStratSamCt
		#landings converted to average numbers
		samp$expNums = samp$expLands/samp$sppStratAvgW
		
		#EXPAND
		
		#iterate over expanded sppStrata
		lencom = data.frame(species=character(0), disp=character(0), gear=character(0), port=character(0), mcat=numeric(0), flength=numeric(0), sex=numeric(0), ex=numeric(0))
		for(i in (1:nrow(samp))){
		        #samp[i,'expNums'] will be zero in strata that require borrows, should use the the borrowed expNum
		        sampB = samp[
		                samp$actGear==samp[i,'gear'] &
		                samp$actPort==samp[i,'port'] &
		                samp$actMcat==samp[i,'mcat'] &
		                samp$actLive==samp[i,'live'] &
		                samp$actSpecies==samp[i,'species']
		        ,]
		        #get the sample stats for the sppStrata
		        stats = samphead[
		                samphead$gear==samp[i,'gear'] &
		                samphead$port==samp[i,'port'] &
		                samphead$mcat==samp[i,'mcat'] &
		                samphead$live==samp[i,'live'] &
		                samphead$species==samp[i,'species']
		        ,] 
			
		        #
		        z = 0
		        for(sid in stats$sample_no){
		                #get the fish for the sppStrat
		                fish = sampfish[
		                        sampfish$sample_no==sid &
		                        sampfish$species==samp[i,'species']
		                ,]
				#avoids warning if null
				if(length(fish$flength[fish$flength>0])==0 | length(fish$sex[fish$flength>0])==0){ next }
		                #
				h = squash::hist2(fish$flength[fish$flength>0], fish$sex[fish$flength>0], xbreaks=0:151, ybreaks=c(1,2,3,9), plot=F)
				hz = h$z
		                hz[is.na(hz)] = 0
		                #weight by landings
		                z = z + hz*stats[stats$sample_no==sid, 'totLbs'] 
		        }
		        #
		        grid = expand.grid(h$x[-length(h$x)], h$y[-length(h$y)])
			
		        #The term (hz*stats[stats$sample_no==sid, 'totLbs']) above weights by landings and the term (stats$ageCt/stats$totCt), as Don uses, is not needed here.
		        histLS = z/sum(z) * sampB$expNums 
		        histX = matrix(grid[,1], ncol=3)
		        histY = matrix(grid[,2], ncol=3)
		
		        #
		        out = cbind(species=samp[i,'actSpecies'], year=year, disp=samp[i,'actLive'], gear=samp[i,'actGear'], port=samp[i,'actPort'], mcat=samp[i,'actMcat'], flength=histX[!is.na(histLS)], sex=histY[!is.na(histLS)], ex=histLS[!is.na(histLS)])
		        lencom = rbind(lencom, out)
		}
		lencom$flength = as.numeric(lencom$flength)
		lencom$mcat = as.numeric(lencom$mcat)
		lencom$x = as.numeric(lencom$ex)
		lencom = subset(lencom, select = c(-ex))
		lencom$sex[lencom$sex==3] = 9
		colnames(lencom) = c("species", "year", "disp", "gear", "port", "mcat", "flength", "sex", "binHeight")
		#reorder?
		#lencom = lencom[,c('year','disp','mcat','gear','port','source','spp','lands','comp')]
		expOut = rbind(expOut, lencom)
	}
	
	#
	return(expOut)
}

#' A version of the core expansion function executing an automated length expansion 
#' based on the borrowing dictated by a doc file. 
#' 
#' @param calcomLenData    A list as returned by getCalcomLenData.
#' @param doc           The filename (or vector of filenames) of the doc files 
#'      to use in manual borrowing overrides. Defaults to sprintf("lendoc%s.csv", unique(calcomLenData$tempLen1$year)))
#' @param files         A boolean flag to produce verbose error files and/or expansion output 
#'      files such as sppdoc.
#'
#' @return a data.frame reminiscent of the calcom ___ table.
estLenCompDoc = function(calcomLenData, doc=sprintf("lendoc%s.csv", unique(calcomLenData$tempLen1$year)), files=T){ 
	#calcomLenData : a list as returned by getCalcomLenData
	#exDoc      : a filename (or vector of filenames for multiple years) of a CSV file to define which data are used to expand which strata.
        #               strata which do not appear in the exDoc file are assumed to be nominal.	
	#files      : a boolean flag to produce verbose error files and/or expansion output files such as exdoc
        #
        #value      : 
	
	#SOME LOCAL CONSTANTS
	
	#
	ctThresh = 0
	totThresh = 2204/4 #1/4 of a metric ton
	#
	notSpp = c(
	        'ALBC', 'URCK', 'RCK8', 'THDS', 'MISC', 'SCLP', 'OCTO', 'OCRB',
	        'SPRW', 'SKAT', 'SHRK', 'PWHT', 'GRDR', 'PRCH', 'UFLT', 'RPRW',
	        'UDAB', 'OFLT', 'UINV', 'PMCK', 'RCK3', 'BONI', 'DCRB', 'GROU',
	        'JMCK', 'MSQD', 'OCRK', 'RCK6', 'RCRB', 'SRMP', 'UMCK', 'RCK4'
	)
	##NOTE: there seems to be a special rule for these gears (not TWL) no port to borrow from north or south AND one of these gears then IGNORE 
	##I think basically we only want to borrow in TWL gear  
	#notGear = c('HKL', 'FPT', 'NET', 'MDT')
	notGear = c('UNK', 'MSC', 'OTH')
	yesSource = c("ACTUAL", "NOMINAL", "BORROW")

	#DEAL W/ YEAR
	
	#
        sppExpYear = as.numeric(unique(calcomLenData$tempLen1$year))
        fishYear = as.numeric(unique(substr(calcomLenData$tempLen3$sample_no, 1, 4)))
	#make sure species expansion years match the fish level years 
        stopifnot( sppExpYear %in% fishYear )
	
	#getting all the exDocs and checking year situation
        lenDocs = list()
        for(i in 1:length(fishYear)){
                #
                lenDocY = read.csv(doc[i])
                lenDocYear = unique(lenDocY$yearAct)
		#
                stopifnot( length(lenDocYear)==1 ) #the yealy exdoc files should only contain strata from a single year
                stopifnot( lenDocYear %in% fishYear )
                #
                lenDocs[[lenDocYear]] = lenDocY
        }
	
	#
        expOut = c()
        for(year in fishYear){
                #
                twoDigitYear = year%%100
                #
                tempLen1Y = calcomLenData$tempLen1[calcomLenData$tempLen1$year==year,]
                tempLen2Y = calcomLenData$tempLen2[calcomLenData$tempLen2$YR==twoDigitYear,]
                tempLen3Y = calcomLenData$tempLen3[substr(calcomLenData$tempLen3$sample_no, 1, 4)==year,]
                tempLen4Y = calcomLenData$tempLen4[substr(calcomLenData$tempLen4$sample_no, 1, 4)==year,]
		##NOTE:maybe year loop here?
        	#year = fishYear[1]
		#lenDocY = read.csv(lenDoc[1])
		lenDocY = lenDocs[[year]]
	
		#UNPACK CALCOM SPPCOMP DATA

		#remove some gears
		landstrat1 = tempLen1Y[!(tempLen1Y$gear_grp%in%notGear),]
		#don only keep sources that end in "AL" or "OW"; I'm more explicit here
		landstrat1 = landstrat1[landstrat1$source%in%yesSource,]
		colnames(landstrat1) = c('year', 'species', 'live', 'gear', 'port', 'mcat', 'source', 'lbs')

		#
		lenct1 = tempLen2Y
		colnames(lenct1) = c('yr', 'species', 'live', 'gear', 'port', 'mcat', 'lenct')
		
		#
		lenstrat2 = merge(landstrat1[,-1], lenct1[,-1], all.x=T)
		lenstrat2$lenct[is.na(lenstrat2$lenct)] = 0
		#remove rows where the sample count is below 10 where the lbs are also below 10
		lenstrat2 = lenstrat2[!(lenstrat2$lenct<10 & lenstrat2$lbs<10),]	

		#UNPACK LENGTH DATA

		#for every stratum, age, length, and sex
		#identify totLbs, alsLbs, totCt, #NOTE:alsCt, expLands 
		
		#sample statistics of the raw fish
		samphead = tempLen3Y
		samphead = samphead[samphead$mark_cat!=0,]
		colnames(samphead) = c('species', 'gear', 'port', 'mcat', 'totLbs', 'sample_no', 'alsLbs', 'live')

		#raw fish level
		sampfish = tempLen4Y
		sampfish$flength = sampfish$flength/10 

		#
		#NOTE: confirm that we don't want to include 'sex' here VVVVVVVVVVVVVVVV
		totCt = aggregate(sampfish$sample_no, by=sampfish[,c('sample_no', 'species')], FUN=length)
		colnames(totCt) = c('sample_no', 'species', 'totCt')
		samphead = merge(samphead, totCt)
		#
		#average weight
		samphead$avgW = samphead$alsLbs/samphead$totCt
		#average numbers
		samphead$avgN = samphead$totLbs/samphead$avgW

		#LENUSE/LENDOC CODE
		
		#
		nc = sum(!lenstrat2$species%in%notSpp)
		lenuse = data.frame(lenstrat2[!lenstrat2$species%in%notSpp,], borrSpecies=character(nc), borrLive=character(nc), borrGear=character(nc), borrPort=character(nc), borrMcat=character(nc), lenSource=character(nc))
		#actuals
		aWho = lenuse$lenct>ctThresh
		actuals = cbind(lenuse[aWho, c('lenct', 'species', 'live', 'gear', 'port', 'mcat')], rep('A', sum(aWho)))
		lenuse[aWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = actuals[,-1]
		#ignores
		iWho = (lenuse$lenct<=ctThresh & lenuse$lbs<totThresh) | lenuse$gear=='MSC' | lenuse$gear=='OTH'
		lenuse[iWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = rep('I', 6)

		#borrow
		bWho = lenuse$lenSource==''
		lenuse[bWho, 'lenSource'] = 'B'
		for(i in which(bWho)){
		        #
		        sam = actuals[
		                actuals$live==lenuse$live[i] &
		                actuals$gear==lenuse$gear[i] &
		                actuals$mcat==lenuse$mcat[i] &
		                actuals$species==lenuse$species[i]
		        ,]
		        #if no available samples found in adjacent strata, then the strata is nominal
		        if( nrow(sam)==0 ){ lenuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = rep('I', 6); next } 
			
			#look up borr instead of calculating it
			bor = lenDocY[
				lenDocY$dispAct==lenuse$live[i] &
        	                lenDocY$gearAct==lenuse$gear[i] &
				lenDocY$portAct==lenuse$port[i] &
        	                lenDocY$mcatAct==lenuse$mcat[i] &  
        	                lenDocY$sppAct==lenuse$species[i]
			,c('sppUse','dispUse','gearUse','portUse','mcatUse')]
			borr = c(bor, "B")
			#if its not in the lenDoc this it must have been ignored
			if( nrow(bor)==0 ){ borr=rep("I", 6) } 
			#
			lenuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'lenSource')] = borr
		}
		colnames(lenuse) = c('species', 'live', 'gear', 'port', 'mcat',  'source', 'expLands', 'sppStratSamCt', 'borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'alsSource')
		
		##
		#lendoc = lenuse[,c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'alsSource', 'alsSource', 'borrGear', 'borrPort', 'sppStratSamCt')]
		#colnames(lendoc) = c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'expanded', 'borrowed', 'borrGear', 'borrPort', 'sppStratSamCt')
		#lendoc = cbind(year, lendoc)
		##
		#lendoc$expanded[lendoc$expanded!="I"] = "Y"
		#lendoc$expanded[lendoc$expanded=="I"] = "N"
		##
		#numSource = sapply(lendoc$borrowed, function(l){which(LETTERS==l)})
		#lendoc$borrowed = "N"
		#lendoc$borrowed[numSource>1 & numSource<9] = "Y"
		#lendoc[lendoc$borrowed!="Y", c("borrGear", "borrPort")] = ""
		##NOTE: this is what don does, but in sppComp we only include expanded strata, but we include all expanded strata sources

		#PREP FOR EXPAND
		
		#species and stratum level; a working object
		samp = lenuse[lenuse$alsSource!='I',] #implements the ignores     
		colnames(samp) = c('actSpecies', 'actLive', 'actGear', 'actPort', 'actMcat',  'source', 'expLands', 'sppStratSamCt', 'species', 'live', 'gear', 'port', 'mcat', 'alsSource')
		#
		sppStratSamLbs = aggregate(samphead$alsLbs, by=samphead[,c('species', 'gear', 'port', 'mcat', 'live')], FUN=sum)
		colnames(sppStratSamLbs) = c('species', 'gear', 'port', 'mcat', 'live', 'sppStratSamLbs')
		samp = merge(samp, sppStratSamLbs)
		#
		#average spp weight in stratum
		samp$sppStratAvgW = samp$sppStratSamLbs/samp$sppStratSamCt
		#landings converted to average numbers
		samp$expNums = samp$expLands/samp$sppStratAvgW
		
		#EXPAND
		
		#iterate over expanded sppStrata
		lencom = data.frame(species=character(0), disp=character(0), gear=character(0), port=character(0), mcat=numeric(0), flength=numeric(0), sex=numeric(0), ex=numeric(0))
		for(i in (1:nrow(samp))){
		        #samp[i,'expNums'] will be zero in strata that require borrows, should use the the borrowed expNum
		        sampB = samp[
		                samp$actGear==samp[i,'gear'] &
		                samp$actPort==samp[i,'port'] &
		                samp$actMcat==samp[i,'mcat'] &
		                samp$actLive==samp[i,'live'] &
		                samp$actSpecies==samp[i,'species']
		        ,]
		        #get the sample stats for the sppStrata
		        stats = samphead[
		                samphead$gear==samp[i,'gear'] &
		                samphead$port==samp[i,'port'] &
		                samphead$mcat==samp[i,'mcat'] &
		                samphead$live==samp[i,'live'] &
		                samphead$species==samp[i,'species']
		        ,] 
			
		        #
		        z = 0
		        for(sid in stats$sample_no){
		                #get the fish for the sppStrat
		                fish = sampfish[
		                        sampfish$sample_no==sid &
		                        sampfish$species==samp[i,'species']
		                ,]
				#avoids warning if null
        	                if(length(fish$flength[fish$flength>0])==0 | length(fish$sex[fish$flength>0])==0){ next }
				#
		                h = squash::hist2(fish$flength[fish$flength>0], fish$sex[fish$flength>0], xbreaks=0:151, ybreaks=c(1,2,3,9), plot=F)
		                hz = h$z
		                hz[is.na(hz)] = 0
		                #weight by landings
		                z = z + hz*stats[stats$sample_no==sid, 'totLbs'] 
		        }
		        #
		        grid = expand.grid(h$x[-length(h$x)], h$y[-length(h$y)])
			
		        #The term (hz*stats[stats$sample_no==sid, 'totLbs']) above weights by landings and the term (stats$ageCt/stats$totCt), as Don uses, is not needed here.
		        histLS = z/sum(z) * sampB$expNums 
		        histX = matrix(grid[,1], ncol=3)
		        histY = matrix(grid[,2], ncol=3)
		
		        #
		        out = cbind(species=samp[i,'actSpecies'], year=year, disp=samp[i,'actLive'], gear=samp[i,'actGear'], port=samp[i,'actPort'], mcat=samp[i,'actMcat'], flength=histX[!is.na(histLS)], sex=histY[!is.na(histLS)], ex=histLS[!is.na(histLS)])
		        lencom = rbind(lencom, out)
		}
		lencom$flength = as.numeric(lencom$flength)
		lencom$mcat = as.numeric(lencom$mcat)
		lencom$x = as.numeric(lencom$ex)
		lencom = subset(lencom, select = c(-ex))
		lencom$sex[lencom$sex==3] = 9
		colnames(lencom) = c("species", "year", "disp", "gear", "port", "mcat", "flength", "sex", "binHeight")
		#reorder?
		#lencom = lencom[,c('year','disp','mcat','gear','port','source','spp','lands','comp')]
		expOut = rbind(expOut, lencom)
	}
	
	#
	return(expOut)
}

##
##TEST
##
#
##
##future inputs
#year = 2022 #2013:2015 #2020 #c(2021, 2022) #2021 #
##NOTE: throw an error if year less 1975
##NOTE: 2016, 2017; OCA in lenuse$port[i]
##NOTE: 2013: WARNING
##1: In `[<-.data.frame`(`*tmp*`, i, c("borrSpecies", "borrLive", "borrGear",  :
##  replacement element 1 has 2 rows to replace 1 rows
##NOTE 2000,?2001?: no samples? no actuals
#
##
#calcomLenDat = getCalcomLenData(year, save=T, fromFile=T) #
##
#lenExp1 = estLenComp(calcomLenDat, files=T)
#lenExp2 = estLenCompDoc(calcomLenDat) #, files=T)


