#rm(list=ls())

#
#DEPENDENCIES
#

#RJDBC requires the driver files for each database to function:
#       MS-SQL  : ./sqljdbc4.jar
#       Oracle  : ./ojdbc8.jar
#
##upon installing RJDBC you may need to run "R CMD javareconf" command in the 
##terminal as root to add Java support to R.
#suppressMessages(library(RJDBC, quietly=FALSE))
##
#suppressMessages(library(dplyr, quietly=FALSE))
##
#suppressMessages(library(getPass, quietly=FALSE))
##
#suppressMessages(library(squash, quietly=FALSE))

#Based on notes from donExpandAgeButREj.r

#
#ANCILLARY FUNCTIONS
#

#NOTE: already provided in lenCompFunk.r
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

##NOTE: please remove the squash dependency at some point and roll your own version of the squash::hist2() function.
#base2DHist = function(df, xbins, ybins, freak=F){
#	# https://www.r-bloggers.com/2014/09/5-ways-to-do-2d-histograms-in-r/
#	# http://stackoverflow.com/questi?ons/18089752/r-generate-2d-histogram-from-raw-data
#	
#	#
#	freq =  as.data.frame(table(findInterval(df[,1], xbins),findInterval(df[,2], ybins)))
#	freq[,1] = as.numeric(freq[,1])
#	freq[,2] = as.numeric(freq[,2])
#	
#	#
#	freq2D = matrix(0, length(xbins), length(ybins))
#	freq2D[cbind(freq[,1], freq[,2])] = freq[,3]
#
#	#return proportion
#	if( !freq ){
#		return( freq2D/sum(freq2D) )
#	}
#
#	#
#	return( freq2D )
#		
#	## Normal
#	#image(x.bin, y.bin, freq2D, col=r)
#	#
#	## Log
#	#image(x.bin, y.bin, log(freq2D), col=r)
#}


#
#HEADER
#

##future inputs
#year = 2002 #2022 #2018 #2
##NOTE: throw an error if year less 1975
#
##
#map = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
#
##
#qtrMatrix = matrix(
#        c(
#        2, 3, 4,
#        3, 1, 4,
#        2, 4, 1,
#        3, 2, 1
#        ),
#4, 3, byrow=T)
#colnames(qtrMatrix) = c('first', 'second', 'third')
#rownames(qtrMatrix) = 1:4

##Two Ports Away, No Conception
#portMatrix = matrix( 
#	c(
#        'ERK', 'BRG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'CRS', 'BRG', 'BDG'    , 'NOMINAL', 'NOMINAL',
#        'ERK', 'BDG', 'OSF'    , 'CRS'    , 'NOMINAL',
#        'OSF', 'BRG', 'MNT'    , 'ERK'    , 'NOMINAL',
#        'BDG', 'MNT', 'BRG'    , 'MRO'    , 'NOMINAL',
#        'OSF', 'MRO', 'BDG'    , 'NOMINAL', 'NOMINAL',
#        'MNT', 'OSF', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OLA', 'OSD', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OSB', 'OSD', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OLA', 'OSB', 'NOMINAL', 'NOMINAL', 'NOMINAL'
#        ),
#10, 5, byrow=T)
#colnames(portMatrix) = c('first', 'second', 'third', 'fourth', 'fifth')
#rownames(portMatrix) = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')

##One Port Away, No Conception
#portMatrix1 = matrix(
#        c(
#        'ERK', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'CRS', 	   'BRG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'ERK',     'BDG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OSF',     'BRG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'BDG',     'MNT', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OSF',     'MRO', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'MNT', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OLA', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OSB',     'OSD', 'NOMINAL', 'NOMINAL', 'NOMINAL',
#        'OLA', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL'
#        ),	
#10, 5, byrow=T)
#colnames(portMatrix1) = c('first', 'second', 'third', 'fourth', 'fifth')
#rownames(portMatrix1) = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')

#
#DATABASE FUNCTIONS
#

#
#length_expand.sql FROM CALCOM
#

#pull from calcom 
#' Collect and prepare Calcom data for age expansion.
#'
#' @param year  The year of the expansion given as a four digit integer. Year may 
#'      be given as a vector to prepare data for a multi-year expansion.
#' @param save  A filename.csv to save data to file or a logical. If save=True 
#'      the filename defalts to sprintf('calcom%sAgeData%s', year, Sys.time()); 
#'      save=False (Default) does not save.
#' @param fromFile A filename.csv to read data from file or a logical. 
#'      fromFile=True defaults to the most recent sprintf('pacfin%sData*', year) 
#'      file; fromFile=False (Default) reads data from calcom.
#'
#' @return Returns a list of the various objects from calcom needed to compute 
#'      a length expansions for the given years.
getCalcomAgeData = function(year, save=F, fromFile=F){
        #year     : the year of the expansion. 
        #save     : a filename.RData to save data to file or a logical.
        #               True defaults to the filename sprintf('calcom%sData%s', year, Sys.time()); False (Default) does not save.
        #fromFile : a filename.rda to read data from file or a logical.
        #               True defaults to the most recent sprintf('calcom%sData*', year) file; False (Default) reads data from calcom.
        #
        #value    : returns a list of all of the various objects in calcom needed to compute an expansion in the given year.

	#NOTE: Still need to figure out how to recreate the exact results from pacfin only

        #error if the given year is before the data would exist (also catches two digit years)
        stopifnot( year>=1975 )
        #year should not be fractional
        stopifnot( floor(year)%in%year )

	#
        if( fromFile!=F ){
                #if fromFile=T make the name the most recent date available
                if( fromFile==T ){
                        years = paste(unique(year), collapse='')
                        #get possible files
                        possibleFiles = list.files(path='.', pattern=glob2rx(sprintf('calcom%sAgeData*', years)))
                        stopifnot( length(possibleFiles)>0 )
                        #
                        firstPart = sprintf("calcom%sAgeData", years)
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
			
			##the lazy version
			#mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'")
			## CALCOM connection
			#mCon = RJDBC::dbConnect(mDrv, 'jdbc:sqlserver://sql2016.psmfc.org\\calcom;databaseName=CALCOM', 'ngrunloh', 'calcom!PSMFC2022') #
			#NOTE: swap out passwords for getPass call
			#NOTE: swap out dirver for 
			mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', system.file("drivers/sqljdbc4.jar", package="calcomExpansions"), identifier.quote="'")
        		# CALCOM connection	
        		mCon = RJDBC::dbConnect(mDrv, 'jdbc:sqlserver://sql2016.psmfc.org\\calcom;databaseName=CALCOM', getPass::getPass('CALCOM User: '), getPass::getPass('Password: ')) 
			
			#
			writeLines("\nReading CALCOM Age Data From CALCOM Connection...")
			
			#
        		tempAge1 = c()
        		tempAge2 = c()
        		tempAge3 = c()
        		tempAge4 = c()
        		for(y in year){
				#year%%100: uses modular arithmatic to get last two digits of the year.
				twoDigitYear = y%%100
				
				#Five (four) Files		
				
				#NOTE: sums over qtr 
				#?I think for data weighting by expanded landing within a species?
				tempAge1 = rbind(tempAge1, RJDBC::dbGetQuery(mCon, sprintf('
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
				#tempAge1$LBS = as.numeric(tempAge1$LBS)
				
				#2. select * into temp_len_2 from len_samp_vu where lenct1>9 and @expandyr=yr order by live_fish, species, mark_cat, port_complex, gear_grp
				tempAge2 = rbind(tempAge2, RJDBC::dbGetQuery(mCon, sprintf('select * from age_samp_vu where agect>9 and %d=yr order by live_fish, species, mark_cat, port_complex, gear_grp', twoDigitYear)))
				
				#3. select * into temp_len_3 from len_expand_data_vu where substring(sample_no,3,2)=@expandyr
				tempAge3 = rbind(tempAge3, RJDBC::dbGetQuery(mCon, sprintf('select * from age_expand_data_vu where substring(sample_no,3,2)=%d', twoDigitYear)))
				
				#4. select sample_no, species, flength, age, sex into temp_len_4 from master_fish where substring(sample_no,3,2)=@expandyr
				#select sample_no, species,flength,age,sex,substring(sample_no,3,2) from master_fish
				tempAge4 = rbind(tempAge4, RJDBC::dbGetQuery(mCon, sprintf('
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
				#tempAge4 = tempAge4[!is.na(tempAge4$age),]
				#NOTE: dont remove NA just count the non NAs
			}
			tempAge1$LBS = as.numeric(tempAge1$LBS)
			tempAge2$YR = as.numeric(tempAge2$YR)

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
        	        tempAge1 = tempAge1,
        	        tempAge2 = tempAge2,
        	        tempAge3 = tempAge3,
        	        tempAge4 = tempAge4
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
        	        if( save==T ){ save=sprintf('calcom%sAgeData%s.rda', years, as.POSIXlt(Sys.time())) }
        	        #write the data to file.
        	        saveRDS(out, file=save)
        	}
	#fromFile else
	}

        #
        return( out )
}

#
#EXPANSION FUNCTIONS
#

#' The core expansion function executing an automated age expansion of Don's 
#' Visual Basic code.
#' 
#' @param calcomAgeData A list as returned by getCalcomAgeData.
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
estAgeComp = function(calcomAgeData, portBorr=portMatrix1, files=T){
        #calcomAgeData : a list as returned by getCalcomAgeData
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
	#NOTE: there seems to be a special rule for these gears (not TWL) no port to borrow from north or south AND one of these gears then IGNORE 
	#I think basically we only want to borrow in TWL gear  
	notGear = c('HKL', 'FPT', 'NET', 'MDT')
	
	#DEAL W/ YEAR

        #
        sppExpYear = as.numeric(unique(calcomAgeData$tempAge1$year))
        fishYear = as.numeric(unique(substr(calcomAgeData$tempAge3$sample_no, 1, 4)))
        #make sure species expansion years match the fish level years 
        stopifnot( sppExpYear %in% fishYear )

	#handle years without ages
	isNoAges = c()
	for(i in 1:length(fishYear)){
		#	
		ages = calcomAgeData$tempAge4[
			substr(calcomAgeData$tempAge4$sample_no, 1, 4)==fishYear[i] &
			!calcomAgeData$tempAge4$species%in%notSpp
		,'age']
		isNoAges = c(isNoAges, all(is.na(ages)))
	}
	#
	if( any(isNoAges) ){
		stop(sprintf("\n\t No Ages in %s.", fishYear[isNoAges]))
	}	
	
	#
        expOut = c()
        for(year in fishYear){
		
		#UNPACK
		
		#
	        twoDigitYear = year%%100
	        #
	        tempAge1 = calcomAgeData$tempAge1[calcomAgeData$tempAge1$year==year,]
	        tempAge2 = calcomAgeData$tempAge2[calcomAgeData$tempAge2$YR==twoDigitYear,]
	        tempAge3 = calcomAgeData$tempAge3[substr(calcomAgeData$tempAge3$sample_no, 1, 4)==year,]
	        tempAge4 = calcomAgeData$tempAge4[substr(calcomAgeData$tempAge4$sample_no, 1, 4)==year,]	
		#print(head(tempAge4))
		
		# Getting Landings
		
		#remove some gears
		landstrat1 = tempAge1[
			tempAge1$gear_grp!='UNK' & 
			tempAge1$gear_grp!='MSC' 
			#& tempLen1$gear_grp!='OTH' #NOTE: Done in Length but Don doesn't do it for Age
		,]
		#NOTE: all BORROWs that I've seen in the data end in T-#
		#only keep sources that end in "AL" or "OW"
		#get the right two characters of source for comparison #NOTE: this is how don did it, but I'd rather just know which categories hes wants to keep here. 
		rightTwo = substring(landstrat1$source, nchar(landstrat1$source)-2+1, nchar(landstrat1$source))
		landstrat1 = landstrat1[rightTwo=="OW" | rightTwo=="AL",]
		colnames(landstrat1) = c('year', 'species', 'live', 'gear', 'port', 'mcat', 'source', 'lbs')
		
		#
		agect1 = tempAge2
		colnames(agect1) = c('yr', 'species', 'live', 'gear', 'port', 'mcat', 'agect')
		
		#	
		agestrat2 = merge(landstrat1[,-1], agect1[,-1], all.x=T)
		agestrat2$agect[is.na(agestrat2$agect)] = 0
		#remove rows where the sample count is below 10 where the lbs are also below 10
		agestrat2 = agestrat2[!(agestrat2$agect<10 & agestrat2$lbs<10),]
		
		#Get Length Data
		
		#for every stratum, age, length, and sex
		#identify totLbs, alsLbs, totCt, alsCt, expLands 
			
		#
		#raw fish level
		sampfish = tempAge4
		sampfish$flength = sampfish$flength/10 #round(sampfish$flength/10)
		#sampfish = sampfish[sampfish$flength>0]
		#sampfish$flength[sampfish$flength>149] = 150
	
		#NOTE: opted to error instead	
		##handle years without ages
		#if( all(is.na(sampfish$age)) ){
		#	warning(sprintf("\n\t No Ages in %s: Age expansion not done in %s.", year, year))
		#	next
		#}	
		
		#sample statistics of the raw fish
		samphead = tempAge3
		samphead = samphead[samphead$mark_cat!=0,]
		colnames(samphead) = c('species', 'live', 'gear', 'port', 'mcat', 'totLbs', 'sample_no', 'alsLbs')
		
		#NOTE: confirm that we don't want to include 'sex' here VVVVVVVVVVVVVVVV
		totCt = aggregate(sampfish$sample_no, by=sampfish[,c('sample_no', 'species')], FUN=length)
		colnames(totCt) = c('sample_no', 'species', 'totCt')
		samphead = merge(samphead, totCt)
		#number of aged fish
		ageCt = aggregate(sampfish$sample_no[!is.na(sampfish$age)], by=sampfish[!is.na(sampfish$age),c('sample_no', 'species')], FUN=length)
		colnames(ageCt) = c('sample_no', 'species', 'ageCt')
		samphead = merge(samphead, ageCt)
		#
		#average weight
		samphead$avgW = samphead$alsLbs/samphead$totCt
		#average length numbers
		samphead$avgLN = samphead$totLbs/samphead$avgW
		#NOTE: best I can figure from documentation: convert average numbers of length'd fish to numbers of aged fish
		samphead$avgAN = samphead$avgLN*(samphead$ageCt/samphead$totCt)
		#NOTE: equivalent to samphead$avgW = samphead$alsLbs/samphead$ageCt
		#NOTE: not used
		
		#AGEUSE/AGEDOC CODE
		#Working...
		
		#
		nc = sum(!(agestrat2$species%in%notSpp))
		ageuse = data.frame(agestrat2[!agestrat2$species%in%notSpp,], borrSpecies=character(nc), borrLive=character(nc), borrGear=character(nc), borrPort=character(nc), borrMcat=character(nc), ageSource=character(nc))
		#actuals
		aWho = ageuse$agect>ctThresh
		actuals = cbind(ageuse[aWho, c('agect', 'species', 'live', 'gear', 'port', 'mcat')], 'A')
		ageuse[aWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = actuals[,-1] 
		#ignores
		iWho = (ageuse$lenct<=ctThresh & ageuse$lbs<totThresh) | ageuse$gear=='MSC' | ageuse$gear=='OTH'
		ageuse[iWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = rep('I', 6)
		
		#borrow
		bWho = ageuse$ageSource==''
		ageuse[bWho, 'ageSource'] = 'B'
		for(i in which(bWho)){
			#
			sam = actuals[	
				actuals$live==ageuse$live[i]		&
				actuals$gear==ageuse$gear[i]		&
				actuals$mcat==ageuse$mcat[i]		&
				actuals$species==ageuse$species[i]
			,]
			#if no available samples found in adjacent strata, then the strata is nominal
		        if( nrow(sam)==0 ){ ageuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = rep('I', 6); next }
			
			#
			borr = rep("I", 6)
			#NOTE: in 2017 an unknown port appears that creates an error
			#port priority for the given missing strata
		        portPriority = portBorr[ageuse$port[i],]
			fars = unlist(howFar(ageuse$port[i], portPriority))	
			for(d in sort(unique(fars))){
				# a version of portPriority that is at the radius d
				pp = portPriority[names(fars)[fars==d]]
				# a version of sam that is at the radius d
				ss = sam[sam$port%in%pp,]	
			
				#if no samples at d move to a larger d
				if( length(ss$agect)==0 ){ next }	
				
				#borrow from the smallest d with the most samples
				rowSource = LETTERS[d+1]
				borr = cbind(ss[ss$agect==max(ss$agect), c('species', 'live', 'gear', 'port', 'mcat')], rowSource)
				#if you've made it this far there borr has the strata with the most samples at the smallest d 
				break
			}
			#
			ageuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = borr
		}
		colnames(ageuse) = c('species', 'live', 'gear', 'port', 'mcat',  'source', 'expLands', 'sppStratSamCt', 'borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'alsSource')
		
		#
		agedoc = ageuse[,c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'alsSource', 'alsSource', 'borrGear', 'borrPort', 'sppStratSamCt')]
		colnames(agedoc) = c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'expanded', 'borrowed', 'borrGear', 'borrPort', 'sppStratSamCt')
		agedoc = cbind(year, agedoc)
		#
		agedoc$expanded[agedoc$expanded!="I"] = "Y"
		agedoc$expanded[agedoc$expanded=="I"] = "N"
		#
		numSource = sapply(agedoc$borrowed, function(l){which(LETTERS==l)})
		agedoc$borrowed = "N"
		agedoc$borrowed[numSource>1 & numSource<9] = "Y"
		agedoc[agedoc$borrowed!="Y", c("borrGear", "borrPort")] = ""
		#NOTE: this is what don does, but in sppComp we only include expanded strata, but we include all expanded strata sources
		agedocDon = agedoc
		
		#NOTE: the extra stuff needed to complete agedoc as done in lendoc
		agedoc = agedoc[agedoc$expanded=="Y",]
                isBorr = agedoc$borrowed=="Y"
                #copy over stuff where no borrowing can happen
                agedoc$yearUse = agedoc$year
                agedoc$sppUse  = agedoc$species 
                agedoc$dispUse = agedoc$live
                agedoc$mcatUse = agedoc$mcat
                #               
                agedoc$borrGear[!isBorr] = agedoc$gear[!isBorr]  
                agedoc$borrPort[!isBorr] = agedoc$port[!isBorr]  
                #               
                for(i in which(isBorr)){ 
                        agedoc$sppStratSamCt[i] = max(agedoc[
                                agedoc$year==agedoc$yearUse[i]   &
                                agedoc$species==agedoc$sppUse[i] &
                                agedoc$live==agedoc$dispUse[i]   &
                                agedoc$mcat==agedoc$mcatUse[i]   &
                                agedoc$gear==agedoc$borrGear[i]  &
                                agedoc$port==agedoc$borrPort[i]
                       ,'sppStratSamCt']) 
                }
                #remove the unneccessary columns, reorder, and then rename
                agedoc = agedoc[,c('year', 'species', 'live', 'mcat', 'gear', 'port', 'yearUse', 'sppUse', 'dispUse', 'mcatUse', 'borrGear', 'borrPort', 'sppStratSamCt')]
                colnames(agedoc) = c('yearAct', 'sppAct', 'dispAct', 'mcatAct', 'gearAct', 'portAct', 'yearUse', 'sppUse', 'dispUse', 'mcatUse', 'gearUse', 'portUse', 'ctUse')
		
		#
                if( files ){
                        write.csv(agedoc, sprintf("agedoc%s.csv", year), row.names=F, quote=F)
                }
		
		#PREP FOR EXPAND
		
		#species and stratum level; a working object
		samp = ageuse[ageuse$alsSource!='I',] #implements the ignores      #lenuse[lenuse$alsSource=='A',]#
		colnames(samp) = c('actSpecies', 'actLive', 'actGear', 'actPort', 'actMcat',  'source', 'expLands', 'sppStratSamCt', 'species', 'live', 'gear', 'port', 'mcat', 'alsSource')
		#samp = merge(samp, samphead, all.x=T)
		#samp = subset(samp, select = c(-sppStratSamCt))
		#
		sppStratSamLbs = aggregate(samphead$alsLbs, by=samphead[,c('species', 'gear', 'port', 'mcat', 'live')], FUN=sum)
		colnames(sppStratSamLbs) = c('species', 'gear', 'port', 'mcat', 'live', 'sppStratSamLbs')
		samp = merge(samp, sppStratSamLbs)
		#
		sppStratAgeCt = aggregate(samphead$ageCt, by=samphead[,c('species', 'gear', 'port', 'mcat', 'live')], FUN=sum)
		colnames(sppStratAgeCt) = c('species', 'gear', 'port', 'mcat', 'live', 'sppStratAgeCt')
		samp = merge(samp, sppStratAgeCt)
		#
		#average spp weight in stratum (amoung all fish)           (amoung aged fish)
		samp$sppStratAvgW = samp$sppStratSamLbs/samp$sppStratAgeCt #samp$sppStratSamCt #
		#landings converted to average numbers
		samp$expNums = samp$expLands/samp$sppStratAvgW
		
		#EXPAND
		
		#iterate over expanded sppStrata
		agecom = data.frame(species=character(0), year=numeric(0), disp=character(0), gear=character(0), port=character(0), mcat=numeric(0), flength=numeric(0), sex=numeric(0), ex=numeric(0))
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
			##get the fish for the sppStrat
			#fish = sampfish[
			#	sampfish$sample_no%in%stats[,'sample_no'] &
			#	sampfish$species==samp[i,'species']
			#,]
			
			#
			z = 0
			for(sid in stats$sample_no){
				#get the fish for the sppStrat
		        	fish = sampfish[
		        	        sampfish$sample_no==sid &
		        	        sampfish$species==samp[i,'species']
		        	,]
				h = squash::hist2(fish$age, fish$sex, xbreaks=0:99, ybreaks=c(1,2,3,9), plot=F)
				hz = h$z
				hz[is.na(hz)] = 0
				#avgLN, avgAN, or totLbs
				z = z + hz*stats[stats$sample_no==sid, 'avgAN'] #'totLbs'] #'avgLN'] #
			}
			#
			grid = expand.grid(h$x[-length(h$x)], h$y[-length(h$y)])
		
			##NOTE: multiple samples in stratum; should weight each sample by relatively how many fish were aged in the sample?
			##if( nrow(fish)==0 ){ next }
			###centered on bins name  [fish$flength>0]
			##h = hist2(fish$flength[fish$flength>0], fish$sex[fish$flength>0], xbreaks=-1:150+0.5, ybreaks=c(0.5,1.5,2.5,9.5), plot=F)
			##grid = expand.grid((h$x+0.5)[-length(h$x)], (h$y+0.5)[-length(h$y)])
			##left edge is bin name
			#h = hist2(fish$age, fish$sex, xbreaks=0:99, ybreaks=c(1,2,3,9), plot=F)
			#grid = expand.grid(h$x[-length(h$x)], h$y[-length(h$y)])	
			
			#
			histLS = z/sum(z) * sampB$expNums #NOTE: not this because of above * (stats$ageCt/stats$totCt)
			histX = matrix(grid[,1], ncol=3)
			histY = matrix(grid[,2], ncol=3)
			
			#
			#out = cbind(species=samp[i,'species'], disp=samp[i,'live'], gear=samp[i,'gear'], port=samp[i,'port'], mcat=samp[i,'mcat'], flength=histX[!is.na(histLS)], sex=histY[!is.na(histLS)], ex=histLS[!is.na(histLS)])
			out = cbind(species=samp[i,'actSpecies'], year=year, disp=samp[i,'actLive'], gear=samp[i,'actGear'], port=samp[i,'actPort'], mcat=samp[i,'actMcat'], age=histX[!is.na(histLS)], sex=histY[!is.na(histLS)], ex=histLS[!is.na(histLS)])
			agecom = rbind(agecom, out)
		}
		agecom$age = as.numeric(agecom$age)
		agecom$mcat = as.numeric(agecom$mcat)
		agecom$x = as.numeric(agecom$ex)
		agecom = subset(agecom, select = c(-ex))
		agecom$sex[agecom$sex==3] = 9
		colnames(agecom) = c("species", "year", "disp", "gear", "port", "mcat", "age", "sex", "binHeight")
	
		#
		expOut = rbind(expOut, agecom)
	}
	
	#
	return(expOut)
}

#' A version of the core expansion function executing an automated age expansion 
#' based on the borrowing dictated by a doc file. 
#' 
#' @param calcomAgeData    A list as returned by getCalcomAgeData.
#' @param doc           The filename (or vector of filenames) of the doc files 
#'      to use in manual borrowing overrides. Defaults to sprintf("lendoc%s.csv", unique(calcomLenData$tempLen1$year)
#' @param files         A boolean flag to produce verbose error files and/or expansion output 
#'      files such as sppdoc.
#'
#' @return a data.frame reminiscent of the calcom ___ table.
estAgeCompDoc = function(calcomAgeData, doc=sprintf("agedoc%s.csv", unique(calcomAgeData$tempAge1$year)), files=T){
        #calcomAgeData : a list as returned by getCalcomAgeData
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
	#NOTE: there seems to be a special rule for these gears (not TWL) no port to borrow from north or south AND one of these gears then IGNORE 
	#I think basically we only want to borrow in TWL gear  
	notGear = c('HKL', 'FPT', 'NET', 'MDT')
	
	#DEAL W/ YEAR

        #
        sppExpYear = as.numeric(unique(calcomAgeData$tempAge1$year))
        fishYear = as.numeric(unique(substr(calcomAgeData$tempAge3$sample_no, 1, 4)))
        #make sure species expansion years match the fish level years 
        stopifnot( sppExpYear %in% fishYear )
	
	#getting all the exDocs and checking year situation
        ageDocs = list()
        for(i in 1:length(fishYear)){
                #
                ageDocY = read.csv(doc[i])
                ageDocYear = unique(ageDocY$yearAct)
                #
                stopifnot( length(ageDocYear)==1 ) #the yealy exdoc files should only contain strata from a single year
                stopifnot( ageDocYear %in% fishYear )
                #
                ageDocs[[ageDocYear]] = ageDocY
        }

	#
        expOut = c()
        for(year in fishYear){
		
		#UNPACK
		
		#
	        twoDigitYear = year%%100
	        #
	        tempAge1 = calcomAgeData$tempAge1[calcomAgeData$tempAge1$year==year,]
	        tempAge2 = calcomAgeData$tempAge2[calcomAgeData$tempAge2$YR==twoDigitYear,]
	        tempAge3 = calcomAgeData$tempAge3[substr(calcomAgeData$tempAge3$sample_no, 1, 4)==year,]
	        tempAge4 = calcomAgeData$tempAge4[substr(calcomAgeData$tempAge4$sample_no, 1, 4)==year,]	
		#
		ageDocY = ageDocs[[year]]
		
		# Getting Landings
		
		#remove some gears
		landstrat1 = tempAge1[
			tempAge1$gear_grp!='UNK' & 
			tempAge1$gear_grp!='MSC' 
			#& tempLen1$gear_grp!='OTH' #NOTE: Done in Length but Don doesn't do it for Age
		,]
		#NOTE: all BORROWs that I've seen in the data end in T-#
		#only keep sources that end in "AL" or "OW"
		#get the right two characters of source for comparison #NOTE: this is how don did it, but I'd rather just know which categories hes wants to keep here. 
		rightTwo = substring(landstrat1$source, nchar(landstrat1$source)-2+1, nchar(landstrat1$source))
		landstrat1 = landstrat1[rightTwo=="OW" | rightTwo=="AL",]
		colnames(landstrat1) = c('year', 'species', 'live', 'gear', 'port', 'mcat', 'source', 'lbs')
		
		#
		agect1 = tempAge2
		colnames(agect1) = c('yr', 'species', 'live', 'gear', 'port', 'mcat', 'agect')
		
		#
		agestrat2 = merge(landstrat1[,-1], agect1[,-1], all.x=T)
		agestrat2$agect[is.na(agestrat2$agect)] = 0
		#remove rows where the sample count is below 10 where the lbs are also below 10
		agestrat2 = agestrat2[!(agestrat2$agect<10 & agestrat2$lbs<10),]
	
		#Get Length Data

		#for every stratum, age, length, and sex
		#identify totLbs, alsLbs, totCt, alsCt, expLands 
			
		#
		#raw fish level
		sampfish = tempAge4
		sampfish$flength = sampfish$flength/10 #round(sampfish$flength/10)
		#sampfish = sampfish[sampfish$flength>0]
		#sampfish$flength[sampfish$flength>149] = 150
		
		#handle years without ages
		if( all(is.na(sampfish$age)) ){
			warning(sprintf("\n\t No Ages in %s: Age expansion not done in %s.", year, year))
			next
		}	
		
		#sample statistics of the raw fish
		samphead = tempAge3
		samphead = samphead[samphead$mark_cat!=0,]
		colnames(samphead) = c('species', 'live', 'gear', 'port', 'mcat', 'totLbs', 'sample_no', 'alsLbs')
		
		#NOTE: confirm that we don't want to include 'sex' here VVVVVVVVVVVVVVVV
		totCt = aggregate(sampfish$sample_no, by=sampfish[,c('sample_no', 'species')], FUN=length)
		colnames(totCt) = c('sample_no', 'species', 'totCt')
		samphead = merge(samphead, totCt)
		#number of aged fish
		ageCt = aggregate(sampfish$sample_no[!is.na(sampfish$age)], by=sampfish[!is.na(sampfish$age),c('sample_no', 'species')], FUN=length)
		colnames(ageCt) = c('sample_no', 'species', 'ageCt')
		samphead = merge(samphead, ageCt)
		#
		#average weight
		samphead$avgW = samphead$alsLbs/samphead$totCt
		#average length numbers
		samphead$avgLN = samphead$totLbs/samphead$avgW
		#NOTE: best I can figure from documentation: convert average numbers of length'd fish to numbers of aged fish
		samphead$avgAN = samphead$avgLN*(samphead$ageCt/samphead$totCt)
		#NOTE: equivalent to samphead$avgW = samphead$alsLbs/samphead$ageCt
		#NOTE: not used
		
		#AGEUSE/AGEDOC CODE
		#Working...
		
		#
		nc = sum(!agestrat2$species%in%notSpp)
		ageuse = data.frame(agestrat2[!agestrat2$species%in%notSpp,], borrSpecies=character(nc), borrLive=character(nc), borrGear=character(nc), borrPort=character(nc), borrMcat=character(nc), ageSource=character(nc))
		#actuals
		aWho = ageuse$agect>ctThresh
		actuals = cbind(ageuse[aWho, c('agect', 'species', 'live', 'gear', 'port', 'mcat')], 'A')
		ageuse[aWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = actuals[,-1] 
		#ignores
		iWho = (ageuse$lenct<=ctThresh & ageuse$lbs<totThresh) | ageuse$gear=='MSC' | ageuse$gear=='OTH'
		ageuse[iWho, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = rep('I', 6)
		
		#borrow
		bWho = ageuse$ageSource==''
		ageuse[bWho, 'ageSource'] = 'B'
		for(i in which(bWho)){
			#
			sam = actuals[	
				actuals$live==ageuse$live[i]		&
				actuals$gear==ageuse$gear[i]		&
				actuals$mcat==ageuse$mcat[i]		&
				actuals$species==ageuse$species[i]
			,]
			#if no available samples found in adjacent strata, then the strata is nominal
		        if( nrow(sam)==0 ){ ageuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = rep('I', 6); next }
			
			#look up borr instead of calculating it
                        bor = ageDocY[
                                ageDocY$dispAct==ageuse$live[i] &
                                ageDocY$gearAct==ageuse$gear[i] &
                                ageDocY$portAct==ageuse$port[i] &
                                ageDocY$mcatAct==ageuse$mcat[i] &
                                ageDocY$sppAct==ageuse$species[i]
                        ,c('sppUse','dispUse','gearUse','portUse','mcatUse')]
                        borr = c(bor, "B")
                        #if its not in the lenDoc this it must have been ignored
                        if( nrow(bor)==0 ){ borr=rep("I", 6) }
			#
			ageuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = borr
			
			##NOTE: no neeeded in the doc version
			#borr = rep("I", 6)
			##port priority for the given missing strata
		        #portPriority = portBorr[ageuse$port[i],]
			#fars = unlist(howFar(ageuse$port[i], portPriority))	
			#for(d in sort(unique(fars))){
			#	# a version of portPriority that is at the radius d
			#	pp = portPriority[names(fars)[fars==d]]
			#	# a version of sam that is at the radius d
			#	ss = sam[sam$port%in%pp,]	
			#
			#	#if no samples at d move to a larger d
			#	if( length(ss$agect)==0 ){ next }	
			#	
			#	#borrow from the smallest d with the most samples
			#	rowSource = LETTERS[d+1]
			#	borr = cbind(ss[ss$agect==max(ss$agect), c('species', 'live', 'gear', 'port', 'mcat')], rowSource)
			#	#if you've made it this far there borr has the strata with the most samples at the smallest d 
			#	break
			#}
			##
			#ageuse[i, c('borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'ageSource')] = borr
		}
		colnames(ageuse) = c('species', 'live', 'gear', 'port', 'mcat',  'source', 'expLands', 'sppStratSamCt', 'borrSpecies', 'borrLive', 'borrGear', 'borrPort', 'borrMcat', 'alsSource')
		
		##NOTE: not needed in the doc version
		#agedoc = ageuse[,c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'alsSource', 'alsSource', 'borrGear', 'borrPort', 'sppStratSamCt')]
		#colnames(agedoc) = c('species', 'live', 'gear', 'port', 'mcat', 'expLands', 'expanded', 'borrowed', 'borrGear', 'borrPort', 'sppStratSamCt')
		#agedoc = cbind(year, agedoc)
		##
		#agedoc$expanded[agedoc$expanded!="I"] = "Y"
		#agedoc$expanded[agedoc$expanded=="I"] = "N"
		##
		#numSource = sapply(agedoc$borrowed, function(l){which(LETTERS==l)})
		#agedoc$borrowed = "N"
		#agedoc$borrowed[numSource>1 & numSource<9] = "Y"
		#agedoc[agedoc$borrowed!="Y", c("borrGear", "borrPort")] = ""
		##NOTE: this is what don does, but in sppComp we only include expanded strata, but we include all expanded strata sources
		#agedocDon = agedoc
		#
		##NOTE: the extra stuff needed to complete agedoc as done in lendoc
		#agedoc = agedoc[agedoc$expanded=="Y",]
                #isBorr = agedoc$borrowed=="Y"
                ##copy over stuff where no borrowing can happen
                #agedoc$yearUse = agedoc$year
                #agedoc$sppUse  = agedoc$species 
                #agedoc$dispUse = agedoc$live
                #agedoc$mcatUse = agedoc$mcat
                ##               
                #agedoc$borrGear[!isBorr] = agedoc$gear[!isBorr]  
                #agedoc$borrPort[!isBorr] = agedoc$port[!isBorr]  
                ##               
                #for(i in which(isBorr)){ 
                #        agedoc$sppStratSamCt[i] = max(agedoc[
                #                agedoc$year==agedoc$yearUse[i]   &
                #                agedoc$species==agedoc$sppUse[i] &
                #                agedoc$live==agedoc$dispUse[i]   &
                #                agedoc$mcat==agedoc$mcatUse[i]   &
                #                agedoc$gear==agedoc$borrGear[i]  &
                #                agedoc$port==agedoc$borrPort[i]
                #       ,'sppStratSamCt']) 
                #}
                ##remove the unneccessary columns, reorder, and then rename
                #agedoc = agedoc[,c('year', 'species', 'live', 'mcat', 'gear', 'port', 'yearUse', 'sppUse', 'dispUse', 'mcatUse', 'borrGear', 'borrPort', 'sppStratSamCt')]
                #colnames(agedoc) = c('yearAct', 'sppAct', 'dispAct', 'mcatAct', 'gearAct', 'portAct', 'yearUse', 'sppUse', 'dispUse', 'mcatUse', 'gearUse', 'portUse', 'ctUse')
		#
		##
                #if( files ){
                #        write.csv(agedoc, sprintf("agedoc%s.csv", year), row.names=F, quote=F)
                #}
		
		#PREP FOR EXPAND
		
		#species and stratum level; a working object
		samp = ageuse[ageuse$alsSource!='I',] #implements the ignores      #lenuse[lenuse$alsSource=='A',]#
		colnames(samp) = c('actSpecies', 'actLive', 'actGear', 'actPort', 'actMcat',  'source', 'expLands', 'sppStratSamCt', 'species', 'live', 'gear', 'port', 'mcat', 'alsSource')
		#samp = merge(samp, samphead, all.x=T)
		#samp = subset(samp, select = c(-sppStratSamCt))
		#
		sppStratSamLbs = aggregate(samphead$alsLbs, by=samphead[,c('species', 'gear', 'port', 'mcat', 'live')], FUN=sum)
		colnames(sppStratSamLbs) = c('species', 'gear', 'port', 'mcat', 'live', 'sppStratSamLbs')
		samp = merge(samp, sppStratSamLbs)
		#
		sppStratAgeCt = aggregate(samphead$ageCt, by=samphead[,c('species', 'gear', 'port', 'mcat', 'live')], FUN=sum)
		colnames(sppStratAgeCt) = c('species', 'gear', 'port', 'mcat', 'live', 'sppStratAgeCt')
		samp = merge(samp, sppStratAgeCt)
		#
		#average spp weight in stratum (amoung all fish)           (amoung aged fish)
		samp$sppStratAvgW = samp$sppStratSamLbs/samp$sppStratAgeCt #samp$sppStratSamCt #
		#landings converted to average numbers
		samp$expNums = samp$expLands/samp$sppStratAvgW
		
		#EXPAND
		
		#iterate over expanded sppStrata
		agecom = data.frame(species=character(0), year=numeric(0), disp=character(0), gear=character(0), port=character(0), mcat=numeric(0), flength=numeric(0), sex=numeric(0), ex=numeric(0))
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
			##get the fish for the sppStrat
			#fish = sampfish[
			#	sampfish$sample_no%in%stats[,'sample_no'] &
			#	sampfish$species==samp[i,'species']
			#,]
			
			#
			z = 0
			for(sid in stats$sample_no){
				#get the fish for the sppStrat
		        	fish = sampfish[
		        	        sampfish$sample_no==sid &
		        	        sampfish$species==samp[i,'species']
		        	,]
				h = squash::hist2(fish$age, fish$sex, xbreaks=0:99, ybreaks=c(1,2,3,9), plot=F)
				hz = h$z
				hz[is.na(hz)] = 0
				#avgLN, avgAN, or totLbs
				z = z + hz*stats[stats$sample_no==sid, 'avgAN'] #'totLbs'] #'avgLN'] #
			}
			#
			grid = expand.grid(h$x[-length(h$x)], h$y[-length(h$y)])
		
			##NOTE: multiple samples in stratum; should weight each sample by relatively how many fish were aged in the sample?
			##if( nrow(fish)==0 ){ next }
			###centered on bins name  [fish$flength>0]
			##h = hist2(fish$flength[fish$flength>0], fish$sex[fish$flength>0], xbreaks=-1:150+0.5, ybreaks=c(0.5,1.5,2.5,9.5), plot=F)
			##grid = expand.grid((h$x+0.5)[-length(h$x)], (h$y+0.5)[-length(h$y)])
			##left edge is bin name
			#h = hist2(fish$age, fish$sex, xbreaks=0:99, ybreaks=c(1,2,3,9), plot=F)
			#grid = expand.grid(h$x[-length(h$x)], h$y[-length(h$y)])	
			
			#
			histLS = z/sum(z) * sampB$expNums #NOTE: not this because of above * (stats$ageCt/stats$totCt)
			histX = matrix(grid[,1], ncol=3)
			histY = matrix(grid[,2], ncol=3)
			
			#
			#out = cbind(species=samp[i,'species'], disp=samp[i,'live'], gear=samp[i,'gear'], port=samp[i,'port'], mcat=samp[i,'mcat'], flength=histX[!is.na(histLS)], sex=histY[!is.na(histLS)], ex=histLS[!is.na(histLS)])
			out = cbind(species=samp[i,'actSpecies'], year=year, disp=samp[i,'actLive'], gear=samp[i,'actGear'], port=samp[i,'actPort'], mcat=samp[i,'actMcat'], age=histX[!is.na(histLS)], sex=histY[!is.na(histLS)], ex=histLS[!is.na(histLS)])
			agecom = rbind(agecom, out)
		}
		agecom$age = as.numeric(agecom$age)
		agecom$mcat = as.numeric(agecom$mcat)
		agecom$x = as.numeric(agecom$ex)
		agecom = subset(agecom, select = c(-ex))
		agecom$sex[agecom$sex==3] = 9
		colnames(agecom) = c("species", "year", "disp", "gear", "port", "mcat", "age", "sex", "binHeight")
	
		#
		expOut = rbind(expOut, agecom)
	}
	
	#
	return(expOut)
}






#NOTE: 
#add agedocexpand functions



#write.csv(agecom, sprintf("agecom%s.csv", twoDigitYear), quote=F, row.names=F)
























		###
		###TEST
		###
		##
		###
		##expDiff = function(exp1, exp2){
		##        #
		##        exp2Needs = suppressMessages(anti_join(exp1, exp2))
		##        exp1Needs = suppressMessages(anti_join(exp2, exp1))
		##        #matchup diffs into a single data struture somehow
		##        return(list(twoNeeds=exp2Needs, oneNeeds=exp1Needs))
		##}
		##
		###
		###test = read.table("../CALCOM_LENGTH_EXPANSION_2020_Nick-EJexample/files after EXPAND/lenuse.txt", 
		###test = read.table("../CALCOM_LENGTH_EXPANSION_2021/files after EXPAND/lenuse.txt", 
		##test = read.table("../CALCOM_AGE_EXPANSION_2018_SABL_Nick-EJexample/files after EXPAND/ageuse.txt",
		##     fill=T,
		##     sep=""
		##)
		##test[test[,9]=='IIIIIIIIIIIIIIIIII', c(9,10,11,12,13,14)] = 'I'
		##colnames(test) = colnames(ageuse)
		###
		##needs = expDiff(ageuse[ageuse$species=='SABL',], test)
		##
		###
		###END TEST
		###
		
		#Get Length Data
		
		#for every stratum, age, length, and sex
		#identify totLbs, alsLbs, totCt, alsCt, expLands 









#lencom$flength = as.numeric(lencom$flength)
#lencom$mcat = as.numeric(lencom$mcat)
#lencom$x = as.numeric(lencom$ex)
#lencom = subset(lencom, select = c(-ex))
#lencom$sex[lencom$sex==3] = 9
#colnames(lencom) = c("species", "disp", "gear", "port", "mcat", "flength", "sex", "binHeight")
#
##
#write.csv(lencom, sprintf("lencom%s.csv", twoDigitYear), quote=F, row.names=F)










##
##TEST
##
#
##
#test = dbGetQuery(mCon, sprintf('
#                select * 
#                
#                from COM_AGE_COMPS 
#		
#		where year=%s
#        ', year)
#)
#test = test[,-2]
#test = test[,-9]
#test = test[,c('species', 'live', 'gear_grp', 'port_complex', 'mark_cat', 'age', 'sex', 'total')]
#test = test[test$age<99,]
#colnames(test) = colnames(agecom)
#test$mcat = as.character(test$mcat)
#test$binHeight = as.numeric(test$binHeight)
#
###
###test = read.table("../CALCOM_LENGTH_EXPANSION_2020_Nick-EJexample/files after EXPAND/lencom20.txt",
###test = read.table("../CALCOM_LENGTH_EXPANSION_2021/files after EXPAND/lencom21.txt",
##test = read.table("../CALCOM_AGE_EXPANSION_2018_SABL_Nick-EJexample/files after EXPAND/agecom18.txt",
##        fill=T,
##        sep=""
##)
###test = test[test$V10=='ACTUAL']
##test = test[,-1]
##test = test[,-9]
##colnames(test) = colnames(agecom)
##test$mcat = as.character(test$mcat)
#
###
##library(foreach)
##library(doParallel)
#library(RColorBrewer)
#
##
#makeTransparent = function(someColor, alpha=100){
#        newColor = col2rgb(someColor)
#        apply(newColor, 2, function(curcoldata){
#                rgb(red=curcoldata[1], green=curcoldata[2], blue=curcoldata[3], alpha=alpha, maxColorValue=255)
#        })
#}
#
##as.data.table(rep(diamonds$carat,diamonds$depth),ncol=1,byrow=TRUE)
#agecomAll = c()
#for(i in 1:nrow(agecom)){
##registerDoParallel(8)
##opts = list(preschedule=T)
##foreach(i=(1:length(lencom)), .options.multicore = opts) %dopar% {
#	#
#	r = round(agecom$binHeight[i])
#	if( r==0 ){ next }		#lencom[i,1:7]
#	strata = paste(agecom[i,c('species', 'disp', 'gear', 'port', 'mcat', 'sex')], collapse='')
#	agecomAll = rbind(agecomAll, matrix(unlist(rep(c(strata, agecom$age[i]), r)), ncol=2, byrow=T) )
#}
#colnames(agecomAll) = c('strata', 'age') #c('species', 'disp', 'gear', 'port', 'mcat', 'flength', 'sex')
#agecomAll = as.data.frame(agecomAll)
#agecomAll$age = as.numeric(agecomAll$age)
#
##
#agecomMean = aggregate(agecomAll$age, by=list(agecomAll$strata), FUN=mean)
#agecomVar = aggregate(agecomAll$age, by=list(agecomAll$strata), FUN=var)
#
##
#testAll = c()
#for(i in 1:nrow(test)){
##registerDoParallel(8)
##opts = list(preschedule=T)
##foreach(i=(1:length(test)), .options.multicore = opts) %dopar% {
#	#
#	r = round(test$binHeight[i])
#	if( r==0 ){ next }		#lencom[i,1:7]
#	strata = paste(test[i,c('species', 'disp', 'gear', 'port', 'mcat', 'sex')], collapse='')
#	testAll = rbind(testAll, matrix(unlist(rep(c(strata, test$age[i]), r)), ncol=2, byrow=T) )
#}
#colnames(testAll) = c('strata', 'age') #c('species', 'disp', 'gear', 'port', 'mcat', 'flength', 'sex')
#testAll = as.data.frame(testAll)
#testAll$age = as.numeric(testAll$age)
#testAll[order(),]
#
##
#testMean = aggregate(testAll$age, by=list(testAll$strata), FUN=mean)
#testVar = aggregate(testAll$age, by=list(testAll$strata), FUN=var)
#
##
#vs=merge(agecomVar, testVar, by="Group.1")
#ms=merge(agecomMean, testMean, by="Group.1")
#
##
#place = sprintf("./plots%s/", year)
#
##
#png(sprintf("%sMeans.png", place))
#plot(ms$x.y, ms$x.x, xlab="VB", ylab="R", xlim=c(0,110), ylim=c(0,110), main="All Spp:Strata Means")
#abline(0,1,col='red')
#dev.off()
##
#png(sprintf("%sSds.png", place))
#plot(sqrt(vs$x.y), sqrt(vs$x.x), xlab="VB", ylab="R", xlim=c(0,25), ylim=c(0,25), main="All Spp:Strata SDs")
#abline(0,1,col='red')
#dev.off()
#
#
##Dark2
#pal = brewer.pal(9, 'Set1');
##
#sexs = unique(agecom$sex)
#spps = names(head(sort(table(agecom$species), decreasing=T), 10))
#gear = unique(agecom$gear)
##disp = unique(lencom$disp)
##port = unique(lencom$port)
##mcat = unique(lencom$mcat)
#
##
#for(s in spps){
#	#
#	searchStr = sprintf("%s*", s)
#	testSearch = grep(searchStr, testAll$strata)
#	agecomSearch = grep(searchStr, agecomAll$strata)
#	if( length(testSearch)==0 | length(agecomSearch)==0){ next }
#	#
#	testStrat = testAll[grep(searchStr, testAll$strata),'age']
#	agecomStrat = agecomAll[grep(searchStr, agecomAll$strata),'age']
#	mm = min(c(testStrat, agecomStrat))
#	MM = max(c(testStrat, agecomStrat))
#	#
#	png(sprintf("%s%s_.png", place, s))
#	hist(testStrat, breaks=mm:MM, col=makeTransparent(pal[1], alpha=100), xlab="Length", main=sprintf("%s", s), freq=F)
#	hist(agecomStrat, breaks=mm:MM, col=makeTransparent(pal[2], alpha=100), add=T, freq=F)
#	legend("topright", legend=c("VB", "R"), fill=pal[1:2])
#	dev.off()
#	#
#	for(g in gear){
#		#
#		searchStr = sprintf("%s.%s*", s, g)
#		testSearch = grep(searchStr, testAll$strata)
#		agecomSearch = grep(searchStr, agecomAll$strata)
#		if( length(testSearch)==0 | length(agecomSearch)==0){ next }
#		#
#		testStrat = testAll[grep(searchStr, testAll$strata),'age']
#		agecomStrat = agecomAll[grep(searchStr, agecomAll$strata),'age']
#		mm = min(c(testStrat, agecomStrat))
#		MM = max(c(testStrat, agecomStrat))
#		#
#		png(sprintf("%s%s_%s.png", place, s, g))
#		hist(testStrat, breaks=mm:MM, col=makeTransparent(pal[1], alpha=100), xlab="Length", main=sprintf("%s %s", s, g), freq=F)
#		hist(agecomStrat, breaks=mm:MM, col=makeTransparent(pal[2], alpha=100), add=T, freq=F)
#		legend("topright", legend=c("VB", "R"), fill=pal[1:2])
#		dev.off()
#		#
#		for(sx in sexs){
#			#
#			searchStr = sprintf("%s.%s......%s", s, g, sx)
#			testSearch = grep(searchStr, testAll$strata)
#			agecomSearch = grep(searchStr, agecomAll$strata)
#			if( length(testSearch)==0 | length(agecomSearch)==0){ next }
#			#
#			testStrat = testAll[grep(searchStr, testAll$strata),'age']
#			agecomStrat = agecomAll[grep(searchStr, agecomAll$strata),'age']
#			mm = min(c(testStrat, agecomStrat))
#			MM = max(c(testStrat, agecomStrat))
#			#
#			png(sprintf("%s%s_%s_%s.png", place, s, g, sx))
#			hist(testStrat, breaks=mm:MM, col=makeTransparent(pal[1], alpha=100), xlab="Length", main=sprintf("%s %s %s", s, g, sx), freq=F)
#			hist(agecomStrat, breaks=mm:MM, col=makeTransparent(pal[2], alpha=100), add=T, freq=F)
#			legend("topright", legend=c("VB", "R"), fill=pal[1:2])
#			dev.off()
#		}
#	}
#}









##dev.new()
##
#	testStrat = testAll[grep("BCAC.TWL*", testAll$strata),'flength']
#	lencomStrat = lencomAll[grep("BCAC.TWL*", lencomAll$strata),'flength']
#	mm = min(c(testStrat, lencomStrat))
#	MM = max(c(testStrat, lencomStrat))
#	png(sprintf("%s%s.png", place, s))
#	hist(testStrat, breaks=mm:MM, col=makeTransparent(pal[1], alpha=100), xlab="Length", main=sprintf("%s", s))
#	hist(lencomStrat, breaks=mm:MM, col=makeTransparent(pal[2], alpha=100), add=T)
#	dev.off()
##plot( density(testAll[grep("*TWL*", testAll$strata), 'flength']) )
##polygon( density(testAll[grep("*TWL*", testAll$strata), 'flength']) , col='grey')
#dev.new()
#hist(testAll[grep("*HKL*", testAll$strata), 'flength'])
#dev.new()
#hist(testAll[grep("*NET*", testAll$strata), 'flength'])




##
#boxplot(flength~strata, data=lencomAll, outline=F, ylim=c(0, 150), main="Mine")
#dev.new()
#boxplot(flength~strata, data=testAll, outline=F, ylim=c(0, 150), main="Don")










##                             $one
#fishProp = lencom #aggregate(lencom$ex, by=lencom[,c('species', 'live', 'gear', 'port', 'mcat', 'flength', 'sex')], FUN=sum)
#fishSum = aggregate(lencom$x, by=lencom[,c('species', 'disp', 'gear', 'port', 'mcat', 'sex')], FUN=sum)
#colnames(fishSum) = c('species', 'disp', 'gear', 'port', 'mcat', 'sex', 'sum')
#fishProp = merge(fishProp, fishSum)
#fishProp$prop = fishProp$x/fishProp$sum
#fishProp = subset(fishProp, select = c(-x,-sum))
#colnames(fishProp) = c('species', 'disp', 'gear', 'port', 'mcat', 'sex', 'flength', 'x')
#
#
#
##
#fuzzyMatch = function(exp1, exp2, q=0.025){
#        #
#        exp1$x1 = exp1$x
#        exp2$x2 = exp2$x
#        #
#        exp1 = subset(exp1, select = -x)
#        exp2 = subset(exp2, select = -x)
#        #
#        out = merge(exp1, exp2, all=T)
#        out$percentError = (out$x1-out$x2)/out$x1
#        #
#        return(out)
#}




##
#out = fuzzyMatch(test, lencom)
#
###
##dev.new()
##plot(log(out$x1), log(out$x2), xlab="Dons", ylab="Mine", main="Expanded")
##abline(0, 1, col='red')
#
###
##out1 = out[is.na(out$x1),]
##out2 = merge(lencom, out1)
##
#
##
#testSum = aggregate(test$x, by=test[,c('species', 'disp', 'gear', 'port', 'mcat', 'sex')], FUN=sum)
#colnames(testSum) = c('species', 'disp', 'gear', 'port', 'mcat', 'sex', 'sum')
#testProp = merge(test, testSum)
#testProp$prop = testProp$x/testProp$sum
#testProp = subset(testProp, select = c(-x, -sum))
#colnames(testProp) = c('species', 'disp', 'gear', 'port', 'mcat', 'sex', 'flength', 'x')
#
##
#outProp = fuzzyMatch(testProp, fishProp)
#
##
#dev.new()
#plot(log10(outProp$x1), log10(outProp$x2), xlab="Dons", ylab="Mine", main="Proportion", pch='.', xlim=c(-5, 0), ylim=c(-5, 0))
#abline(0, 1, col='red')
##rug(log10(outProp$x2[is.na(outProp$x1)]), side=2)
##rug(log10(outProp$x1[is.na(outProp$x2)]), side=1)
#
##options(width = 300)



##
#rProp = 0
#stratSam = samp[samp$live=='N' & samp$mcat==674 & samp$gear=='HKL' & samp$port=='MNT',]
#for(i in 1:nrow(stratSam)){
#	#
#	stratFish = sampfish[sampfish$sample_no==stratSam[i,'sample_no'],c('flength', 'sex')]
#	h = hist2(stratFish$flength, stratFish$sex, xbreaks=0:149+0.5, ybreaks=c(0.5,1.5,2.5,9.5), plot=F)
#	prop = h$z/sum(h$z, na.rm=T)
#	prop[is.na(prop)] = 0
#	#
#	rProp = rProp + prop*stratSam[i,'avgN']
#}



##NOTE: confirm that we don't want to include 'sex' here VVVVVVVVVVVVVVVV
#totCt = aggregate(sampfish$sample_no, by=sampfish[,c('sample_no', 'species')], FUN=length)
#colnames(totCt) = c('sample_no', 'species', 'totCt')
#
##NOTE: maybe not merge here
##samp = merge(samp, totCt)
#samphead = merge(samphead, totCt)
#
##sample stats
##average weight
#samphead$avgW = samphead$alsLbs/samphead$totCt
##average numbers
#samphead$avgN = samphead$totLbs/samp$avgW

#samp$avgW = samp$alsLbs/samp$totCt
##average numbers
#samp$avgN = samp$totLbs/samp$avgW
#
##
#rProp = 0
#stratSam = samp[samp$live=='N' & samp$mcat==674 & samp$gear=='HKL' & samp$port=='MNT',]
#for(i in 1:nrow(stratSam)){
#	#
#	stratFish = sampfish[sampfish$sample_no==stratSam[i,'sample_no'],c('flength', 'sex')]
#	h = hist2(stratFish$flength, stratFish$sex, xbreaks=0:149+0.5, ybreaks=c(0.5,1.5,2.5,9.5), plot=F)
#	prop = h$z/sum(h$z, na.rm=T)
#	prop[is.na(prop)] = 0
#	#
#	rProp = rProp + prop*stratSam[i,'avgN']
#}
	
##average weight
#samp$avgW = samp$alsLbs/samp$totCt
##average numbers
#samp$avgN = samp$totLbs/samp$avgW

##
#stratFish = sampfish[sampfish$sample_no%in%stratSam$sample_no,c('flength', 'sex')]
##
#h = hist2(stratFish$flength, stratFish$sex, xbreaks=0:149+0.5, ybreaks=c(0.5,1.5,2.5,9.5), plot=F) #xbreaks=0:150, ybreaks=c(0.5,1.5,2.5,9.5))
#prop = h$z/sum(h$z, na.rm=T)
#rProp = 0
#for(an in stratSam$avgN){ rProp = rProp + prop*an }



#
##sampled weight that is not ignored
#sampleWgt = aggregate(samp$alsLbs, by=list(samp$sample_no), FUN=sum)
#colnames(sampleWgt) = c('sample_no', 'sampleWgt')
#samp = merge(samp, sampleWgt, all.x=T)
#
##sampled species weight that is not ignored
#sampleSppWgt = aggregate(samp$alsLbs, by=samp[,c('sample_no', 'species')], FUN=sum)
#colnames(sampleSppWgt) = c('sample_no', 'species', 'sampleSppWgt')
#samp = merge(samp, sampleSppWgt, all.x=T)
#
##1. Determine how many fish each sampled fish represents
#samp$rep = samp$totLbs/samp$sampleWgt #round(samp$totLbs/samp$sampleWgt, 1)
#
##2. Calculate expanded landings of each species on the boat that was sampled that is not ignored
#samp$val = (samp$sampleSppWgt/samp$sampleWgt)*samp$totLbs
##sum across samples in each strata
#sumVal = aggregate(samp$val, by=samp[,c('port', 'gear', 'live', 'mcat', 'species')], FUN=sum) #samp[,c('sample_no', 'species')], FUN=sum)
#colnames(sumVal) = c('port', 'gear', 'live', 'mcat', 'species', 'sumVal')
#samp = merge(samp, sumVal, all.x=T)
#
##4. Determine how many fish are represented by each sampled fish
#samp$ex = (samp$rep*(samp$expLands/samp$sumVal)) #NOTE: Don explicitly rounds here
#samp$one = 1 
#
##
#lencomAll = merge(samp, sampfish)
#lencom = aggregate(lencomAll$ex, by=lencomAll[,c('species', 'live', 'gear', 'port', 'mcat', 'flength', 'sex')], FUN=sum)
##			      $one
#fishProp = aggregate(lencomAll$ex, by=lencomAll[,c('species', 'live', 'gear', 'port', 'mcat', 'flength', 'sex')], FUN=sum)
#fishSum = aggregate(lencomAll$ex, by=lencomAll[,c('species', 'live', 'gear', 'port', 'mcat', 'sex')], FUN=sum)
#colnames(fishSum) = c('species', 'live', 'gear', 'port', 'mcat', 'sex', 'sum')
#fishProp = merge(fishProp, fishSum)
#fishProp$prop = fishProp$x/fishProp$sum
#fishProp = subset(fishProp, select = c(-x,-sum))
#colnames(fishProp) = c('species', 'live', 'gear', 'port', 'mcat', 'sex', 'flength', 'x')
#
##
##TEST
##
#
##
#fuzzyMatch = function(exp1, exp2, q=0.025){
#	#
#	exp1$x1 = exp1$x
#	exp2$x2 = exp2$x
#	#
#	exp1 = subset(exp1, select = -x)
#	exp2 = subset(exp2, select = -x)
#	#
#	out = merge(exp1, exp2, all=T)
#	out$percentError = (out$x1-out$x2)/out$x1
#	#
#	return(out)
#}
#
##
#test = read.table("../CALCOM_LENGTH_EXPANSION_2020_Nick-EJexample/files after EXPAND/lencom20.txt", 
#	fill=T,
#	sep=""
#)
##test = test[test$V10=='ACTUAL']
#test = test[,-1]
#test = test[,-9]
#colnames(test) = colnames(lencom)
#test$mcat = as.character(test$mcat)
#
##
#out = fuzzyMatch(test, lencom)
#
##
#dev.new()
#plot(log(out$x1), log(out$x2), xlab="Dons", ylab="Mine", main="Expanded")
#abline(0, 1, col='red')
#
###
##out1 = out[is.na(out$x1),]
##out2 = merge(lencomAll, out1)
##
#
#
##
#testSum = aggregate(test$x, by=test[,c('species', 'live', 'gear', 'port', 'mcat', 'sex')], FUN=sum)
#colnames(testSum) = c('species', 'live', 'gear', 'port', 'mcat', 'sex', 'sum')
#testProp = merge(test, testSum)
#testProp$prop = testProp$x/testProp$sum
#testProp = subset(testProp, select = c(-x, -sum))
#colnames(testProp) = c('species', 'live', 'gear', 'port', 'mcat', 'sex', 'flength', 'x')
#
##
#outProp = fuzzyMatch(testProp, fishProp)
#
##
#dev.new()
#plot(log10(outProp$x1), log10(outProp$x2), xlab="Dons", ylab="Mine", main="Proportion", pch='.')
#abline(0, 1, col='red')
#rug(log10(outProp$x2[is.na(outProp$x1)]), side=2)












##
#dev.new() 
#hist(log(outProp$x2[is.na(outProp$x1)]))


#isAllSamp = (outCount$x1/outCount$x2)%%1!=0
#isAllSamp[is.na(isAllSamp)] = F
#out3 = out[isAllSamp,]



##alsCt
#oneDenom = aggregate(sampfish$sample_no, by=list(sampfish$sample_no), FUN=length)
#colnames(oneDenom) = c('sample_no', 'oneDenom')
##add alsCt to sampHead
#samphead = merge(samphead, oneDenom)

##alsCt
#alsCt = aggregate(sampfish$sample_no, by=sampfish[,c('sample_no', 'species')], FUN=length)
#colnames(alsCt) = c('sample_no', 'species', 'alsCt')
##add alsCt to sampHead
#samphead = merge(samphead, alsCt)

##add samphead to lenuse, I need to merge on the borrStrata not the base
#samp = lenuse #lenuse[lenuse$alsSource=='A',]

##
#sampIg = samp[samp$alsSource!='I',]
##
#ignrNoSpp = aggregate(sampIg$expLands , by=sampIg[,c('live', 'gear', 'port', 'mcat')], FUN=sum)
#sampNoSpp = aggregate(samp$expLands , by=samp[,c('live', 'gear', 'port', 'mcat')], FUN=sum)
##
#colnames(ignrNoSpp) = c('live', 'gear', 'port', 'mcat', 'expLandsSumIG')
#colnames(sampNoSpp) = c('live', 'gear', 'port', 'mcat', 'expLandsSum')
#sampNoSpp = merge(sampNoSpp, ignrNoSpp, all.x=T)
#samp = merge(samp, sampNoSpp, all.x=T)
#samp$expExpLands = samp$expLandsSum*samp$expLands/samp$expLandsSumIG
#
##NOTE: try combining the expLands from ignored species in a strata, and removing the alsLbs from the sum_val calculation
##lenuse[lenuse$port=="MNT" & lenuse$gear=='HKL' & lenuse$mcat==674,]
###lenuse[lenuse$port=='OLA' & lenuse$gear=='HKL' & lenuse$mcat=='256',]



##1. Determine how many fish each sampled fish represents (rep)
#denom = aggregate(samp$alsLbs, by=samp[,c('sample_no', 'alsSource')], FUN=sum)
##the name 'sumoutSppClust' is the denominator from steps 1. and 2. that sums out spp and cluster from alsLbs within a sample_no
#colnames(denom) = c('sample_no', 'intSource', 'oneDenom')
#samp = merge(samp, denom[denom$intSource=='A',-2], all.x=T)
#samp$rep = samp$totLbs/samp$oneDenom
##NOTE: rep should be greater than one since you can not sample more than the entire landing, I will be enforcing this
#samp$rep[samp$rep<1] = 1
#
##2. Calculate lbs of each species in each sample
#numer = aggregate(samp$alsLbs, by=samp[,c('sample_no', 'species', 'alsSource')], FUN=sum) 
##the name 'sumoutClust' is the numerator from step 2. that sums out cluster from alsLbs within a sample_no
#colnames(numer) = c('sample_no', 'species', 'intSource', 'twoNumer')
#samp = merge(samp, numer[numer$intSource=='A', -3], all.x=T)
##sum_val
#samp$sum_val = (samp$twoNumer/samp$oneDenom)*samp$totLbs #(samp$sumoutClust/samp$sumoutSppClust)*samp$totLbs
#
##4. Determine how many fish are represented by each sampled fish
#sumSumVal = aggregate(samp$sum_val, by=list(samp$port, samp$gear, samp$live, samp$mcat, samp$species, samp$alsSource), FUN=sum)
#colnames(sumSumVal) = c('port', 'gear', 'live', 'mcat', 'species', 'intSource', 'sumSumVal')
#samp = merge(samp, sumSumVal[sumSumVal$intSource=='A',-6])
##length
##NOTE: OH GOD why so much rounding 
#samp$exfac = round(samp$rep*samp$expLands/samp$sumSumVal)
##samp$exfac = round(samp$rep*samp$expExpLands/samp$sumSumVal)

###3. Adjust for unaged fish (adj)
##samp$exfac = (samp$totCt/samp$alsCt)*samp$exfac
##
##samp$adj = (samp$totCt/samp$alsCt)*samp$rep
##age only
##samp$exfac = samp$adj*(samp$expLands/samp$sumSumVal)
##samp$exfac = (samp$totCT/samp$alsCT)*samp*exfac
#
##NOTE:will need to get totCT for age expansion
###species, length, age, sex level ct [sample_no identifies a strata (samples can only come from one strata)]
##ct = aggregate(sampfish, by=list(sampfish$sample_no, sampfish$species, sampfish$flength, sampfish$age, sampfish$sex), FUN=length)
##colnames(ct) = c('sample_no', 'species', 'flength', 'age', 'sex', 'ct')
##ct = ct[,c('sample_no', 'species', 'flength', 'age', 'sex', 'ct')]
###add ct to samp
##samp = merge(samp, ct)
#
##
#lencom = merge(samp, sampfish)
#lencom = aggregate(lencom$exfac, by=lencom[,c('species', 'live', 'gear', 'port', 'mcat', 'flength', 'sex')], FUN=sum)
#
###
###TEST
###
##
###
##expDiff = function(exp1, exp2){
##        #
##        exp2Needs = suppressMessages(anti_join(exp1, exp2))
##        exp1Needs = suppressMessages(anti_join(exp2, exp1))
##        #matchup diffs into a single data struture somehow
##        return(list(twoNeeds=exp2Needs, oneNeeds=exp1Needs))
##}
##
####
###test = read.table("../CALCOM_LENGTH_EXPANSION_2020_Nick-EJexample/files after EXPAND/lenuse.txt", 
###	fill=T,
###	sep=""
###)
###test[test[,9]=='IIIIIIIIIIIIIIIIII', c(9,10,11,12,13,14)] = 'I'
###colnames(test) = colnames(lenuse)
###
####
###needs = expDiff(lenuse, test)
####NOTE: with one port borrowing BRG is two ports away from OSF
####$twoNeeds
####  species live gear port mcat  source  lbs lenct borrSpecies borrLive borrGear borrPort borrMcat lenSource
####1    BLGL    N  TWL  OSF  667 NOMINAL 1696     0           I        I        I        I        I         I
####
####$oneNeeds
####  species live gear port mcat  source  lbs lenct borrSpecies borrLive borrGear borrPort borrMcat lenSource
####1    BLGL    N  TWL  OSF  667 NOMINAL 1696     0        BLGL        N      TWL      BRG      667         B
###
#####NOTE: UDAB should be excluded by the notSpp filtering step
####$oneNeeds
####  species live gear port mcat  source  lbs lenct borrSpecies borrLive borrGear borrPort borrMcat lenSource
####2    UDAB    N  HKL  OLA  225  ACTUAL  415    24        UDAB        N      HKL      OLA      225         A





#nc = nrow(sampstrat3CTSumQtr)
##lingcod
##NOTE: handle the case of 195?? I think or whatever that special case was
#sourceDF$source[sourceDF$mcat==195] = 'N'
##is there a reason not to hard code this? might it change in time?
##sablefish is hardcoded. should it be?
##if they dont take Actual samples of sablefish it can remain down there, otherwise it should probably go up with lingcod
#sourceDF = data.frame(sampstrat3CTSumQtr[,c('disp', 'port', 'gear', 'mcat')], source=character(nc), borrPort=character(nc), borrGear=character(nc), borrMcat=character(nc))

#
##ahead of everything this happens and skips the rest
#sourceDF$source[sampstrat3TotSumQtr$x<totNomThresh] = 'N'
#sourceDF$source[sampstrat3CTSumQtr$x>0] = 'A'
##nominal gears
#sourceDF$source[sourceDF$gear=='OTH'] = 'N'
#sourceDF$source[sourceDF$gear=='FPT'] = 'N'
#sourceDF$source[sourceDF$gear=='UNK'] = 'N'
##sablefish are nominal
#sourceDF$source[sourceDF$mcat==190] = 'N'
##nominal port
#sourceDF$source[sourceDF$port=='Nick Says Other'] = 'N'


