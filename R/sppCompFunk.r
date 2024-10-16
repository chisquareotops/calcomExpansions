#
#DEPENDENCIES
#

#RJDBC requires the driver files for each database to function:
#       MS-SQL  : ./sqljdbc4.jar
#       Oracle  : ./ojdbc8.jar
#
#upon installing RJDBC you may need to run "R CMD javareconf" command in the 
#terminal as root to add Java support to R.

#suppressMessages(library(RJDBC, quietly=FALSE)) #JDBC, dbSendUpdate, maybe DBI: dbConnect, dbExistsTable, dbWriteTable
#suppressMessages(library(lares, quietly=FALSE)) #warnifnot
#suppressMessages(library(dplyr, quietly=FALSE)) #appearantly not used
#suppressMessages(library(getPass, quietly=FALSE)) #getPass

#
#ANCILLARY FUNCTIONS
#

#' A function primarily for internal use that encodes a strata as Don's magic number coding.
#'
#' @param disp disposition
#' @param port three letter port code
#' @param gear three letter gear code
#' @param mcat numeric market category
#' @param qtr numeric quarter
makeStratCode = function(disp, port, gear, mcat, qtr){
        #
        dispKey = c("Y", "N")
        othrY = c("P")
        if(disp %in% othrY){ disp="Y" }
        #
        portKey = c("CRS", "ERK", "BRG", "BDG", "OSF", "MNT", "MRO", "OSB", "OLA", "OSD", "Nick Says Other")
        othrPRT = c("UNK", "OCA")
        if( port %in% othrPRT ){ port="Nick Says Other" } #just a string I know is not otherwise in the data
        #       
        gearKey  = c("HKL", "FPT", "TWL", NA, NA, NA, "NET", "OTH", "UNK")
        othrTWL  = c("DNT", "FTS", "MDT", "GFS", "GFL")
        if( gear %in% othrTWL ){ gear="TWL" }
        #
        magicNumber = which(dispKey==disp)*1e7 + which(portKey==port)*1e5 + which(gearKey==gear)*1e4 + as.numeric(mcat)*10 + as.numeric(qtr)
        return( magicNumber )
}
makeStratCode = Vectorize(makeStratCode,  vectorize.args=c('disp', 'gear', 'port', 'mcat', 'qtr'))

#' A function primarily for internal use that decodes the strata from Don's magic number coding.
#'
#' @param code Don's magic number
makeCodeStrat = function(code){
        #
        dispKey = c("Y", "N")
        gearKey  = c("HKL", "FPT", "TWL", NA, NA, NA, "NET", "OTH", "UNK")
        portKey = c("CRS", "ERK", "BRG", "BDG", "OSF", "MNT", "MRO", "OSB", "OLA", "OSD", "Nick Says Other")
 #magicNumber = which(dispKey==disp)*1e7 + which(portKey==portC)*1e5 + which(gearKey==gear)*1e4 + as.numeric(mcat)*10 + qtr
        out = data.frame(disp=dispKey[code%/%1e7], port=portKey[code%/%1e5%%100], gear=gearKey[code%/%1e4%%10], mcat=code%/%10%%1000, qtr=code%%10)
        return( out )
}

#' A function primarily for internal use that converts three letter port codes to pacfin port codes.
#'
#' @param port three letter port code
pacfinPortCode = function(port){
	#
	numPortKey = c("ERK", "BRG", "BDG", "OSF", "MNT", "MRO", "CRS")
	letPortKey = c("OSB", "OLA", "OSD")
	#
	num = which(numPortKey==port)
	let = LETTERS[-2][which(letPortKey==port)]
	#
	return( c(num, let, 7)[1] )
}

#' A function primarily for internal use that compares two expansion objects.
#'
#' @param exp1 An object that looks like an expansion.
#' @param exp2 An object that looks like an expansion.
#' @param by Which columns to join on.
expDiff = function(exp1, exp2, by=NULL){
	#
	exp2Needs = suppressMessages(dplyr::anti_join(exp1, exp2, by=by))
	exp1Needs = suppressMessages(dplyr::anti_join(exp2, exp1, by=by))
	#matchup diffs into a single data struture somehow
	return(list(twoNeeds=exp2Needs, oneNeeds=exp1Needs))
}

#
#DATABASE FUNCTIONS
#

#' A function primarily for internal use that finds path of the package install. Built for use in referencing the driver location.
getSelfPath = function(){
	paths = file.path(.libPaths(), "calcomExpansions")
	paths[dir.exists(paths)][1]
}

#' A function primarily for internal use that checks to make sure the database drivers are in place. Requires an internet connection to download.
getDrivers = function(){
	#check is driver exist
        if( !all(file.exists("sqljdbc4.jar", "ojdbc8.jar")) ){
		#
		writeLines("\nDownloading Database Drivers...")
		#if not download them into the current working ditrectory. This is where the rest of the code expects them. Any code that needs them will probably need an internet connection anyway
		utils::download.file(
			url="https://github.com/chisquareotops/calcomExpansions/raw/main/inst/drivers/sqljdbc4.jar",
			destfile = "sqljdbc4.jar"
		)
		utils::download.file(
			url="https://github.com/chisquareotops/calcomExpansions/raw/main/inst/drivers/ojdbc8.jar",
			destfile = "ojdbc8.jar"
		)
	}
}

#' A function primarily for internal use that performs a default backup of COM_LANDS and COMP_EXPAND_DOCS
#' 
#' @param con 	An RJDBC connection to an SQL database
#' @param dbName A table name to backup, from the given database, given as a string.
#' @param force	A boolean indicating if a backup should be overwrite an existing 
#'	backup made on the same day. By default this function only makes one 
#'	backup a day, unless run with force=True to force an overwrite of the 
#'	current day's backup.  
backup = function(con, dbName, force=F){ #overwrite?
	#backup year(s) into one dedicated temp_file
		#one backup a day, only do a backup is one does not already exist. 
		#separate functionality ot force a backup, but also check for a daily backup and 
		#dbSendQuery #NOTE: from video? <<<< VVVVV
		#xxx_COM_LANDS_BAK_YYYYMMDD
		#select * into xxx_COM_LANDS_BAK_YYYYMMDD from com_lands 
		#xxx_COMP_EXPAND_DOCS_BAK_YYYYMMDD

	#
	dateStamp = format(Sys.time(), "%Y%m%d")  #"20190416" #
	backupName = sprintf("xxx_%s_BAK_%s", dbName, dateStamp)	
	
	#check is backup exists
	if( !RJDBC::dbExistsTable(con, backupName) ){	
		#do backup
		RJDBC::dbSendUpdate(con, sprintf("select * into %s from %s", backupName, dbName))
		
		#report that backup was done
		return(T)
	}
	
	#force a backup
	if( force ){
		#a backup may or not exists when forcing
		if( RJDBC::dbExistsTable(con, backupName) ){ 
			RJDBC::dbSendUpdate(con, sprintf("DROP TABLE %s", backupName)) 
		}
		
		#do backup
                RJDBC::dbSendUpdate(con, sprintf("select * into %s from %s", backupName, dbName))
		
		#report that backup was done
                return(T)
	}
	
	#report that back was not done
	return(F)
}

#' Collect and prepare Pacfin data for species expansion.
#'
#' @param year	The year of the expansion given as landing_year is expressed in 
#'	the pacfin_marts.comprehensive_ft table. Year may be given as a vector to 
#'	prepare data for a multi-year expansion.
#' @param save	A filename.csv to save data to file or a logical. If save=True 
#'	the filename defalts to sprintf('pacfin%sSppData%s', year, Sys.time()); 
#'	save=False (Default) does not save.
#' @param fromFile A filename.csv to read data from file or a logical. 
#'	fromFile=True defaults to the most recent sprintf('pacfin%sSppData*', year) 
#'	file; fromFile=False (Default) reads data from pacfin.
#'
#' @return Returns a data.frame of the neccessary fish ticket data from pacfin 
#'	to perform species expansions for the given years
getPacfinSppData = function(year, save=F, fromFile=F){
	#year     : the year of the expansion given as landing_year is expressed in the pacfin_marts.comprehensive_ft table. (e.g. year=2019 at the time of coding this in 2022)
	#save	  : a filename.csv to save data to file or a logical.
	#		True defaults to the filename sprintf('pacfin%sSppData%s', year, Sys.time()); False (Default) does not save.
	#fromFile : a filename.csv to read data from file or a logical.
	#		True defaults to the most recent sprintf('pacfin%sSppData*', year) file; False (Default) reads data from pacfin.
	#
	#value	  : returns fish ticket data from pacfin
	
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
			possibleFiles = list.files(path='.', pattern=glob2rx(sprintf('pacfin%sSppData*', years)))
			stopifnot( length(possibleFiles)>0 )
			#
			firstPart = sprintf("pacfin%sSppData", years)
			dates = as.POSIXlt(sub(firstPart, "", possibleFiles))
			fromFile = sprintf("%s%s.csv", firstPart, max(dates))	
		}
		#
		writeLines(sprintf("\nReading PacFIN Species Data From %s...", fromFile))
		#read in pacfinTix
		pacfinTix = read.csv(fromFile)
	#otherwise get from database
	}else{
		#
		flag = T
		while( flag ){
			#
			flag = tryCatch({		
				# Create Oracle connection driver and open connection to PacFIN
				# PacFIN is an Oracle sql server on the NOAA VPN (Not the psmfc VPN)
				# ojdbc8.jar downloaded from https://www.oracle.com/database/technologies/appdev/jdbc-downloads.html
				#oDrv = RJDBC::JDBC(driverClass='oracle.jdbc.OracleDriver', classPath='./ojdbc8.jar', identifier.quote="'")
				#oDrv = RJDBC::JDBC(driverClass='oracle.jdbc.OracleDriver', classPath=file.path(dPath, "drivers", "ojdbc8.jar"), identifier.quote="'")
				oDrv = RJDBC::JDBC(driverClass='oracle.jdbc.OracleDriver', classPath=system.file("drivers/ojdbc8.jar", package="calcomExpansions"), identifier.quote="'")
				#PacFIN connection
				writeLines("\nReading PacFIN Species Data From PacFIN Connection...")
				#template connection string:"jdbc:oracle:thin:@//database.hostname.com:port/service_name_or_sid"
				name =  getPass::getPass('PacFIN User: ')
				password  = getPass::getPass('Password: ')
				oCon = RJDBC::dbConnect(oDrv, 'jdbc:oracle:thin:@//pacfindb.psmfc.org:2045/pacfin.psmfc.org', name, password) #getPass::getPass('PacFIN User: '), getPass::getPass('Password: ')) 
				
				#
				pacfinTix = c()
				for(y in year){
					pacfinTix = rbind(pacfinTix, RJDBC::dbGetQuery(oCon,
					       sprintf("
					               select 
					                       landing_year            as yr, 
					                       landing_month           as mon, 
					                       pacfin_port_code        as port, 
					                       species_code            as mcat, 
					                       condition_code          as cond, 
					                       pacfin_gear_code        as gear, 
					                       sum(landed_weight_lbs)  as lbs
					               
					               from pacfin_marts.comprehensive_ft
					               
					               where 
					                       agency_code='C'         and 
					                       landing_year=%d         and 
					                       species_code in (147,150,152,153,154,165, 166,170,171,173,174,175,176,177,178, 190,195,198,200,201,202,203,204,205,206,207,208,209,210,211,225,226,227,228,230,231,235,236,237,238,239,240,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,267,268,269,270,271,289,290, 650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,956,957,958,959,960,961,962,963,964, 970,971,972,973,974,975,976)
					               
					               group by 
					                       landing_year, 
					                       landing_month, 
					                       pacfin_port_code, 
					                       condition_code, 
					                       species_code, 
					                       pacfin_gear_code
					       ", y) #year)
					))
				}
				#exit loop when this eventually doesn't error
				flag = F
			}, error=function(err){
				#
				print(err)
				#NOTE: agency IP adress instead of NOAA
				readline("\nDo you have a NOAA IP address? Join NOAA VPN.\n(Ctrl-C to Escape -or- Enter to Try Again)")
				writeLines('')
				#
				flag = T
			})
		}
		
		#clean data: trim white space & cast as all uppercase
		pacfinTix$GEAR = toupper(trimws(pacfinTix$GEAR, 'both'))
		pacfinTix$PORT = toupper(trimws(pacfinTix$PORT, 'both'))
		pacfinTix$MCAT = toupper(trimws(pacfinTix$MCAT, 'both'))
		pacfinTix$COND = toupper(trimws(pacfinTix$COND, 'both'))	
	
		#
		if( save!=F ){
			years = paste(unique(year), collapse='')
			#if save=T make the name the date
			if( save==T ){ save=sprintf('pacfin%sSppData%s.csv', years, as.POSIXlt(Sys.time())) }
			#write the data to file.
			write.csv(pacfinTix, save, row.names=F, quote=F)
		}
	}	
	
	#
	return( pacfinTix )
}

#' Collect and prepare Calcom data for species expansion.
#'
#' @param year	The year of the expansion given as a four digit integer. Year may 
#'	be given as a vector to prepare data for a multi-year expansion.
#' @param save	A filename.csv to save data to file or a logical. If save=True 
#'	the filename defalts to sprintf('calcom%sSppData%s', year, Sys.time()); 
#'	save=False (Default) does not save.
#' @param fromFile A filename.csv to read data from file or a logical. 
#'	fromFile=True defaults to the most recent sprintf('pacfin%sData*', year) 
#'	file; fromFile=False (Default) reads data from calcom.
#'
#' @return Returns a list of the various objects from calcom needed to compute 
#'	a species expansions for the given years.
getCalcomSppData = function(year, save=F, fromFile=F){
	#year     : the year of the expansion. 
	#save	  : a filename.RData to save data to file or a logical.
	#		True defaults to the filename sprintf('calcom%sData%s', year, Sys.time()); False (Default) does not save.
	#fromFile : a filename.rda to read data from file or a logical.
	#		True defaults to the most recent sprintf('calcom%sData*', year) file; False (Default) reads data from calcom.
	#
	#value	  : returns a list of all of the various objects in calcom needed to compute an expansion in the given year.
	
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
			possibleFiles = list.files(path='.', pattern=glob2rx(sprintf('calcom%sSppData*', years)))
			stopifnot( length(possibleFiles)>0 )
			#
			firstPart = sprintf("calcom%sSppData", years)
			dates = as.POSIXlt(sub(firstPart, "", possibleFiles))
			fromFile = sprintf("%s%s.rda", firstPart, max(dates))	
		}
		#
		writeLines(sprintf("\nReading CALCOM Species Data From %s...", fromFile))
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
				#pull calcom data
				#       grid from pacfin_gear_codes
				#       pcid from pacfin_port_codes
				#       nominal_species from market_categories

				# Create microsoft sql connection driver and open connection to CALCOM
				# CALCOM is an MS-SQL server on the PSMFC VPN
				# sqljdbc4.jar file is required for creating the microsoft sql driver
				#mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'")
				#mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', file.path(dPath, "drivers", "sqljdbc4.jar"), identifier.quote="'")
				mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', system.file("drivers/sqljdbc4.jar", package="calcomExpansions"), identifier.quote="'")
				# CALCOM connection
				writeLines("\nReading CALCOM Species Data From CALCOM Connection...")                  #CALCOM_test
				mCon = RJDBC::dbConnect(mDrv, 'jdbc:sqlserver://sql2016.psmfc.org\\calcom;databaseName=CALCOM', getPass::getPass('CALCOM User: '), getPass::getPass('Password: '))
					
				#get calcom tables
				gearCodes = RJDBC::dbGetQuery(mCon,"select * from pacfin_gear_codes")
				portCodes = RJDBC::dbGetQuery(mCon,"select * from pacfin_port_codes")
				nmSpCodes = RJDBC::dbGetQuery(mCon,"select * from market_categories")
				#test = dbGetQuery(mCon,"select * from temp_lrcpt")
					
				#clean data: trim white space & cast as all uppercase
				gearCodes$GRID = toupper(trimws(gearCodes$GRID, 'both'))
				portCodes$PCID = toupper(trimws(portCodes$PCID, 'both'))
				nmSpCodes$mark_cat = toupper(trimws(nmSpCodes$mark_cat, 'both'))
				
				#
				temp1 = c()
				temp2 = c()
				mcat_list = c()
				for(y in year){
					#year%%100: uses modular arithmatic to get last two digits of the year.
					twoDigitYear = y%%100
		
					#1) is not a datbase call
					
					#2) select * from ann_samp_vu where right([AT]expandyr,2)=substring(samp_no,3,2)
					#Getting Sample Data by joining MASTER_SAMPLES and MASTR_CLUSTS
					temp1 = rbind(temp1, RJDBC::dbGetQuery(mCon, sprintf('select * from ann_samp_vu where %d=substring(samp_no,3,2)', twoDigitYear)) )
					
					#3) select * from samp_strat_vu where right([AT]expandyr,2)=yr order by port_complex, live_fish, gear_grp, mark_cat, qrtr
					#samp_strat_vu summarizes the number of samples (not including clusters) at each strata
                			#samp_strat_vu is only a function of MASTER_SAMPLES
					temp2 = rbind(temp2, RJDBC::dbGetQuery(mCon, sprintf('select * from samp_strat_vu where %d=yr order by port_complex, live_fish, gear_grp, mark_cat, qrtr', twoDigitYear)) )
					
					#4) select distinct mark_cat from master_samples where substring(sample_no,3,2)=right([AT]expandyr,2)
					#NOTE: say in english 
					#NOTE: Confirm that master_samples is not a function of temp_lrpt
					mcat_list = rbind(mcat_list, cbind(year=y, RJDBC::dbGetQuery(mCon, sprintf('select distinct mark_cat from master_samples where substring(sample_no,3,2)=%d', twoDigitYear)) ))
					
					#5) requires both pacfin and calcom data, thus this error checking is done in the expandSpCompToLandings() function 
				}
				#
				temp1$species = toupper(trimws(temp1$species, 'both'))
				#
				colnames(temp2)[6] = 'qtr'
				temp2$qtr = as.numeric(temp2$qtr)

				#
				species_codes = RJDBC::dbGetQuery(mCon, 'select * from species_codes')
				market_categories = RJDBC::dbGetQuery(mCon, 'select * from market_categories')
				
				#
				
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
			gearCodes = gearCodes, 
			portCodes = portCodes,
			nmSpCodes = nmSpCodes,
			temp1 = temp1,
			temp2 = temp2,
			mcat_list = mcat_list,
			species_codes = species_codes,
			market_categories = market_categories
		)
		
		#
		if( save!=F ){
			years = paste(unique(year), collapse='')
			#if save=T make the name the date
			if( save==T ){ save=sprintf('calcom%sData%s.rda', years, as.POSIXlt(Sys.time())) }
			#write the data to file.
			saveRDS(out, file=save)
		}
	}	
	
	#
	return( out )
}

#' A function to perform the various exporting tasks needed after a species expansion
#' 
#' @param exp   An expansion object created by estSppComp or estSppCompDoc to 
#'	be exported.
#' @param human A boolean indicating if the human feed should be written to 
#'	file (i.e. hfeedYY.csv).
#' @param pacfin A boolean indicating if the pacfin feed should be written to 
#'	file (i.e. pfeedYY.DAT).
#' @param calcom A boolean indicating if the the calcom database should be 
#'	backed up and updated with the given species expansion.
#' @param doc 	If calcom=True, a vector of doc filenames to be exported to 
#'	calcom.
#'
#' @return NULL See current directory and/or database.
exportSpp = function(exp, human=T, pacfin=T, calcom=F, doc=NULL){
	#exp    :
	#human  :
	#pacfin :
	#calcom :
	#doc    :
	#
	#value  :


	##vu filtering
	#exp = exp[exp$source!='N',] 
	#exp = exp[!exp$gear%in%c('UNK','OTH'),]	
	##
	#tot = aggregate(exp$x, by=exp[,c('year', 'qtr', 'disp', 'mcat', 'gear', 'port')], FUN=sum)
	#colnames(tot) = c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'comp')
	#exp = merge(exp, tot)
	#exp$comp = exp$x/exp$comp
	
	#
	sourceMatrix = c(A="ACTUAL", B="ACTUAL", C="BORROW T-1", D="BORROW T-2", N="NOMINAL", E="BORROW T-∞", F="BORROW T-∞", G="BORROW T-∞", H="BORROW T-∞", I="BORROW T-∞", J="BORROW T-∞", K="BORROW T-∞", L="BORROW T-∞", M="BORROW T-∞")
	
	#	
	years = sort(unique(exp$year))
	disps = sort(unique(exp$disp))
	qtrs  = sort(unique(exp$qtr))
	ports = unique(exp$port)
	gears = sort(unique(exp$gear))
	mcats = sort(unique(exp$mcat))

	#
	uNames = c('URCK', 'USHR', 'USLF', 'USLP', 'UDNR', 'RBR1', 'RCK3', 'RCK6', 'RCK8', 'RCK9')
	unknowns = data.frame(spp=uNames, lbs=rep(0, length(uNames))) #as.data.frame(matrix(0, ncol=length(uNames)))
	#
	for(y in years){
		#
		sppTotals = aggregate(exp$lands[exp$year==y], by=list(exp$spp[exp$year==y]), FUN=sum)
		colnames(sppTotals) = c('spp', 'lbs')
		uTotals = sppTotals[sppTotals$spp%in%uNames,]
		#
		unknowns = merge(unknowns, uTotals, by='spp', all=T)
		#print(unknowns)
		rownames(unknowns) = unknowns$spp
		unknowns = cbind( unknowns$spp, rowSums(unknowns[,c('lbs.x', 'lbs.y')], na.rm=T) )
		colnames(unknowns) = c(' spp', 'lbs')
		#
		write.table(unknowns, file=sprintf('unknowns%s.csv', y%%100), sep=',', quote=F, row.names=F, col.names=T)	
	}
			
	#
	if( pacfin ){
		#vu filtering
        	expM = exp[exp$source!='N',]
        	expM = expM[!expM$gear%in%c('UNK','OTH'),]
		#
		expM$pacfin = round(expM$comp*10000)
		#calculate the rounding correction for the largest spp
		sums = aggregate(expM$pacfin, by=expM[,c('year', 'qtr', 'disp', 'mcat', 'gear', 'port')], FUN=sum)
		sums$corr = sums$x-10000
		sums = subset(sums, select = -x)
		expM = merge(expM, sums)
		
		# 
		for(y in years){
			out = c()
			for(d in disps){
			for(q in qtrs ){
			for(p in ports){
			for(g in gears){
			for(m in mcats){
				#
				strat = expM[
					expM$year==y &
					expM$disp==d &
					expM$qtr ==q &
					expM$port==p &
					expM$gear==g &
					expM$mcat==m
				,]
				if( nrow(strat)==0 ){ next }
				#alphebetize spp
				strat = strat[order(strat$spp),]
				#add trailing space to three letter spp codes
				strat$spp[strat$spp=="REX"] = "REX "
				strat$spp[strat$spp=="POP"] = "POP "
				#apply correction to largest spp
				
				MM = max(strat$pacfin)
				strat[strat$pacfin==MM,'pacfin'] = strat[strat$pacfin==MM,'pacfin'] - strat[strat$pacfin==MM,'corr']
				#deal with gear
				if( g=='FPT' ){ g="POT" } 
				if( g%in%c('MDT', 'FTS', 'GFS', 'GFL') ){ g="TWL" }	
				
				#
				#int(2) year
				#char(1) live
				#int(1) qtr
				#int(1)/char(1) port
				#char(1) gear
				#int(3) mcat
				#1 
				#char(4)int(5) namelbs order species alphabetically
				oString = sprintf("%2d%1s%1d%1s%3s%3d1", y%%100, d, q, pacfinPortCode(p), g, m)
				for(i in 1:nrow(strat)){
					#
					sString = sprintf("%4s%5d", strat$spp[i], strat$pacfin[i])
					oString = sprintf("%s%s", oString, sString)
				}
				#
				out = c(out, oString)	
			}}}}}	
			
			#
			write.table(out, file=sprintf('pfeed%2d.DAT', y%%100), quote=F, row.names=F, col.names=F)
		}
	}
	
	#
	if( human ){
		#
		for(y in years){
			#
			write.csv(exp[exp$year==y,c('year','qtr','disp','mcat','gear','port','source','spp','lands','comp')], file=sprintf('hfeed%2d.csv', y%%100), quote=F, row.names=F)
		}
	}
	
	#propogate calcom SQL tables: 
	if( calcom ){
		#
		if( length(doc)!=length(years) ){
			stop(sprintf("\n\tdoc filename must be specified for each year expanded when calcom==T"))
		}
		#
		flag = T
		while( flag ){
			#
			flag = tryCatch({		
				# Create microsoft sql connection driver and open connection to CALCOM
				# CALCOM is an MS-SQL server on the PSMFC VPN
				# sqljdbc4.jar file is required for creating the microsoft sql driver
				#mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'")
				#mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', file.path(dPath, "drivers", "sqljdbc4.jar"), identifier.quote="'")
				mDrv = RJDBC::JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', system.file("drivers/sqljdbc4.jar", package="calcomExpansions"), identifier.quote="'")
				# CALCOM connection
				writeLines("\nWriting Expansion to CALCOM Connection...")                  #CALCOM_test
				mCon = RJDBC::dbConnect(mDrv, 'jdbc:sqlserver://sql2016.psmfc.org\\calcom;databaseName=CALCOM', getPass::getPass('CALCOM User: '), getPass::getPass('Password: '))
				
				#
				#dbWriteTable(ch, "bigNewIPTest", dat, row.names=F, overwrite=T)
				#dbWriteTable(ch, "bigNewIPTest", dat, row.names=F, overwrite=F, append=T)
				#
				#backup year(s) into one dedicated temp_file
					#one backup a day, only do a backup if one does not already exist. 
					#separate functionality to force a backup
					#dbSendQuery 
					#xxx_COM_LANDS_BAK_YYYYMMDD
					#select * into xxx_COM_LANDS_BAK_YYYYMMDD from com_lands 
					#xxx_COMP_EXPAND_DOCS_BAK_YYYYMMDD
				#delete previous data
					#delete only given years?	
				#write new year(s) data 
					#dbWriteTable( , row.names=F, append=T, overwrite=F)
					#close(mCon)
				
				#before we do anything with the database backup!
				backup(mCon, 'COM_LANDS')
				backup(mCon, 'COMP_EXPAND_DOCS')
				
				#read in docs here to separate it from potentially interfering with writting to the database.
				docs = list()
				for(i in 1:length(years)){
					#
					docs[[years[i]]] = read.csv(doc[i])[,c('yearAct', 'qtrAct', 'dispAct', 'mcatAct', 'gearAct', 'portAct', 'dispUse', 'mcatUse', 'gearUse', 'portUse', 'qtrUse', 'ctUse','yearUse')]
					colnames(docs[[years[i]]]) = c('year', 'real_quarter', 'real_live', 'real_mark_cat', 'real_gear_grp', 'real_port_complex', 'used_live', 'used_mark_cat', 'used_gear_grp', 'used_port_complex', 'used_quarter', 'sample_count', 'used_year')
				}
				
				#update SQL
                		for(y in years){					
					#
					toComLands = exp[exp$year==y, c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'spp', 'lands', 'source')]
					colnames(toComLands) = c('YEAR', 'QUARTER', 'LIVE', 'MARK_CAT', 'GEAR_GRP', 'PORT_COMPLEX', 'SPECIES', 'POUNDS', 'SOURCE')
					toComLands = toComLands[toComLands$SPECIES!='OMIT',]
					toComLands$SPECIES_GRP = NA
					#
					toComLands = toComLands[toComLands$POUNDS>=1,]
					toComLands$SOURCE = sourceMatrix[toComLands$SOURCE]	
					
					#COM_LANDS
					RJDBC::dbSendUpdate(mCon, sprintf("DELETE from COM_LANDS where year=%s", y) )
					RJDBC::dbWriteTable(mCon, "COM_LANDS", toComLands, row.names=F, append=T, overwrite=F)
					
					#COMP_EXPAND_DOCS
					RJDBC::dbSendUpdate(mCon, sprintf("DELETE from COMP_EXPAND_DOCS where year=%s", y) )
					RJDBC::dbWriteTable(mCon, "COMP_EXPAND_DOCS", docs[[y]], row.names=F, append=T, overwrite=F)
					
					##
					#unknowns = c('URCK', 'USHR', 'RCK3', 'RCK6', 'RBR1', 'RCK8', 'RCK9', 'USLF', 'USLP', 'UDNR')
					#writeLines(sprintf("URCK lbs = %s", sum(toComLands$POUNDS[toComLands$SPECIES=='URCK'])))
					##sum(toComLands$POUNDS[toComLands$SPECIES=='URCK'])
				}
						
				#exit loop when this eventually doesn't error
				flag = F
			}, error=function(err){
				#
				print(err)
				readline("\nPSMFC IP address? SQL permissions? Wrong password?\n(Ctrl-C to Escape -or- Enter to Try Again)")
				writeLines('')
				#
				flag = T
			})
		}
	}
	
	#NOTE: maybe return some sort of diagnostic information?
	return(NULL)
}

#
#EXPANSION
#

#' The core expansion function executing an automated species expansion of Don's 
#' Visual Basic code.
#' 
#' @param pacfinData	A data.frame as returned by getPacfinSppData.
#' @param calcomData 	A list as returned by getCalcomSppData.
#' @param portBorr  	A matrix to define the priority of port borrowing. 
#'	Rownames should indicate the port complex code of the actual stratum to 
#'	be filled. The first column contains the first priorty for borrowing 
#'	from, second column contains the second priority, ... etc. Elements 
#'	should be port complex codes as they appear in the samples. Elements not 
#'	given as appearing in the samples will code for Nominal.
#' @param qtrBorr	A matrix to define the priority of qtr borrowing.
#'	Rownames should define the actual qtr of the stratum to be filled.
#'	The first column contains the the first priority to borrowing from, 
#'	second column contains the second priority, ... etc. Elements should be 
#'	integers %in% 1:4. Elements not given %in% 1:4 will code for Nominal.
#' @param files		A boolean flag to produce verbose error files and/or 
#'	expansion output files such as sppdoc.
#'
#' @return a data.frame reminiscent of the calcom.com_lands table. 
estSppComp = function(pacfinData, calcomData, portBorr=portMatrix2, qtrBorr=qtrMatrix, files=T){
	#pacfinData : a data.frame as returned by getPacfinData
	#calcomData : a list as returned by getCalcomData
	#portBorr   : a matrix to define the priority of port borrowing. 
	#		rownames should indicate the port complex code of the actual stratum to be filled.
	#		the first column contains the first priorty for borrowing from, second column contains the second priority, ... etc. 
	#		elements should be port complex codes as they appear in the samples.
	#		elements not given as appearing in the samples will code for Nominal.
	#qtrBorr    : a matrix to define the priority of qtr borrowing.
	#		rownames should define the actual qrt of the stratum to be filled.
	#		the first column contains the the first priority to borrowing from, second column contains the second priority, ... etc. 
	#		elements should be integers %in% 1:4.
	#		elements not given %in% 1:4 will code for Nominal.
	#files 	    : a boolean flag to produce verbose error files and/or expansion output files such as sppdoc
	#
	#value      : 

	#UNPACK CALCOM
	
	#
	gearCodes = calcomData$gearCodes
	portCodes = calcomData$portCodes
	nmSpCodes = calcomData$nmSpCodes
	#
	species_codes = calcomData$species_codes
        market_categories = calcomData$market_categories

	#DEAL W/ YEAR

	#
	calcomT1Year = as.numeric(unique(substr(calcomData$temp1$samp_no, 1, 4)))
	calcomT2Year = as.numeric(unique(calcomData$temp2$yr))
	calcomMYear = unique(calcomData$mcat_list$year)
	pacfinYear = unique(pacfinData$YR)
	#4 choose 2 comparisons to make sure all the years are the same in all the data sources
	stopifnot( calcomT1Year%%100 %in% calcomT2Year )
	stopifnot( calcomMYear%%100 %in% calcomT2Year )
	stopifnot( pacfinYear%%100 %in% calcomT2Year )
	stopifnot( calcomT1Year %in% calcomMYear )
	stopifnot( calcomT1Year %in% pacfinYear )
	stopifnot( calcomMYear %in% pacfinYear )
	
	#
	expLand = c()
	for(year in pacfinYear){
		#
		temp1Y = calcomData$temp1[as.numeric(substr(calcomData$temp1$samp_no, 1, 4))==year,]
		temp2Y = calcomData$temp2[as.numeric(calcomData$temp2$yr)==year%%100,]
		mcat_listY = calcomData$mcat_list[calcomData$mcat_list$year==year,]
		pacfinDataY = pacfinData[pacfinData$YR==year,]
	
		#MAKE TEMP
		
		#
		n = nrow(pacfinDataY) #NOTE: do I really need both gear/gear_grp and port/port_complex???? maybe get rid of gear and port
		temp = data.frame(year=pacfinDataY$YR, qtr=NA, month=pacfinDataY$MON, gear=pacfinDataY$GEAR, gear_grp=NA, port=pacfinDataY$PORT, port_complex=NA, nominal_species=NA, mcat=pacfinDataY$MCAT, cond=pacfinDataY$COND, lbs=pacfinDataY$LBS)
		
		#sync tix with pacfin codes in calcom:
		#       gear_grp
		#       port_complex
		#       nominal_species
		for( j in 1:nrow(gearCodes) ){ temp$gear_grp[pacfinDataY$GEAR==gearCodes$GRID[j]] = gearCodes$gear_grp[j] }
		for( j in 1:nrow(portCodes) ){ temp$port_complex[pacfinDataY$PORT==portCodes$PCID[j]] = portCodes$port_complex[j] }
		for( j in 1:nrow(nmSpCodes) ){ temp$nominal_species[pacfinDataY$MCAT==nmSpCodes$mark_cat[j]] = nmSpCodes$nominal_species[j] }
		#warn the user if pacfin contains gears, ports, mcats that calcom does not
		lares::warnifnot( all(pacfinDataY$GEAR%in%gearCodes$GRID) )
		lares::warnifnot( all(pacfinDataY$PORT%in%portCodes$PCID) )
		lares::warnifnot( all(pacfinDataY$MCAT%in%nmSpCodes$mark_cat) )

		#define quarters from months
		#for(q in 0:3){ temp$qtr[ pacfinTix$MON%in%c((q*3+1):(q*3+3)) ]=q+1 }
		#easier to read
		temp$qtr[ pacfinDataY$MON %in% c(1,2,3)   ] = 1
		temp$qtr[ pacfinDataY$MON %in% c(4,5,6)   ] = 2
		temp$qtr[ pacfinDataY$MON %in% c(7,8,9)   ] = 3
		temp$qtr[ pacfinDataY$MON %in% c(10,11,12)] = 4
		#warn the user if pacfin contains a month that is not a month (key punch error)
		lares::warnifnot( all(pacfinDataY$MON%in%1:12) )
		
		#live/no live condition
		# 'Y'=live
		# 'A'=live
		#o.w. : not live
		temp$cond[temp$cond=='A'] = 'Y'
		temp$cond[temp$cond!='Y'] = 'N'
		
		#limit temp to the chosen few mcats
		keepers = c(146, 147, 150, 152, 155, 166, 167, 170:178, 190, 195:212, 225:271, 289, 290, 650:679, 956:976)
		temp = temp[ temp$mcat%in%keepers, ]

		#FIVE FILES

		#1) select ltrim(rtrim(mcat)), ltrim(rtrim(cond)), port_complex, gear_grp, sum(lbs), qrtr, nominal_species from temp_lrcpt group by mcat,cond,port_complex, gear_grp, qrtr, nominal_species order by mcat,co
		#NOTE: say in english
		nomPink = aggregate(temp$lbs, by=temp[,c('mcat', 'cond', 'port_complex', 'gear_grp', 'qtr', 'nominal_species')], FUN=sum)
		colnames(nomPink)[7] = 'lbs'
		nomPink$mcat = as.numeric(nomPink$mcat)
		
		#2) select * from ann_samp_vu where right([AT]expandyr,2)=substring(samp_no,3,2)	
		#Getting Sample Data but joining MASTER_SAMPLES and MASTR_CLUSTS
		#formerly temp1
		samp = temp1Y
		
		#3) select * from samp_strat_vu where right([AT]expandyr,2)=yr order by port_complex, live_fish, gear_grp, mark_cat, qrtr	
		#samp_strat_vu summarizes the number of samples (not including clusters) at each strata
		#samp_strat_vu is only a function of MASTER_SAMPLES
		#formerly temp2
		sampstrat = temp2Y	
		colnames(sampstrat)[c(4,5)] = c('mcat', 'cond')
		#sampstrat codes qtr in a more expected way
		sampstrat$qtr = sampstrat$qtr+1

		#4) select distinct mark_cat from master_samples where substring(sample_no,3,2)=right([AT]expandyr,2)
		#NOTE: say in english
		#NOTE: Confirm that master_samples is not a function of temp_lrpt
		mcat_list = mcat_listY
		
		#5) select * from temp_lrcpt where nominal_species is null or port_complex is null or gear_grp is null or qrtr is null
		whoNull = is.na(temp$nominal_species) | is.na(temp$port_complex) | is.na(temp$gear_grp) | is.na(temp$qtr)	
		#if any(whoNull) this needs to be fixed in pacfin 
		if( any(whoNull) ){
			nullName = sprintf('nullRcpts%s.csv', as.POSIXlt(Sys.time()))
			#Error and write a file to be checked.
			if( files ){
				#write null temp to file to help debug
				write.csv( temp[whoNull,], nullName, row.names=F, quote=F)
				stop(sprintf("\n\tFound %s PacFIN data outside of CALCOM categories.\n\tWritting problematic data to '%s'.\n\tWork with PacFIN to correct data.", year, nullName))
			}else{
				stop(sprintf("\n\tFound %s PacFIN data outside of CALCOM categories.\n\tRerun with files=True to write problematic data to nullRctps.csv.\n\tThen work with PacFIN to correct data.", year))
			}
		}
		
		#Warning and write badSpp.csv file
		goodSpp = species_codes[species_codes$comp_use!='N', 'species']
		isBadSpp = !samp$species%in%goodSpp 
		if( any(isBadSpp) ){
			#
			badName = sprintf('bad%sSpp.csv', year) #sprintf('bad%sSpp%s.csv', year, as.POSIXlt(Sys.time()))
			badSpp  = unique(samp[isBadSpp,'species'])
			#
			if( files ){
				#
				warning(sprintf("\n\t%s Bad Species Found: %s\n\tSample species code not found in valid CALCOM species_codes for species expansion.\n\tWritting problematic samples to '%s'.", year, toString(badSpp), badName))
        	        	#write the data to file.
				write.csv(samp[isBadSpp,], badName, row.names=F, quote=F)
			}else{
				warning(sprintf("\n\t%s Bad Species Found: %s\n\tSample species code not found in valid CALCOM species_codes for species expansion.\n\tSet argument files=True to write problematic samples out to a badSpp.csv file.", year, toString(badSpp)))
			}
		}

		#PREP
		
		#Samples
		#lump sample gears GFS, GFL, FTS, DNT, MDT into TWL
		samp$gear[samp$gear=="GFS"] = "TWL"
		samp$gear[samp$gear=="GFL"] = "TWL"
		samp$gear[samp$gear=="FTS"] = "TWL"
		samp$gear[samp$gear=="DNT"] = "TWL"
		samp$gear[samp$gear=="MDT"] = "TWL"
		
		#define qtr from month since the orignial vu did no have qtr
		samp$qtr[ samp$mon %in% c(1,2,3)   ] = 1  
		samp$qtr[ samp$mon %in% c(4,5,6)   ] = 2 
		samp$qtr[ samp$mon %in% c(7,8,9)   ] = 3 
		samp$qtr[ samp$mon %in% c(10,11,12)] = 4
		#warn the user if sample key punch error in sample month
		lares::warnifnot( all(samp$MON%in%1:12) )
		
		#dump samples
		samp = samp[samp$mark_cat!=0,]
		samp = samp[samp$tot_wgt!=0,]
		samp = samp[samp$sp_wgt!=0,] # one sample in 2019 records a weight of zero and is dropped
		#NOTE: does not drop NA or NULL, probably should throw a warning in this case
		lares::warnifnot( all(is.numeric(samp$mark_cat)) )
		lares::warnifnot( all(is.numeric(samp$tot_wgt)) )
		lares::warnifnot( all(is.numeric(samp$sp_wgt)) )
		
		#Summing over cluster here to clean up work later
		samp = aggregate(samp$sp_wgt, by=samp[,-c(2,3,10)], FUN=sum)
		colnames(samp) = c('sampNo', 'gear', 'port', 'mcat', 'disp', 'lands', 'spp', 'qtr', 'samp')

		#Split Sample and Nonsampled Strata	
		
		#filter nonsampled market categories
		whoSampled = nomPink$mcat %in% mcat_list$mark_cat
		#sampled rows
		samPink = nomPink[whoSampled,]
		#split out the non sampled rows
		nomPink = nomPink[!whoSampled,]
		#prepping for later in expansion
		nomPink = cbind(year, nomPink, 'N')
		colnames(nomPink) = c('year', 'mcat', 'disp', 'port', 'gear', 'qtr', 'spp', 'x', 'source')
		nomPink = nomPink[,c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp')]
	
		#None of the coding or ordering (as Don does; not done here) is neccessary if used with a joint merge on all of (disp, port, gear, mcat, qtr) jointly
		#I have left the coding system (ordering replaced with merge calls) to match the aggregation of disp port and gear don mixes in in these steps.
		#in don's pink.tmp some pounds appear to be ceilings, some floors, and some rounded. I have left my numbers un-altered	

		#make the coded array (coding aggregates disp, port, and gear)	
		pinkCode = makeStratCode(samPink$cond, samPink$port_complex, samPink$gear_grp, samPink$mcat, samPink$qtr)
		pink = cbind(code=pinkCode, totLbs=samPink$lbs)
		#retrieve samples
		sampCode = makeStratCode(sampstrat$cond, sampstrat$port_complex, sampstrat$gear_grp, sampstrat$mcat, sampstrat$qtr)
		sampstratCode = cbind(code=sampCode, ct=sampstrat$ct, samLbs=as.numeric(sampstrat$sampwt))
		sampstratCode = merge(pink, sampstratCode, by=c('code'), all.x=T)	

		#total number of total pounds, sampled pounds, and #(samples) taken in each strata
		uncodedStrat = makeCodeStrat(sampstratCode$code)
		sampstratCT  = aggregate(sampstratCode$ct, by=uncodedStrat, FUN=function(x){sum(x, na.rm=T)})
		sampstratTot = aggregate(sampstratCode$totLbs, by=uncodedStrat, FUN=function(x){sum(x, na.rm=T)})
		#sum out qtr
		sampstratCTSumQ  = aggregate(sampstratCode$ct, by=uncodedStrat[,c('disp', 'port', 'gear', 'mcat')], FUN=function(x){sum(x, na.rm=T)})
		sampstratTotSumQ = aggregate(sampstratCode$totLbs, by=uncodedStrat[,c('disp', 'port', 'gear', 'mcat')], FUN=function(x){sum(x, na.rm=T)})
		#QP STARTED HERE
		sampstratCT = merge(sampstratCT, sampstratCTSumQ, by=c('disp', 'port', 'gear', 'mcat'))
		colnames(sampstratCT) = c('disp', 'port', 'gear', 'mcat', 'qtr', 'x', 'xSumQ')
		sampstratTot = merge(sampstratTot, sampstratTotSumQ, by=c('disp', 'port', 'gear', 'mcat'))
		colnames(sampstratTot) = c('disp', 'port', 'gear', 'mcat', 'qtr', 'x', 'xSumQ')		

		#stashing exdoc stuff for actual actuals
		#NOTE: maybe this should be samp.... (later... I think sampstratCT is good)
		docAdd = sampstratCT[sampstratCT$x>0,c('qtr', 'disp', 'mcat', 'gear', 'port', 'x')]
		doc = cbind(year, docAdd[,c('qtr', 'disp', 'mcat', 'gear', 'port')], year, docAdd)
		colnames(doc) = c('yearAct', 'qtrAct', 'dispAct', 'mcatAct', 'gearAct', 'portAct', 'yearUse', 'qtrUse', 'dispUse', 'mcatUse', 'gearUse', 'portUse', 'ctUse')

		#sampledstrata.txt: appears to filter c(tot, ct) by quarter; including any row where any ct occured
		#sample_list.txt: appears to be a different way of making sampledstrata.txt
		#neither of these are ever used by the code. they were for the the user to look at to manually borrow

		#Assign Strata
		
		#a metric ton
		totNomThresh = 2204
		nc = nrow(sampstratCT)
		sourceDF = data.frame(sampstratCT[,c('disp', 'port', 'gear', 'mcat', 'qtr')], source=character(nc), borrPort=character(nc), borrGear=character(nc), borrMcat=character(nc), borrQtr=integer(nc))
		
		#lingcod
		#NOTE: handle the case of 195?? I think or whatever that special case was
		sourceDF$source[sourceDF$mcat==195] = 'N'
		#is there a reason not to hard code this? might it change in time?
		#sablefish is hardcoded. should it be?
		#if they dont take Actual samples of sablefish it can remain down there, otherwise it should probably go up with lingcod
		
		#ahead of everything this happens and skips the rest	
		sourceDF$source[sampstratTot$xSumQ<totNomThresh] = 'N'
		sourceDF$source[sampstratCT$xSumQ>0] = 'A'
		#nominal gears
		sourceDF$source[sourceDF$gear=='OTH'] = 'N'
		sourceDF$source[sourceDF$gear=='FPT'] = 'N'
		sourceDF$source[sourceDF$gear=='UNK'] = 'N'
		#sablefish are nominal
		sourceDF$source[sourceDF$mcat==190] = 'N'
		#nominal port
		sourceDF$source[sourceDF$port=='Nick Says Other'] = 'N'
		
		#Port Borrowing Directions
			#following dons code I calculate port borrow (and qtr:port borrows) directions but will not apply them until later
		
		borrowLimit = 2
		map = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
		
		#loop over unsampled that are not already either Nominal or Actual
		for(row in which(sourceDF$source=='')){
		        #find available samples to port borrow from
		        sam = sampstratCT[
		                sampstratCT$x>0	              		&
		                sampstratCT$disp==sourceDF$disp[row]   	&
		                sampstratCT$gear==sourceDF$gear[row]   	&
		                sampstratCT$mcat==sourceDF$mcat[row]
		        ,]
			
		        #if no available samples found in adjacent strata, then the strata is nominal
		        if( nrow(sam)==0 ){ sourceDF$source[row] = 'N'; next }

		        #port priority for the given missing strata
		        portPriority = portBorr[sourceDF[row,'port'],]
		        #priorize samples
		        prior = which(portPriority%in%sam$port)
			#if no prior is found the strata is nominal
		        if( length(prior)==0 ){ sourceDF$source[row] = 'N'; next }
		        #find the port with the most favored priority among the available samples
		        borrowPort = portPriority[ min(prior) ]
		
		        #make the 'C' 'D'... source a thing     #rowSource = 'B'
			def = which( map==sourceDF$port[row] )  #def = which( map==sourceDF$port[row] )
        	        rep = which( map==borrowPort )          #rep = which( map==borrowPort )
			rowSource = LETTERS[abs(def-rep)+2]	#if( abs(def-rep)>=borrowLimit ){ rowSource=LETTERS[abs(def-rep)+1] } #'C'
		       
			#assume we can use the current qtr
			borrowQtr = sourceDF[row, 'qtr']
			samGivenP = sam[sam$port==borrowPort,]
			#qtr borrow given port borrow (if the current qtr not available)
			if( !(borrowQtr%in%samGivenP[,'qtr']) ){
				#qsGivenP = samGiven[,'qtr']
				qtrPriority = qtrBorr[sourceDF[row, 'qtr'],]
                		prior = which(qtrPriority%in%samGivenP[,'qtr'])
                        	borrowQtr = qtrPriority[ min(prior) ]
			}
			
		        #if a sample that can be prioritzed is found, then borrow
		        sourceDF$source[row] = rowSource
		        sourceDF[row,c('borrPort', 'borrGear', 'borrMcat', 'borrQtr')] = sam[sam$port==borrowPort & sam$qtr==borrowQtr,c('port', 'gear', 'mcat', 'qtr')]

			#NOTE: when is ignore ever a thing?
				#it used to be an option when hand entering borrows
				#it mearly skipped expanding that strat
				#I have no mechanism for doing that
		}
			
		#
		expanduseCT = merge(sampstratCT, sourceDF, all.x=T)
		expanduseTot = merge(sampstratTot, sourceDF, all.x=T)

		#Expand	Nominals
		
		#
		species = species_codes$species[species_codes$comp_use!='N' & !is.na(species_codes$comp_use)]
		nom_mcats = market_categories[!is.na(market_categories$nominal_species), c('mark_cat', 'nominal_species')]
		rownames(nom_mcats) = as.character(market_categories[!is.na(market_categories$nominal_species),'mark_cat'])
			
		#now filter and add the nominals from the Assign step
		nomMcat = as.character(expanduseTot$mcat[expanduseTot$source=='N'])
		moreNom = expanduseTot[expanduseTot$source=='N', c('qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source')]
		#glomming nominals onto output variable
		moreNom = cbind(year, moreNom, nom_mcats[nomMcat, 'nominal_species'],1)
		colnames(moreNom) = c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp', '1')
		
		#Spp Comps For Actuals
			#when borrowing occurs the fine grained available sppComps are just applied to differently designated strata
			#thus first calculate the fine grained sppComps 
		
		#sum samp over spp
		sBot = aggregate(samp$samp, by=samp[,c('sampNo', 'gear', 'port', 'mcat', 'disp', 'lands', 'qtr')], FUN=sum)
		spComp = merge(samp, sBot, all.x=T)
		colnames(spComp)[ncol(spComp)] = 'sampSum'
		#fine grained spp comps
		spComp$spc = spComp$samp/spComp$sampSum
		
		#calculate weighting factor for weighting by landings
		lTop = aggregate(samp$lands, by=samp[,c('sampNo', 'gear', 'port', 'mcat', 'disp', 'qtr')], FUN=unique)
		colnames(lTop)[ncol(lTop)] = 'land'
		#sum over samples
		lBot = aggregate(lTop$land, by=lTop[,c('gear', 'port', 'mcat', 'disp', 'qtr')], FUN=sum)
		landW = merge(lTop, lBot, all.x=T)
		colnames(landW)[ncol(landW)] = 'landSum'
		landW$w = landW$land/landW$landSum
		
		#weight fine grained spp comps
		wComp = merge(spComp, landW, all.x=T)
		wComp$spw = wComp$spc*wComp$w
		wComp = aggregate(wComp$spw, wComp[,c('gear', 'port', 'mcat', 'disp', 'qtr', 'spp')], FUN=sum)
		colnames(wComp)[ncol(wComp)] = 'wc'
		
		#preparing to expand the actuals by setting landings next to comps for available fine grained comps
		#at this point expanduseTot$source=='A' represents actual actuals and qtr-only borrows	
		actLand = merge(expanduseTot[expanduseTot$source=='A', c('disp', 'port', 'gear', 'mcat', 'qtr', 'x')], wComp, all=T)
		actLand = cbind(year, actLand[!is.na(actLand$x), c('qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'wc', 'spp')])
		actLandSnap = actLand #take a snapshot to work on before addding to it	
	
		#Qtr Borrowing
			#Since Don goes super saiyan in his qtr borrowing code, I'm doing it differently.	
			#Borrowed qtr holes are filled directly with the fine grained sppComp calculations above.
			#Basically below is the same as the port borrowing code, but the fill is directly applied rather than only recording the borrowing directions.
		
		#print(actLandSnap[actLandSnap$port=='CRS' & actLandSnap$mcat==147 & actLandSnap$gear=='TWL',])
		#print(samp[samp$port=='CRS' & samp$mcat==147 & samp$gear=='TWL',])
		#print(sampstrat[sampstrat$port=='CRS' & sampstrat$mcat==147 & sampstrat$gear=='TWL',])
		#print( head(samp) )
		#print( head(sampstrat) )

		#loop over actLand where is.na(wc)
		for( row in which(is.na(actLandSnap$wc)) ){
		        #find available strata to borrow from
		        act = actLandSnap[
		                actLandSnap$disp==actLandSnap$disp[row] &
		                actLandSnap$gear==actLandSnap$gear[row] &
		                actLandSnap$port==actLandSnap$port[row] &
		                actLandSnap$mcat==actLandSnap$mcat[row]
		        ,]
		        #
		        act = act[!is.na(act$wc),]
		        if( nrow(act)==0 ){ next } #i=i+1; #appearantly no samples
			
		        #
		        qtrPriority = qtrBorr[actLandSnap[row, 'qtr'],]
		        prior = which(qtrPriority%in%act$qtr)
		        #if( length(prior)==0 ){ next }
		        borrowQtr = qtrPriority[ min(prior) ]
			
		        #make a filler
		        fill = act[act$qtr==borrowQtr,]
		        fill$x = actLandSnap$x[row]
		        fill$qtr = actLandSnap$qtr[row]
			
			#add to exdoc
			docAdd = cbind( 
				actLandSnap[row, c('year', 'qtr', 'disp', 'mcat', 'gear', 'port')], 
				year,
				sampstratCT[
					sampstratCT$qtr==borrowQtr	    	&
					sampstratCT$disp==actLandSnap$disp[row] &
					sampstratCT$gear==actLandSnap$gear[row] &
					sampstratCT$port==actLandSnap$port[row] &
					sampstratCT$mcat==actLandSnap$mcat[row] 
				, c('qtr', 'disp', 'mcat', 'gear', 'port', 'x')]
			)
			#exdocAdd = cbind(actLand[row,c('year', 'qtr', 'disp', 'mcat', 'gear', 'port')], fill[,c('year', 'qtr', 'disp', 'mcat', 'gear', 'port')])
			colnames(docAdd) = c('yearAct', 'qtrAct', 'dispAct', 'mcatAct', 'gearAct', 'portAct', 'yearUse', 'qtrUse', 'dispUse', 'mcatUse', 'gearUse', 'portUse', 'ctUse')
			doc = rbind(doc, docAdd)	
			
		        #add the fill to actLand
		        actLand = rbind(actLand, fill)
		}
		#remove the null rows all at the end so as to avoid messing with indexing
		actLand = actLand[!is.na(actLand$wc),]
		actLandB = actLand[,-7] #get this copy for use with Borrow, -7 should remove the 'x' column
		#print(actLandB[actLandB$mcat==663 & actLandB$port=="MNT",])
		#expand
		actLand$x = actLand$x*actLand$wc
		#regigger names and stuff
		actLand$source = 'A'
		colnames(actLand) = c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', '1', 'spp', 'source')
		actLand = actLand[c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp', '1')]
				
		#Port Borrowing Expand
	
		#at this point !expanduseTot$source%in%c('N', 'A') represents port borrows
		#use borrow tags to look up spcomps for borrows and then expand the borrows
		borrLand = expanduseTot[!expanduseTot$source%in%c('N', 'A'),c('disp', 'port', 'gear', 'mcat', 'qtr', 'x', 'source', 'borrPort', 'borrQtr')]
		#renaming 'borrPort' as 'port' (and 'borrQtr' as 'qtr') allows us to merge borrLand with the borrowed qtr actLandB 
		colnames(borrLand) = c('disp', 'actPort', 'gear', 'mcat', 'actQtr', 'x', 'source', 'port', 'qtr')
		borrLand = merge(borrLand, actLandB, all=T)
		borrLand$x = borrLand$x*borrLand$wc 
		#we merged on 'borrPort' (and 'borrQtr' as 'qtr'), but we actually want to display the actual port (and qtr)
		borrLand = borrLand[!is.na(borrLand$x), c('year', 'actQtr', 'disp', 'mcat', 'gear', 'actPort', 'x', 'source', 'spp', 'wc')]
		colnames(borrLand) = c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp', '1') #rename 'actPort' for glomming below
		#QP MOD almost ENDED HERE
				
		#figure out samples for qtr within port borrowing only is files is requested 
		expCT = expanduseCT[!expanduseCT$source%in%c('N', 'A'),]
		for(row in 1:nrow(expCT)){
			#
			bor = expanduseCT[
				expanduseCT$x>0			      &
				expanduseCT$source=='A'		      &
				expanduseCT$disp==expCT$disp[row]     &
				expanduseCT$port==expCT$borrPort[row] &
				expanduseCT$gear==expCT$borrGear[row] &
				expanduseCT$mcat==expCT$borrMcat[row] 	
			,]
	
			##
                        #qtrPriority = qtrBorr[expCT$qtr,]
			#prior = which(qtrPriority%in%bor$qtr)
                        #borrowQtr = qtrPriority[ min(prior) ]	
			##
			
			#assume we can use the current qtr
                        borrowQtr = expCT[row, 'qtr']
                        #qtr borrow given port borrow (if the current qtr not available)
                        if( !(borrowQtr%in%bor[,'qtr']) ){
				#
                                qtrPriority = qtrBorr[expCT[row, 'qtr'],]
                                prior = which(qtrPriority%in%bor[,'qtr'])
                                borrowQtr = qtrPriority[ min(prior) ]
                        }
			#QP MOD ENDED HERE
				
			#
			docAdd = cbind(
				year, 
				expCT[row,c('qtr', 'disp', 'mcat', 'gear', 'port')],
				year,
				bor[bor$qtr==borrowQtr,c('qtr', 'disp', 'mcat', 'gear', 'port', 'x')]
			)
			colnames(docAdd) = c('yearAct', 'qtrAct', 'dispAct', 'mcatAct', 'gearAct', 'portAct', 'yearUse', 'qtrUse', 'dispUse', 'mcatUse', 'gearUse', 'portUse', 'ctUse')
                        doc = rbind(doc, docAdd)
		}
			
		#
		if( files ){ 
			write.csv(doc, sprintf("sppdoc%s.csv", year), row.names=F, quote=F) 
		}
		
		#dump nominal species into output file
		expLand  = rbind(expLand, cbind(nomPink, 1)) #start with the nomPink from earlier
		expLand  = rbind(expLand, moreNom)
		#glomming actuals (without and with borrowed qtr) onto output variable
		m = merge(actLand, doc, 
			by.x=c("year", "qtr", "disp", "mcat", "gear", "port"),
			by.y=c("yearAct", "qtrAct", "dispAct", "mcatAct", "gearAct", "portAct")
		)
		m$source[m$qtr!=m$qtrUse] = 'B'
		actLand = m[,c("year", "qtr", "disp", "mcat", "gear", "port", "x", "source", "spp", "1")]
		expLand = rbind(expLand, actLand)
		#glomming borrowed port expansion onto output variable
        	expLand = rbind(expLand, borrLand)
	}	

	#clean up colnames
	colnames(expLand) = c('year','qtr','disp','mcat','gear','port','lands', 'source','spp','comp')
	expLand = expLand[,c('year','qtr','disp','mcat','gear','port','source','spp','lands','comp')]
	#reorder rows
	expLand = expLand[order(expLand$year, expLand$mcat, expLand$disp, expLand$gear, expLand$port, expLand$qtr),]
	#
	return( expLand )
	#return( expLand[,c('year','qtr','disp','mcat','gear','port','source','spp','lands','comp')] )
}

#' A version of the core expansion function executing an automated species expansion 
#' based on the borrowing dictated by a doc file. 
#' 
#' @param pacfinData 	A data.frame as returned by getPacfinSppData.
#' @param calcomData 	A list as returned by getCalcomSppData.
#' @param doc		The filename (or vector of filenames) of the doc files 
#'	to use in manual borrowing overrides. Defaults to sprintf("sppdoc%s.csv", unique(pacfinData$YR))
#' @param qtrBorr    	A matrix to define the priority of qtr borrowing. Rownames 
#'	should define the actual qtr of the stratum to be filled. The first 
#'	column contains the the first priority to borrowing from, second column 
#'	contains the second priority, ... etc. Elements should be integers %in% 1:4. 
#'	Elements not given %in% 1:4 will code for Nominal.
#' @param files    	A boolean flag to produce verbose error files and/or expansion output 
#'	files such as sppdoc.
#'
#' @return a data.frame reminiscent of the calcom.com_lands table.
estSppCompDoc = function(pacfinData, calcomData, doc=sprintf("sppdoc%s.csv", unique(pacfinData$YR)), qtrBorr=qtrMatrix, files=T){
	#pacfinData : a data.frame as returned by getPacfinData
	#calcomData : a list as returned by getCalcomData
	#doc      : a filename (or vector of filenames for multiple years) of a CSV file to define which data are used to expand which strata.
	#		strata which do not appear in the doc file are assumed to be nominal.
	#files     : a boolean flag to produce verbose error files and/or expansion output files.
	#
	#value      : 

	#UNPACK CALCOM
	
	#
	gearCodes = calcomData$gearCodes
	portCodes = calcomData$portCodes
	nmSpCodes = calcomData$nmSpCodes
	#
	species_codes = calcomData$species_codes
        market_categories = calcomData$market_categories

	#DEAL W/ YEAR

	#
	calcomT1Year = as.numeric(unique(substr(calcomData$temp1$samp_no, 1, 4)))
	calcomT2Year = as.numeric(unique(calcomData$temp2$yr))
	calcomMYear = unique(calcomData$mcat_list$year)
	pacfinYear = unique(pacfinData$YR)
	#4 choose 2 comparisons to make sure all the years are the same in all the data sources
	stopifnot( calcomT1Year%%100 %in% calcomT2Year )
	stopifnot( calcomMYear%%100 %in% calcomT2Year )
	stopifnot( pacfinYear%%100 %in% calcomT2Year )
	stopifnot( calcomT1Year %in% calcomMYear )
	stopifnot( calcomT1Year %in% pacfinYear )
	stopifnot( calcomMYear %in% pacfinYear )
	
	#getting all the docs and checking year situation
	docs = list()
	for(i in 1:length(pacfinYear)){
		#
		docY = read.csv(doc[i])
		docYear = unique(docY$yearAct)
		#
		stopifnot( length(docYear)==1 ) #the yealy exdoc files should only contain strata from a single year
		stopifnot( docYear %in% pacfinYear )
		#
		docs[[docYear]] = docY
	}

	#
	expLand = c()
	for(year in pacfinYear){
		#parsing out the yearly data structures
		temp1Y = calcomData$temp1[as.numeric(substr(calcomData$temp1$samp_no, 1, 4))==year,]
		temp2Y = calcomData$temp2[as.numeric(calcomData$temp2$yr)==year%%100,]
		mcat_listY = calcomData$mcat_list[calcomData$mcat_list$year==year,]
		pacfinDataY = pacfinData[pacfinData$YR==year,]
		#	
		docY = docs[[year]]
		
		#MAKE TEMP
		
		#
		n = nrow(pacfinDataY)
		temp = data.frame(year=pacfinDataY$YR, qtr=NA, month=pacfinDataY$MON, gear=pacfinDataY$GEAR, gear_grp=NA, port=pacfinDataY$PORT, port_complex=NA, nominal_species=NA, mcat=pacfinDataY$MCAT, cond=pacfinDataY$COND, lbs=pacfinDataY$LBS)
		
		#sync tix with pacfin codes in calcom:
		#       gear_grp
		#       port_complex
		#       nominal_species
		for( j in 1:nrow(gearCodes) ){ temp$gear_grp[pacfinDataY$GEAR==gearCodes$GRID[j]] = gearCodes$gear_grp[j] }
		for( j in 1:nrow(portCodes) ){ temp$port_complex[pacfinDataY$PORT==portCodes$PCID[j]] = portCodes$port_complex[j] }
		for( j in 1:nrow(nmSpCodes) ){ temp$nominal_species[pacfinDataY$MCAT==nmSpCodes$mark_cat[j]] = nmSpCodes$nominal_species[j] }
		#warn the user if pacfin contains gears, ports, mcats that calcom does not
		lares::warnifnot( all(pacfinDataY$GEAR%in%gearCodes$GRID) )
		lares::warnifnot( all(pacfinDataY$PORT%in%portCodes$PCID) )
		lares::warnifnot( all(pacfinDataY$MCAT%in%nmSpCodes$mark_cat) )

		#define quarters from months
		#for(q in 0:3){ temp$qtr[ pacfinTix$MON%in%c((q*3+1):(q*3+3)) ]=q+1 }
		#easier to read
		temp$qtr[ pacfinDataY$MON %in% c(1,2,3)   ] = 1
		temp$qtr[ pacfinDataY$MON %in% c(4,5,6)   ] = 2
		temp$qtr[ pacfinDataY$MON %in% c(7,8,9)   ] = 3
		temp$qtr[ pacfinDataY$MON %in% c(10,11,12)] = 4
		#warn the user if pacfin contains a month that is not a month (key punch error)
		lares::warnifnot( all(pacfinDataY$MON%in%1:12) )
		
		#live/no live condition
		# 'Y'=live
		# 'A'=live
		#o.w. : not live
		temp$cond[temp$cond=='A'] = 'Y'
		temp$cond[temp$cond!='Y'] = 'N'
		
		#limit temp to the chosen few mcats
		keepers = c(146, 147, 150, 152, 155, 166, 167, 170:178, 190, 195:212, 225:271, 289, 290, 650:679, 956:976)
		temp = temp[ temp$mcat%in%keepers, ]

		#FIVE FILES

		#1) select ltrim(rtrim(mcat)), ltrim(rtrim(cond)), port_complex, gear_grp, sum(lbs), qrtr, nominal_species from temp_lrcpt group by mcat,cond,port_complex, gear_grp, qrtr, nominal_species order by mcat,co
		#NOTE: say in english
		nomPink = aggregate(temp$lbs, by=temp[,c('mcat', 'cond', 'port_complex', 'gear_grp', 'qtr', 'nominal_species')], FUN=sum)
		colnames(nomPink)[7] = 'lbs'
		nomPink$mcat = as.numeric(nomPink$mcat)
		
		#2) select * from ann_samp_vu where right([AT]expandyr,2)=substring(samp_no,3,2)
		#Getting Sample Data but joining MASTER_SAMPLES and MASTR_CLUSTS
                #formerly temp1
		samp = temp1Y	
		
		#3) select * from samp_strat_vu where right([AT]expandyr,2)=yr order by port_complex, live_fish, gear_grp, mark_cat, qrtr
		#samp_strat_vu summarizes the number of samples (not including clusters) at each strata
                #samp_strat_vu is only a function of MASTER_SAMPLES
                #formerly temp2
		sampstrat = temp2Y	
		colnames(sampstrat)[c(4,5)] = c('mcat', 'cond')
		#sampstrat codes qtr in a more expected way
		sampstrat$qtr = sampstrat$qtr+1

		#4) select distinct mark_cat from master_samples where substring(sample_no,3,2)=right([AT]expandyr,2)
		#NOTE: say in english
		#NOTE: Confirm that master_samples is not a function of temp_lrpt
		mcat_list = mcat_listY
		
		#5) select * from temp_lrcpt where nominal_species is null or port_complex is null or gear_grp is null or qrtr is null
		whoNull = is.na(temp$nominal_species) | is.na(temp$port_complex) | is.na(temp$gear_grp) | is.na(temp$qtr)	
		#if any(whoNull) this needs to be fixed in pacfin 
		if( any(whoNull) ){
			nullName = sprintf('nullRcpts%s.csv', as.POSIXlt(Sys.time()))
			#Error and write a file to be checked.
			if( files ){
				#write null temp to file to help debug
				write.csv( temp[whoNull,], nullName, row.names=F, quote=F)
				stop(sprintf("\n\tFound %s PacFIN data outside of CALCOM categories.\n\tWritting problematic data to '%s'.\n\tWork with PacFIN to correct data.", year, nullName))
			}else{
				stop(sprintf("\n\tFound %s PacFIN data outside of CALCOM categories.\n\tRerun with files=True to write problematic data to nullRctps.csv.\n\tThen work with PacFIN to correct data.", year))
			}
		}
		
		#NOTE: do I want badSpp for doc
		#Warning and write badSpp.csv file
		goodSpp = species_codes[species_codes$comp_use!='N', 'species']
		isBadSpp = !samp$species%in%goodSpp 
		#if( any(isBadSpp) ){
		#	#
		#	badName = sprintf('bad%sSpp.csv', year) #sprintf('bad%sSpp%s.csv', year, as.POSIXlt(Sys.time()))
		#	badSpp  = unique(samp[isBadSpp,'species'])
		#	#
		#	if( files ){
		#		#
		#		warning(sprintf("\n\t%s Bad Species Found: %s\n\tSample species code not found in valid CALCOM species_codes for species expansion.\n\tWritting problematic samples to '%s'.", year, toString(badSpp), badName))
        	#        	#write the data to file.
		#		write.csv(samp[isBadSpp,], badName, row.names=F, quote=F)
		#	}else{
		#		warning(sprintf("\n\t%s Bad Species Found: %s\n\tSample species code not found in valid CALCOM species_codes for species expansion.\n\tSet argument files=True to write problematic samples out to a badSpp.csv file.", year, toString(badSpp)))
		#	}
		#}

		#PREP
		
		#Samples
		#lump sample gears GFS, GFL, FTS, DNT, MDT into TWL
		samp$gear[samp$gear=="GFS"] = "TWL"
		samp$gear[samp$gear=="GFL"] = "TWL"
		samp$gear[samp$gear=="FTS"] = "TWL"
		samp$gear[samp$gear=="DNT"] = "TWL"
		samp$gear[samp$gear=="MDT"] = "TWL"
		
		#define qtr from month since the orignial vu did no have qtr
		samp$qtr[ samp$mon %in% c(1,2,3)   ] = 1  
		samp$qtr[ samp$mon %in% c(4,5,6)   ] = 2 
		samp$qtr[ samp$mon %in% c(7,8,9)   ] = 3 
		samp$qtr[ samp$mon %in% c(10,11,12)] = 4
		#warn the user if sample key punch error in sample month
		lares::warnifnot( all(samp$MON%in%1:12) )
		
		#dump samples
		samp = samp[samp$mark_cat!=0,]
		samp = samp[samp$tot_wgt!=0,]
		samp = samp[samp$sp_wgt!=0,] # one sample in 2019 records a weight of zero and is dropped
		#NOTE: does not drop NA or NULL, probably should throw a warning in this case
		lares::warnifnot( all(is.numeric(samp$mark_cat)) )
		lares::warnifnot( all(is.numeric(samp$tot_wgt)) )
		lares::warnifnot( all(is.numeric(samp$sp_wgt)) )
		
		#Summing over cluster here to clean up work later
		samp = aggregate(samp$sp_wgt, by=samp[,-c(2,3,10)], FUN=sum)
		colnames(samp) = c('sampNo', 'gear', 'port', 'mcat', 'disp', 'lands', 'spp', 'qtr', 'samp')

		#Split Sample and Nonsampled Strata	
		
		#filter nonsampled market categories
		whoSampled = nomPink$mcat %in% mcat_list$mark_cat
		#sampled rows
		samPink = nomPink[whoSampled,]
		#split out the non sampled rows
		nomPink = nomPink[!whoSampled,]
		#NOTE: prepping for later in expansion
		nomPink = cbind(year, nomPink, 'N')
		colnames(nomPink) = c('year', 'mcat', 'disp', 'port', 'gear', 'qtr', 'spp', 'x', 'source')
		nomPink = nomPink[,c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp')]

		#None of the coding or ordering (as Don does; not done here) is neccessary if used with a joint merge on all of (disp, port, gear, mcat, qtr) jointly
		#I have left the coding system (ordering replaced with merge calls) to match the aggregation of disp port and gear don mixes in in these steps.
		#in don's pink.tmp some pounds appear to be ceilings, some floors, and some rounded. I have left my numbers un-altered	

		#make the coded array (coding aggregates disp, port, and gear)	
		pinkCode = makeStratCode(samPink$cond, samPink$port_complex, samPink$gear_grp, samPink$mcat, samPink$qtr)
		pink = cbind(code=pinkCode, totLbs=samPink$lbs)
		#retrieve samples
		sampCode = makeStratCode(sampstrat$cond, sampstrat$port_complex, sampstrat$gear_grp, sampstrat$mcat, sampstrat$qtr)
		sampstratCode = cbind(code=sampCode, ct=sampstrat$ct, samLbs=as.numeric(sampstrat$sampwt))
		sampstratCode = merge(pink, sampstratCode, by=c('code'), all.x=T)	

		#total number of total pounds, sampled pounds, and #(samples) taken in each strata
		uncodedStrat = makeCodeStrat(sampstratCode$code)
		sampstratCT  = aggregate(sampstratCode$ct, by=uncodedStrat, FUN=function(x){sum(x, na.rm=T)})
		sampstratTot = aggregate(sampstratCode$totLbs, by=uncodedStrat, FUN=function(x){sum(x, na.rm=T)})
		#sum out qtr
		sampstratCTSumQ  = aggregate(sampstratCode$ct, by=uncodedStrat[,c('disp', 'port', 'gear', 'mcat')], FUN=function(x){sum(x, na.rm=T)})
		sampstratTotSumQ = aggregate(sampstratCode$totLbs, by=uncodedStrat[,c('disp', 'port', 'gear', 'mcat')], FUN=function(x){sum(x, na.rm=T)})
		#QP MOD
		sampstratCT = merge(sampstratCT, sampstratCTSumQ, by=c('disp', 'port', 'gear', 'mcat'))
                colnames(sampstratCT) = c('disp', 'port', 'gear', 'mcat', 'qtr', 'x', 'xSumQ')
                sampstratTot = merge(sampstratTot, sampstratTotSumQ, by=c('disp', 'port', 'gear', 'mcat'))
                colnames(sampstratTot) = c('disp', 'port', 'gear', 'mcat', 'qtr', 'x', 'xSumQ')
		
		#sampledstrata.txt: appears to filter c(tot, ct) by quarter; including any row where any ct occured
		#sample_list.txt: appears to be a different way of making sampledstrata.txt
		#neither of these are ever used by the code. they were for the the user to look at to manually borrow
		
		#Assign Strata
		
		#a metric ton
		totNomThresh = 2204
		nc = nrow(sampstratCT)
		sourceDF = data.frame(sampstratCT[,c('disp', 'port', 'gear', 'mcat', 'qtr')], source=character(nc), borrPort=character(nc), borrGear=character(nc), borrMcat=character(nc), borrQtr=integer(nc))

		#lingcod
		#NOTE: handle the case of 195?? I think or whatever that special case was
		sourceDF$source[sourceDF$mcat==195] = 'N'
		#is there a reason not to hard code this? might it change in time?
		#sablefish is hardcoded. should it be?
		#if they dont take Actual samples of sablefish it can remain down there, otherwise it should probably go up with lingcod
		
		#ahead of everything this happens and skips the rest	
		sourceDF$source[sampstratTot$xSumQ<totNomThresh] = 'N'
                sourceDF$source[sampstratCT$xSumQ>0] = 'A'
		#nominal gears
		sourceDF$source[sourceDF$gear=='OTH'] = 'N'
		sourceDF$source[sourceDF$gear=='FPT'] = 'N'
		sourceDF$source[sourceDF$gear=='UNK'] = 'N'
		#sablefish are nominal
		sourceDF$source[sourceDF$mcat==190] = 'N'
		#nominal port
		sourceDF$source[sourceDF$port=='Nick Says Other'] = 'N'
		
		#Port Borrowing Directions
		#	#I calculate port borrowing directions in expandSppToLand and record them in doc
		
		#
		map = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
		
		#
		for(row in which(sourceDF$source=='')){
			#find available samples to borrow from
                	sam = sampstratCT[
                	        sampstratCT$x>0			 	&
                	        sampstratCT$disp==sourceDF$disp[row] 	&
                	        sampstratCT$gear==sourceDF$gear[row] 	&
                	        sampstratCT$mcat==sourceDF$mcat[row]
                	,]
                	#if no available samples found in adjacent strata, then the strata is nominal
                	if( nrow(sam)==0 ){ sourceDF$source[row]='N'; next }
			
			#look up borrow strata from doc unique(
			borrow = docY[
				docY$dispAct==sourceDF$disp[row] &
				docY$gearAct==sourceDF$gear[row] &
				docY$mcatAct==sourceDF$mcat[row] &
				docY$portAct==sourceDF$port[row] #&
				#exDocY$qtrAct ==sourceDF$qtr[row]
			, c("portUse", "qtrAct", "qtrUse")]
			borrowPort = unique(borrow$portUse)	
			
			#
			samGivenP = sam[sam$port==borrowPort,]
			#if none: nominal
			if( length(borrowPort)==0 ){ sourceDF$source[row]='N'; next }
			if( nrow(samGivenP)==0 ){ sourceDF$source[row]='N'; next }

			#make the 'C' source a thing            #rowSource = 'B'
			def = which( map==sourceDF$port[row] )  #def = which( map==sourceDF$port[row] )
        		rep = which( map==borrowPort )          #rep = which( map==borrowPort )
			rowSource = LETTERS[abs(def-rep)+2]	#if( abs(def-rep)>=borrowLimit ){ rowSource=LETTERS[abs(def-rep)+1] } #'C'
		        
			#first try the doc qtr
			borrowQtr = borrow$qtrUse[borrow$qtrAct==sourceDF$qtr[row]]
			if( !(borrowQtr%in%samGivenP[,'qtr']) ){
				#next assume we can use the current qtr
                        	borrowQtr = sourceDF[row, 'qtr']
                        	#qtr borrow given port borrow (if the above assumtions were not available)
                        	if( !(borrowQtr%in%samGivenP[,'qtr']) ){
                        	        ##qsGivenP = samGiven[,'qtr']
                        	        qtrPriority = qtrBorr[sourceDF[row, 'qtr'],]
                        	        prior = which(qtrPriority%in%samGivenP[,'qtr'])
                        	        borrowQtr = qtrPriority[ min(prior) ]
				}
			}	
			#
			samGivenPQ = samGivenP[samGivenP$qtr==borrowQtr,]
			#only ever true if the inner most qtr borrow fails
			if( nrow(samGivenPQ)==0 ){ sourceDF$source[row]='N'; next }	
			
			#if a sample that can be prioritzed is found, then borrow
                        sourceDF$source[row] = rowSource
                        sourceDF[row,c('borrPort', 'borrGear', 'borrMcat', 'borrQtr')] = samGivenPQ[,c('port', 'gear', 'mcat', 'qtr')]
		
			##NOTE: when is ignore ever a thing?
			#	#it used to be an option when hand entering borrows
			#	#it mearly skipped expanding that strat
			#	#I have no mechanism for doing that
		}	
		#
		expanduseCT = merge(sampstratCT, sourceDF, all.x=T)
		expanduseTot = merge(sampstratTot, sourceDF, all.x=T)

		#Expand	Nominals
		
		#
		species = species_codes$species[species_codes$comp_use!='N' & !is.na(species_codes$comp_use)]
		nom_mcats = market_categories[!is.na(market_categories$nominal_species), c('mark_cat', 'nominal_species')]
		rownames(nom_mcats) = as.character(market_categories[!is.na(market_categories$nominal_species),'mark_cat'])
		
		#now filter and add the nominals from the Assign step
		nomMcat = as.character(expanduseTot$mcat[expanduseTot$source=='N'])
		moreNom = expanduseTot[expanduseTot$source=='N', c('qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source')]
		#glomming nominals onto output variable
		moreNom = cbind(year, moreNom, nom_mcats[nomMcat, 'nominal_species'],1)
		colnames(moreNom) = c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp',1)
		#expLand  = rbind(expLand, cbind(moreNom,1))

		#Spp Comps For Actuals
			#when borrowing occurs the fine grained available sppComps are just applied to differently designated strata
			#thus first calculate the fine grained sppComps 
		
		#sum samp over spp
		sBot = aggregate(samp$samp, by=samp[,c('sampNo', 'gear', 'port', 'mcat', 'disp', 'lands', 'qtr')], FUN=sum)
		spComp = merge(samp, sBot, all.x=T)
		colnames(spComp)[ncol(spComp)] = 'sampSum'
		#fine grained spp comps
		spComp$spc = spComp$samp/spComp$sampSum
		
		#calculate weighting factor for weighting by landings
		lTop = aggregate(samp$lands, by=samp[,c('sampNo', 'gear', 'port', 'mcat', 'disp', 'qtr')], FUN=unique)
		colnames(lTop)[ncol(lTop)] = 'land'
		#sum over samples
		lBot = aggregate(lTop$land, by=lTop[,c('gear', 'port', 'mcat', 'disp', 'qtr')], FUN=sum)
		landW = merge(lTop, lBot, all.x=T)
		colnames(landW)[ncol(landW)] = 'landSum'
		landW$w = landW$land/landW$landSum
		
		#weight fine grained spp comps
		wComp = merge(spComp, landW, all.x=T)
		wComp$spw = wComp$spc*wComp$w
		wComp = aggregate(wComp$spw, wComp[,c('gear', 'port', 'mcat', 'disp', 'qtr', 'spp')], FUN=sum)
		colnames(wComp)[ncol(wComp)] = 'wc'
		
		#preparing to expand the actuals by setting landings next to comps for available fine grained comps
		#at this point expanduseTot$source=='A' represents actual actuals and qtr-only borrows
		actLand = merge(expanduseTot[expanduseTot$source=='A', c('disp', 'port', 'gear', 'mcat', 'qtr', 'x')], wComp, all=T)
		actLand = cbind(year, actLand[!is.na(actLand$x), c('qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'wc', 'spp')])
		actLandSnap = actLand #take a snapshot to work on before addding to it	
		
		#Qtr Borrowing
			#Since Don goes super saiyan in his qtr borrowing code, I'm doing it differently.	
			#Borrowed qtr holes are filled directly with the fine grained sppComp calculations above.
			#Basically below is the same as the port borrowing code, but the fill is driectly applied rather than only recording the borrowing directions.
		
		#loop over actLand where is.na(wc)
		for( row in which(is.na(actLandSnap$wc)) ){
		        #find available strata to borrow from
		        act = actLandSnap[
		                actLandSnap$disp==actLandSnap$disp[row] &
		                actLandSnap$gear==actLandSnap$gear[row] &
		                actLandSnap$port==actLandSnap$port[row] &
		                actLandSnap$mcat==actLandSnap$mcat[row]
		        ,]
		        #
		        act = act[!is.na(act$wc),]
		        if( nrow(act)==0 ){ next } #i=i+1; #appearantly no samples
			
			#
			borrowQtr = docY[
				docY$qtrAct==actLandSnap$qtr[row]   &
                                docY$dispAct==actLandSnap$disp[row] &
                                docY$gearAct==actLandSnap$gear[row] &
                                docY$mcatAct==actLandSnap$mcat[row] &
                                docY$portAct==actLandSnap$port[row] 	
                        , "qtrUse"]	
			
		        #make a filler
		        fill = act[act$qtr==borrowQtr,]
		        fill$x = actLandSnap$x[row]
		        fill$qtr = actLandSnap$qtr[row]
			
		        #add the fill to actLand
		        actLand = rbind(actLand, fill)					
		}
		#remove the null rows all at the end so as to avoid messing with indexing
		actLand = actLand[!is.na(actLand$wc),]
		actLandB = actLand[,-7] #get this copy for use with Borrow, -7 should remove the 'x' column
		#expand
		actLand$x = actLand$x*actLand$wc
		#regigger names and stuff
		actLand$source = 'A'
		colnames(actLand) = c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', '1', 'spp', 'source')
		actLand = actLand[c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp', '1')]
		##glomming actuals (with borrowed qtr) onto output variable
		#expLand = rbind(expLand, actLand)
		
		#Port Borrowing Expand
		
		#use borrow tags to look up spcomps for borrows and then expand the borrows
		#at this point !expanduseTot$source%in%c('N', 'A') represents port borrows
		borrLand = expanduseTot[!expanduseTot$source%in%c('N', 'A'),c('disp', 'port', 'gear', 'mcat', 'qtr', 'x', 'source', 'borrPort', 'borrQtr')]	
		#renaming 'borrPort' as 'port' (and 'borrQtr' as 'qtr') allows us to merge borrLand with the borrowed qtr actLandB 
		colnames(borrLand) = c('disp', 'actPort', 'gear', 'mcat', 'actQtr', 'x', 'source', 'port', 'qtr')
		borrLand = merge(borrLand, actLandB, all=T)
		borrLand$x = borrLand$x*borrLand$wc
		#we merged on 'borrPort' (and 'borrQtr' as 'qtr'), but we actually want to display the actual port (and qtr)
		borrLand = borrLand[!is.na(borrLand$x), c('year', 'actQtr', 'disp', 'mcat', 'gear', 'actPort', 'x', 'source', 'spp', 'wc')]
		colnames(borrLand) = c('year', 'qtr', 'disp', 'mcat', 'gear', 'port', 'x', 'source', 'spp', '1') #rename 'actPort' for glomming below
		##glomming borrowed port expansion onto output variable
        	#expLand = rbind(expLand, borrLand)
		
		#Save
		
		#dump nominal species into output file
		expLand  = rbind(expLand, cbind(nomPink, 1)) #start with the nomPink from earlier
		expLand  = rbind(expLand, moreNom)
		#glomming actuals (without and with borrowed qtr) onto output variable
		m = merge(actLand, docY, 
			by.x=c("year", "qtr", "disp", "mcat", "gear", "port"),
			by.y=c("yearAct", "qtrAct", "dispAct", "mcatAct", "gearAct", "portAct")
		)
		m$source[m$qtr!=m$qtrUse] = 'B'
		actLand = m[,c("year", "qtr", "disp", "mcat", "gear", "port", "x", "source", "spp", "1")]
		expLand = rbind(expLand, actLand)
		#glomming borrowed port expansion onto output variable
        	expLand = rbind(expLand, borrLand)
	}
	#clean up colnames
	colnames(expLand) = c('year','qtr','disp','mcat','gear','port','lands', 'source','spp','comp')
	expLand = expLand[,c('year','qtr','disp','mcat','gear','port','source','spp','lands','comp')]
	#reorder rows
	expLand = expLand[order(expLand$year, expLand$mcat, expLand$disp, expLand$gear, expLand$port, expLand$qtr),]
	#
	return( expLand )
}

#
#INHERENTS 
#

#' A default matrix that encodes the default qtr sharing rules from Dons code.
#' 
#' Row index defines the qtr to be filled and column values define the priority of sharing across qtrs for the given row.
qtrMatrix = matrix(
        c(
        2, 3, 4,
        3, 1, 4,
        2, 4, 1,
        3, 2, 1
        ),
4, 3, byrow=T)
colnames(qtrMatrix) = c('first', 'second', 'third')
rownames(qtrMatrix) = 1:4

#Two Ports Away, No Conception
#' A default matrix that encodes the default 2-away port sharing rules (no borrowing across Point Conception).
#' 
#' Row index defines the port to be filled in north to south encoding 
#' c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD'). 
#' Column values define the priority of sharing across ports for the given row.
#' Any value outside of c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD') codes for 'NOMINAL'.
portMatrix2 = matrix(
        c(
        'ERK', 'BRG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'CRS', 'BRG', 'BDG'    , 'NOMINAL', 'NOMINAL',
        'ERK', 'BDG', 'OSF'    , 'CRS'    , 'NOMINAL',
        'OSF', 'BRG', 'MNT'    , 'ERK'    , 'NOMINAL',
        'BDG', 'MNT', 'BRG'    , 'MRO'    , 'NOMINAL',
        'OSF', 'MRO', 'BDG'    , 'NOMINAL', 'NOMINAL',
        'MNT', 'OSF', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OLA', 'OSD', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OSB', 'OSD', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OLA', 'OSB', 'NOMINAL', 'NOMINAL', 'NOMINAL'
        ),
10, 5, byrow=T)
colnames(portMatrix2) = c('first', 'second', 'third', 'fourth', 'fifth')
rownames(portMatrix2) = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')

##
##TEST
##
#
##probably a future argument
#year = 2022 #2018:2022 #2019 #2021 # #2010 #1981:2012 #1981:2021 #2019:2021 #2000:2001 #
#
##issues in calcom in 1980 and before
#
##grep "2010,.,N,147,TWL,CRS,.*" exdoc2010.csv 
##2010,4,N,147,TWL,CRS appears twice. once using qtr 4 to expand and another to use qtr 3 to expand later
##looks like there is a record in sampstrat in qtr 4, but only a samp in qtr 3
##sampstrat summary does not match the actual samples in samp.
#
##2013 error in exDocExpandSppToLand
##Error in `$<-.data.frame`(`*tmp*`, "x", value = 34256) : 
##  replacement has 1 row, data has 0
#
##
#pacfinTix = getPacfinData(year, save=T, fromFile=T)
##
#calcomDat = getCalcomData(year, save=T, fromFile=T)
#
##
#sppExp = estSppComp(pacfinTix, calcomDat, files=T)
#sppExpDoc = estSppCompDoc(pacfinTix, calcomDat) #, doc="exdoc2019NoOLVE.csv")


