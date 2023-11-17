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
usethis::use_data(qtrMatrix, overwrite=T)


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
usethis::use_data(portMatrix2, overwrite=T)

#One Port Away, No Conception
#' A default matrix that encodes the default 1-away port sharing rules (no borrowing across Point Conception).
#' 
#' Row index defines the port to be filled in north to south encoding 
#' c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD'). 
#' Column values define the priority of sharing across ports for the given row.
#' Any value outside of c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD') codes for 'NOMINAL'.
portMatrix1 = matrix(
        c(
        'ERK', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'CRS',     'BRG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'ERK',     'BDG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OSF',     'BRG', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'BDG',     'MNT', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OSF',     'MRO', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'MNT', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OLA', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OSB',     'OSD', 'NOMINAL', 'NOMINAL', 'NOMINAL',
        'OLA', 'NOMINAL', 'NOMINAL', 'NOMINAL', 'NOMINAL'
        ),
10, 5, byrow=T)
colnames(portMatrix1) = c('first', 'second', 'third', 'fourth', 'fifth')
rownames(portMatrix1) = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
usethis::use_data(portMatrix1, overwrite=T)
