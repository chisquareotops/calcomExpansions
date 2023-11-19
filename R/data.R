#INHERENTS

#One Port Away, No Conception
#' A default matrix that encodes the default 1-away port sharing rules (no borrowing across Point Conception).
#' 
#' Row index defines the port to be filled in north to south encoding 
#' c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD'). 
#' Column values define the priority of sharing across ports for the given row.
#' Any value outside of c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD') codes for 'NOMINAL'.
"portMatrix1"

#Two Ports Away, No Conception
#' A default matrix that encodes the default 2-away port sharing rules (no borrowing across Point Conception).
#' 
#' Row index defines the port to be filled in north to south encoding 
#' c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD'). 
#' Column values define the priority of sharing across ports for the given row.
#' Any value outside of c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD') codes for 'NOMINAL'.
"portMatrix2"