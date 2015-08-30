setwd('~/tmp/rdata')
load('run.masterdata.rdata')

nodes <- subset(ref_objects, show=='yes')

uniqueAndSorted <- function(col){
    u <- unique(as.character(col))      
    sort(u) 
}

fix.status <- function(col){
   col <- as.character(col)
   col[col=="N/A"] <- "Active"
   col[col=="UC"] <-  "Active"
   col
}

countries <- uniqueAndSorted(nodes$country.name)
level <- uniqueAndSorted(nodes$level)
nodes$status <- fix.status(nodes$status)

tree <- lapply(countries, function(country){
        level.list <- lapply(level[c(4,1,3,5,2)], function(alevel){
                energy.sources <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel)$energy.source)
                energy.sources.list <- lapply(energy.sources, function(asource){
                    status <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource)$status)
                    status.list <- lapply(status, function(astatus){
                           names <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource & status == astatus)$name)        
                           return(names)
                    })
                    names(status.list) <- status
                    return(status.list)
                })
                names(energy.sources.list) <- energy.sources
                return(energy.sources.list)
        })
        names(level.list) <- level[c(4,1,3,5,2)]
        return(level.list)
})
names(tree) <- countries




