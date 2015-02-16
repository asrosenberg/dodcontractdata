## =============================================================================
## Example
## =============================================================================

## Network fun
## require(network)
## require(data.table)
## load_all("~/lib/R/jmisc")
## init_graphics()

## DOD <- read.csv("DOD-2000.csv.bz2")
## setDT(DOD)
## setkey(DOD, "parentdunsnumber", "contractingofficeagencyid")

## Just keep a few columns.
## DOD <- subset(DOD, select=c("agencyid", "baseandalloptionsvalue",
##                        "congressionaldistrict", "contractingofficeagencyid",
##                        "contractingofficeid", "currentcompletiondate",
##                        "dollarsobligated", "dunsnumber", "effectivedate",
##                        "fiscal_year", "mod_agency", "mod_parent", "modnumber",
##                        "parentdunsnumber", "piid",
##                        "placeofperformancecongressionaldistrict",
##                        "ultimatecompletiondate",
##                        "unique_transaction_id", "zipcode"))

## Totals <- DOD[, list(obligated=sum(dollarsobligated)), by=parentdunsnumber]
## summary(Totals)

## Get the top 25%
## Top2pct <- DOD[Totals[obligated >= quantile(obligated, prob=0.98)]]
## Top2pct$obligated <- NULL
## save(Top2pct, file="Top2pct.RData", compress="bzip2")

require(data.table)
require(network)
require(lattice)

load("Top2pct.RData")

## data.table magic
A <- Top2pct[CJ(unique(parentdunsnumber), unique(contractingofficeagencyid)),
             .N, by = .EACHI]
A

## Reshape is slow.
A <- reshape(as.data.frame(A), v.names = "N", idvar = "parentdunsnumber",
             timevar = "contractingofficeagencyid", direction = "wide")

## Convert to network
rownames(A) <- A[,1]
A[,1] <- NULL
A <- as.matrix(A)

## Plot as bipartite network
make_type <- function(adj)
    c(rep("actor", nrow(adj)), rep("group", ncol(adj)))

N <- network(A, directed=FALSE, bipartite=TRUE)
type <- make_type(A)

plot(N, pad=0, edge.col=grey[2], vertex.border=FALSE,
     vertex.cex=ifelse(type == "actor", 0.5, 0.75),
     vertex.col=ifelse(type == "actor", "gray", "red"))
