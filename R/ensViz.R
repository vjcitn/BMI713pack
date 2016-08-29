
ensViz = function( eid, sym="CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", ideo=TRUE, genome="hg19", ...) {
#
# use Gviz to display a gene model (exon layout per transcript)
# derived from Ensembl
#
# a 'transcript' column must be present in any GRanges passed
# to GeneRegionTrack to get this type of display
#
# if ideo=TRUE, an ideogram track will be fetched and displayed
# this will be cached at the session level, first run can take
# some time
#
  require(edbnm, character.only=TRUE)
  edb = get(edbnm)
  if (!missing(eid)) {
    sym = select(edb, keys=eid, keytype="GENEID", columns="GENENAME")
    ex = ensembldb::exonsBy(edb, by="tx", filter=GeneidFilter(eid))
    }
  else if (!missing(sym) || is.character(sym)) {
    eid = select(edb, keys=sym, keytype="GENENAME", columns="GENEID")
    ex = ensembldb::exonsBy(edb, by="tx", filter=GeneidFilter(eid$GENEID[1]))
    }
  ex = unlist(ex)
  ex$transcript = ex$tx_id
  seqlevelsStyle(ex) = "UCSC"
  cn = as.character(seqnames(ex))[1] # check representative?
  if (!all(cn == as.character(seqnames(ex)))) warning("multiple seqnames found")
  itr = NULL
  if (ideo) itr = IdeogramTrack(chromosome=cn, genome=genome)
  plotTracks(c(itr, GenomeAxisTrack(), GeneRegionTrack(ex, name=paste0(sym, 
    " (", cn, ")"),
     transcriptAnnotation="transcript")))
}

EnsScanBamParam = function(eid, sym="CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", extra=1e4, ...) {
 em = EnsModel(eid, sym, edbnm)
 ScanBamParam(which=range(em), ...)
}
  

#BamTrack = function(bf, isPaired=FALSE, type="coverage", param=NULL, ...) {
# ga = readGAlignments(bf, param=param)
# gag = as(ga, "GRanges")
# AlignmentsTrack(gag, isPaired=isPaired, type=type, ...)
#}

EnsRegionTrack = 
function (eid, sym = "CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", 
    ideo = TRUE, genome = "hg19", ...) 
{
 #   require(edbnm, character.only = TRUE)
 #   edb = get(edbnm)
 #   if (!missing(eid)) {
 #       sym = select(edb, keys = eid, keytype = "GENEID", columns = "GENENAME")
 #       ex = ensembldb::exonsBy(edb, by = "tx", filter = GeneidFilter(eid))
 #   }
 #   else if (!missing(sym) || is.character(sym)) {
 #       eid = select(edb, keys = sym, keytype = "GENENAME", columns = "GENEID")
 #       ex = ensembldb::exonsBy(edb, by = "tx", filter = GeneidFilter(eid$GENEID[1]))
 #   }
 #   ex = unlist(ex)
 #   ex$transcript = ex$tx_id
 #   seqlevelsStyle(ex) = "UCSC"
 #   cn = as.character(seqnames(ex))[1]
 #   if (!all(cn == as.character(seqnames(ex)))) 
 #       warning("multiple seqnames found")
 ex = EnsModel( eid=eid, sym=sym, edbnm=edbnm, ideo=ideo, genome=genome, ...)
 cn = as.character(seqnames(ex))[1] # check representative?
 if (!all(cn == as.character(seqnames(ex)))) warning("multiple seqnames found")
 GeneRegionTrack(ex, 
        name = paste0(sym, " (", cn, ")"), transcriptAnnotation = "transcript")
}

EnsModel = function( eid, sym="CRISPLD2", 
     edbnm = "EnsDb.Hsapiens.v75", ideo=TRUE, genome="hg19", ...) {
  require(edbnm, character.only=TRUE)
  edb = get(edbnm)
  if (!missing(eid)) {
    sym = select(edb, keys=eid, keytype="GENEID", columns="GENENAME")
    ex = ensembldb::exonsBy(edb, by="tx", filter=GeneidFilter(eid))
    }
  else if (!missing(sym) || is.character(sym)) {
    eid = select(edb, keys=sym, keytype="GENENAME", columns="GENEID")
    ex = ensembldb::exonsBy(edb, by="tx", filter=GeneidFilter(eid$GENEID[1]))
    }
  ex = unlist(ex)
  ex$transcript = ex$tx_id
  seqlevelsStyle(ex) = "UCSC"
  cn = as.character(seqnames(ex))[1] # check representative?
  if (!all(cn == as.character(seqnames(ex)))) warning("multiple seqnames found")
  ex
}
