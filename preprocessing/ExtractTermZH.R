#  source("D:/Utilisateurs/turenne/Dropbox/UIC/LectureTextMining/ExtractTermZH.R")
#
#corpus <- "马英九2008年5月当选为台湾总统时肩负着一项使命：缓和台海两岸的紧张关系。他如今确实取得了进展，这一点几乎没人能否认。 @ 马英九的和睦政策让两岸关系有所解冻——当奉行北京方面斥之为“分裂主义”的台独路线的前任陈水扁在任时，两岸关系非常紧张甚至有些危险。 @ 对于将在明年决定是否让马英九连任的台湾选民而言，问题在于，这种和解的代价是否过高。反对者肯定会说，代价确实过高。他们会说，去年与大陆签署的加强两岸经济合作的一个框架协议，让马英九领导的执政党国民党落入了北京方面设置的陷阱。"
#
#corpus="今天是非常不棒棒的一天"
#
#usage
# library(spacyr)
# spacy_finalize()
# spacy_initialize(model = "zh_core_web_sm")
# x.zh.sr ( corpus, taille=1, MonoPoly=0, DelInclud=1, Dlink=1 ,  Dnum=1, SizeMin=2, SizeMax=3, seuil=0, stp_wrds_zh )
# x.hanzi ( corpus, 3 )
# x.zh.np ( corpus, 1 )

library(ngram)
library(jiebaR)
library(NLP)
library(tm)

Sys.setlocale(category="LC_ALL", locale = "Chinese")


x.hanzi <- function( corpus, seuil ) {
	
	hanzi = strsplit(corpus, "")[[1]]

	hanzi = gsub( " ", "", hanzi)
	hanzi = gsub( "#", "", hanzi)
	hanzi = gsub( "%", "", hanzi)
	hanzi = gsub( "&", "", hanzi)
	hanzi = gsub( "'", "", hanzi)
	hanzi = gsub( "*", "", hanzi)
	hanzi = gsub( "+", "", hanzi)
	hanzi = gsub( ",", "", hanzi)
	hanzi = gsub( "-", "", hanzi)
	hanzi = gsub( "/", "", hanzi)
	hanzi = gsub( "0", "", hanzi)
	hanzi = gsub( "1", "", hanzi)
	hanzi = gsub( "2", "", hanzi)
	hanzi = gsub( "3", "", hanzi)
	hanzi = gsub( "4", "", hanzi)
	hanzi = gsub( "5", "", hanzi)
	hanzi = gsub( "6", "", hanzi)
	hanzi = gsub( "7", "", hanzi)
	hanzi = gsub( "8", "", hanzi)
	hanzi = gsub( "9", "", hanzi)
	hanzi = gsub( ":", "", hanzi)
	hanzi = gsub( "<", "", hanzi)
	hanzi = gsub( "<U+2022>", "", hanzi)
	hanzi = gsub( "=", "", hanzi)
	hanzi = gsub( ">", "", hanzi)
	hanzi = gsub( "?", "", hanzi)
	hanzi = gsub( "@", "", hanzi)
	hanzi = gsub( "\\(", "", hanzi)
	hanzi = gsub( "\\)", "", hanzi)
	hanzi = gsub( "_", "", hanzi)
	hanzi = gsub( "a", "", hanzi)
	hanzi = gsub( "b", "", hanzi)
	hanzi = gsub( "B", "", hanzi)
	hanzi = gsub( "C", "", hanzi)
	hanzi = gsub( "c", "", hanzi)
	hanzi = gsub( "D", "", hanzi)
	hanzi = gsub( "d", "", hanzi)
	hanzi = gsub( "e", "", hanzi)
	hanzi = gsub( "E", "", hanzi)
	hanzi = gsub( "F", "", hanzi)
	hanzi = gsub( "f", "", hanzi)
	hanzi = gsub( "G", "", hanzi)
	hanzi = gsub( "g", "", hanzi)
	hanzi = gsub( "h", "", hanzi)
	hanzi = gsub( "H", "", hanzi)
	hanzi = gsub( "i", "", hanzi)
	hanzi = gsub( "I", "", hanzi)
	hanzi = gsub( "j", "", hanzi)
	hanzi = gsub( "J", "", hanzi)
	hanzi = gsub( "k", "", hanzi)
	hanzi = gsub( "K", "", hanzi)
	hanzi = gsub( "l", "", hanzi)
	hanzi = gsub( "L", "", hanzi)
	hanzi = gsub( "M", "", hanzi)
	hanzi = gsub( "m", "", hanzi)
	hanzi = gsub( "n", "", hanzi)
	hanzi = gsub( "N", "", hanzi)
	hanzi = gsub( "O", "", hanzi)
	hanzi = gsub( "o", "", hanzi)
	hanzi = gsub( "p", "", hanzi)
	hanzi = gsub( "P", "", hanzi)
	hanzi = gsub( "Q", "", hanzi)
	hanzi = gsub( "q", "", hanzi)
	hanzi = gsub( "r", "", hanzi)
	hanzi = gsub( "R", "", hanzi)
	hanzi = gsub( "s", "", hanzi)
	hanzi = gsub( "S", "", hanzi)
	hanzi = gsub( "T", "", hanzi)
	hanzi = gsub( "t", "", hanzi)
	hanzi = gsub( "u", "", hanzi)
	hanzi = gsub( "U", "", hanzi)
	hanzi = gsub( "v", "", hanzi)
	hanzi = gsub( "V", "", hanzi)
	hanzi = gsub( "w", "", hanzi)
	hanzi = gsub( "W", "", hanzi)
	hanzi = gsub( "X", "", hanzi)
	hanzi = gsub( "x", "", hanzi)
	hanzi = gsub( "Y", "", hanzi)
	hanzi = gsub( "y", "", hanzi)
	hanzi = gsub( "z", "", hanzi)
	hanzi = gsub( "Z", "", hanzi)
	hanzi = gsub( "á", "", hanzi)
	hanzi = gsub( "è", "", hanzi)
	hanzi = gsub( "í", "", hanzi)
	hanzi = gsub( "ü", "", hanzi)
	hanzi = gsub( "—", "", hanzi)
	hanzi = gsub( "‘", "", hanzi)
	hanzi = gsub( "’", "", hanzi)
	hanzi = gsub( "…", "", hanzi)
	hanzi = gsub( "、", "", hanzi)
	hanzi = gsub( "。", "", hanzi)
	hanzi = gsub( "《", "", hanzi)
	hanzi = gsub( "》", "", hanzi)
	hanzi = gsub( "！", "", hanzi)
	hanzi = gsub( "％", "", hanzi)
	hanzi = gsub( "（", "", hanzi)
	hanzi = gsub( "）", "", hanzi)
	hanzi = gsub( "，", "", hanzi)
	hanzi = gsub( "／", "", hanzi)
	hanzi = gsub( "：", "", hanzi)
	hanzi = gsub( "；", "", hanzi)
	hanzi = gsub( "？", "", hanzi)
	hanzi = gsub( "８", "", hanzi)

	listHanzi = table( hanzi )
	listhanzi_sort = sort(listHanzi[ listHanzi >= seuil ], decreasing = T)

	return( listhanzi_sort );
}

x.zh.sr <- function( corpus, taille, MonoPoly, DelInclud, Dlink ,  Dnum, SizeMin, SizeMax, seuil, stp_wrds ) {

	if( MonoPoly == 0 && taille == 1 ) {stop( "select size >1 for repeated segments only"); }

	#corpus = iconv( corpus,  to="UTF-8")
	corpus = tolower(corpus)

	if( Dlink == 1 ) corpus = gsub("http.*\\s|http.*$|.[a-zA-Z]{2,3}/.+\\s|.[a-zA-Z]{2,3}/.+$", " ", corpus)

	corpus = gsub("(\\{|\\}|\\[|\\||\\'|\\(|\\)|\\*|\\.|\\,|\\;|\\:|\\!|\\?|\\])", " " , corpus ); 
	if( Dnum == 1 )  corpus = gsub("\\s[0-9]*\\s|^[0-9]*\\s|\\s[0-9]*$"      , " " , corpus ); 
	
	tailleA= 0; tailleB = sum(nchar(corpus));
	#while( (tailleB - tailleA) != 0) {
	#	tailleB = sum(nchar(corpus));
	#	SMin = 1; SMax = (SizeMin-1)
	#	corpus = gsub( paste0("^([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}$|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s") ," ", corpus )
	#	SMin = (SizeMax+1); SMax = 255
	#	corpus = gsub( paste0("^([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}$|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s") ," ", corpus )
	#	tailleA = sum(nchar(corpus));
	#}
	corpus = gsub("\\s+"," ", corpus )

	engine1 = worker()			#open a worker
	resu = segment(corpus, engine1)		#make segmentation
	resu					# 今天 是 非常 不 棒棒 的 一天

	n=length(resu);

	corpus_new = paste0(resu, collapse=" ")
	k=0;aa = sapply(1:(n-k+1), function(x) { paste0(resu[x:(x+k)], collapse=" ") })
	k=1;bb = sapply(1:(n-k+1), function(x) { paste0(resu[x:(x+k)], collapse=" ") })
	k=2;cc = sapply(1:(n-k+1), function(x) { paste0(resu[x:(x+k)], collapse=" ") })
	k=3;dd = sapply(1:(n-k+1), function(x) { paste0(resu[x:(x+k)], collapse=" ") })
	k=4;ee = sapply(1:(n-k+1), function(x) { paste0(resu[x:(x+k)], collapse=" ") })
	k=5;ff = sapply(1:(n-k+1), function(x) { paste0(resu[x:(x+k)], collapse=" ") })

	catLists =  c()
	if( taille == 1 ) bb=cc=dd=ee=ff=c()
	if( taille == 2 ) cc=dd=ee=ff=c()
	if( taille == 3 ) dd=ee=ff=c()
	if( taille == 4 ) ee=ff=c()
	if( taille == 5 ) ff=c()

	catLists = unlist ( list( aa,bb,cc,dd,ee,ff) ) 

	ListSeg  = table( catLists )
	catLists = names( ListSeg[ ListSeg > seuil ] )

	catLists =  cleanTerm( catLists, stp_wrds)  

	IndNouvList = grep(".*\\s.*", catLists, value = FALSE )
	NouvList       = catLists[ IndNouvList ]

	mm = sapply(NouvList, grep , NouvList)
	ListT = sapply(mm, length)

	if( length(IndNouvList) != 0 ) { ListeMonolexical     = unique( catLists[-IndNouvList]  ) } 	else { ListeMonolexical     = unique( catLists ) }

	ListeMaxiPolylexical = names( ListT )
	if( DelInclud == 1 ) ListeMaxiPolylexical = names( ListT[ ListT == 1 ] ) 
	ListeGlobale = ListeMaxiPolylexical
	if( MonoPoly == 1 ) ListeGlobale = c( ListeMonolexical , ListeMaxiPolylexical )
	if( MonoPoly == 1 && taille == 1 ) ListeGlobale = c( ListeMonolexical )

	ListeGlobale_new = gsub( " ", "", ListeGlobale)

	gg = sapply( paste0( ListeGlobale_new ) , grep , paste0( " ", corpus, " ") )
	FreqDoc = sapply(gg, length)

	ListSegRep =  sort( FreqDoc[ FreqDoc > seuil ] )
	names(ListSegRep) =  gsub("^\\s+|\\s+$", "", names(ListSegRep)) 

	return (ListSegRep)

} # end fonction

stp_wrds_zh= c( "的" , "不" , "我" , "你" ,
"。" , "，" , "than" , "too" , "NA", "just", "still", "got", "very" , "can", "will"
);


cleanTerm <- function( liste=c() , stp_wrds = c()) {

	stp_wrds= sort( unique(stp_wrds) )

	patR <- paste0( "(\\s", paste0(stp_wrds, collapse="$|\\s"), "$|[[:punct:]]$)" )

	patL <- paste0( "(^[[:punct:]]|^",	paste0(stp_wrds, collapse="\\s|^"),  "\\s)" )    

	patC <- paste0( "(^",   paste0(stp_wrds, collapse="$|^"),  ")")    

	#LLL= cleanborn( liste ,  patR,  patL, patC );

	patR2 <- paste0( "\\s", paste0(stp_wrds, collapse="$|\\s"), "$|" )
	patL2 <- paste0( "^",	paste0(stp_wrds, collapse="\\s|^"),  "\\s|" )    
	patC2 <- paste0( "^",   paste0(stp_wrds, collapse="$|^") ,  "$|")    
	patB2 = "^\\s+|\\s+$"
	pat  <- paste0( "(", paste0(patR2, patL2, patC2, patB2, collapse="|"),  ")" )    

	LLL= cleanborn2( liste ,  pat );

	return (LLL)

}

cleanborn <- function( liste=c(), pat1=c(), pat2=c(), pat3=c()  ) {

	tailleB = sum(nchar(liste));

	liste = gsub(pat1,"",liste)
	liste = gsub("^\\s+|\\s+$", "", liste)

	liste = gsub(pat2,"",liste)
	liste = gsub("^\\s+|\\s+$", "", liste)

	liste = gsub(pat3,"",liste)

	liste = gsub(".*,\\s.*|.*\\s,.*|.*,.*","",liste)

	NewList = liste[liste != ""]

	tailleA = sum(nchar(NewList));
	if( (tailleB - tailleA) != 0 )  cleanborn( NewList, pat1, pat2, pat3)
	else return ( liste[liste != ""] )  

}

cleanborn2 <- function( liste=c(), pat=c()  ) {

	tailleB = sum(nchar(liste));

	liste = gsub(pat,"",liste)
	liste = gsub("^\\s+|\\s+$", "", liste)

	tailleA = sum(nchar(liste));

	NewList = unique( liste[liste != ""] )

	if( (tailleB - tailleA) > 0 )  cleanborn2( NewList, pat )
	else return ( NewList )  

}

#*********************************************************************************************************

x.zh.np <- function( corpus, seuil ) {

newCol_s_sp_np = c()
newCol_s_np   = c()
df_s_sp = corpus

for (idf in 1:(length( df_s_sp  )) ) {
 
 s=df_s_sp[idf][[1]]
 
 indexA = 0
 indexB = 0
 
 a3 <- spacy_parse( s , dependency = TRUE)

 k = 0
 
 for (i in 1:(nrow(a3)-2)) {
 if( i && length(a3$token_id[[i]]) ) { 
 
  if ( length(a3$pos[i]) && length(a3$pos[i+1]) ){
  if (( a3$pos[i] == "NOUN" && a3$pos[i+1] == "NOUN") | 
 ( a3$pos[i] == "NOUN" && a3$pos[i+1] == "NOUN") | 
 ( a3$pos[i] == "NOUN" && a3$pos[i+1] == "NOUN") | 
 ( a3$pos[i] == "ADJ" && a3$pos[i+1] == "NOUN") | 
 ( a3$pos[i] == "ADJ" && a3$pos[i+1] == "NOUN") | 
 ( a3$pos[i] == "ADJ" && a3$pos[i+1] == "NOUN")) {
  k = k+1
  indexA[k] = i
  indexB[k] = i+1
  }} #endif
 
  if ( length(a3$pos[i]) && length(a3$pos[i+1]) && length(a3$pos[i+2]) ){
  if ( ( a3$pos[i] == "NOUN" && a3$pos[i+1] == "ADP" && a3$pos[i+2] == "NOUN" ) | 
     ( a3$pos[i] == "NOUN" && a3$pos[i+1] == "ADP" && a3$pos[i+2] == "NOUN" ) | 
     ( a3$pos[i] == "NOUN" && a3$pos[i+1] == "ADP" && a3$pos[i+2] == "NOUN")) {
  k = k+1
  indexA[k] = i
  indexB[k] = i+2
  }} #endif
 
 } #endif
 } #endfor
 
  s_np =""
  if (k){
  for (i in 1:k) {
  str =  paste(a3$token[indexA[i]:indexB[i]], collapse=" ")

 str = gsub(" ", "_", str)
 s_np <- paste(s_np, str, collapse = " ")
 
  }} #endfor i
 
 s_np  <- as.String(s_np)
 s_sp  <- as.String(s)
 s_sp_np <- paste(s , "\n", s_np, collapse = " ")
 s_sp_np <- as.String(s_sp_np)
 k
 s_sp
 s_np
 s_sp_np
 newCol_s_sp_np[idf] <- s_sp_np
 newCol_s_np[idf]  <- s_np
 
 } #endfor idf

myTdm <- TermDocumentMatrix( Corpus(VectorSource( newCol_s_np )) , control = list(minWordLength=2))
ListSeg =  rowSums( as.matrix(myTdm) )

ListSegRep =  sort( ListSeg[ ListSeg >= seuil ] )

	return (ListSegRep)

} # end fonction

