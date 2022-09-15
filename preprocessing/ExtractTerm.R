#  source("C:/Users/nicolas/Dropbox/projet_EXIA_PEPS_ParisEst/CodeR/ExtractTerm.R")
#  source("D:/Utilisateurs/turenne/Dropbox/projet_EXIA_PEPS_ParisEst/CodeR/ExtractTerm.R")

library(ngram)
library(tm)
library(NLP)

x.sr <- function( corpus, taille, MonoPoly, DelInclud, Dlink ,  Dnum, SizeMin, SizeMax, seuil, stp_wrds ) {
	
	#corpus = iconv( corpus,  to="UTF-8")
	corpus = tolower(corpus)
	if( Dlink == 1 ) corpus = gsub("http.*\\s|http.*$|.[a-zA-Z]{2,3}/.+\\s|.[a-zA-Z]{2,3}/.+$", " ", corpus);

	corpus = gsub("(\\{|\\}|\\[|\\||\\�|\\�|\\'|\\(|\\)|\\*|\\.|\\,|\\;|\\:|\\!|\\?|\\�|\\])", " " , corpus); 
	if( Dnum == 1 )  corpus = gsub("\\s[0-9]*\\s|^[0-9]*\\s|\\s[0-9]*$"      , " " , corpus ); 
	
	tailleA= 0; tailleB = sum(nchar(corpus));
	while( (tailleB - tailleA) != 0) {
		tailleB = sum(nchar(corpus));
		SMin = 1; SMax = (SizeMin-1)
		corpus = gsub( paste0("^([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}$|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s") ," ", corpus )
		SMin = (SizeMax+1); SMax = 255
		corpus = gsub( paste0("^([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}$|\\s([[:alnum:]]|[[:punct:]]){",SMin,",",SMax,"}\\s") ," ", corpus )
		tailleA = sum(nchar(corpus));
	}
	corpus = gsub("\\s+"," ", corpus )

	aa = lapply(corpus, function(x) return(tryCatch(get.ngrams( ngram (x , n =1) ), error=function(e) NULL) )  )
	bb = lapply(corpus, function(x) return(tryCatch(get.ngrams( ngram (x , n =2) ), error=function(e) NULL) )  )
	cc = lapply(corpus, function(x) return(tryCatch(get.ngrams( ngram (x , n =3) ), error=function(e) NULL) )  )
	dd = lapply(corpus, function(x) return(tryCatch(get.ngrams( ngram (x , n =4) ), error=function(e) NULL) )  )
	ee = lapply(corpus, function(x) return(tryCatch(get.ngrams( ngram (x , n =5) ), error=function(e) NULL) )  )
	ff = lapply(corpus, function(x) return(tryCatch(get.ngrams( ngram (x , n =6) ), error=function(e) NULL) )  )
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
	NouvList       = catLists[IndNouvList]

	mm = sapply(NouvList, grep , NouvList)
	ListT = sapply(mm, length)

	if( length(IndNouvList) != 0 ) { ListeMonolexical     = unique( catLists[-IndNouvList]  ) } 	else { ListeMonolexical     = unique( catLists ) }

	ListeMaxiPolylexical = names( ListT )
	if( DelInclud == 1 ) ListeMaxiPolylexical = names( ListT[ ListT == 1 ] ) 
	ListeGlobale = ListeMaxiPolylexical
	if( MonoPoly == 1 ) ListeGlobale = c( ListeMonolexical , ListeMaxiPolylexical )

	gg = sapply( paste0( " ", ListeGlobale , " ") , grep , paste0( " ", corpus, " ") )
	FreqDoc = sapply(gg, length)

	ListSegRep =  sort( FreqDoc[ FreqDoc > seuil ] )
	names(ListSegRep) =  gsub("^\\s+|\\s+$", "", names(ListSegRep)) 

	return (ListSegRep)
} # end fonction

stp_wrds_fr= c("au","aux" ,"avec","ce","ces", "dans","de","des","du",
"le","leur","lui","ma","mais","me","m�me","mes","moi","mon","ne","nos","notre","nous", "on","ou",
"par","pas","pour", "qu","que","qui","sa","se","ses","son","sur","ta","te","tes","toi","ton", 
"tu","un","une","vos","votre","vous", "c", "d", "j", "l", "�", "m", "n", "s", "t", "y",
"�t�","�t�e", "�t�es","�t�s", "�tant","suis","es","est","sommes","�tes",  "sont","serai", "seras", "sera","serons","serez",
"seront","serais","serait","serions","seriez","seraient","�tais", "�tait", "�tions","�tiez", "�taient","fus","fut","f�mes", "f�tes", "furent",
"sois","soit","soyons","soyez", "soient","fusse","fusses","f�t","fussions","fussiez","fussent","ayant","eu","eue","eues", "eus", 
"ai","as","avons","avez", "ont","aurai","auras","aura","aurons","aurez","auront","aurais","aurait","aurions","auriez","auraient",
"avais","avait","avions","aviez","avaient","eut","e�mes","e�tes","eurent","aie","aies", "ait","ayons","ayez", "aient","eusse",
"eusses","e�t","eussions","eussiez","eussent","ceci", "cela", "cel�", "cet","cette","ici","ils","les","leurs","quel", "quels",
"quelle","quelles","sans", "soi","enfin", "a", "faut",
"elle","en","et","eux","il","je","la","au", "a" , "fi", 
"enfin", "i", "ee", "et", "a", "le",
"c'est" , "NA" ,
"-" , "." , "'s" , ":" , ">" , "!", "<", "+" , "''", "?", ";",
#"|"  , "''", "?", ";",
#"#"  
"/font" , "font" , "'s" ,
"peuvent" , "près" , "d'une" , "après" , "environ",
"cccccc" , "color=" , "s'en" , "déjà" , "étaient" , "d'une" , 
"d'autres" , "comme" , "vers" , "d'un" , "font" , "donc" , "où" , "entre" , 
"va" , "chez" , "https" , "http" , "ça"  , "étaient" , "été", "à",
"aussi" , "veut" , "doit" , "lors", "amod", "compound", "nmod"
);

stp_wrds_en= c( "i" , "me" , "my" , "myself" , "we" , "our" , "ours" , "ourselves" , "you" , "your" , "yours" , "yourself" , "yourselves"  ,
"he" , "him" , "his" , "himself" , "she" , "her" , "hers" , "herself" , "it" , "its" , "itself" , "they" , "them" , 
"their" , "theirs" , "themselves" , "what" , "which" , "who" , "whom" , "this" , "that" , "these" , "those" , "am" , "is" ,
"are" , "was" , "were" , "be" , "been" , "being" , "have" , "has" , "had" , "having" , "do" , "does" , "did" ,
"doing" , "would" , "should" , "could" , "ought" , "i'm" , "you're" , "he's" , "she's" , "it's" , "we're" , "they're" , "i've" , 
"you've" , "we've" , "they've" , "i'd" , "you'd" , "he'd" , "she'd" , "we'd" , "they'd" , "i'll" , "you'll" , "he'll" , "she'll" ,
"we'll" , "they'll" , "isn't" , "aren't" , "wasn't" , "weren't" , "hasn't" , "haven't" , "hadn't" , "doesn't" , "don't" , "didn't" , "won't" ,
"wouldn't" , "shan't" , "shouldn't" , "can't" , "cannot" , "couldn't" , "mustn't" , "let's" , "that's" , "who's" , "what's" , "here's" , "there's" ,
"when's" , "where's" , "why's" , "how's" , "a" , "an" , "the" , "and" , "but" , "if" , "or" , "because" , "as" ,
"until" , "while" , "of" , "at" , "by" , "for" , "with" , "about" , "against" , "between" , "into" , "through" , "during" ,
"before" , "after" , "above" , "below" , "to" , "from" , "up" , "down" , "in" , "out" , "on" , "off" , "over" ,
"under" , "again" , "further" , "then" , "once" , "here" , "there" , "when" , "where" , "why" , "how" , "all" , "any" ,
"both" , "each" , "few" , "more" , "most" , "other" , "some" , "such" , "no" , "nor" , "not" , "only" , "own" ,
"same" , "so" , "than" , "too" , "NA", "just", "still", "got", "very" , "can", "will"
);

cleanTerm <- function( liste=c() , stp_wrds = c()) {

	stp_wrds= sort( unique(stp_wrds) )

	patR <- paste0( "(\\s", paste0(stp_wrds, collapse="$|\\s"), "$|[[:punct:]]$|�$)" )

	patL <- paste0( "(^[[:punct:]]|^�|^",	paste0(stp_wrds, collapse="\\s|^"),  "\\s)" )    

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

x.np <- function( corpus, seuil ) {

newCol_s_sp_np = c()
newCol_s_np   = c()
df_s_sp = corpus

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

for (idf in 1:(length( df_s_sp  )) ) {
 
 s=df_s_sp[idf][[1]]
 
 a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
 a3 <- annotate(s, pos_tag_annotator, a2)
 
 indexA = 0
 indexB = 0
 
 k = 0
 
 for (i in 1:(length(a3)-2)) {
 if( i && length(a3$features[[i]]) ) { 
 
  if ( length(a3$features[[i]]$POS) && length(a3$features[[i+1]]$POS) ){
  if (( a3$features[[i]]$POS == "NNP" && a3$features[[i+1]]$POS == "NNP") | 
 ( a3$features[[i]]$POS == "NN" && a3$features[[i+1]]$POS == "NNP") | 
 ( a3$features[[i]]$POS == "NN" && a3$features[[i+1]]$POS == "NN") | 
 ( a3$features[[i]]$POS == "JJ" && a3$features[[i+1]]$POS == "NNP") | 
 ( a3$features[[i]]$POS == "JJ" && a3$features[[i+1]]$POS == "NNS") | 
 ( a3$features[[i]]$POS == "JJ" && a3$features[[i+1]]$POS == "NN")) {
  k = k+1
  indexA[k] = a3$start[i]
  indexB[k] = a3$end[i+1]
  }} #endif
 
  if ( length(a3$features[[i]]$POS) && length(a3$features[[i+1]]$POS) && length(a3$features[[i+2]]$POS) ){
  if ( ( a3$features[[i]]$POS == "NNS" && a3$features[[i+1]]$POS == "IN" && a3$features[[i+2]]$POS == "NN" ) | 
     ( a3$features[[i]]$POS == "NN" && a3$features[[i+1]]$POS == "IN" && a3$features[[i+2]]$POS == "NN" ) | 
     ( a3$features[[i]]$POS == "NNS" && a3$features[[i+1]]$POS == "IN" && a3$features[[i+2]]$POS == "NNS")) {
  k = k+1
  indexA[k] = a3$start[i]
  indexB[k] = a3$end[i+2]
  }} #endif
 
 } #endif
 } #endfor
  
  s_np =""
  if (k){
  for (i in 1:k) {
 str = substring(s,indexA[i],indexB[i])
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

ListSegRep =  sort( ListSeg[ ListSeg > seuil ] )

	return (ListSegRep)

} # end fonction

