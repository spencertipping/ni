
# PARSER /lambda
	A bracketed lambda function in context ''

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  </series>
	  ']'
	) -> {$$_[1]}

# PARSER /op
	A single operator in the context ''

## DEFINITION
	(
	| (
	  | ''7z://' /.*/ -> {resource_quote_op "7z://$_"}
	  | ''7zentry://' /.*/ -> {resource_quote_op "7zentry://$_"}
	  | ''aaws://' /.*/ -> {resource_quote_op "aaws://$_"}
	  | ''aawt://' /.*/ -> {resource_quote_op "aawt://$_"}
	  | ''abws://' /.*/ -> {resource_quote_op "abws://$_"}
	  | ''abwt://' /.*/ -> {resource_quote_op "abwt://$_"}
	  | ''acews://' /.*/ -> {resource_quote_op "acews://$_"}
	  | ''acewt://' /.*/ -> {resource_quote_op "acewt://$_"}
	  | ''adyws://' /.*/ -> {resource_quote_op "adyws://$_"}
	  | ''adywt://' /.*/ -> {resource_quote_op "adywt://$_"}
	  | ''afws://' /.*/ -> {resource_quote_op "afws://$_"}
	  | ''afwt://' /.*/ -> {resource_quote_op "afwt://$_"}
	  | ''akws://' /.*/ -> {resource_quote_op "akws://$_"}
	  | ''akwt://' /.*/ -> {resource_quote_op "akwt://$_"}
	  | ''alsws://' /.*/ -> {resource_quote_op "alsws://$_"}
	  | ''alswt://' /.*/ -> {resource_quote_op "alswt://$_"}
	  | ''amws://' /.*/ -> {resource_quote_op "amws://$_"}
	  | ''amwt://' /.*/ -> {resource_quote_op "amwt://$_"}
	  | ''angws://' /.*/ -> {resource_quote_op "angws://$_"}
	  | ''angwt://' /.*/ -> {resource_quote_op "angwt://$_"}
	  | ''anws://' /.*/ -> {resource_quote_op "anws://$_"}
	  | ''anwt://' /.*/ -> {resource_quote_op "anwt://$_"}
	  | ''arcws://' /.*/ -> {resource_quote_op "arcws://$_"}
	  | ''arcwt://' /.*/ -> {resource_quote_op "arcwt://$_"}
	  | ''arws://' /.*/ -> {resource_quote_op "arws://$_"}
	  | ''arwt://' /.*/ -> {resource_quote_op "arwt://$_"}
	  | ''aryws://' /.*/ -> {resource_quote_op "aryws://$_"}
	  | ''arywt://' /.*/ -> {resource_quote_op "arywt://$_"}
	  | ''arzws://' /.*/ -> {resource_quote_op "arzws://$_"}
	  | ''arzwt://' /.*/ -> {resource_quote_op "arzwt://$_"}
	  | ''astws://' /.*/ -> {resource_quote_op "astws://$_"}
	  | ''astwt://' /.*/ -> {resource_quote_op "astwt://$_"}
	  | ''asws://' /.*/ -> {resource_quote_op "asws://$_"}
	  | ''aswt://' /.*/ -> {resource_quote_op "aswt://$_"}
	  | ''atjws://' /.*/ -> {resource_quote_op "atjws://$_"}
	  | ''atjwt://' /.*/ -> {resource_quote_op "atjwt://$_"}
	  | ''avkws://' /.*/ -> {resource_quote_op "avkws://$_"}
	  | ''avkwt://' /.*/ -> {resource_quote_op "avkwt://$_"}
	  | ''avws://' /.*/ -> {resource_quote_op "avws://$_"}
	  | ''avwt://' /.*/ -> {resource_quote_op "avwt://$_"}
	  | ''awaws://' /.*/ -> {resource_quote_op "awaws://$_"}
	  | ''awawt://' /.*/ -> {resource_quote_op "awawt://$_"}
	  | ''ayws://' /.*/ -> {resource_quote_op "ayws://$_"}
	  | ''aywt://' /.*/ -> {resource_quote_op "aywt://$_"}
	  | ''azbws://' /.*/ -> {resource_quote_op "azbws://$_"}
	  | ''azbwt://' /.*/ -> {resource_quote_op "azbwt://$_"}
	  | ''azws://' /.*/ -> {resource_quote_op "azws://$_"}
	  | ''azwt://' /.*/ -> {resource_quote_op "azwt://$_"}
	  | ''banws://' /.*/ -> {resource_quote_op "banws://$_"}
	  | ''banwt://' /.*/ -> {resource_quote_op "banwt://$_"}
	  | ''barws://' /.*/ -> {resource_quote_op "barws://$_"}
	  | ''barwt://' /.*/ -> {resource_quote_op "barwt://$_"}
	  | ''bat-smgws://' /.*/ -> {resource_quote_op "bat-smgws://$_"}
	  | ''bat-smgwt://' /.*/ -> {resource_quote_op "bat-smgwt://$_"}
	  | ''baws://' /.*/ -> {resource_quote_op "baws://$_"}
	  | ''bawt://' /.*/ -> {resource_quote_op "bawt://$_"}
	  | ''bclws://' /.*/ -> {resource_quote_op "bclws://$_"}
	  | ''bclwt://' /.*/ -> {resource_quote_op "bclwt://$_"}
	  | ''be-taraskws://' /.*/ -> {resource_quote_op "be-taraskws://$_"}
	  | ''be-taraskwt://' /.*/ -> {resource_quote_op "be-taraskwt://$_"}
	  | ''bews://' /.*/ -> {resource_quote_op "bews://$_"}
	  | ''bewt://' /.*/ -> {resource_quote_op "bewt://$_"}
	  | ''bgws://' /.*/ -> {resource_quote_op "bgws://$_"}
	  | ''bgwt://' /.*/ -> {resource_quote_op "bgwt://$_"}
	  | ''bhws://' /.*/ -> {resource_quote_op "bhws://$_"}
	  | ''bhwt://' /.*/ -> {resource_quote_op "bhwt://$_"}
	  | ''biws://' /.*/ -> {resource_quote_op "biws://$_"}
	  | ''biwt://' /.*/ -> {resource_quote_op "biwt://$_"}
	  | ''bjnws://' /.*/ -> {resource_quote_op "bjnws://$_"}
	  | ''bjnwt://' /.*/ -> {resource_quote_op "bjnwt://$_"}
	  | ''bmws://' /.*/ -> {resource_quote_op "bmws://$_"}
	  | ''bmwt://' /.*/ -> {resource_quote_op "bmwt://$_"}
	  | ''bnws://' /.*/ -> {resource_quote_op "bnws://$_"}
	  | ''bnwt://' /.*/ -> {resource_quote_op "bnwt://$_"}
	  | ''bows://' /.*/ -> {resource_quote_op "bows://$_"}
	  | ''bowt://' /.*/ -> {resource_quote_op "bowt://$_"}
	  | ''bpyws://' /.*/ -> {resource_quote_op "bpyws://$_"}
	  | ''bpywt://' /.*/ -> {resource_quote_op "bpywt://$_"}
	  | ''brws://' /.*/ -> {resource_quote_op "brws://$_"}
	  | ''brwt://' /.*/ -> {resource_quote_op "brwt://$_"}
	  | ''bsws://' /.*/ -> {resource_quote_op "bsws://$_"}
	  | ''bswt://' /.*/ -> {resource_quote_op "bswt://$_"}
	  | ''bugws://' /.*/ -> {resource_quote_op "bugws://$_"}
	  | ''bugwt://' /.*/ -> {resource_quote_op "bugwt://$_"}
	  | ''bxrws://' /.*/ -> {resource_quote_op "bxrws://$_"}
	  | ''bxrwt://' /.*/ -> {resource_quote_op "bxrwt://$_"}
	  | ''caws://' /.*/ -> {resource_quote_op "caws://$_"}
	  | ''cawt://' /.*/ -> {resource_quote_op "cawt://$_"}
	  | ''cbk-zamws://' /.*/ -> {resource_quote_op "cbk-zamws://$_"}
	  | ''cbk-zamwt://' /.*/ -> {resource_quote_op "cbk-zamwt://$_"}
	  | ''cdows://' /.*/ -> {resource_quote_op "cdows://$_"}
	  | ''cdowt://' /.*/ -> {resource_quote_op "cdowt://$_"}
	  | ''cebws://' /.*/ -> {resource_quote_op "cebws://$_"}
	  | ''cebwt://' /.*/ -> {resource_quote_op "cebwt://$_"}
	  | ''cews://' /.*/ -> {resource_quote_op "cews://$_"}
	  | ''cewt://' /.*/ -> {resource_quote_op "cewt://$_"}
	  | ''chows://' /.*/ -> {resource_quote_op "chows://$_"}
	  | ''chowt://' /.*/ -> {resource_quote_op "chowt://$_"}
	  | ''chrws://' /.*/ -> {resource_quote_op "chrws://$_"}
	  | ''chrwt://' /.*/ -> {resource_quote_op "chrwt://$_"}
	  | ''chws://' /.*/ -> {resource_quote_op "chws://$_"}
	  | ''chwt://' /.*/ -> {resource_quote_op "chwt://$_"}
	  | ''chyws://' /.*/ -> {resource_quote_op "chyws://$_"}
	  | ''chywt://' /.*/ -> {resource_quote_op "chywt://$_"}
	  | ''ckbws://' /.*/ -> {resource_quote_op "ckbws://$_"}
	  | ''ckbwt://' /.*/ -> {resource_quote_op "ckbwt://$_"}
	  | ''cows://' /.*/ -> {resource_quote_op "cows://$_"}
	  | ''cowt://' /.*/ -> {resource_quote_op "cowt://$_"}
	  | ''crhws://' /.*/ -> {resource_quote_op "crhws://$_"}
	  | ''crhwt://' /.*/ -> {resource_quote_op "crhwt://$_"}
	  | ''crws://' /.*/ -> {resource_quote_op "crws://$_"}
	  | ''crwt://' /.*/ -> {resource_quote_op "crwt://$_"}
	  | ''csbws://' /.*/ -> {resource_quote_op "csbws://$_"}
	  | ''csbwt://' /.*/ -> {resource_quote_op "csbwt://$_"}
	  | ''csws://' /.*/ -> {resource_quote_op "csws://$_"}
	  | ''cswt://' /.*/ -> {resource_quote_op "cswt://$_"}
	  | ''cuws://' /.*/ -> {resource_quote_op "cuws://$_"}
	  | ''cuwt://' /.*/ -> {resource_quote_op "cuwt://$_"}
	  | ''cvws://' /.*/ -> {resource_quote_op "cvws://$_"}
	  | ''cvwt://' /.*/ -> {resource_quote_op "cvwt://$_"}
	  | ''cyws://' /.*/ -> {resource_quote_op "cyws://$_"}
	  | ''cywt://' /.*/ -> {resource_quote_op "cywt://$_"}
	  | ''daws://' /.*/ -> {resource_quote_op "daws://$_"}
	  | ''dawt://' /.*/ -> {resource_quote_op "dawt://$_"}
	  | ''dews://' /.*/ -> {resource_quote_op "dews://$_"}
	  | ''dewt://' /.*/ -> {resource_quote_op "dewt://$_"}
	  | ''dinws://' /.*/ -> {resource_quote_op "dinws://$_"}
	  | ''dinwt://' /.*/ -> {resource_quote_op "dinwt://$_"}
	  | ''diqws://' /.*/ -> {resource_quote_op "diqws://$_"}
	  | ''diqwt://' /.*/ -> {resource_quote_op "diqwt://$_"}
	  | ''dsbws://' /.*/ -> {resource_quote_op "dsbws://$_"}
	  | ''dsbwt://' /.*/ -> {resource_quote_op "dsbwt://$_"}
	  | ''dtyws://' /.*/ -> {resource_quote_op "dtyws://$_"}
	  | ''dtywt://' /.*/ -> {resource_quote_op "dtywt://$_"}
	  | ''dvws://' /.*/ -> {resource_quote_op "dvws://$_"}
	  | ''dvwt://' /.*/ -> {resource_quote_op "dvwt://$_"}
	  | ''dzws://' /.*/ -> {resource_quote_op "dzws://$_"}
	  | ''dzwt://' /.*/ -> {resource_quote_op "dzwt://$_"}
	  | ''eews://' /.*/ -> {resource_quote_op "eews://$_"}
	  | ''eewt://' /.*/ -> {resource_quote_op "eewt://$_"}
	  | ''elws://' /.*/ -> {resource_quote_op "elws://$_"}
	  | ''elwt://' /.*/ -> {resource_quote_op "elwt://$_"}
	  | ''emlws://' /.*/ -> {resource_quote_op "emlws://$_"}
	  | ''emlwt://' /.*/ -> {resource_quote_op "emlwt://$_"}
	  | ''enws://' /.*/ -> {resource_quote_op "enws://$_"}
	  | ''enwt://' /.*/ -> {resource_quote_op "enwt://$_"}
	  | ''eows://' /.*/ -> {resource_quote_op "eows://$_"}
	  | ''eowt://' /.*/ -> {resource_quote_op "eowt://$_"}
	  | ''esws://' /.*/ -> {resource_quote_op "esws://$_"}
	  | ''eswt://' /.*/ -> {resource_quote_op "eswt://$_"}
	  | ''etws://' /.*/ -> {resource_quote_op "etws://$_"}
	  | ''etwt://' /.*/ -> {resource_quote_op "etwt://$_"}
	  | ''euws://' /.*/ -> {resource_quote_op "euws://$_"}
	  | ''euwt://' /.*/ -> {resource_quote_op "euwt://$_"}
	  | ''extws://' /.*/ -> {resource_quote_op "extws://$_"}
	  | ''extwt://' /.*/ -> {resource_quote_op "extwt://$_"}
	  | ''faws://' /.*/ -> {resource_quote_op "faws://$_"}
	  | ''fawt://' /.*/ -> {resource_quote_op "fawt://$_"}
	  | ''ffws://' /.*/ -> {resource_quote_op "ffws://$_"}
	  | ''ffwt://' /.*/ -> {resource_quote_op "ffwt://$_"}
	  | ''file-closure://' /.*/ -> {resource_quote_op "file-closure://$_"}
	  | ''file://' /.*/ -> {resource_quote_op "file://$_"}
	  | ''fiu-vrows://' /.*/ -> {resource_quote_op "fiu-vrows://$_"}
	  | ''fiu-vrowt://' /.*/ -> {resource_quote_op "fiu-vrowt://$_"}
	  | ''fiws://' /.*/ -> {resource_quote_op "fiws://$_"}
	  | ''fiwt://' /.*/ -> {resource_quote_op "fiwt://$_"}
	  | ''fjws://' /.*/ -> {resource_quote_op "fjws://$_"}
	  | ''fjwt://' /.*/ -> {resource_quote_op "fjwt://$_"}
	  | ''fows://' /.*/ -> {resource_quote_op "fows://$_"}
	  | ''fowt://' /.*/ -> {resource_quote_op "fowt://$_"}
	  | ''frpws://' /.*/ -> {resource_quote_op "frpws://$_"}
	  | ''frpwt://' /.*/ -> {resource_quote_op "frpwt://$_"}
	  | ''frrws://' /.*/ -> {resource_quote_op "frrws://$_"}
	  | ''frrwt://' /.*/ -> {resource_quote_op "frrwt://$_"}
	  | ''frws://' /.*/ -> {resource_quote_op "frws://$_"}
	  | ''frwt://' /.*/ -> {resource_quote_op "frwt://$_"}
	  | ''furws://' /.*/ -> {resource_quote_op "furws://$_"}
	  | ''furwt://' /.*/ -> {resource_quote_op "furwt://$_"}
	  | ''fyws://' /.*/ -> {resource_quote_op "fyws://$_"}
	  | ''fywt://' /.*/ -> {resource_quote_op "fywt://$_"}
	  | ''gagws://' /.*/ -> {resource_quote_op "gagws://$_"}
	  | ''gagwt://' /.*/ -> {resource_quote_op "gagwt://$_"}
	  | ''ganws://' /.*/ -> {resource_quote_op "ganws://$_"}
	  | ''ganwt://' /.*/ -> {resource_quote_op "ganwt://$_"}
	  | ''gaws://' /.*/ -> {resource_quote_op "gaws://$_"}
	  | ''gawt://' /.*/ -> {resource_quote_op "gawt://$_"}
	  | ''gcrws://' /.*/ -> {resource_quote_op "gcrws://$_"}
	  | ''gcrwt://' /.*/ -> {resource_quote_op "gcrwt://$_"}
	  | ''gdws://' /.*/ -> {resource_quote_op "gdws://$_"}
	  | ''gdwt://' /.*/ -> {resource_quote_op "gdwt://$_"}
	  | ''git://' /.*/ -> {resource_quote_op "git://$_"}
	  | ''gitblob://' /.*/ -> {resource_quote_op "gitblob://$_"}
	  | ''gitclosure://' /.*/ -> {resource_quote_op "gitclosure://$_"}
	  | ''gitcommit://' /.*/ -> {resource_quote_op "gitcommit://$_"}
	  | ''gitcommitmeta://' /.*/ -> {resource_quote_op "gitcommitmeta://$_"}
	  | ''gitddelta://' /.*/ -> {resource_quote_op "gitddelta://$_"}
	  | ''gitdelta://' /.*/ -> {resource_quote_op "gitdelta://$_"}
	  | ''gitdiff://' /.*/ -> {resource_quote_op "gitdiff://$_"}
	  | ''gitdsnap://' /.*/ -> {resource_quote_op "gitdsnap://$_"}
	  | ''githistory://' /.*/ -> {resource_quote_op "githistory://$_"}
	  | ''gitnmhistory://' /.*/ -> {resource_quote_op "gitnmhistory://$_"}
	  | ''gitpdiff://' /.*/ -> {resource_quote_op "gitpdiff://$_"}
	  | ''gitsnap://' /.*/ -> {resource_quote_op "gitsnap://$_"}
	  | ''gittree://' /.*/ -> {resource_quote_op "gittree://$_"}
	  | ''glkws://' /.*/ -> {resource_quote_op "glkws://$_"}
	  | ''glkwt://' /.*/ -> {resource_quote_op "glkwt://$_"}
	  | ''glws://' /.*/ -> {resource_quote_op "glws://$_"}
	  | ''glwt://' /.*/ -> {resource_quote_op "glwt://$_"}
	  | ''gnws://' /.*/ -> {resource_quote_op "gnws://$_"}
	  | ''gnwt://' /.*/ -> {resource_quote_op "gnwt://$_"}
	  | ''gomws://' /.*/ -> {resource_quote_op "gomws://$_"}
	  | ''gomwt://' /.*/ -> {resource_quote_op "gomwt://$_"}
	  | ''gorws://' /.*/ -> {resource_quote_op "gorws://$_"}
	  | ''gorwt://' /.*/ -> {resource_quote_op "gorwt://$_"}
	  | ''gotws://' /.*/ -> {resource_quote_op "gotws://$_"}
	  | ''gotwt://' /.*/ -> {resource_quote_op "gotwt://$_"}
	  | ''guws://' /.*/ -> {resource_quote_op "guws://$_"}
	  | ''guwt://' /.*/ -> {resource_quote_op "guwt://$_"}
	  | ''gvws://' /.*/ -> {resource_quote_op "gvws://$_"}
	  | ''gvwt://' /.*/ -> {resource_quote_op "gvwt://$_"}
	  | ''hakws://' /.*/ -> {resource_quote_op "hakws://$_"}
	  | ''hakwt://' /.*/ -> {resource_quote_op "hakwt://$_"}
	  | ''haws://' /.*/ -> {resource_quote_op "haws://$_"}
	  | ''hawt://' /.*/ -> {resource_quote_op "hawt://$_"}
	  | ''hawws://' /.*/ -> {resource_quote_op "hawws://$_"}
	  | ''hawwt://' /.*/ -> {resource_quote_op "hawwt://$_"}
	  | ''hdfs://' /.*/ -> {resource_quote_op "hdfs://$_"}
	  | ''hdfsc://' /.*/ -> {resource_quote_op "hdfsc://$_"}
	  | ''hdfscname://' /.*/ -> {resource_quote_op "hdfscname://$_"}
	  | ''hdfsj://' /.*/ -> {resource_quote_op "hdfsj://$_"}
	  | ''hdfsjname://' /.*/ -> {resource_quote_op "hdfsjname://$_"}
	  | ''hdfsrm://' /.*/ -> {resource_quote_op "hdfsrm://$_"}
	  | ''hdfst://' /.*/ -> {resource_quote_op "hdfst://$_"}
	  | ''hews://' /.*/ -> {resource_quote_op "hews://$_"}
	  | ''hewt://' /.*/ -> {resource_quote_op "hewt://$_"}
	  | ''hifws://' /.*/ -> {resource_quote_op "hifws://$_"}
	  | ''hifwt://' /.*/ -> {resource_quote_op "hifwt://$_"}
	  | ''hiws://' /.*/ -> {resource_quote_op "hiws://$_"}
	  | ''hiwt://' /.*/ -> {resource_quote_op "hiwt://$_"}
	  | ''hows://' /.*/ -> {resource_quote_op "hows://$_"}
	  | ''howt://' /.*/ -> {resource_quote_op "howt://$_"}
	  | ''hrws://' /.*/ -> {resource_quote_op "hrws://$_"}
	  | ''hrwt://' /.*/ -> {resource_quote_op "hrwt://$_"}
	  | ''hsbws://' /.*/ -> {resource_quote_op "hsbws://$_"}
	  | ''hsbwt://' /.*/ -> {resource_quote_op "hsbwt://$_"}
	  | ''http://' /.*/ -> {resource_quote_op "http://$_"}
	  | ''https://' /.*/ -> {resource_quote_op "https://$_"}
	  | ''htws://' /.*/ -> {resource_quote_op "htws://$_"}
	  | ''htwt://' /.*/ -> {resource_quote_op "htwt://$_"}
	  | ''huws://' /.*/ -> {resource_quote_op "huws://$_"}
	  | ''huwt://' /.*/ -> {resource_quote_op "huwt://$_"}
	  | ''hyws://' /.*/ -> {resource_quote_op "hyws://$_"}
	  | ''hywt://' /.*/ -> {resource_quote_op "hywt://$_"}
	  | ''hywws://' /.*/ -> {resource_quote_op "hywws://$_"}
	  | ''hywwt://' /.*/ -> {resource_quote_op "hywwt://$_"}
	  | ''hzws://' /.*/ -> {resource_quote_op "hzws://$_"}
	  | ''hzwt://' /.*/ -> {resource_quote_op "hzwt://$_"}
	  | ''iaws://' /.*/ -> {resource_quote_op "iaws://$_"}
	  | ''iawt://' /.*/ -> {resource_quote_op "iawt://$_"}
	  | ''idws://' /.*/ -> {resource_quote_op "idws://$_"}
	  | ''idwt://' /.*/ -> {resource_quote_op "idwt://$_"}
	  | ''iews://' /.*/ -> {resource_quote_op "iews://$_"}
	  | ''iewt://' /.*/ -> {resource_quote_op "iewt://$_"}
	  | ''igws://' /.*/ -> {resource_quote_op "igws://$_"}
	  | ''igwt://' /.*/ -> {resource_quote_op "igwt://$_"}
	  | ''iiws://' /.*/ -> {resource_quote_op "iiws://$_"}
	  | ''iiwt://' /.*/ -> {resource_quote_op "iiwt://$_"}
	  | ''ikws://' /.*/ -> {resource_quote_op "ikws://$_"}
	  | ''ikwt://' /.*/ -> {resource_quote_op "ikwt://$_"}
	  | ''ilows://' /.*/ -> {resource_quote_op "ilows://$_"}
	  | ''ilowt://' /.*/ -> {resource_quote_op "ilowt://$_"}
	  | ''inhws://' /.*/ -> {resource_quote_op "inhws://$_"}
	  | ''inhwt://' /.*/ -> {resource_quote_op "inhwt://$_"}
	  | ''iows://' /.*/ -> {resource_quote_op "iows://$_"}
	  | ''iowt://' /.*/ -> {resource_quote_op "iowt://$_"}
	  | ''isws://' /.*/ -> {resource_quote_op "isws://$_"}
	  | ''iswt://' /.*/ -> {resource_quote_op "iswt://$_"}
	  | ''itws://' /.*/ -> {resource_quote_op "itws://$_"}
	  | ''itwt://' /.*/ -> {resource_quote_op "itwt://$_"}
	  | ''iuws://' /.*/ -> {resource_quote_op "iuws://$_"}
	  | ''iuwt://' /.*/ -> {resource_quote_op "iuwt://$_"}
	  | ''jamws://' /.*/ -> {resource_quote_op "jamws://$_"}
	  | ''jamwt://' /.*/ -> {resource_quote_op "jamwt://$_"}
	  | ''jaws://' /.*/ -> {resource_quote_op "jaws://$_"}
	  | ''jawt://' /.*/ -> {resource_quote_op "jawt://$_"}
	  | ''jbows://' /.*/ -> {resource_quote_op "jbows://$_"}
	  | ''jbowt://' /.*/ -> {resource_quote_op "jbowt://$_"}
	  | ''jvws://' /.*/ -> {resource_quote_op "jvws://$_"}
	  | ''jvwt://' /.*/ -> {resource_quote_op "jvwt://$_"}
	  | ''kaaws://' /.*/ -> {resource_quote_op "kaaws://$_"}
	  | ''kaawt://' /.*/ -> {resource_quote_op "kaawt://$_"}
	  | ''kabws://' /.*/ -> {resource_quote_op "kabws://$_"}
	  | ''kabwt://' /.*/ -> {resource_quote_op "kabwt://$_"}
	  | ''kaws://' /.*/ -> {resource_quote_op "kaws://$_"}
	  | ''kawt://' /.*/ -> {resource_quote_op "kawt://$_"}
	  | ''kbdws://' /.*/ -> {resource_quote_op "kbdws://$_"}
	  | ''kbdwt://' /.*/ -> {resource_quote_op "kbdwt://$_"}
	  | ''kbpws://' /.*/ -> {resource_quote_op "kbpws://$_"}
	  | ''kbpwt://' /.*/ -> {resource_quote_op "kbpwt://$_"}
	  | ''kgws://' /.*/ -> {resource_quote_op "kgws://$_"}
	  | ''kgwt://' /.*/ -> {resource_quote_op "kgwt://$_"}
	  | ''kiws://' /.*/ -> {resource_quote_op "kiws://$_"}
	  | ''kiwt://' /.*/ -> {resource_quote_op "kiwt://$_"}
	  | ''kjws://' /.*/ -> {resource_quote_op "kjws://$_"}
	  | ''kjwt://' /.*/ -> {resource_quote_op "kjwt://$_"}
	  | ''kkws://' /.*/ -> {resource_quote_op "kkws://$_"}
	  | ''kkwt://' /.*/ -> {resource_quote_op "kkwt://$_"}
	  | ''klws://' /.*/ -> {resource_quote_op "klws://$_"}
	  | ''klwt://' /.*/ -> {resource_quote_op "klwt://$_"}
	  | ''kmws://' /.*/ -> {resource_quote_op "kmws://$_"}
	  | ''kmwt://' /.*/ -> {resource_quote_op "kmwt://$_"}
	  | ''knws://' /.*/ -> {resource_quote_op "knws://$_"}
	  | ''knwt://' /.*/ -> {resource_quote_op "knwt://$_"}
	  | ''koiws://' /.*/ -> {resource_quote_op "koiws://$_"}
	  | ''koiwt://' /.*/ -> {resource_quote_op "koiwt://$_"}
	  | ''kows://' /.*/ -> {resource_quote_op "kows://$_"}
	  | ''kowt://' /.*/ -> {resource_quote_op "kowt://$_"}
	  | ''krcws://' /.*/ -> {resource_quote_op "krcws://$_"}
	  | ''krcwt://' /.*/ -> {resource_quote_op "krcwt://$_"}
	  | ''krws://' /.*/ -> {resource_quote_op "krws://$_"}
	  | ''krwt://' /.*/ -> {resource_quote_op "krwt://$_"}
	  | ''kshws://' /.*/ -> {resource_quote_op "kshws://$_"}
	  | ''kshwt://' /.*/ -> {resource_quote_op "kshwt://$_"}
	  | ''ksws://' /.*/ -> {resource_quote_op "ksws://$_"}
	  | ''kswt://' /.*/ -> {resource_quote_op "kswt://$_"}
	  | ''kuws://' /.*/ -> {resource_quote_op "kuws://$_"}
	  | ''kuwt://' /.*/ -> {resource_quote_op "kuwt://$_"}
	  | ''kvws://' /.*/ -> {resource_quote_op "kvws://$_"}
	  | ''kvwt://' /.*/ -> {resource_quote_op "kvwt://$_"}
	  | ''kwws://' /.*/ -> {resource_quote_op "kwws://$_"}
	  | ''kwwt://' /.*/ -> {resource_quote_op "kwwt://$_"}
	  | ''kyws://' /.*/ -> {resource_quote_op "kyws://$_"}
	  | ''kywt://' /.*/ -> {resource_quote_op "kywt://$_"}
	  | ''ladws://' /.*/ -> {resource_quote_op "ladws://$_"}
	  | ''ladwt://' /.*/ -> {resource_quote_op "ladwt://$_"}
	  | ''laws://' /.*/ -> {resource_quote_op "laws://$_"}
	  | ''lawt://' /.*/ -> {resource_quote_op "lawt://$_"}
	  | ''lbews://' /.*/ -> {resource_quote_op "lbews://$_"}
	  | ''lbewt://' /.*/ -> {resource_quote_op "lbewt://$_"}
	  | ''lbws://' /.*/ -> {resource_quote_op "lbws://$_"}
	  | ''lbwt://' /.*/ -> {resource_quote_op "lbwt://$_"}
	  | ''lezws://' /.*/ -> {resource_quote_op "lezws://$_"}
	  | ''lezwt://' /.*/ -> {resource_quote_op "lezwt://$_"}
	  | ''lfnws://' /.*/ -> {resource_quote_op "lfnws://$_"}
	  | ''lfnwt://' /.*/ -> {resource_quote_op "lfnwt://$_"}
	  | ''lgws://' /.*/ -> {resource_quote_op "lgws://$_"}
	  | ''lgwt://' /.*/ -> {resource_quote_op "lgwt://$_"}
	  | ''lijws://' /.*/ -> {resource_quote_op "lijws://$_"}
	  | ''lijwt://' /.*/ -> {resource_quote_op "lijwt://$_"}
	  | ''liws://' /.*/ -> {resource_quote_op "liws://$_"}
	  | ''liwt://' /.*/ -> {resource_quote_op "liwt://$_"}
	  | ''lldws://' /.*/ -> {resource_quote_op "lldws://$_"}
	  | ''lldwt://' /.*/ -> {resource_quote_op "lldwt://$_"}
	  | ''lmows://' /.*/ -> {resource_quote_op "lmows://$_"}
	  | ''lmowt://' /.*/ -> {resource_quote_op "lmowt://$_"}
	  | ''lnws://' /.*/ -> {resource_quote_op "lnws://$_"}
	  | ''lnwt://' /.*/ -> {resource_quote_op "lnwt://$_"}
	  | ''lows://' /.*/ -> {resource_quote_op "lows://$_"}
	  | ''lowt://' /.*/ -> {resource_quote_op "lowt://$_"}
	  | ''lrcws://' /.*/ -> {resource_quote_op "lrcws://$_"}
	  | ''lrcwt://' /.*/ -> {resource_quote_op "lrcwt://$_"}
	  | ''ltgws://' /.*/ -> {resource_quote_op "ltgws://$_"}
	  | ''ltgwt://' /.*/ -> {resource_quote_op "ltgwt://$_"}
	  | ''ltws://' /.*/ -> {resource_quote_op "ltws://$_"}
	  | ''ltwt://' /.*/ -> {resource_quote_op "ltwt://$_"}
	  | ''lvws://' /.*/ -> {resource_quote_op "lvws://$_"}
	  | ''lvwt://' /.*/ -> {resource_quote_op "lvwt://$_"}
	  | ''maiws://' /.*/ -> {resource_quote_op "maiws://$_"}
	  | ''maiwt://' /.*/ -> {resource_quote_op "maiwt://$_"}
	  | ''map-bmsws://' /.*/ -> {resource_quote_op "map-bmsws://$_"}
	  | ''map-bmswt://' /.*/ -> {resource_quote_op "map-bmswt://$_"}
	  | ''mdfws://' /.*/ -> {resource_quote_op "mdfws://$_"}
	  | ''mdfwt://' /.*/ -> {resource_quote_op "mdfwt://$_"}
	  | ''mgws://' /.*/ -> {resource_quote_op "mgws://$_"}
	  | ''mgwt://' /.*/ -> {resource_quote_op "mgwt://$_"}
	  | ''mhrws://' /.*/ -> {resource_quote_op "mhrws://$_"}
	  | ''mhrwt://' /.*/ -> {resource_quote_op "mhrwt://$_"}
	  | ''mhws://' /.*/ -> {resource_quote_op "mhws://$_"}
	  | ''mhwt://' /.*/ -> {resource_quote_op "mhwt://$_"}
	  | ''minws://' /.*/ -> {resource_quote_op "minws://$_"}
	  | ''minwt://' /.*/ -> {resource_quote_op "minwt://$_"}
	  | ''miws://' /.*/ -> {resource_quote_op "miws://$_"}
	  | ''miwt://' /.*/ -> {resource_quote_op "miwt://$_"}
	  | ''mkws://' /.*/ -> {resource_quote_op "mkws://$_"}
	  | ''mkwt://' /.*/ -> {resource_quote_op "mkwt://$_"}
	  | ''mlws://' /.*/ -> {resource_quote_op "mlws://$_"}
	  | ''mlwt://' /.*/ -> {resource_quote_op "mlwt://$_"}
	  | ''mnws://' /.*/ -> {resource_quote_op "mnws://$_"}
	  | ''mnwt://' /.*/ -> {resource_quote_op "mnwt://$_"}
	  | ''mnwws://' /.*/ -> {resource_quote_op "mnwws://$_"}
	  | ''mnwwt://' /.*/ -> {resource_quote_op "mnwwt://$_"}
	  | ''mrjws://' /.*/ -> {resource_quote_op "mrjws://$_"}
	  | ''mrjwt://' /.*/ -> {resource_quote_op "mrjwt://$_"}
	  | ''mrws://' /.*/ -> {resource_quote_op "mrws://$_"}
	  | ''mrwt://' /.*/ -> {resource_quote_op "mrwt://$_"}
	  | ''msws://' /.*/ -> {resource_quote_op "msws://$_"}
	  | ''mswt://' /.*/ -> {resource_quote_op "mswt://$_"}
	  | ''mtws://' /.*/ -> {resource_quote_op "mtws://$_"}
	  | ''mtwt://' /.*/ -> {resource_quote_op "mtwt://$_"}
	  | ''musws://' /.*/ -> {resource_quote_op "musws://$_"}
	  | ''muswt://' /.*/ -> {resource_quote_op "muswt://$_"}
	  | ''mwlws://' /.*/ -> {resource_quote_op "mwlws://$_"}
	  | ''mwlwt://' /.*/ -> {resource_quote_op "mwlwt://$_"}
	  | ''myvws://' /.*/ -> {resource_quote_op "myvws://$_"}
	  | ''myvwt://' /.*/ -> {resource_quote_op "myvwt://$_"}
	  | ''myws://' /.*/ -> {resource_quote_op "myws://$_"}
	  | ''mywt://' /.*/ -> {resource_quote_op "mywt://$_"}
	  | ''mznws://' /.*/ -> {resource_quote_op "mznws://$_"}
	  | ''mznwt://' /.*/ -> {resource_quote_op "mznwt://$_"}
	  | ''nahws://' /.*/ -> {resource_quote_op "nahws://$_"}
	  | ''nahwt://' /.*/ -> {resource_quote_op "nahwt://$_"}
	  | ''napws://' /.*/ -> {resource_quote_op "napws://$_"}
	  | ''napwt://' /.*/ -> {resource_quote_op "napwt://$_"}
	  | ''naws://' /.*/ -> {resource_quote_op "naws://$_"}
	  | ''nawt://' /.*/ -> {resource_quote_op "nawt://$_"}
	  | ''nds-nlws://' /.*/ -> {resource_quote_op "nds-nlws://$_"}
	  | ''nds-nlwt://' /.*/ -> {resource_quote_op "nds-nlwt://$_"}
	  | ''ndsws://' /.*/ -> {resource_quote_op "ndsws://$_"}
	  | ''ndswt://' /.*/ -> {resource_quote_op "ndswt://$_"}
	  | ''news://' /.*/ -> {resource_quote_op "news://$_"}
	  | ''newt://' /.*/ -> {resource_quote_op "newt://$_"}
	  | ''newws://' /.*/ -> {resource_quote_op "newws://$_"}
	  | ''newwt://' /.*/ -> {resource_quote_op "newwt://$_"}
	  | ''ngws://' /.*/ -> {resource_quote_op "ngws://$_"}
	  | ''ngwt://' /.*/ -> {resource_quote_op "ngwt://$_"}
	  | ''nlws://' /.*/ -> {resource_quote_op "nlws://$_"}
	  | ''nlwt://' /.*/ -> {resource_quote_op "nlwt://$_"}
	  | ''nnws://' /.*/ -> {resource_quote_op "nnws://$_"}
	  | ''nnwt://' /.*/ -> {resource_quote_op "nnwt://$_"}
	  | ''novws://' /.*/ -> {resource_quote_op "novws://$_"}
	  | ''novwt://' /.*/ -> {resource_quote_op "novwt://$_"}
	  | ''nows://' /.*/ -> {resource_quote_op "nows://$_"}
	  | ''nowt://' /.*/ -> {resource_quote_op "nowt://$_"}
	  | ''nqows://' /.*/ -> {resource_quote_op "nqows://$_"}
	  | ''nqowt://' /.*/ -> {resource_quote_op "nqowt://$_"}
	  | ''nrmws://' /.*/ -> {resource_quote_op "nrmws://$_"}
	  | ''nrmwt://' /.*/ -> {resource_quote_op "nrmwt://$_"}
	  | ''nsows://' /.*/ -> {resource_quote_op "nsows://$_"}
	  | ''nsowt://' /.*/ -> {resource_quote_op "nsowt://$_"}
	  | ''nvws://' /.*/ -> {resource_quote_op "nvws://$_"}
	  | ''nvwt://' /.*/ -> {resource_quote_op "nvwt://$_"}
	  | ''nyws://' /.*/ -> {resource_quote_op "nyws://$_"}
	  | ''nywt://' /.*/ -> {resource_quote_op "nywt://$_"}
	  | ''ocws://' /.*/ -> {resource_quote_op "ocws://$_"}
	  | ''ocwt://' /.*/ -> {resource_quote_op "ocwt://$_"}
	  | ''olows://' /.*/ -> {resource_quote_op "olows://$_"}
	  | ''olowt://' /.*/ -> {resource_quote_op "olowt://$_"}
	  | ''omws://' /.*/ -> {resource_quote_op "omws://$_"}
	  | ''omwt://' /.*/ -> {resource_quote_op "omwt://$_"}
	  | ''orws://' /.*/ -> {resource_quote_op "orws://$_"}
	  | ''orwt://' /.*/ -> {resource_quote_op "orwt://$_"}
	  | ''osws://' /.*/ -> {resource_quote_op "osws://$_"}
	  | ''oswt://' /.*/ -> {resource_quote_op "oswt://$_"}
	  | ''pagws://' /.*/ -> {resource_quote_op "pagws://$_"}
	  | ''pagwt://' /.*/ -> {resource_quote_op "pagwt://$_"}
	  | ''pamws://' /.*/ -> {resource_quote_op "pamws://$_"}
	  | ''pamwt://' /.*/ -> {resource_quote_op "pamwt://$_"}
	  | ''papws://' /.*/ -> {resource_quote_op "papws://$_"}
	  | ''papwt://' /.*/ -> {resource_quote_op "papwt://$_"}
	  | ''paws://' /.*/ -> {resource_quote_op "paws://$_"}
	  | ''pawt://' /.*/ -> {resource_quote_op "pawt://$_"}
	  | ''pcdws://' /.*/ -> {resource_quote_op "pcdws://$_"}
	  | ''pcdwt://' /.*/ -> {resource_quote_op "pcdwt://$_"}
	  | ''pdcws://' /.*/ -> {resource_quote_op "pdcws://$_"}
	  | ''pdcwt://' /.*/ -> {resource_quote_op "pdcwt://$_"}
	  | ''pflws://' /.*/ -> {resource_quote_op "pflws://$_"}
	  | ''pflwt://' /.*/ -> {resource_quote_op "pflwt://$_"}
	  | ''pihws://' /.*/ -> {resource_quote_op "pihws://$_"}
	  | ''pihwt://' /.*/ -> {resource_quote_op "pihwt://$_"}
	  | ''piws://' /.*/ -> {resource_quote_op "piws://$_"}
	  | ''piwt://' /.*/ -> {resource_quote_op "piwt://$_"}
	  | ''plws://' /.*/ -> {resource_quote_op "plws://$_"}
	  | ''plwt://' /.*/ -> {resource_quote_op "plwt://$_"}
	  | ''pmsws://' /.*/ -> {resource_quote_op "pmsws://$_"}
	  | ''pmswt://' /.*/ -> {resource_quote_op "pmswt://$_"}
	  | ''pnbws://' /.*/ -> {resource_quote_op "pnbws://$_"}
	  | ''pnbwt://' /.*/ -> {resource_quote_op "pnbwt://$_"}
	  | ''pntws://' /.*/ -> {resource_quote_op "pntws://$_"}
	  | ''pntwt://' /.*/ -> {resource_quote_op "pntwt://$_"}
	  | ''psws://' /.*/ -> {resource_quote_op "psws://$_"}
	  | ''pswt://' /.*/ -> {resource_quote_op "pswt://$_"}
	  | ''ptws://' /.*/ -> {resource_quote_op "ptws://$_"}
	  | ''ptwt://' /.*/ -> {resource_quote_op "ptwt://$_"}
	  | ''quws://' /.*/ -> {resource_quote_op "quws://$_"}
	  | ''quwt://' /.*/ -> {resource_quote_op "quwt://$_"}
	  | ''rmws://' /.*/ -> {resource_quote_op "rmws://$_"}
	  | ''rmwt://' /.*/ -> {resource_quote_op "rmwt://$_"}
	  | ''rmyws://' /.*/ -> {resource_quote_op "rmyws://$_"}
	  | ''rmywt://' /.*/ -> {resource_quote_op "rmywt://$_"}
	  | ''rnws://' /.*/ -> {resource_quote_op "rnws://$_"}
	  | ''rnwt://' /.*/ -> {resource_quote_op "rnwt://$_"}
	  | ''roa-rupws://' /.*/ -> {resource_quote_op "roa-rupws://$_"}
	  | ''roa-rupwt://' /.*/ -> {resource_quote_op "roa-rupwt://$_"}
	  | ''roa-taraws://' /.*/ -> {resource_quote_op "roa-taraws://$_"}
	  | ''roa-tarawt://' /.*/ -> {resource_quote_op "roa-tarawt://$_"}
	  | ''rows://' /.*/ -> {resource_quote_op "rows://$_"}
	  | ''rowt://' /.*/ -> {resource_quote_op "rowt://$_"}
	  | ''ruews://' /.*/ -> {resource_quote_op "ruews://$_"}
	  | ''ruewt://' /.*/ -> {resource_quote_op "ruewt://$_"}
	  | ''ruws://' /.*/ -> {resource_quote_op "ruws://$_"}
	  | ''ruwt://' /.*/ -> {resource_quote_op "ruwt://$_"}
	  | ''rwws://' /.*/ -> {resource_quote_op "rwws://$_"}
	  | ''rwwt://' /.*/ -> {resource_quote_op "rwwt://$_"}
	  | ''s3cmd://' /.*/ -> {resource_quote_op "s3cmd://$_"}
	  | ''sahws://' /.*/ -> {resource_quote_op "sahws://$_"}
	  | ''sahwt://' /.*/ -> {resource_quote_op "sahwt://$_"}
	  | ''satws://' /.*/ -> {resource_quote_op "satws://$_"}
	  | ''satwt://' /.*/ -> {resource_quote_op "satwt://$_"}
	  | ''saws://' /.*/ -> {resource_quote_op "saws://$_"}
	  | ''sawt://' /.*/ -> {resource_quote_op "sawt://$_"}
	  | ''scnws://' /.*/ -> {resource_quote_op "scnws://$_"}
	  | ''scnwt://' /.*/ -> {resource_quote_op "scnwt://$_"}
	  | ''scows://' /.*/ -> {resource_quote_op "scows://$_"}
	  | ''scowt://' /.*/ -> {resource_quote_op "scowt://$_"}
	  | ''scws://' /.*/ -> {resource_quote_op "scws://$_"}
	  | ''scwt://' /.*/ -> {resource_quote_op "scwt://$_"}
	  | ''sdws://' /.*/ -> {resource_quote_op "sdws://$_"}
	  | ''sdwt://' /.*/ -> {resource_quote_op "sdwt://$_"}
	  | ''sews://' /.*/ -> {resource_quote_op "sews://$_"}
	  | ''sewt://' /.*/ -> {resource_quote_op "sewt://$_"}
	  | ''sftp://' /.*/ -> {resource_quote_op "sftp://$_"}
	  | ''sgws://' /.*/ -> {resource_quote_op "sgws://$_"}
	  | ''sgwt://' /.*/ -> {resource_quote_op "sgwt://$_"}
	  | ''shnws://' /.*/ -> {resource_quote_op "shnws://$_"}
	  | ''shnwt://' /.*/ -> {resource_quote_op "shnwt://$_"}
	  | ''shws://' /.*/ -> {resource_quote_op "shws://$_"}
	  | ''shwt://' /.*/ -> {resource_quote_op "shwt://$_"}
	  | ''simplews://' /.*/ -> {resource_quote_op "simplews://$_"}
	  | ''simplewt://' /.*/ -> {resource_quote_op "simplewt://$_"}
	  | ''siws://' /.*/ -> {resource_quote_op "siws://$_"}
	  | ''siwt://' /.*/ -> {resource_quote_op "siwt://$_"}
	  | ''skws://' /.*/ -> {resource_quote_op "skws://$_"}
	  | ''skwt://' /.*/ -> {resource_quote_op "skwt://$_"}
	  | ''slws://' /.*/ -> {resource_quote_op "slws://$_"}
	  | ''slwt://' /.*/ -> {resource_quote_op "slwt://$_"}
	  | ''smws://' /.*/ -> {resource_quote_op "smws://$_"}
	  | ''smwt://' /.*/ -> {resource_quote_op "smwt://$_"}
	  | ''snws://' /.*/ -> {resource_quote_op "snws://$_"}
	  | ''snwt://' /.*/ -> {resource_quote_op "snwt://$_"}
	  | ''solr://' /.*/ -> {resource_quote_op "solr://$_"}
	  | ''sows://' /.*/ -> {resource_quote_op "sows://$_"}
	  | ''sowt://' /.*/ -> {resource_quote_op "sowt://$_"}
	  | ''sqlite://' /.*/ -> {resource_quote_op "sqlite://$_"}
	  | ''sqliteq://' /.*/ -> {resource_quote_op "sqliteq://$_"}
	  | ''sqlites://' /.*/ -> {resource_quote_op "sqlites://$_"}
	  | ''sqlitet://' /.*/ -> {resource_quote_op "sqlitet://$_"}
	  | ''sqws://' /.*/ -> {resource_quote_op "sqws://$_"}
	  | ''sqwt://' /.*/ -> {resource_quote_op "sqwt://$_"}
	  | ''srnws://' /.*/ -> {resource_quote_op "srnws://$_"}
	  | ''srnwt://' /.*/ -> {resource_quote_op "srnwt://$_"}
	  | ''srws://' /.*/ -> {resource_quote_op "srws://$_"}
	  | ''srwt://' /.*/ -> {resource_quote_op "srwt://$_"}
	  | ''ssws://' /.*/ -> {resource_quote_op "ssws://$_"}
	  | ''sswt://' /.*/ -> {resource_quote_op "sswt://$_"}
	  | ''stqws://' /.*/ -> {resource_quote_op "stqws://$_"}
	  | ''stqwt://' /.*/ -> {resource_quote_op "stqwt://$_"}
	  | ''stws://' /.*/ -> {resource_quote_op "stws://$_"}
	  | ''stwt://' /.*/ -> {resource_quote_op "stwt://$_"}
	  | ''suws://' /.*/ -> {resource_quote_op "suws://$_"}
	  | ''suwt://' /.*/ -> {resource_quote_op "suwt://$_"}
	  | ''svws://' /.*/ -> {resource_quote_op "svws://$_"}
	  | ''svwt://' /.*/ -> {resource_quote_op "svwt://$_"}
	  | ''swws://' /.*/ -> {resource_quote_op "swws://$_"}
	  | ''swwt://' /.*/ -> {resource_quote_op "swwt://$_"}
	  | ''szlws://' /.*/ -> {resource_quote_op "szlws://$_"}
	  | ''szlwt://' /.*/ -> {resource_quote_op "szlwt://$_"}
	  | ''szyws://' /.*/ -> {resource_quote_op "szyws://$_"}
	  | ''szywt://' /.*/ -> {resource_quote_op "szywt://$_"}
	  | ''tar://' /.*/ -> {resource_quote_op "tar://$_"}
	  | ''tarentry://' /.*/ -> {resource_quote_op "tarentry://$_"}
	  | ''taws://' /.*/ -> {resource_quote_op "taws://$_"}
	  | ''tawt://' /.*/ -> {resource_quote_op "tawt://$_"}
	  | ''tcyws://' /.*/ -> {resource_quote_op "tcyws://$_"}
	  | ''tcywt://' /.*/ -> {resource_quote_op "tcywt://$_"}
	  | ''tetws://' /.*/ -> {resource_quote_op "tetws://$_"}
	  | ''tetwt://' /.*/ -> {resource_quote_op "tetwt://$_"}
	  | ''tews://' /.*/ -> {resource_quote_op "tews://$_"}
	  | ''tewt://' /.*/ -> {resource_quote_op "tewt://$_"}
	  | ''tgws://' /.*/ -> {resource_quote_op "tgws://$_"}
	  | ''tgwt://' /.*/ -> {resource_quote_op "tgwt://$_"}
	  | ''thws://' /.*/ -> {resource_quote_op "thws://$_"}
	  | ''thwt://' /.*/ -> {resource_quote_op "thwt://$_"}
	  | ''tiws://' /.*/ -> {resource_quote_op "tiws://$_"}
	  | ''tiwt://' /.*/ -> {resource_quote_op "tiwt://$_"}
	  | ''tkws://' /.*/ -> {resource_quote_op "tkws://$_"}
	  | ''tkwt://' /.*/ -> {resource_quote_op "tkwt://$_"}
	  | ''tlws://' /.*/ -> {resource_quote_op "tlws://$_"}
	  | ''tlwt://' /.*/ -> {resource_quote_op "tlwt://$_"}
	  | ''tnws://' /.*/ -> {resource_quote_op "tnws://$_"}
	  | ''tnwt://' /.*/ -> {resource_quote_op "tnwt://$_"}
	  | ''tows://' /.*/ -> {resource_quote_op "tows://$_"}
	  | ''towt://' /.*/ -> {resource_quote_op "towt://$_"}
	  | ''tpiws://' /.*/ -> {resource_quote_op "tpiws://$_"}
	  | ''tpiwt://' /.*/ -> {resource_quote_op "tpiwt://$_"}
	  | ''trws://' /.*/ -> {resource_quote_op "trws://$_"}
	  | ''trwt://' /.*/ -> {resource_quote_op "trwt://$_"}
	  | ''tsws://' /.*/ -> {resource_quote_op "tsws://$_"}
	  | ''tswt://' /.*/ -> {resource_quote_op "tswt://$_"}
	  | ''ttws://' /.*/ -> {resource_quote_op "ttws://$_"}
	  | ''ttwt://' /.*/ -> {resource_quote_op "ttwt://$_"}
	  | ''tumws://' /.*/ -> {resource_quote_op "tumws://$_"}
	  | ''tumwt://' /.*/ -> {resource_quote_op "tumwt://$_"}
	  | ''twws://' /.*/ -> {resource_quote_op "twws://$_"}
	  | ''twwt://' /.*/ -> {resource_quote_op "twwt://$_"}
	  | ''tyvws://' /.*/ -> {resource_quote_op "tyvws://$_"}
	  | ''tyvwt://' /.*/ -> {resource_quote_op "tyvwt://$_"}
	  | ''tyws://' /.*/ -> {resource_quote_op "tyws://$_"}
	  | ''tywt://' /.*/ -> {resource_quote_op "tywt://$_"}
	  | ''udmws://' /.*/ -> {resource_quote_op "udmws://$_"}
	  | ''udmwt://' /.*/ -> {resource_quote_op "udmwt://$_"}
	  | ''ugws://' /.*/ -> {resource_quote_op "ugws://$_"}
	  | ''ugwt://' /.*/ -> {resource_quote_op "ugwt://$_"}
	  | ''ukws://' /.*/ -> {resource_quote_op "ukws://$_"}
	  | ''ukwt://' /.*/ -> {resource_quote_op "ukwt://$_"}
	  | ''urws://' /.*/ -> {resource_quote_op "urws://$_"}
	  | ''urwt://' /.*/ -> {resource_quote_op "urwt://$_"}
	  | ''uzws://' /.*/ -> {resource_quote_op "uzws://$_"}
	  | ''uzwt://' /.*/ -> {resource_quote_op "uzwt://$_"}
	  | ''vecws://' /.*/ -> {resource_quote_op "vecws://$_"}
	  | ''vecwt://' /.*/ -> {resource_quote_op "vecwt://$_"}
	  | ''vepws://' /.*/ -> {resource_quote_op "vepws://$_"}
	  | ''vepwt://' /.*/ -> {resource_quote_op "vepwt://$_"}
	  | ''vews://' /.*/ -> {resource_quote_op "vews://$_"}
	  | ''vewt://' /.*/ -> {resource_quote_op "vewt://$_"}
	  | ''viws://' /.*/ -> {resource_quote_op "viws://$_"}
	  | ''viwt://' /.*/ -> {resource_quote_op "viwt://$_"}
	  | ''vlsws://' /.*/ -> {resource_quote_op "vlsws://$_"}
	  | ''vlswt://' /.*/ -> {resource_quote_op "vlswt://$_"}
	  | ''vows://' /.*/ -> {resource_quote_op "vows://$_"}
	  | ''vowt://' /.*/ -> {resource_quote_op "vowt://$_"}
	  | ''warws://' /.*/ -> {resource_quote_op "warws://$_"}
	  | ''warwt://' /.*/ -> {resource_quote_op "warwt://$_"}
	  | ''waws://' /.*/ -> {resource_quote_op "waws://$_"}
	  | ''wawt://' /.*/ -> {resource_quote_op "wawt://$_"}
	  | ''wiki://' /.*/ -> {resource_quote_op "wiki://$_"}
	  | ''wows://' /.*/ -> {resource_quote_op "wows://$_"}
	  | ''wowt://' /.*/ -> {resource_quote_op "wowt://$_"}
	  | ''wuuws://' /.*/ -> {resource_quote_op "wuuws://$_"}
	  | ''wuuwt://' /.*/ -> {resource_quote_op "wuuwt://$_"}
	  | ''xalws://' /.*/ -> {resource_quote_op "xalws://$_"}
	  | ''xalwt://' /.*/ -> {resource_quote_op "xalwt://$_"}
	  | ''xhws://' /.*/ -> {resource_quote_op "xhws://$_"}
	  | ''xhwt://' /.*/ -> {resource_quote_op "xhwt://$_"}
	  | ''xlsx://' /.*/ -> {resource_quote_op "xlsx://$_"}
	  | ''xlsxsheet://' /.*/ -> {resource_quote_op "xlsxsheet://$_"}
	  | ''xmfws://' /.*/ -> {resource_quote_op "xmfws://$_"}
	  | ''xmfwt://' /.*/ -> {resource_quote_op "xmfwt://$_"}
	  | ''yiws://' /.*/ -> {resource_quote_op "yiws://$_"}
	  | ''yiwt://' /.*/ -> {resource_quote_op "yiwt://$_"}
	  | ''yows://' /.*/ -> {resource_quote_op "yows://$_"}
	  | ''yowt://' /.*/ -> {resource_quote_op "yowt://$_"}
	  | ''yt://' /.*/ -> {resource_quote_op "yt://$_"}
	  | ''zaws://' /.*/ -> {resource_quote_op "zaws://$_"}
	  | ''zawt://' /.*/ -> {resource_quote_op "zawt://$_"}
	  | ''zeaws://' /.*/ -> {resource_quote_op "zeaws://$_"}
	  | ''zeawt://' /.*/ -> {resource_quote_op "zeawt://$_"}
	  | ''zh-classicalws://' /.*/ -> {resource_quote_op "zh-classicalws://$_"}
	  | ''zh-classicalwt://' /.*/ -> {resource_quote_op "zh-classicalwt://$_"}
	  | ''zh-min-nanws://' /.*/ -> {resource_quote_op "zh-min-nanws://$_"}
	  | ''zh-min-nanwt://' /.*/ -> {resource_quote_op "zh-min-nanwt://$_"}
	  | ''zh-yuews://' /.*/ -> {resource_quote_op "zh-yuews://$_"}
	  | ''zh-yuewt://' /.*/ -> {resource_quote_op "zh-yuewt://$_"}
	  | ''zhws://' /.*/ -> {resource_quote_op "zhws://$_"}
	  | ''zhwt://' /.*/ -> {resource_quote_op "zhwt://$_"}
	  | ''zip://' /.*/ -> {resource_quote_op "zip://$_"}
	  | ''zipentry://' /.*/ -> {resource_quote_op "zipentry://$_"}
	  | ''zuws://' /.*/ -> {resource_quote_op "zuws://$_"}
	  | ''zuwt://' /.*/ -> {resource_quote_op "zuwt://$_"}
	  | '7z://' /.*/ -> {resource_append_op "7z://$_"}
	  | '7zentry://' /.*/ -> {resource_append_op "7zentry://$_"}
	  | 'aaws://' /.*/ -> {resource_append_op "aaws://$_"}
	  | 'aawt://' /.*/ -> {resource_append_op "aawt://$_"}
	  | 'abws://' /.*/ -> {resource_append_op "abws://$_"}
	  | 'abwt://' /.*/ -> {resource_append_op "abwt://$_"}
	  | 'acews://' /.*/ -> {resource_append_op "acews://$_"}
	  | 'acewt://' /.*/ -> {resource_append_op "acewt://$_"}
	  | 'adyws://' /.*/ -> {resource_append_op "adyws://$_"}
	  | 'adywt://' /.*/ -> {resource_append_op "adywt://$_"}
	  | 'afws://' /.*/ -> {resource_append_op "afws://$_"}
	  | 'afwt://' /.*/ -> {resource_append_op "afwt://$_"}
	  | 'akws://' /.*/ -> {resource_append_op "akws://$_"}
	  | 'akwt://' /.*/ -> {resource_append_op "akwt://$_"}
	  | 'alsws://' /.*/ -> {resource_append_op "alsws://$_"}
	  | 'alswt://' /.*/ -> {resource_append_op "alswt://$_"}
	  | 'amws://' /.*/ -> {resource_append_op "amws://$_"}
	  | 'amwt://' /.*/ -> {resource_append_op "amwt://$_"}
	  | 'angws://' /.*/ -> {resource_append_op "angws://$_"}
	  | 'angwt://' /.*/ -> {resource_append_op "angwt://$_"}
	  | 'anws://' /.*/ -> {resource_append_op "anws://$_"}
	  | 'anwt://' /.*/ -> {resource_append_op "anwt://$_"}
	  | 'arcws://' /.*/ -> {resource_append_op "arcws://$_"}
	  | 'arcwt://' /.*/ -> {resource_append_op "arcwt://$_"}
	  | 'arws://' /.*/ -> {resource_append_op "arws://$_"}
	  | 'arwt://' /.*/ -> {resource_append_op "arwt://$_"}
	  | 'aryws://' /.*/ -> {resource_append_op "aryws://$_"}
	  | 'arywt://' /.*/ -> {resource_append_op "arywt://$_"}
	  | 'arzws://' /.*/ -> {resource_append_op "arzws://$_"}
	  | 'arzwt://' /.*/ -> {resource_append_op "arzwt://$_"}
	  | 'astws://' /.*/ -> {resource_append_op "astws://$_"}
	  | 'astwt://' /.*/ -> {resource_append_op "astwt://$_"}
	  | 'asws://' /.*/ -> {resource_append_op "asws://$_"}
	  | 'aswt://' /.*/ -> {resource_append_op "aswt://$_"}
	  | 'atjws://' /.*/ -> {resource_append_op "atjws://$_"}
	  | 'atjwt://' /.*/ -> {resource_append_op "atjwt://$_"}
	  | 'avkws://' /.*/ -> {resource_append_op "avkws://$_"}
	  | 'avkwt://' /.*/ -> {resource_append_op "avkwt://$_"}
	  | 'avws://' /.*/ -> {resource_append_op "avws://$_"}
	  | 'avwt://' /.*/ -> {resource_append_op "avwt://$_"}
	  | 'awaws://' /.*/ -> {resource_append_op "awaws://$_"}
	  | 'awawt://' /.*/ -> {resource_append_op "awawt://$_"}
	  | 'ayws://' /.*/ -> {resource_append_op "ayws://$_"}
	  | 'aywt://' /.*/ -> {resource_append_op "aywt://$_"}
	  | 'azbws://' /.*/ -> {resource_append_op "azbws://$_"}
	  | 'azbwt://' /.*/ -> {resource_append_op "azbwt://$_"}
	  | 'azws://' /.*/ -> {resource_append_op "azws://$_"}
	  | 'azwt://' /.*/ -> {resource_append_op "azwt://$_"}
	  | 'banws://' /.*/ -> {resource_append_op "banws://$_"}
	  | 'banwt://' /.*/ -> {resource_append_op "banwt://$_"}
	  | 'barws://' /.*/ -> {resource_append_op "barws://$_"}
	  | 'barwt://' /.*/ -> {resource_append_op "barwt://$_"}
	  | 'bat-smgws://' /.*/ -> {resource_append_op "bat-smgws://$_"}
	  | 'bat-smgwt://' /.*/ -> {resource_append_op "bat-smgwt://$_"}
	  | 'baws://' /.*/ -> {resource_append_op "baws://$_"}
	  | 'bawt://' /.*/ -> {resource_append_op "bawt://$_"}
	  | 'bclws://' /.*/ -> {resource_append_op "bclws://$_"}
	  | 'bclwt://' /.*/ -> {resource_append_op "bclwt://$_"}
	  | 'be-taraskws://' /.*/ -> {resource_append_op "be-taraskws://$_"}
	  | 'be-taraskwt://' /.*/ -> {resource_append_op "be-taraskwt://$_"}
	  | 'bews://' /.*/ -> {resource_append_op "bews://$_"}
	  | 'bewt://' /.*/ -> {resource_append_op "bewt://$_"}
	  | 'bgws://' /.*/ -> {resource_append_op "bgws://$_"}
	  | 'bgwt://' /.*/ -> {resource_append_op "bgwt://$_"}
	  | 'bhws://' /.*/ -> {resource_append_op "bhws://$_"}
	  | 'bhwt://' /.*/ -> {resource_append_op "bhwt://$_"}
	  | 'biws://' /.*/ -> {resource_append_op "biws://$_"}
	  | 'biwt://' /.*/ -> {resource_append_op "biwt://$_"}
	  | 'bjnws://' /.*/ -> {resource_append_op "bjnws://$_"}
	  | 'bjnwt://' /.*/ -> {resource_append_op "bjnwt://$_"}
	  | 'bmws://' /.*/ -> {resource_append_op "bmws://$_"}
	  | 'bmwt://' /.*/ -> {resource_append_op "bmwt://$_"}
	  | 'bnws://' /.*/ -> {resource_append_op "bnws://$_"}
	  | 'bnwt://' /.*/ -> {resource_append_op "bnwt://$_"}
	  | 'bows://' /.*/ -> {resource_append_op "bows://$_"}
	  | 'bowt://' /.*/ -> {resource_append_op "bowt://$_"}
	  | 'bpyws://' /.*/ -> {resource_append_op "bpyws://$_"}
	  | 'bpywt://' /.*/ -> {resource_append_op "bpywt://$_"}
	  | 'brws://' /.*/ -> {resource_append_op "brws://$_"}
	  | 'brwt://' /.*/ -> {resource_append_op "brwt://$_"}
	  | 'bsws://' /.*/ -> {resource_append_op "bsws://$_"}
	  | 'bswt://' /.*/ -> {resource_append_op "bswt://$_"}
	  | 'bugws://' /.*/ -> {resource_append_op "bugws://$_"}
	  | 'bugwt://' /.*/ -> {resource_append_op "bugwt://$_"}
	  | 'bxrws://' /.*/ -> {resource_append_op "bxrws://$_"}
	  | 'bxrwt://' /.*/ -> {resource_append_op "bxrwt://$_"}
	  | 'caws://' /.*/ -> {resource_append_op "caws://$_"}
	  | 'cawt://' /.*/ -> {resource_append_op "cawt://$_"}
	  | 'cbk-zamws://' /.*/ -> {resource_append_op "cbk-zamws://$_"}
	  | 'cbk-zamwt://' /.*/ -> {resource_append_op "cbk-zamwt://$_"}
	  | 'cdows://' /.*/ -> {resource_append_op "cdows://$_"}
	  | 'cdowt://' /.*/ -> {resource_append_op "cdowt://$_"}
	  | 'cebws://' /.*/ -> {resource_append_op "cebws://$_"}
	  | 'cebwt://' /.*/ -> {resource_append_op "cebwt://$_"}
	  | 'cews://' /.*/ -> {resource_append_op "cews://$_"}
	  | 'cewt://' /.*/ -> {resource_append_op "cewt://$_"}
	  | 'chows://' /.*/ -> {resource_append_op "chows://$_"}
	  | 'chowt://' /.*/ -> {resource_append_op "chowt://$_"}
	  | 'chrws://' /.*/ -> {resource_append_op "chrws://$_"}
	  | 'chrwt://' /.*/ -> {resource_append_op "chrwt://$_"}
	  | 'chws://' /.*/ -> {resource_append_op "chws://$_"}
	  | 'chwt://' /.*/ -> {resource_append_op "chwt://$_"}
	  | 'chyws://' /.*/ -> {resource_append_op "chyws://$_"}
	  | 'chywt://' /.*/ -> {resource_append_op "chywt://$_"}
	  | 'ckbws://' /.*/ -> {resource_append_op "ckbws://$_"}
	  | 'ckbwt://' /.*/ -> {resource_append_op "ckbwt://$_"}
	  | 'cows://' /.*/ -> {resource_append_op "cows://$_"}
	  | 'cowt://' /.*/ -> {resource_append_op "cowt://$_"}
	  | 'crhws://' /.*/ -> {resource_append_op "crhws://$_"}
	  | 'crhwt://' /.*/ -> {resource_append_op "crhwt://$_"}
	  | 'crws://' /.*/ -> {resource_append_op "crws://$_"}
	  | 'crwt://' /.*/ -> {resource_append_op "crwt://$_"}
	  | 'csbws://' /.*/ -> {resource_append_op "csbws://$_"}
	  | 'csbwt://' /.*/ -> {resource_append_op "csbwt://$_"}
	  | 'csws://' /.*/ -> {resource_append_op "csws://$_"}
	  | 'cswt://' /.*/ -> {resource_append_op "cswt://$_"}
	  | 'cuws://' /.*/ -> {resource_append_op "cuws://$_"}
	  | 'cuwt://' /.*/ -> {resource_append_op "cuwt://$_"}
	  | 'cvws://' /.*/ -> {resource_append_op "cvws://$_"}
	  | 'cvwt://' /.*/ -> {resource_append_op "cvwt://$_"}
	  | 'cyws://' /.*/ -> {resource_append_op "cyws://$_"}
	  | 'cywt://' /.*/ -> {resource_append_op "cywt://$_"}
	  | 'daws://' /.*/ -> {resource_append_op "daws://$_"}
	  | 'dawt://' /.*/ -> {resource_append_op "dawt://$_"}
	  | 'dews://' /.*/ -> {resource_append_op "dews://$_"}
	  | 'dewt://' /.*/ -> {resource_append_op "dewt://$_"}
	  | 'dinws://' /.*/ -> {resource_append_op "dinws://$_"}
	  | 'dinwt://' /.*/ -> {resource_append_op "dinwt://$_"}
	  | 'diqws://' /.*/ -> {resource_append_op "diqws://$_"}
	  | 'diqwt://' /.*/ -> {resource_append_op "diqwt://$_"}
	  | 'dsbws://' /.*/ -> {resource_append_op "dsbws://$_"}
	  | 'dsbwt://' /.*/ -> {resource_append_op "dsbwt://$_"}
	  | 'dtyws://' /.*/ -> {resource_append_op "dtyws://$_"}
	  | 'dtywt://' /.*/ -> {resource_append_op "dtywt://$_"}
	  | 'dvws://' /.*/ -> {resource_append_op "dvws://$_"}
	  | 'dvwt://' /.*/ -> {resource_append_op "dvwt://$_"}
	  | 'dzws://' /.*/ -> {resource_append_op "dzws://$_"}
	  | 'dzwt://' /.*/ -> {resource_append_op "dzwt://$_"}
	  | 'eews://' /.*/ -> {resource_append_op "eews://$_"}
	  | 'eewt://' /.*/ -> {resource_append_op "eewt://$_"}
	  | 'elws://' /.*/ -> {resource_append_op "elws://$_"}
	  | 'elwt://' /.*/ -> {resource_append_op "elwt://$_"}
	  | 'emlws://' /.*/ -> {resource_append_op "emlws://$_"}
	  | 'emlwt://' /.*/ -> {resource_append_op "emlwt://$_"}
	  | 'enws://' /.*/ -> {resource_append_op "enws://$_"}
	  | 'enwt://' /.*/ -> {resource_append_op "enwt://$_"}
	  | 'eows://' /.*/ -> {resource_append_op "eows://$_"}
	  | 'eowt://' /.*/ -> {resource_append_op "eowt://$_"}
	  | 'esws://' /.*/ -> {resource_append_op "esws://$_"}
	  | 'eswt://' /.*/ -> {resource_append_op "eswt://$_"}
	  | 'etws://' /.*/ -> {resource_append_op "etws://$_"}
	  | 'etwt://' /.*/ -> {resource_append_op "etwt://$_"}
	  | 'euws://' /.*/ -> {resource_append_op "euws://$_"}
	  | 'euwt://' /.*/ -> {resource_append_op "euwt://$_"}
	  | 'extws://' /.*/ -> {resource_append_op "extws://$_"}
	  | 'extwt://' /.*/ -> {resource_append_op "extwt://$_"}
	  | 'faws://' /.*/ -> {resource_append_op "faws://$_"}
	  | 'fawt://' /.*/ -> {resource_append_op "fawt://$_"}
	  | 'ffws://' /.*/ -> {resource_append_op "ffws://$_"}
	  | 'ffwt://' /.*/ -> {resource_append_op "ffwt://$_"}
	  | 'file-closure://' /.*/ -> {resource_append_op "file-closure://$_"}
	  | 'file://' /.*/ -> {resource_append_op "file://$_"}
	  | 'fiu-vrows://' /.*/ -> {resource_append_op "fiu-vrows://$_"}
	  | 'fiu-vrowt://' /.*/ -> {resource_append_op "fiu-vrowt://$_"}
	  | 'fiws://' /.*/ -> {resource_append_op "fiws://$_"}
	  | 'fiwt://' /.*/ -> {resource_append_op "fiwt://$_"}
	  | 'fjws://' /.*/ -> {resource_append_op "fjws://$_"}
	  | 'fjwt://' /.*/ -> {resource_append_op "fjwt://$_"}
	  | 'fows://' /.*/ -> {resource_append_op "fows://$_"}
	  | 'fowt://' /.*/ -> {resource_append_op "fowt://$_"}
	  | 'frpws://' /.*/ -> {resource_append_op "frpws://$_"}
	  | 'frpwt://' /.*/ -> {resource_append_op "frpwt://$_"}
	  | 'frrws://' /.*/ -> {resource_append_op "frrws://$_"}
	  | 'frrwt://' /.*/ -> {resource_append_op "frrwt://$_"}
	  | 'frws://' /.*/ -> {resource_append_op "frws://$_"}
	  | 'frwt://' /.*/ -> {resource_append_op "frwt://$_"}
	  | 'furws://' /.*/ -> {resource_append_op "furws://$_"}
	  | 'furwt://' /.*/ -> {resource_append_op "furwt://$_"}
	  | 'fyws://' /.*/ -> {resource_append_op "fyws://$_"}
	  | 'fywt://' /.*/ -> {resource_append_op "fywt://$_"}
	  | 'gagws://' /.*/ -> {resource_append_op "gagws://$_"}
	  | 'gagwt://' /.*/ -> {resource_append_op "gagwt://$_"}
	  | 'ganws://' /.*/ -> {resource_append_op "ganws://$_"}
	  | 'ganwt://' /.*/ -> {resource_append_op "ganwt://$_"}
	  | 'gaws://' /.*/ -> {resource_append_op "gaws://$_"}
	  | 'gawt://' /.*/ -> {resource_append_op "gawt://$_"}
	  | 'gcrws://' /.*/ -> {resource_append_op "gcrws://$_"}
	  | 'gcrwt://' /.*/ -> {resource_append_op "gcrwt://$_"}
	  | 'gdws://' /.*/ -> {resource_append_op "gdws://$_"}
	  | 'gdwt://' /.*/ -> {resource_append_op "gdwt://$_"}
	  | 'git://' /.*/ -> {resource_append_op "git://$_"}
	  | 'gitblob://' /.*/ -> {resource_append_op "gitblob://$_"}
	  | 'gitclosure://' /.*/ -> {resource_append_op "gitclosure://$_"}
	  | 'gitcommit://' /.*/ -> {resource_append_op "gitcommit://$_"}
	  | 'gitcommitmeta://' /.*/ -> {resource_append_op "gitcommitmeta://$_"}
	  | 'gitddelta://' /.*/ -> {resource_append_op "gitddelta://$_"}
	  | 'gitdelta://' /.*/ -> {resource_append_op "gitdelta://$_"}
	  | 'gitdiff://' /.*/ -> {resource_append_op "gitdiff://$_"}
	  | 'gitdsnap://' /.*/ -> {resource_append_op "gitdsnap://$_"}
	  | 'githistory://' /.*/ -> {resource_append_op "githistory://$_"}
	  | 'gitnmhistory://' /.*/ -> {resource_append_op "gitnmhistory://$_"}
	  | 'gitpdiff://' /.*/ -> {resource_append_op "gitpdiff://$_"}
	  | 'gitsnap://' /.*/ -> {resource_append_op "gitsnap://$_"}
	  | 'gittree://' /.*/ -> {resource_append_op "gittree://$_"}
	  | 'glkws://' /.*/ -> {resource_append_op "glkws://$_"}
	  | 'glkwt://' /.*/ -> {resource_append_op "glkwt://$_"}
	  | 'glws://' /.*/ -> {resource_append_op "glws://$_"}
	  | 'glwt://' /.*/ -> {resource_append_op "glwt://$_"}
	  | 'gnws://' /.*/ -> {resource_append_op "gnws://$_"}
	  | 'gnwt://' /.*/ -> {resource_append_op "gnwt://$_"}
	  | 'gomws://' /.*/ -> {resource_append_op "gomws://$_"}
	  | 'gomwt://' /.*/ -> {resource_append_op "gomwt://$_"}
	  | 'gorws://' /.*/ -> {resource_append_op "gorws://$_"}
	  | 'gorwt://' /.*/ -> {resource_append_op "gorwt://$_"}
	  | 'gotws://' /.*/ -> {resource_append_op "gotws://$_"}
	  | 'gotwt://' /.*/ -> {resource_append_op "gotwt://$_"}
	  | 'guws://' /.*/ -> {resource_append_op "guws://$_"}
	  | 'guwt://' /.*/ -> {resource_append_op "guwt://$_"}
	  | 'gvws://' /.*/ -> {resource_append_op "gvws://$_"}
	  | 'gvwt://' /.*/ -> {resource_append_op "gvwt://$_"}
	  | 'hakws://' /.*/ -> {resource_append_op "hakws://$_"}
	  | 'hakwt://' /.*/ -> {resource_append_op "hakwt://$_"}
	  | 'haws://' /.*/ -> {resource_append_op "haws://$_"}
	  | 'hawt://' /.*/ -> {resource_append_op "hawt://$_"}
	  | 'hawws://' /.*/ -> {resource_append_op "hawws://$_"}
	  | 'hawwt://' /.*/ -> {resource_append_op "hawwt://$_"}
	  | 'hdfs://' /.*/ -> {resource_append_op "hdfs://$_"}
	  | 'hdfsc://' /.*/ -> {resource_append_op "hdfsc://$_"}
	  | 'hdfscname://' /.*/ -> {resource_append_op "hdfscname://$_"}
	  | 'hdfsj://' /.*/ -> {resource_append_op "hdfsj://$_"}
	  | 'hdfsjname://' /.*/ -> {resource_append_op "hdfsjname://$_"}
	  | 'hdfsrm://' /.*/ -> {resource_append_op "hdfsrm://$_"}
	  | 'hdfst://' /.*/ -> {resource_append_op "hdfst://$_"}
	  | 'hews://' /.*/ -> {resource_append_op "hews://$_"}
	  | 'hewt://' /.*/ -> {resource_append_op "hewt://$_"}
	  | 'hifws://' /.*/ -> {resource_append_op "hifws://$_"}
	  | 'hifwt://' /.*/ -> {resource_append_op "hifwt://$_"}
	  | 'hiws://' /.*/ -> {resource_append_op "hiws://$_"}
	  | 'hiwt://' /.*/ -> {resource_append_op "hiwt://$_"}
	  | 'hows://' /.*/ -> {resource_append_op "hows://$_"}
	  | 'howt://' /.*/ -> {resource_append_op "howt://$_"}
	  | 'hrws://' /.*/ -> {resource_append_op "hrws://$_"}
	  | 'hrwt://' /.*/ -> {resource_append_op "hrwt://$_"}
	  | 'hsbws://' /.*/ -> {resource_append_op "hsbws://$_"}
	  | 'hsbwt://' /.*/ -> {resource_append_op "hsbwt://$_"}
	  | 'http://' /.*/ -> {resource_append_op "http://$_"}
	  | 'https://' /.*/ -> {resource_append_op "https://$_"}
	  | 'htws://' /.*/ -> {resource_append_op "htws://$_"}
	  | 'htwt://' /.*/ -> {resource_append_op "htwt://$_"}
	  | 'huws://' /.*/ -> {resource_append_op "huws://$_"}
	  | 'huwt://' /.*/ -> {resource_append_op "huwt://$_"}
	  | 'hyws://' /.*/ -> {resource_append_op "hyws://$_"}
	  | 'hywt://' /.*/ -> {resource_append_op "hywt://$_"}
	  | 'hywws://' /.*/ -> {resource_append_op "hywws://$_"}
	  | 'hywwt://' /.*/ -> {resource_append_op "hywwt://$_"}
	  | 'hzws://' /.*/ -> {resource_append_op "hzws://$_"}
	  | 'hzwt://' /.*/ -> {resource_append_op "hzwt://$_"}
	  | 'iaws://' /.*/ -> {resource_append_op "iaws://$_"}
	  | 'iawt://' /.*/ -> {resource_append_op "iawt://$_"}
	  | 'idws://' /.*/ -> {resource_append_op "idws://$_"}
	  | 'idwt://' /.*/ -> {resource_append_op "idwt://$_"}
	  | 'iews://' /.*/ -> {resource_append_op "iews://$_"}
	  | 'iewt://' /.*/ -> {resource_append_op "iewt://$_"}
	  | 'igws://' /.*/ -> {resource_append_op "igws://$_"}
	  | 'igwt://' /.*/ -> {resource_append_op "igwt://$_"}
	  | 'iiws://' /.*/ -> {resource_append_op "iiws://$_"}
	  | 'iiwt://' /.*/ -> {resource_append_op "iiwt://$_"}
	  | 'ikws://' /.*/ -> {resource_append_op "ikws://$_"}
	  | 'ikwt://' /.*/ -> {resource_append_op "ikwt://$_"}
	  | 'ilows://' /.*/ -> {resource_append_op "ilows://$_"}
	  | 'ilowt://' /.*/ -> {resource_append_op "ilowt://$_"}
	  | 'inhws://' /.*/ -> {resource_append_op "inhws://$_"}
	  | 'inhwt://' /.*/ -> {resource_append_op "inhwt://$_"}
	  | 'iows://' /.*/ -> {resource_append_op "iows://$_"}
	  | 'iowt://' /.*/ -> {resource_append_op "iowt://$_"}
	  | 'isws://' /.*/ -> {resource_append_op "isws://$_"}
	  | 'iswt://' /.*/ -> {resource_append_op "iswt://$_"}
	  | 'itws://' /.*/ -> {resource_append_op "itws://$_"}
	  | 'itwt://' /.*/ -> {resource_append_op "itwt://$_"}
	  | 'iuws://' /.*/ -> {resource_append_op "iuws://$_"}
	  | 'iuwt://' /.*/ -> {resource_append_op "iuwt://$_"}
	  | 'jamws://' /.*/ -> {resource_append_op "jamws://$_"}
	  | 'jamwt://' /.*/ -> {resource_append_op "jamwt://$_"}
	  | 'jaws://' /.*/ -> {resource_append_op "jaws://$_"}
	  | 'jawt://' /.*/ -> {resource_append_op "jawt://$_"}
	  | 'jbows://' /.*/ -> {resource_append_op "jbows://$_"}
	  | 'jbowt://' /.*/ -> {resource_append_op "jbowt://$_"}
	  | 'jvws://' /.*/ -> {resource_append_op "jvws://$_"}
	  | 'jvwt://' /.*/ -> {resource_append_op "jvwt://$_"}
	  | 'kaaws://' /.*/ -> {resource_append_op "kaaws://$_"}
	  | 'kaawt://' /.*/ -> {resource_append_op "kaawt://$_"}
	  | 'kabws://' /.*/ -> {resource_append_op "kabws://$_"}
	  | 'kabwt://' /.*/ -> {resource_append_op "kabwt://$_"}
	  | 'kaws://' /.*/ -> {resource_append_op "kaws://$_"}
	  | 'kawt://' /.*/ -> {resource_append_op "kawt://$_"}
	  | 'kbdws://' /.*/ -> {resource_append_op "kbdws://$_"}
	  | 'kbdwt://' /.*/ -> {resource_append_op "kbdwt://$_"}
	  | 'kbpws://' /.*/ -> {resource_append_op "kbpws://$_"}
	  | 'kbpwt://' /.*/ -> {resource_append_op "kbpwt://$_"}
	  | 'kgws://' /.*/ -> {resource_append_op "kgws://$_"}
	  | 'kgwt://' /.*/ -> {resource_append_op "kgwt://$_"}
	  | 'kiws://' /.*/ -> {resource_append_op "kiws://$_"}
	  | 'kiwt://' /.*/ -> {resource_append_op "kiwt://$_"}
	  | 'kjws://' /.*/ -> {resource_append_op "kjws://$_"}
	  | 'kjwt://' /.*/ -> {resource_append_op "kjwt://$_"}
	  | 'kkws://' /.*/ -> {resource_append_op "kkws://$_"}
	  | 'kkwt://' /.*/ -> {resource_append_op "kkwt://$_"}
	  | 'klws://' /.*/ -> {resource_append_op "klws://$_"}
	  | 'klwt://' /.*/ -> {resource_append_op "klwt://$_"}
	  | 'kmws://' /.*/ -> {resource_append_op "kmws://$_"}
	  | 'kmwt://' /.*/ -> {resource_append_op "kmwt://$_"}
	  | 'knws://' /.*/ -> {resource_append_op "knws://$_"}
	  | 'knwt://' /.*/ -> {resource_append_op "knwt://$_"}
	  | 'koiws://' /.*/ -> {resource_append_op "koiws://$_"}
	  | 'koiwt://' /.*/ -> {resource_append_op "koiwt://$_"}
	  | 'kows://' /.*/ -> {resource_append_op "kows://$_"}
	  | 'kowt://' /.*/ -> {resource_append_op "kowt://$_"}
	  | 'krcws://' /.*/ -> {resource_append_op "krcws://$_"}
	  | 'krcwt://' /.*/ -> {resource_append_op "krcwt://$_"}
	  | 'krws://' /.*/ -> {resource_append_op "krws://$_"}
	  | 'krwt://' /.*/ -> {resource_append_op "krwt://$_"}
	  | 'kshws://' /.*/ -> {resource_append_op "kshws://$_"}
	  | 'kshwt://' /.*/ -> {resource_append_op "kshwt://$_"}
	  | 'ksws://' /.*/ -> {resource_append_op "ksws://$_"}
	  | 'kswt://' /.*/ -> {resource_append_op "kswt://$_"}
	  | 'kuws://' /.*/ -> {resource_append_op "kuws://$_"}
	  | 'kuwt://' /.*/ -> {resource_append_op "kuwt://$_"}
	  | 'kvws://' /.*/ -> {resource_append_op "kvws://$_"}
	  | 'kvwt://' /.*/ -> {resource_append_op "kvwt://$_"}
	  | 'kwws://' /.*/ -> {resource_append_op "kwws://$_"}
	  | 'kwwt://' /.*/ -> {resource_append_op "kwwt://$_"}
	  | 'kyws://' /.*/ -> {resource_append_op "kyws://$_"}
	  | 'kywt://' /.*/ -> {resource_append_op "kywt://$_"}
	  | 'ladws://' /.*/ -> {resource_append_op "ladws://$_"}
	  | 'ladwt://' /.*/ -> {resource_append_op "ladwt://$_"}
	  | 'laws://' /.*/ -> {resource_append_op "laws://$_"}
	  | 'lawt://' /.*/ -> {resource_append_op "lawt://$_"}
	  | 'lbews://' /.*/ -> {resource_append_op "lbews://$_"}
	  | 'lbewt://' /.*/ -> {resource_append_op "lbewt://$_"}
	  | 'lbws://' /.*/ -> {resource_append_op "lbws://$_"}
	  | 'lbwt://' /.*/ -> {resource_append_op "lbwt://$_"}
	  | 'lezws://' /.*/ -> {resource_append_op "lezws://$_"}
	  | 'lezwt://' /.*/ -> {resource_append_op "lezwt://$_"}
	  | 'lfnws://' /.*/ -> {resource_append_op "lfnws://$_"}
	  | 'lfnwt://' /.*/ -> {resource_append_op "lfnwt://$_"}
	  | 'lgws://' /.*/ -> {resource_append_op "lgws://$_"}
	  | 'lgwt://' /.*/ -> {resource_append_op "lgwt://$_"}
	  | 'lijws://' /.*/ -> {resource_append_op "lijws://$_"}
	  | 'lijwt://' /.*/ -> {resource_append_op "lijwt://$_"}
	  | 'liws://' /.*/ -> {resource_append_op "liws://$_"}
	  | 'liwt://' /.*/ -> {resource_append_op "liwt://$_"}
	  | 'lldws://' /.*/ -> {resource_append_op "lldws://$_"}
	  | 'lldwt://' /.*/ -> {resource_append_op "lldwt://$_"}
	  | 'lmows://' /.*/ -> {resource_append_op "lmows://$_"}
	  | 'lmowt://' /.*/ -> {resource_append_op "lmowt://$_"}
	  | 'lnws://' /.*/ -> {resource_append_op "lnws://$_"}
	  | 'lnwt://' /.*/ -> {resource_append_op "lnwt://$_"}
	  | 'lows://' /.*/ -> {resource_append_op "lows://$_"}
	  | 'lowt://' /.*/ -> {resource_append_op "lowt://$_"}
	  | 'lrcws://' /.*/ -> {resource_append_op "lrcws://$_"}
	  | 'lrcwt://' /.*/ -> {resource_append_op "lrcwt://$_"}
	  | 'ltgws://' /.*/ -> {resource_append_op "ltgws://$_"}
	  | 'ltgwt://' /.*/ -> {resource_append_op "ltgwt://$_"}
	  | 'ltws://' /.*/ -> {resource_append_op "ltws://$_"}
	  | 'ltwt://' /.*/ -> {resource_append_op "ltwt://$_"}
	  | 'lvws://' /.*/ -> {resource_append_op "lvws://$_"}
	  | 'lvwt://' /.*/ -> {resource_append_op "lvwt://$_"}
	  | 'maiws://' /.*/ -> {resource_append_op "maiws://$_"}
	  | 'maiwt://' /.*/ -> {resource_append_op "maiwt://$_"}
	  | 'map-bmsws://' /.*/ -> {resource_append_op "map-bmsws://$_"}
	  | 'map-bmswt://' /.*/ -> {resource_append_op "map-bmswt://$_"}
	  | 'mdfws://' /.*/ -> {resource_append_op "mdfws://$_"}
	  | 'mdfwt://' /.*/ -> {resource_append_op "mdfwt://$_"}
	  | 'mgws://' /.*/ -> {resource_append_op "mgws://$_"}
	  | 'mgwt://' /.*/ -> {resource_append_op "mgwt://$_"}
	  | 'mhrws://' /.*/ -> {resource_append_op "mhrws://$_"}
	  | 'mhrwt://' /.*/ -> {resource_append_op "mhrwt://$_"}
	  | 'mhws://' /.*/ -> {resource_append_op "mhws://$_"}
	  | 'mhwt://' /.*/ -> {resource_append_op "mhwt://$_"}
	  | 'minws://' /.*/ -> {resource_append_op "minws://$_"}
	  | 'minwt://' /.*/ -> {resource_append_op "minwt://$_"}
	  | 'miws://' /.*/ -> {resource_append_op "miws://$_"}
	  | 'miwt://' /.*/ -> {resource_append_op "miwt://$_"}
	  | 'mkws://' /.*/ -> {resource_append_op "mkws://$_"}
	  | 'mkwt://' /.*/ -> {resource_append_op "mkwt://$_"}
	  | 'mlws://' /.*/ -> {resource_append_op "mlws://$_"}
	  | 'mlwt://' /.*/ -> {resource_append_op "mlwt://$_"}
	  | 'mnws://' /.*/ -> {resource_append_op "mnws://$_"}
	  | 'mnwt://' /.*/ -> {resource_append_op "mnwt://$_"}
	  | 'mnwws://' /.*/ -> {resource_append_op "mnwws://$_"}
	  | 'mnwwt://' /.*/ -> {resource_append_op "mnwwt://$_"}
	  | 'mrjws://' /.*/ -> {resource_append_op "mrjws://$_"}
	  | 'mrjwt://' /.*/ -> {resource_append_op "mrjwt://$_"}
	  | 'mrws://' /.*/ -> {resource_append_op "mrws://$_"}
	  | 'mrwt://' /.*/ -> {resource_append_op "mrwt://$_"}
	  | 'msws://' /.*/ -> {resource_append_op "msws://$_"}
	  | 'mswt://' /.*/ -> {resource_append_op "mswt://$_"}
	  | 'mtws://' /.*/ -> {resource_append_op "mtws://$_"}
	  | 'mtwt://' /.*/ -> {resource_append_op "mtwt://$_"}
	  | 'musws://' /.*/ -> {resource_append_op "musws://$_"}
	  | 'muswt://' /.*/ -> {resource_append_op "muswt://$_"}
	  | 'mwlws://' /.*/ -> {resource_append_op "mwlws://$_"}
	  | 'mwlwt://' /.*/ -> {resource_append_op "mwlwt://$_"}
	  | 'myvws://' /.*/ -> {resource_append_op "myvws://$_"}
	  | 'myvwt://' /.*/ -> {resource_append_op "myvwt://$_"}
	  | 'myws://' /.*/ -> {resource_append_op "myws://$_"}
	  | 'mywt://' /.*/ -> {resource_append_op "mywt://$_"}
	  | 'mznws://' /.*/ -> {resource_append_op "mznws://$_"}
	  | 'mznwt://' /.*/ -> {resource_append_op "mznwt://$_"}
	  | 'nahws://' /.*/ -> {resource_append_op "nahws://$_"}
	  | 'nahwt://' /.*/ -> {resource_append_op "nahwt://$_"}
	  | 'napws://' /.*/ -> {resource_append_op "napws://$_"}
	  | 'napwt://' /.*/ -> {resource_append_op "napwt://$_"}
	  | 'naws://' /.*/ -> {resource_append_op "naws://$_"}
	  | 'nawt://' /.*/ -> {resource_append_op "nawt://$_"}
	  | 'nds-nlws://' /.*/ -> {resource_append_op "nds-nlws://$_"}
	  | 'nds-nlwt://' /.*/ -> {resource_append_op "nds-nlwt://$_"}
	  | 'ndsws://' /.*/ -> {resource_append_op "ndsws://$_"}
	  | 'ndswt://' /.*/ -> {resource_append_op "ndswt://$_"}
	  | 'news://' /.*/ -> {resource_append_op "news://$_"}
	  | 'newt://' /.*/ -> {resource_append_op "newt://$_"}
	  | 'newws://' /.*/ -> {resource_append_op "newws://$_"}
	  | 'newwt://' /.*/ -> {resource_append_op "newwt://$_"}
	  | 'ngws://' /.*/ -> {resource_append_op "ngws://$_"}
	  | 'ngwt://' /.*/ -> {resource_append_op "ngwt://$_"}
	  | 'nlws://' /.*/ -> {resource_append_op "nlws://$_"}
	  | 'nlwt://' /.*/ -> {resource_append_op "nlwt://$_"}
	  | 'nnws://' /.*/ -> {resource_append_op "nnws://$_"}
	  | 'nnwt://' /.*/ -> {resource_append_op "nnwt://$_"}
	  | 'novws://' /.*/ -> {resource_append_op "novws://$_"}
	  | 'novwt://' /.*/ -> {resource_append_op "novwt://$_"}
	  | 'nows://' /.*/ -> {resource_append_op "nows://$_"}
	  | 'nowt://' /.*/ -> {resource_append_op "nowt://$_"}
	  | 'nqows://' /.*/ -> {resource_append_op "nqows://$_"}
	  | 'nqowt://' /.*/ -> {resource_append_op "nqowt://$_"}
	  | 'nrmws://' /.*/ -> {resource_append_op "nrmws://$_"}
	  | 'nrmwt://' /.*/ -> {resource_append_op "nrmwt://$_"}
	  | 'nsows://' /.*/ -> {resource_append_op "nsows://$_"}
	  | 'nsowt://' /.*/ -> {resource_append_op "nsowt://$_"}
	  | 'nvws://' /.*/ -> {resource_append_op "nvws://$_"}
	  | 'nvwt://' /.*/ -> {resource_append_op "nvwt://$_"}
	  | 'nyws://' /.*/ -> {resource_append_op "nyws://$_"}
	  | 'nywt://' /.*/ -> {resource_append_op "nywt://$_"}
	  | 'ocws://' /.*/ -> {resource_append_op "ocws://$_"}
	  | 'ocwt://' /.*/ -> {resource_append_op "ocwt://$_"}
	  | 'olows://' /.*/ -> {resource_append_op "olows://$_"}
	  | 'olowt://' /.*/ -> {resource_append_op "olowt://$_"}
	  | 'omws://' /.*/ -> {resource_append_op "omws://$_"}
	  | 'omwt://' /.*/ -> {resource_append_op "omwt://$_"}
	  | 'orws://' /.*/ -> {resource_append_op "orws://$_"}
	  | 'orwt://' /.*/ -> {resource_append_op "orwt://$_"}
	  | 'osws://' /.*/ -> {resource_append_op "osws://$_"}
	  | 'oswt://' /.*/ -> {resource_append_op "oswt://$_"}
	  | 'pagws://' /.*/ -> {resource_append_op "pagws://$_"}
	  | 'pagwt://' /.*/ -> {resource_append_op "pagwt://$_"}
	  | 'pamws://' /.*/ -> {resource_append_op "pamws://$_"}
	  | 'pamwt://' /.*/ -> {resource_append_op "pamwt://$_"}
	  | 'papws://' /.*/ -> {resource_append_op "papws://$_"}
	  | 'papwt://' /.*/ -> {resource_append_op "papwt://$_"}
	  | 'paws://' /.*/ -> {resource_append_op "paws://$_"}
	  | 'pawt://' /.*/ -> {resource_append_op "pawt://$_"}
	  | 'pcdws://' /.*/ -> {resource_append_op "pcdws://$_"}
	  | 'pcdwt://' /.*/ -> {resource_append_op "pcdwt://$_"}
	  | 'pdcws://' /.*/ -> {resource_append_op "pdcws://$_"}
	  | 'pdcwt://' /.*/ -> {resource_append_op "pdcwt://$_"}
	  | 'pflws://' /.*/ -> {resource_append_op "pflws://$_"}
	  | 'pflwt://' /.*/ -> {resource_append_op "pflwt://$_"}
	  | 'pihws://' /.*/ -> {resource_append_op "pihws://$_"}
	  | 'pihwt://' /.*/ -> {resource_append_op "pihwt://$_"}
	  | 'piws://' /.*/ -> {resource_append_op "piws://$_"}
	  | 'piwt://' /.*/ -> {resource_append_op "piwt://$_"}
	  | 'plws://' /.*/ -> {resource_append_op "plws://$_"}
	  | 'plwt://' /.*/ -> {resource_append_op "plwt://$_"}
	  | 'pmsws://' /.*/ -> {resource_append_op "pmsws://$_"}
	  | 'pmswt://' /.*/ -> {resource_append_op "pmswt://$_"}
	  | 'pnbws://' /.*/ -> {resource_append_op "pnbws://$_"}
	  | 'pnbwt://' /.*/ -> {resource_append_op "pnbwt://$_"}
	  | 'pntws://' /.*/ -> {resource_append_op "pntws://$_"}
	  | 'pntwt://' /.*/ -> {resource_append_op "pntwt://$_"}
	  | 'psws://' /.*/ -> {resource_append_op "psws://$_"}
	  | 'pswt://' /.*/ -> {resource_append_op "pswt://$_"}
	  | 'ptws://' /.*/ -> {resource_append_op "ptws://$_"}
	  | 'ptwt://' /.*/ -> {resource_append_op "ptwt://$_"}
	  | 'quws://' /.*/ -> {resource_append_op "quws://$_"}
	  | 'quwt://' /.*/ -> {resource_append_op "quwt://$_"}
	  | 'rmws://' /.*/ -> {resource_append_op "rmws://$_"}
	  | 'rmwt://' /.*/ -> {resource_append_op "rmwt://$_"}
	  | 'rmyws://' /.*/ -> {resource_append_op "rmyws://$_"}
	  | 'rmywt://' /.*/ -> {resource_append_op "rmywt://$_"}
	  | 'rnws://' /.*/ -> {resource_append_op "rnws://$_"}
	  | 'rnwt://' /.*/ -> {resource_append_op "rnwt://$_"}
	  | 'roa-rupws://' /.*/ -> {resource_append_op "roa-rupws://$_"}
	  | 'roa-rupwt://' /.*/ -> {resource_append_op "roa-rupwt://$_"}
	  | 'roa-taraws://' /.*/ -> {resource_append_op "roa-taraws://$_"}
	  | 'roa-tarawt://' /.*/ -> {resource_append_op "roa-tarawt://$_"}
	  | 'rows://' /.*/ -> {resource_append_op "rows://$_"}
	  | 'rowt://' /.*/ -> {resource_append_op "rowt://$_"}
	  | 'ruews://' /.*/ -> {resource_append_op "ruews://$_"}
	  | 'ruewt://' /.*/ -> {resource_append_op "ruewt://$_"}
	  | 'ruws://' /.*/ -> {resource_append_op "ruws://$_"}
	  | 'ruwt://' /.*/ -> {resource_append_op "ruwt://$_"}
	  | 'rwws://' /.*/ -> {resource_append_op "rwws://$_"}
	  | 'rwwt://' /.*/ -> {resource_append_op "rwwt://$_"}
	  | 's3cmd://' /.*/ -> {resource_append_op "s3cmd://$_"}
	  | 'sahws://' /.*/ -> {resource_append_op "sahws://$_"}
	  | 'sahwt://' /.*/ -> {resource_append_op "sahwt://$_"}
	  | 'satws://' /.*/ -> {resource_append_op "satws://$_"}
	  | 'satwt://' /.*/ -> {resource_append_op "satwt://$_"}
	  | 'saws://' /.*/ -> {resource_append_op "saws://$_"}
	  | 'sawt://' /.*/ -> {resource_append_op "sawt://$_"}
	  | 'scnws://' /.*/ -> {resource_append_op "scnws://$_"}
	  | 'scnwt://' /.*/ -> {resource_append_op "scnwt://$_"}
	  | 'scows://' /.*/ -> {resource_append_op "scows://$_"}
	  | 'scowt://' /.*/ -> {resource_append_op "scowt://$_"}
	  | 'scws://' /.*/ -> {resource_append_op "scws://$_"}
	  | 'scwt://' /.*/ -> {resource_append_op "scwt://$_"}
	  | 'sdws://' /.*/ -> {resource_append_op "sdws://$_"}
	  | 'sdwt://' /.*/ -> {resource_append_op "sdwt://$_"}
	  | 'sews://' /.*/ -> {resource_append_op "sews://$_"}
	  | 'sewt://' /.*/ -> {resource_append_op "sewt://$_"}
	  | 'sftp://' /.*/ -> {resource_append_op "sftp://$_"}
	  | 'sgws://' /.*/ -> {resource_append_op "sgws://$_"}
	  | 'sgwt://' /.*/ -> {resource_append_op "sgwt://$_"}
	  | 'shnws://' /.*/ -> {resource_append_op "shnws://$_"}
	  | 'shnwt://' /.*/ -> {resource_append_op "shnwt://$_"}
	  | 'shws://' /.*/ -> {resource_append_op "shws://$_"}
	  | 'shwt://' /.*/ -> {resource_append_op "shwt://$_"}
	  | 'simplews://' /.*/ -> {resource_append_op "simplews://$_"}
	  | 'simplewt://' /.*/ -> {resource_append_op "simplewt://$_"}
	  | 'siws://' /.*/ -> {resource_append_op "siws://$_"}
	  | 'siwt://' /.*/ -> {resource_append_op "siwt://$_"}
	  | 'skws://' /.*/ -> {resource_append_op "skws://$_"}
	  | 'skwt://' /.*/ -> {resource_append_op "skwt://$_"}
	  | 'slws://' /.*/ -> {resource_append_op "slws://$_"}
	  | 'slwt://' /.*/ -> {resource_append_op "slwt://$_"}
	  | 'smws://' /.*/ -> {resource_append_op "smws://$_"}
	  | 'smwt://' /.*/ -> {resource_append_op "smwt://$_"}
	  | 'snws://' /.*/ -> {resource_append_op "snws://$_"}
	  | 'snwt://' /.*/ -> {resource_append_op "snwt://$_"}
	  | 'solr://' /.*/ -> {resource_append_op "solr://$_"}
	  | 'sows://' /.*/ -> {resource_append_op "sows://$_"}
	  | 'sowt://' /.*/ -> {resource_append_op "sowt://$_"}
	  | 'sqlite://' /.*/ -> {resource_append_op "sqlite://$_"}
	  | 'sqliteq://' /.*/ -> {resource_append_op "sqliteq://$_"}
	  | 'sqlites://' /.*/ -> {resource_append_op "sqlites://$_"}
	  | 'sqlitet://' /.*/ -> {resource_append_op "sqlitet://$_"}
	  | 'sqws://' /.*/ -> {resource_append_op "sqws://$_"}
	  | 'sqwt://' /.*/ -> {resource_append_op "sqwt://$_"}
	  | 'srnws://' /.*/ -> {resource_append_op "srnws://$_"}
	  | 'srnwt://' /.*/ -> {resource_append_op "srnwt://$_"}
	  | 'srws://' /.*/ -> {resource_append_op "srws://$_"}
	  | 'srwt://' /.*/ -> {resource_append_op "srwt://$_"}
	  | 'ssws://' /.*/ -> {resource_append_op "ssws://$_"}
	  | 'sswt://' /.*/ -> {resource_append_op "sswt://$_"}
	  | 'stqws://' /.*/ -> {resource_append_op "stqws://$_"}
	  | 'stqwt://' /.*/ -> {resource_append_op "stqwt://$_"}
	  | 'stws://' /.*/ -> {resource_append_op "stws://$_"}
	  | 'stwt://' /.*/ -> {resource_append_op "stwt://$_"}
	  | 'suws://' /.*/ -> {resource_append_op "suws://$_"}
	  | 'suwt://' /.*/ -> {resource_append_op "suwt://$_"}
	  | 'svws://' /.*/ -> {resource_append_op "svws://$_"}
	  | 'svwt://' /.*/ -> {resource_append_op "svwt://$_"}
	  | 'swws://' /.*/ -> {resource_append_op "swws://$_"}
	  | 'swwt://' /.*/ -> {resource_append_op "swwt://$_"}
	  | 'szlws://' /.*/ -> {resource_append_op "szlws://$_"}
	  | 'szlwt://' /.*/ -> {resource_append_op "szlwt://$_"}
	  | 'szyws://' /.*/ -> {resource_append_op "szyws://$_"}
	  | 'szywt://' /.*/ -> {resource_append_op "szywt://$_"}
	  | 'tar://' /.*/ -> {resource_append_op "tar://$_"}
	  | 'tarentry://' /.*/ -> {resource_append_op "tarentry://$_"}
	  | 'taws://' /.*/ -> {resource_append_op "taws://$_"}
	  | 'tawt://' /.*/ -> {resource_append_op "tawt://$_"}
	  | 'tcyws://' /.*/ -> {resource_append_op "tcyws://$_"}
	  | 'tcywt://' /.*/ -> {resource_append_op "tcywt://$_"}
	  | 'tetws://' /.*/ -> {resource_append_op "tetws://$_"}
	  | 'tetwt://' /.*/ -> {resource_append_op "tetwt://$_"}
	  | 'tews://' /.*/ -> {resource_append_op "tews://$_"}
	  | 'tewt://' /.*/ -> {resource_append_op "tewt://$_"}
	  | 'tgws://' /.*/ -> {resource_append_op "tgws://$_"}
	  | 'tgwt://' /.*/ -> {resource_append_op "tgwt://$_"}
	  | 'thws://' /.*/ -> {resource_append_op "thws://$_"}
	  | 'thwt://' /.*/ -> {resource_append_op "thwt://$_"}
	  | 'tiws://' /.*/ -> {resource_append_op "tiws://$_"}
	  | 'tiwt://' /.*/ -> {resource_append_op "tiwt://$_"}
	  | 'tkws://' /.*/ -> {resource_append_op "tkws://$_"}
	  | 'tkwt://' /.*/ -> {resource_append_op "tkwt://$_"}
	  | 'tlws://' /.*/ -> {resource_append_op "tlws://$_"}
	  | 'tlwt://' /.*/ -> {resource_append_op "tlwt://$_"}
	  | 'tnws://' /.*/ -> {resource_append_op "tnws://$_"}
	  | 'tnwt://' /.*/ -> {resource_append_op "tnwt://$_"}
	  | 'tows://' /.*/ -> {resource_append_op "tows://$_"}
	  | 'towt://' /.*/ -> {resource_append_op "towt://$_"}
	  | 'tpiws://' /.*/ -> {resource_append_op "tpiws://$_"}
	  | 'tpiwt://' /.*/ -> {resource_append_op "tpiwt://$_"}
	  | 'trws://' /.*/ -> {resource_append_op "trws://$_"}
	  | 'trwt://' /.*/ -> {resource_append_op "trwt://$_"}
	  | 'tsws://' /.*/ -> {resource_append_op "tsws://$_"}
	  | 'tswt://' /.*/ -> {resource_append_op "tswt://$_"}
	  | 'ttws://' /.*/ -> {resource_append_op "ttws://$_"}
	  | 'ttwt://' /.*/ -> {resource_append_op "ttwt://$_"}
	  | 'tumws://' /.*/ -> {resource_append_op "tumws://$_"}
	  | 'tumwt://' /.*/ -> {resource_append_op "tumwt://$_"}
	  | 'twws://' /.*/ -> {resource_append_op "twws://$_"}
	  | 'twwt://' /.*/ -> {resource_append_op "twwt://$_"}
	  | 'tyvws://' /.*/ -> {resource_append_op "tyvws://$_"}
	  | 'tyvwt://' /.*/ -> {resource_append_op "tyvwt://$_"}
	  | 'tyws://' /.*/ -> {resource_append_op "tyws://$_"}
	  | 'tywt://' /.*/ -> {resource_append_op "tywt://$_"}
	  | 'udmws://' /.*/ -> {resource_append_op "udmws://$_"}
	  | 'udmwt://' /.*/ -> {resource_append_op "udmwt://$_"}
	  | 'ugws://' /.*/ -> {resource_append_op "ugws://$_"}
	  | 'ugwt://' /.*/ -> {resource_append_op "ugwt://$_"}
	  | 'ukws://' /.*/ -> {resource_append_op "ukws://$_"}
	  | 'ukwt://' /.*/ -> {resource_append_op "ukwt://$_"}
	  | 'urws://' /.*/ -> {resource_append_op "urws://$_"}
	  | 'urwt://' /.*/ -> {resource_append_op "urwt://$_"}
	  | 'uzws://' /.*/ -> {resource_append_op "uzws://$_"}
	  | 'uzwt://' /.*/ -> {resource_append_op "uzwt://$_"}
	  | 'vecws://' /.*/ -> {resource_append_op "vecws://$_"}
	  | 'vecwt://' /.*/ -> {resource_append_op "vecwt://$_"}
	  | 'vepws://' /.*/ -> {resource_append_op "vepws://$_"}
	  | 'vepwt://' /.*/ -> {resource_append_op "vepwt://$_"}
	  | 'vews://' /.*/ -> {resource_append_op "vews://$_"}
	  | 'vewt://' /.*/ -> {resource_append_op "vewt://$_"}
	  | 'viws://' /.*/ -> {resource_append_op "viws://$_"}
	  | 'viwt://' /.*/ -> {resource_append_op "viwt://$_"}
	  | 'vlsws://' /.*/ -> {resource_append_op "vlsws://$_"}
	  | 'vlswt://' /.*/ -> {resource_append_op "vlswt://$_"}
	  | 'vows://' /.*/ -> {resource_append_op "vows://$_"}
	  | 'vowt://' /.*/ -> {resource_append_op "vowt://$_"}
	  | 'warws://' /.*/ -> {resource_append_op "warws://$_"}
	  | 'warwt://' /.*/ -> {resource_append_op "warwt://$_"}
	  | 'waws://' /.*/ -> {resource_append_op "waws://$_"}
	  | 'wawt://' /.*/ -> {resource_append_op "wawt://$_"}
	  | 'wiki://' /.*/ -> {resource_append_op "wiki://$_"}
	  | 'wows://' /.*/ -> {resource_append_op "wows://$_"}
	  | 'wowt://' /.*/ -> {resource_append_op "wowt://$_"}
	  | 'wuuws://' /.*/ -> {resource_append_op "wuuws://$_"}
	  | 'wuuwt://' /.*/ -> {resource_append_op "wuuwt://$_"}
	  | 'xalws://' /.*/ -> {resource_append_op "xalws://$_"}
	  | 'xalwt://' /.*/ -> {resource_append_op "xalwt://$_"}
	  | 'xhws://' /.*/ -> {resource_append_op "xhws://$_"}
	  | 'xhwt://' /.*/ -> {resource_append_op "xhwt://$_"}
	  | 'xlsx://' /.*/ -> {resource_append_op "xlsx://$_"}
	  | 'xlsxsheet://' /.*/ -> {resource_append_op "xlsxsheet://$_"}
	  | 'xmfws://' /.*/ -> {resource_append_op "xmfws://$_"}
	  | 'xmfwt://' /.*/ -> {resource_append_op "xmfwt://$_"}
	  | 'yiws://' /.*/ -> {resource_append_op "yiws://$_"}
	  | 'yiwt://' /.*/ -> {resource_append_op "yiwt://$_"}
	  | 'yows://' /.*/ -> {resource_append_op "yows://$_"}
	  | 'yowt://' /.*/ -> {resource_append_op "yowt://$_"}
	  | 'yt://' /.*/ -> {resource_append_op "yt://$_"}
	  | 'zaws://' /.*/ -> {resource_append_op "zaws://$_"}
	  | 'zawt://' /.*/ -> {resource_append_op "zawt://$_"}
	  | 'zeaws://' /.*/ -> {resource_append_op "zeaws://$_"}
	  | 'zeawt://' /.*/ -> {resource_append_op "zeawt://$_"}
	  | 'zh-classicalws://' /.*/ -> {resource_append_op "zh-classicalws://$_"}
	  | 'zh-classicalwt://' /.*/ -> {resource_append_op "zh-classicalwt://$_"}
	  | 'zh-min-nanws://' /.*/ -> {resource_append_op "zh-min-nanws://$_"}
	  | 'zh-min-nanwt://' /.*/ -> {resource_append_op "zh-min-nanwt://$_"}
	  | 'zh-yuews://' /.*/ -> {resource_append_op "zh-yuews://$_"}
	  | 'zh-yuewt://' /.*/ -> {resource_append_op "zh-yuewt://$_"}
	  | 'zhws://' /.*/ -> {resource_append_op "zhws://$_"}
	  | 'zhwt://' /.*/ -> {resource_append_op "zhwt://$_"}
	  | 'zip://' /.*/ -> {resource_append_op "zip://$_"}
	  | 'zipentry://' /.*/ -> {resource_append_op "zipentry://$_"}
	  | 'zuws://' /.*/ -> {resource_append_op "zuws://$_"}
	  | 'zuwt://' /.*/ -> {resource_append_op "zuwt://$_"}
	  )
	| <filename> -> {cat_op $_}
	| </short>
	)

# PARSER /qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| </lambda>
	| </suffix>
	)

# PARSER /series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  </op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER /short
	Dispatch table for short options in context ''

## DEFINITION
	(
	| '!' (
	  | 'p' <perl_asserter_code> -> {perl_assert_op $_}
	  )
	| '$Hcae' '' -> {conf_get_op 'Hcae'}
	| '$Hccp' '' -> {conf_get_op 'Hccp'}
	| '$Hcgu' '' -> {conf_get_op 'Hcgu'}
	| '$Hcld' '' -> {conf_get_op 'Hcld'}
	| '$Hcmm' '' -> {conf_get_op 'Hcmm'}
	| '$Hcof' '' -> {conf_get_op 'Hcof'}
	| '$Hcpp' '' -> {conf_get_op 'Hcpp'}
	| '$Hcps' '' -> {conf_get_op 'Hcps'}
	| '$Hcrm' '' -> {conf_get_op 'Hcrm'}
	| '$Hcsfr' '' -> {conf_get_op 'Hcsfr'}
	| '$Hctd' '' -> {conf_get_op 'Hctd'}
	| '$Hdb' '' -> {conf_get_op 'Hdb'}
	| '$Hdbpc' '' -> {conf_get_op 'Hdbpc'}
	| '$Hdchkr' '' -> {conf_get_op 'Hdchkr'}
	| '$Hdchna' '' -> {conf_get_op 'Hdchna'}
	| '$Hdcrps' '' -> {conf_get_op 'Hdcrps'}
	| '$Hdcst' '' -> {conf_get_op 'Hdcst'}
	| '$Hdcwps' '' -> {conf_get_op 'Hdcwps'}
	| '$Hddnbb' '' -> {conf_get_op 'Hddnbb'}
	| '$Hddndd' '' -> {conf_get_op 'Hddndd'}
	| '$Hddnh' '' -> {conf_get_op 'Hddnh'}
	| '$Hddnmtt' '' -> {conf_get_op 'Hddnmtt'}
	| '$Hddns' '' -> {conf_get_op 'Hddns'}
	| '$Hdmsi' '' -> {conf_get_op 'Hdmsi'}
	| '$Hdnnap' '' -> {conf_get_op 'Hdnnap'}
	| '$Hdnnba' '' -> {conf_get_op 'Hdnnba'}
	| '$Hdnnbha' '' -> {conf_get_op 'Hdnnbha'}
	| '$Hdnncd' '' -> {conf_get_op 'Hdnncd'}
	| '$Hdnnced' '' -> {conf_get_op 'Hdnnced'}
	| '$Hdnncp' '' -> {conf_get_op 'Hdnncp'}
	| '$Hdnned' '' -> {conf_get_op 'Hdnned'}
	| '$Hdnnhri' '' -> {conf_get_op 'Hdnnhri'}
	| '$Hdnnhttp' '' -> {conf_get_op 'Hdnnhttp'}
	| '$Hdnnhttps' '' -> {conf_get_op 'Hdnnhttps'}
	| '$Hdnnmo' '' -> {conf_get_op 'Hdnnmo'}
	| '$Hdnnnd' '' -> {conf_get_op 'Hdnnnd'}
	| '$Hdnnndr' '' -> {conf_get_op 'Hdnnndr'}
	| '$Hdnnrc' '' -> {conf_get_op 'Hdnnrc'}
	| '$Hdnnri' '' -> {conf_get_op 'Hdnnri'}
	| '$Hdnnrm' '' -> {conf_get_op 'Hdnnrm'}
	| '$Hdnnrms' '' -> {conf_get_op 'Hdnnrms'}
	| '$Hdnnrpts' '' -> {conf_get_op 'Hdnnrpts'}
	| '$Hdnnse' '' -> {conf_get_op 'Hdnnse'}
	| '$Hdnnsha' '' -> {conf_get_op 'Hdnnsha'}
	| '$Hdnnstp' '' -> {conf_get_op 'Hdnnstp'}
	| '$Hdnnup' '' -> {conf_get_op 'Hdnnup'}
	| '$Hdpe' '' -> {conf_get_op 'Hdpe'}
	| '$Hdps' '' -> {conf_get_op 'Hdps'}
	| '$Hfcbd' '' -> {conf_get_op 'Hfcbd'}
	| '$Hfd' '' -> {conf_get_op 'Hfd'}
	| '$Hfdfs' '' -> {conf_get_op 'Hfdfs'}
	| '$Hfdi' '' -> {conf_get_op 'Hfdi'}
	| '$Hfieldsep' '' -> {conf_get_op 'Hfieldsep'}
	| '$Hfmokvfs' '' -> {conf_get_op 'Hfmokvfs'}
	| '$Hfrokvfs' '' -> {conf_get_op 'Hfrokvfs'}
	| '$Hifi' '' -> {conf_get_op 'Hifi'}
	| '$Hifsmax' '' -> {conf_get_op 'Hifsmax'}
	| '$Hifsmin' '' -> {conf_get_op 'Hifsmin'}
	| '$Hifsmpn' '' -> {conf_get_op 'Hifsmpn'}
	| '$Hifsmpr' '' -> {conf_get_op 'Hifsmpr'}
	| '$Hikkvs' '' -> {conf_get_op 'Hikkvs'}
	| '$Hill' '' -> {conf_get_op 'Hill'}
	| '$Hillm' '' -> {conf_get_op 'Hillm'}
	| '$Himdf' '' -> {conf_get_op 'Himdf'}
	| '$Himdm' '' -> {conf_get_op 'Himdm'}
	| '$Hinla' '' -> {conf_get_op 'Hinla'}
	| '$Hipc' '' -> {conf_get_op 'Hipc'}
	| '$Hisc' '' -> {conf_get_op 'Hisc'}
	| '$Hisf' '' -> {conf_get_op 'Hisf'}
	| '$Hisr' '' -> {conf_get_op 'Hisr'}
	| '$Hjca' '' -> {conf_get_op 'Hjca'}
	| '$Hjcat' '' -> {conf_get_op 'Hjcat'}
	| '$Hjcc' '' -> {conf_get_op 'Hjcc'}
	| '$Hjcci' '' -> {conf_get_op 'Hjcci'}
	| '$Hjcf' '' -> {conf_get_op 'Hjcf'}
	| '$Hjcft' '' -> {conf_get_op 'Hjcft'}
	| '$Hjcla' '' -> {conf_get_op 'Hjcla'}
	| '$Hjclf' '' -> {conf_get_op 'Hjclf'}
	| '$Hjcpa' '' -> {conf_get_op 'Hjcpa'}
	| '$Hjcpf' '' -> {conf_get_op 'Hjcpf'}
	| '$Hjcsc' '' -> {conf_get_op 'Hjcsc'}
	| '$Hjcscn' '' -> {conf_get_op 'Hjcscn'}
	| '$Hje' '' -> {conf_get_op 'Hje'}
	| '$Hjenra' '' -> {conf_get_op 'Hjenra'}
	| '$Hjenri' '' -> {conf_get_op 'Hjenri'}
	| '$Hjenu' '' -> {conf_get_op 'Hjenu'}
	| '$Hji' '' -> {conf_get_op 'Hji'}
	| '$Hjic' '' -> {conf_get_op 'Hjic'}
	| '$Hjj' '' -> {conf_get_op 'Hjj'}
	| '$Hjjn' '' -> {conf_get_op 'Hjjn'}
	| '$Hjk' '' -> {conf_get_op 'Hjk'}
	| '$Hjld' '' -> {conf_get_op 'Hjld'}
	| '$Hjm' '' -> {conf_get_op 'Hjm'}
	| '$Hjmc' '' -> {conf_get_op 'Hjmc'}
	| '$Hjmpt' '' -> {conf_get_op 'Hjmpt'}
	| '$Hjn' '' -> {conf_get_op 'Hjn'}
	| '$Hjoc' '' -> {conf_get_op 'Hjoc'}
	| '$Hjogcc' '' -> {conf_get_op 'Hjogcc'}
	| '$Hjokc' '' -> {conf_get_op 'Hjokc'}
	| '$Hjokcc' '' -> {conf_get_op 'Hjokcc'}
	| '$Hjovc' '' -> {conf_get_op 'Hjovc'}
	| '$Hjp' '' -> {conf_get_op 'Hjp'}
	| '$Hjpc' '' -> {conf_get_op 'Hjpc'}
	| '$Hjq' '' -> {conf_get_op 'Hjq'}
	| '$Hjr' '' -> {conf_get_op 'Hjr'}
	| '$Hjrc' '' -> {conf_get_op 'Hjrc'}
	| '$Hjrsc' '' -> {conf_get_op 'Hjrsc'}
	| '$Hjs' '' -> {conf_get_op 'Hjs'}
	| '$Hjso' '' -> {conf_get_op 'Hjso'}
	| '$Hjssc' '' -> {conf_get_op 'Hjssc'}
	| '$Hjssnt' '' -> {conf_get_op 'Hjssnt'}
	| '$Hjsstt' '' -> {conf_get_op 'Hjsstt'}
	| '$Hjta' '' -> {conf_get_op 'Hjta'}
	| '$Hjtbat' '' -> {conf_get_op 'Hjtbat'}
	| '$Hjteti' '' -> {conf_get_op 'Hjteti'}
	| '$Hjtha' '' -> {conf_get_op 'Hjtha'}
	| '$Hjthc' '' -> {conf_get_op 'Hjthc'}
	| '$Hjthef' '' -> {conf_get_op 'Hjthef'}
	| '$Hjthf' '' -> {conf_get_op 'Hjthf'}
	| '$Hjthis' '' -> {conf_get_op 'Hjthis'}
	| '$Hjti' '' -> {conf_get_op 'Hjti'}
	| '$Hjtjbs' '' -> {conf_get_op 'Hjtjbs'}
	| '$Hjtjcl' '' -> {conf_get_op 'Hjtjcl'}
	| '$Hjtjl' '' -> {conf_get_op 'Hjtjl'}
	| '$Hjtjlcs' '' -> {conf_get_op 'Hjtjlcs'}
	| '$Hjtjt' '' -> {conf_get_op 'Hjtjt'}
	| '$Hjtmmm' '' -> {conf_get_op 'Hjtmmm'}
	| '$Hjtmp' '' -> {conf_get_op 'Hjtmp'}
	| '$Hjtmrm' '' -> {conf_get_op 'Hjtmrm'}
	| '$Hjtpja' '' -> {conf_get_op 'Hjtpja'}
	| '$Hjtpjd' '' -> {conf_get_op 'Hjtpjd'}
	| '$Hjtpjh' '' -> {conf_get_op 'Hjtpjh'}
	| '$Hjtr' '' -> {conf_get_op 'Hjtr'}
	| '$Hjtrcs' '' -> {conf_get_op 'Hjtrcs'}
	| '$Hjtrr' '' -> {conf_get_op 'Hjtrr'}
	| '$Hjtsd' '' -> {conf_get_op 'Hjtsd'}
	| '$Hjtt' '' -> {conf_get_op 'Hjtt'}
	| '$Hjttl' '' -> {conf_get_op 'Hjttl'}
	| '$Hjttmp' '' -> {conf_get_op 'Hjttmp'}
	| '$Hjtttc' '' -> {conf_get_op 'Hjtttc'}
	| '$Hjtttm' '' -> {conf_get_op 'Hjtttm'}
	| '$Hjtwt' '' -> {conf_get_op 'Hjtwt'}
	| '$Hjun' '' -> {conf_get_op 'Hjun'}
	| '$Hjurh' '' -> {conf_get_op 'Hjurh'}
	| '$Hjwd' '' -> {conf_get_op 'Hjwd'}
	| '$Hmcm' '' -> {conf_get_op 'Hmcm'}
	| '$Hmds' '' -> {conf_get_op 'Hmds'}
	| '$Hme' '' -> {conf_get_op 'Hme'}
	| '$Hmfm' '' -> {conf_get_op 'Hmfm'}
	| '$Hmif' '' -> {conf_get_op 'Hmif'}
	| '$Hmil' '' -> {conf_get_op 'Hmil'}
	| '$Hmis' '' -> {conf_get_op 'Hmis'}
	| '$Hmjo' '' -> {conf_get_op 'Hmjo'}
	| '$Hmll' '' -> {conf_get_op 'Hmll'}
	| '$Hmm' '' -> {conf_get_op 'Hmm'}
	| '$Hmmm' '' -> {conf_get_op 'Hmmm'}
	| '$Hmoc' '' -> {conf_get_op 'Hmoc'}
	| '$Hmokfs' '' -> {conf_get_op 'Hmokfs'}
	| '$Hmovc' '' -> {conf_get_op 'Hmovc'}
	| '$Hmr' '' -> {conf_get_op 'Hmr'}
	| '$Hmrg' '' -> {conf_get_op 'Hmrg'}
	| '$Hms' '' -> {conf_get_op 'Hms'}
	| '$Hmsm' '' -> {conf_get_op 'Hmsm'}
	| '$Hmspcai' '' -> {conf_get_op 'Hmspcai'}
	| '$Hmssp' '' -> {conf_get_op 'Hmssp'}
	| '$Hnfields' '' -> {conf_get_op 'Hnfields'}
	| '$Hntcnm' '' -> {conf_get_op 'Hntcnm'}
	| '$Hntnsmi' '' -> {conf_get_op 'Hntnsmi'}
	| '$Hntsfn' '' -> {conf_get_op 'Hntsfn'}
	| '$Hntsna' '' -> {conf_get_op 'Hntsna'}
	| '$Hofc' '' -> {conf_get_op 'Hofc'}
	| '$Hofcc' '' -> {conf_get_op 'Hofcc'}
	| '$Hofct' '' -> {conf_get_op 'Hofct'}
	| '$Hofo' '' -> {conf_get_op 'Hofo'}
	| '$Holo' '' -> {conf_get_op 'Holo'}
	| '$Hoskc' '' -> {conf_get_op 'Hoskc'}
	| '$Hosvc' '' -> {conf_get_op 'Hosvc'}
	| '$Hots' '' -> {conf_get_op 'Hots'}
	| '$Hpblo' '' -> {conf_get_op 'Hpblo'}
	| '$Hpbro' '' -> {conf_get_op 'Hpbro'}
	| '$Hpcp' '' -> {conf_get_op 'Hpcp'}
	| '$Hpe' '' -> {conf_get_op 'Hpe'}
	| '$Hpei' '' -> {conf_get_op 'Hpei'}
	| '$Hpif' '' -> {conf_get_op 'Hpif'}
	| '$Hpijm' '' -> {conf_get_op 'Hpijm'}
	| '$Hpijr' '' -> {conf_get_op 'Hpijr'}
	| '$Hpijrr' '' -> {conf_get_op 'Hpijrr'}
	| '$Hpijrw' '' -> {conf_get_op 'Hpijrw'}
	| '$Hpkco' '' -> {conf_get_op 'Hpkco'}
	| '$Hpkpo' '' -> {conf_get_op 'Hpkpo'}
	| '$Hpp' '' -> {conf_get_op 'Hpp'}
	| '$Hrds' '' -> {conf_get_op 'Hrds'}
	| '$Hre' '' -> {conf_get_op 'Hre'}
	| '$Hrfm' '' -> {conf_get_op 'Hrfm'}
	| '$Hribp' '' -> {conf_get_op 'Hribp'}
	| '$Hrjo' '' -> {conf_get_op 'Hrjo'}
	| '$Hrll' '' -> {conf_get_op 'Hrll'}
	| '$Hrm' '' -> {conf_get_op 'Hrm'}
	| '$Hrmbp' '' -> {conf_get_op 'Hrmbp'}
	| '$Hrmit' '' -> {conf_get_op 'Hrmit'}
	| '$Hrmm' '' -> {conf_get_op 'Hrmm'}
	| '$Hrmt' '' -> {conf_get_op 'Hrmt'}
	| '$Hrs' '' -> {conf_get_op 'Hrs'}
	| '$Hrsct' '' -> {conf_get_op 'Hrsct'}
	| '$Hrsibp' '' -> {conf_get_op 'Hrsibp'}
	| '$Hrsm' '' -> {conf_get_op 'Hrsm'}
	| '$Hrsmp' '' -> {conf_get_op 'Hrsmp'}
	| '$Hrsp' '' -> {conf_get_op 'Hrsp'}
	| '$Hrspcai' '' -> {conf_get_op 'Hrspcai'}
	| '$Hrsrt' '' -> {conf_get_op 'Hrsrt'}
	| '$Hs3ak' '' -> {conf_get_op 'Hs3ak'}
	| '$Hs3as' '' -> {conf_get_op 'Hs3as'}
	| '$Hs3nk' '' -> {conf_get_op 'Hs3nk'}
	| '$Hs3ns' '' -> {conf_get_op 'Hs3ns'}
	| '$Hsjcpa' '' -> {conf_get_op 'Hsjcpa'}
	| '$Hsjtpa' '' -> {conf_get_op 'Hsjtpa'}
	| '$Htai' '' -> {conf_get_op 'Htai'}
	| '$Htdl' '' -> {conf_get_op 'Htdl'}
	| '$Htfpfp' '' -> {conf_get_op 'Htfpfp'}
	| '$Htfpft' '' -> {conf_get_op 'Htfpft'}
	| '$Htid' '' -> {conf_get_op 'Htid'}
	| '$Htim' '' -> {conf_get_op 'Htim'}
	| '$Htisf' '' -> {conf_get_op 'Htisf'}
	| '$Htism' '' -> {conf_get_op 'Htism'}
	| '$Htmpr' '' -> {conf_get_op 'Htmpr'}
	| '$Htod' '' -> {conf_get_op 'Htod'}
	| '$Htpart' '' -> {conf_get_op 'Htpart'}
	| '$Htpm' '' -> {conf_get_op 'Htpm'}
	| '$Htpp' '' -> {conf_get_op 'Htpp'}
	| '$Htpr' '' -> {conf_get_op 'Htpr'}
	| '$Htprof' '' -> {conf_get_op 'Htprof'}
	| '$Htssa' '' -> {conf_get_op 'Htssa'}
	| '$Htt' '' -> {conf_get_op 'Htt'}
	| '$Httcls' '' -> {conf_get_op 'Httcls'}
	| '$Httct' '' -> {conf_get_op 'Httct'}
	| '$Httd' '' -> {conf_get_op 'Httd'}
	| '$Httdi' '' -> {conf_get_op 'Httdi'}
	| '$Httdn' '' -> {conf_get_op 'Httdn'}
	| '$Htteb' '' -> {conf_get_op 'Htteb'}
	| '$Httha' '' -> {conf_get_op 'Httha'}
	| '$Htthi' '' -> {conf_get_op 'Htthi'}
	| '$Htthn' '' -> {conf_get_op 'Htthn'}
	| '$Htthsa' '' -> {conf_get_op 'Htthsa'}
	| '$Htthsp' '' -> {conf_get_op 'Htthsp'}
	| '$Htthst' '' -> {conf_get_op 'Htthst'}
	| '$Httht' '' -> {conf_get_op 'Httht'}
	| '$Htti' '' -> {conf_get_op 'Htti'}
	| '$Httim' '' -> {conf_get_op 'Httim'}
	| '$Httldmsk' '' -> {conf_get_op 'Httldmsk'}
	| '$Httldmss' '' -> {conf_get_op 'Httldmss'}
	| '$Httmtm' '' -> {conf_get_op 'Httmtm'}
	| '$Httnsr' '' -> {conf_get_op 'Httnsr'}
	| '$Httr' '' -> {conf_get_op 'Httr'}
	| '$Httra' '' -> {conf_get_op 'Httra'}
	| '$Httrtm' '' -> {conf_get_op 'Httrtm'}
	| '$Httt' '' -> {conf_get_op 'Httt'}
	| '$Htttm' '' -> {conf_get_op 'Htttm'}
	| '$Httts' '' -> {conf_get_op 'Httts'}
	| '$Htulk' '' -> {conf_get_op 'Htulk'}
	| '$cc' '' -> {conf_get_op 'cc'}
	| '$cc_opts' '' -> {conf_get_op 'cc_opts'}
	| '$col/disallow-cut' '' -> {conf_get_op 'col/disallow-cut'}
	| '$ffmpeg' '' -> {conf_get_op 'ffmpeg'}
	| '$hadoop/jobconf' '' -> {conf_get_op 'hadoop/jobconf'}
	| '$hadoop/name' '' -> {conf_get_op 'hadoop/name'}
	| '$hadoop/streaming-jar' '' -> {conf_get_op 'hadoop/streaming-jar'}
	| '$hdfs/tmpdir' '' -> {conf_get_op 'hdfs/tmpdir'}
	| '$image_command' '' -> {conf_get_op 'image_command'}
	| '$mapbox/key' '' -> {conf_get_op 'mapbox/key'}
	| '$mapbox/tileset' '' -> {conf_get_op 'mapbox/tileset'}
	| '$monitor' '' -> {conf_get_op 'monitor'}
	| '$monitor/interval' '' -> {conf_get_op 'monitor/interval'}
	| '$monitor/start' '' -> {conf_get_op 'monitor/start'}
	| '$pager' '' -> {conf_get_op 'pager'}
	| '$row/seed' '' -> {conf_get_op 'row/seed'}
	| '$row/sort-buffer' '' -> {conf_get_op 'row/sort-buffer'}
	| '$row/sort-compress' '' -> {conf_get_op 'row/sort-compress'}
	| '$row/sort-parallel' '' -> {conf_get_op 'row/sort-parallel'}
	| '$scale/ibuf' '' -> {conf_get_op 'scale/ibuf'}
	| '$scale/obuf' '' -> {conf_get_op 'scale/obuf'}
	| '$sqlite' '' -> {conf_get_op 'sqlite'}
	| '$tmpdir' '' -> {conf_get_op 'tmpdir'}
	| '$xargs/arg' '' -> {conf_get_op 'xargs/arg'}
	| '$ytdl' '' -> {conf_get_op 'ytdl'}
	| '%' (
	    <number>?
	    </qfn>
	  ) -> {interleave_op @$_}
	| ''' (
	    (
	      /(?^:\[)/
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      /[^]].*/
	      <empty>?
	    ) -> {$$_[0]}*
	    (
	      /(?^:\])/
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {$$_[1]} -> {resource_quote_many_op @$_}
	| '+' </qfn> -> {append_op    @$_}
	| ',' (
	  | <cell/lambda>
	  | <cell/suffix>
	  )
	| '--dev/backdoor' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {dev_backdoor_op $_}
	| '--dev/local-operate' </qfn> -> {dev_local_operate_op $_}
	| '--http/wse' '' -> {http_websocket_encode_op}
	| '--http/wse-batch' <integer>? -> {http_websocket_encode_batch_op $_}
	| '--internal/lambda' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {run_quoted_lambda_op $_}
	| '--license' '' -> {meta_key_op 'license'}
	| '//:' <closure_name> -> {memory_closure_append_op $_}
	| '//@' <closure_name> -> {file_closure_append_op $_}
	| '//help' //(.*)/? -> {meta_help_op $_}
	| '//license' '' -> {meta_key_op 'license'}
	| '//ni' '' -> {meta_image_op}
	| '//ni/' (
	    /[^][]+$/
	    <empty>?
	  ) -> {$$_[0]} -> {meta_key_op $_}
	| '//ni/conf' '' -> {meta_conf_op}
	| '//ni/eval/' <integer> -> {meta_eval_number_op $_}
	| '//ni/keys' '' -> {meta_keys_op}
	| '//ni/map/short' '' -> {meta_short_availability_op}
	| '//ni/op/' (
	    /.+/
	    <empty>?
	  ) -> {$$_[0]} -> {meta_op_op $_}
	| '//ni/ops' '' -> {meta_ops_op}
	| '//ni/options' '' -> {meta_options_op}
	| '//ni/parser/' (
	    /.+/
	    <empty>?
	  ) -> {$$_[0]} -> {meta_parser_op $_}
	| '//ni/parsers' '' -> {meta_parsers_op}
	| '//ni/perl_prefix' '' -> {perl_prefix_op}
	| '//你' '' -> {meta_image_op}
	| '1' '' -> {n_op 1, 2}
	| ':' (
	    (
	      <nefilename>
	      <empty>?
	    ) -> {$$_[0]}
	    <nefilelist>?
	  )? -> {$_ ? inline_checkpoint_op @$_
	                           : identity_op}
	| '::' (
	    (
	      <closure_name>
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {memory_data_closure_op @$_}
	| ':@' (
	    (
	      <closure_name>
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {file_data_closure_op @$_}
	| '<' '' -> {file_read_op}
	| '=' </qfn> -> {divert_op    @$_}
	| '>' <nefilename>? -> {file_write_op $_}
	| '>'R' '' -> {encode_resource_stream_op}
	| 'AE' <media_format_spec> -> {audio_extract_op @$_}
	| 'B' (
	  | 'n' '' -> {buffer_null_op}
	  )
	| 'C' (
	  | (
	      (
	        (
	          /A/
	          <empty>?
	        ) -> {$$_[0]}
	        (
	          /[^][]+/ -> {[/\+([^][+]+)/g]}
	          <empty>?
	        ) -> {$$_[0]}
	      ) -> {$$_[1]}
	      </qfn>
	    ) -> {docker_run_dynamic_op alpine_dockerfile(@{$$_[0]}), @{$$_[1]}}
	  | (
	      (
	        (
	          /U/
	          <empty>?
	        ) -> {$$_[0]}
	        (
	          /[^][]+/ -> {[/\+([^][+]+)/g]}
	          <empty>?
	        ) -> {$$_[0]}
	      ) -> {$$_[1]}
	      </qfn>
	    ) -> {docker_run_dynamic_op ubuntu_dockerfile(@{$$_[0]}), @{$$_[1]}}
	  | (
	      (
	        /[^][]+/
	        <empty>?
	      ) -> {$$_[0]}
	      </qfn>
	    ) -> {docker_run_image_op $$_[0], @{$$_[1]}}
	  )
	| 'D' <generic_code> -> {destructure_op $_}
	| 'E' (
	    (
	      /[^][]+/
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {docker_exec_op $$_[0], @{$$_[1]}}
	| 'F' (
	  | '/' <regex> -> {split_regex_op $_}
	  | ':' /./ -> {split_chr_op   $_}
	  | 'C' '' -> {split_chr_op   ','}
	  | 'D' '' -> {split_chr_op   '\/'}
	  | 'P' '' -> {split_chr_op   '|'}
	  | 'S' '' -> {split_regex_op '\s+'}
	  | 'V' '' -> {split_proper_csv_op}
	  | 'W' '' -> {split_regex_op '[^\w\n]+'}
	  | 'm' (
	      '/'
	      <regex> -> {scan_regex_op $_}
	    ) -> {$$_[1]}
	  )
	| 'F^' (
	  | ':' /./ -> {sh_op 'tr "\t" "'.$_.'"'}
	  | 'C' '' -> {sh_op 'tr "\t" ,'}
	  | 'P' '' -> {sh_op 'tr "\t" "|"'}
	  | 'S' '' -> {sh_op 'tr "\t" " "'}
	  )
	| 'G' (
	    <gnuplot_colspec>
	    <gnuplot_code>
	  ) -> {stream_to_gnuplot_op @$_}
	| 'G*' <gnuplot_code> -> {gnuplot_all_op $_}
	| 'GF' <shell_command>? -> {sh_op "ffmpeg -f image2pipe -i - $_"}
	| 'GF^' <shell_command>? -> {sh_op "ffmpeg -i - $_ -f image2pipe -c:v png -"}
	| 'H' (
	  | '#' '' -> {hadoop_make_nukeable_op}
	  | 'DS' (
	      <empty>?
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {my (undef, $m, $c, $r) = @$_;
	                              my @cr =
	                                (defined $c ? (row_sort_op(sort_args [0]), @$c) : (),
	                                 defined $r ? (row_sort_op(sort_args [0]), @$r) : ());
	                              [@$m, @cr]}
	  | 'R' (
	      <number>
	      <empty>?
	    ) -> {$$_[0]} -> {configure_op {'Hjr' => "$_"},
	                          [hadoop_streaming_op [], undef, []]}
	  | 'RR' (
	      <number>
	      <empty>?
	    ) -> {$$_[0]} -> {configure_op {'Hjr' => "$_"},
	                          [hadoop_streaming_op
	                            [perl_mapper_op 'print "$.\t$_\n";()'],
	                            undef,
	                            [cols_op 2, 1, -1]]}
	  | 'S' (
	      <empty>?
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {hadoop_streaming_op @$_[1..$#$_]}
	  | 'T' (
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {hadoop_test_op @$_}
	  )
	| 'H>' <nefilename> -> {hadoop_outpath_set_op $_}
	| 'I' </qfn> -> {each_image_op $_}
	| 'IC' (
	    (
	      <image_command>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <image_command>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <image_command>
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {composite_images_op @$_}
	| 'IJ' '' -> {each_image_op [sh_op "convert - jpg:-"]}
	| 'IV' <media_format_spec> -> {imagepipe_to_video_op @$_}
	| 'J' (
	    <colspec1>?
	    <integer>?
	    </qfn>
	  ) -> {memory_join_op $$_[0] || 0, $$_[1] || 1, $$_[2]}
	| 'MM' '' -> {[geojsonify_op, mapomatic_op]}
	| 'N' (
	    <colspec1>?
	    <pycode>
	  ) -> {numpy_dense_op @$_}
	| 'O' <sortspec> -> {row_sort_op '-rn', sort_args @$_}
	| 'P' (
	  | 'L' <pyspark_rdd> -> {[pyspark_local_text_op($_),
	                                 file_read_op,
	                                 row_match_op '/part-']}
	  | 'dev/compile' <pyspark_rdd> -> {pyspark_preview_op $_}
	  )
	| 'Q' (
	  | 'dev/compile' <sql_query> -> {sql_preview_op($_[0])}
	  )
	| 'S' (
	  | (
	      'X'
	      (
	        (
	          <integer>
	          <empty>?
	        ) -> {$$_[0]}
	        (
	          <shell_arg>
	          <empty>?
	        ) -> {$$_[0]}
	        (
	          <shell_arg>
	          <empty>?
	        ) -> {$$_[0]}
	        </qfn>
	      )
	    ) -> {$$_[1]} -> {row_xargs_scale_op @$_}
	  | (
	      <integer>
	      </qfn>
	    ) -> {row_fixed_scale_op @$_}
	  )
	| 'S>' </qfn>? -> {sharded_write_op $_}
	| 'U' '' -> {unordered_count_op}
	| 'VI' /\w+/? -> {video_to_imagepipe_op $_}
	| 'W' </qfn> -> {with_left_op  @$_}
	| 'W<' (
	    <colspec1>?
	    <generic_code>?
	  ) -> {file_prepend_name_read_op @$_}
	| 'W>' </qfn>? -> {file_prepend_name_write_op $_}
	| 'Wn<' (
	    <colspec1>?
	    <generic_code>?
	  ) -> {file_prepend_name_number_read_op @$_}
	| 'X' <colspec1>? -> {sparse_to_dense_op $_}
	| 'XP' '' -> {pivot_table_op}
	| 'Y' <colspec1>? -> {dense_to_sparse_op $_}
	| 'Z' (
	  | <integer> -> {unflatten_op 0 + $_}
	  | <colspec1> -> {partial_transpose_op $_}
	  )
	| '^' </qfn> -> {prepend_op   @$_}
	| '^{' (
	    <config_option_map>
	    </qfn>
	  ) -> {configure_op @$_}
	| '_' <integer>? -> {flatten_tabs_op $_}
	| 'b' (
	  | 'f' <generic_code> -> {binary_fixed_op $_}
	  | 'p' <plcode ni::binary_perl_mapper> -> {binary_perl_op $_}
	  )
	| 'c' '' -> {count_op}
	| 'cleandos' '' -> {cleandos_op}
	| 'e' <shell_command> -> {sh_op $_}
	| 'f' (
	  | <colspec> -> {cols_op @$_}
	  )
	| 'f[' (
	    <empty>?
	    <fn_bindings>
	    </series>
	    ']'
	  ) -> {[@$_[1,2]]} -> {op_fn_op @$_}
	| 'fx' (
	    <empty>?
	    (
	      <integer>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      '['
	      <empty>?
	    ) -> {$$_[0]}
	    <fn_bindings>
	    </series>
	    (
	      ']'
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {[@$_[1,3,4]]} -> {xargs_fn_op @$_}
	| 'g' (
	  | (
	      /_/
	      <integer>
	    ) -> {$$_[1]} -> {partial_sort_op               $_}
	  | <sortspec> -> {row_sort_op        sort_args @$_}
	  )
	| 'geojsonify' '' -> {geojsonify_op}
	| 'gg' (
	    <colspec1>
	    <sortspec>
	  ) -> {row_grouped_sort_op @$_}
	| 'git<' (
	    <filename>
	    <empty>?
	  ) -> {$$_[0]}? -> {git_cat_objects_op "--batch", $_}
	| 'gitm<' (
	    <filename>
	    <empty>?
	  ) -> {$$_[0]}? -> {git_cat_objects_op "--batch-check", $_}
	| 'i' <id_text> -> {echo_op $_}
	| 'j' (
	    <colspec>?
	    </qfn>
	  ) -> {join_op $$_[0] || [1, 0], $$_[0] || [1, 0], $$_[1]}
	| 'l' <lispcode> -> {lisp_code_op lisp_mapgen->(prefix => lisp_prefix,
	                                                   body   => $_)}
	| 'l[' (
	    <empty>?
	    <let_bindings>
	    </series>
	    ']'
	  ) -> {[@$_[1,2]]} -> {op_let_op @$_}
	| 'm' (
	  | <rbcode> -> {ruby_mapper_op $_}
	  )
	| 'mdtable' '' -> {mdtable_op}
	| 'n' <number>? -> {n_op 1, defined $_ ? $_ + 1 : -1}
	| 'n0' <number>? -> {n_op 0, defined $_ ? $_ : -1}
	| 'o' <sortspec> -> {row_sort_op '-n',  sort_args @$_}
	| 'p' (
	  | <perl_mapper_code> -> {perl_mapper_op $_}
	  )
	| 'pR' </qfn> -> {perl_require_op @$_}
	| 'r' (
	  | (
	      'l'
	      <lispcode>
	    ) -> {$$_[1]} -> {lisp_code_op lisp_grepgen->(prefix => lisp_prefix,
	                                                 body   => $_)}
	  | (
	      'm'
	      <rbcode>
	    ) -> {$$_[1]} -> {ruby_grepper_op $_}
	  | (
	      '^b'
	      (
	        <colspec1>
	        <empty>?
	      ) -> {$$_[0]}
	      </qfn>
	    ) -> {[@$_[1,2]]} -> {bloom_rows_op 0, @$_}
	  | (
	      'b'
	      (
	        <colspec1>
	        <empty>?
	      ) -> {$$_[0]}
	      </qfn>
	    ) -> {[@$_[1,2]]} -> {bloom_rows_op 1, @$_}
	  | (
	      'p'
	      <perl_grepper_code>
	    ) -> {$$_[1]} -> {perl_grepper_op $_}
	  | (
	      /[+~]/
	      <integer>
	    ) -> {$$_[1]} -> {tail_op '-n', '',  $_}
	  | (
	      /-/
	      <integer>
	    ) -> {$$_[1]} -> {tail_op '-n', '+', ($_ + 1)}
	  | (
	      /s/
	      <number>
	    ) -> {$$_[1]} -> {safe_head_op  $_}
	  | (
	      /x/
	      <number>
	    ) -> {$$_[1]} -> {row_every_op  $_}
	  | (
	      /x/
	      <colspec1>
	    ) -> {$$_[1]} -> {row_repeat_op $_}
	  | (
	      ///
	      <regex>
	    ) -> {$$_[1]} -> {row_match_op  $_}
	  | /\.\d+/ -> {row_sample_op $_}
	  | <integer> -> {head_op '-n', 0 + $_}
	  | (
	      'i'
	      <colspec1>
	      </qfn>
	    ) -> {[@$_[1,2]]} -> {row_include_or_exclude_exact_op 1, @$_}
	  | (
	      'I'
	      <colspec1>
	      </qfn>
	    ) -> {[@$_[1,2]]} -> {row_include_or_exclude_exact_op 0, @$_}
	  | <colspec_fixed> -> {row_cols_defined_op @$_}
	  )
	| 's' (
	    <ssh_host_full>
	    </qfn>
	  ) -> {ssh_op @$_}
	| 'sF' (
	    <ssh_host_full>
	    <integer>
	  ) -> {port_forward_op @$_}
	| 'u' '' -> {uniq_op}
	| 'v' (
	    <colspec_fixed>
	    </qfn>
	  ) -> {vertical_apply_op @$_}
	| 'w' </qfn> -> {with_right_op @$_}
	| 'wcl' '' -> {wc_l_op}
	| 'x' <colspec>? -> {ref $_ ? colswap_op @$_ : colswap_op 2, 1}
	| 'z' <compressor_spec>
	| 'zB' (
	    <bloom_size_spec>
	    <bloom_fp_spec>
	  ) -> {bloomify_op @$_}
	| 'zBH' (
	    <bloom_size_spec>
	    <bloom_fp_spec>
	  ) -> {bloomify_hex_op @$_}
	| 'zBP' (
	    <bloom_size_spec>
	    <bloom_fp_spec>
	  ) -> {bloomify_prehashed_op @$_}
	| 'zd' <'', evaluate as [decode]>
	| 'zn' <'', evaluate as [sink_null]>
	)

# PARSER /suffix
	A string of operators unbroken by whitespace

## DEFINITION
	</op>*

# PARSER alt/colalt

## DEFINITION
	(
	| <colspec> -> {cols_op @$_}
	)

# PARSER alt/dockeralt

## DEFINITION
	(
	| (
	    (
	      (
	        /A/
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        /[^][]+/ -> {[/\+([^][+]+)/g]}
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {$$_[1]}
	    </qfn>
	  ) -> {docker_run_dynamic_op alpine_dockerfile(@{$$_[0]}), @{$$_[1]}}
	| (
	    (
	      (
	        /U/
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        /[^][]+/ -> {[/\+([^][+]+)/g]}
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {$$_[1]}
	    </qfn>
	  ) -> {docker_run_dynamic_op ubuntu_dockerfile(@{$$_[0]}), @{$$_[1]}}
	| (
	    (
	      /[^][]+/
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {docker_run_image_op $$_[0], @{$$_[1]}}
	)

# PARSER alt/perlalt

## DEFINITION
	(
	| <perl_mapper_code> -> {perl_mapper_op $_}
	)

# PARSER alt/pysparkrowalt

## DEFINITION
	(
	| <integer> -> {gen "%v.sample(False, $_)"}
	| /\.(\d+)/ -> {gen "%v.takeSample(False, $_)"}
	| <pyspark_fn> -> {gen "%v.filter($_)"}
	)

# PARSER alt/rowalt

## DEFINITION
	(
	| (
	    'l'
	    <lispcode>
	  ) -> {$$_[1]} -> {lisp_code_op lisp_grepgen->(prefix => lisp_prefix,
	                                               body   => $_)}
	| (
	    'm'
	    <rbcode>
	  ) -> {$$_[1]} -> {ruby_grepper_op $_}
	| (
	    '^b'
	    (
	      <colspec1>
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {bloom_rows_op 0, @$_}
	| (
	    'b'
	    (
	      <colspec1>
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {bloom_rows_op 1, @$_}
	| (
	    'p'
	    <perl_grepper_code>
	  ) -> {$$_[1]} -> {perl_grepper_op $_}
	| (
	    /[+~]/
	    <integer>
	  ) -> {$$_[1]} -> {tail_op '-n', '',  $_}
	| (
	    /-/
	    <integer>
	  ) -> {$$_[1]} -> {tail_op '-n', '+', ($_ + 1)}
	| (
	    /s/
	    <number>
	  ) -> {$$_[1]} -> {safe_head_op  $_}
	| (
	    /x/
	    <number>
	  ) -> {$$_[1]} -> {row_every_op  $_}
	| (
	    /x/
	    <colspec1>
	  ) -> {$$_[1]} -> {row_repeat_op $_}
	| (
	    ///
	    <regex>
	  ) -> {$$_[1]} -> {row_match_op  $_}
	| /\.\d+/ -> {row_sample_op $_}
	| <integer> -> {head_op '-n', 0 + $_}
	| (
	    'i'
	    <colspec1>
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {row_include_or_exclude_exact_op 1, @$_}
	| (
	    'I'
	    <colspec1>
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {row_include_or_exclude_exact_op 0, @$_}
	| <colspec_fixed> -> {row_cols_defined_op @$_}
	)

# PARSER alt/rubyalt

## DEFINITION
	(
	| <rbcode> -> {ruby_mapper_op $_}
	)

# PARSER alt/scalealt

## DEFINITION
	(
	| (
	    'X'
	    (
	      (
	        <integer>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <shell_arg>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <shell_arg>
	        <empty>?
	      ) -> {$$_[0]}
	      </qfn>
	    )
	  ) -> {$$_[1]} -> {row_xargs_scale_op @$_}
	| (
	    <integer>
	    </qfn>
	  ) -> {row_fixed_scale_op @$_}
	)

# PARSER alt/sortalt

## DEFINITION
	(
	| (
	    /_/
	    <integer>
	  ) -> {$$_[1]} -> {partial_sort_op               $_}
	| <sortspec> -> {row_sort_op        sort_args @$_}
	)

# PARSER alt/sqljoinalt

## DEFINITION
	(
	| (
	    'L'
	    <sql_query>
	  ) -> {$$_[1]} -> {['ljoin', $_]}
	| (
	    'R'
	    <sql_query>
	  ) -> {$$_[1]} -> {['rjoin', $_]}
	| (
	    'N'
	    <sql_query>
	  ) -> {$$_[1]} -> {['njoin', $_]}
	| <sql_query> -> {['ijoin', $_]}
	)

# PARSER alt/sqlrowalt

## DEFINITION
	(
	| <integer> -> {['take',   $_]}
	| <sqlcode> -> {['filter', $_]}
	)

# PARSER attenuate_spec

## DEFINITION
	<number>? -> {$_ || 4}

# PARSER bloom_fp_spec

## DEFINITION
	/(?^:\d(?:\.\d)?)/ -> {10 ** -$_}

# PARSER bloom_size_spec

## DEFINITION
	/(?^:\d(?:\.\d)?)/ -> {10 **  $_}

# PARSER cell/lambda
	A bracketed lambda function in context 'cell'

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  <cell/series>
	  ']'
	) -> {$$_[1]}

# PARSER cell/op
	A single operator in the context 'cell'

## DEFINITION
	(
	| <cell/short>
	)

# PARSER cell/qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| <cell/lambda>
	| <cell/suffix>
	)

# PARSER cell/series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  <cell/op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER cell/short
	Dispatch table for short options in context 'cell'

## DEFINITION
	(
	| 'A' (
	    <cellspec_fixed>
	    <attenuate_spec>
	  ) -> {attenuate_op @$_}
	| 'BP' (
	    <cellspec_fixed>
	    <bloom_size_spec>
	    <bloom_fp_spec>
	  ) -> {bloom_prehash_op @$_}
	| 'C' (
	  | 'd' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/[^-0-9]/}
	  | 'f' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/[^-+eE.0-9]/}
	  | 'w' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/\W/}
	  | 'x' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/[^-0-9a-fA-F]/}
	  )
	| 'G' (
	    <cellspec_fixed>
	    <integer>?
	  ) -> {geohash_decode_op @$_}
	| 'H' (
	    <cellspec_fixed>
	    <integer>?
	  ) -> {real_hash_op      @$_}
	| 'J' (
	    <cellspec_fixed>
	    <jitter_mag>
	  ) -> {jitter_gaussian_op @$_}
	| 'L' (
	    <cellspec_fixed>
	    <log_base>
	  ) -> {cell_signed_log_op @$_}
	| 'Q' (
	    <cellspec_fixed>
	    <quant_spec>
	  ) -> { my ($cellspec, $quantum) = @$_;
	            [quantize_op($cellspec, $quantum),
	             jitter_uniform_op($cellspec, $quantum * 0.9)] }
	| 'a' <cellspec_fixed> -> {col_average_op $_}
	| 'ag' <colspec1> -> {
	      my $col  = $_;
	      my $fs   = "0..$col";
	      my $se   = "se" . ("A".."Z")[$col];
	      my $next = ("a".."z")[$col + 1];
	      perl_mapper_op "
	        my \@fs = F_($fs);
	        my (\$t, \$n) = $se { (\$_[0] + $next, \$_[1] + 1) } 0, 0;
	        r \@fs, \$t / (\$n || 1)";
	    }
	| 'd' <cellspec_fixed> -> {col_delta_op   $_}
	| 'e' (
	    <cellspec_fixed>
	    <log_base>
	  ) -> {cell_exp_op @$_}
	| 'g' (
	    <cellspec_fixed>
	    (
	    | <integer>
	    | <'', evaluate as 12>
	    )
	  ) -> {geohash_encode_op @$_}
	| 'h' (
	    <cellspec_fixed>
	    <integer>?
	  ) -> {intify_hash_op    @$_}
	| 'j' (
	    <cellspec_fixed>
	    <jitter_mag>
	    <jitter_bias>
	  ) -> {jitter_uniform_op @$_}
	| 'l' (
	    <cellspec_fixed>
	    <log_base>
	  ) -> {cell_log_op @$_}
	| 'm' <cellspec_fixed> -> {md5_op $_}
	| 'q' (
	    <cellspec_fixed>
	    <quant_spec>
	  ) -> {quantize_op @$_}
	| 's' <cellspec_fixed> -> {col_sum_op     $_}
	| 'sg' <colspec1> -> {
	      my $col  = $_;
	      my $fs   = "0..$col";
	      my $se   = "se" . ("A".."Z")[$col];
	      my $next = ("a".."z")[$col + 1];
	      perl_mapper_op "r F_($fs), $se { \$_[0] + $next } 0";
	    }
	| 't' <cellspec_fixed> -> {epoch_to_formatted_op $_}
	| 'z' <cellspec_fixed> -> {intify_compact_op $_}
	)

# PARSER cell/suffix
	A string of operators unbroken by whitespace

## DEFINITION
	<cell/op>*

# PARSER cellspec

## DEFINITION
	<colspec>? -> {$_ || [1, 0]}

# PARSER cellspec_fixed

## DEFINITION
	<colspec_fixed>? -> {$_ || [1, 0]}

# PARSER closure_name

## DEFINITION
	/[^][]+/

# PARSER colspec
	A set of columns, possibly including '.' ("the rest")

## DEFINITION
	(
	  ','?
	  (
	  | <colspec_range>
	  | <colspec1>
	  | <colspec_rest>
	  )
	) -> {$$_[1]}+ -> {[map ref() ? @$_ : $_, @$_]} -> {[max(@$_) + 1, @$_]}

# PARSER colspec1
	A way to identify a single column; either A-Z or #N

## DEFINITION
	(
	| (
	    '#'
	    <integer>
	  ) -> {$$_[1]}
	| /[A-Z]/ -> {ord() - 65}
	)

# PARSER colspec_fixed
	A set of definite columns; disallows '.' ("the rest")

## DEFINITION
	(
	  ','?
	  (
	  | <colspec_range>
	  | <colspec1>
	  )
	) -> {$$_[1]}+ -> {[map ref() ? @$_ : $_, @$_]} -> {[max(@$_) + 1, @$_]}

# PARSER colspec_range
	A range of columns, e.g. A-Q or #10-#20

## DEFINITION
	(
	  <colspec1>
	  '-'
	  <colspec1>
	) -> {[$$_[0] .. $$_[2]]}

# PARSER colspec_rest
	"The rest of the columns": everything to the right of the rightmost
	explicitly-specified column

## DEFINITION
	'.' -> {-1}

# PARSER compressor_name

## DEFINITION
	/[gzxo4b]/

# PARSER compressor_spec

## DEFINITION
	(
	  <compressor_name>?
	  <integer>?
	) -> {my ($c, $level) = @$_;
	           $c = $ni::compressors{$c || 'g'};
	           defined $level ? sh_op "$c -$level" : sh_op $c}

# PARSER computed

## DEFINITION
	(
	  '$'
	  /.*/
	) -> {$$_[1]} -> {eval "(sub {$_})->()"}

# PARSER config_map_key

## DEFINITION
	/[^=]+/

# PARSER config_map_kv

## DEFINITION
	(
	  <config_map_key>
	  '='
	  <config_map_value>
	) -> {[@$_[0,2]]}

# PARSER config_map_value

## DEFINITION
	(
	  /.*[^}]+|/
	  <empty>?
	) -> {$$_[0]}

# PARSER config_option_map

## DEFINITION
	(
	  <config_map_kv>*
	  (
	    /}/
	    <empty>?
	  ) -> {$$_[0]}
	) -> {$$_[0]} -> {my %h; $h{$$_[0]} = $$_[1] for @{$_[0]}; \%h}

# PARSER dsp/assertdsp

## DEFINITION
	(
	| 'p' <perl_asserter_code> -> {perl_assert_op $_}
	)

# PARSER dsp/binaryalt

## DEFINITION
	(
	| 'f' <generic_code> -> {binary_fixed_op $_}
	| 'p' <plcode ni::binary_perl_mapper> -> {binary_perl_op $_}
	)

# PARSER dsp/bufferalt

## DEFINITION
	(
	| 'n' '' -> {buffer_null_op}
	)

# PARSER dsp/cleanalt

## DEFINITION
	(
	| 'd' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/[^-0-9]/}
	| 'f' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/[^-+eE.0-9]/}
	| 'w' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/\W/}
	| 'x' <cellspec_fixed> -> {cell_clean_regex_op $_, qr/[^-0-9a-fA-F]/}
	)

# PARSER dsp/combinealt

## DEFINITION
	(
	| ':' /./ -> {sh_op 'tr "\t" "'.$_.'"'}
	| 'C' '' -> {sh_op 'tr "\t" ,'}
	| 'P' '' -> {sh_op 'tr "\t" "|"'}
	| 'S' '' -> {sh_op 'tr "\t" " "'}
	)

# PARSER dsp/gnuplot_code_prefixalt

## DEFINITION
	(
	| '%d' <'', evaluate as plot "-" with dots >
	| '%i' <'', evaluate as plot "-" with impulses >
	| '%l' <'', evaluate as plot "-" with lines >
	| '%t' <generic_code> -> {"title '$_'"}
	| '%u' <generic_code> -> {"using $_"}
	| '%v' <'', evaluate as plot "-" with vectors >
	| 'J' <gnuplot_terminal_size> -> {"set terminal jpeg $_;"}
	| 'P' <gnuplot_terminal_size> -> {"set terminal png $_;"}
	| 'PC' <gnuplot_terminal_size> -> {"set terminal pngcairo $_;"}
	| 'Q' 'P'? -> {"set terminal qt persist;"}
	| 'W' 'P'? -> {"set terminal wx persist;"}
	| 'X' 'P'? -> {"set terminal x11 persist;"}
	)

# PARSER dsp/hadoopalt

## DEFINITION
	(
	| '#' '' -> {hadoop_make_nukeable_op}
	| 'DS' (
	    <empty>?
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {my (undef, $m, $c, $r) = @$_;
	                            my @cr =
	                              (defined $c ? (row_sort_op(sort_args [0]), @$c) : (),
	                               defined $r ? (row_sort_op(sort_args [0]), @$r) : ());
	                            [@$m, @cr]}
	| 'R' (
	    <number>
	    <empty>?
	  ) -> {$$_[0]} -> {configure_op {'Hjr' => "$_"},
	                        [hadoop_streaming_op [], undef, []]}
	| 'RR' (
	    <number>
	    <empty>?
	  ) -> {$$_[0]} -> {configure_op {'Hjr' => "$_"},
	                        [hadoop_streaming_op
	                          [perl_mapper_op 'print "$.\t$_\n";()'],
	                          undef,
	                          [cols_op 2, 1, -1]]}
	| 'S' (
	    <empty>?
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {hadoop_streaming_op @$_[1..$#$_]}
	| 'T' (
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {hadoop_test_op @$_}
	)

# PARSER dsp/resourcealt

## DEFINITION
	(
	| ''7z://' /.*/ -> {resource_quote_op "7z://$_"}
	| ''7zentry://' /.*/ -> {resource_quote_op "7zentry://$_"}
	| ''aaws://' /.*/ -> {resource_quote_op "aaws://$_"}
	| ''aawt://' /.*/ -> {resource_quote_op "aawt://$_"}
	| ''abws://' /.*/ -> {resource_quote_op "abws://$_"}
	| ''abwt://' /.*/ -> {resource_quote_op "abwt://$_"}
	| ''acews://' /.*/ -> {resource_quote_op "acews://$_"}
	| ''acewt://' /.*/ -> {resource_quote_op "acewt://$_"}
	| ''adyws://' /.*/ -> {resource_quote_op "adyws://$_"}
	| ''adywt://' /.*/ -> {resource_quote_op "adywt://$_"}
	| ''afws://' /.*/ -> {resource_quote_op "afws://$_"}
	| ''afwt://' /.*/ -> {resource_quote_op "afwt://$_"}
	| ''akws://' /.*/ -> {resource_quote_op "akws://$_"}
	| ''akwt://' /.*/ -> {resource_quote_op "akwt://$_"}
	| ''alsws://' /.*/ -> {resource_quote_op "alsws://$_"}
	| ''alswt://' /.*/ -> {resource_quote_op "alswt://$_"}
	| ''amws://' /.*/ -> {resource_quote_op "amws://$_"}
	| ''amwt://' /.*/ -> {resource_quote_op "amwt://$_"}
	| ''angws://' /.*/ -> {resource_quote_op "angws://$_"}
	| ''angwt://' /.*/ -> {resource_quote_op "angwt://$_"}
	| ''anws://' /.*/ -> {resource_quote_op "anws://$_"}
	| ''anwt://' /.*/ -> {resource_quote_op "anwt://$_"}
	| ''arcws://' /.*/ -> {resource_quote_op "arcws://$_"}
	| ''arcwt://' /.*/ -> {resource_quote_op "arcwt://$_"}
	| ''arws://' /.*/ -> {resource_quote_op "arws://$_"}
	| ''arwt://' /.*/ -> {resource_quote_op "arwt://$_"}
	| ''aryws://' /.*/ -> {resource_quote_op "aryws://$_"}
	| ''arywt://' /.*/ -> {resource_quote_op "arywt://$_"}
	| ''arzws://' /.*/ -> {resource_quote_op "arzws://$_"}
	| ''arzwt://' /.*/ -> {resource_quote_op "arzwt://$_"}
	| ''astws://' /.*/ -> {resource_quote_op "astws://$_"}
	| ''astwt://' /.*/ -> {resource_quote_op "astwt://$_"}
	| ''asws://' /.*/ -> {resource_quote_op "asws://$_"}
	| ''aswt://' /.*/ -> {resource_quote_op "aswt://$_"}
	| ''atjws://' /.*/ -> {resource_quote_op "atjws://$_"}
	| ''atjwt://' /.*/ -> {resource_quote_op "atjwt://$_"}
	| ''avkws://' /.*/ -> {resource_quote_op "avkws://$_"}
	| ''avkwt://' /.*/ -> {resource_quote_op "avkwt://$_"}
	| ''avws://' /.*/ -> {resource_quote_op "avws://$_"}
	| ''avwt://' /.*/ -> {resource_quote_op "avwt://$_"}
	| ''awaws://' /.*/ -> {resource_quote_op "awaws://$_"}
	| ''awawt://' /.*/ -> {resource_quote_op "awawt://$_"}
	| ''ayws://' /.*/ -> {resource_quote_op "ayws://$_"}
	| ''aywt://' /.*/ -> {resource_quote_op "aywt://$_"}
	| ''azbws://' /.*/ -> {resource_quote_op "azbws://$_"}
	| ''azbwt://' /.*/ -> {resource_quote_op "azbwt://$_"}
	| ''azws://' /.*/ -> {resource_quote_op "azws://$_"}
	| ''azwt://' /.*/ -> {resource_quote_op "azwt://$_"}
	| ''banws://' /.*/ -> {resource_quote_op "banws://$_"}
	| ''banwt://' /.*/ -> {resource_quote_op "banwt://$_"}
	| ''barws://' /.*/ -> {resource_quote_op "barws://$_"}
	| ''barwt://' /.*/ -> {resource_quote_op "barwt://$_"}
	| ''bat-smgws://' /.*/ -> {resource_quote_op "bat-smgws://$_"}
	| ''bat-smgwt://' /.*/ -> {resource_quote_op "bat-smgwt://$_"}
	| ''baws://' /.*/ -> {resource_quote_op "baws://$_"}
	| ''bawt://' /.*/ -> {resource_quote_op "bawt://$_"}
	| ''bclws://' /.*/ -> {resource_quote_op "bclws://$_"}
	| ''bclwt://' /.*/ -> {resource_quote_op "bclwt://$_"}
	| ''be-taraskws://' /.*/ -> {resource_quote_op "be-taraskws://$_"}
	| ''be-taraskwt://' /.*/ -> {resource_quote_op "be-taraskwt://$_"}
	| ''bews://' /.*/ -> {resource_quote_op "bews://$_"}
	| ''bewt://' /.*/ -> {resource_quote_op "bewt://$_"}
	| ''bgws://' /.*/ -> {resource_quote_op "bgws://$_"}
	| ''bgwt://' /.*/ -> {resource_quote_op "bgwt://$_"}
	| ''bhws://' /.*/ -> {resource_quote_op "bhws://$_"}
	| ''bhwt://' /.*/ -> {resource_quote_op "bhwt://$_"}
	| ''biws://' /.*/ -> {resource_quote_op "biws://$_"}
	| ''biwt://' /.*/ -> {resource_quote_op "biwt://$_"}
	| ''bjnws://' /.*/ -> {resource_quote_op "bjnws://$_"}
	| ''bjnwt://' /.*/ -> {resource_quote_op "bjnwt://$_"}
	| ''bmws://' /.*/ -> {resource_quote_op "bmws://$_"}
	| ''bmwt://' /.*/ -> {resource_quote_op "bmwt://$_"}
	| ''bnws://' /.*/ -> {resource_quote_op "bnws://$_"}
	| ''bnwt://' /.*/ -> {resource_quote_op "bnwt://$_"}
	| ''bows://' /.*/ -> {resource_quote_op "bows://$_"}
	| ''bowt://' /.*/ -> {resource_quote_op "bowt://$_"}
	| ''bpyws://' /.*/ -> {resource_quote_op "bpyws://$_"}
	| ''bpywt://' /.*/ -> {resource_quote_op "bpywt://$_"}
	| ''brws://' /.*/ -> {resource_quote_op "brws://$_"}
	| ''brwt://' /.*/ -> {resource_quote_op "brwt://$_"}
	| ''bsws://' /.*/ -> {resource_quote_op "bsws://$_"}
	| ''bswt://' /.*/ -> {resource_quote_op "bswt://$_"}
	| ''bugws://' /.*/ -> {resource_quote_op "bugws://$_"}
	| ''bugwt://' /.*/ -> {resource_quote_op "bugwt://$_"}
	| ''bxrws://' /.*/ -> {resource_quote_op "bxrws://$_"}
	| ''bxrwt://' /.*/ -> {resource_quote_op "bxrwt://$_"}
	| ''caws://' /.*/ -> {resource_quote_op "caws://$_"}
	| ''cawt://' /.*/ -> {resource_quote_op "cawt://$_"}
	| ''cbk-zamws://' /.*/ -> {resource_quote_op "cbk-zamws://$_"}
	| ''cbk-zamwt://' /.*/ -> {resource_quote_op "cbk-zamwt://$_"}
	| ''cdows://' /.*/ -> {resource_quote_op "cdows://$_"}
	| ''cdowt://' /.*/ -> {resource_quote_op "cdowt://$_"}
	| ''cebws://' /.*/ -> {resource_quote_op "cebws://$_"}
	| ''cebwt://' /.*/ -> {resource_quote_op "cebwt://$_"}
	| ''cews://' /.*/ -> {resource_quote_op "cews://$_"}
	| ''cewt://' /.*/ -> {resource_quote_op "cewt://$_"}
	| ''chows://' /.*/ -> {resource_quote_op "chows://$_"}
	| ''chowt://' /.*/ -> {resource_quote_op "chowt://$_"}
	| ''chrws://' /.*/ -> {resource_quote_op "chrws://$_"}
	| ''chrwt://' /.*/ -> {resource_quote_op "chrwt://$_"}
	| ''chws://' /.*/ -> {resource_quote_op "chws://$_"}
	| ''chwt://' /.*/ -> {resource_quote_op "chwt://$_"}
	| ''chyws://' /.*/ -> {resource_quote_op "chyws://$_"}
	| ''chywt://' /.*/ -> {resource_quote_op "chywt://$_"}
	| ''ckbws://' /.*/ -> {resource_quote_op "ckbws://$_"}
	| ''ckbwt://' /.*/ -> {resource_quote_op "ckbwt://$_"}
	| ''cows://' /.*/ -> {resource_quote_op "cows://$_"}
	| ''cowt://' /.*/ -> {resource_quote_op "cowt://$_"}
	| ''crhws://' /.*/ -> {resource_quote_op "crhws://$_"}
	| ''crhwt://' /.*/ -> {resource_quote_op "crhwt://$_"}
	| ''crws://' /.*/ -> {resource_quote_op "crws://$_"}
	| ''crwt://' /.*/ -> {resource_quote_op "crwt://$_"}
	| ''csbws://' /.*/ -> {resource_quote_op "csbws://$_"}
	| ''csbwt://' /.*/ -> {resource_quote_op "csbwt://$_"}
	| ''csws://' /.*/ -> {resource_quote_op "csws://$_"}
	| ''cswt://' /.*/ -> {resource_quote_op "cswt://$_"}
	| ''cuws://' /.*/ -> {resource_quote_op "cuws://$_"}
	| ''cuwt://' /.*/ -> {resource_quote_op "cuwt://$_"}
	| ''cvws://' /.*/ -> {resource_quote_op "cvws://$_"}
	| ''cvwt://' /.*/ -> {resource_quote_op "cvwt://$_"}
	| ''cyws://' /.*/ -> {resource_quote_op "cyws://$_"}
	| ''cywt://' /.*/ -> {resource_quote_op "cywt://$_"}
	| ''daws://' /.*/ -> {resource_quote_op "daws://$_"}
	| ''dawt://' /.*/ -> {resource_quote_op "dawt://$_"}
	| ''dews://' /.*/ -> {resource_quote_op "dews://$_"}
	| ''dewt://' /.*/ -> {resource_quote_op "dewt://$_"}
	| ''dinws://' /.*/ -> {resource_quote_op "dinws://$_"}
	| ''dinwt://' /.*/ -> {resource_quote_op "dinwt://$_"}
	| ''diqws://' /.*/ -> {resource_quote_op "diqws://$_"}
	| ''diqwt://' /.*/ -> {resource_quote_op "diqwt://$_"}
	| ''dsbws://' /.*/ -> {resource_quote_op "dsbws://$_"}
	| ''dsbwt://' /.*/ -> {resource_quote_op "dsbwt://$_"}
	| ''dtyws://' /.*/ -> {resource_quote_op "dtyws://$_"}
	| ''dtywt://' /.*/ -> {resource_quote_op "dtywt://$_"}
	| ''dvws://' /.*/ -> {resource_quote_op "dvws://$_"}
	| ''dvwt://' /.*/ -> {resource_quote_op "dvwt://$_"}
	| ''dzws://' /.*/ -> {resource_quote_op "dzws://$_"}
	| ''dzwt://' /.*/ -> {resource_quote_op "dzwt://$_"}
	| ''eews://' /.*/ -> {resource_quote_op "eews://$_"}
	| ''eewt://' /.*/ -> {resource_quote_op "eewt://$_"}
	| ''elws://' /.*/ -> {resource_quote_op "elws://$_"}
	| ''elwt://' /.*/ -> {resource_quote_op "elwt://$_"}
	| ''emlws://' /.*/ -> {resource_quote_op "emlws://$_"}
	| ''emlwt://' /.*/ -> {resource_quote_op "emlwt://$_"}
	| ''enws://' /.*/ -> {resource_quote_op "enws://$_"}
	| ''enwt://' /.*/ -> {resource_quote_op "enwt://$_"}
	| ''eows://' /.*/ -> {resource_quote_op "eows://$_"}
	| ''eowt://' /.*/ -> {resource_quote_op "eowt://$_"}
	| ''esws://' /.*/ -> {resource_quote_op "esws://$_"}
	| ''eswt://' /.*/ -> {resource_quote_op "eswt://$_"}
	| ''etws://' /.*/ -> {resource_quote_op "etws://$_"}
	| ''etwt://' /.*/ -> {resource_quote_op "etwt://$_"}
	| ''euws://' /.*/ -> {resource_quote_op "euws://$_"}
	| ''euwt://' /.*/ -> {resource_quote_op "euwt://$_"}
	| ''extws://' /.*/ -> {resource_quote_op "extws://$_"}
	| ''extwt://' /.*/ -> {resource_quote_op "extwt://$_"}
	| ''faws://' /.*/ -> {resource_quote_op "faws://$_"}
	| ''fawt://' /.*/ -> {resource_quote_op "fawt://$_"}
	| ''ffws://' /.*/ -> {resource_quote_op "ffws://$_"}
	| ''ffwt://' /.*/ -> {resource_quote_op "ffwt://$_"}
	| ''file-closure://' /.*/ -> {resource_quote_op "file-closure://$_"}
	| ''file://' /.*/ -> {resource_quote_op "file://$_"}
	| ''fiu-vrows://' /.*/ -> {resource_quote_op "fiu-vrows://$_"}
	| ''fiu-vrowt://' /.*/ -> {resource_quote_op "fiu-vrowt://$_"}
	| ''fiws://' /.*/ -> {resource_quote_op "fiws://$_"}
	| ''fiwt://' /.*/ -> {resource_quote_op "fiwt://$_"}
	| ''fjws://' /.*/ -> {resource_quote_op "fjws://$_"}
	| ''fjwt://' /.*/ -> {resource_quote_op "fjwt://$_"}
	| ''fows://' /.*/ -> {resource_quote_op "fows://$_"}
	| ''fowt://' /.*/ -> {resource_quote_op "fowt://$_"}
	| ''frpws://' /.*/ -> {resource_quote_op "frpws://$_"}
	| ''frpwt://' /.*/ -> {resource_quote_op "frpwt://$_"}
	| ''frrws://' /.*/ -> {resource_quote_op "frrws://$_"}
	| ''frrwt://' /.*/ -> {resource_quote_op "frrwt://$_"}
	| ''frws://' /.*/ -> {resource_quote_op "frws://$_"}
	| ''frwt://' /.*/ -> {resource_quote_op "frwt://$_"}
	| ''furws://' /.*/ -> {resource_quote_op "furws://$_"}
	| ''furwt://' /.*/ -> {resource_quote_op "furwt://$_"}
	| ''fyws://' /.*/ -> {resource_quote_op "fyws://$_"}
	| ''fywt://' /.*/ -> {resource_quote_op "fywt://$_"}
	| ''gagws://' /.*/ -> {resource_quote_op "gagws://$_"}
	| ''gagwt://' /.*/ -> {resource_quote_op "gagwt://$_"}
	| ''ganws://' /.*/ -> {resource_quote_op "ganws://$_"}
	| ''ganwt://' /.*/ -> {resource_quote_op "ganwt://$_"}
	| ''gaws://' /.*/ -> {resource_quote_op "gaws://$_"}
	| ''gawt://' /.*/ -> {resource_quote_op "gawt://$_"}
	| ''gcrws://' /.*/ -> {resource_quote_op "gcrws://$_"}
	| ''gcrwt://' /.*/ -> {resource_quote_op "gcrwt://$_"}
	| ''gdws://' /.*/ -> {resource_quote_op "gdws://$_"}
	| ''gdwt://' /.*/ -> {resource_quote_op "gdwt://$_"}
	| ''git://' /.*/ -> {resource_quote_op "git://$_"}
	| ''gitblob://' /.*/ -> {resource_quote_op "gitblob://$_"}
	| ''gitclosure://' /.*/ -> {resource_quote_op "gitclosure://$_"}
	| ''gitcommit://' /.*/ -> {resource_quote_op "gitcommit://$_"}
	| ''gitcommitmeta://' /.*/ -> {resource_quote_op "gitcommitmeta://$_"}
	| ''gitddelta://' /.*/ -> {resource_quote_op "gitddelta://$_"}
	| ''gitdelta://' /.*/ -> {resource_quote_op "gitdelta://$_"}
	| ''gitdiff://' /.*/ -> {resource_quote_op "gitdiff://$_"}
	| ''gitdsnap://' /.*/ -> {resource_quote_op "gitdsnap://$_"}
	| ''githistory://' /.*/ -> {resource_quote_op "githistory://$_"}
	| ''gitnmhistory://' /.*/ -> {resource_quote_op "gitnmhistory://$_"}
	| ''gitpdiff://' /.*/ -> {resource_quote_op "gitpdiff://$_"}
	| ''gitsnap://' /.*/ -> {resource_quote_op "gitsnap://$_"}
	| ''gittree://' /.*/ -> {resource_quote_op "gittree://$_"}
	| ''glkws://' /.*/ -> {resource_quote_op "glkws://$_"}
	| ''glkwt://' /.*/ -> {resource_quote_op "glkwt://$_"}
	| ''glws://' /.*/ -> {resource_quote_op "glws://$_"}
	| ''glwt://' /.*/ -> {resource_quote_op "glwt://$_"}
	| ''gnws://' /.*/ -> {resource_quote_op "gnws://$_"}
	| ''gnwt://' /.*/ -> {resource_quote_op "gnwt://$_"}
	| ''gomws://' /.*/ -> {resource_quote_op "gomws://$_"}
	| ''gomwt://' /.*/ -> {resource_quote_op "gomwt://$_"}
	| ''gorws://' /.*/ -> {resource_quote_op "gorws://$_"}
	| ''gorwt://' /.*/ -> {resource_quote_op "gorwt://$_"}
	| ''gotws://' /.*/ -> {resource_quote_op "gotws://$_"}
	| ''gotwt://' /.*/ -> {resource_quote_op "gotwt://$_"}
	| ''guws://' /.*/ -> {resource_quote_op "guws://$_"}
	| ''guwt://' /.*/ -> {resource_quote_op "guwt://$_"}
	| ''gvws://' /.*/ -> {resource_quote_op "gvws://$_"}
	| ''gvwt://' /.*/ -> {resource_quote_op "gvwt://$_"}
	| ''hakws://' /.*/ -> {resource_quote_op "hakws://$_"}
	| ''hakwt://' /.*/ -> {resource_quote_op "hakwt://$_"}
	| ''haws://' /.*/ -> {resource_quote_op "haws://$_"}
	| ''hawt://' /.*/ -> {resource_quote_op "hawt://$_"}
	| ''hawws://' /.*/ -> {resource_quote_op "hawws://$_"}
	| ''hawwt://' /.*/ -> {resource_quote_op "hawwt://$_"}
	| ''hdfs://' /.*/ -> {resource_quote_op "hdfs://$_"}
	| ''hdfsc://' /.*/ -> {resource_quote_op "hdfsc://$_"}
	| ''hdfscname://' /.*/ -> {resource_quote_op "hdfscname://$_"}
	| ''hdfsj://' /.*/ -> {resource_quote_op "hdfsj://$_"}
	| ''hdfsjname://' /.*/ -> {resource_quote_op "hdfsjname://$_"}
	| ''hdfsrm://' /.*/ -> {resource_quote_op "hdfsrm://$_"}
	| ''hdfst://' /.*/ -> {resource_quote_op "hdfst://$_"}
	| ''hews://' /.*/ -> {resource_quote_op "hews://$_"}
	| ''hewt://' /.*/ -> {resource_quote_op "hewt://$_"}
	| ''hifws://' /.*/ -> {resource_quote_op "hifws://$_"}
	| ''hifwt://' /.*/ -> {resource_quote_op "hifwt://$_"}
	| ''hiws://' /.*/ -> {resource_quote_op "hiws://$_"}
	| ''hiwt://' /.*/ -> {resource_quote_op "hiwt://$_"}
	| ''hows://' /.*/ -> {resource_quote_op "hows://$_"}
	| ''howt://' /.*/ -> {resource_quote_op "howt://$_"}
	| ''hrws://' /.*/ -> {resource_quote_op "hrws://$_"}
	| ''hrwt://' /.*/ -> {resource_quote_op "hrwt://$_"}
	| ''hsbws://' /.*/ -> {resource_quote_op "hsbws://$_"}
	| ''hsbwt://' /.*/ -> {resource_quote_op "hsbwt://$_"}
	| ''http://' /.*/ -> {resource_quote_op "http://$_"}
	| ''https://' /.*/ -> {resource_quote_op "https://$_"}
	| ''htws://' /.*/ -> {resource_quote_op "htws://$_"}
	| ''htwt://' /.*/ -> {resource_quote_op "htwt://$_"}
	| ''huws://' /.*/ -> {resource_quote_op "huws://$_"}
	| ''huwt://' /.*/ -> {resource_quote_op "huwt://$_"}
	| ''hyws://' /.*/ -> {resource_quote_op "hyws://$_"}
	| ''hywt://' /.*/ -> {resource_quote_op "hywt://$_"}
	| ''hywws://' /.*/ -> {resource_quote_op "hywws://$_"}
	| ''hywwt://' /.*/ -> {resource_quote_op "hywwt://$_"}
	| ''hzws://' /.*/ -> {resource_quote_op "hzws://$_"}
	| ''hzwt://' /.*/ -> {resource_quote_op "hzwt://$_"}
	| ''iaws://' /.*/ -> {resource_quote_op "iaws://$_"}
	| ''iawt://' /.*/ -> {resource_quote_op "iawt://$_"}
	| ''idws://' /.*/ -> {resource_quote_op "idws://$_"}
	| ''idwt://' /.*/ -> {resource_quote_op "idwt://$_"}
	| ''iews://' /.*/ -> {resource_quote_op "iews://$_"}
	| ''iewt://' /.*/ -> {resource_quote_op "iewt://$_"}
	| ''igws://' /.*/ -> {resource_quote_op "igws://$_"}
	| ''igwt://' /.*/ -> {resource_quote_op "igwt://$_"}
	| ''iiws://' /.*/ -> {resource_quote_op "iiws://$_"}
	| ''iiwt://' /.*/ -> {resource_quote_op "iiwt://$_"}
	| ''ikws://' /.*/ -> {resource_quote_op "ikws://$_"}
	| ''ikwt://' /.*/ -> {resource_quote_op "ikwt://$_"}
	| ''ilows://' /.*/ -> {resource_quote_op "ilows://$_"}
	| ''ilowt://' /.*/ -> {resource_quote_op "ilowt://$_"}
	| ''inhws://' /.*/ -> {resource_quote_op "inhws://$_"}
	| ''inhwt://' /.*/ -> {resource_quote_op "inhwt://$_"}
	| ''iows://' /.*/ -> {resource_quote_op "iows://$_"}
	| ''iowt://' /.*/ -> {resource_quote_op "iowt://$_"}
	| ''isws://' /.*/ -> {resource_quote_op "isws://$_"}
	| ''iswt://' /.*/ -> {resource_quote_op "iswt://$_"}
	| ''itws://' /.*/ -> {resource_quote_op "itws://$_"}
	| ''itwt://' /.*/ -> {resource_quote_op "itwt://$_"}
	| ''iuws://' /.*/ -> {resource_quote_op "iuws://$_"}
	| ''iuwt://' /.*/ -> {resource_quote_op "iuwt://$_"}
	| ''jamws://' /.*/ -> {resource_quote_op "jamws://$_"}
	| ''jamwt://' /.*/ -> {resource_quote_op "jamwt://$_"}
	| ''jaws://' /.*/ -> {resource_quote_op "jaws://$_"}
	| ''jawt://' /.*/ -> {resource_quote_op "jawt://$_"}
	| ''jbows://' /.*/ -> {resource_quote_op "jbows://$_"}
	| ''jbowt://' /.*/ -> {resource_quote_op "jbowt://$_"}
	| ''jvws://' /.*/ -> {resource_quote_op "jvws://$_"}
	| ''jvwt://' /.*/ -> {resource_quote_op "jvwt://$_"}
	| ''kaaws://' /.*/ -> {resource_quote_op "kaaws://$_"}
	| ''kaawt://' /.*/ -> {resource_quote_op "kaawt://$_"}
	| ''kabws://' /.*/ -> {resource_quote_op "kabws://$_"}
	| ''kabwt://' /.*/ -> {resource_quote_op "kabwt://$_"}
	| ''kaws://' /.*/ -> {resource_quote_op "kaws://$_"}
	| ''kawt://' /.*/ -> {resource_quote_op "kawt://$_"}
	| ''kbdws://' /.*/ -> {resource_quote_op "kbdws://$_"}
	| ''kbdwt://' /.*/ -> {resource_quote_op "kbdwt://$_"}
	| ''kbpws://' /.*/ -> {resource_quote_op "kbpws://$_"}
	| ''kbpwt://' /.*/ -> {resource_quote_op "kbpwt://$_"}
	| ''kgws://' /.*/ -> {resource_quote_op "kgws://$_"}
	| ''kgwt://' /.*/ -> {resource_quote_op "kgwt://$_"}
	| ''kiws://' /.*/ -> {resource_quote_op "kiws://$_"}
	| ''kiwt://' /.*/ -> {resource_quote_op "kiwt://$_"}
	| ''kjws://' /.*/ -> {resource_quote_op "kjws://$_"}
	| ''kjwt://' /.*/ -> {resource_quote_op "kjwt://$_"}
	| ''kkws://' /.*/ -> {resource_quote_op "kkws://$_"}
	| ''kkwt://' /.*/ -> {resource_quote_op "kkwt://$_"}
	| ''klws://' /.*/ -> {resource_quote_op "klws://$_"}
	| ''klwt://' /.*/ -> {resource_quote_op "klwt://$_"}
	| ''kmws://' /.*/ -> {resource_quote_op "kmws://$_"}
	| ''kmwt://' /.*/ -> {resource_quote_op "kmwt://$_"}
	| ''knws://' /.*/ -> {resource_quote_op "knws://$_"}
	| ''knwt://' /.*/ -> {resource_quote_op "knwt://$_"}
	| ''koiws://' /.*/ -> {resource_quote_op "koiws://$_"}
	| ''koiwt://' /.*/ -> {resource_quote_op "koiwt://$_"}
	| ''kows://' /.*/ -> {resource_quote_op "kows://$_"}
	| ''kowt://' /.*/ -> {resource_quote_op "kowt://$_"}
	| ''krcws://' /.*/ -> {resource_quote_op "krcws://$_"}
	| ''krcwt://' /.*/ -> {resource_quote_op "krcwt://$_"}
	| ''krws://' /.*/ -> {resource_quote_op "krws://$_"}
	| ''krwt://' /.*/ -> {resource_quote_op "krwt://$_"}
	| ''kshws://' /.*/ -> {resource_quote_op "kshws://$_"}
	| ''kshwt://' /.*/ -> {resource_quote_op "kshwt://$_"}
	| ''ksws://' /.*/ -> {resource_quote_op "ksws://$_"}
	| ''kswt://' /.*/ -> {resource_quote_op "kswt://$_"}
	| ''kuws://' /.*/ -> {resource_quote_op "kuws://$_"}
	| ''kuwt://' /.*/ -> {resource_quote_op "kuwt://$_"}
	| ''kvws://' /.*/ -> {resource_quote_op "kvws://$_"}
	| ''kvwt://' /.*/ -> {resource_quote_op "kvwt://$_"}
	| ''kwws://' /.*/ -> {resource_quote_op "kwws://$_"}
	| ''kwwt://' /.*/ -> {resource_quote_op "kwwt://$_"}
	| ''kyws://' /.*/ -> {resource_quote_op "kyws://$_"}
	| ''kywt://' /.*/ -> {resource_quote_op "kywt://$_"}
	| ''ladws://' /.*/ -> {resource_quote_op "ladws://$_"}
	| ''ladwt://' /.*/ -> {resource_quote_op "ladwt://$_"}
	| ''laws://' /.*/ -> {resource_quote_op "laws://$_"}
	| ''lawt://' /.*/ -> {resource_quote_op "lawt://$_"}
	| ''lbews://' /.*/ -> {resource_quote_op "lbews://$_"}
	| ''lbewt://' /.*/ -> {resource_quote_op "lbewt://$_"}
	| ''lbws://' /.*/ -> {resource_quote_op "lbws://$_"}
	| ''lbwt://' /.*/ -> {resource_quote_op "lbwt://$_"}
	| ''lezws://' /.*/ -> {resource_quote_op "lezws://$_"}
	| ''lezwt://' /.*/ -> {resource_quote_op "lezwt://$_"}
	| ''lfnws://' /.*/ -> {resource_quote_op "lfnws://$_"}
	| ''lfnwt://' /.*/ -> {resource_quote_op "lfnwt://$_"}
	| ''lgws://' /.*/ -> {resource_quote_op "lgws://$_"}
	| ''lgwt://' /.*/ -> {resource_quote_op "lgwt://$_"}
	| ''lijws://' /.*/ -> {resource_quote_op "lijws://$_"}
	| ''lijwt://' /.*/ -> {resource_quote_op "lijwt://$_"}
	| ''liws://' /.*/ -> {resource_quote_op "liws://$_"}
	| ''liwt://' /.*/ -> {resource_quote_op "liwt://$_"}
	| ''lldws://' /.*/ -> {resource_quote_op "lldws://$_"}
	| ''lldwt://' /.*/ -> {resource_quote_op "lldwt://$_"}
	| ''lmows://' /.*/ -> {resource_quote_op "lmows://$_"}
	| ''lmowt://' /.*/ -> {resource_quote_op "lmowt://$_"}
	| ''lnws://' /.*/ -> {resource_quote_op "lnws://$_"}
	| ''lnwt://' /.*/ -> {resource_quote_op "lnwt://$_"}
	| ''lows://' /.*/ -> {resource_quote_op "lows://$_"}
	| ''lowt://' /.*/ -> {resource_quote_op "lowt://$_"}
	| ''lrcws://' /.*/ -> {resource_quote_op "lrcws://$_"}
	| ''lrcwt://' /.*/ -> {resource_quote_op "lrcwt://$_"}
	| ''ltgws://' /.*/ -> {resource_quote_op "ltgws://$_"}
	| ''ltgwt://' /.*/ -> {resource_quote_op "ltgwt://$_"}
	| ''ltws://' /.*/ -> {resource_quote_op "ltws://$_"}
	| ''ltwt://' /.*/ -> {resource_quote_op "ltwt://$_"}
	| ''lvws://' /.*/ -> {resource_quote_op "lvws://$_"}
	| ''lvwt://' /.*/ -> {resource_quote_op "lvwt://$_"}
	| ''maiws://' /.*/ -> {resource_quote_op "maiws://$_"}
	| ''maiwt://' /.*/ -> {resource_quote_op "maiwt://$_"}
	| ''map-bmsws://' /.*/ -> {resource_quote_op "map-bmsws://$_"}
	| ''map-bmswt://' /.*/ -> {resource_quote_op "map-bmswt://$_"}
	| ''mdfws://' /.*/ -> {resource_quote_op "mdfws://$_"}
	| ''mdfwt://' /.*/ -> {resource_quote_op "mdfwt://$_"}
	| ''mgws://' /.*/ -> {resource_quote_op "mgws://$_"}
	| ''mgwt://' /.*/ -> {resource_quote_op "mgwt://$_"}
	| ''mhrws://' /.*/ -> {resource_quote_op "mhrws://$_"}
	| ''mhrwt://' /.*/ -> {resource_quote_op "mhrwt://$_"}
	| ''mhws://' /.*/ -> {resource_quote_op "mhws://$_"}
	| ''mhwt://' /.*/ -> {resource_quote_op "mhwt://$_"}
	| ''minws://' /.*/ -> {resource_quote_op "minws://$_"}
	| ''minwt://' /.*/ -> {resource_quote_op "minwt://$_"}
	| ''miws://' /.*/ -> {resource_quote_op "miws://$_"}
	| ''miwt://' /.*/ -> {resource_quote_op "miwt://$_"}
	| ''mkws://' /.*/ -> {resource_quote_op "mkws://$_"}
	| ''mkwt://' /.*/ -> {resource_quote_op "mkwt://$_"}
	| ''mlws://' /.*/ -> {resource_quote_op "mlws://$_"}
	| ''mlwt://' /.*/ -> {resource_quote_op "mlwt://$_"}
	| ''mnws://' /.*/ -> {resource_quote_op "mnws://$_"}
	| ''mnwt://' /.*/ -> {resource_quote_op "mnwt://$_"}
	| ''mnwws://' /.*/ -> {resource_quote_op "mnwws://$_"}
	| ''mnwwt://' /.*/ -> {resource_quote_op "mnwwt://$_"}
	| ''mrjws://' /.*/ -> {resource_quote_op "mrjws://$_"}
	| ''mrjwt://' /.*/ -> {resource_quote_op "mrjwt://$_"}
	| ''mrws://' /.*/ -> {resource_quote_op "mrws://$_"}
	| ''mrwt://' /.*/ -> {resource_quote_op "mrwt://$_"}
	| ''msws://' /.*/ -> {resource_quote_op "msws://$_"}
	| ''mswt://' /.*/ -> {resource_quote_op "mswt://$_"}
	| ''mtws://' /.*/ -> {resource_quote_op "mtws://$_"}
	| ''mtwt://' /.*/ -> {resource_quote_op "mtwt://$_"}
	| ''musws://' /.*/ -> {resource_quote_op "musws://$_"}
	| ''muswt://' /.*/ -> {resource_quote_op "muswt://$_"}
	| ''mwlws://' /.*/ -> {resource_quote_op "mwlws://$_"}
	| ''mwlwt://' /.*/ -> {resource_quote_op "mwlwt://$_"}
	| ''myvws://' /.*/ -> {resource_quote_op "myvws://$_"}
	| ''myvwt://' /.*/ -> {resource_quote_op "myvwt://$_"}
	| ''myws://' /.*/ -> {resource_quote_op "myws://$_"}
	| ''mywt://' /.*/ -> {resource_quote_op "mywt://$_"}
	| ''mznws://' /.*/ -> {resource_quote_op "mznws://$_"}
	| ''mznwt://' /.*/ -> {resource_quote_op "mznwt://$_"}
	| ''nahws://' /.*/ -> {resource_quote_op "nahws://$_"}
	| ''nahwt://' /.*/ -> {resource_quote_op "nahwt://$_"}
	| ''napws://' /.*/ -> {resource_quote_op "napws://$_"}
	| ''napwt://' /.*/ -> {resource_quote_op "napwt://$_"}
	| ''naws://' /.*/ -> {resource_quote_op "naws://$_"}
	| ''nawt://' /.*/ -> {resource_quote_op "nawt://$_"}
	| ''nds-nlws://' /.*/ -> {resource_quote_op "nds-nlws://$_"}
	| ''nds-nlwt://' /.*/ -> {resource_quote_op "nds-nlwt://$_"}
	| ''ndsws://' /.*/ -> {resource_quote_op "ndsws://$_"}
	| ''ndswt://' /.*/ -> {resource_quote_op "ndswt://$_"}
	| ''news://' /.*/ -> {resource_quote_op "news://$_"}
	| ''newt://' /.*/ -> {resource_quote_op "newt://$_"}
	| ''newws://' /.*/ -> {resource_quote_op "newws://$_"}
	| ''newwt://' /.*/ -> {resource_quote_op "newwt://$_"}
	| ''ngws://' /.*/ -> {resource_quote_op "ngws://$_"}
	| ''ngwt://' /.*/ -> {resource_quote_op "ngwt://$_"}
	| ''nlws://' /.*/ -> {resource_quote_op "nlws://$_"}
	| ''nlwt://' /.*/ -> {resource_quote_op "nlwt://$_"}
	| ''nnws://' /.*/ -> {resource_quote_op "nnws://$_"}
	| ''nnwt://' /.*/ -> {resource_quote_op "nnwt://$_"}
	| ''novws://' /.*/ -> {resource_quote_op "novws://$_"}
	| ''novwt://' /.*/ -> {resource_quote_op "novwt://$_"}
	| ''nows://' /.*/ -> {resource_quote_op "nows://$_"}
	| ''nowt://' /.*/ -> {resource_quote_op "nowt://$_"}
	| ''nqows://' /.*/ -> {resource_quote_op "nqows://$_"}
	| ''nqowt://' /.*/ -> {resource_quote_op "nqowt://$_"}
	| ''nrmws://' /.*/ -> {resource_quote_op "nrmws://$_"}
	| ''nrmwt://' /.*/ -> {resource_quote_op "nrmwt://$_"}
	| ''nsows://' /.*/ -> {resource_quote_op "nsows://$_"}
	| ''nsowt://' /.*/ -> {resource_quote_op "nsowt://$_"}
	| ''nvws://' /.*/ -> {resource_quote_op "nvws://$_"}
	| ''nvwt://' /.*/ -> {resource_quote_op "nvwt://$_"}
	| ''nyws://' /.*/ -> {resource_quote_op "nyws://$_"}
	| ''nywt://' /.*/ -> {resource_quote_op "nywt://$_"}
	| ''ocws://' /.*/ -> {resource_quote_op "ocws://$_"}
	| ''ocwt://' /.*/ -> {resource_quote_op "ocwt://$_"}
	| ''olows://' /.*/ -> {resource_quote_op "olows://$_"}
	| ''olowt://' /.*/ -> {resource_quote_op "olowt://$_"}
	| ''omws://' /.*/ -> {resource_quote_op "omws://$_"}
	| ''omwt://' /.*/ -> {resource_quote_op "omwt://$_"}
	| ''orws://' /.*/ -> {resource_quote_op "orws://$_"}
	| ''orwt://' /.*/ -> {resource_quote_op "orwt://$_"}
	| ''osws://' /.*/ -> {resource_quote_op "osws://$_"}
	| ''oswt://' /.*/ -> {resource_quote_op "oswt://$_"}
	| ''pagws://' /.*/ -> {resource_quote_op "pagws://$_"}
	| ''pagwt://' /.*/ -> {resource_quote_op "pagwt://$_"}
	| ''pamws://' /.*/ -> {resource_quote_op "pamws://$_"}
	| ''pamwt://' /.*/ -> {resource_quote_op "pamwt://$_"}
	| ''papws://' /.*/ -> {resource_quote_op "papws://$_"}
	| ''papwt://' /.*/ -> {resource_quote_op "papwt://$_"}
	| ''paws://' /.*/ -> {resource_quote_op "paws://$_"}
	| ''pawt://' /.*/ -> {resource_quote_op "pawt://$_"}
	| ''pcdws://' /.*/ -> {resource_quote_op "pcdws://$_"}
	| ''pcdwt://' /.*/ -> {resource_quote_op "pcdwt://$_"}
	| ''pdcws://' /.*/ -> {resource_quote_op "pdcws://$_"}
	| ''pdcwt://' /.*/ -> {resource_quote_op "pdcwt://$_"}
	| ''pflws://' /.*/ -> {resource_quote_op "pflws://$_"}
	| ''pflwt://' /.*/ -> {resource_quote_op "pflwt://$_"}
	| ''pihws://' /.*/ -> {resource_quote_op "pihws://$_"}
	| ''pihwt://' /.*/ -> {resource_quote_op "pihwt://$_"}
	| ''piws://' /.*/ -> {resource_quote_op "piws://$_"}
	| ''piwt://' /.*/ -> {resource_quote_op "piwt://$_"}
	| ''plws://' /.*/ -> {resource_quote_op "plws://$_"}
	| ''plwt://' /.*/ -> {resource_quote_op "plwt://$_"}
	| ''pmsws://' /.*/ -> {resource_quote_op "pmsws://$_"}
	| ''pmswt://' /.*/ -> {resource_quote_op "pmswt://$_"}
	| ''pnbws://' /.*/ -> {resource_quote_op "pnbws://$_"}
	| ''pnbwt://' /.*/ -> {resource_quote_op "pnbwt://$_"}
	| ''pntws://' /.*/ -> {resource_quote_op "pntws://$_"}
	| ''pntwt://' /.*/ -> {resource_quote_op "pntwt://$_"}
	| ''psws://' /.*/ -> {resource_quote_op "psws://$_"}
	| ''pswt://' /.*/ -> {resource_quote_op "pswt://$_"}
	| ''ptws://' /.*/ -> {resource_quote_op "ptws://$_"}
	| ''ptwt://' /.*/ -> {resource_quote_op "ptwt://$_"}
	| ''quws://' /.*/ -> {resource_quote_op "quws://$_"}
	| ''quwt://' /.*/ -> {resource_quote_op "quwt://$_"}
	| ''rmws://' /.*/ -> {resource_quote_op "rmws://$_"}
	| ''rmwt://' /.*/ -> {resource_quote_op "rmwt://$_"}
	| ''rmyws://' /.*/ -> {resource_quote_op "rmyws://$_"}
	| ''rmywt://' /.*/ -> {resource_quote_op "rmywt://$_"}
	| ''rnws://' /.*/ -> {resource_quote_op "rnws://$_"}
	| ''rnwt://' /.*/ -> {resource_quote_op "rnwt://$_"}
	| ''roa-rupws://' /.*/ -> {resource_quote_op "roa-rupws://$_"}
	| ''roa-rupwt://' /.*/ -> {resource_quote_op "roa-rupwt://$_"}
	| ''roa-taraws://' /.*/ -> {resource_quote_op "roa-taraws://$_"}
	| ''roa-tarawt://' /.*/ -> {resource_quote_op "roa-tarawt://$_"}
	| ''rows://' /.*/ -> {resource_quote_op "rows://$_"}
	| ''rowt://' /.*/ -> {resource_quote_op "rowt://$_"}
	| ''ruews://' /.*/ -> {resource_quote_op "ruews://$_"}
	| ''ruewt://' /.*/ -> {resource_quote_op "ruewt://$_"}
	| ''ruws://' /.*/ -> {resource_quote_op "ruws://$_"}
	| ''ruwt://' /.*/ -> {resource_quote_op "ruwt://$_"}
	| ''rwws://' /.*/ -> {resource_quote_op "rwws://$_"}
	| ''rwwt://' /.*/ -> {resource_quote_op "rwwt://$_"}
	| ''s3cmd://' /.*/ -> {resource_quote_op "s3cmd://$_"}
	| ''sahws://' /.*/ -> {resource_quote_op "sahws://$_"}
	| ''sahwt://' /.*/ -> {resource_quote_op "sahwt://$_"}
	| ''satws://' /.*/ -> {resource_quote_op "satws://$_"}
	| ''satwt://' /.*/ -> {resource_quote_op "satwt://$_"}
	| ''saws://' /.*/ -> {resource_quote_op "saws://$_"}
	| ''sawt://' /.*/ -> {resource_quote_op "sawt://$_"}
	| ''scnws://' /.*/ -> {resource_quote_op "scnws://$_"}
	| ''scnwt://' /.*/ -> {resource_quote_op "scnwt://$_"}
	| ''scows://' /.*/ -> {resource_quote_op "scows://$_"}
	| ''scowt://' /.*/ -> {resource_quote_op "scowt://$_"}
	| ''scws://' /.*/ -> {resource_quote_op "scws://$_"}
	| ''scwt://' /.*/ -> {resource_quote_op "scwt://$_"}
	| ''sdws://' /.*/ -> {resource_quote_op "sdws://$_"}
	| ''sdwt://' /.*/ -> {resource_quote_op "sdwt://$_"}
	| ''sews://' /.*/ -> {resource_quote_op "sews://$_"}
	| ''sewt://' /.*/ -> {resource_quote_op "sewt://$_"}
	| ''sftp://' /.*/ -> {resource_quote_op "sftp://$_"}
	| ''sgws://' /.*/ -> {resource_quote_op "sgws://$_"}
	| ''sgwt://' /.*/ -> {resource_quote_op "sgwt://$_"}
	| ''shnws://' /.*/ -> {resource_quote_op "shnws://$_"}
	| ''shnwt://' /.*/ -> {resource_quote_op "shnwt://$_"}
	| ''shws://' /.*/ -> {resource_quote_op "shws://$_"}
	| ''shwt://' /.*/ -> {resource_quote_op "shwt://$_"}
	| ''simplews://' /.*/ -> {resource_quote_op "simplews://$_"}
	| ''simplewt://' /.*/ -> {resource_quote_op "simplewt://$_"}
	| ''siws://' /.*/ -> {resource_quote_op "siws://$_"}
	| ''siwt://' /.*/ -> {resource_quote_op "siwt://$_"}
	| ''skws://' /.*/ -> {resource_quote_op "skws://$_"}
	| ''skwt://' /.*/ -> {resource_quote_op "skwt://$_"}
	| ''slws://' /.*/ -> {resource_quote_op "slws://$_"}
	| ''slwt://' /.*/ -> {resource_quote_op "slwt://$_"}
	| ''smws://' /.*/ -> {resource_quote_op "smws://$_"}
	| ''smwt://' /.*/ -> {resource_quote_op "smwt://$_"}
	| ''snws://' /.*/ -> {resource_quote_op "snws://$_"}
	| ''snwt://' /.*/ -> {resource_quote_op "snwt://$_"}
	| ''solr://' /.*/ -> {resource_quote_op "solr://$_"}
	| ''sows://' /.*/ -> {resource_quote_op "sows://$_"}
	| ''sowt://' /.*/ -> {resource_quote_op "sowt://$_"}
	| ''sqlite://' /.*/ -> {resource_quote_op "sqlite://$_"}
	| ''sqliteq://' /.*/ -> {resource_quote_op "sqliteq://$_"}
	| ''sqlites://' /.*/ -> {resource_quote_op "sqlites://$_"}
	| ''sqlitet://' /.*/ -> {resource_quote_op "sqlitet://$_"}
	| ''sqws://' /.*/ -> {resource_quote_op "sqws://$_"}
	| ''sqwt://' /.*/ -> {resource_quote_op "sqwt://$_"}
	| ''srnws://' /.*/ -> {resource_quote_op "srnws://$_"}
	| ''srnwt://' /.*/ -> {resource_quote_op "srnwt://$_"}
	| ''srws://' /.*/ -> {resource_quote_op "srws://$_"}
	| ''srwt://' /.*/ -> {resource_quote_op "srwt://$_"}
	| ''ssws://' /.*/ -> {resource_quote_op "ssws://$_"}
	| ''sswt://' /.*/ -> {resource_quote_op "sswt://$_"}
	| ''stqws://' /.*/ -> {resource_quote_op "stqws://$_"}
	| ''stqwt://' /.*/ -> {resource_quote_op "stqwt://$_"}
	| ''stws://' /.*/ -> {resource_quote_op "stws://$_"}
	| ''stwt://' /.*/ -> {resource_quote_op "stwt://$_"}
	| ''suws://' /.*/ -> {resource_quote_op "suws://$_"}
	| ''suwt://' /.*/ -> {resource_quote_op "suwt://$_"}
	| ''svws://' /.*/ -> {resource_quote_op "svws://$_"}
	| ''svwt://' /.*/ -> {resource_quote_op "svwt://$_"}
	| ''swws://' /.*/ -> {resource_quote_op "swws://$_"}
	| ''swwt://' /.*/ -> {resource_quote_op "swwt://$_"}
	| ''szlws://' /.*/ -> {resource_quote_op "szlws://$_"}
	| ''szlwt://' /.*/ -> {resource_quote_op "szlwt://$_"}
	| ''szyws://' /.*/ -> {resource_quote_op "szyws://$_"}
	| ''szywt://' /.*/ -> {resource_quote_op "szywt://$_"}
	| ''tar://' /.*/ -> {resource_quote_op "tar://$_"}
	| ''tarentry://' /.*/ -> {resource_quote_op "tarentry://$_"}
	| ''taws://' /.*/ -> {resource_quote_op "taws://$_"}
	| ''tawt://' /.*/ -> {resource_quote_op "tawt://$_"}
	| ''tcyws://' /.*/ -> {resource_quote_op "tcyws://$_"}
	| ''tcywt://' /.*/ -> {resource_quote_op "tcywt://$_"}
	| ''tetws://' /.*/ -> {resource_quote_op "tetws://$_"}
	| ''tetwt://' /.*/ -> {resource_quote_op "tetwt://$_"}
	| ''tews://' /.*/ -> {resource_quote_op "tews://$_"}
	| ''tewt://' /.*/ -> {resource_quote_op "tewt://$_"}
	| ''tgws://' /.*/ -> {resource_quote_op "tgws://$_"}
	| ''tgwt://' /.*/ -> {resource_quote_op "tgwt://$_"}
	| ''thws://' /.*/ -> {resource_quote_op "thws://$_"}
	| ''thwt://' /.*/ -> {resource_quote_op "thwt://$_"}
	| ''tiws://' /.*/ -> {resource_quote_op "tiws://$_"}
	| ''tiwt://' /.*/ -> {resource_quote_op "tiwt://$_"}
	| ''tkws://' /.*/ -> {resource_quote_op "tkws://$_"}
	| ''tkwt://' /.*/ -> {resource_quote_op "tkwt://$_"}
	| ''tlws://' /.*/ -> {resource_quote_op "tlws://$_"}
	| ''tlwt://' /.*/ -> {resource_quote_op "tlwt://$_"}
	| ''tnws://' /.*/ -> {resource_quote_op "tnws://$_"}
	| ''tnwt://' /.*/ -> {resource_quote_op "tnwt://$_"}
	| ''tows://' /.*/ -> {resource_quote_op "tows://$_"}
	| ''towt://' /.*/ -> {resource_quote_op "towt://$_"}
	| ''tpiws://' /.*/ -> {resource_quote_op "tpiws://$_"}
	| ''tpiwt://' /.*/ -> {resource_quote_op "tpiwt://$_"}
	| ''trws://' /.*/ -> {resource_quote_op "trws://$_"}
	| ''trwt://' /.*/ -> {resource_quote_op "trwt://$_"}
	| ''tsws://' /.*/ -> {resource_quote_op "tsws://$_"}
	| ''tswt://' /.*/ -> {resource_quote_op "tswt://$_"}
	| ''ttws://' /.*/ -> {resource_quote_op "ttws://$_"}
	| ''ttwt://' /.*/ -> {resource_quote_op "ttwt://$_"}
	| ''tumws://' /.*/ -> {resource_quote_op "tumws://$_"}
	| ''tumwt://' /.*/ -> {resource_quote_op "tumwt://$_"}
	| ''twws://' /.*/ -> {resource_quote_op "twws://$_"}
	| ''twwt://' /.*/ -> {resource_quote_op "twwt://$_"}
	| ''tyvws://' /.*/ -> {resource_quote_op "tyvws://$_"}
	| ''tyvwt://' /.*/ -> {resource_quote_op "tyvwt://$_"}
	| ''tyws://' /.*/ -> {resource_quote_op "tyws://$_"}
	| ''tywt://' /.*/ -> {resource_quote_op "tywt://$_"}
	| ''udmws://' /.*/ -> {resource_quote_op "udmws://$_"}
	| ''udmwt://' /.*/ -> {resource_quote_op "udmwt://$_"}
	| ''ugws://' /.*/ -> {resource_quote_op "ugws://$_"}
	| ''ugwt://' /.*/ -> {resource_quote_op "ugwt://$_"}
	| ''ukws://' /.*/ -> {resource_quote_op "ukws://$_"}
	| ''ukwt://' /.*/ -> {resource_quote_op "ukwt://$_"}
	| ''urws://' /.*/ -> {resource_quote_op "urws://$_"}
	| ''urwt://' /.*/ -> {resource_quote_op "urwt://$_"}
	| ''uzws://' /.*/ -> {resource_quote_op "uzws://$_"}
	| ''uzwt://' /.*/ -> {resource_quote_op "uzwt://$_"}
	| ''vecws://' /.*/ -> {resource_quote_op "vecws://$_"}
	| ''vecwt://' /.*/ -> {resource_quote_op "vecwt://$_"}
	| ''vepws://' /.*/ -> {resource_quote_op "vepws://$_"}
	| ''vepwt://' /.*/ -> {resource_quote_op "vepwt://$_"}
	| ''vews://' /.*/ -> {resource_quote_op "vews://$_"}
	| ''vewt://' /.*/ -> {resource_quote_op "vewt://$_"}
	| ''viws://' /.*/ -> {resource_quote_op "viws://$_"}
	| ''viwt://' /.*/ -> {resource_quote_op "viwt://$_"}
	| ''vlsws://' /.*/ -> {resource_quote_op "vlsws://$_"}
	| ''vlswt://' /.*/ -> {resource_quote_op "vlswt://$_"}
	| ''vows://' /.*/ -> {resource_quote_op "vows://$_"}
	| ''vowt://' /.*/ -> {resource_quote_op "vowt://$_"}
	| ''warws://' /.*/ -> {resource_quote_op "warws://$_"}
	| ''warwt://' /.*/ -> {resource_quote_op "warwt://$_"}
	| ''waws://' /.*/ -> {resource_quote_op "waws://$_"}
	| ''wawt://' /.*/ -> {resource_quote_op "wawt://$_"}
	| ''wiki://' /.*/ -> {resource_quote_op "wiki://$_"}
	| ''wows://' /.*/ -> {resource_quote_op "wows://$_"}
	| ''wowt://' /.*/ -> {resource_quote_op "wowt://$_"}
	| ''wuuws://' /.*/ -> {resource_quote_op "wuuws://$_"}
	| ''wuuwt://' /.*/ -> {resource_quote_op "wuuwt://$_"}
	| ''xalws://' /.*/ -> {resource_quote_op "xalws://$_"}
	| ''xalwt://' /.*/ -> {resource_quote_op "xalwt://$_"}
	| ''xhws://' /.*/ -> {resource_quote_op "xhws://$_"}
	| ''xhwt://' /.*/ -> {resource_quote_op "xhwt://$_"}
	| ''xlsx://' /.*/ -> {resource_quote_op "xlsx://$_"}
	| ''xlsxsheet://' /.*/ -> {resource_quote_op "xlsxsheet://$_"}
	| ''xmfws://' /.*/ -> {resource_quote_op "xmfws://$_"}
	| ''xmfwt://' /.*/ -> {resource_quote_op "xmfwt://$_"}
	| ''yiws://' /.*/ -> {resource_quote_op "yiws://$_"}
	| ''yiwt://' /.*/ -> {resource_quote_op "yiwt://$_"}
	| ''yows://' /.*/ -> {resource_quote_op "yows://$_"}
	| ''yowt://' /.*/ -> {resource_quote_op "yowt://$_"}
	| ''yt://' /.*/ -> {resource_quote_op "yt://$_"}
	| ''zaws://' /.*/ -> {resource_quote_op "zaws://$_"}
	| ''zawt://' /.*/ -> {resource_quote_op "zawt://$_"}
	| ''zeaws://' /.*/ -> {resource_quote_op "zeaws://$_"}
	| ''zeawt://' /.*/ -> {resource_quote_op "zeawt://$_"}
	| ''zh-classicalws://' /.*/ -> {resource_quote_op "zh-classicalws://$_"}
	| ''zh-classicalwt://' /.*/ -> {resource_quote_op "zh-classicalwt://$_"}
	| ''zh-min-nanws://' /.*/ -> {resource_quote_op "zh-min-nanws://$_"}
	| ''zh-min-nanwt://' /.*/ -> {resource_quote_op "zh-min-nanwt://$_"}
	| ''zh-yuews://' /.*/ -> {resource_quote_op "zh-yuews://$_"}
	| ''zh-yuewt://' /.*/ -> {resource_quote_op "zh-yuewt://$_"}
	| ''zhws://' /.*/ -> {resource_quote_op "zhws://$_"}
	| ''zhwt://' /.*/ -> {resource_quote_op "zhwt://$_"}
	| ''zip://' /.*/ -> {resource_quote_op "zip://$_"}
	| ''zipentry://' /.*/ -> {resource_quote_op "zipentry://$_"}
	| ''zuws://' /.*/ -> {resource_quote_op "zuws://$_"}
	| ''zuwt://' /.*/ -> {resource_quote_op "zuwt://$_"}
	| '7z://' /.*/ -> {resource_append_op "7z://$_"}
	| '7zentry://' /.*/ -> {resource_append_op "7zentry://$_"}
	| 'aaws://' /.*/ -> {resource_append_op "aaws://$_"}
	| 'aawt://' /.*/ -> {resource_append_op "aawt://$_"}
	| 'abws://' /.*/ -> {resource_append_op "abws://$_"}
	| 'abwt://' /.*/ -> {resource_append_op "abwt://$_"}
	| 'acews://' /.*/ -> {resource_append_op "acews://$_"}
	| 'acewt://' /.*/ -> {resource_append_op "acewt://$_"}
	| 'adyws://' /.*/ -> {resource_append_op "adyws://$_"}
	| 'adywt://' /.*/ -> {resource_append_op "adywt://$_"}
	| 'afws://' /.*/ -> {resource_append_op "afws://$_"}
	| 'afwt://' /.*/ -> {resource_append_op "afwt://$_"}
	| 'akws://' /.*/ -> {resource_append_op "akws://$_"}
	| 'akwt://' /.*/ -> {resource_append_op "akwt://$_"}
	| 'alsws://' /.*/ -> {resource_append_op "alsws://$_"}
	| 'alswt://' /.*/ -> {resource_append_op "alswt://$_"}
	| 'amws://' /.*/ -> {resource_append_op "amws://$_"}
	| 'amwt://' /.*/ -> {resource_append_op "amwt://$_"}
	| 'angws://' /.*/ -> {resource_append_op "angws://$_"}
	| 'angwt://' /.*/ -> {resource_append_op "angwt://$_"}
	| 'anws://' /.*/ -> {resource_append_op "anws://$_"}
	| 'anwt://' /.*/ -> {resource_append_op "anwt://$_"}
	| 'arcws://' /.*/ -> {resource_append_op "arcws://$_"}
	| 'arcwt://' /.*/ -> {resource_append_op "arcwt://$_"}
	| 'arws://' /.*/ -> {resource_append_op "arws://$_"}
	| 'arwt://' /.*/ -> {resource_append_op "arwt://$_"}
	| 'aryws://' /.*/ -> {resource_append_op "aryws://$_"}
	| 'arywt://' /.*/ -> {resource_append_op "arywt://$_"}
	| 'arzws://' /.*/ -> {resource_append_op "arzws://$_"}
	| 'arzwt://' /.*/ -> {resource_append_op "arzwt://$_"}
	| 'astws://' /.*/ -> {resource_append_op "astws://$_"}
	| 'astwt://' /.*/ -> {resource_append_op "astwt://$_"}
	| 'asws://' /.*/ -> {resource_append_op "asws://$_"}
	| 'aswt://' /.*/ -> {resource_append_op "aswt://$_"}
	| 'atjws://' /.*/ -> {resource_append_op "atjws://$_"}
	| 'atjwt://' /.*/ -> {resource_append_op "atjwt://$_"}
	| 'avkws://' /.*/ -> {resource_append_op "avkws://$_"}
	| 'avkwt://' /.*/ -> {resource_append_op "avkwt://$_"}
	| 'avws://' /.*/ -> {resource_append_op "avws://$_"}
	| 'avwt://' /.*/ -> {resource_append_op "avwt://$_"}
	| 'awaws://' /.*/ -> {resource_append_op "awaws://$_"}
	| 'awawt://' /.*/ -> {resource_append_op "awawt://$_"}
	| 'ayws://' /.*/ -> {resource_append_op "ayws://$_"}
	| 'aywt://' /.*/ -> {resource_append_op "aywt://$_"}
	| 'azbws://' /.*/ -> {resource_append_op "azbws://$_"}
	| 'azbwt://' /.*/ -> {resource_append_op "azbwt://$_"}
	| 'azws://' /.*/ -> {resource_append_op "azws://$_"}
	| 'azwt://' /.*/ -> {resource_append_op "azwt://$_"}
	| 'banws://' /.*/ -> {resource_append_op "banws://$_"}
	| 'banwt://' /.*/ -> {resource_append_op "banwt://$_"}
	| 'barws://' /.*/ -> {resource_append_op "barws://$_"}
	| 'barwt://' /.*/ -> {resource_append_op "barwt://$_"}
	| 'bat-smgws://' /.*/ -> {resource_append_op "bat-smgws://$_"}
	| 'bat-smgwt://' /.*/ -> {resource_append_op "bat-smgwt://$_"}
	| 'baws://' /.*/ -> {resource_append_op "baws://$_"}
	| 'bawt://' /.*/ -> {resource_append_op "bawt://$_"}
	| 'bclws://' /.*/ -> {resource_append_op "bclws://$_"}
	| 'bclwt://' /.*/ -> {resource_append_op "bclwt://$_"}
	| 'be-taraskws://' /.*/ -> {resource_append_op "be-taraskws://$_"}
	| 'be-taraskwt://' /.*/ -> {resource_append_op "be-taraskwt://$_"}
	| 'bews://' /.*/ -> {resource_append_op "bews://$_"}
	| 'bewt://' /.*/ -> {resource_append_op "bewt://$_"}
	| 'bgws://' /.*/ -> {resource_append_op "bgws://$_"}
	| 'bgwt://' /.*/ -> {resource_append_op "bgwt://$_"}
	| 'bhws://' /.*/ -> {resource_append_op "bhws://$_"}
	| 'bhwt://' /.*/ -> {resource_append_op "bhwt://$_"}
	| 'biws://' /.*/ -> {resource_append_op "biws://$_"}
	| 'biwt://' /.*/ -> {resource_append_op "biwt://$_"}
	| 'bjnws://' /.*/ -> {resource_append_op "bjnws://$_"}
	| 'bjnwt://' /.*/ -> {resource_append_op "bjnwt://$_"}
	| 'bmws://' /.*/ -> {resource_append_op "bmws://$_"}
	| 'bmwt://' /.*/ -> {resource_append_op "bmwt://$_"}
	| 'bnws://' /.*/ -> {resource_append_op "bnws://$_"}
	| 'bnwt://' /.*/ -> {resource_append_op "bnwt://$_"}
	| 'bows://' /.*/ -> {resource_append_op "bows://$_"}
	| 'bowt://' /.*/ -> {resource_append_op "bowt://$_"}
	| 'bpyws://' /.*/ -> {resource_append_op "bpyws://$_"}
	| 'bpywt://' /.*/ -> {resource_append_op "bpywt://$_"}
	| 'brws://' /.*/ -> {resource_append_op "brws://$_"}
	| 'brwt://' /.*/ -> {resource_append_op "brwt://$_"}
	| 'bsws://' /.*/ -> {resource_append_op "bsws://$_"}
	| 'bswt://' /.*/ -> {resource_append_op "bswt://$_"}
	| 'bugws://' /.*/ -> {resource_append_op "bugws://$_"}
	| 'bugwt://' /.*/ -> {resource_append_op "bugwt://$_"}
	| 'bxrws://' /.*/ -> {resource_append_op "bxrws://$_"}
	| 'bxrwt://' /.*/ -> {resource_append_op "bxrwt://$_"}
	| 'caws://' /.*/ -> {resource_append_op "caws://$_"}
	| 'cawt://' /.*/ -> {resource_append_op "cawt://$_"}
	| 'cbk-zamws://' /.*/ -> {resource_append_op "cbk-zamws://$_"}
	| 'cbk-zamwt://' /.*/ -> {resource_append_op "cbk-zamwt://$_"}
	| 'cdows://' /.*/ -> {resource_append_op "cdows://$_"}
	| 'cdowt://' /.*/ -> {resource_append_op "cdowt://$_"}
	| 'cebws://' /.*/ -> {resource_append_op "cebws://$_"}
	| 'cebwt://' /.*/ -> {resource_append_op "cebwt://$_"}
	| 'cews://' /.*/ -> {resource_append_op "cews://$_"}
	| 'cewt://' /.*/ -> {resource_append_op "cewt://$_"}
	| 'chows://' /.*/ -> {resource_append_op "chows://$_"}
	| 'chowt://' /.*/ -> {resource_append_op "chowt://$_"}
	| 'chrws://' /.*/ -> {resource_append_op "chrws://$_"}
	| 'chrwt://' /.*/ -> {resource_append_op "chrwt://$_"}
	| 'chws://' /.*/ -> {resource_append_op "chws://$_"}
	| 'chwt://' /.*/ -> {resource_append_op "chwt://$_"}
	| 'chyws://' /.*/ -> {resource_append_op "chyws://$_"}
	| 'chywt://' /.*/ -> {resource_append_op "chywt://$_"}
	| 'ckbws://' /.*/ -> {resource_append_op "ckbws://$_"}
	| 'ckbwt://' /.*/ -> {resource_append_op "ckbwt://$_"}
	| 'cows://' /.*/ -> {resource_append_op "cows://$_"}
	| 'cowt://' /.*/ -> {resource_append_op "cowt://$_"}
	| 'crhws://' /.*/ -> {resource_append_op "crhws://$_"}
	| 'crhwt://' /.*/ -> {resource_append_op "crhwt://$_"}
	| 'crws://' /.*/ -> {resource_append_op "crws://$_"}
	| 'crwt://' /.*/ -> {resource_append_op "crwt://$_"}
	| 'csbws://' /.*/ -> {resource_append_op "csbws://$_"}
	| 'csbwt://' /.*/ -> {resource_append_op "csbwt://$_"}
	| 'csws://' /.*/ -> {resource_append_op "csws://$_"}
	| 'cswt://' /.*/ -> {resource_append_op "cswt://$_"}
	| 'cuws://' /.*/ -> {resource_append_op "cuws://$_"}
	| 'cuwt://' /.*/ -> {resource_append_op "cuwt://$_"}
	| 'cvws://' /.*/ -> {resource_append_op "cvws://$_"}
	| 'cvwt://' /.*/ -> {resource_append_op "cvwt://$_"}
	| 'cyws://' /.*/ -> {resource_append_op "cyws://$_"}
	| 'cywt://' /.*/ -> {resource_append_op "cywt://$_"}
	| 'daws://' /.*/ -> {resource_append_op "daws://$_"}
	| 'dawt://' /.*/ -> {resource_append_op "dawt://$_"}
	| 'dews://' /.*/ -> {resource_append_op "dews://$_"}
	| 'dewt://' /.*/ -> {resource_append_op "dewt://$_"}
	| 'dinws://' /.*/ -> {resource_append_op "dinws://$_"}
	| 'dinwt://' /.*/ -> {resource_append_op "dinwt://$_"}
	| 'diqws://' /.*/ -> {resource_append_op "diqws://$_"}
	| 'diqwt://' /.*/ -> {resource_append_op "diqwt://$_"}
	| 'dsbws://' /.*/ -> {resource_append_op "dsbws://$_"}
	| 'dsbwt://' /.*/ -> {resource_append_op "dsbwt://$_"}
	| 'dtyws://' /.*/ -> {resource_append_op "dtyws://$_"}
	| 'dtywt://' /.*/ -> {resource_append_op "dtywt://$_"}
	| 'dvws://' /.*/ -> {resource_append_op "dvws://$_"}
	| 'dvwt://' /.*/ -> {resource_append_op "dvwt://$_"}
	| 'dzws://' /.*/ -> {resource_append_op "dzws://$_"}
	| 'dzwt://' /.*/ -> {resource_append_op "dzwt://$_"}
	| 'eews://' /.*/ -> {resource_append_op "eews://$_"}
	| 'eewt://' /.*/ -> {resource_append_op "eewt://$_"}
	| 'elws://' /.*/ -> {resource_append_op "elws://$_"}
	| 'elwt://' /.*/ -> {resource_append_op "elwt://$_"}
	| 'emlws://' /.*/ -> {resource_append_op "emlws://$_"}
	| 'emlwt://' /.*/ -> {resource_append_op "emlwt://$_"}
	| 'enws://' /.*/ -> {resource_append_op "enws://$_"}
	| 'enwt://' /.*/ -> {resource_append_op "enwt://$_"}
	| 'eows://' /.*/ -> {resource_append_op "eows://$_"}
	| 'eowt://' /.*/ -> {resource_append_op "eowt://$_"}
	| 'esws://' /.*/ -> {resource_append_op "esws://$_"}
	| 'eswt://' /.*/ -> {resource_append_op "eswt://$_"}
	| 'etws://' /.*/ -> {resource_append_op "etws://$_"}
	| 'etwt://' /.*/ -> {resource_append_op "etwt://$_"}
	| 'euws://' /.*/ -> {resource_append_op "euws://$_"}
	| 'euwt://' /.*/ -> {resource_append_op "euwt://$_"}
	| 'extws://' /.*/ -> {resource_append_op "extws://$_"}
	| 'extwt://' /.*/ -> {resource_append_op "extwt://$_"}
	| 'faws://' /.*/ -> {resource_append_op "faws://$_"}
	| 'fawt://' /.*/ -> {resource_append_op "fawt://$_"}
	| 'ffws://' /.*/ -> {resource_append_op "ffws://$_"}
	| 'ffwt://' /.*/ -> {resource_append_op "ffwt://$_"}
	| 'file-closure://' /.*/ -> {resource_append_op "file-closure://$_"}
	| 'file://' /.*/ -> {resource_append_op "file://$_"}
	| 'fiu-vrows://' /.*/ -> {resource_append_op "fiu-vrows://$_"}
	| 'fiu-vrowt://' /.*/ -> {resource_append_op "fiu-vrowt://$_"}
	| 'fiws://' /.*/ -> {resource_append_op "fiws://$_"}
	| 'fiwt://' /.*/ -> {resource_append_op "fiwt://$_"}
	| 'fjws://' /.*/ -> {resource_append_op "fjws://$_"}
	| 'fjwt://' /.*/ -> {resource_append_op "fjwt://$_"}
	| 'fows://' /.*/ -> {resource_append_op "fows://$_"}
	| 'fowt://' /.*/ -> {resource_append_op "fowt://$_"}
	| 'frpws://' /.*/ -> {resource_append_op "frpws://$_"}
	| 'frpwt://' /.*/ -> {resource_append_op "frpwt://$_"}
	| 'frrws://' /.*/ -> {resource_append_op "frrws://$_"}
	| 'frrwt://' /.*/ -> {resource_append_op "frrwt://$_"}
	| 'frws://' /.*/ -> {resource_append_op "frws://$_"}
	| 'frwt://' /.*/ -> {resource_append_op "frwt://$_"}
	| 'furws://' /.*/ -> {resource_append_op "furws://$_"}
	| 'furwt://' /.*/ -> {resource_append_op "furwt://$_"}
	| 'fyws://' /.*/ -> {resource_append_op "fyws://$_"}
	| 'fywt://' /.*/ -> {resource_append_op "fywt://$_"}
	| 'gagws://' /.*/ -> {resource_append_op "gagws://$_"}
	| 'gagwt://' /.*/ -> {resource_append_op "gagwt://$_"}
	| 'ganws://' /.*/ -> {resource_append_op "ganws://$_"}
	| 'ganwt://' /.*/ -> {resource_append_op "ganwt://$_"}
	| 'gaws://' /.*/ -> {resource_append_op "gaws://$_"}
	| 'gawt://' /.*/ -> {resource_append_op "gawt://$_"}
	| 'gcrws://' /.*/ -> {resource_append_op "gcrws://$_"}
	| 'gcrwt://' /.*/ -> {resource_append_op "gcrwt://$_"}
	| 'gdws://' /.*/ -> {resource_append_op "gdws://$_"}
	| 'gdwt://' /.*/ -> {resource_append_op "gdwt://$_"}
	| 'git://' /.*/ -> {resource_append_op "git://$_"}
	| 'gitblob://' /.*/ -> {resource_append_op "gitblob://$_"}
	| 'gitclosure://' /.*/ -> {resource_append_op "gitclosure://$_"}
	| 'gitcommit://' /.*/ -> {resource_append_op "gitcommit://$_"}
	| 'gitcommitmeta://' /.*/ -> {resource_append_op "gitcommitmeta://$_"}
	| 'gitddelta://' /.*/ -> {resource_append_op "gitddelta://$_"}
	| 'gitdelta://' /.*/ -> {resource_append_op "gitdelta://$_"}
	| 'gitdiff://' /.*/ -> {resource_append_op "gitdiff://$_"}
	| 'gitdsnap://' /.*/ -> {resource_append_op "gitdsnap://$_"}
	| 'githistory://' /.*/ -> {resource_append_op "githistory://$_"}
	| 'gitnmhistory://' /.*/ -> {resource_append_op "gitnmhistory://$_"}
	| 'gitpdiff://' /.*/ -> {resource_append_op "gitpdiff://$_"}
	| 'gitsnap://' /.*/ -> {resource_append_op "gitsnap://$_"}
	| 'gittree://' /.*/ -> {resource_append_op "gittree://$_"}
	| 'glkws://' /.*/ -> {resource_append_op "glkws://$_"}
	| 'glkwt://' /.*/ -> {resource_append_op "glkwt://$_"}
	| 'glws://' /.*/ -> {resource_append_op "glws://$_"}
	| 'glwt://' /.*/ -> {resource_append_op "glwt://$_"}
	| 'gnws://' /.*/ -> {resource_append_op "gnws://$_"}
	| 'gnwt://' /.*/ -> {resource_append_op "gnwt://$_"}
	| 'gomws://' /.*/ -> {resource_append_op "gomws://$_"}
	| 'gomwt://' /.*/ -> {resource_append_op "gomwt://$_"}
	| 'gorws://' /.*/ -> {resource_append_op "gorws://$_"}
	| 'gorwt://' /.*/ -> {resource_append_op "gorwt://$_"}
	| 'gotws://' /.*/ -> {resource_append_op "gotws://$_"}
	| 'gotwt://' /.*/ -> {resource_append_op "gotwt://$_"}
	| 'guws://' /.*/ -> {resource_append_op "guws://$_"}
	| 'guwt://' /.*/ -> {resource_append_op "guwt://$_"}
	| 'gvws://' /.*/ -> {resource_append_op "gvws://$_"}
	| 'gvwt://' /.*/ -> {resource_append_op "gvwt://$_"}
	| 'hakws://' /.*/ -> {resource_append_op "hakws://$_"}
	| 'hakwt://' /.*/ -> {resource_append_op "hakwt://$_"}
	| 'haws://' /.*/ -> {resource_append_op "haws://$_"}
	| 'hawt://' /.*/ -> {resource_append_op "hawt://$_"}
	| 'hawws://' /.*/ -> {resource_append_op "hawws://$_"}
	| 'hawwt://' /.*/ -> {resource_append_op "hawwt://$_"}
	| 'hdfs://' /.*/ -> {resource_append_op "hdfs://$_"}
	| 'hdfsc://' /.*/ -> {resource_append_op "hdfsc://$_"}
	| 'hdfscname://' /.*/ -> {resource_append_op "hdfscname://$_"}
	| 'hdfsj://' /.*/ -> {resource_append_op "hdfsj://$_"}
	| 'hdfsjname://' /.*/ -> {resource_append_op "hdfsjname://$_"}
	| 'hdfsrm://' /.*/ -> {resource_append_op "hdfsrm://$_"}
	| 'hdfst://' /.*/ -> {resource_append_op "hdfst://$_"}
	| 'hews://' /.*/ -> {resource_append_op "hews://$_"}
	| 'hewt://' /.*/ -> {resource_append_op "hewt://$_"}
	| 'hifws://' /.*/ -> {resource_append_op "hifws://$_"}
	| 'hifwt://' /.*/ -> {resource_append_op "hifwt://$_"}
	| 'hiws://' /.*/ -> {resource_append_op "hiws://$_"}
	| 'hiwt://' /.*/ -> {resource_append_op "hiwt://$_"}
	| 'hows://' /.*/ -> {resource_append_op "hows://$_"}
	| 'howt://' /.*/ -> {resource_append_op "howt://$_"}
	| 'hrws://' /.*/ -> {resource_append_op "hrws://$_"}
	| 'hrwt://' /.*/ -> {resource_append_op "hrwt://$_"}
	| 'hsbws://' /.*/ -> {resource_append_op "hsbws://$_"}
	| 'hsbwt://' /.*/ -> {resource_append_op "hsbwt://$_"}
	| 'http://' /.*/ -> {resource_append_op "http://$_"}
	| 'https://' /.*/ -> {resource_append_op "https://$_"}
	| 'htws://' /.*/ -> {resource_append_op "htws://$_"}
	| 'htwt://' /.*/ -> {resource_append_op "htwt://$_"}
	| 'huws://' /.*/ -> {resource_append_op "huws://$_"}
	| 'huwt://' /.*/ -> {resource_append_op "huwt://$_"}
	| 'hyws://' /.*/ -> {resource_append_op "hyws://$_"}
	| 'hywt://' /.*/ -> {resource_append_op "hywt://$_"}
	| 'hywws://' /.*/ -> {resource_append_op "hywws://$_"}
	| 'hywwt://' /.*/ -> {resource_append_op "hywwt://$_"}
	| 'hzws://' /.*/ -> {resource_append_op "hzws://$_"}
	| 'hzwt://' /.*/ -> {resource_append_op "hzwt://$_"}
	| 'iaws://' /.*/ -> {resource_append_op "iaws://$_"}
	| 'iawt://' /.*/ -> {resource_append_op "iawt://$_"}
	| 'idws://' /.*/ -> {resource_append_op "idws://$_"}
	| 'idwt://' /.*/ -> {resource_append_op "idwt://$_"}
	| 'iews://' /.*/ -> {resource_append_op "iews://$_"}
	| 'iewt://' /.*/ -> {resource_append_op "iewt://$_"}
	| 'igws://' /.*/ -> {resource_append_op "igws://$_"}
	| 'igwt://' /.*/ -> {resource_append_op "igwt://$_"}
	| 'iiws://' /.*/ -> {resource_append_op "iiws://$_"}
	| 'iiwt://' /.*/ -> {resource_append_op "iiwt://$_"}
	| 'ikws://' /.*/ -> {resource_append_op "ikws://$_"}
	| 'ikwt://' /.*/ -> {resource_append_op "ikwt://$_"}
	| 'ilows://' /.*/ -> {resource_append_op "ilows://$_"}
	| 'ilowt://' /.*/ -> {resource_append_op "ilowt://$_"}
	| 'inhws://' /.*/ -> {resource_append_op "inhws://$_"}
	| 'inhwt://' /.*/ -> {resource_append_op "inhwt://$_"}
	| 'iows://' /.*/ -> {resource_append_op "iows://$_"}
	| 'iowt://' /.*/ -> {resource_append_op "iowt://$_"}
	| 'isws://' /.*/ -> {resource_append_op "isws://$_"}
	| 'iswt://' /.*/ -> {resource_append_op "iswt://$_"}
	| 'itws://' /.*/ -> {resource_append_op "itws://$_"}
	| 'itwt://' /.*/ -> {resource_append_op "itwt://$_"}
	| 'iuws://' /.*/ -> {resource_append_op "iuws://$_"}
	| 'iuwt://' /.*/ -> {resource_append_op "iuwt://$_"}
	| 'jamws://' /.*/ -> {resource_append_op "jamws://$_"}
	| 'jamwt://' /.*/ -> {resource_append_op "jamwt://$_"}
	| 'jaws://' /.*/ -> {resource_append_op "jaws://$_"}
	| 'jawt://' /.*/ -> {resource_append_op "jawt://$_"}
	| 'jbows://' /.*/ -> {resource_append_op "jbows://$_"}
	| 'jbowt://' /.*/ -> {resource_append_op "jbowt://$_"}
	| 'jvws://' /.*/ -> {resource_append_op "jvws://$_"}
	| 'jvwt://' /.*/ -> {resource_append_op "jvwt://$_"}
	| 'kaaws://' /.*/ -> {resource_append_op "kaaws://$_"}
	| 'kaawt://' /.*/ -> {resource_append_op "kaawt://$_"}
	| 'kabws://' /.*/ -> {resource_append_op "kabws://$_"}
	| 'kabwt://' /.*/ -> {resource_append_op "kabwt://$_"}
	| 'kaws://' /.*/ -> {resource_append_op "kaws://$_"}
	| 'kawt://' /.*/ -> {resource_append_op "kawt://$_"}
	| 'kbdws://' /.*/ -> {resource_append_op "kbdws://$_"}
	| 'kbdwt://' /.*/ -> {resource_append_op "kbdwt://$_"}
	| 'kbpws://' /.*/ -> {resource_append_op "kbpws://$_"}
	| 'kbpwt://' /.*/ -> {resource_append_op "kbpwt://$_"}
	| 'kgws://' /.*/ -> {resource_append_op "kgws://$_"}
	| 'kgwt://' /.*/ -> {resource_append_op "kgwt://$_"}
	| 'kiws://' /.*/ -> {resource_append_op "kiws://$_"}
	| 'kiwt://' /.*/ -> {resource_append_op "kiwt://$_"}
	| 'kjws://' /.*/ -> {resource_append_op "kjws://$_"}
	| 'kjwt://' /.*/ -> {resource_append_op "kjwt://$_"}
	| 'kkws://' /.*/ -> {resource_append_op "kkws://$_"}
	| 'kkwt://' /.*/ -> {resource_append_op "kkwt://$_"}
	| 'klws://' /.*/ -> {resource_append_op "klws://$_"}
	| 'klwt://' /.*/ -> {resource_append_op "klwt://$_"}
	| 'kmws://' /.*/ -> {resource_append_op "kmws://$_"}
	| 'kmwt://' /.*/ -> {resource_append_op "kmwt://$_"}
	| 'knws://' /.*/ -> {resource_append_op "knws://$_"}
	| 'knwt://' /.*/ -> {resource_append_op "knwt://$_"}
	| 'koiws://' /.*/ -> {resource_append_op "koiws://$_"}
	| 'koiwt://' /.*/ -> {resource_append_op "koiwt://$_"}
	| 'kows://' /.*/ -> {resource_append_op "kows://$_"}
	| 'kowt://' /.*/ -> {resource_append_op "kowt://$_"}
	| 'krcws://' /.*/ -> {resource_append_op "krcws://$_"}
	| 'krcwt://' /.*/ -> {resource_append_op "krcwt://$_"}
	| 'krws://' /.*/ -> {resource_append_op "krws://$_"}
	| 'krwt://' /.*/ -> {resource_append_op "krwt://$_"}
	| 'kshws://' /.*/ -> {resource_append_op "kshws://$_"}
	| 'kshwt://' /.*/ -> {resource_append_op "kshwt://$_"}
	| 'ksws://' /.*/ -> {resource_append_op "ksws://$_"}
	| 'kswt://' /.*/ -> {resource_append_op "kswt://$_"}
	| 'kuws://' /.*/ -> {resource_append_op "kuws://$_"}
	| 'kuwt://' /.*/ -> {resource_append_op "kuwt://$_"}
	| 'kvws://' /.*/ -> {resource_append_op "kvws://$_"}
	| 'kvwt://' /.*/ -> {resource_append_op "kvwt://$_"}
	| 'kwws://' /.*/ -> {resource_append_op "kwws://$_"}
	| 'kwwt://' /.*/ -> {resource_append_op "kwwt://$_"}
	| 'kyws://' /.*/ -> {resource_append_op "kyws://$_"}
	| 'kywt://' /.*/ -> {resource_append_op "kywt://$_"}
	| 'ladws://' /.*/ -> {resource_append_op "ladws://$_"}
	| 'ladwt://' /.*/ -> {resource_append_op "ladwt://$_"}
	| 'laws://' /.*/ -> {resource_append_op "laws://$_"}
	| 'lawt://' /.*/ -> {resource_append_op "lawt://$_"}
	| 'lbews://' /.*/ -> {resource_append_op "lbews://$_"}
	| 'lbewt://' /.*/ -> {resource_append_op "lbewt://$_"}
	| 'lbws://' /.*/ -> {resource_append_op "lbws://$_"}
	| 'lbwt://' /.*/ -> {resource_append_op "lbwt://$_"}
	| 'lezws://' /.*/ -> {resource_append_op "lezws://$_"}
	| 'lezwt://' /.*/ -> {resource_append_op "lezwt://$_"}
	| 'lfnws://' /.*/ -> {resource_append_op "lfnws://$_"}
	| 'lfnwt://' /.*/ -> {resource_append_op "lfnwt://$_"}
	| 'lgws://' /.*/ -> {resource_append_op "lgws://$_"}
	| 'lgwt://' /.*/ -> {resource_append_op "lgwt://$_"}
	| 'lijws://' /.*/ -> {resource_append_op "lijws://$_"}
	| 'lijwt://' /.*/ -> {resource_append_op "lijwt://$_"}
	| 'liws://' /.*/ -> {resource_append_op "liws://$_"}
	| 'liwt://' /.*/ -> {resource_append_op "liwt://$_"}
	| 'lldws://' /.*/ -> {resource_append_op "lldws://$_"}
	| 'lldwt://' /.*/ -> {resource_append_op "lldwt://$_"}
	| 'lmows://' /.*/ -> {resource_append_op "lmows://$_"}
	| 'lmowt://' /.*/ -> {resource_append_op "lmowt://$_"}
	| 'lnws://' /.*/ -> {resource_append_op "lnws://$_"}
	| 'lnwt://' /.*/ -> {resource_append_op "lnwt://$_"}
	| 'lows://' /.*/ -> {resource_append_op "lows://$_"}
	| 'lowt://' /.*/ -> {resource_append_op "lowt://$_"}
	| 'lrcws://' /.*/ -> {resource_append_op "lrcws://$_"}
	| 'lrcwt://' /.*/ -> {resource_append_op "lrcwt://$_"}
	| 'ltgws://' /.*/ -> {resource_append_op "ltgws://$_"}
	| 'ltgwt://' /.*/ -> {resource_append_op "ltgwt://$_"}
	| 'ltws://' /.*/ -> {resource_append_op "ltws://$_"}
	| 'ltwt://' /.*/ -> {resource_append_op "ltwt://$_"}
	| 'lvws://' /.*/ -> {resource_append_op "lvws://$_"}
	| 'lvwt://' /.*/ -> {resource_append_op "lvwt://$_"}
	| 'maiws://' /.*/ -> {resource_append_op "maiws://$_"}
	| 'maiwt://' /.*/ -> {resource_append_op "maiwt://$_"}
	| 'map-bmsws://' /.*/ -> {resource_append_op "map-bmsws://$_"}
	| 'map-bmswt://' /.*/ -> {resource_append_op "map-bmswt://$_"}
	| 'mdfws://' /.*/ -> {resource_append_op "mdfws://$_"}
	| 'mdfwt://' /.*/ -> {resource_append_op "mdfwt://$_"}
	| 'mgws://' /.*/ -> {resource_append_op "mgws://$_"}
	| 'mgwt://' /.*/ -> {resource_append_op "mgwt://$_"}
	| 'mhrws://' /.*/ -> {resource_append_op "mhrws://$_"}
	| 'mhrwt://' /.*/ -> {resource_append_op "mhrwt://$_"}
	| 'mhws://' /.*/ -> {resource_append_op "mhws://$_"}
	| 'mhwt://' /.*/ -> {resource_append_op "mhwt://$_"}
	| 'minws://' /.*/ -> {resource_append_op "minws://$_"}
	| 'minwt://' /.*/ -> {resource_append_op "minwt://$_"}
	| 'miws://' /.*/ -> {resource_append_op "miws://$_"}
	| 'miwt://' /.*/ -> {resource_append_op "miwt://$_"}
	| 'mkws://' /.*/ -> {resource_append_op "mkws://$_"}
	| 'mkwt://' /.*/ -> {resource_append_op "mkwt://$_"}
	| 'mlws://' /.*/ -> {resource_append_op "mlws://$_"}
	| 'mlwt://' /.*/ -> {resource_append_op "mlwt://$_"}
	| 'mnws://' /.*/ -> {resource_append_op "mnws://$_"}
	| 'mnwt://' /.*/ -> {resource_append_op "mnwt://$_"}
	| 'mnwws://' /.*/ -> {resource_append_op "mnwws://$_"}
	| 'mnwwt://' /.*/ -> {resource_append_op "mnwwt://$_"}
	| 'mrjws://' /.*/ -> {resource_append_op "mrjws://$_"}
	| 'mrjwt://' /.*/ -> {resource_append_op "mrjwt://$_"}
	| 'mrws://' /.*/ -> {resource_append_op "mrws://$_"}
	| 'mrwt://' /.*/ -> {resource_append_op "mrwt://$_"}
	| 'msws://' /.*/ -> {resource_append_op "msws://$_"}
	| 'mswt://' /.*/ -> {resource_append_op "mswt://$_"}
	| 'mtws://' /.*/ -> {resource_append_op "mtws://$_"}
	| 'mtwt://' /.*/ -> {resource_append_op "mtwt://$_"}
	| 'musws://' /.*/ -> {resource_append_op "musws://$_"}
	| 'muswt://' /.*/ -> {resource_append_op "muswt://$_"}
	| 'mwlws://' /.*/ -> {resource_append_op "mwlws://$_"}
	| 'mwlwt://' /.*/ -> {resource_append_op "mwlwt://$_"}
	| 'myvws://' /.*/ -> {resource_append_op "myvws://$_"}
	| 'myvwt://' /.*/ -> {resource_append_op "myvwt://$_"}
	| 'myws://' /.*/ -> {resource_append_op "myws://$_"}
	| 'mywt://' /.*/ -> {resource_append_op "mywt://$_"}
	| 'mznws://' /.*/ -> {resource_append_op "mznws://$_"}
	| 'mznwt://' /.*/ -> {resource_append_op "mznwt://$_"}
	| 'nahws://' /.*/ -> {resource_append_op "nahws://$_"}
	| 'nahwt://' /.*/ -> {resource_append_op "nahwt://$_"}
	| 'napws://' /.*/ -> {resource_append_op "napws://$_"}
	| 'napwt://' /.*/ -> {resource_append_op "napwt://$_"}
	| 'naws://' /.*/ -> {resource_append_op "naws://$_"}
	| 'nawt://' /.*/ -> {resource_append_op "nawt://$_"}
	| 'nds-nlws://' /.*/ -> {resource_append_op "nds-nlws://$_"}
	| 'nds-nlwt://' /.*/ -> {resource_append_op "nds-nlwt://$_"}
	| 'ndsws://' /.*/ -> {resource_append_op "ndsws://$_"}
	| 'ndswt://' /.*/ -> {resource_append_op "ndswt://$_"}
	| 'news://' /.*/ -> {resource_append_op "news://$_"}
	| 'newt://' /.*/ -> {resource_append_op "newt://$_"}
	| 'newws://' /.*/ -> {resource_append_op "newws://$_"}
	| 'newwt://' /.*/ -> {resource_append_op "newwt://$_"}
	| 'ngws://' /.*/ -> {resource_append_op "ngws://$_"}
	| 'ngwt://' /.*/ -> {resource_append_op "ngwt://$_"}
	| 'nlws://' /.*/ -> {resource_append_op "nlws://$_"}
	| 'nlwt://' /.*/ -> {resource_append_op "nlwt://$_"}
	| 'nnws://' /.*/ -> {resource_append_op "nnws://$_"}
	| 'nnwt://' /.*/ -> {resource_append_op "nnwt://$_"}
	| 'novws://' /.*/ -> {resource_append_op "novws://$_"}
	| 'novwt://' /.*/ -> {resource_append_op "novwt://$_"}
	| 'nows://' /.*/ -> {resource_append_op "nows://$_"}
	| 'nowt://' /.*/ -> {resource_append_op "nowt://$_"}
	| 'nqows://' /.*/ -> {resource_append_op "nqows://$_"}
	| 'nqowt://' /.*/ -> {resource_append_op "nqowt://$_"}
	| 'nrmws://' /.*/ -> {resource_append_op "nrmws://$_"}
	| 'nrmwt://' /.*/ -> {resource_append_op "nrmwt://$_"}
	| 'nsows://' /.*/ -> {resource_append_op "nsows://$_"}
	| 'nsowt://' /.*/ -> {resource_append_op "nsowt://$_"}
	| 'nvws://' /.*/ -> {resource_append_op "nvws://$_"}
	| 'nvwt://' /.*/ -> {resource_append_op "nvwt://$_"}
	| 'nyws://' /.*/ -> {resource_append_op "nyws://$_"}
	| 'nywt://' /.*/ -> {resource_append_op "nywt://$_"}
	| 'ocws://' /.*/ -> {resource_append_op "ocws://$_"}
	| 'ocwt://' /.*/ -> {resource_append_op "ocwt://$_"}
	| 'olows://' /.*/ -> {resource_append_op "olows://$_"}
	| 'olowt://' /.*/ -> {resource_append_op "olowt://$_"}
	| 'omws://' /.*/ -> {resource_append_op "omws://$_"}
	| 'omwt://' /.*/ -> {resource_append_op "omwt://$_"}
	| 'orws://' /.*/ -> {resource_append_op "orws://$_"}
	| 'orwt://' /.*/ -> {resource_append_op "orwt://$_"}
	| 'osws://' /.*/ -> {resource_append_op "osws://$_"}
	| 'oswt://' /.*/ -> {resource_append_op "oswt://$_"}
	| 'pagws://' /.*/ -> {resource_append_op "pagws://$_"}
	| 'pagwt://' /.*/ -> {resource_append_op "pagwt://$_"}
	| 'pamws://' /.*/ -> {resource_append_op "pamws://$_"}
	| 'pamwt://' /.*/ -> {resource_append_op "pamwt://$_"}
	| 'papws://' /.*/ -> {resource_append_op "papws://$_"}
	| 'papwt://' /.*/ -> {resource_append_op "papwt://$_"}
	| 'paws://' /.*/ -> {resource_append_op "paws://$_"}
	| 'pawt://' /.*/ -> {resource_append_op "pawt://$_"}
	| 'pcdws://' /.*/ -> {resource_append_op "pcdws://$_"}
	| 'pcdwt://' /.*/ -> {resource_append_op "pcdwt://$_"}
	| 'pdcws://' /.*/ -> {resource_append_op "pdcws://$_"}
	| 'pdcwt://' /.*/ -> {resource_append_op "pdcwt://$_"}
	| 'pflws://' /.*/ -> {resource_append_op "pflws://$_"}
	| 'pflwt://' /.*/ -> {resource_append_op "pflwt://$_"}
	| 'pihws://' /.*/ -> {resource_append_op "pihws://$_"}
	| 'pihwt://' /.*/ -> {resource_append_op "pihwt://$_"}
	| 'piws://' /.*/ -> {resource_append_op "piws://$_"}
	| 'piwt://' /.*/ -> {resource_append_op "piwt://$_"}
	| 'plws://' /.*/ -> {resource_append_op "plws://$_"}
	| 'plwt://' /.*/ -> {resource_append_op "plwt://$_"}
	| 'pmsws://' /.*/ -> {resource_append_op "pmsws://$_"}
	| 'pmswt://' /.*/ -> {resource_append_op "pmswt://$_"}
	| 'pnbws://' /.*/ -> {resource_append_op "pnbws://$_"}
	| 'pnbwt://' /.*/ -> {resource_append_op "pnbwt://$_"}
	| 'pntws://' /.*/ -> {resource_append_op "pntws://$_"}
	| 'pntwt://' /.*/ -> {resource_append_op "pntwt://$_"}
	| 'psws://' /.*/ -> {resource_append_op "psws://$_"}
	| 'pswt://' /.*/ -> {resource_append_op "pswt://$_"}
	| 'ptws://' /.*/ -> {resource_append_op "ptws://$_"}
	| 'ptwt://' /.*/ -> {resource_append_op "ptwt://$_"}
	| 'quws://' /.*/ -> {resource_append_op "quws://$_"}
	| 'quwt://' /.*/ -> {resource_append_op "quwt://$_"}
	| 'rmws://' /.*/ -> {resource_append_op "rmws://$_"}
	| 'rmwt://' /.*/ -> {resource_append_op "rmwt://$_"}
	| 'rmyws://' /.*/ -> {resource_append_op "rmyws://$_"}
	| 'rmywt://' /.*/ -> {resource_append_op "rmywt://$_"}
	| 'rnws://' /.*/ -> {resource_append_op "rnws://$_"}
	| 'rnwt://' /.*/ -> {resource_append_op "rnwt://$_"}
	| 'roa-rupws://' /.*/ -> {resource_append_op "roa-rupws://$_"}
	| 'roa-rupwt://' /.*/ -> {resource_append_op "roa-rupwt://$_"}
	| 'roa-taraws://' /.*/ -> {resource_append_op "roa-taraws://$_"}
	| 'roa-tarawt://' /.*/ -> {resource_append_op "roa-tarawt://$_"}
	| 'rows://' /.*/ -> {resource_append_op "rows://$_"}
	| 'rowt://' /.*/ -> {resource_append_op "rowt://$_"}
	| 'ruews://' /.*/ -> {resource_append_op "ruews://$_"}
	| 'ruewt://' /.*/ -> {resource_append_op "ruewt://$_"}
	| 'ruws://' /.*/ -> {resource_append_op "ruws://$_"}
	| 'ruwt://' /.*/ -> {resource_append_op "ruwt://$_"}
	| 'rwws://' /.*/ -> {resource_append_op "rwws://$_"}
	| 'rwwt://' /.*/ -> {resource_append_op "rwwt://$_"}
	| 's3cmd://' /.*/ -> {resource_append_op "s3cmd://$_"}
	| 'sahws://' /.*/ -> {resource_append_op "sahws://$_"}
	| 'sahwt://' /.*/ -> {resource_append_op "sahwt://$_"}
	| 'satws://' /.*/ -> {resource_append_op "satws://$_"}
	| 'satwt://' /.*/ -> {resource_append_op "satwt://$_"}
	| 'saws://' /.*/ -> {resource_append_op "saws://$_"}
	| 'sawt://' /.*/ -> {resource_append_op "sawt://$_"}
	| 'scnws://' /.*/ -> {resource_append_op "scnws://$_"}
	| 'scnwt://' /.*/ -> {resource_append_op "scnwt://$_"}
	| 'scows://' /.*/ -> {resource_append_op "scows://$_"}
	| 'scowt://' /.*/ -> {resource_append_op "scowt://$_"}
	| 'scws://' /.*/ -> {resource_append_op "scws://$_"}
	| 'scwt://' /.*/ -> {resource_append_op "scwt://$_"}
	| 'sdws://' /.*/ -> {resource_append_op "sdws://$_"}
	| 'sdwt://' /.*/ -> {resource_append_op "sdwt://$_"}
	| 'sews://' /.*/ -> {resource_append_op "sews://$_"}
	| 'sewt://' /.*/ -> {resource_append_op "sewt://$_"}
	| 'sftp://' /.*/ -> {resource_append_op "sftp://$_"}
	| 'sgws://' /.*/ -> {resource_append_op "sgws://$_"}
	| 'sgwt://' /.*/ -> {resource_append_op "sgwt://$_"}
	| 'shnws://' /.*/ -> {resource_append_op "shnws://$_"}
	| 'shnwt://' /.*/ -> {resource_append_op "shnwt://$_"}
	| 'shws://' /.*/ -> {resource_append_op "shws://$_"}
	| 'shwt://' /.*/ -> {resource_append_op "shwt://$_"}
	| 'simplews://' /.*/ -> {resource_append_op "simplews://$_"}
	| 'simplewt://' /.*/ -> {resource_append_op "simplewt://$_"}
	| 'siws://' /.*/ -> {resource_append_op "siws://$_"}
	| 'siwt://' /.*/ -> {resource_append_op "siwt://$_"}
	| 'skws://' /.*/ -> {resource_append_op "skws://$_"}
	| 'skwt://' /.*/ -> {resource_append_op "skwt://$_"}
	| 'slws://' /.*/ -> {resource_append_op "slws://$_"}
	| 'slwt://' /.*/ -> {resource_append_op "slwt://$_"}
	| 'smws://' /.*/ -> {resource_append_op "smws://$_"}
	| 'smwt://' /.*/ -> {resource_append_op "smwt://$_"}
	| 'snws://' /.*/ -> {resource_append_op "snws://$_"}
	| 'snwt://' /.*/ -> {resource_append_op "snwt://$_"}
	| 'solr://' /.*/ -> {resource_append_op "solr://$_"}
	| 'sows://' /.*/ -> {resource_append_op "sows://$_"}
	| 'sowt://' /.*/ -> {resource_append_op "sowt://$_"}
	| 'sqlite://' /.*/ -> {resource_append_op "sqlite://$_"}
	| 'sqliteq://' /.*/ -> {resource_append_op "sqliteq://$_"}
	| 'sqlites://' /.*/ -> {resource_append_op "sqlites://$_"}
	| 'sqlitet://' /.*/ -> {resource_append_op "sqlitet://$_"}
	| 'sqws://' /.*/ -> {resource_append_op "sqws://$_"}
	| 'sqwt://' /.*/ -> {resource_append_op "sqwt://$_"}
	| 'srnws://' /.*/ -> {resource_append_op "srnws://$_"}
	| 'srnwt://' /.*/ -> {resource_append_op "srnwt://$_"}
	| 'srws://' /.*/ -> {resource_append_op "srws://$_"}
	| 'srwt://' /.*/ -> {resource_append_op "srwt://$_"}
	| 'ssws://' /.*/ -> {resource_append_op "ssws://$_"}
	| 'sswt://' /.*/ -> {resource_append_op "sswt://$_"}
	| 'stqws://' /.*/ -> {resource_append_op "stqws://$_"}
	| 'stqwt://' /.*/ -> {resource_append_op "stqwt://$_"}
	| 'stws://' /.*/ -> {resource_append_op "stws://$_"}
	| 'stwt://' /.*/ -> {resource_append_op "stwt://$_"}
	| 'suws://' /.*/ -> {resource_append_op "suws://$_"}
	| 'suwt://' /.*/ -> {resource_append_op "suwt://$_"}
	| 'svws://' /.*/ -> {resource_append_op "svws://$_"}
	| 'svwt://' /.*/ -> {resource_append_op "svwt://$_"}
	| 'swws://' /.*/ -> {resource_append_op "swws://$_"}
	| 'swwt://' /.*/ -> {resource_append_op "swwt://$_"}
	| 'szlws://' /.*/ -> {resource_append_op "szlws://$_"}
	| 'szlwt://' /.*/ -> {resource_append_op "szlwt://$_"}
	| 'szyws://' /.*/ -> {resource_append_op "szyws://$_"}
	| 'szywt://' /.*/ -> {resource_append_op "szywt://$_"}
	| 'tar://' /.*/ -> {resource_append_op "tar://$_"}
	| 'tarentry://' /.*/ -> {resource_append_op "tarentry://$_"}
	| 'taws://' /.*/ -> {resource_append_op "taws://$_"}
	| 'tawt://' /.*/ -> {resource_append_op "tawt://$_"}
	| 'tcyws://' /.*/ -> {resource_append_op "tcyws://$_"}
	| 'tcywt://' /.*/ -> {resource_append_op "tcywt://$_"}
	| 'tetws://' /.*/ -> {resource_append_op "tetws://$_"}
	| 'tetwt://' /.*/ -> {resource_append_op "tetwt://$_"}
	| 'tews://' /.*/ -> {resource_append_op "tews://$_"}
	| 'tewt://' /.*/ -> {resource_append_op "tewt://$_"}
	| 'tgws://' /.*/ -> {resource_append_op "tgws://$_"}
	| 'tgwt://' /.*/ -> {resource_append_op "tgwt://$_"}
	| 'thws://' /.*/ -> {resource_append_op "thws://$_"}
	| 'thwt://' /.*/ -> {resource_append_op "thwt://$_"}
	| 'tiws://' /.*/ -> {resource_append_op "tiws://$_"}
	| 'tiwt://' /.*/ -> {resource_append_op "tiwt://$_"}
	| 'tkws://' /.*/ -> {resource_append_op "tkws://$_"}
	| 'tkwt://' /.*/ -> {resource_append_op "tkwt://$_"}
	| 'tlws://' /.*/ -> {resource_append_op "tlws://$_"}
	| 'tlwt://' /.*/ -> {resource_append_op "tlwt://$_"}
	| 'tnws://' /.*/ -> {resource_append_op "tnws://$_"}
	| 'tnwt://' /.*/ -> {resource_append_op "tnwt://$_"}
	| 'tows://' /.*/ -> {resource_append_op "tows://$_"}
	| 'towt://' /.*/ -> {resource_append_op "towt://$_"}
	| 'tpiws://' /.*/ -> {resource_append_op "tpiws://$_"}
	| 'tpiwt://' /.*/ -> {resource_append_op "tpiwt://$_"}
	| 'trws://' /.*/ -> {resource_append_op "trws://$_"}
	| 'trwt://' /.*/ -> {resource_append_op "trwt://$_"}
	| 'tsws://' /.*/ -> {resource_append_op "tsws://$_"}
	| 'tswt://' /.*/ -> {resource_append_op "tswt://$_"}
	| 'ttws://' /.*/ -> {resource_append_op "ttws://$_"}
	| 'ttwt://' /.*/ -> {resource_append_op "ttwt://$_"}
	| 'tumws://' /.*/ -> {resource_append_op "tumws://$_"}
	| 'tumwt://' /.*/ -> {resource_append_op "tumwt://$_"}
	| 'twws://' /.*/ -> {resource_append_op "twws://$_"}
	| 'twwt://' /.*/ -> {resource_append_op "twwt://$_"}
	| 'tyvws://' /.*/ -> {resource_append_op "tyvws://$_"}
	| 'tyvwt://' /.*/ -> {resource_append_op "tyvwt://$_"}
	| 'tyws://' /.*/ -> {resource_append_op "tyws://$_"}
	| 'tywt://' /.*/ -> {resource_append_op "tywt://$_"}
	| 'udmws://' /.*/ -> {resource_append_op "udmws://$_"}
	| 'udmwt://' /.*/ -> {resource_append_op "udmwt://$_"}
	| 'ugws://' /.*/ -> {resource_append_op "ugws://$_"}
	| 'ugwt://' /.*/ -> {resource_append_op "ugwt://$_"}
	| 'ukws://' /.*/ -> {resource_append_op "ukws://$_"}
	| 'ukwt://' /.*/ -> {resource_append_op "ukwt://$_"}
	| 'urws://' /.*/ -> {resource_append_op "urws://$_"}
	| 'urwt://' /.*/ -> {resource_append_op "urwt://$_"}
	| 'uzws://' /.*/ -> {resource_append_op "uzws://$_"}
	| 'uzwt://' /.*/ -> {resource_append_op "uzwt://$_"}
	| 'vecws://' /.*/ -> {resource_append_op "vecws://$_"}
	| 'vecwt://' /.*/ -> {resource_append_op "vecwt://$_"}
	| 'vepws://' /.*/ -> {resource_append_op "vepws://$_"}
	| 'vepwt://' /.*/ -> {resource_append_op "vepwt://$_"}
	| 'vews://' /.*/ -> {resource_append_op "vews://$_"}
	| 'vewt://' /.*/ -> {resource_append_op "vewt://$_"}
	| 'viws://' /.*/ -> {resource_append_op "viws://$_"}
	| 'viwt://' /.*/ -> {resource_append_op "viwt://$_"}
	| 'vlsws://' /.*/ -> {resource_append_op "vlsws://$_"}
	| 'vlswt://' /.*/ -> {resource_append_op "vlswt://$_"}
	| 'vows://' /.*/ -> {resource_append_op "vows://$_"}
	| 'vowt://' /.*/ -> {resource_append_op "vowt://$_"}
	| 'warws://' /.*/ -> {resource_append_op "warws://$_"}
	| 'warwt://' /.*/ -> {resource_append_op "warwt://$_"}
	| 'waws://' /.*/ -> {resource_append_op "waws://$_"}
	| 'wawt://' /.*/ -> {resource_append_op "wawt://$_"}
	| 'wiki://' /.*/ -> {resource_append_op "wiki://$_"}
	| 'wows://' /.*/ -> {resource_append_op "wows://$_"}
	| 'wowt://' /.*/ -> {resource_append_op "wowt://$_"}
	| 'wuuws://' /.*/ -> {resource_append_op "wuuws://$_"}
	| 'wuuwt://' /.*/ -> {resource_append_op "wuuwt://$_"}
	| 'xalws://' /.*/ -> {resource_append_op "xalws://$_"}
	| 'xalwt://' /.*/ -> {resource_append_op "xalwt://$_"}
	| 'xhws://' /.*/ -> {resource_append_op "xhws://$_"}
	| 'xhwt://' /.*/ -> {resource_append_op "xhwt://$_"}
	| 'xlsx://' /.*/ -> {resource_append_op "xlsx://$_"}
	| 'xlsxsheet://' /.*/ -> {resource_append_op "xlsxsheet://$_"}
	| 'xmfws://' /.*/ -> {resource_append_op "xmfws://$_"}
	| 'xmfwt://' /.*/ -> {resource_append_op "xmfwt://$_"}
	| 'yiws://' /.*/ -> {resource_append_op "yiws://$_"}
	| 'yiwt://' /.*/ -> {resource_append_op "yiwt://$_"}
	| 'yows://' /.*/ -> {resource_append_op "yows://$_"}
	| 'yowt://' /.*/ -> {resource_append_op "yowt://$_"}
	| 'yt://' /.*/ -> {resource_append_op "yt://$_"}
	| 'zaws://' /.*/ -> {resource_append_op "zaws://$_"}
	| 'zawt://' /.*/ -> {resource_append_op "zawt://$_"}
	| 'zeaws://' /.*/ -> {resource_append_op "zeaws://$_"}
	| 'zeawt://' /.*/ -> {resource_append_op "zeawt://$_"}
	| 'zh-classicalws://' /.*/ -> {resource_append_op "zh-classicalws://$_"}
	| 'zh-classicalwt://' /.*/ -> {resource_append_op "zh-classicalwt://$_"}
	| 'zh-min-nanws://' /.*/ -> {resource_append_op "zh-min-nanws://$_"}
	| 'zh-min-nanwt://' /.*/ -> {resource_append_op "zh-min-nanwt://$_"}
	| 'zh-yuews://' /.*/ -> {resource_append_op "zh-yuews://$_"}
	| 'zh-yuewt://' /.*/ -> {resource_append_op "zh-yuewt://$_"}
	| 'zhws://' /.*/ -> {resource_append_op "zhws://$_"}
	| 'zhwt://' /.*/ -> {resource_append_op "zhwt://$_"}
	| 'zip://' /.*/ -> {resource_append_op "zip://$_"}
	| 'zipentry://' /.*/ -> {resource_append_op "zipentry://$_"}
	| 'zuws://' /.*/ -> {resource_append_op "zuws://$_"}
	| 'zuwt://' /.*/ -> {resource_append_op "zuwt://$_"}
	)

# PARSER dsp/sparkprofile

## DEFINITION
	(
	| 'L' <pyspark_rdd> -> {[pyspark_local_text_op($_),
	                               file_read_op,
	                               row_match_op '/part-']}
	| 'dev/compile' <pyspark_rdd> -> {pyspark_preview_op $_}
	)

# PARSER dsp/splitalt

## DEFINITION
	(
	| '/' <regex> -> {split_regex_op $_}
	| ':' /./ -> {split_chr_op   $_}
	| 'C' '' -> {split_chr_op   ','}
	| 'D' '' -> {split_chr_op   '\/'}
	| 'P' '' -> {split_chr_op   '|'}
	| 'S' '' -> {split_regex_op '\s+'}
	| 'V' '' -> {split_proper_csv_op}
	| 'W' '' -> {split_regex_op '[^\w\n]+'}
	| 'm' (
	    '/'
	    <regex> -> {scan_regex_op $_}
	  ) -> {$$_[1]}
	)

# PARSER dsp/sqlprofile

## DEFINITION
	(
	| 'dev/compile' <sql_query> -> {sql_preview_op($_[0])}
	)

# PARSER filename
	The name of an existing file

## DEFINITION
	(
	| <computed>
	| /file://(.+)/
	| /\.?/(?:[^/]|$)[^]]*/
	| /[^][]+/ such that {-e}
	)

# PARSER float

## DEFINITION
	/-?(?:[\d_]+(?:\.[\d_]*)?|[\d_]*\.[\d_]+)(?:[eE][-+]?[\d_]+)?/ such that {length} -> {0 + $_}

# PARSER fn_bindings

## DEFINITION
	(
	  (
	    /(?^:[^:=]+)/
	    <empty>?
	  ) -> {$$_[0]}*
	  (
	    /(?^::)/
	    <empty>?
	  ) -> {$$_[0]}
	) -> {$$_[0]}

# PARSER fn_expander

## DEFINITION
	<core parser {
	  
	        my ($self, @xs) = @_;
	        my (undef, $context, $formals, $positions, $expansion) = @$self;
	        my ($parsed_formals, @rest) = parse pn(1, popt pempty, $formals), @xs;
	        return () unless defined $parsed_formals;
	  
	        my %args;
	        $args{$_} = $$parsed_formals[$$positions{$_}] for keys %$positions;
	        parse parser "$context/op",
	              evaluate_fn_expansion(%args, @$expansion), @rest;
	      
	}>

# PARSER generic_code
	Counts brackets outside quoted strings, which in our case are '' and "".
	Doesn't look for regular expressions because these vary by language; but this
	parser should be able to handle most straightforward languages with quoted
	string literals and backslash escapes.

## DEFINITION
	<core parser {
	  my ($self, $code, @xs) = @_;
	      return ($code, '', @xs) unless $code =~ /\]$/;
	      (my $tcode = $code) =~ s/"([^"\\]+|\\.)"|'([^'\\]+|\\.)'//g;
	      my $balance = length(sgr $tcode, qr/[^[]/, '') - length(sgr $tcode, qr/[^]]/, '');
	      $balance ? (substr($code, 0, $balance), substr($code, $balance), @xs)
	               : ($code, '', @xs)
	}>

# PARSER gnuplot_code

## DEFINITION
	(
	  <dsp/gnuplot_code_prefixalt>*
	  <generic_code>?
	) -> {join "", map ref($_) ? @$_ : $_, @$_}

# PARSER gnuplot_colspec

## DEFINITION
	(
	| <colspec1>
	| ':' -> {undef}
	)

# PARSER gnuplot_terminal_size

## DEFINITION
	(
	  <integer>
	  /[x,]/
	  <integer>
	) -> {[@$_[0,2]]}? -> {defined $_ ? "size " . join ',', @$_ : ""}

# PARSER hadoop_streaming_lambda

## DEFINITION
	(
	| (
	    /_/
	    <empty>?
	  ) -> {$$_[0]} -> {undef}
	| (
	    /:/
	    <empty>?
	  ) -> {$$_[0]} -> {[]}
	| </qfn>
	)

# PARSER id_text

## DEFINITION
	(
	| <super_brackets> -> {join "\t", @$_}
	| <multiword_ws> -> {join "\t", @$_}
	| <multiword> -> {join "\t", @$_}
	| /[^][]+/
	)

# PARSER image_command

## DEFINITION
	(
	| ':' -> {''}
	| <shell_command>
	)

# PARSER integer

## DEFINITION
	(
	| <neval> -> {int}
	| /E(-?[\d_]+)/ -> {10 ** $_}
	| /B([\d_]+)/ -> {1 << $_}
	| /x[_0-9a-fA-F]+/ -> {0 + "0$_"}
	| /-?[1-9][\d_]*(?:[eE][\d_]+)?/ -> {0 + $_}
	| '0'
	)

# PARSER jitter_bias

## DEFINITION
	<number>? -> {dor $_, 0}

# PARSER jitter_mag

## DEFINITION
	(
	| /,/ -> {0.9}
	| <number>?
	) -> {$_ || 1}

# PARSER let_binding

## DEFINITION
	(
	  /(?^:[^:=]+)/
	  '='
	  (
	    /[\s\S]+/
	    <empty>?
	  ) -> {$$_[0]}
	) -> {[@$_[0,2]]}

# PARSER let_bindings

## DEFINITION
	(
	  <let_binding>*
	  (
	    /(?^::)/
	    <empty>?
	  ) -> {$$_[0]}
	) -> {$$_[0]}

# PARSER lispcode

## DEFINITION
	(
	  /.*[^]]+/
	  <empty>?
	) -> {$$_[0]}

# PARSER log_base

## DEFINITION
	<number>? -> {$_ || exp 1}

# PARSER media_format_spec

## DEFINITION
	(
	  /\w+/
	  (
	    ///
	    /\w+/
	  ) -> {$$_[1]}?
	  (
	    ///
	    /\w+/
	  ) -> {$$_[1]}?
	)

# PARSER multiword
	A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
	metacharacters within the arguments won't be expanded). If you use this form,
	no ARGV entry can end in a closing bracket; otherwise ni will assume you wanted
	to close the list.

## DEFINITION
	(
	  /\[/
	  (
	    /[\s\S]*[^]]/
	    <empty>?
	  ) -> {$$_[0]}+
	  /\]/
	) -> {$$_[1]}

# PARSER multiword_ws
	A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
	metacharacters within the arguments won't be expanded). Whitespace is required
	around both brackets.

## DEFINITION
	(
	  (
	    /\[$/
	    <empty>?
	  ) -> {$$_[0]}
	  !/\]$/+
	  /\]$/
	) -> {$$_[1]}

# PARSER nefilelist

## DEFINITION
	(
	| <super_brackets>
	| <multiword_ws>
	| <multiword>
	)

# PARSER nefilename
	The name of a possibly-nonexisting file

## DEFINITION
	(
	| <filename>
	| /[^][]+/
	)

# PARSER neval
	An expression evaluated by Perl; e.g. =3+4 for 7

## DEFINITION
	/=([^]=]+)/ -> {eval}

# PARSER number

## DEFINITION
	(
	| <neval>
	| <float>
	| <integer>
	)

# PARSER paltr

## DEFINITION
	<core parser {
	  my ($self, @xs, @ps, @r) = @_;
	        @r = parse $_, @xs and return @r for @ps = @{parser $$self[1]}; ()
	}>

# PARSER pcond

## DEFINITION
	<core parser {
	  my ($self, @is) = @_;
	        my (undef, $f, $p) = @$self;
	        $f = fn $f;
	        my @xs = parse $p, @is; @xs && &$f($_ = $xs[0]) ? @xs : ()
	}>

# PARSER pdspr

## DEFINITION
	<core parser {
	  my ($self, $x, @xs, $k, @ys, %ls, $c) = @_;
	        my (undef, $ps) = @$self;
	        return () unless defined $x;
	        ++$ls{length $_} for keys %$ps;
	        for my $l (sort {$b <=> $a} keys %ls) {
	          return (@ys = parse $$ps{$c}, substr($x, $l), @xs) ? @ys : ()
	          if exists $$ps{$c = substr $x, 0, $l} and $l <= length $x;
	        }
	        ()
	}>

# PARSER pempty

## DEFINITION
	<core parser {
	  defined $_[1] && length $_[1] ? () : (0, @_[2..$#_])
	}>

# PARSER pend

## DEFINITION
	<core parser {
	  @_ > 1                        ? () : (0)
	}>

# PARSER perl_asserter_code

## DEFINITION
	<plcode ni::perl_asserter>

# PARSER perl_cell_transform_code

## DEFINITION
	<plcode ni::perl_mapper>

# PARSER perl_grepper_code

## DEFINITION
	<plcode ni::perl_grepper>

# PARSER perl_mapper_code

## DEFINITION
	<plcode ni::perl_mapper>

# PARSER pk

## DEFINITION
	<core parser {
	  (${$_[0]}[1], @_[1..$#_])
	}>

# PARSER plcode

## DEFINITION
	<core parser {
	  
	    my ($self, $code, @xs) = @_;
	    die <<EOF if $code =~ /\\r?\n/;
	  ni: you have a trailing backslash in the perl code "$code", which perl will
	      interpret as a reference operator and things will go horribly wrong. If,
	      for some reason, you really do want a trailing backslash, you can bypass
	      this error by putting a space or a comment after it.
	  
	      # this is why you're seeing this message (you don't want the backslash):
	      p'foo \\
	        bar'
	  
	      # this is the correct way to write it:
	      p'foo
	        bar'
	  EOF
	  
	    return $_[1], '', @_[2..$#_] unless $code =~ /\]$/;
	    my $safecode      = $code;
	    my $begin_warning = $safecode =~ s/BEGIN/ END /g;
	    my $codegen       = $$self[1];
	    my $status        = 0;
	    my $x             = '';
	    $x .= ']' while $status = syntax_check 'perl -c -', &$codegen($safecode)
	                    and ($safecode =~ s/\]$//, $code =~ s/\]$//);
	  
	    die <<EOF if $status;
	  ni: failed to get closing bracket count for perl code "$code$x", possibly
	      because BEGIN-block metaprogramming is disabled when ni tries to figure
	      this out. To avoid this, make sure the shell argument containing your code
	      ends with something that isn't a closing bracket; e.g:
	  
	      p'[[some code]]'            # this may fail due to bracket inference
	      p'[[some code]] '           # this works by bypassing it
	      [p'[some code] ' ]          # this works for ni lambdas
	  EOF
	  
	    ($code, $x, @xs);
	}>

# PARSER pmap

## DEFINITION
	<core parser {
	  my ($self, @is) = @_;
	        my (undef, $f, $p) = @$self;
	        $f = fn $f;
	        my @xs = parse $p, @is; @xs ? (&$f($_ = $xs[0]), @xs[1..$#xs]) : ()
	}>

# PARSER pnone

## DEFINITION
	<core parser {
	  (undef,       @_[1..$#_])
	}>

# PARSER pnx

## DEFINITION
	<core parser {
	  my ($self, $x, @xs) = @_;
	        !defined $x || $x =~ /^(?:$$self[1])/ ? () : ($x, @xs)
	}>

# PARSER popt

## DEFINITION
	<core parser {
	  my ($self, @is) = @_;
	        my @xs = parse $$self[1], @is; @xs ? @xs : (undef, @is)
	}>

# PARSER prep

## DEFINITION
	<core parser {
	  my ($self, @is, @c, @r) = @_;
	        my (undef, $p, $n) = (@$self, 0);
	        push @r, $_ while ($_, @is) = parse $p, (@c = @is);
	        @r >= $n ? (\@r, @c) : ()
	}>

# PARSER prx

## DEFINITION
	<core parser {
	  my ($self, $x, @xs) = @_;
	        defined $x && $x =~ s/^($$self[1])// ? (dor($2, $1), $x, @xs) : ()
	}>

# PARSER pseq

## DEFINITION
	<core parser {
	  my ($self, @is, $x, @xs, @ys) = @_;
	        my (undef, @ps) = @$self;
	        (($x, @is) = parse $_, @is) ? push @xs, $x : return () for @ps;
	        (\@xs, @is)
	}>

# PARSER pstr

## DEFINITION
	<core parser {
	  my ($self, $x, @xs) = @_;
	        defined $x && index($x, $$self[1]) == 0
	          ? ($$self[1], substr($x, length $$self[1]), @xs)
	          : ()
	}>

# PARSER pycode

## DEFINITION
	<generic_code> -> {pydent $_}

# PARSER pyspark/lambda
	A bracketed lambda function in context 'pyspark'

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  <pyspark/series>
	  ']'
	) -> {$$_[1]}

# PARSER pyspark/op
	A single operator in the context 'pyspark'

## DEFINITION
	(
	| <pyspark/short>
	)

# PARSER pyspark/qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| <pyspark/lambda>
	| <pyspark/suffix>
	)

# PARSER pyspark/series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  <pyspark/op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER pyspark/short
	Dispatch table for short options in context 'pyspark'

## DEFINITION
	(
	| '*' <pyspark_rdd> -> {gen "%v.intersect($_)"}
	| '+' <pyspark_rdd> -> {gen "%v.union($_)"}
	| 'e' /([^]]+)/ -> {TODO(); gen "%v.pipe(" . pyquote($_) . ")"}
	| 'g' <'', evaluate as <opaque code reference>>
	| 'm' <pyspark_fn> -> {gen "%v.map(lambda x: $_)"}
	| 'n' <integer> -> {gen "%v.union(sc.parallelize(range(1, 1+$_)))"}
	| 'n0' <integer> -> {gen "%v.union(sc.parallelize(range($_)))"}
	| 'r' (
	  | <integer> -> {gen "%v.sample(False, $_)"}
	  | /\.(\d+)/ -> {gen "%v.takeSample(False, $_)"}
	  | <pyspark_fn> -> {gen "%v.filter($_)"}
	  )
	| 'u' <'', evaluate as <opaque code reference>>
	)

# PARSER pyspark/suffix
	A string of operators unbroken by whitespace

## DEFINITION
	<pyspark/op>*

# PARSER pyspark_fn

## DEFINITION
	<pycode> -> {pyspark_create_lambda $_}

# PARSER pyspark_rdd

## DEFINITION
	<pyspark/qfn> -> {pyspark_compile 'input', @$_}

# PARSER quant_spec

## DEFINITION
	<number>? -> {$_ || 1}

# PARSER rbcode

## DEFINITION
	<core parser {
	  
	    return $_[1], '', @_[2..$#_] unless $_[1] =~ /\]$/;
	    my ($self, $code, @xs) = @_;
	    my ($x, $status) = ('', 0);
	    $x .= ']' while $status = syntax_check 'ruby -c -', $code and $code =~ s/\]$//;
	    die <<EOF if $status;
	  ni: failed to get closing bracket count for ruby code "$code$x"; this means
	      your code has a syntax error.
	  EOF
	    ($code, $x, @xs);
	}>

# PARSER regex
	Regular expression, delimited by slashes

## DEFINITION
	/(?^:^(?:[^\\/]+|\\.)*/)/ -> {s/\/$//; $_}

# PARSER shell_arg

## DEFINITION
	(
	| <super_brackets> -> {shell_quote @$_}
	| <multiword_ws> -> {shell_quote @$_}
	| <multiword> -> {shell_quote @$_}
	| /[^][]+/ -> {shell_quote $_}
	)

# PARSER shell_command
	A quoted or bracketed shell command

## DEFINITION
	(
	| <super_brackets> -> {shell_quote @$_}
	| <multiword_ws> -> {shell_quote @$_}
	| <multiword> -> {shell_quote @$_}
	| /[^][]+/
	)

# PARSER sortspec

## DEFINITION
	(
	  <colspec1>
	  /[-gn]+/?
	)*

# PARSER sql/lambda
	A bracketed lambda function in context 'sql'

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  <sql/series>
	  ']'
	) -> {$$_[1]}

# PARSER sql/op
	A single operator in the context 'sql'

## DEFINITION
	(
	| <sql/short>
	)

# PARSER sql/qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| <sql/lambda>
	| <sql/suffix>
	)

# PARSER sql/series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  <sql/op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER sql/short
	Dispatch table for short options in context 'sql'

## DEFINITION
	(
	| '*' <sql_query> -> {['intersect',  $_]}
	| '+' <sql_query> -> {['union',      $_]}
	| '-' <sql_query> -> {['difference', $_]}
	| 'O' <sqlcode> -> {['order_by', "$_ DESC"]}
	| 'g' <sqlcode> -> {['order_by', $_]}
	| 'j' (
	  | (
	      'L'
	      <sql_query>
	    ) -> {$$_[1]} -> {['ljoin', $_]}
	  | (
	      'R'
	      <sql_query>
	    ) -> {$$_[1]} -> {['rjoin', $_]}
	  | (
	      'N'
	      <sql_query>
	    ) -> {$$_[1]} -> {['njoin', $_]}
	  | <sql_query> -> {['ijoin', $_]}
	  )
	| 'm' <sqlcode> -> {['map', $_]}
	| 'o' <sqlcode> -> {['order_by', "$_ ASC"]}
	| 'r' (
	  | <integer> -> {['take',   $_]}
	  | <sqlcode> -> {['filter', $_]}
	  )
	| 'u' <'', evaluate as [uniq]>
	)

# PARSER sql/suffix
	A string of operators unbroken by whitespace

## DEFINITION
	<sql/op>*

# PARSER sql_query

## DEFINITION
	(
	  <sql_table>
	  (
	  | <sql/lambda>
	  | <sql/suffix>
	  )?
	) -> {sql_compile $$_[0], @{$$_[1]}}

# PARSER sql_table

## DEFINITION
	(
	  /^[^][]*/
	  <empty>?
	) -> {$$_[0]} -> {sqlgen $_}

# PARSER sqlcode

## DEFINITION
	<generic_code>

# PARSER ssh_host

## DEFINITION
	/[^][/,]+/

# PARSER ssh_host_full

## DEFINITION
	(
	| (
	    <ssh_host> -> {[$_]}
	    <empty>?
	  ) -> {$$_[0]}
	| (
	    <multiword>
	    <empty>?
	  ) -> {$$_[0]}
	)

# PARSER super_brackets

## DEFINITION
	<core parser {
	  
	      my ($self, @xs) = @_;
	      return () unless $xs[0] =~ s/^(\^[^[]*)\[//;
	      my $superness = $1;
	      my @r;
	      push @r, shift @xs while @xs && $xs[0] !~ s/^(\Q$superness\E)\]//;
	      $1 eq $superness ? (\@r, @xs) : ();
	    
	}>
