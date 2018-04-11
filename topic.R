suppressPackageStartupMessages({
  require(stringr)
  require(topicmodels)
  require(tm)
  require(dplyr)
  require(RCurl)
  require(text2vec)
  require(gtools)
})

#Define functions
stopwords = unique(c("div", "href", "rel", "com", "relnofollow", "про", "что","dneprcity.net","novostimira.com","depo.ua","bbc.com",
                     "для", "relnofollow", "alt", "zero","img", "alignleft", "hspace", "vspace","studic.info","description","delo.ua",
                     "alignleft", "hspace", "vspace", "pbrp","altновини", "hrefhttpgsfmopinionhtml", "mode","strong", "pstrong", "targetblank",
                     "styletext-align", "justifi","altнапк", "classattachment-decosingl", "classreadmor","http:img.ura","pigua.info",
                     "ethereum", "hrefhttpresonanceua", "hrefhttpsinforesistorg","replyua.net","newsoboz.org","mc.async","manabalss.lv",
                     "hrefhttpblogiuaus", "hrefhttpvycherpnockua", "hrefhttpwwwkmugovuacontrolukpublisharticleartidampcatid","mid.ru",
                     "noneimg", "solid", "start", "stylefont-s", "stylefloat", "classfield-item","cackle.me","container","dtp.kiev.ua",
                     "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "hrefhttpwwwaddthiscombookmarkphpv","replyua.netпро",
                     "srcbmimgcomuaberlinstoragefinancexebfcbeadfdeccjpg","cackle_widget.push","document.createelement","polytics","loading",
                     "eff", "i","classfield-item", "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "datatyp","i.lb.ua",
                     "propertyrdfslabel", "datatyp", "propertyrdfslabel", "skospreflabel", "typeofskosconcept","mc.src","erve.ua","www.pravda.com.ua",
                     "reldcsubjecta", "classfield-tag", "field-type-taxonomy-term-refer","document.createelement","mc.type","lb.ua",
                     "classimagecach", "classlink", "classrtejustifyspan", "classtranslation-link", "classtranslationru","goo.gl","newsru.co.il",
                     "clearfixdiv", "data-colorbox-gallerygallery-node--ytzaljghei","document.getelementbyid","javascript","mon.gov.ua",
                     "propertyrdfslabel", "skospreflabel","этом","это","які","від","datatyp","-tag","function","innerhtml","sevastopolnews.info",
                     "hrefhttpkorupciyacom", "дуже", "там", "так", "але","span", "width", "classleftimgimg", "stylecolor", "stylefont-famili",
                     "hspace", "vspace", "clearal","classback-block","tabletrtd", "valigntop","document.location.protocol","zakarpatpost.net",
                     "hrefhttpwwwaddthiscombookmarkphpv", "even", "как", "titl","document.getelementsbytagname","text","true","sport.ua",
                     "sea", "black", "hold", "one","stylemargin", "color", "outlin", "pad", "none","nbsp","widget.js","www.unian.net",
                     "centerspan", "size", "stylefont-s", "font-siz", "divfont","s.nextsibling","s.parentnode.insertbefore","www.ukrinform.ru",
                     "justifi", "center", "width", "height", "classfeed-descriptionp","window.cackle_widget","xcoal","politics","newsru.co.il",
                     "pimg", "wp-post-imag", "margin", "sizesmax-width","justifystrong","joinfo.ua","1news","journal","unn.com.ua","newsonline24",
                     "srchttpimageunncomuaoriginaljpg", "altновости", "centerimg","styletextalign","stylefontsize","justify","fontsize","padding",
                     "helvetica","laquoР","raquo","httpnovostimiracomnewshtml","hrefhttprkrinuauploadspostsmatigeroyinyajpg","news_180574",
                     "hrefhttpskeletinfoorg","pem","leaders","hstrong","development","religious","targetblankstrong","politico","news_180572",
                     "che","glucosio","person","primarily","hrefhttpskeletinfoorg","classmetanav","clearall","newsoboz","politika","news_180571",
                     "stylefontfamily","arial","fontfamily","outline","sansserif","textalign","border","inherit","left","pspan","naviny.by",         
                     "justifyspan","rgb","styleboxsizing","small","googleadsectionend","womenbox.net","arianespace","polityka","porfyrios",
                     "classfielditem","classfielditemsdiv","fieldlabelhiddendiv", "also", "article", "Article","cellpadding","finanso.net",
                     "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","alt","feed","image","src","http",
                     "jpg", "larger", "may", "number","class","новости","gazeta.ua","rossii.html","zn.ua","cellspacing","portal",
                     "shown", "study", "studies", "this","img","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","p","photocharles",
                     "view", "View", "the", "biol","div","via", "image", "doi", "one", "classbackblock","dubinsky.pro","posted","news_180560",
                     "analysis","nbspap","photocharl","dharapak","pimg", "srcbmimgcomuaberlinstoragenewsxabddbdbajpg", "alignleft", "nbspnbspnbsp", "href",                      
                     "fieldnamebody","fieldtypetextwithsummary","title","datatype","fieldtypeimage","typeoffoafimage","www.globallookpress.com",               
                     "classcolorbox","fieldnamefieldimage","fieldtypetaxonomytermreference","rdfsseealso","relogimage","evenidfirstp",
                     "sizesmaxwidth","wppostimage","sizedecosingle","styledisplayblock","classattachmentdecosingle","hrefhttpspolitekanet",                                 
                     "httpspolitekanetwpcontentuploadsefebimagexjpeg","httpspolitekanetwpcontentuploadsgettyimageskopiyaxjpg","replyua.net.так",
                     "httpspolitekanetwpcontentuploadsunianxjpg","httpspolitekanetwpcontentuploadsgettyimagesxjpg","joomla","slavdelo.dn.ua",
                     "classfielditems","odd", "classfielditem","fieldtypetext","stylewidth","classimage","classimageattachteaser","imageattachnode",              
                     "medium","tahomaarialhelveticasansserif","imagethumbnail","classimagecache","pxa", "fieldfieldnewslead","fieldfieldyandexnews",
                     "srchttpinmediakitemscachedddddbbecbesjpg","srchttpsgoroddpuapicnewsnewsimagesmjpg","srchttpwwwkanzasuauploadpicresizejpg",
                     "ampraquo","hrefhttpelvisticomnode","hrefhttpwwwgazetamistoteualajntranslyatsiyaforumudetsentralizatsiyaosnovaformuvannyanovoyisystemyupravlinnyatamistsevogorozvytku",
                     "forklog","hidden","hrefhttpgiuauseridurlhttpsaffnewstutbyfeconomicsfhtml","lineheight","overflow","colorspan", "pxbbrspan","sizeb",
                     "srcbmimgcomuaacommonimgicstarsemptyvioletgif","altimg","titleСЂР","hrefhttpvideobigmirnetuser","srchttpbmimgcomuavideoimgsxjpg", "table",
                     "mediumspan","sansarialsansserifspan","langruruspan","classfooterinfosocialitemfollows","classfooterinfosocialitemiconspan",
                     "hrefhttpwwwpravdacomuarusnews", "classfbcommentscount","що","который","которые","также","таким","новости","несмотря","в","на","и","не",
                     "что","у","о","по","с","про","за","з","до","і","що","экс","об","этом","из","к","от","та","для","екс","це","его","а","как","politeka","он",
                     "від","його","новости","это","твій","года","суті","при","під","после","того","через","будет","так","липня","том","уже","більше","як","все",
                     "но","щодо","які","которые","то","2017","против","также","далее","новостей","він","своей","також","еще","перед","который","є","под","сути",
                     "твой","знай","михаил","со","году","року","я","видео","проти","сил","із","сейчас","чтобы","dneprcity.net","же","й","может","над","или",
                     "стало","было","може","тому","буде","те","был","быть","между","фото","112","який","ua","только","чем","является","ли","более","21",
                     "26.07.17","без","було","вже","где","зі","ще","19","2018","laquo","мы","raquo","вона","кто","ми","своїй","ст","яких","были","ему","их",
                     "наших","него","особо","очень","себе","щоб","де","ее","зараз","котором","сам","своем","теперь","цього","але","если","которая","себя",
                     "тем","эти","https","больше","був","бути","два","навіщо","нас","таки","тогда","replyua.net","будут","йому","між","можно","ним","новость",
                     "п","ч","чи","12","50","mc","должен","им","именно","источник","которых","меньше","стал","эту","буду","має","стоит","требует","этот","11",
                     "http","была","высокий","день","имеет","її","мог","несколько","них","она","этого","вони","всего","вся","когда","которым","одним","почти",
                     "разі","стала","була","всі","вы","даже","дал","дать","кого","которое","м","нет","ни","новину","оборони","один","поки","потому","свою",
                     "стали","таким","этой","яка","якому","яку","script","the","var","ей","новини","одно","понад","проте","серед","такий","чому","вот","дати","есть",
                     "без", "більш", "більше", "буде", "начебто", "би", "був", "була", "були", "було", "бути", "вам", "вас", "адже", "увесь", "уздовж", "раптом", "замість", 
                     "поза", "униз", "унизу", "усередині", "в", "навколо", "от", "втім", "усі", "завжди", "усього", "усіх", "усю", "ви", "де", "так", "давай", "давати", "навіть", 
                     "для", "до", "досить", "інший", "його", "йому", "її", "її", "їй", "якщо", "є", "ще", "же", "за", "за винятком", "тут", "з", "через","або", "їм", "мати", 
                     "іноді", "їх", "якось", "хто", "коли", "крім", "хто", "куди", "чи", "або", "між", "мене", "мені", "багато", "може", "моє", "мої", "мій", "ми", "на", "назавжди",
                     "над", "треба", "нарешті", "нас", "наш", "не", "його", "ні", "небудь", "ніколи", "їм", "їх", "нічого", "але", "ну", "про", "однак", "він", "вона", "вони", 
                     "воно", "знову", "від", "тому", "дуже", "перед", "по", "під", "після", "потім", "тому", "тому що", "майже", "при", "про", "раз", "хіба", "свою", "себе", 
                     "сказати", "з", "зовсім", "так", "також", "такі", "такий", "там", "ті", "тебе", "тем", "тепер", "те", "тоді", "того", "теж", "тієї", "тільки", "тому", "той", 
                     "отут", "ти", "уже", "хоч", "хоча", "чого", "чогось", "чий", "чому", "через", "що", "щось", "щоб", "ледве", "чиє", "чия", "ця", "ці", "це", "цю", "цього", 
                     "цьому", "цей","і","у","та","я","а","й","як","які","бо","із","який","тим","нам","б","всі","ж","яку","зі","яких","всіх","цим","1997","1991","1992","1998","2008",
                     "2009","2010","2011","2012","2013","2014","2015","2016","2017","рік","все","роком","році","нехай","хай","року","яка","них","ним","1996","то"))

text.clean = function(x){ 
  require("tm")
  x  =  gsub("<.*?>", " ", x)
  x  =  removeNumbers(x)  
  x  =  removeWords(x, stopwords("english"))
  x  =  removeWords(x, stopwords("russian"))
  x  =  removeWords(x, stopwords)
  x  =  stemDocument(x,language="russian")
  x  =  stripWhitespace(x)                  
  x  =  gsub("^\\s+|\\s+$", "", x)  
  x  =  removePunctuation(x)
  return(x)
}

train_lda <- function(text,caption,k){
  it_train = itoken(text.clean(text), 
                    preprocessor = tolower, 
                    ids = 1:length(text), 
                    progressbar = T)
  vocab = create_vocabulary(it_train,stopwords = stopwords)
  
  pruned_vocab = prune_vocabulary(vocab, 
                                  term_count_min = 5, 
                                  doc_proportion_max = 0.5,
                                  doc_proportion_min = 0.0001)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_train  = create_dtm(it_train, vectorizer)
  
  lda_model = LDA$new(n_topics = k)
  
  doc_topic_distr = 
    lda_model$fit_transform(x = dtm_train, n_iter = 2000, 
                            convergence_tol = 0.001, n_check_convergence = 25, 
                            progressbar = T)
  
  gammaDF <- as_tibble(doc_topic_distr)
  names(gammaDF) <- c(1:k)
  
  toptopics <- tibble(theme = as.numeric(apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))][1])),
                      theme_quality = apply(gammaDF,1,max))
  
  tem <- bind_cols(data_frame(caption,text),toptopics)
  slov <- left_join((tem %>% 
                       group_by(theme) %>% 
                       summarise(theme_quality=max(theme_quality)[1])),tem[-2],by=c("theme_quality","theme")) %>% 
    rename("theme2"="caption")
  left_join(tem,(slov[!duplicated(slov[,'theme']),] %>% select(theme,theme2)),by=c("theme")) %>% 
    select(-theme) %>% 
    rename("theme"="theme2")
}
