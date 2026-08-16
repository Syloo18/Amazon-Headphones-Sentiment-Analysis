library(NLP)
library(tm)
library(SnowballC)

txtpath<-file.path("C:/Users/admin/Downloads/ANL312/ECA/archive/AllProductReviews.csv")
reviews<-read.csv(txtpath, encoding="latin1")
sum(duplicated(reviews))
set.seed(312)
S_reviews<-reviews[sample(nrow(reviews),1000),]

comment_corpus<- VCorpus(DataframeSource(S_reviews))
comments<-comment_corpus
comments<-tm_map(comments,content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) {return(gsub(pattern, " ", x))})
comments <- tm_map(comments, toSpace, "[[:punct:]]")
comments<-tm_map(comments,removeNumbers)
toSpace <- content_transformer(function(x,pattern) gsub(pattern," ",x))
comments <- tm_map(comments, toSpace, "[^A-Za-z']")

stopwords<-scan(file="C:/Users/admin/Downloads/ANL312/ECA/archive/mystopwords.data", what=character(),sep="\n")
comments<-tm_map(comments,removeWords,stopwords[1:671])
comments<-tm_map(comments, removeWords, c(stopwords("english"),"nil", "na", "absolutely", "acha", "aftar", "aftr", "allno", "allwayz", "alwayz", "anytime", "araaam",
                                          "atleast", "aur", "baa", "bahot", "bcoz", "bcz", "becous", "bina", "btdon", "burai", "bur", "cake", "completely", "coz",
                                          "csi", "dat", "didn", "didnt", "dint", "doesn", "doesnt", "dat", "definitely", "despite", "dis", "djent", "dnt", "dock",
                                          "don", "doom", "dosent", "dose", "everythingand", "everythingits", "evry", "eventually", "extremely", "everytime", "everyday",
                                          "entirely", "fadu", "finally", "fortunately", "frankly", "freakingly", "fully", "firstly", "highly", "higly", "hai", "harman",
                                          "havng", "haven", "honestly", "hindsight", "hav", "have", "iam", "itz", "izzzz", "isn", "isnt", "itbecause", "inevitably", "itit",
                                          "importantly", "ireally", "imp", "jiofi", "jst", "khar", "kkhar", "kijiye", "kumar", "koi", "krne", "lol", "literally", "mahesh",
                                          "mohit", "mine", "mrp", "mei", "me", "noticeably", "nhi", "omg", "oops", "personally", "paisa", "pau", "rha", "rely", "seriously", "shouldn",
                                          "soo", "sooo", "sould", "sre", "shiva", "shockingly", "soi", "surely", "suddenly", "tbh", "thatreally", "thr", "totally",
                                          "tje", "tooo", "thing", "thong", "ths", "thi", "tatti", "tbt", "upto", "undar", "unable", "vth", "wasn", "wasool", "wat", "you",
                                          "ufug", "ultra", "utter", "vary", "whatsoever", "wouldd", "wouldn", "wellnot", "damn", "peice", "simply", "barely", "bearly"))

synonyms<-list(list(word="accord", syns=c("accroding")),
               list(word="absence", syns=c("lack")),
               list(word="adapter", syns=c("jack", "connector", "usb")),
               list(word="attach", syns=c("attack")),
               list(word="assistant", syns=c("siri")),
               list(word="add", syns=c("additional")),
               list(word="adjust", syns=c("adjztbut", "adjustment")),
               list(word="allay", syns=c("alleviate")),
               list(word="amazon", syns=c("amzon", "amzone")),
               list(word="amplify", syns=c("amplification")),
               list(word="audiophile", syns=c("audiophiles", "enthusiast", "fanatic", "freak")),
               list(word="auto", syns=c("automatically", "passive")),
               list(word="average", syns=c("avarage", "avg", "averagenot", "avrage", "alright", "basic", "fair", "medium", "neutral", "traditional", "ohk", "okayish", "okie", "okmaterial", "okmultiple", "oksuggestion", "ordinary")),
               list(word="aspect", syns=c("dpt")),
               list(word="allow", syns=c("enable", "accept", "attend")),
               list(word="adhesive", syns=c("feviquick")),
               list(word="aesthetically", syns=c("stylish", "style")),
               list(word="bass", syns=c("basss", "bassy", "bassbattery", "basscons", "bassit", "bassnormal", "bassroom", "outputbass", "mebass", "pressure", "thump", "thumpingly")),
               list(word="battery", syns=c("bat", "batery", "batry", "batter", "batterly", "battry", "bettry", "btry")),
               list(word="bluetooth", syns=c("bluettoh", "blutooth", "tooth", "remote", "wireless")),
               list(word="buyer", syns=c("customer", "consumer", "listener", "user")),
               list(word="buy", syns=c("order", "purchase", "usebuy", "book", "pay", "invest", "investment")),
               list(word="bad", syns=c("suck", "stupid", "terrible", "useless", "unbearable", "unacceptable", "unpleasant", "worthless", "wrost", "fail", "rediculous", "anoyee", "annoy", "hassel", "troublesome", "hassle", "disaster", "junk", "hiccup", "incompetent", "inferior", "miserable", "pathetic", "pathetically", "poor"  )),  
               list(word="build", syns=c("coat", "holder", "paddings")),
               list(word="button", syns=c("key")),
               list(word="call", syns=c("contact")),
               list(word="caller", syns=c("opposite")),
               list(word="company", syns=c("compony")),
               list(word="confident", syns=c("brandblindly", "blindly")),
               list(word="cancel", syns=c("cancelation", "cancellation", "cancellationcons")),
               list(word="cant", syns=c("cannaught", "couldn")),
               list(word="care", syns=c("carepassive", "maintenance")),
               list(word="careful", syns=c("carefully", "beware", "safe", "safety")),
               list(word="clarity", syns=c("clearity", "clearly", "clearty", "clear", "clearcons", "cristal", "crisp", "crystal", "makeupclarity", "niceclearity", "precise")),
               list(word="clothe", syns=c("jean", "pant")),
               list(word="choice", syns=c("choose", "decision", "decide", "opt", "option")),
               list(word="comfort", syns=c("comfortable", "comfartbly", "comfortness", "conformable", "comfortably", "ergonomically", "leisure", "painless")),
               list(word="compare", syns=c("comparison", "comparable")),
               list(word="colour", syns=c("color")),
               list(word="competition", syns=c("counterpart", "class", "category", "market", "segment")),
               list(word="connection", syns=c("connective", "connectivity", "contectvity", "signal", "connectedonly", "conneted", "coonecting", "access", "reconnect", "connect")),
               list(word="cable", syns=c("wire", "cord", "chord")),
               list(word="concern", syns=c("consered", "consider", "consideration", "doubt", "worry", "question")),
               list(word="constant", syns=c("constantly", "continue", "continuous", "continuously", "continously", "persist", "sustain")),
               list(word="convenience", syns=c("convenient", "convinient", "portability", "flexible", "flexibility")),
               list(word="costly", syns=c("expensive", "expensiveinstead", "overpriced")),
               list(word="couple", syns=c("xouple")), 
               list(word="dead", syns=c("die")),
               list(word="damage", syns=c("badbroke", "breakin", "conk", "damged", "defective", "defect", "faulty", "misbehave", "rip", "snap", "snag", "scratch", "cut", "deteriorate", "abuse")),
               list(word="daily", syns=c("dialy")),
               list(word="drop", syns=c("degrade", "decrease")),
               list(word="delivery", syns=c("deliver", "delevered", "shipment", "arrive", "dispatch", "transit" )),
               list(word="disadvantage", syns=c("con", "drawback", "flaw", "usedconsnoise", "negative", "nagative")),
               list(word="device", syns=c("devise", "equipment", "gadget", "item", "model", "stufs", "object", "productsit", "ptoduct", "prodcts", "stock", "product", "production")),
               list(word="unhappy", syns=c("dissatisfy", "disgustedthis", "hate", "dislike", "upset", "irritate", "disappoint", "dispointed", "dissapointed", "disspointed", "disappointment", "complain", "complaint")),
               list(word="describe", syns=c("description", "advertise")),
               list(word= "range", syns=c("distance", "diatance", "radius", "metre", "mtr", "meter")),
               list(word="disconnect", syns=c("diconnect")),
               list(word="distortion", syns=c("distort", "distrotion", "delay", "buffer", "hang", "lag", "letter", "latency", "disruptionsometimes", "break", "crack", "crackle", "interrupt", "interrupption", "disturb", "disturbance", "distubance", "crazy", "gibberish")),
               list(word="drain", syns=c("defuse", "discharge")),
               list(word="duplicate", syns=c("extra", "spare")),
               list(word="durable", syns=c("durability", "sturdy", "rugged")),
               list(word="difficult", syns=c("impossible")),
               list(word="discount", syns=c("lightning", "sale", "clearance")),
               list(word="domestic", syns=c("house", "local")),
               list(word="early", syns=c("preemptive")),
               list(word="earbud", syns=c("earbuds", "earfones", "earpines", "earphn", "earphone", "earpiece", "earplug", "earpods", "bud", "bid", "ibuds", "pod", "earphines")),
               list(word="easy", syns=c("easily", "easilynot", "simple", "prone")),
               list(word="happy", syns=c("enjoyable", "glad", "enjoy", "njoy", "pleasure", "soothe", "harmon", "pleasant", "pleasent", "immerse", "guysthankssekhar", "kudo", "thank", "thankful", "satisfy", "satisafaction", "appreciate", "link")),
               list(word="excellent", syns=c("excelent", "excellentits", "execellent", "extraordinary", "fantastic", "fabulous", "heavenly", "marvelous", "phenomenal", "perfect", "parfect", "perfectly", "terrific", "wonderful", "notch", "ultimate", "amezing", "ankickass", "awesome", "awesomeness", "awesomesuper", "awrsome", "awseom", "awsome", "awsum", "awasm", "amass", "beast", "bomb", "kick", "mesmerize", "owesome", "speechless", "mind", "blow", "blast", "rock", "amaze", "superb", "superbly", "superbb", "superup", "soppar", "superrbb", "super", "fav", "bestt", "favo", "essential", "outstanding", "outdtanding")),
               list(word="equaliser", syns=c("equalize", "equalizer", "equilizer")),
               list(word="experience", syns=c("experiencedont", "face")),
               list(word="effect", syns=c("effe")),
               list(word="exercise", syns=c("gym", "gymming", "jog", "runner", "sport", "sportz", "workout", "gyming")),
               list(word="function", syns=c("fuctions", "functionality", "feature", "capability", "property" )),
               list(word="friendly", syns=c("friendliness")),
               list(word="fragile", syns=c("fregile", "delicate", "flimsy", "baddurability", "sensitive")),
               list(word="fool", syns=c("foolish", "joker")),
               list(word="frequently", syns=c("frequentely", "frequentlywhen", "meny", "regular", "regularly")),
               list(word="frequency", syns=c("freqencies")),
               list(word="feel", syns=c("feelingthought", "suppose")),
               list(word="fraud", syns=c("replica", "fake", "cheat", "myth")),
               list(word="fit", syns=c("fitit")),
               list(word="fraction", syns=c("half", "portion")),
               list(word="fix", syns=c("resolve", "repair", "solution", "stetch")),
               list(word="functional", syns=c("usable", "standed", "workingi", "work", "receptive", "audibility", "audible")),
               list(word="focus", syns=c("concentration", "highlight")), 
               list(word="fast", syns=c("quick", "timely", "prompt")),
               list(word="game", syns=c("gameing", "gaming", "pubg", "battle", "royale", "pub")),
               list(word="genuine", syns=c("authenticity", "original", "originality", "official")),
               list(word="german", syns=c("germany")),
               list(word="good", syns=c("goodand", "goodbass", "goodcons", "gud", "itgud", "muchgood", "usegood", "usefull", "effective", "decent", "descent", "finish", "adv", "prossound", "advantage", "grea", "commendable", "impressive", "cool", "impress", "ice", "smokin", "superior", "great", "satisfactory", "beautiful", "lovely", "premium", "porsche", "rich", "classy", "nice", "nic", "nicebut", "niceeeeee", "nicely", "nicesuper", "noice")),
               list(word="guarantee", syns=c("warranty", "warrantysound", "warrienty", "policy")),
               list(word="honest", syns=c("frank", "true")),
               list(word="handfree", syns=c("handfrees", "hanfree", "handsfree")),
               list(word="headphone", syns=c("headphonealso", "headphonesvoice", "headset", "headfone", "haedphone", "hps")),
               list(word="help", syns=c("helpful", "handy")),
               list(word="hour", syns=c("hourse", "hrsgood", "hrconnectivity")),
               list(word="increase", syns=c("erection")),  
               list(word="isolation", syns=c("block", "suppression", "mask", "attenuation", "reduce", "reduction")),       
               list(word="issue", syns=c("issuessound", "issuewhen", "quirk", "problem", "problemwire", "challenge", "trouble", "situation")), 
               list(word="intensive", syns=c("intensity", "obsessive", "rigorously", "rigoursly", "rigousous", "roughly", "rough")),         
               list(word="internal", syns=c("inside", "inbuilt")),     
               list(word="install", syns=c("download")),     
               list(word="indicate", syns=c("indicator")), 
               list(word="india", syns=c("dia")), 
               list(word="love", syns=c("lovng", "betterlove")),  
               list(word="loud", syns=c("loudly", "laudness", "deafen")),  
               list(word="lover", syns=c("loversound")),  
               list(word="lose", syns=c("loss")),  
               list(word="listen", syns=c("listining")),  
               list(word="light", syns=c("lightweight", "lighten", "lite")), 
               list(word="large", syns=c("big", "huge", "major")), 
               list(word="long", syns=c("lengthy")),
               list(word="loose", syns=c("slip", "slippery")),
               list(word="magnet", syns=c("magnetic")),  
               list(word="manufacture", syns=c("manufacturering", "manafacture", "manufacturer")),  
               list(word="microphone", syns=c("mic", "mics", "maxmic", "mike", "piecemicrophone")),  
               list(word="month", syns=c("mnts", "mths", "monthawesome", "monthy", "moth")),  
               list(word="music", syns=c("instrument", "musiccalling", "song", "death", "metal", "edm", "hip", "hop", "playlists", "trance", "vocal")),  
               list(word="money", syns=c("moneygood", "penny", "buck", "inr", "rupee")),  
               list(word="minmium", syns=c("min")),  
               list(word="middle", syns=c("mid")),  
               list(word="maximum", syns=c("max")),  
               list(word="minute", syns=c("mint")),
               list(word="manual", syns=c("instruction", "specification")),
               list(word="midtones", syns=c("midrange", "midsconclusion")),
               list(word="neat", syns=c("neatly")),  
               list(word="online", syns=c("internet", "wifi")),
               list(word="observe", syns=c("observation", "view", "notice", "realise", "read", "watch")),
               list(word="opinion", syns=c("apponeion")),
               list(word="output", syns=c("outpurlt")),
               list(word="noisy", syns=c("haptic", "unnecessary")),
               list(word="please", syns=c("plz", "plzz")),  
               list(word="preset", syns=c("confugiration")), 
               list(word="preety", syns=c("pretty", "petty")),
               list(word="proper", syns=c("properly", "properlyplease")),  
               list(word="package", syns=c("packageing", "parcel", "packet", "box", "bag", "carriage")),  
               list(word="pain", syns=c("painful", "ache", "fatigue", "hurt", "heart")),  
               list(word="picture", syns=c("pic", "photo", "image")),  
               list(word="price", syns=c("pricesound", "cost", "prise", "prize")),  
               list(word="pickup", syns=c("puckup")),  
               list(word="part", syns=c("paart")),  
               list(word="person", syns=c("people")),  
               list(word="peer", syns=c("friend")),  
               list(word="perform", syns=c("performance")),  
               list(word="period", syns=c("piriod")),  
               list(word="plug", syns=c("plugin", "port", "slot")),
               list(word="put", syns=c("putting")),
               list(word="phone", syns=c("fone", "mobile", "cellphone", "smartphones", "smart", "android", "andriod", "apple", "iphone", "iphones", "ios", "ipodgood")),
               list(word="punch", syns=c("punchy", "blare")),
               list(word="quality", syns=c("qualitybluetooth", "qualitycons", "qualityi", "qualits", "qualitly", "qualityloud", "qualitysound", "qualitysuper", "qualityvalue", "qualityvery", "qualityamazing", "quatily", "quilty", "standard", "mark", "par")),
               list(word="respond", syns=c("response", "responsive", "reply", "engage", "answer")),  
               list(word="receive", syns=c("recieved", "reveived")),
               list(word="review", syns=c("reviewer", "comment")),  
               list(word="reasonable", syns=c("reasonably")), 
               list(word="replace", syns=c("areplacement", "replacement", "exchange", "reverse")),
               list(word="randomly", syns=c("intermittently")),
               list(word="reason", syns=c("justify", "explain")),
               list(word="return", syns=c("refund")), 
               list(word="reputable", syns=c("repute")), 
               list(word="recommend", syns=c("suggeste", "suggestion")),  
               list(word="suitable", syns=c("suit", "compatible")),
               list(word="strong", syns=c("deep", "overpower", "prominent", "solid")),
               list(word="sound", syns=c("soundpowerful", "soundmike", "sond", "sonds", "starnoise", "upsound", "noise", "audio", "position", "stereo", "tune", "voice")),  
               list(word="stop", syns=c("stope")),  
               list(word="surround", syns=c("sorroundings", "background", "ambience", "outer")), 
               list(word="smooth", syns=c("smoothly")),   
               list(word="sign", syns=c("sighns")),  
               list(word="speak", syns=c("talk", "conversation")),
               list(word="time", syns=c("duration", "moment", "session", "state", "tym", "window")),
               list(word="tell", syns=c("convey", "identify", "post", "reflect", "write")), 
               list(word="treble", syns=c("trebel", "trebl")), 
               list(word="tag", syns=c("tagg")),
               list(word="travel", syns=c("traffic", "transport")), 
               list(word="typical", syns=c("usual")),  
               list(word="tangle", syns=c("entangle", "mess")),
               list(word="update", syns=c("revise")), 
               list(word="unique", syns=c("special", "specially")),  
               list(word="unsure", syns=c("skeptical", "confuse", "wonder")),
               list(word="uncomfortable", syns=c("uneasy", "discomfortimpressed", "harsh", "itch", "stretch")),
               list(word="worth", syns=c("worthy", "wrt", "economic", "entry", "steal", "affordable")),  
               list(word="usage", syns=c("usingone", "claim")),  
               list(word="upgrade", syns=c("improvement", "improve")),  
               list(word="volume", syns=c("valume")),  
               list(word="vibration", syns=c("viberate")),  
               list(word="video", syns=c("vid", "movie", "youtube")),  
               list(word="walk", syns=c("walikng")), 
               list(word="want", syns=c("wanna", "wish", "desire", "pray", "hope", "expect", "expectation", "expectedstill", "requirement")),  
               list(word="wear", syns=c("ware")),
               list(word="wrong", syns=c("wrongly")),
               list(word="brand", syns=c("sennheiser", "seinheiser", "seinnheiser", "senheiser", "sennheisers", "sennhiser", "skullcandy", "candy", "skull", "boss", "bose", "boat", "rocker", "rokerz", "rockerz", "samsung", "samsungs")),
               list(word="workhorse", syns=c("horse")))


library(textstem)
comments<-tm_map(comments, content_transformer(textstem::lemmatize_strings))
comments<-tm_map(comments,stripWhitespace) 

replaceSynonyms <- content_transformer(function(x, syn = NULL) {
  Reduce(function(a, b) {

    pattern <- paste0("\\b(", paste(b$syns, collapse = "|"), ")\\b")

    gsub(pattern, b$word, a)
  }, syn, x)
})

comments <- tm_map(comments, replaceSynonyms, synonyms)

dtm<-DocumentTermMatrix(comments)
options(max.print=1000000)
dtm$dimnames$Terms
dim(dtm)

dtm_matrix <- as.matrix(dtm)
row_totals <- rowSums(dtm_matrix) 
dtm <- dtm[row_totals > 0, ]
dim(dtm)

dtm_tfidf<-weightTfIdf(dtm, normalize=TRUE)

library(topicmodels)
library(slam)

k_values <- 5:15
perplexities <- numeric(length(k_values))
logliks <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  lda_model <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = 312))
  
  perplexities[i] <- perplexity(lda_model, dtm)
  logliks[i] <- logLik(lda_model)
}

# Plot perplexity, lower = better
plot(k_values, perplexities, type = "b", xlab = "Number of Topics", ylab = "Perplexity",
     main = "Perplexity vs Number of Topics")

# Plot log-likelihood, higher = better
plot(k_values, logliks, type = "b", xlab = "Number of Topics", ylab = "Log-Likelihood",
     main = "Log-Likelihood vs Number of Topics")

#### New Code!!!!  Filter the terms in the Document-Term Matrix according to TF-IDF value

# Step 1: Convert to matrix (dense)
m_tfidf <- as.matrix(dtm_tfidf)

# Step 2: Compute mean tf-idf per term
term_means <- colMeans(m_tfidf)

# Step 3: Compute threshold as median
threshold <- median(term_means)

# Step 4: Select terms above threshold: keep the terms whose mean tf-idf is ≥ the threshold and extracts the names (terms) of the elements that passed the threshold
selected_terms <- names(term_means[term_means >= threshold])

# Step 5: Subset original DTM (not the tf-idf one)
dtm_reduced <- dtm[, selected_terms]
dtm_reduced<-dtm_reduced[row_sums(dtm_reduced)>0,] # keep the documents with at least one term

# inspect the reduced document-term-matrix and check the dimension
inspect(dtm_reduced)
dim(dtm_reduced)

#lda model
k<-13
seed<-312
model_lda<-list(VEM=LDA(dtm_reduced,k=k,method="VEM",control=list(seed=seed)),Gibbs=LDA(dtm_reduced,k=k,method="Gibbs",control=list(seed=seed,burnin=1000,thin=100,iter=1000)))

#results
terms_vem<-terms(model_lda$VEM,10)
terms_gibbs<-terms(model_lda$Gibbs,10)
terms_vem
terms_gibbs

topic_gibbs_1<-topics(model_lda$Gibbs,1) #using Gibbs method as an example
topic_gibbs_1[1:5]

topic_gibbs_2<-topics(model_lda$Gibbs,2) #using Gibbs method as an example
topic_gibbs_2[,1:5]


#cell values as posterior topic distribution for each document.This will show you what documents belong to which topic with the highest probability
gammaDF_gibbs<-as.data.frame(model_lda$Gibbs@gamma,row.names=model_lda$Gibbs@documents)
names(gammaDF_gibbs)<-c(1:k)
View(gammaDF_gibbs)

k <- 13
custom_topic_names <- c("Bluetooth connectivity","Worth the Money","Bad Performance","Excellent Performance","Good Performance",
  "Ergonomics","Product Durability","Sound Clarity","Bass Quality","Regret Purchase","After-sales Service","Brands","Battery Life")

names(gammaDF_gibbs) <- custom_topic_names
View(gammaDF_gibbs)

#save the gamma table as a csv file, for Windows
write.csv(gammaDF_gibbs,"C:/Users/admin/Downloads/ANL312/ECA/archive/gammaDF_gibbs1.csv", row.names = TRUE)

library(sentimentr)
library(dplyr)

# Perform document-level sentiment analysis directly on ORIGINAL reviews text
# No sentence splitting - analyze each review as a whole document
sent <- sentiment_by(S_reviews$text, by = NULL) %>% 
  mutate(doc_id = as.character(S_reviews$doc_id))

# View sentiment results
head(sent)

# Keep only numeric columns from gammaDF_gibbs
gibbs_theta <- gammaDF_gibbs %>%
  select(where(is.numeric))

# Find dominant topic for each document
dom <- data.frame(
  doc_id = rownames(gammaDF_gibbs),
  topic = apply(gibbs_theta, 1, function(x) custom_topic_names[which.max(x)]),
  stringsAsFactors = FALSE
)

# Merge sentiment with original review text and dominant topic
sent_complete <- sent %>%
  left_join(S_reviews %>% 
              mutate(doc_id = as.character(doc_id)) %>%
              select(doc_id, text), 
            by = "doc_id") %>%
  left_join(dom, by = "doc_id")

# Reorder columns for better readability
sent_complete <- sent_complete %>%
  select(doc_id, element_id, word_count, sd, ave_sentiment, topic, text)

# Calculate average sentiment by topic
topic_sent <- sent_complete %>%
  group_by(topic) %>%
  summarise(
    avg_sentiment = mean(ave_sentiment), 
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(topic)

# View results
print("Average Sentiment by Topic:")
print(topic_sent)

# View the structure to confirm columns
str(sent_complete)

# Save to CSV
write.csv(sent_complete, 
          "C:/Users/admin/Downloads/ANL312/ECA/archive/sentiment_analysis_results.csv", 
          row.names = FALSE)

