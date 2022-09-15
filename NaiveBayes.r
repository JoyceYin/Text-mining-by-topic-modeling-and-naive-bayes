library('openNLP')
library('tm')
library('NLP')
library('dplyr')
library('data.table')
library('tmcn')  #text mining tool for Chinese
library('chinese.misc')
library('caret')
library('e1071')
library('pROC')
library('naivebayes')
getwd()
Sys.setlocale(category="LC_ALL",locale="chinese")

datapreprocess = function(corp_data, index_data){
    call1 = match.call()
    
    #id with freuqency of 1
    id_unique_list = c()
    tttable = table(index_data[,'doc_id'])

    for (i in 1:length(tttable)){
        if (tttable[[i]]==1){
            id_unique_list = c(id_unique_list, as.integer(names(tttable)[i]))
        }
    }
    
    df_train_index = data.frame(doc_id = id_unique_list, stringsAsFactors = FALSE)
    #train_doc_id = unique(index_data[,'doc_id'])
    
    #----create match id table
    topic = unique(index_data$topic)
    idlist = c()
    topiclist = c()
    num = 0
    for (i in topic){
        idlist = c(idlist, num)
        topiclist = c(topiclist, i)
        num = num +1
    }
    match = data.frame(id = idlist, topic = topiclist, stringsAsFactors = FALSE)
    
    #-------dataframe for document term matrix
    df_dtm_train = merge(x = df_train_index, y = corp_data, by = "doc_id", all.x = TRUE) 
    
    #matched table for training model
    index_data = merge(x = index_data, y=df_train_index, by ="doc_id")
    train_match = merge(x = index_data, y = match, by = "topic")   #topic doc_id id
    train_match = train_match[,c('doc_id','id')] 
    train_match = train_match[order(train_match$doc_id),]
    
    #----return
    list(call=call1, dtm = df_dtm_train, match = match, train_match=train_match)
}

train.mnb = function (dtm,labels){

    V <- ncol(dtm)
    N <- nrow(dtm)

    call = match.call()
    prior <- table(labels)/N
    labelnames <- names(prior)
    nclass <- length(prior)
    cond.probs <- matrix(nrow=V,ncol=nclass)  #create an empty matrix for condtional probability
    dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]]
    dimnames(cond.probs)[[2]] <- labelnames
    index <- list(length=nclass)
    for(j in 1:nclass){
        index[[j]] <- c(1:N)[labels == labelnames[j]]
    }

    for(i in 1:V){
        #flush.console()
        for(j in 1:nclass){
            cond.probs[i,j] <- (sum(dtm[index[[j]],i])+1)/(sum(dtm[index[[j]],])+V)
        }
    }
    list(call=call,prior=prior,cond.probs=cond.probs)
}

predict.mnb = function (model_train,test.dtm)
{
    classlabels <- dimnames(model_train$cond.probs)[[2]]
    logprobs <- as.matrix(test.dtm) %*% log(model_train$cond.probs)
    N <- nrow(test.dtm)
    nclass <- ncol(model_train$cond.probs)
    logprobs <- logprobs+matrix(nrow=N,ncol=nclass,log(model_train$prior),byrow=T)
    classlabels[max.col(logprobs)]
}

#implement by own
wholeprocess = function(df_corp, train_index, test_index, sparsity, en_zh)
{
    #call = match.call()
    datapre = datapreprocess(df_corp, train_index)
    #print(head(datapre$dtm))
    
    print('-----------Document Term Matrix Generating')
    Corpus = VCorpus(DataframeSource(datapre$dtm));  #summary(Corpus);
    if (en_zh == 'en'){
        TDM = DocumentTermMatrix(Corpus);
    }else{
        TDM = DocumentTermMatrix(Corpus, control = list(wordLengths=c(1, Inf)));
    }
    #remove terms with set sparsity
    dtm <- removeSparseTerms(TDM,sparsity)
    inspect(dtm)
    
    print(datapre$match[,'topic'])
    
    flush.console()
    
    print('-----------Naive Bayes Model Building')
    train_match = datapre$train_match
    labels <- train_match[,'id']
    model_train <- train.mnb(dtm,labels)
    print(model_train$prior)
    flush.console()

    names(model_train$prior) <- datapre$match[,'topic']
    #model_train$prior
    model_train$cond.probs = as.matrix(model_train$cond.probs)
    dimnames(model_train$cond.probs)[[2]] <- datapre$match[,'topic']
    
    flush.console()
    
    print('-----------Testing')
    print('-----------Document Term Matrix Generating')
    datapre_test = datapreprocess(df_corp, test_index)
    df_dtm_test = datapre_test$dtm

    Corpus_test = VCorpus(DataframeSource(df_dtm_test));  #summary(Corpus);
    test.dtm = DocumentTermMatrix(Corpus_test, list(dictionary=dimnames(dtm)[[2]]));
    inspect(test.dtm)
    
    flush.console()

    print('-----------Predicting')
    model_predict <- predict.mnb(model_train,test.dtm)

    #show id corresponded topic
    true_labels = merge(x=datapre_test$train_match, y=datapre_test$match, by="id", all.x = TRUE)
    true_labels = true_labels[order(true_labels$doc_id),]
    table(model_predict,true_labels[,'topic'])
    flush.console()
    
    print('-----------Evaluating')
    evalu = data.frame(pred = model_predict, true = true_labels[,'topic'], stringsAsFactors = TRUE)
    confusion = confusionMatrix(data = evalu[,'pred'],reference = evalu[,'true'])
}

#implement by packages
naive.bayes = function(df_corp, train_index, test_index){
    process = datapreprocess(df_corp, train_index)
    data_match = merge(x = process$train_match, y=process$match, by = "id", all.x = TRUE)
    data_match = data_match[,c('doc_id','topic')]

    #process$dtm
    Corpus = VCorpus(DataframeSource(process$dtm));  #summary(Corpus);
    TDM = DocumentTermMatrix(Corpus, control = list(stopwords=TRUE,
                                                   wordLengths=c(1, Inf)));
    #inspect(TDM)
    dtm <- removeSparseTerms(TDM,0.97)
    inspect(dtm)

    df = data.frame(as.matrix(dtm), stringsAsFactors=FALSE)
    df <- cbind(doc_id = rownames(df), df)
    rownames(df) <- 1:nrow(df)

    data_train = merge(x =df, y=data_match, by = "doc_id", all.x = TRUE)
    nb = naive_bayes(topic ~ ., data_train[,-1], laplace = 1)
    summary(nb)
    tables(nb, 1)

    process_test = datapreprocess(df_corp, test_index)
    data_match = merge(x = process_test$train_match, y=process_test$match, by = "id", all.x = TRUE)
    data_match = data_match[,c('doc_id','topic')]

    #process$dtm
    test.Corpus = VCorpus(DataframeSource(process_test$dtm));  #summary(Corpus);
    test.dtm = DocumentTermMatrix(test.Corpus, list(dictionary=dimnames(dtm)[[2]]));

    test.df = data.frame(as.matrix(test.dtm), stringsAsFactors=FALSE)
    test.df <- cbind(doc_id = rownames(test.df), test.df)
    rownames(test.df) <- 1:nrow(test.df)

    data_test = merge(x =test.df, y=data_match, by = "doc_id", all.x = TRUE)
    test_pred = data_test[,-dim(data_test)[2]]
    test_pred = test_pred[,-1]
    
    laplace1 = predict(nb, data_test, type="class")
    #table(laplace1, data_test$topic, dnn=c("predict", "actual"))
    evalu = data.frame(pred = laplace1, true = data_test$topic, stringsAsFactors = TRUE)
    confusion = confusionMatrix(data = evalu[,'pred'],reference = evalu[,'true'])
}

df_corp=read.csv('wholedataset_en.csv')
Sys.setlocale(category="LC_ALL",locale="chinese")
df_corp_zh = data.table::fread('wholedataset_zh.csv')

train_index1 = read.csv('Experimental dataset/dataset1train.csv')
test_index1 = read.csv('Experimental dataset/dataset1test.csv')
colnames(train_index1) <- c("doc_id", "topic")
colnames(test_index1) <- c("doc_id", "topic")

naivebayes_en1 = wholeprocess(df_corp, train_index1, test_index1, 0.97, 'en')

naivebayes_en1

confusion1 = naive.bayes(df_corp, train_index1, test_index1)
confusion1

naivebayes_zh1 = wholeprocess(df_corp_zh, train_index1, test_index1, 0.97, 'zh')

naivebayes_zh1

confusion1 = naive.bayes(df_corp_zh, train_index1, test_index1)
confusion1

train_index2 = read.csv('Experimental dataset/dataset2train.csv')
test_index2 = read.csv('Experimental dataset/dataset2test.csv')
colnames(train_index2) <- c("doc_id", "topic")
colnames(test_index2) <- c("doc_id", "topic")

naivebayes_en2 = wholeprocess(df_corp, train_index2, test_index2, 0.97, 'en')

naivebayes_en2

confusion2 = naive.bayes(df_corp, train_index2, test_index2)
confusion2

naivebayes_zh2 = wholeprocess(df_corp_zh, train_index2, test_index2, 0.97, 'zh')

naivebayes_zh2

train_index3 = read.csv('Experimental dataset/dataset3train.csv')
test_index3 = read.csv('Experimental dataset/dataset3test.csv')
colnames(train_index3) <- c("doc_id", "topic")
colnames(test_index3) <- c("doc_id", "topic")

naivebayes_en3 = wholeprocess(df_corp, train_index3, test_index3, 0.97, 'en')

naivebayes_en3

confusion3 = naive.bayes(df_corp, train_index3, test_index3)
confusion3

naivebayes_zh3 = wholeprocess(df_corp_zh, train_index3, test_index3, 0.97, 'zh')

naivebayes_zh3

train_index4 = read.csv('Experimental dataset/dataset4train.csv')
test_index4 = read.csv('Experimental dataset/dataset4test.csv')
colnames(train_index4) <- c("doc_id", "topic")
colnames(test_index4) <- c("doc_id", "topic")

naivebayes_en4 = wholeprocess(df_corp, train_index4, test_index4, 0.97, 'en')

naivebayes_en4

confusion4 = naive.bayes(df_corp, train_index4, test_index4)
confusion4

naivebayes_zh4 = wholeprocess(df_corp_zh, train_index4, test_index4, 0.97, 'zh')

naivebayes_zh4
