prepCorpus<-function(corpus){
  library(tm)
  corpus<-tm_map(corpus, content_transformer(removePunctuation))
  corpus<-tm_map(corpus, content_transformer(removeNumbers))
  return(corpus)
}

hardClean<-function(corpus){
  corp.names<-names(corpus)
  text.vector<-NULL
  library(tm)
  corpus<-tm_map(corpus, tolower)
  for (i in 1:length(corpus)){
    curr.text<-corpus[[i]]
    curr.text<-unlist(strsplit(curr.text, ' '))
    curr.text<-paste(curr.text, collapse=' ')
    curr.text.split<-unlist(strsplit(curr.text, ''))
    dash.index<-which(curr.text.split == "-")
    curr.text.split[dash.index]<-' '
    bad.index<-which(!(curr.text.split %in% letters))
    space.index<-which(bad.index %in% which(curr.text.split == ' '))
    if (length(space.index) < length(bad.index)){
      bad.index<-bad.index[-space.index]
      curr.text.clean<-curr.text.split[-bad.index]
    } else {
      curr.text.clean<-curr.text.split
    }
    curr.text.clean<-paste(curr.text.clean, collapse='')
    text.vector<-c(text.vector, curr.text.clean)
  }
  text.vector.source<-VectorSource(text.vector)
  new.corpus<-Corpus(text.vector.source, readerControl=list(language="English"))
  names(new.corpus)<-corp.names
  return (new.corpus)
}

makeScaledDtm<-function(corpus, stop.words=F, sparsity=1){
  library(tm)
  if (stop.words == T){
    params<-list(stopwords=TRUE, wordLengths=c(1,Inf))
  } else {
    params<-list(wordLengths=c(1,Inf))
  }
  full.dtm<-DocumentTermMatrix(corpus)
  full.dtm<-as.matrix(full.dtm)
  scaling<-rowSums(full.dtm)
  corpus.dtm<-DocumentTermMatrix(corpus, control=params)
  if (sparsity!=1){
    corpus.dtm<-removeSparseTerms(corpus.dtm, sparsity)
  }
  corpus.dtm<-as.matrix(corpus.dtm)
  dtm.scaled<-corpus.dtm[,1:ncol(corpus.dtm)]/scaling
  return(dtm.scaled)
}

makeDtm<-function(corpus, stop.words=F, sparsity=1){
  library(tm)
  if (stop.words == T){
    params<-list(stopwords=TRUE, wordLengths=c(1,Inf))
  } else {
    params<-list(wordLengths=c(1,Inf))
  }
  corpus.dtm<-DocumentTermMatrix(corpus, control=params)
  if (sparsity!=1){
    corpus.dtm<-removeSparseTerms(corpus.dtm, sparsity)
  }
  corpus.dtm<-as.matrix(corpus.dtm)
  return(corpus.dtm)
}

sampler<-function(full.table, group.list, samp.size, samp='PerGroup'){
  new.table<-NULL
  new.groups<-NULL
  unique.groups<-levels(as.factor(group.list))
  for (i in 1:length(unique.groups)){
    #print(unique.groups[i])
    curr.group.index<-which(group.list == unique.groups[i])
    if (length(curr.group.index)>samp.size){
      curr.group.index<-curr.group.index[sample(1:length(curr.group.index), samp.size, replace=FALSE)]
    }
    curr.group<-full.table[curr.group.index,]
    curr.group.list<-group.list[curr.group.index]
    new.table<-rbind(new.table, curr.group)
    new.groups<-c(new.groups, as.character(curr.group.list))
  }
  final.list<-list(new.table,new.groups)
  return(final.list)
}

corpusExtract<-function(corpus){
  corpus.vector<-NULL
  for(i in 1:length(corpus)){
    curr.text<-corpus[[i]]$content
    corpus.vector<-c(corpus.vector, curr.text)
  }
  names(corpus.vector)<-names(corpus)
  return(corpus.vector)
}

xmlImport<-function(dir.source, meta.fields){
  require(XML)
  file.list<-list.files(dir.source, pattern=".xml")
  meta.table<-NULL
  for(i in 1:length(file.list)){
    curr.file<-paste(dir.source, file.list[i], sep='/')
    print(curr.file)
    import.file<-scan(curr.file, what='character', sep='\n', encoding="UTF-8")
    bad.index<-grep("\f", import.file)
    if(length(bad.index)>0){
      import.file<-import.file[-bad.index]
    }
    import.file<-paste(import.file, collapse=" ")
    text<-xmlTreeParse(import.file, useInternalNodes=T)
    text.vector<-NULL
    for(j in 1:length(meta.fields)){
      curr.field<-xpathSApply(text, paste("//*/", meta.fields[j], sep=''), xmlValue)
      text.vector<-c(text.vector, curr.field)
    }
    meta.table<-rbind(meta.table, text.vector)
  }
  colnames(meta.table)<-meta.fields
  return(meta.table)
}