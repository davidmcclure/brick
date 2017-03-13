soft_clean<-function(text){
  text.split<-unlist(strsplit(text, ''))
  print(length(text.split))
  #keep.char<-c(letters, LETTERS, '.', "?", "!", ",",";",":","'","\"","(",")","[","]","<",">", "&", ' ', '-', '\n')
  keep.char<-c(letters, LETTERS, '.', "?", "!", ",",";",":", "&", " ", "\n")
  keep.index<-which(text.split %in% keep.char)
  #print(length(keep.index))
  num.index<-which(!is.na(as.numeric(text.split)))
  full.index<-c(keep.index)
  text.clean<-text.split[full.index]
  text.clean<-paste(text.clean, collapse='')
  return(text.clean)
}

sentence_split<-function(text, separate="_", no.caps=T, max.sent=100){
  library (openNLP)
  library (openNLPdata)
  library (NLP)
  
  #define annotator based on Maxent OpenNLP package (sentence, word and part of speech)
  sent_annotate<-Maxent_Sent_Token_Annotator()
  
  #turn text into string
  text<-as.String(text)
  
  #divide text into sentences
  text.sent<-annotate(text, list(sent_annotate))
  
  #put sentences into vector of characters
  sent.vector<-text[text.sent]
  print(length(sent.vector))
  
  #collapse sentences into blocks of text max.sent long
  num.blocks<-length(sent.vector) %/% max.sent
  block.vector<-NULL
  start.pos<-1
  end.pos<-max.sent
  
  while (end.pos<=length(sent.vector)){
    curr.block<-sent.vector[start.pos:end.pos]
    block.char<-paste(curr.block, collapse=' ')
    block.vector<-c(block.vector, block.char)
    start.pos<-end.pos+1
    end.pos<-end.pos+max.sent
  }
  
  if(start.pos<length(sent.vector)){
    curr.block<-sent.vector[start.pos:length(sent.vector)]
    block.char<-paste(curr.block, collapse=' ')
    block.vector<-c(block.vector, block.char)
  }
  
  
  #remove unneeded varaibles to free space
  remove(text)
  remove(text.sent)
  remove(sent.vector)
  gc(verbose=FALSE)
  
  #variable to put attached text into
  full.text<-NULL
  
  #print(length(sent.vector))
  
  #loop through sentences and annotate with POS tagger
  for (i in 1:length(block.vector)){
    #print(i)
    curr.block.tagged<-pos_tagger(block.vector[i], separate=separate, no.caps=no.caps)
    gc(verbose=FALSE)
    full.text<-c(full.text, curr.block.tagged)
  }
  full.text<-paste(full.text, collapse=' ')
  return(full.text)
}

pos_tagger<-function(text, separate="_", no.caps=T){
  require(openNLP)
  require(openNLPdata)
  require(NLP)
  
  #define annotators based on Maxent OpenNLP package (sentence, word and part of speech)
  sent_annotate<-Maxent_Sent_Token_Annotator()
  word_annotate<-Maxent_Word_Token_Annotator()
  pos_annotate<-Maxent_POS_Tag_Annotator()
  
  #turn text into string
  text.s<-as.String(text)
  
  #create annotation variable
  token_annotation<-annotate(text.s, list(sent_annotate, word_annotate))
  pos_annotation<-annotate(text.s, pos_annotate, token_annotation)
  
  #retrieve words from annotation object
  pos_words<-subset(pos_annotation, type=="word")
  
  #create POS tags
  pos_tags<-sapply(pos_words$features, '[[', "POS")
  
  #convert original text to lowercase
  if(no.caps){
    text.s<-tolower(text.s)
  }
  
  #create sprintf separator
  sep.char<-paste("%s", separate, "%s", sep='')
  
  #bind tags to original text with separator
  tagged.string<-sprintf(sep.char, text.s[pos_words], pos_tags)
  
  #collapse the list of tagged words into character string
  tagged.text<-paste(tagged.string, collapse=" ")
  
  
  return(tagged.text)
}

pos_tag_file<-function(text, separate="_", no.caps=T){
  text<-unlist(strsplit(text, ''))
  sep.char.index<-which(text==separate)
  if (length(sep.char.index)>0){  
    text<-text[-sep.char.index]
  }
  text<-paste(text, collapse='')
  text<-soft_clean(text)
  #in.file<unlist(strsplit(in.file, ' '))
  file.tagged<-sentence_split(text, separate, no.caps)
  return(file.tagged)
}

pos_tag_dir<-function(in.dir, out.dir, separate="_", no.caps=T, resume=F){
  

  file.list<-list.files(path=in.dir, pattern=".txt")
  print(length(file.list))
  
  if(resume){
    outfile.list<-list.files(path=out.dir, pattern=".txt")
    outfile.list.stem<-unlist(strsplit(outfile.list, '_pos.txt'))
    file.list.stem<-unlist(strsplit(file.list, '.txt'))
    done.index<-which(file.list.stem %in% outfile.list.stem)
    file.list<-file.list[-done.index]
    print(length(file.list))
  }
  bad.files<-NULL
  for (i in 1:length(file.list)){
    print(file.list[i])
    file.path=paste(in.dir, file.list[i], sep="/")
    in.file<-scan(file.path, what="character", sep='\n')
    in.file<-paste(in.file, collapse=' ')
    in.file<-unlist(strsplit(in.file, ''))
    sep.char.index<-which(in.file==separate)
    if (length(sep.char.index)>0){  
      in.file<-in.file[-sep.char.index]
    }
    in.file<-paste(in.file, collapse='')
    in.file<-soft_clean(in.file)
    #in.file<unlist(strsplit(in.file, ' '))
    file.tagged<-try(sentence_split(in.file, separate, no.caps), silent=TRUE)
    if ('try-error' %in% class(file.tagged)){
      bad.files<-c(bad.files, file.list[i])
      next
    } else {
      file.name<-unlist(strsplit(file.list[i], '.txt'))
      new.file.name<-paste(file.name, "_pos.txt", sep='')
      out.path<-paste(out.dir, new.file.name, sep="/")
      write(file.tagged, out.path)
    }
  }
}

extractPOSTags<-function(pos.text, sep.tag="_", ret='POS', separate=F){
  text.split<-unlist(strsplit(pos.text, ' '))
  pos.split<-unlist(strsplit(text.split, sep.tag))
  pos.index<-seq(2, length(pos.split), by=2)
  text.index<-seq(1, length(pos.split), by=2)
  if (ret=="POS"){
    return.text<-pos.split[pos.index]
  } else {
    return.text<-pos.split[text.index]
  }
  if (separate==F){
    return.text<-paste(return.text, collapse=' ')
  }
  return(return.text)
}

#extract either POS tags or raw words from a POS tagged corpus (ret can either be POS or Text)
posCorpus<-function(corpus, sep.tag="_", ret='POS'){
  library(tm)
  new.vector<-NULL
  for (i in 1:length(corpus)){
    extracted.text<-extractPOSTags(corpus[[i]], sep.tag, ret)
    new.vector<-c(new.vector, extracted.text)
  }
  new.vectorsource<-VectorSource(new.vector)
  extracted.corpus<-Corpus(new.vectorsource)
  return(extracted.corpus)
}

writeAndConv<-function(text.vector, group.name){
  groups<-rep(group.name, length(text.vector))
  filenames<-paste(groups, "_", as.character(seq(1,length(groups), by=1)), ".txt", sep='')
  for (i in 1:length(text.vector)){
    converted.text<-iconv(text.vector[i], "UTF-8", "ASCII", sub='')
    write(converted.text, file=filenames[i])
  }
}

