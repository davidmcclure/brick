#global Dirs
#dropbox.path<-"D:/Dropbox"
#dropbox.path<-"C:/users/malgeehe/dropbox/Transfer/Suspense/POS Field Tags/NeuralNet/New Revised Model/Brick"
dropbox.path<-getwd()
#dropbox.path<-"/Users/markalgee-hewitt/Dropbox"

#scans a directory with tagged texts and tests each text with each tag label to find
#any texts with missing tag pairs
testTags<-function(tag.table, test.dir){
  file.list<-list.files(test.dir, pattern='.txt')
  for (i in 1:length(file.list)){
    print(file.list[i])
    curr.fn<-paste(test.dir, file.list[i], sep='/')
    tagged.text<-scan(curr.fn, what='character', sep='\n')
    tagged.text<-paste(tagged.text, collapse=' ')
    tagged.text<-unlist(strsplit(tagged.text, ' '))
    for(j in 1:length(tag.table)){
      print(tag.table[j])
      test.tags<-NULL
      test.tags<-getTags(tag.table[j], tagged.text)
      if (is.null(nrow(test.tags))){
        print('0')
      } else {
        print(as.character(nrow(test.tags)))
      }
    }
  }
}

softClean<-function(text){
  text<-tolower(text)
  text.split<-unlist(strsplit(text, ''))
  tag.symbol.field<-c('/', '<', '>', ' ', '=')
  letter.index<-which(text.split %in% c(letters, LETTERS, as.character(seq(0,9, by=1)), tag.symbol.field))
  text.clean.split<-text.split[letter.index]
  #text.clean.split<-text.split
  #text.clean.split[not.letter.index]<-' '
  text.clean<-paste(text.clean.split, collapse='')
  text.clean.split<-unlist(strsplit(text.clean, ' '))
  blank.index<-which(text.clean.split == "")
  if(length(blank.index>0)){
    text.clean.split<-text.clean.split[-blank.index]
  }
  text.clean<-paste(text.clean.split, collapse=' ')
  return(text.clean)
}

hardPOSClean<-function(pos.split){
  bad.index<-NULL
  for (i in 1:length(pos.split)){
    tag.split<-unlist(strsplit(pos.split[i], '_'))
    pos.part<-tag.split[2]
    curr.word<-tag.split[1]
    curr.word.split<-unlist(strsplit(curr.word, ''))
    good.index<-which(curr.word.split %in% c(letters, LETTERS))
    if (length(good.index)==0){
      bad.index<-c(bad.index, i)
    } else {
      curr.word.clean<-curr.word.split[good.index]
      curr.word<-paste(curr.word.clean, collapse='')
      curr.pos.tag<-paste(curr.word, pos.part, sep='_')
      pos.split[i]<-curr.pos.tag
    }
  }
  if (length(bad.index)>0){
    pos.words.clean<-pos.split[-bad.index]
  } else {
    pos.words.clean<-pos.split
  }
  return(pos.words.clean)
}

getTags<-function(sep.tag, curr.text){
  start.tag<-paste("<", sep.tag, ">", sep="")
  close.tag<-paste("</", sep.tag, ">", sep="")
  if (start.tag %in% curr.text){
    start.tag.index<-which(curr.text == start.tag)
    close.tag.index<-which(curr.text == close.tag)
    if (length(start.tag.index) == length(close.tag.index)){
      #make separate lists for each tag
      tag.matrix<-cbind(start.tag.index, close.tag.index)
      return(tag.matrix)
    } else {
      print("Invalid Tagging")
    }
  }
}

stripPOSPunct<-function(sep.text){
  other.tags<-c('_LS', '_POS', '_SYM', '_\'')
  other.index<-unlist(lapply(other.tags, function(x) grep(x, sep.text)))
  punct.index<-which(sep.text %in% c(',_,', '._.', '._?', '._!', '(_-LRB-', ')_-RRB-', ';_:', ':_:'))
  all.num.index<-grep('_CD', sep.text)
  all.num<-sep.text[all.num.index]
  all.num<-unlist(strsplit(all.num, '_'))
  num.part<-all.num[seq(1,length(all.num), by=2)]
  suppressWarnings(bad.num.index<-which(!is.na(as.numeric(num.part))))
  num.remove<-all.num.index[bad.num.index]
  remove.index<-c(other.index, punct.index, num.remove)
  strip.text<-sep.text[-remove.index]
  return(strip.text)
}

attachPOSPunct<-function(sep.text){
  remove.index<-c("-LRB-", "-RRB-", "LS", "SYM", "``", "''", "$")
  if(length(which(sep.text %in% remove.index))>0){
    sep.text<-sep.text[-which(sep.text %in% remove.index)]
  }
  punct.index<-which(sep.text %in% c(".", ",", ":"))
  prev.index<-punct.index-1
  bad.index<-which(prev.index<1)
  if(length(bad.index)>0){
    punct.index<-punct.index[-bad.index]
    prev.index<-prev.index[-bad.index]
  }
  attached.vals<-unlist(mapply(function(x,y) paste(x,y, sep="_"), sep.text[prev.index], sep.text[punct.index]))
  sep.text[prev.index]<-attached.vals
  sep.text<-sep.text[-punct.index]
  return(sep.text)
}

createWindows<-function(bin.vec, bin.size, bin.advance){
  #print(length(bin.vec))
  nbins<-100/bin.advance
  if(length(bin.vec)<nbins){
    print("Vector too small")
    return(NA)
  } else {
    bin.advance.size<-floor(((length(bin.vec) * bin.advance)/100))
    #print(bin.advance.size)
    bin.size.abs<-round(((length(bin.vec) * bin.size)/100),0)
    bin.starts<-seq(1,length(bin.vec), by=bin.advance.size)
    if((length(bin.vec)/bin.advance.size)>nbins){
      bin.starts<-bin.starts[1:nbins]
    }
    #return(bin.starts)
    bin.ends<-bin.starts+bin.size.abs
    bin.ends[which(bin.ends>=length(bin.vec))]<-length(bin.vec)
    bin.ends[length(bin.ends)]<-length(bin.vec)
    centroids<-mapply(function(x,y) x+(floor(((y-x)/2))), bin.starts, bin.ends, SIMPLIFY=F)
    centroids<-unlist(centroids)
    text.df<-data.frame(bin.starts, centroids, bin.ends)
    #print(text.df)
    #text.extracts<-mapply(function(x,y) bin.vec[x:y], bin.starts, bin.ends, SIMPLIFY = F)
    return(text.df)
  }
}

createWindows_old<-function(text.length, window, advance){
  #window.list<-list()
  #for(i in seq((window %/% 2),text.length, by=advance)){
  #  window.s<-i-((window %/% 2)+1)
  #  if(window.s<0){
  #    window.s<-1
  #  }
  #  window.e<-i+(window %/% 2)
  #  if (window.e>text.length){
  #    window.e<-text.length
  #  }
  #  window.list<-c(window.list, list(c(window.s, i, window.e)))
  #}
  #window.matrix<-matrix(unlist(window.list), ncol=3, byrow=T)
  #return(window.matrix)
  centroids<-seq((window %/% 2), text.length, by=advance)
  window.s<-centroids-((window %/% 2)+1)
  window.e<-centroids+(window %/% 2)
  if (window.s[1]<1){
    window.s[1]<-1
  }
  window.e<-window.e[which(window.e<text.length)]
  window.e[length(window.e)]<-text.length
  window.s<-window.s[1:length(window.e)]
  centroids<-centroids[1:length(window.e)]
  last.window.size<-window.e[length(window.e)]-window.s[length(window.s)]
  window.df<-data.frame(window.s, centroids, window.e)
  return(window.df)
}

dist.cal<-function(window.length, y.values){
  total.distance<-0
  for (i in 2:length(y.values)){
    curr.distance<-sqrt(((window.length)^2)+((y.values[i]-y.values[i-1])^2))
    total.distance<-total.distance+curr.distance
  }
  return(total.distance)
}

changes.cal<-function(y.values){
  total.changes<-0
  if ((y.values[2]-y.values[1])>0){
    curr.direction<-'increasing'
  } else if ((y.values[2]-y.values[1])<0){
    curr.direction<-'decreasing'
  } else {
    curr.direction<-'flat'
  }
  for(i in 3:length(y.values)){
    if((y.values[i]-y.values[i-1])>0){
      new.direction<-'increasing'
    } else if ((y.values[i]-y.values[i-1])<0){
      new.direction<-'decreasing'
    } else {
      new.direction<-'flat'
    }
    if (new.direction != curr.direction){
      total.changes<-total.changes+1
    }
    curr.direction<-new.direction
  }
  return(total.changes)
}

probWord<-function(word, text){
  text.length<-length(text)
  nword<-length(which(text==word))
  word.prob<-nword/text.length
  return(word.prob)
}

scaleFieldWordsMinus<-function(fields, slice, text){
  field.probs<-sapply(fields, probWord, text=text)
  field.obs<-sapply(fields, function(x) length(which(slice==x)))
  field.exp<-round(field.probs*length(slice), 0)
  field.sig<-field.obs-field.exp
  total.field<-sum(field.sig)
  if(total.field<0) total.field<-0
  return(total.field)
}

scaleFieldWordsDiv<-function(fields, slice, text){
  field.probs<-sapply(fields, probWord, text=text)
  print(field.probs)
  field.obs<-sapply(fields, function(x) length(which(slice==x)))
  print(field.obs)
  field.probs.scaled<-field.obs*field.probs
  print(field.probs.scaled)
  field.adj.slice<-field.probs.scaled*length(slice)
  print(field.adj.slice)
  total.field<-sum(field.adj.slice)
  return(total.field)
}

#takes two space-sparated files (1 with raw text, 1 with tags), list of fields, 
#maxtirx of tags and colours, moving window centroid, window size (if 'window.type' is 
#'percentage', then thse should have the respective percentages of the text, whose actual
#values will be calculated early in the script), type (either POS or Text) and a 
#ploy degree (or line) and returns a ggplot with the field presence in the moving window
#plotted as a scatter plot with lines and tags returned as shaded areas of the graph
#stats, if true, will include line length and number of turns (absolute) for each field in the legend
plotTaggedFields<-function(source.text, 
                           tag.text, 
                           fields, 
                           tags,
                           window.type='fixed',
                           window.advance,
                           window,
                           scale="minus",
                           text.name='Text',
                           type='POS', 
                           poly.degree='line', 
                           stats=T,
                           add.metrics=F,
                           output='chart'){
  library(ggplot2)
  #source("c:/users/malgeehe/dropbox/text mining/POS dev/POStagger.R")
  #source("c:/users/malgeehe/dropbox/text mining/matrixTransform.r")
  #source("d:/dropbox/text mining/POS dev/POStagger.R")
  #source("d:/dropbox/text mining/matrixTransform.r")
  source(paste(dropbox.path, "POStagger.R", sep='/'))
  source(paste(dropbox.path, "matrixTransform.r", sep='/'))
  tag.table.list<-list()
  bad.list<-NULL

  for (i in 1:nrow(tags)){
    curr.tags<-getTags(tags[i,1], tag.text)
    if (length(curr.tags)==0){
      bad.list<-c(bad.list, i)
    } else {
    tag.table.list<-c(tag.table.list, list(curr.tags))
    }
  }
  if (!is.null(bad.list)){
    tags<-tags[-bad.list,]
  }
  combined.tag.table<-NULL
  tag.names<-NULL
  real.names<-NULL
  if(length(tag.table.list)>0){
    for (i in 1:length(tag.table.list)){
      combined.tag.table<-rbind(combined.tag.table, tag.table.list[[i]])
      tag.names<-c(tag.names, rep(tags[i,2], nrow(tag.table.list[[i]])))
      real.names<-c(real.names, rep(tags[i,1], nrow(tag.table.list[[i]])))
    }
    combined.tag.df<-data.frame(combined.tag.table,tag.names, real.names)
    colnames(combined.tag.df)<-c('tag.start', 'tag.close', 'tag.name', 'real.name')
    names(tag.table.list)<-tags[,1]
  }
  field.table<-NULL
  centroids<-NULL
  if(add.metrics){
    pos.text<-source.text
    source.text<-stripPOSPunct(source.text)
    source.text<-hardPOSClean(source.text)
    pos.text<-extractPOSTags(pos.text, ret="POS", separate=T)
    pos.text<-attachPOSPunct(pos.text)
    pos.slices<-list()
  }
  converted.text<-extractPOSTags(source.text, ret=type, separate=T)
  cut.text<-NULL

  text.slices<-list()
  if (window.type=="percentage"){
    text.length=length(source.text)
    #window.advance=round(((text.length*window.advance)/100), 0)
    #print(window.advance)
    #window=round(((text.length*window)/100),0)
    #window<-round((text.length * (window/100)),0)
    #window.advance<-round((text.length * (window.advance/100)), 0)
  }
  text.windows<-createWindows(text.length, window, window.advance)
  
#  for (i in seq(1, length(source.text), by=window.advance)){
  for (i in 1:nrow(text.windows)){
    centroids<-c(centroids, text.windows[i,2])
    window.min<-text.windows[i,1]
#   if(window.min < 1){
#    window.min<-1
#  }
#    window.max<-(i+(window/2))
    window.max<-text.windows[i,3]
#    if (window.max>length(source.text)){
#      window.max<-length(source.text)
#    }
    #print(paste("Slice ", as.character(i), ": ", as.character(window.min), " to ", as.character(window.max)))
    curr.slice<-converted.text[window.min:window.max]
    #retrieve pos information for each slice
    if(add.metrics){
      curr.pos.slice<-pos.text[window.min:window.max]
      pos.slices<-c(pos.slices, list(curr.pos.slice))
    }
    slice.paste<-paste(curr.slice, collapse=' ')
    cut.text<-c(cut.text, slice.paste)
    text.slices<-c(text.slices, list(curr.slice))
  }
  
  require(tm)
  source(paste(dropbox.path, "tm_commands.r", sep='/'))
  #source("c:/users/malgeehe/dropbox/text mining/tm_commands.r")
  collapsed.text<-sapply(text.slices, function(x) paste(x, collapse=' '))
  collapsed.text<-unlist(collapsed.text)
  slice.corp<-Corpus(VectorSource(collapsed.text))
  slice.dtm<-makeDtm(slice.corp)
  slice.features<-colSums(slice.dtm)
  slice.probs<-slice.features/sum(slice.dtm)
  
  text.scores<-lapply(text.slices, function(x) sliceCal(x, fields, slice.probs))
  
  field.table<-matrix(unlist(text.scores), ncol=ncol(fields), byrow=T)
  colnames(field.table)<-colnames(fields)

  zero.index<-which(colSums(field.table)==0)
  if (length(zero.index>0)){
    field.table<-field.table[,-which(colSums(field.table)==0)]
  }
  rownames(field.table)<-NULL
  line.lengths<-rep(0,ncol(field.table))
  n.turns<-rep(0, ncol(field.table))
  for (i in 1:length(line.lengths)){
    #print(i)
    #print(dim(field.table))
    line.lengths[i]<-dist.cal(1, field.table[,i])
    n.turns[i]<-changes.cal(field.table[,i])
  }
  line.lengths<-round(line.lengths, 2)
  legend.names<-colnames(fields)
  if (length(zero.index>0)){
    legend.names<-legend.names[-zero.index]
  }
  final.legend.names<-paste(legend.names, "; Length=", as.character(line.lengths), "; Turns=", as.character(n.turns), sep='')
  colnames(field.table)<-legend.names
  complete.field.table<-data.frame(centroids, field.table)
  output.table<-complete.field.table
  
  #additional metrics calculated for neural network classifciation
  if(add.metrics){
    narrative.perc<-calNarrativePerc(centroids, length(source.text))
    output.table$narrative_percent<-narrative.perc
    pos.scores<-lapply(pos.slices, function(x) posCal(x))
    pos.tags<-c("CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "MD", "NN", "NNS", "NNP", "NNPS", "PDT", "PRP", "PRPos", "RB", "RBR", "RBS", "RP", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WPPos", "WRB", "Sentence_end", "Clause_marker", "Sentence_break")
    pos.table<-matrix(unlist(pos.scores), ncol=length(pos.tags), byrow=T)
    colnames(pos.table)<-pos.tags
    output.table<-cbind(output.table, pos.table)
    ttr<-lapply(pos.slices, function(x) length(unique(x))/length(x))
    output.table$ttr<-unlist(ttr)
    
  }
  
  #find which centroids in output table lie within tag boundaries and add to output.table
  centroid.tags<-rep("None", nrow(output.table))
  if (length(tag.table.list)>0){
    tag.wordlist<-mapply(function(x,y) seq(x,y,by=1), combined.tag.df$tag.start, combined.tag.df$tag.close)
    tag.wordlist<-unlist(tag.wordlist)
    tag.namelist<-mapply(function(x,y,z) rep(z, ((y-x)+1)), combined.tag.df$tag.start, combined.tag.df$tag.close, combined.tag.df$real.name)
    names(tag.wordlist)<-unlist(tag.namelist)
    centroid.tags[which(output.table$centroids %in% tag.wordlist)]<-names(tag.wordlist[which(tag.wordlist %in% output.table$centroids)])
  }
  output.table$tags<-centroid.tags
  
  output.table$text<-cut.text
  plot.fields<-melt(complete.field.table)
  plot.title<-paste("Plot of Fields in", text.name, sep=' ')
  y.label<-"Percent of Words in Window"
  text.plot<-ggplot()
  scatter.plot<-text.plot+geom_point(data=plot.fields, aes(x=x.values, y=y.values, color=groups))#, shape=groups))
  if (poly.degree=='line'){
    line.plot<-scatter.plot+geom_line(data=plot.fields, aes(x=x.values, y=y.values, color=groups))
  } else {
    line.plot<-scatter.plot+stat_smooth(data=plot.fields, method='lm', formula=y~poly(x,poly.degree), se=F, aes(x=x.values, y=y.values, color=groups))
  }
  legend.plot<-line.plot+scale_color_discrete(name="Fields", breaks=unique(plot.fields$groups), labels=final.legend.names)
  #legend.plot<-legend.plot+scale_shape_discrete(name="Fields", breaks=unique(plot.fields$groups), labels=final.legend.names)
    #tag.plot<-line.plot
  if(length(tag.table.list)>0){
    #tag.plot<-legend.plot+geom_rect(data=combined.tag.df, aes(xmin=tag.start, xmax=tag.close, ymin=0, ymax=Inf), fill=combined.tag.df$tag.name, alpha=0.3)
    tag.plot<-text.plot+ylim(c(0,1))+geom_rect(data=combined.tag.df, aes(xmin=tag.start, xmax=tag.close, ymin=0, ymax=Inf), fill=combined.tag.df$tag.name, alpha=0.3)
    #tag.plot<-line.plot+geom_rect(data=combined.tag.df, aes(xmin=tag.start, xmax=tag.start, ymin=0, ymax=Inf, fill=tag.name), alpha=0.1)
    tag.labels.vector<-NULL
    for (i in 1:nrow(tags)){
      curr.label<-paste('Field ', tags[i,1], '=', tags[i,2], sep=' ')
     tag.labels.vector<-c(tag.labels.vector, curr.label)
    }
    #changed when moved to new tagging system
    #final.labels<-paste(tag.labels.vector, collapse='\n')
    suspense.range<-paste("Tag Range: ", tags[1,1], "-", tags[nrow(tags),1], sep='')
    window.information<-paste("Graph shows percentage of field in window size of ", as.character(window), " words, advanced by ", as.character(window.advance),"words", sep='')
    final.labels<-paste(suspense.range, window.information, sep="\n")
  } else {
    tag.plot<-line.plot
    final.labels<-'No Tags'
  }
  require(grid)
  title.plot<-tag.plot+ggtitle(plot.title)+xlab(final.labels)+ylab(y.label)#theme(text=element_text(size=60), legend.key.size=unit(6, 'lines'))
  if (output=='chart'){
    output.list<-list(title.plot, output.table)
    return(output.list)
  } else if (output=='table'){
    if(length(tag.table.list)==0){
      combined.tag.df<-c("No Tags")
    }
    complete.field.table<-complete.field.table[,-1]
    table.list<-list(complete.field.table, combined.tag.df, output.table)
    return (table.list)
  }
  
  #return(field.table)
}

multi.field.plot<-function(source.text.dir, 
                           tag.text.dir, 
                           output.dir,
                           fields, 
                           tags,
                           window.type='fixed',
                           window.advance,
                           window,
                           type='text', 
                           poly.degree='line',
                           convert.tags=T,
                           add.metrics=F,
                           output='chart'){
  source.file.list<-list.files(source.text.dir)
  #print(source.file.list)
  tag.file.list<-list.files(tag.text.dir)
  complete.output.table<-data.frame()
  for (i in 1:length(tag.file.list)){
    print(tag.file.list[i])
    curr.source.text<-scan(file=paste(source.text.dir, source.file.list[i], sep='/'), what='character', sep='\n', encoding='UTF-8')
    curr.source.text<-paste(curr.source.text, collapse=' ')
    curr.source.text<-unlist(strsplit(curr.source.text, ' '))
    if (type=='POS'){
      curr.source.text<-stripPOSPunct(curr.source.text)
    }
    if (!add.metrics){
      curr.source.text<-hardPOSClean(curr.source.text)
    }
    curr.tag.text<-scan(file=paste(tag.text.dir, tag.file.list[i], sep='/'), what='character', sep='\n', encoding='UTF-8')
    curr.tag.text<-paste(curr.tag.text, collapse= ' ')
    curr.tag.text<-softClean(curr.tag.text)
    if (convert.tags){
      curr.tag.text<-convertTagsText(curr.tag.text)
    }
    write(curr.tag.text, file="TestTag.txt")
    curr.tag.text<-unlist(strsplit(curr.tag.text, ' '))
    text.name<-unlist(strsplit(source.file.list[i], '.txt'))
    text.name.split<-unlist(strsplit(text.name, ' - '))
    text.name<-text.name.split[2]
    pdf.file.name<-paste(text.name, '.pdf', sep='')
    pdf.file.name<-paste(output.dir, pdf.file.name, sep='/')
    table.file.name<-paste(text.name, '.csv', sep='')
    table.file.name<-paste(output.dir, table.file.name, sep='/')
    curr.plot<-plotTaggedFields(source.text=curr.source.text,
                                tag.text=curr.tag.text,
                                fields=fields,
                                tags=tags,
                                window.type=window.type,
                                window.advance=window.advance,
                                window=window,
                                text.name=text.name,
                                type=type,
                                poly.degree=poly.degree,
                                add.metrics=add.metrics,
                                output=output)
  if (output=='chart'){
      table.plot<-curr.plot[[1]]
      pdf(pdf.file.name, height=10, width=30)
      print(table.plot)
      dev.off()
      output.table<-curr.plot[[2]]
      write.csv(output.table, table.file.name)
    } else {
      output.table<-curr.plot[[3]]
      Text.Name<-rep(table.file.name, nrow(output.table))
      output.table<-cbind(Text.Name, output.table)
      complete.output.table<-rbind(complete.output.table, output.table)
      write.csv(output.table, table.file.name)
    }
  }
  if(output=='table'){
    return(complete.output.table)
  }
}

multiRunFields<-function(source.text.dir, 
                           tag.text.dir, 
                           output.dir.base,
                           fields,
                           field.control.table,
                           tags,
                           window.type='percentage',
                           window.advance,
                           window,
                           type='text', 
                           poly.degree='line',
                           convert.tags=T){

  unique.fields<-unique(field.control.table$FieldClass)
  for (i in 1:length(unique.fields)){
    #print(unique.fields[i])
    new.out.dir<-paste(output.dir.base, unique.fields[i], sep="_")
    dir.create(new.out.dir)
    sub.field.list<-field.control.table[which(field.control.table$FieldClass==unique.fields[i]),1]
    sub.field.table<-fields[,which(colnames(fields)%in%sub.field.list)]
    multi.field.plot(source.text.dir=source.text.dir, 
                               tag.text.dir=tag.text.dir, 
                               output.dir=new.out.dir,
                               fields=sub.field.table, 
                               tags=tags,
                               window.type=window.type,
                               window.advance=window.advance,
                               window=window,
                               type=type, 
                               poly.degree=poly.degree,
                               convert.tags=convert.tags)
  }
  
}

cor.multi.field.plot.table<-function(source.text.dir, 
                                     tag.text.dir, 
                                     output.dir,
                                     fields, 
                                     tags,
                                     window.type='fixed',
                           window.advance,
                           window,
                           type='POS', 
                           poly.degree='line'){
  correlation.tables<-NULL
  text.names<-NULL
  source.file.list<-list.files(source.text.dir)
  #print(source.file.list)
  tag.file.list<-list.files(tag.text.dir)
  for (i in 1:length(tag.file.list)){
    print(tag.file.list[i])
    curr.source.text<-scan(file=paste(source.text.dir, source.file.list[i], sep='/'), what='character', sep='\n')
    curr.source.text<-paste(curr.source.text, collapse=' ')
    curr.source.text<-unlist(strsplit(curr.source.text, ' '))
    curr.source.text<-stripPOSPunct(curr.source.text)
    curr.source.text<-hardPOSClean(curr.source.text)
    curr.tag.text<-scan(file=paste(tag.text.dir, tag.file.list[i], sep='/'), what='character', sep='\n')
    curr.tag.text<-paste(curr.tag.text, collapse= ' ')
    curr.tag.text<-softClean(curr.tag.text)
    curr.tag.text<-unlist(strsplit(curr.tag.text, ' '))
    text.name<-unlist(strsplit(source.file.list[i], '.txt'))
    text.name.split<-unlist(strsplit(text.name, ' - '))
    test.name<-text.name.split[2]
    text.names<-c(text.names, test.name)

    curr.table.list<-plotTaggedFields(source.text=curr.source.text,
                                tag.text=curr.tag.text,
                                fields=fields,
                                tags=tags,
                                window.type=window.type,
                                window.advance=window.advance,
                                window=window,
                                text.name=text.name,
                                type=type,
                                poly.degree=poly.degree,
                                output='table')
    
    file.name.fields<-paste(text.name, '_fields_correlation', '.csv', sep='')
    file.name.fields<-paste(output.dir, file.name.fields, sep='/')
    correlation.table<-as.matrix(cor(curr.table.list[[1]]))
    correlation.tables<-c(correlation.tables, list(correlation.table))
    #file.name.tags<-paste(text.name, '_tags', '.csv', sep='')
    #file.name.tags<-paste(output.dir, file.name.tags, sep='/')
    write.csv(correlation.table, file=file.name.fields)
    #write.csv(curr.table.list[[2]], file=file.name.tags)
    
  }
  names(correlation.tables)<-text.names
  return(correlation.tables)
}

makeEndTag<-function(tag){
  tag.split<-unlist(strsplit(tag, ''))
  tag.split<-tag.split[-c(1,length(tag.split))]
  tag.split<-paste(tag.split, collapse='')
  tag.split<-paste("</", tag.split, ">", sep='')
  return(tag.split)
}

convertTagAttr<-function(curr.tag){
  curr.tag<-unlist(strsplit(curr.tag, ''))
  start.tag=2
  num.attr<-suppressWarnings(curr.tag[which(!is.na(as.numeric(curr.tag)))])
  if(length(num.attr>0)){
    num.attr<-paste(num.attr, collapse='')
    space.index<-which(curr.tag==' ')
    end.tag<-space.index[1]-1
    new.tag<-paste(curr.tag[start.tag:end.tag], collapse='')
    new.tag<-paste(new.tag, num.attr, sep='')
    new.tag<-paste("<", new.tag, ">", sep='')
  } else {
    new.tag<-paste(curr.tag, collapse='')
  }
  return(new.tag)
}

convertTagsText<-function(curr.text){
  text.split<-unlist(strsplit(curr.text," "))
  open.tag.index<-grep("<", text.split)
  close.tag.index<-grep(">", text.split)
  end.tag.index<-grep("/", text.split)
  open.tag.index<-open.tag.index[-which(open.tag.index %in% end.tag.index)]
  close.tag.index<-close.tag.index[-which(close.tag.index %in% end.tag.index)]
  #combined.tags<-mapply(function(x,y) paste(x, y, sep=" "), text.split[open.tag.index], text.split[close.tag.index])
  combined.tags<-mapply(function(x,y) paste(text.split[x:y], collapse=" "), open.tag.index, close.tag.index)
  converted.tags<-sapply(combined.tags, convertTagAttr)
  converted.end.tags<-sapply(converted.tags, makeEndTag)
  text.split[open.tag.index]<-converted.tags
  text.split[end.tag.index]<-converted.end.tags
  text.split<-text.split[-close.tag.index]
  text.split<-paste(text.split, collapse=" ")
  return(text.split)
}

buildColorTable<-function(primary.tag, num.levels, color.range=c("blue", "red")){
  color.build<-colorRampPalette(color.range)
  colors<-color.build(num.levels)
  tags<-paste(rep(primary.tag, num.levels), seq(1, num.levels, by=1), sep='')
  color.table<-cbind(tags, colors)
  color.table<-as.data.frame(color.table, stringsAsFactors=F)
  colnames(color.table)<-c('Tag', 'Color')
  return(color.table)
}

corpusExtract<-function(corpus){
  corpus.vector<-NULL
  for(i in 1:length(corpus)){
    curr.text<-corpus[[i]]$content
    curr.text<-paste(curr.text, collapse=" ")
    corpus.vector<-c(corpus.vector, curr.text)
  }
  names(corpus.vector)<-names(corpus)
  return(corpus.vector)
}

#given a corpus and a table of tags, creates a new corpus out of all tags, labels the groups according to the table
#tag.groups (which shoudl contain a column per group, each column all of the tags that belong in that group). If working with
#differently tagged corpora, not all of which need to be converted, then program can take an additional corpus of texts
#that need to be converted, converts them and the runs the tag MDW on all of the texts. 
multiTagMDW<-function(tagged.corpus, tag.groups, convert.corpus=NULL){
  require(tm)
  #source("/Volumes/G-Raid/Dropbox/Text Mining/GetTaggedText.r")
  source(paste(dropbox.path, "Text Mining/GetTaggedText.r", sep='/'))
  if (!is.null(convert.corpus)){
    print("Converting Corpus")
    convert.vector<-corpusExtract(convert.corpus)
    #print(length(convert.vector))
    convert.vector<-unlist(lapply(convert.vector, softClean))
    converted.vector<-unlist(lapply(convert.vector, convertTagsText))
    tagged.corpus<-corpusExtract(tagged.corpus)
    tagged.corpus<-c(tagged.corpus, converted.vector)
    tagged.corpus<-Corpus(VectorSource(tagged.corpus))
  }
  final.tag.names<-colnames(tag.groups)
  final.corpus<-NULL
  group.names<-NULL
  print("Extracting Tags")
  for (i in 1:length(final.tag.names)){
    #print(tagged.corpus)
    #curr.tag.corpus<-lapply(tag.groups[,i], taggedToCorpusMulti(), initial.corpus=tagged.corpus, output="vector")
    #curr.tag.corpus<-unlist(curr.tag.corpus)
    curr.tag.corpus<-NULL
    for (j in 1:length(tag.groups[,i])){
      print(tag.groups[j,i])
      curr.tag<-taggedToCorpusMulti(tagged.corpus, tag.groups[j,i], output="vector")
      print(length(curr.tag))
      curr.tag.corpus<-c(curr.tag.corpus,curr.tag)
    }
    final.corpus<-c(final.corpus, curr.tag.corpus)
    group.names<-c(group.names, rep(final.tag.names[i], length(curr.tag.corpus)))
  }
  final.corpus<-Corpus(VectorSource(final.corpus))
  print("Finding MDWs")
  #source("/Volumes/G-Raid/Dropbox/Text Mining/getGroupMDWs.r")
  source(paste(dropbox.path, "Text Mining/getGroupMDWs.r", sep='/'))
  #return(final.corpus)
  tag.MDWs<-getGroupMDWs(final.corpus, group.names, exclude.zero="Group")
  tag.MDWs<-t(tag.MDWs)
  return(tag.MDWs)
}

#wrapper for running multiple corpora, some in need of conversion and some not through
#"multi.field.plot" generates one large table as writes it to csv.

multi.corpus.plot<-function(source.text.dir, 
                           tag.text.dir,
                           convert.source.dir,
                           convert.tag.dir,
                           output.dir,
                           fields, 
                           tags,
                           window.type='fixed',
                           window.advance,
                           window,
                           type='text'){
  complete.feature.table<-data.frame()
  curr.output<-multi.field.plot(convert.source.dir, 
                                convert.tag.dir, 
                                output.dir,
                                fields, 
                                tags,
                                window.type=window.type,
                                window.advance=window.advance,
                                window=window,
                                type=type, 
                                poly.degree='line',
                                convert.tags=T,
                                output='table')
  complete.feature.table<-rbind(complete.feature.table, curr.output)
  curr.output<-multi.field.plot(source.text.dir, 
                             tag.text.dir, 
                             output.dir,
                             fields, 
                             tags,
                             window.type=window.type,
                             window.advance=window.advance,
                             window=window,
                             type=type, 
                             poly.degree='line',
                             convert.tags=F,
                             output='table')
  complete.feature.table<-rbind(complete.feature.table, curr.output)
  write.csv(complete.feature.table, file="NeuralNet/New Model/NewMasterFeatureTable.csv")
  return(complete.feature.table)
}
  
textTagAlign<-function(old.tag, text.name, text.type.table, all.tags){
  if (old.tag=="None"){
    new.tag<-old.tag
  } else {
    if(old.tag %in% all.tags[5:14,2]){
      new.tag<-all.tags[which(all.tags[,2]==old.tag),1]
    } else {
      if(text.type.table[which(text.type.table[,1]==text.name),2]=="U"){
        tag.part<-all.tags[3:4,]
      } else if(text.type.table[which(text.type.table[,1]==text.name),2]=="S"){
        tag.part<-all.tags[1:2,]
      }
      new.tag<-tag.part[which(tag.part[,2]==old.tag),1]
    }
  }
  return(new.tag)
}  


#calculates the change between successive values of narrative windows over a single text. Function
#takes an original table, a column indicating the titles and a vector of column numbers that
#contain the variables
differenceTable<-function(table.orig, title.col, vars){
  out.table<-data.frame()
  unique.texts<-levels(as.factor(table.orig[,title.col]))
  for (i in 1: length(unique.texts)){
    print(unique.texts[i])
    text.table<-table.orig[which(table.orig[,title.col]==unique.texts[i]),]
    for (j in 2:nrow(text.table)){
      diff.var<-text.table[j,vars]-text.table[j-1,vars]
      text.table[j,vars]<-diff.var
    }
    text.table<-text.table[-1,]
    out.table<-rbind(out.table, text.table)
  }
  return(out.table)
}


fieldCal<-function(field.vector, split.text, prob.list){
  field.vector<-field.vector[which(field.vector!="")]
  exp.vector<-prob.list[which(names(prob.list) %in% field.vector)]
  exp.vector<-prob.list*length(split.text)
  hit.list<-split.text[which(split.text %in% field.vector)]
  hit.table<-table(hit.list)
  if (length(hit.table)>0){
    #print(names(hit.table))
    exp.hits<-exp.vector[which(names(exp.vector) %in% names(hit.table))]
    obs.exp<-hit.table-exp.hits
    field.value<-sum(obs.exp)
    if(field.value<0) field.value<-0
    field.perc<-(field.value/length(split.text))*100
    
  } else {
    field.perc<-0
  }
  return(field.perc)
}

sliceCal<-function(split.text, fields, slice.probs){
  field.list<-list()
  for(i in 1:ncol(fields)){
    field.list<-c(field.list, list(fields[,i]))
  }
  slice.scores<-lapply(fields, function(x) fieldCal(x, split.text, slice.probs))
  slice.scores<-unlist(slice.scores)
  return(slice.scores)
}

#function takes a split text and instructions on creating overlapping windows, it also takes
#a classification model, a set of fields and a boolean indicating if aoa scores are also to
#be calculated as part of the model. The function returns a tagged text
autoTag<-function(source.text, div.size, div.advance, div.type='percentage', class.model, class.fields, aoa=F, pos=F, plot.type='bar', add.metrics=T, smooth.plot=F){
  #print("Calculating POS values")
  #source(paste(dropbox.path, "POS.R", sep='/'))
  #source.text<-pos_tag_file(text)
  #source.text<-unlist(strsplit(source.text, ' '))
  print("Calculating Metrics")
  
  if(add.metrics){
    text.char<-stripPOSPunct(source.text)
    text.char<-hardPOSClean(text.char)
    text.char<-extractPOSTags(text.char, ret="text", separate=T)
    pos.text<-extractPOSTags(source.text, ret="POS", separate=T)
    text.pos<-attachPOSPunct(pos.text)
  } else {
    text.char<-extractPOSTags(text.char, ret="text", separate=T)
  }
  #clean text?
  
  if (div.type=="percentage"){
    text.length=length(text.char)
    window<-round((text.length * (div.size/100)),0)
    window.advance<-round((text.length * (div.advance/100)), 0)
  }
  text.windows<-createWindows(text.char, div.size, div.advance)
  #text.windows<-createWindows_old(length(text.char), window, window.advance)
  text.slices<-mapply(function(x,y) text.char[x:y], text.windows[,1], text.windows[,3])
  if (add.metrics){
    pos.slices<-mapply(function(x,y) text.pos[x:y], text.windows[,1], text.windows[,3])
  }
  centroids<-text.windows[,2]

  require(tm)
  source(paste(dropbox.path, "tm_commands.r", sep='/'))
  #source("c:/users/malgeehe/dropbox/text mining/tm_commands.r")
  #source("d:/dropbox/text mining/tm_commands.r")
  collapsed.text<-sapply(text.slices, function(x) paste(x, collapse=' '))
  collapsed.text<-unlist(collapsed.text)
  slice.corp<-Corpus(VectorSource(collapsed.text))
  slice.dtm<-makeDtm(slice.corp)
  slice.features<-colSums(slice.dtm)
  slice.probs<-slice.features/sum(slice.dtm)

  text.scores<-lapply(text.slices, function(x) sliceCal(x,class.fields, slice.probs))
  
  test.score.matrix<-matrix(unlist(text.scores), ncol=ncol(class.fields), byrow=T)
  colnames(test.score.matrix)<-colnames(class.fields)
  test.score.matrix<-as.data.frame(test.score.matrix, stringsAsFactors=F)

  if (aoa){
    #source("c:/users/malgeehe/Dropbox/Text Mining/AoAcalc.R")
    source(paste(dropbox.path, "text mining/AoAcalc.R", sep='/'))
    #source("d:/Dropbox/Text Mining/AoAcalc.R")
    aoa.scores<-windowAoA(source.text=text, window=div.size, window.advance=div.advance, window.type=div.type, POS=pos)
    test.score.matrix<-cbind(test.score.matrix, aoa.scores[,2])
    colnames(test.score.matrix)<-c(colnames(class.fields), "aoa.scores")
  }
  if(add.metrics){
    narrative.perc<-calNarrativePerc(centroids, length(text.char))
    test.score.matrix$narrative_percent<-narrative.perc
    pos.scores<-lapply(pos.slices, function(x) posCal(x))
    pos.tags<-c("CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "MD", "NN", "NNS", "NNP", "NNPS", "PDT", "PRP", "PRPos", "RB", "RBR", "RBS", "RP", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WPPos", "WRB", "Sentence_end", "Clause_marker", "Sentence_break")
    pos.table<-matrix(unlist(pos.scores), ncol=length(pos.tags), byrow=T)
    colnames(pos.table)<-pos.tags
    test.score.matrix<-cbind(test.score.matrix, pos.table)
    ttr<-lapply(pos.slices, function(x) length(unique(x))/length(x))
    test.score.matrix$ttr<-unlist(ttr)
  }
  test.score.matrix[is.na(test.score.matrix)]<-0

  #old code
  #source("c:/users/malgeehe/Dropbox/Text Mining/NeuralnetFunctions.R")
  #source("d:/Dropbox/Text Mining/NeuralnetFunctions.R")
  #model.results<-netClass(net.train=class.model, new.var=test.score.matrix, test=F)
  #model.results<-unlist(model.results)
  
  
  #model.results[which(model.results==1)]<-"suspense"
  #model.results[which(model.results==2)]<-"unsuspense"
  #end old code
  #new code
  model.results<-netClassDiff(net.train=class.model, new.var=test.score.matrix, var.names=c("highSuspense", "lowSuspense"))

  
  #build tag table
  
  color.build<-colorRampPalette(c("blue", "red"))
  colors<-color.build(8)
  low.colors<-colors[1:4]
  high.colors<-colors[5:8]
  final.colors<-c(high.colors, low.colors)

  color.tags<-c("highSuspense1", "highSuspense2", "highSuspense3", "highSuspense4", "lowSuspense1", "lowSuspense2",  "lowSuspense3", "lowSuspense4")
  
  if (plot.type=='bar'){
    color.table<-cbind(color.tags, final.colors)
  } else {
    number.sequence<-c(5,6,7,8,1,2,3,4)
    color.table<-cbind(color.tags, number.sequence)
  }
  if(plot.type=='line'){
    #tag.vector<-lapply(model.res)
  }

  
  final.text=''
  
  clean.text.slices<-unlist(lapply(text.slices, function(x) paste(x, collapse=' ')))
  
  for(i in 1:length(clean.text.slices)){
    curr.tag<-model.results[i]
    if(curr.tag!="None"){
      open.tag<-paste("<", curr.tag, ">", sep='')
      close.tag<-paste("</", curr.tag, ">", sep='')
      final.text<-paste(final.text, open.tag, clean.text.slices[i], close.tag, sep=' ')
    } else {
      final.text<-paste(final.text, clean.text.slices[i])
    }
  }
  #tags<-levels(factor(model.results))
  #tags<-cbind(tags, c('#FF0000', '#0000FF'))
  split.final<-unlist(strsplit(final.text, ' '))
  name.text<-unlist(strsplit(text.char, ' '))
  text.name<-paste(name.text[1], name.text[2], name.text[3], sep=" ")
  tag.plot<-plotTags(split.final, color.table, text.name, plot.type=plot.type, smooth.plot=smooth.plot)
  library(openNLP)
  if(plot.type=="line"){
    for(i in 1:nrow(color.table)){
      curr.tag<-color.table[i,1]
      if(length(which(model.results==curr.tag)>0)){
        model.results[which(model.results==curr.tag)]<-color.table[i,2]
      }
    }
  }
  results.table<-cbind(model.results, clean.text.slices)
  if (plot.type=="line"){
    plot<-tag.plot[[1]]
    total.suspense<-tag.plot[[2]]
    suspense.vector<-tag.plot[[3]]
    output.list<-list(plot, final.text, total.suspense, results.table, test.score.matrix, suspense.vector)
  } else {
    output.list<-list(tag.plot, final.text, list(results.table), list(test.score.matrix))
  }
  return(output.list)
}

plotPDF<-function(pdf.filename, plot, h=10, w=25){
  pdf(pdf.filename, height=h, width=w)
  print(plot)
  dev.off()
}

extractDate<-function(text.string){
  text.string<-unlist(strsplit(text.string, '.txt'))
  char.vec<-unlist(strsplit(text.string, ''))
  date.vector<-char.vec[(length(char.vec)-3):(length(char.vec))]
  #print(date.vector)
  date.vector<-paste(date.vector, collapse='')
  date<-as.numeric(date.vector)
  return(date)
}

#requires filenames with the date in the final position
sortFiles<-function(file.list){
  file.dates<-lapply(file.list, function(x) extractDate(x))
  file.dates<-unlist(file.dates)
  files.ordered<-file.list[order(file.dates)]
  return(files.ordered)
}

#function takes a directory of texts (either pos or not) and uses a model to tag all texts and output them to a new directory
multiAutotag<-function(indir, 
                       outdir.plot, 
                       outdir.text, 
                       div.size, 
                       div.advance, 
                       div.type='percentage', 
                       class.model, 
                       class.fields, 
                       aoa=T, 
                       pos=F, 
                       plot.type="bar"){
  file.list<-list.files(indir, pattern='.txt')
  
  #sorts files into dates assuming date is the last four characters
  file.list<-sortFiles(file.list)
  
  file.list<-paste(indir, file.list, sep='/')
  text.list<-lapply(file.list, function(x) scan(x, what='character', sep='\n', encoding='UTF-8'))
  if (pos){
    text.list<-lapply(text.list, function(x) paste(x, collapse=' '))
    text.list<-lapply(text.list, function(x) unlist(strsplit(x, ' ')))
    text.list<-lapply(text.list, function(x) hardPOSClean(x))
  } else {
    text.list<-lapply(text.list, function(x) paste(x, collapse=' '))
  }
  tagged.texts<-lapply(text.list, function(x) autoTag(x, div.size=div.size, div.advance=div.advance, div.type=div.type, class.model=class.model, class.fields=class.fields, tags=tags, aoa=aoa, pos=pos, plot.type=plot.type))
  file.list<-as.list(file.list)
  raw.text.names<-lapply(file.list, function(x) unlist(strsplit(x, '.txt')))
  raw.text.names<-lapply(raw.text.names, function(x) unlist(strsplit(x, '/')))
  #print(raw.text.names)
  #print(length(raw.text.names))
  raw.text.names<-lapply(raw.text.names, function(x) x[length(raw.text.names[[1]])])
  text.names<-paste(raw.text.names, '_autotagged.txt', sep='')
  plot.names<-paste(raw.text.names, '_autotagged_plots.pdf', sep='')
  text.names<-paste(outdir.text, text.names, sep='/')
  plot.names<-paste(outdir.plot, plot.names, sep='/')
  tagged.text.words<-lapply(tagged.texts, function(x) x[[2]])
  tagged.text.plots<-lapply(tagged.texts, function(x) x[[1]])
  if(plot.type=="line"){
    total.suspense<-lapply(tagged.texts, function(x) x[[3]])
    total.suspense<-unlist(total.suspense)
    all.dates<-lapply(file.list, function(x) extractDate(x))
    all.dates<-unlist(all.dates)
    total.suspense.table<-data.frame(unlist(raw.text.names), all.dates, total.suspense, stringsAsFactors=F)
    write.csv(total.suspense.table, file=paste(outdir.plot, "totalsuspense_Unsuspensecorpus.csv", sep="/"))
  }
  mapply(function(x,y) write(x,file=y), tagged.text.words, text.names)
  #print each plot to its own file
  mapply(function(x,y) plotPDF(x,y), plot.names, tagged.text.plots)
  side=sqrt(length(tagged.text.plots))
  hor=round(side, 0)
  vert=round(side, 0)
  vert=vert+1
  #pdf(paste(outdir.plot, "LinePlots_Unsuspense.pdf", sep='/'), height=vert*3, width=hor*8)
  require(gridExtra)
  all.plots<-marrangeGrob(tagged.text.plots, ncol=hor, nrow=vert)
  #print(all.plots)
  #ggsave(paste(outdir.plot, "allplots_test.pdf", sep='/'), all.plots)
  #lapply(tagged.text.plots, function(x) print(x))
  #dev.off()
  names(tagged.texts)<-text.names
  return(tagged.texts)
}

#function for just plotting tags as bands of colors. Plot type refers to type of output and can either be
#line or bar
plotTags<-function(tag.text,  
                   tags,
                   text.name='Text',
                   output='chart', 
                   plot.type = 'bar',
                   smooth.plot=F){
  library(ggplot2)
  bad.list<-NULL
  tag.table.list<-list()
  for (i in 1:nrow(tags)){
    curr.tags<-getTags(tags[i,1], tag.text)
    if (length(curr.tags)==0){
      bad.list<-c(bad.list, i)
    } else {
      tag.table.list<-c(tag.table.list, list(curr.tags))
    }
  }
  if (!is.null(bad.list)){
    tags<-tags[-bad.list,]
  }
  combined.tag.table<-NULL
  tag.names<-NULL
  real.names<-NULL
  if(length(tag.table.list)>0){
    for (i in 1:length(tag.table.list)){
      combined.tag.table<-rbind(combined.tag.table, tag.table.list[[i]])
      tag.names<-c(tag.names, rep(tags[i,2], nrow(tag.table.list[[i]])))
      real.names<-c(real.names, rep(tags[i,1], nrow(tag.table.list[[i]])))
    }
    combined.tag.df<-data.frame(combined.tag.table,tag.names, real.names)
    colnames(combined.tag.df)<-c('tag.start', 'tag.close', 'tag.name', 'real.name')
    
    if (plot.type=="line"){
      tag.starts<-as.numeric(combined.tag.df$tag.start)
      tag.ends<-as.numeric(combined.tag.df$tag.close)
      tag.centroids<-mapply(function(x,y) mean(c(x,y)), tag.starts, tag.ends)
      tag.levels<-as.character(combined.tag.df$tag.name)
      line.plot.table<-data.frame(tag.centroids, as.numeric(tag.levels), stringsAsFactors=F)
      colnames(line.plot.table)<-c("window.center", "Suspense_Level")
      #print(class(line.plot.table$tag.x))
      #print(class(line.plot.table$tag.y))
      line.plot.table<-line.plot.table[order(line.plot.table$window.center),]
      if(smooth.plot){
        smoothed.values<-NULL
        for(i in 2:(nrow(line.plot.table)-1)){
          raw.values<-line.plot.table$Suspense_Level[c((i-1),i,(i+1))]
          smoothed.value<-mean(raw.values)
          smoothed.values<-c(smoothed.values, smoothed.value)
        }
        
        line.plot.table<-line.plot.table[2:(nrow(line.plot.table)-1),]
        original.suspense.levels<-line.plot.table$Suspense_Level
        line.plot.table$Suspense_Level<-smoothed.values
      }
      suspense.total<-sum(line.plot.table$Suspense_Level)
    }
    
    names(tag.table.list)<-tags[,1]
  }
  
  plot.title<-paste("Plot of tagged sections of ", text.name, sep=' ')
  y.label<-"Percent of Words in Window"
  text.plot<-ggplot()
  if(length(tag.table.list)>0){
    if (plot.type=='bar'){
      tag.plot<-text.plot+geom_rect(data=combined.tag.df, aes(xmin=tag.start, xmax=tag.close, ymin=0, ymax=Inf), fill=combined.tag.df$tag.name)
    } else {
      tag.plot<-ggplot(line.plot.table, aes(x=window.center, y=Suspense_Level))+geom_point()+geom_line()+ylim(0,8)
    }
    
    
    #tag.plot<-line.plot+geom_rect(data=combined.tag.df, aes(xmin=tag.start, xmax=tag.start, ymin=0, ymax=Inf, fill=tag.name), alpha=0.1)
    tag.labels.vector<-NULL
    for (i in 1:nrow(tags)){
      curr.label<-paste('Field ', tags[i,1], '=', tags[i,2], sep=' ')
      tag.labels.vector<-c(tag.labels.vector, curr.label)
    }
    #changed when moved to new tagging system
    #final.labels<-paste(tag.labels.vector, collapse='\n')
    suspense.range<-paste("Tag Range: ", tags[1,1], "-", tags[nrow(tags),1], sep='')
    final.labels<-suspense.range
    require(grid)
    if (plot.type=="bar"){
      title.plot<-tag.plot+ggtitle(plot.title)+xlab(final.labels)+scale_y_continuous(limits=c(0,1))
    } else {
      title.plot<-tag.plot+ggtitle(plot.title)+xlab(final.labels)
    }
    if(plot.type=="bar"){
      return(title.plot)
    } else {
      final.output<-list(title.plot, suspense.total, original.suspense.levels)
      names(final.output)<-c("Plot", "TotalSuspense", "SuspenseValues" )
      return(final.output)
    }
  } else {
    print("No Tags")
  }
  detach(package:ggplot2, unload=T)
}

binQuantile<-function(differences){
  quantiles<-quantile(differences)
  diff.intensity<-rep(0,length(differences))
  for (i in 1:length(differences)){
    curr.diff<-differences[i]
    if(curr.diff>quantiles[4]) diff.intensity[i]<-4
    else if (curr.diff>quantiles[3]) diff.intensity[i]<-3
    else if (curr.diff>quantiles[2]) diff.intensity[i]<-2
    else if (curr.diff>=quantiles[1]) diff.intensity[i]<-1
  }
  return(diff.intensity)
}

netClassDiff<-function(net.train, new.var, var.names=NULL){
  require(neuralnet)
  full.results<-compute(net.train, covariate=new.var)
  result.class<-full.results$net.result
  result.vector<-apply(result.class, 1, which.max)
  if (!is.null(var.names)){
    for (i in 1:length(var.names)){
      result.vector[which(result.vector==i)]<-var.names[i]
    }
  }
  difference<-abs(result.class[,1]-result.class[,2])
  diff.quant<-binQuantile(difference)
  final.tags<-paste(result.vector, diff.quant, sep='')
  return(final.tags)  
}

#using a metadata table and a directory of autotagged texts, function returns sum of suspense values, line length
#and number of turns appended to metadata table, as well as separate table of individual suspense.scores
suspenseTagInfo<-function(meta.table, tag.text.dir, idCol){
  file.names<-list.files(tag.text.dir)
  file.names<-unlist(strsplit(file.names, "_autotagged.txt"))
  table.add<-NULL
  suspense.tag.values<-NULL
  color.tags<-c("highSuspense1", "highSuspense2", "highSuspense3", "highSuspense4", "lowSuspense1", "lowSuspense2",  "lowSuspense3", "lowSuspense4")
  number.sequence<-c(5,6,7,8,1,2,3,4)
  color.table<-cbind(color.tags, number.sequence)
  for(i in 1:nrow(meta.table)){
    curr.id<-meta.table[i,idCol]
    curr.id<-unlist(strsplit(curr.id, '.txt'))
    if(curr.id %in% file.names){
      to.open<-paste(curr.id, "_autotagged.txt", sep='')
      curr.file<-scan(paste(tag.text.dir, to.open, sep='/'), what='character', encoding='UTF-8', sep='\n')
      curr.file<-paste(curr.file, collapse=' ')
      curr.file<-unlist(strsplit(curr.file, ' '))
      text.length<-length(curr.file)
      text.info<-plotTags(curr.file, color.table, plot.type = 'line')
      curr.totalsuspense<-text.info$TotalSuspense
      curr.line.length<-dist.cal(1,text.info$SuspenseValues)
      curr.changes<-changes.cal(text.info$SuspenseValues)
      all.info<-c(curr.totalsuspense, curr.line.length, curr.changes, text.length)
      table.add<-rbind(table.add, all.info)
      suspense.tag.values.line<-rep(NA, 102)
      suspense.tag.values.line[1:length(text.info$SuspenseValues)]<-c(meta.table$Title[i], text.info$SuspenseValues)
      suspense.tag.values<-rbind(suspense.tag.values, suspense.tag.values.line)
    } else {
      table.add<-rbind(table.add, rep(NA, 3))

    }

  }
  table.add<-as.data.frame(table.add, stringsAsFactors=F)
  colnames(table.add)<-c("Total_Suspense", "Suspense_Line_Length", "Number_of_Line_Changes", "Text_Length")
  new.meta.table<-cbind(meta.table, table.add)
  return.list<-list(new.meta.table, suspense.tag.values)
  names(return.list)<-c("FullTable", "SuspenseValues")
  return(return.list)
}

#creates a feature table suitable for training a Neural Net. Identifies field percentages and tags as a 
#primary function but, if add.metrics is set to T, includes narrative percentage and POS percentages as 
#additional features. 

neuralNetTable<-function(source.text.dir, 
                            tag.text.dir,
                            convert.source.dir,
                            convert.tag.dir,
                            output.dir,
                            fields, 
                            tags,
                            window.type='percentage',
                            window.advance=1,
                            window=2,
                            type='text',
                            add.metrics=T, 
                            output.file="NeuralNet/New Revised Model/NewMasterFeatureTable.csv"){
  complete.feature.table<-data.frame()
  curr.output<-multi.field.plot(convert.source.dir, 
                                convert.tag.dir, 
                                output.dir,
                                fields, 
                                tags,
                                window.type=window.type,
                                window.advance=window.advance,
                                window=window,
                                type=type, 
                                poly.degree='line',
                                convert.tags=T,
                                add.metrics=add.metrics,
                                output='table')
  complete.feature.table<-rbind(complete.feature.table, curr.output)
  curr.output<-multi.field.plot(source.text.dir, 
                                tag.text.dir, 
                                output.dir,
                                fields, 
                                tags,
                                window.type=window.type,
                                window.advance=window.advance,
                                window=window,
                                type=type, 
                                poly.degree='line',
                                convert.tags=F,
                                add.metrics=add.metrics,
                                output='table')
  complete.feature.table<-rbind(complete.feature.table, curr.output)
  write.csv(complete.feature.table, file=output.file)
  return(complete.feature.table)
}

#calculate the percent narrative completion per slice
calNarrativePerc<-function(centroid.vec, text.length){
  text.percents<-(centroid.vec/text.length)*100
  text.percents[which(text.percents<26)]<-1
  text.percents[which(text.percents>74)]<-3
  text.percents[which(!text.percents %in% c(1,3))]<-2
  return(text.percents)
}


#find a set of pos scores in a given slice
posCal<-function(pos.slice){
  slice.length<-length(pos.slice)
  slice.cut<-unlist(strsplit(pos.slice, "_"))
  #raw_verbs<-which(pos.slice %in% (c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")))
  #raw_verbs<-length(raw_verbs)/slice.length
  #raw_nouns<-which(pos.slice %in% c("NN", "NNS", "NNP", "NNPS"))
  #raw_nouns<-length(raw_nouns)/slice.length
  #raw_adj<-which(pos.slice %in% c("JJ", "JJR", "JJS"))
  #raw_adj<-length(raw_adj)/slice.length
  #raw_adv<-which(pos.slice %in% c("RB", "RBR", "RBS"))
  #raw_adv<-length(raw_adv)/slice.length
  #slice.pos<-c(raw_verbs, raw_nouns, raw_adj, raw_adv)
  pos.ids<-c("CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "MD", "NN", "NNS", "NNP", "NNPS", "PDT", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB", ".", ",", ":")
  pos.freq<-lapply(pos.ids, function(x) (length(which(slice.cut==x))/length(pos.slice)))
  pos.freq<-unlist(pos.freq)
  return(pos.freq)
}

suspenseTagResolve<-function(feature.table, tag.col, suspense.tags=c("suspense", "suspense8", "suspense9", "suspense10"), unsuspense.tags=c("unsuspense", "nonsuspense", "suspense1", "suspense2", "suspense3")){
  feature.table[which(feature.table[,tag.col] %in% suspense.tags),tag.col]<-"suspense"
  feature.table[which(feature.table[,tag.col] %in% unsuspense.tags),tag.col]<-"unsuspense"
  table.class<-feature.table[which(feature.table[,tag.col] %in% c("suspense", "unsuspense")),]
  return(table.class)
}

testWindows<-function(text.dir, window.size, window.advance, type="percentage"){
  test.files<-list.files(text.dir, pattern=".txt")
  info.table<-list()
  for(i in 1:length(test.files)){
    infile<-paste(text.dir, test.files[i], sep='/')
    curr.text<-scan(infile, what='character', sep="\n")
    curr.text<-paste(curr.text, collapse=" ")
    curr.text<-unlist(strsplit(curr.text, " "))
    #window.s<-round((length(curr.text) * (window.size/100)),0)
    #window.a<-round((length(curr.text) * (window.advance/100)), 0)
    curr.split.table<-createWindows(curr.text, window.size, window.advance)
    return.list<-c(length(curr.text), window.s, window.a, nrow(curr.split.table), curr.split.table[nrow(curr.split.table),2], curr.split.table[nrow(curr.split.table),3], (curr.split.table[nrow(curr.split.table),3]-curr.split.table[nrow(curr.split.table),1]))
    info.table<-c(info.table, list(return.list))
  }
  info.matrix<-matrix(unlist(info.table), ncol=7, byrow=T)
  info.matrix<-as.data.frame(info.matrix, stringsAsFactors=F)
  colnames(info.matrix)<-c("TextLength", "WindowSize", "WindowAdvance", "NumDivisions", "FinalCentroid", "FinalSliceEnd", "FinalSliceSize")
  info.matrix$TextNames<-test.files
  return(info.matrix)
}


#function takes a directory of texts (either pos or not) and uses a model to tag all texts and output them to a new directory
BrickTag<-function(class.model, 
                   class.fields,
                   plot.type="line",
                   plot.grid=T,
                   indir="OriginalTexts", 
                   outdir.plot="Plots", 
                   outdir.text="TaggedTexts", 
                   div.size=2, 
                   div.advance=1, 
                   div.type='percentage', 
                   aoa=F, 
                   pos=F, 
                   add.metrics=T,
                   output.stats=T,
                   smooth.plot=F){
  file.list<-list.files(indir, pattern='.txt')
  print(file.list)
  
  #sorts files into dates a-ssuming date is the last four characters
  file.list<-sortFiles(file.list)
  
  file.list<-paste(indir, file.list, sep='/')
  text.list<-lapply(file.list, function(x) scan(x, what='character', sep='\n', encoding='UTF-8'))
  if (pos){
    text.list<-lapply(text.list, function(x) paste(x, collapse=' '))
    text.list<-lapply(text.list, function(x) unlist(strsplit(x, ' ')))
    text.list<-lapply(text.list, function(x) hardPOSClean(x))
  } else {
    text.list<-lapply(text.list, function(x) paste(x, collapse=' '))
  }
  print("Calculating POS values")
  source(paste(dropbox.path, "POS.R", sep='/'))
  pos.text.list<-lapply(text.list, function(x) pos_tag_file(x))
  pos.text.list<-lapply(pos.text.list, function(x) unlist(strsplit(x, ' ')))
  remove(text.list)
  tagged.texts<-lapply(pos.text.list, function(x) autoTag(x, div.size=div.size, div.advance=div.advance, div.type=div.type, class.model=class.model, class.fields=class.fields, aoa=aoa, pos=pos, plot.type=plot.type, add.metrics=add.metrics, smooth.plot=smooth.plot))
  file.list<-as.list(file.list)
  raw.text.names<-lapply(file.list, function(x) unlist(strsplit(x, '.txt')))
  raw.text.names<-lapply(raw.text.names, function(x) unlist(strsplit(x, '/')))
  #print(raw.text.names)
  #print(length(raw.text.names))
  raw.text.names<-lapply(raw.text.names, function(x) x[length(raw.text.names[[1]])])
  text.names<-paste(raw.text.names, '_autotagged.txt', sep='')
  plot.names<-paste(raw.text.names, '_autotagged_plots.pdf', sep='')
  text.names<-paste(outdir.text, text.names, sep='/')
  plot.names<-paste(outdir.plot, plot.names, sep='/')
  tagged.text.words<-lapply(tagged.texts, function(x) x[[2]])
  tagged.text.plots<-lapply(tagged.texts, function(x) x[[1]])
  if(output.stats){
    if(plot.type=="line"){
      tagged.text.stats<-lapply(tagged.texts, function(x) x[[5]])
      suspense.tags<-lapply(tagged.texts, function(x) x[[4]][,1])
      suspense.tags<-unlist(suspense.tags)
      full.matrix<-do.call(rbind, tagged.text.stats)
      full.matrix<-cbind(full.matrix, suspense.tags)
      suspense.slice.scores<-lapply(tagged.texts, function(x) x[[6]])
      suspense.slice.scores<-do.call(rbind, suspense.slice.scores)
      rownames(suspense.slice.scores)<-unlist(raw.text.names)
      write.csv(full.matrix, file=paste(outdir.plot, "AllSliceStats.csv", sep="/"))
      write.csv(suspense.slice.scores, file=paste(outdir.plot, "SuspenseSliceScores.csv", sep="/"))
    } else {
      print("Stats Unavailable in Bar Graphs")
    }
  }
  if(plot.type=="line"){
    total.suspense<-lapply(tagged.texts, function(x) x[[3]])
    total.suspense<-unlist(total.suspense)
    all.dates<-lapply(file.list, function(x) extractDate(x))
    all.dates<-unlist(all.dates)
    total.suspense.table<-data.frame(unlist(raw.text.names), all.dates, total.suspense, stringsAsFactors=F)
    write.csv(total.suspense.table, file=paste(outdir.plot, "totalsuspense_Unsuspensecorpus.csv", sep="/"))
  }
  mapply(function(x,y) write(x,file=y), tagged.text.words, text.names)
  #print each plot to its own file
  mapply(function(x,y) plotPDF(x,y), plot.names, tagged.text.plots)
  if(plot.grid){
    side=sqrt(length(tagged.text.plots))
    hor=round(side, 0)
    vert=round(side, 0)
    vert=vert+1
    pdf(paste(outdir.plot, "AllPlots.pdf", sep='/'), height=vert*3, width=hor*8)
    library(gridExtra)
    all.plots<-marrangeGrob(tagged.text.plots, ncol=hor, nrow=vert)
    print(all.plots)
    #ggsave(paste(outdir.plot, "allplots_test.pdf", sep='/'), all.plots)
    #lapply(tagged.text.plots, function(x) print(x))
    dev.off()
  }
  names(tagged.texts)<-text.names
  #return(tagged.texts)
  detach(package:ggplot2, unload=T)
}

#function takes a directory of texts (either pos or not) and uses a model to tag all texts and output them to a new directory
BrickTagBigCorpus<-function(class.model, 
                   class.fields,
                   plot.type="line",
                   indir="OriginalTexts", 
                   outdir.plot="Plots", 
                   outdir.text="TaggedTexts", 
                   div.size=2, 
                   div.advance=1, 
                   div.type='percentage', 
                   aoa=F, 
                   pos=F, 
                   add.metrics=T,
                   output.stats=T,
                   smooth.plot=T){

  # output - POS-tagged texts into outdir.text, PDF plots in Plots, segment score CSV in Plots.

  # MPI input sources.
  all.file.list<-list.files(indir, pattern='.txt')

  #print(all.file.list)

  #sorts files into dates assuming date is the last four characters
  #all.file.list<-sortFiles(file.list)

  all.file.list.dir<-paste(indir, all.file.list, sep='/')

  # MPI scatter

  all.stats<-NULL
  for(i in 1:length(all.file.list.dir)){
    file.list<-all.file.list.dir[i]
    filename<-all.file.list[i]
    print(file.list)
    text.list<-lapply(file.list, function(x) scan(x, what='character', sep='\n', encoding='UTF-8'))
    if (pos){
      text.list<-lapply(text.list, function(x) paste(x, collapse=' '))
      text.list<-lapply(text.list, function(x) unlist(strsplit(x, ' ')))
      text.list<-lapply(text.list, function(x) hardPOSClean(x))
    } else {
      text.list<-lapply(text.list, function(x) paste(x, collapse=' '))
    }
    print("Calculating POS values")
    source(paste(dropbox.path, "POS.R", sep='/'))
    pos.text.list<-lapply(text.list, function(x) pos_tag_file(x))
    pos.text.list<-lapply(pos.text.list, function(x) unlist(strsplit(x, ' ')))
    remove(text.list)
    tagged.texts<-lapply(pos.text.list, function(x) autoTag(x, div.size=div.size, div.advance=div.advance, div.type=div.type, class.model=class.model, class.fields=class.fields, aoa=aoa, pos=pos, plot.type=plot.type, add.metrics=add.metrics, smooth.plot=smooth.plot))
    file.list<-as.list(file.list)
    raw.text.names<-lapply(file.list, function(x) unlist(strsplit(x, '.txt')))
    raw.text.names<-lapply(raw.text.names, function(x) unlist(strsplit(x, '/')))
    #print(raw.text.names)
    #print(length(raw.text.names))
    raw.text.names<-lapply(raw.text.names, function(x) x[length(raw.text.names[[1]])])
    text.names<-paste(raw.text.names, '_autotagged.txt', sep='')
    plot.names<-paste(raw.text.names, '_autotagged_plots.pdf', sep='')
    text.names<-paste(outdir.text, text.names, sep='/')
    plot.names<-paste(outdir.plot, plot.names, sep='/')
    tagged.text.words<-lapply(tagged.texts, function(x) x[[2]])
    tagged.text.plots<-lapply(tagged.texts, function(x) x[[1]])
    if(output.stats){
      if(plot.type=="line"){
        suspense.tags<-lapply(tagged.texts, function(x) x[[4]][,1])
        suspense.tags<-as.numeric(unlist(suspense.tags))
        if(length(suspense.tags)<105){
          to.fill<-105-length(suspense.tags)
          suspense.tags<-c(suspense.tags, rep(NA,to.fill))
        }
        all.stats<-rbind(all.stats, c(filename, suspense.tags))
      } else {
        #print("Stats Unavailable in Bar Graphs")
      }
    }
    if(plot.type=="line"){
      #total.suspense<-lapply(tagged.texts, function(x) x[[3]])
      #total.suspense<-unlist(total.suspense)
      #all.dates<-lapply(file.list, function(x) extractDate(x))
      #all.dates<-unlist(all.dates)
      #total.suspense.table<-data.frame(unlist(raw.text.names), all.dates, total.suspense, stringsAsFactors=F)
      #write.csv(total.suspense.table, file=paste(outdir.plot, "totalsuspense_Unsuspensecorpus.csv", sep="/"))
    }
    mapply(function(x,y) write(x,file=y), tagged.text.words, text.names)
    #print each plot to its own file
    mapply(function(x,y) plotPDF(x,y), plot.names, tagged.text.plots)
    names(tagged.texts)<-text.names
    #return(tagged.texts)
    detach(package:ggplot2, unload=T)
    write.csv(all.stats, file=paste(outdir.plot, "AllStats.csv", sep="/"), row.names=F)
  }
}


BrickTagBigCorpusParallel<-function(class.model, 
                   class.fields,
                   plot.type="line",
                   indir="OriginalTexts", 
                   outdir.plot="Plots", 
                   outdir.text="TaggedTexts", 
                   div.size=2, 
                   div.advance=1, 
                   div.type='percentage', 
                   aoa=F, 
                   pos=F, 
                   add.metrics=T,
                   output.stats=T,
                   smooth.plot=T){

  # List input paths.
  paths<-list.files(indir, pattern='.txt', full.names=T)

  # MPI scatter

  all.stats<-NULL
  for (path in paths) {

    # File basename.
    filename<-basename(path)

    # Read the text file as a list of lines.
    text.list<-scan(path, what='character', sep='\n', encoding='UTF-8')

    # Merge lines into a single string.
    text.list<-paste(text.list, collapse=' ')

    # Get list of POS-tagged tokens.
    source(paste(dropbox.path, "POS.R", sep='/'))
    pos.text.list<-lapply(text.list, function(x) pos_tag_file(x))
    pos.text.list<-lapply(pos.text.list, function(x) unlist(strsplit(x, ' ')))

    remove(text.list)

    # Apply classifier.
    tagged.texts<-lapply(pos.text.list, function(x) autoTag(
      x,
      div.size=div.size,
      div.advance=div.advance,
      div.type=div.type,
      class.model=class.model,
      class.fields=class.fields,
      aoa=aoa,
      pos=pos,
      plot.type=plot.type,
      add.metrics=add.metrics,
      smooth.plot=smooth.plot
    ))

    # Build a slug from the file name.
    raw.text.names<-unlist(strsplit(filename, '.txt'))

    # Build paths for tagged text and plots.
    text.names<-paste(raw.text.names, '_autotagged.txt', sep='')
    plot.names<-paste(raw.text.names, '_autotagged_plots.pdf', sep='')
    text.names<-paste(outdir.text, text.names, sep='/')
    plot.names<-paste(outdir.plot, plot.names, sep='/')

    # Destructure the classifier results.
    tagged.text.words<-lapply(tagged.texts, function(x) x[[2]])
    tagged.text.plots<-lapply(tagged.texts, function(x) x[[1]])

    if (output.stats & plot.type == 'line') {

      # Pull out the tags.
      suspense.tags<-lapply(tagged.texts, function(x) x[[4]][,1])
      suspense.tags<-as.numeric(unlist(suspense.tags))

      # Correct bin offset error.
      if (length(suspense.tags) < 105) {
        to.fill<-105-length(suspense.tags)
        suspense.tags<-c(suspense.tags, rep(NA,to.fill))
      }

      # TODO: Return from map.
      all.stats<-rbind(all.stats, c(filename, suspense.tags))

    }

    # Dump tagged words to disk.
    mapply(function(x,y) write(x,file=y), tagged.text.words, text.names)

    # Dump plots to separate files.
    mapply(function(x,y) plotPDF(x,y), plot.names, tagged.text.plots)
    names(tagged.texts)<-text.names

    detach(package:ggplot2, unload=T)

    # Dump stats to CSV.
    # TODO: Do once, outside the map function.
    write.csv(all.stats, file=paste(outdir.plot, "AllStats.csv", sep="/"), row.names=F)

    print(filename)

  }

}
