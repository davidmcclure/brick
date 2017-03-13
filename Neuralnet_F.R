#use a trained neural network to classify a set of test data. If "test" is T, function tests 
#classifications against correct.class (which must be set to a set of output). If "test" is F,
#function only returns new classifications in list and summary form. Function also takes option
#var.names, for renaming categorical variable output from 'neural net'
#If test=T, function has values: full.class - a vector of the full classifications, class.table - a 
#confusion matrix with the test data measured against the actual data, error.rate - the percent correct,
#, confusion plot - a ggplot object with correct classifications
#If test=F, funciton has values: full.class

netClass<-function(net.train, new.var, correct.class=NULL, test=T, var.names=NULL){
  require(neuralnet)
  full.results<-compute(net.train, covariate=new.var)
  result.class<-full.results$net.result
  if (!is.null(dim(result.class))){
    result.vector<-apply(result.class, 1, which.max)
    if (!is.null(var.names)){
      for (i in 1:length(var.names)){
        result.vector[which(result.vector==i)]<-var.names[i]
      }
    }
  } else {
    result.vector<-result.class
  }
  full.class<-result.vector
  if (test){
    if(!is.null(dim(correct.class))){
      test.vector<-apply(correct.class, 1, which.max)
      if (!is.null(var.names)){
        for (i in 1:length(var.names)){
          test.vector[which(test.vector==i)]<-var.names[i]
        }
      }
    } else {
      test.vector<-correct.class
    }
    class.table<-table(result.vector, test.vector)
    error.rate<-sum(diag(prop.table(class.table)))
    per.table<-t(class.table)
    per.table<-per.table/rowSums(per.table)
    require(ggplot2)
    actual.groups<-rep(rownames(per.table), ncol(per.table))
    classified.groups<-sort(actual.groups)
    classified.values<-as.vector(per.table)
    plot.table<-data.frame(actual.groups, classified.values, classified.groups)
    colnames(plot.table)<-c("Groups", "Values", "Classifications")
    confusion.plot<-ggplot(plot.table, aes(Groups, Values, fill=Classifications))+geom_bar(stat='identity')+xlab(paste("Error rate: ", as.character(error.rate), sep=""))
    output.list<-list(full.class, class.table, error.rate,confusion.plot)
    names(output.list)<-c("full.class", "class.table", "error.rate", "confusion.plot")
  } else {
    output.list<-list(full.class)
    names(output.list)<-c("full.class")
  }
  return(output.list)
}


#creates a neural net model using package 'neuralnet' raw.data is a data set: if divide.set=F, then
#raw.data is the training data set and the test data set is passed through variable test.set. If 
#divide.set=T, all data is passed through raw.data is the complete data set and will be sampled
#and divided into the percentages (training and test), given in the sample.per vector
#formula for neuralnet will be computed based on variable.vec (the vector of column numbers containing)
#the variables for analysis) and output.var (the column(s) of output variables), if cat=T, then the
#output categorical variables will be converted to multiple columns of binary variables named for the
#categorical variable labels
#binary parameters for the neural net (as a vector) are passed through net.structure
#return value is the neural net ($nnet), as well as the output list from netClass. Error rates
#from both the neural network and the tet set are printed to the screen
trainNeuralnet<-function(raw.data, 
                         divide.set=F, 
                         test.set=NULL, 
                         sample.per=NULL, 
                         variable.vec, 
                         output.var, 
                         cat=F, 
                         net.structure){
  print(net.structure)
  require(neuralnet)
  table.names<-colnames(raw.data)
  raw.data[is.na(raw.data)]<-0
  net.vars<-raw.data[,variable.vec]
  var.names<-table.names[variable.vec]
  output<-raw.data[,output.var]
  if(cat){
    require(nnet)
    output.class<-class.ind(output)
    output.names<-colnames(output.class)
  } else {
    output.class<-output
    output.names<-table.names[output.var]
  }
  raw.complete<-cbind(net.vars, output.class)
  net.formula<-paste(paste(output.names, collapse='+'), paste(var.names, collapse='+'), sep='~')
  #print(net.formula)
  net.formula<-as.formula(net.formula)
  if (divide.set){
    #train.size<-round((nrow(raw.data)*sample.per), 0)
    #train.sample<-sample(nrow(raw.complete),train.size, replace=F)
    #train.data<-raw.complete[train.sample,]
    sample.index<-dataSample(sample.per, raw.data$tags)
    train.data<-raw.complete[sample.index[[1]],]
    print(dim(train.data))
    test.vars<-net.vars[sample.index[[2]],]
    print(dim(test.vars))
    test.class<-output.class[sample.index[[2]],]
  } else {
    train.data<-raw.complete
    test.set[is.na(test.set)]<-0
    test.vars<-test.set[,variable.vec]
    test.output<-test.set[,output.var]
    if(cat){
      test.class<-class.ind(test.output)
    } else {
      test.class<-test.output
    }
  }
  nn.model<-NULL
  nn.model<-try(neuralnet(formula=net.formula, data=train.data, hidden=net.structure), silent=T)
  converge<-length(nn.model)
  if (converge>8){
    print(nn.model)
    class.results<-netClass(nn.model, new.var=test.vars, correct.class=test.class, var.names=output.names)
    print(paste("Test Error Rate: ", as.character(class.results$error.rate), sep=''))
    print("Test Classification Table: ")
    print(class.results$class.table)
    all.output<-list(nn.model, class.results$full.class, class.results$class.table, class.results$error.rate, class.results$confusion.plot)
    names(all.output)<-c("net.model", names(class.results))
    return(all.output)
  } else {
    null.output<-list(NA, NA, NA, NA, NA)
    names(null.output)<-c("net.model", "full.class", "class.table", "error.rate", "confusion.plot")
    print("Model Failed to Converge")
    return(null.output)
  }
}



multi.net<-function(raw.data, 
                    divide.set=F, 
                    test.set=NULL, 
                    sample.per=NULL, 
                    variable.vec, 
                    output.var, 
                    cat=F, 
                    nodes,
                    levels){
  #build the list of hidden node combinations
  initial.sequence<-seq(nodes[1], nodes[2], by=1)
  struct.list<-list()
  struct.list<-c(struct.list, initial.sequence)
  curr.seq<-matrix(initial.sequence, ncol=1)
  for (i in 1:(levels[2]-1)){
    new.seq<-NULL
    for (j in 1:i){
      #print(curr.seq[,j])
      new.col<-rep(curr.seq[,j], length(initial.sequence))
      new.seq<-cbind(new.seq, new.col)
    }
    add.seq<-sort(new.seq[,1])
    new.seq<-cbind(add.seq, new.seq)
    for (j in 1:nrow(new.seq)){
      struct.list<-c(struct.list, list(new.seq[j,]))
    }
    curr.seq<-new.seq
  }
  remove.index<-NULL
  for(i in 1:length(struct.list)){
    if(length(struct.list[[i]])<levels[1]){
      remove.index<-c(remove.index, i)
    } else if(length(struct.list[[i]])>levels[2]){
      remove.index<-c(remove.index, i)
    } else {
      vector.compare<-sort(struct.list[[i]], decreasing=T)
      if (!identical(struct.list[[i]], vector.compare)){
        remove.index<-c(remove.index, i)
      }
    }
  }
  #struct.list<-struct.list[-remove.index]
  net.trials<-lapply(struct.list, function(x) trainNeuralnet(raw.data=raw.data, 
                                                             divide.set=divide.set,
                                                             test.set=test.set, 
                                                             sample.per=sample.per, 
                                                             variable.vec=variable.vec, 
                                                             output.var=output.var, 
                                                             cat=cat, net.structure=x))
  return(net.trials)
}

multi.net.rep<-function(raw.data, 
                        divide.set=F, 
                        test.set=NULL, 
                        sample.per=NULL, 
                        variable.vec, 
                        output.var, 
                        cat=F, 
                        nodes,
                        levels,
                        rep){
  master.list<-list()
  for (i in 1:rep){
    print(paste("Repetition: ", as.character(i), sep=''))
    curr.model.set<-multi.net(raw.data=raw.data, 
                              divide.set=divide.set, 
                              test.set=test.set,
                              sample.per=sample.per,
                              variable.vec=variable.vec,
                              output.var=output.var,
                              cat=cat,
                              nodes=nodes,
                              levels=levels)
    master.list<-c(master.list, list(curr.model.set))
  }
  #all.error.rates<-NULL
  #for(i in 1:rep){
  #  curr.e.rates<-apply(all.error.rates[[i]], function(x) x$error.rate)
  #  all.error.rates<-cbind(all.error.rates, curr.e.rates)
  #}
  #mean.error.rates<-rowMeans(all.error.rates)
  #master.list<-c(master.list, mean.error.rates)
  return(master.list)
}

extractError<-function(model.list){
  error.rate.table<-NULL
  for(i in 1:length(model.list)){
    curr.rep<-model.list[[i]]
    error.vector<-lapply(curr.rep, function(x) x$error.rate)
    error.vector<-unlist(error.vector)
    error.rate.table<-cbind(error.rate.table, error.vector)
  }
  return(error.rate.table)
}

#creates proportional data samples with equal porportions of each class in response.vector. Sample
#vector is to vector of length two describing the proportions of the training and test data set respectively
dataSample<-function(sample.vector, response.vector){
  training.sample<-NULL
  test.sample<-NULL
  unique.class<-unique(response.vector)
  orig.position<-seq(1, length(response.vector), by=1)
  for(i in 1:length(unique.class)){
    sample.pop<-which(response.vector==unique.class[i])
    #print(length(sample.pop))
    sample.pop<-orig.position[sample.pop]
    sample.size<-round((length(sample.pop) * sample.vector[1]),0)
    #print(sample.size)
    curr.train<-sample(sample.pop, sample.size, replace=F)
    training.sample<-c(training.sample, curr.train)
    curr.test<-sample.pop[which(!sample.pop %in% training.sample)]
    test.sample<-c(test.sample, curr.test)
  }
  final.samples<-list(training.sample, test.sample)
  return(final.samples)
}

#train iterative models of large neural nets based on a matrix of hidden vars
complexTrain<-function(raw.data, 
                       divide.set=T, 
                       test.set=NULL, 
                       sample.per=NULL, 
                       variable.vec, 
                       output.var, 
                       cat=F, 
                       structure.matrix,
                       rep){
  net.structures<-list()
  for(i in 1:nrow(structure.matrix)){
    curr.struct<-structure.matrix[i,]
    curr.struct<-curr.struct[!is.na(curr.struct)]
    net.structures<-c(net.structures, list(curr.struct))
  }
  #print(net.structures)
  master.list<-list()
  for(i in 1:rep){
    net.list<-lapply(net.structures, function(x) trainNeuralnet(raw.data, divide.set=divide.set, sample.per=sample.per, variable.vec=variable.vec, output.var=output.var, cat=T, net.structure=x))
    master.list<-c(master.list, net.list)
  }
  return(master.list)
}

#train iterative models of large neural nets based on a matrix of hidden vars
#rep is a three digit integer, the model will iterate over the complex networks
#until the overall success is greater than rep[1] and the second variable success rate is greater
#than rep[2]. If the model runs more than rep[3] times, the algorithm will stop and the highest for 
#both will be reported along with the models that gave rise to them.
#test.var passes which variable is to be examined for the success criteria
complexTrainSeek<-function(raw.data, 
                       divide.set=T, 
                       test.set=NULL, 
                       sample.per=NULL, 
                       variable.vec, 
                       output.var, 
                       cat=T, 
                       net.structure,
                       test.var,
                       rep){
  #print(net.structures)
  best.var.model<-NULL
  best.overall.model<-NULL
  best.var.error<-0
  best.overall.error<-0
  continue=T
  overall.errors<-NULL
  var.errors<-NULL
  final.model<-NULL
  structure.report<-list()
  n.rep<-1
  good.model<-F
  for(i in 1:length(net.structure)){
    curr.struct<-net.structure[[i]]
    curr.error.report<-NULL
    continue=T
    n.rep<-1
    while (continue){
      print(n.rep)
      curr.net<-trainNeuralnet(raw.data, divide.set=divide.set, sample.per=sample.per, variable.vec=variable.vec, output.var=output.var, cat=T, net.structure=curr.struct)
      if(!is.na(curr.net$error.rate)){
        curr.error.overall<-curr.net$error.rate
        overall.errors<-c(overall.errors, curr.error.overall)
        curr.table<-curr.net$class.table
        curr.var.error<-diag(curr.table)/colSums(curr.table)
        curr.var.error<-curr.var.error[test.var]
        var.errors<-c(var.errors, curr.var.error)
        if(curr.error.overall>rep[1]){
          if(curr.var.error>rep[2]){
            print("Model Success")
            continue=F
            good.model<-T
            final.model<-curr.net
          }
        }
        if(curr.error.overall>best.overall.error){
          best.overall.error<-curr.error.overall
          best.overall.model<-curr.net
        }
        if(curr.var.error>best.var.error){
          best.var.error<-curr.var.error
          best.var.model<-curr.net
        }
        if(n.rep==rep[3]){
          print("No Successful Model")
          continue=F
          if(!good.model){
            final.model<-list(best.overall.model, best.var.model)
          }
        }
      }
      n.rep<-n.rep+1
    }
    curr.error.report<-c(mean(overall.errors), mean(var.errors))
    #print(curr.error.report)
    structure.report<-c(structure.report, list(curr.error.report))
    #print(structure.report)
  }
  final.model<-c(final.model, structure.report)
  return(final.model)
}

#used for networks maked with complexTrain - extracts the total error rate as well as error rates for individual
#variables (suspense and unsuspense)
extractComplexErrors<-function(net.models){
  error.rates<-unlist(lapply(net.models, function(x) x$error.rate))
  error.tables<-lapply(net.models, function(x) x$class.table)
  var.errors<-lapply(error.tables, function(x) diag(x)/colSums(x))
  suspense.errors<-unlist(lapply(var.errors, function(x) x[1]))
  unsuspense.errors<-unlist(lapply(var.errors,function(x) x[2]))
  error.list<-list(error.rates, suspense.errors, unsuspense.errors)
  error.table<-matrix(unlist(error.list), ncol=3, byrow=F)
  colnames(error.table)<-c("Total_Success_Rate", "Suspense_Success", "Unsuspense_Success")
  return(error.table)
}
  