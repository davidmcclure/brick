

load('Brick.RData')

library(Rmpi)
library(parallel)
library(plyr)


BrickTagGale<-function(class.model,
                   class.fields,
                   plot.type="line",
                   indir="gale",
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
  paths<-list.files(indir, pattern='.bz2', full.names=T, recursive=T)

  # Create MPI cluster.
  size <- mpi.universe.size()
  np <- if (size > 1) size - 1 else 1
  cluster <- makeCluster(np, type='MPI', outfile='')

  clusterExport(cluster, c('Brick', 'suspense.fields'))

  # Spread paths across MPI ranks.
  res <- clusterApply(cl=cluster, x=paths, fun=function(path) {

    load('Brick.RData')

    source('Tagging_F.R')
    library(rjson)

    tryCatch({

      # Read JSON.
      json = fromJSON(file=path)

      # Get plain text.
      text.list<-json$plain_text

      # Get list of POS-tagged tokens.
      source(paste(dropbox.path, "POS.R", sep='/'))
      pos.text.list<-lapply(text.list, function(x) pos_tag_file(x))
      pos.text.list<-lapply(pos.text.list, function(x) unlist(strsplit(x, ' ')))

      remove(text.list)

      # Apply classifier.
      tagged.texts<-lapply(pos.text.list, function(x) {

        autoTagMPI(
          source.text=x,
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
        )

      })

      # Build paths for tagged text and plots.
      text.names<-paste(json$identifier, '_autotagged.txt', sep='')
      plot.names<-paste(json$identifier, '_autotagged_plots.pdf', sep='')
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

      }

      # Dump tagged words to disk.
      mapply(function(x,y) write(x,file=y), tagged.text.words, text.names)

      # Dump plots to separate files.
      mapply(function(x,y) plotPDF(x,y), plot.names, tagged.text.plots)
      names(tagged.texts)<-text.names

      detach(package:ggplot2, unload=T)

      print(json$identifier)

      return(c(json$identifier, suspense.tags))

    }, error=function(e) {
      print(e)
      stop(e)
    })

  })

  # Write CSV.
  write.csv(ldply(res), file=paste(outdir.plot, "AllStats.csv", sep="/"), row.names=F)

  stopCluster(cluster)
  mpi.exit()

}


# Run job.
BrickTagGale(
  Brick$net.model,
  suspense.fields,
  indir='/scratch/PI/malgeehe/data/stacks/ext/gail-amfic'
)
