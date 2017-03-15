

load('Brick.RData')

source('Tagging_F.R')

BrickTagBigCorpusParallel(Brick$net.model, suspense.fields)
