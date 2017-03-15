

load('Brick.RData')

source('Tagging_F.R')

BrickTagBigCorpus(Brick$net.model, suspense.fields)
