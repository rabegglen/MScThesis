### model validation

source("./scripts/plsPath4.R")


valMod = plspm(Data = modelDat4, path_matrix = techPath, blocks = modelBlocks, modes = modelModes, boot.val = TRUE, br = 500)

valMod$boot

