validation = plsm(data = data, strucmod = strucmodel, measuremod = measuremodel)
validationResult = sempls(model = validation, data = data, wscheme="B")
factorScores = validationResult$factor_scores
weights = validationResult$outer_loadings
