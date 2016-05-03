### function to clean up sitenames
### years reduced to two digits, e.g. 2005 -> 05
### seasons in lower case letters; site names in capitals
### so seasons can be extracted using gr

clean_snames <- function(x) { x # x is a character vector of site names
  # shorten years and seasons
  x <- gsub('-d20', '.', x) 
  x <- gsub('ver', 'v', x)
  x <- gsub('ve2', 'v', x)
  x <- gsub('inv', 'i', x)
  
  # standardise site names
  x <- gsub('Lagoa.do.Papagaio', 'PAPAG', x)
  x <- gsub('Granja.do.Pântano', 'PANTA', x)
  x <- gsub('Caieira', 'CAIEA', x)
  x <- gsub('Capão.do.Fundo', 'FUNDO', x)
  x <- gsub('Lagoa.do.Rincão', 'RINCO', x)
  x <- gsub('Banhado.da.Alemoa', 'ALEMO', x)
  x <- gsub('Lagoa.do.Sangradouro', 'DOURO', x)
  x <- gsub('Paulo.Santana', 'PAULO', x)
  x <- gsub('Canal.da.lagoa', 'CANAL', x, ignore.case = T)
  x <- gsub('Lagoa.João.Dias', 'JDIAS', x)
  x <- gsub('Banhado.da.Ronda', 'RONDA', x)
  x <- gsub('Lagoa.do.Bonito', 'BNITO', x)
  x <- gsub('Lagoa.São.Simão', 'SIMAO', x)
  x <- gsub('Lagoa.da.Figueira', 'FIGUA', x)
  x <- gsub('PNLP...Banhado.do.balneário', 'BABAL', x)
  x <- gsub('PNLP...Lagoa.do.meio', 'LMEIO', x)
  x <- gsub('PNLP...Trilha.do.Talhamar', 'TALHA', x)
  x <- gsub('PNLP...Barra.da.Lagoa', 'BARRA', x)
  x <- gsub('PNLP...praia', 'PPRAI', x)
  x <- gsub('Parna...Lagoa.da.Veiana', 'VEANA', x)
  x <- gsub('PNLP...Figueiras', 'FIGUA', x)
  x <- gsub('Parna...Lagoa.Pai.João', 'PJOAO', x)
  x <- gsub('Lagoa.do.Meio', 'LMEIO', x, ignore.case = T)
  x <- gsub('Rincão', 'RINCO', x)
  x <- gsub('Trilha.da.Figueira..PNLP.', 'FIGUA', x)
  x <- gsub('Praia..PNLP.', 'PPRAI', x)
  x <- gsub('Trilha.da.Figueira..PNLP.', 'FIGUA', x)
  x <- gsub('Trilha.das.Dunas..PNLP.', 'DUNAS', x)
  x <- gsub('PARNA.Praia', 'PPRAI', x)
  x <- gsub('PARNA.Barra.da.Lagoa', 'BARRA', x)
  x <- gsub('Saída.da.CAIEA', 'SAIDA', x)
  x <- gsub('Praia.do.balneário', 'PRABA', x)
  x <- gsub('Praia.Parna..PNLP', 'PPRAI', x)
  x <- gsub('Barra.da.Lagoa', 'BARRA', x)
  x <- gsub('PNLP..Banhado.do.Balneário', 'BABAL', x)
  x <- gsub('Banhado.do.Balneário', 'BABAL', x)
  x <- gsub('..Barra.Falsa.', '', x)
  x <- gsub('Trilha.Talhamar', 'TALHA', x)
  x <- gsub('Trilha.das.Figueiras', 'FIGUA', x)
  x <- gsub('PNLP.Praia', 'PPRAI', x)
  x <- gsub('Trilha.da.Figueira', 'FIGUA', x)
  x <- gsub('PLNP...Barra', 'BARRA', x)
  x <- gsub('PNLP..BARRA.', 'BARRA', x)
  x <- gsub('PNLP...', '', x)
  x <- gsub('Ponto', 'Pont', x)
  x <- gsub('A14', 'A.14', x)
  x <- gsub('Lagoa.do.paurá', 'PAURA', x)
  
  # return object
  return(x)
}

# clean_snames(rownames(dat.spsite)) # check out the new names

# length(unique(clean_snames(rownames(dat.spsite)))) / 
#   length(rownames(dat.spsite)) # all unique!
