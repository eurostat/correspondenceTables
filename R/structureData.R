library(httr)


structureData = function(prefix, conceptScheme, endpoint, language) {
  
  ### Define endpoint
  if (endpoint == "CELLAR") {
    source = "http://publications.europa.eu/webapi/rdf/sparql"
    url = "data.europa.eu/"
  }
  if (endpoint == "FAO") {
    source = "https://stats.fao.org/caliper/sparql/AllVocs"
    url = "unstats.un.org/"
  }
  
  ## Create Prefixes list 
  prefix_ls = prefixList(endpoint)
  prefix_ls = as.character(paste(prefix_ls, collapse = "\n"))
  ### Load prefixes from Excel file
  #prefix_file = read.csv(paste0("//lu-fsp01/Data_Lux/AgSTAT/Projects/CorrespondenceTables_Rpck/Task 3/prefix_", endpoint, ".csv"))
  #prefix = as.character(paste(prefix_file$List, collapse = "\n"))
  
  ### SPARQL query
  SPARQL.query = paste0(prefix_ls, "
  SELECT  DISTINCT ?Concept_Scheme ?Level ?Depth (COUNT (distinct ?s) AS ?Count) 

        WHERE {
          ?s skos:prefLabel ?Label ;
          #skos:inScheme ", prefix, ":", conceptScheme, " ;
          skos:inScheme ?Scheme ;
          ^skos:member ?Member ;
          skos:prefLabel ?Label ;
          skos:notation ?notation .
          
          ?Member a xkos:ClassificationLevel .
          OPTIONAL {?member xkos:levels ?levels_temp . }
          OPTIONAL {?member xkos:depth ?Depth . }
        
          FILTER (?Scheme = ", prefix, ":", conceptScheme, ")
          FILTER (lang(?Label) = '", language, "')
          #FILTER (datatype(?notation) = rdf:PlainLiteral) 
        
          BIND (STR(?s) AS ?URL)
          #BIND (STR(?notation) AS ?CODE ) 
        
          BIND (STRAFTER(STR(", prefix, ":), 'http') AS ?CLASS_URL)
          #BIND (STRAFTER((?CLASS_URL), '/') AS ?Class)
          BIND (STRAFTER(STR(?Scheme), STR(?CLASS_URL)) AS ?Concept_Scheme)
          BIND (STRAFTER(str(?Member), STR(?CLASS_URL)) As ?Level)
         
          FILTER (STRLEN(?Concept_Scheme) != 0)
          FILTER (STRLEN(?Level) != 0)
  		    #FILTER (?Concept_Scheme != STR('ag'))
        }
        
      GROUP BY ?Concept_Scheme ?Level  ?Depth 
      ORDER BY ?Concept_Scheme ?Level  ?Depth
  ")
          
  
  response = POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  table = read.csv(text=content(response, "text"), sep= ",")  
  table = table[order(table[,3],decreasing=FALSE),]

  return(table)
}