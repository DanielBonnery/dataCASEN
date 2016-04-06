\name{casen2013}
\alias{casen2013}
\docType{data}
\title{casen2013}
\description{}
\usage{data(casen2013)}
\format{
 This table is a selection of variable from Casen 2013 data (see Example below for complete description of the creation of the table)
  \describe{
    \name{casen2013}
    \item{REGION}{Region}
    \item{ZONA}{Zone  (Urban:1, Rural:2)}
\item{COMUNA}{Comune}
\item{EXPC}{Weight for comuna}
\item{EXPR}{Weight for Region}
\item{SEGMENTO}{Segment}
\item{ESTRATO}{Stratum }
\item{IDVIV}{Housing unit ID}
\item{FOLIO}{Household ID}
\item{EDAD}{Age}
\item{SEXO}{Gender}
\item{CORTE}{Povery indicator ("Indigente": very poor,
"Pobre no Indigente": poor "No pobre": not poor)}
  }
}
\source{
\url{http://observatorio.ministeriodesarrollosocial.gob.cl/layout/doc/bases/Casen2013spss.rar}

}
\examples{
#Program used to create the file
if(FALSE){
file.remove(list.files(tmpfolder))
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/documentos/CASEN_2013_MN_B_Principal_spss.rar",
              destfile=file.path(tmpfolder,"CASEN_2013_MN_B_Principal_spss.rar"))
system(
  if(.Platform$OS.type == "unix") {
    'unrar e "CASEN_2013_MN_B_Principal_spss.rar"'
  } else {
    '"C:\\Program Files\\7-Zip\\7z.exe" x "CASEN_2013_MN_B_Principal_spss.rar" -y'  
  }
  )
casen2013=foreign::read.spss(file.path(tmpfolder,"CASEN_2013_MN_B_Principal.sav"),to.data.frame=TRUE)
file.remove(list.files(tmpfolder))
casen2013<-casen2013[is.element(tolower(names(casen2013)),
                                       tolower(c("REGION","COMUNA","EXPC","EXPR","SEGMENTO","ESTRATO","IDVIV2","FOLIO","EDAD","SEXO","CORTE")))]
}}
\keyword{datasets}
  