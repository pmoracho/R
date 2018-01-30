library(RODBC)
library(ggplot2)

start.time <- Sys.time()
cn<-odbcDriverConnect("DRIVER={SQL Server};SERVER=momdb2;Database=master;uid=plussistemas;pwd=plus")
df <- sqlQuery(cn, "
SELECT top 1000000
	     P.SYSTEM_ID,
       P1.USER_ID,
       P1.FULL_NAME,
       P2.USER_ID,
       P2.FULL_NAME,
       DT.DESCRIPTION,
       CREATION_DATE,
       LAST_EDIT_DATE,
       LAST_ACCESS_DATE,
       A.ASUNTO_ID,
       A.ASUNTO_DESC
       FROM LIB_ASUNTOS.DOCSADM.PROFILE P
       LEFT JOIN LIB_ASUNTOS.DOCSADM.PEOPLE P1
          ON P.TYPIST = P1.SYSTEM_ID
       LEFT JOIN LIB_ASUNTOS.DOCSADM.PEOPLE P2
          ON P.AUTHOR = P2.SYSTEM_ID
       LEFT JOIN LIB_ASUNTOS.DOCSADM.DOCUMENTTYPES DT
          ON DT.SYSTEM_ID = P.DOCUMENTTYPE
       LEFT JOIN LIB_ASUNTOS.DOCSADM.MOM_ASUNTOS A
          ON A.SYSTEM_ID = P.MOM_ASUNTOS")

close(cn)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

load("dms.RData")

summary(df)
df[df$SYSTEM_ID == min(df$SYSTEM_ID), ]

autores <- aggregate(df$FULL_NAME.1, by=list(df$FULL_NAME.1), length)
autores <- autores[with(autores, order(-x)), ]
colnames(autores) <- c("AUTHOR", "Cant")

top <- 10
autoresTop <- autores[1:top,]
autoresTop <- rbind(autoresTop, data.frame(AUTHOR="Resto", Cant=sum(autores[-c(1:top),2])))
autoresTop


median(autores$Cant)
mean(autores$Cant)
var(autores$Cant)
sd(autores$Cant)

ggplot(autoresTop, aes(x=AUTHOR, y=Cant)) +
  geom_bar(stat='identity', aes(fill=AUTHOR), width=.5) +
  geom_text(aes(label=Cant), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(name="Documentos", labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill="Autores",
     x=NULL,
     y=NULL,
     title="Autores DMS (TOP 10)",
     caption=paste0("fuente: LIB_ASUNTOS (",sum(autoresTop$Cant)," documentos) al 1/12"))

tiposdoc <- aggregate(df$DOCUMENTTYPE, by=list(df$DOCUMENTTYPE), length)
tiposdoc <- tiposdoc[with(tiposdoc, order(-x)), ]

# Clientes:
# Se migran los Clientes de Central (ClienteId y RazonSocial) que no estuvieran en el DMS
# Se actualiza el cambio de Razon social también
#
# Asuntos
# Se migran los asunto de legales.dbo.LegalesDbAsuntos
# Si el asunto es judical se usa la caratula judicial sino se usa la de factura
# Los clientes relacionados son los del asunto
# Se ve el Flag flagAsuntomigradodms


