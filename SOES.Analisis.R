# #########################################################################################################
# Procesar Posts.xml
# #########################################################################################################
require(xml2)

rm(list=ls())
posts.xml = "C:/Tmp/soes.data/Posts.xml"
doc <- read_xml(posts.xml)
rows <- xml_find_all(doc, "row")

timfmt <- "%Y-%m-%dT%H:%M:%OS"
posts <- data.frame(
    PostTypeId = as.integer(xml_attr(rows,"PostTypeId")),
    Id = as.integer(xml_attr(rows,"Id")),
    Title = xml_attr(rows,"Title"),
    # Body = xml_attr(rows,"Body"),
    AcceptedAnswerId = as.integer(xml_attr(rows,"AcceptedAnswerId")),
    CreationDate = as.POSIXct(xml_attr(rows,"CreationDate"),format=timfmt),
    Score = as.integer(xml_attr(rows,"Score")),
    ViewCount = as.integer(xml_attr(rows,"ViewCount")),
    LastEditDate = as.POSIXct(xml_attr(rows,"LastEditDate"),format=timfmt),
    LastActivityDate = as.POSIXct(xml_attr(rows,"LastActivityDate"),format=timfmt),
    Tags = xml_attr(rows,"Tags"),
    AnswerCount = as.integer(xml_attr(rows,"AnswerCount")),
    CommentCount = as.integer(xml_attr(rows,"CommentCount"))
)

posts$CreationMonth <- as.Date(cut(posts$CreationDate,breaks = "month"))
posts$Respondidas <- as.factor(ifelse(posts$PostTypeId == 1, ifelse(posts$AnswerCount > 0, "Si", "No"), NA))
posts$Aceptadas <- as.factor(ifelse(posts$PostTypeId == 1, ifelse(is.na(posts$AcceptedAnswerId), "No", "Si"), NA))

save(posts,file = "Posts.Rda")

rm(doc)
rm(rows)
rm(posts.xml)



require(ggplot2)
# #########################################################################################################
# Grafica: Preguntas respondidas x mes
# #########################################################################################################
ggplot(data = posts[posts$PostTypeId == 1,], aes(x = CreationMonth)) + 
    geom_bar(aes(fill = Respondidas), width =25) + 
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%Y-%m")  +
    theme(axis.text.x = element_text(angle = 75, vjust = .5)) +
    scale_fill_discrete("") +
    labs(x = "Meses", y = "Preguntas", title = "Preguntas respondidas por mes", caption = "(Según datos de SOes)")

# #########################################################################################################
# Grafica: Preguntas aceptadas x mes
# #########################################################################################################
ggplot(data = posts[posts$PostTypeId == 1,], aes(x = CreationMonth)) + 
    geom_bar(aes(fill = Aceptadas), width =25) + 
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%Y-%m")  +
    theme(axis.text.x = element_text(angle = 75, vjust = .5)) +
    scale_fill_discrete("") +
    labs(x = "Meses", y = "Preguntas", title = "Preguntas aceptadas por mes", caption = "(Según datos de SOes)")
