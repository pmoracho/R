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
load(file = "Posts.Rda")


rm(doc)
rm(rows)
rm(posts.xml)

# #########################################################################################################
# Generar post_tags
# #########################################################################################################
s <- regmatches(posts$Tags, gregexpr("(?<=\\<).*?(?=\\>)", posts$Tags, perl=T))
post_tags <- data.frame(Id = rep(posts$Id, sapply(s, length)), Tag = unlist(s))
post_tags <- post_tags[!is.na(posts$Tags),]
save(post_tags,file = "Posts_Tags.Rda")
load(file = "Posts_Tags.Rda")


# #########################################################################################################
# Etiquetas con más votos
# #########################################################################################################
df <- merge(post_tags, posts, by="Id")[, c("Tag", "Score", "Id")]
tag_score <- do.call(data.frame,aggregate(df$Score, list(Tag = df$Tag), FUN = function(x) c(Score = sum(x), Count = length(x), ScoreByCount=sum(x)/length(x))))
names(tag_score) <- c("Tag", "Score", "Count", "ScoreByCount")
tag_score <- tag_score[order(-tag_score[,4]),]
save(tag_score,file = "Tag_Score.Rda")
load(file = "Tag_Score.Rda")

ts <- tag_score[tag_score$Count>= 50,]
tag_score[tag_score$Tag %in% c("r", "python", "sql"),]

str(tag_score)
head(tag_score)
head(df)

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
