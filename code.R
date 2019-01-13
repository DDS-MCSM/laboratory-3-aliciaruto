#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                     Your Name - Data Driven Securty                          #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping


### 1.1 Obtención de la página web

library("httr")
my_url <- "https://www.mediawiki.org/wiki/MediaWiki"
webpage <- read_html(my_url)

### 1.2 Analisis de el contenido de la web

webpage %>% html_nodes("title")
webpage %>% html_nodes("head")

### 1.3.	Extracción de enlaces

urls <- webpage %>% html_nodes("a") %>% html_attr("href")

### 1.4 Exploración de enlaces

url_mediawiki <- "https://www.mediawiki.org/"

status <- c()
webs_con <- c()
webs_sin <- c()
webs_con_solo <-c()
webs_sin_solo <-c()

for (i in 1:length(urls)){
    if (is.na(urls[i])) {
    } else if (grepl("http", urls[i])) {
      webs_con <- c(webs_con, urls[i])
      webs_con_solo <- c(webs_con_solo, urls[i])
      webs_sin <- c(webs_sin, urls[i])
      status <- c(status, status_code(GET(urls[i])))
    } else if (!grepl("http", urls[i])) {
      webs_sin_solo <- c(webs_sin_solo, urls[i])
      if (grepl("//", urls[i])) {
        webs_con <- c(webs_con, paste("https:",urls[i], sep = ""))
        webs_sin <- c(webs_sin, urls[i])
        status <- c(status, status_code(GET(paste("https:",urls[i], sep = ""))))
      } else {
        webs_con <- c(webs_con, paste(url_mediawiki,urls[i], sep = ""))
        webs_sin <- c(webs_sin, urls[i])
        status <- c(status, status_code(GET(paste(url_mediawiki,urls[i], sep = ""))))
      }
    }  else {
    }
    Sys.sleep(3)
}

### Gráficos en R

### 2.1 Histograma

df_sin <- data.frame(webs_sin_solo)
new_df_sin<-aggregate(df_sin$webs_sin_solo, df_sin, length)

transformacion <-c()
for (i in 1:nrow(new_df_sin[2])){
  transformacion <- c(transformacion,as.numeric(new_df_sin[i,2]))
}

hist(transformacion,freq=FALSE,col="lightcyan", ylim=c(0,3),main="Histograma de webs relativas",xlab="Apariciones")

df_con <- data.frame(webs_con_solo)
new_df_con <- aggregate(df_con$webs_con_solo, df_con, length)

transformacion <- c()
for (i in 1:nrow(new_df_con[2])){
  transformacion <- c(transformacion,as.numeric(new_df_con[i,2]))
}

hist(transformacion,freq=FALSE,col="lightcyan", ylim=c(0,5), xlim=c(1,2), main="Histograma de webs absolutas",xlab="Apariciones")

### 2.2 Un gráfico de barras
dentro <- c()
for (i in 1:length(webs_sin)){
  if (grepl("http", webs_sin[i])) {
    if (grepl("https://www.mediawiki.org", webs_sin[i])) {
      dentro <- c(dentro, 1)
    }
    else {
      dentro <- c(dentro, 0)
    }
  } else if (!grepl("http", webs_sin[i])) {
    dentro <- c(dentro, 1)
  }
}

df_dentro <- data.frame(dentro)
new_df_dentro <- aggregate(df_dentro$dentro, df_dentro, length)

ggplot(data = new_df_dentro, aes(x = dentro, y = x)) + geom_bar(stat = "identity", width = 0.5) + ylab("Número de apariciones") + xlab("Tipo de dominio") + ggtitle("Gráfico según tipo de dominio") + theme_bw()

### 2.3 Pie Chart

df_codigo <- data.frame(status)
new_df_codigo <- aggregate(df_codigo$status, df_codigo, length)
new_df_codigo

pie(table(new_df_codigo$status), main = "Pastel según\n códigos de respuesta http", col = "lightcyan")
