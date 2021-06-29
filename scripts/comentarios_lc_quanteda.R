require(tidyverse)
require(quanteda)
require(quanteda.textplots)
require(seededlda)
require(lubridate)
require(LSX)

comentarios_lc_2020_2021 <- readRDS('./data/notas_lc_con_comentarios_2020_2021.rds') %>% 
  mutate(id = row_number(), .before = fecha_nota)

corp_comentarios <- corpus(x = comentarios_lc_2020_2021$post,
                           docnames = comentarios_lc_2020_2021$id,
                           docvars = data.frame(id = comentarios_lc_2020_2021$id,
                                                fecha = comentarios_lc_2020_2021$fecha_nota))

corp_sent <- corpus_reshape(corp_comentarios, to =  "sentences")
toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(c(stopwords("es"), 'q', 'mar', 'plata', 'años', 'año', 'van', 'vas', 'va',
                  'ciudad','etc'))

dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5)

topfeatures(dfmat_sent, 40)

data_dicc_senti_positive <- read.csv2('./data/lexico_afinn_en_es_para_covid.csv', sep = ',') %>% 
  select(-3) %>% arrange(Puntuacion) %>% filter(Puntuacion > 0)

data_dicc_senti_negative <- read.csv2('./data/lexico_afinn_en_es_para_covid.csv', sep = ',') %>% 
  select(-3) %>% arrange(Puntuacion) %>% filter(Puntuacion < 0)

data_dicc_senti <- dictionary(list(positive = data_dicc_senti_positive$Palabra,
                negative = data_dicc_senti_negative$Palabra))

seed <- as.seedwords(data_dicc_senti)
print(seed)

covid <- char_context(toks_sent, pattern = c("sars*","covid*","coronav*","pandemi*"), p = 0.05)

tmod_lss <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = covid, k = 300, cache = TRUE)

head(coef(tmod_lss), 20) 
tail(coef(tmod_lss), 20) 

textplot_neg <- textplot_terms(tmod_lss, data_dicc_senti["negative"])

textplot_neg$labels[1] <- 'Polaridad'

tabla_polaridad <- textplot_neg$data

textplot_neg$labels[2] <- 'Frecuencia (log)'

textplot_neg + ggtitle('Palabras negativas usadas en los comentarios') +
  geom_text(color = 'violet', alpha = .1) +
  theme(plot.title = element_text(hjust = .5))

dfmat_doc <- dfm_group(dfmat_sent)
dat <- docvars(dfmat_doc)
dat$fit <- predict(tmod_lss, newdata = dfmat_doc)
#dat <- dat %>% filter(!is.na(fit))
dat$date <- dat$fecha

dat_smooth <- smooth_lss(dat, date_var = "date", engine = "locfit")
head(dat_smooth)

plot(dat$date, dat$fit, col = rgb(.5, .3, .9, 0.008), pch = 16, ylim = c(-0.5, 0.5),
     xlab = "Tiempo", ylab = "Sentimientos COVID")
abline(h = 0, lty = c(1, 2), col = 'skyblue',lwd = 2)
lines(dat_smooth$date, dat_smooth$fit, type = "l", col = 'darkred', lwd = 2)
lines(dat_smooth$date, dat_smooth$fit + dat_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96, type = "l", lty = 3)

# gráfico con ggplot2
png('./viz/graf_humor_social.png', width = 1200, height = 800, res = 150)
ggplot() + 
  geom_jitter(aes(x=date, y=fit/10), data = dat, 
              width = 0.5, height = 0.15, alpha=.02, color = 'purple') +
  geom_hline(yintercept = 0, color = 'purple', size = 1.5, alpha = .7) +
  geom_line(aes(dat_smooth$date, dat_smooth$fit), size = 1.5, color = 'darkred') + 
  geom_line(aes(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96,), size =.8, color = 'grey30', linetype = "dotted") + 
  geom_line(aes(dat_smooth$date, dat_smooth$fit + dat_smooth$se.fit * 1.96,), size =.8, color = 'grey30', linetype = "dotted") +
  scale_x_date(date_breaks = "1 months", 
               labels = scales::label_date_short(format = c("%Y", "%b"), sep = "\n"),
               expand=c(0,0)) +
  labs(title = 'Índice de humor social en torno a la pandemia y sus consecuencias en Mar del Plata',
       subtitle = expression(paste('Sentimientos positivos y negativos en los comenarios de lectorxs del diario ',italic("La Capital"))),
       x = NULL,y=NULL, caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5, color = 'grey30'),
        plot.subtitle = element_text(hjust = .5))
dev.off()
#####--modelado--de--topicos----#####
comentarios_lc_2020_2021_agrupados_nota <- readRDS('./data/notas_lc_con_comentarios_2020_2021.rds') %>% 
  group_by(Semana = ceiling_date(fecha_nota, "1 week")) %>% select(Semana, 1:32) %>% ungroup() %>% 
  group_by(Semana, id_join) %>% summarise(post = paste0(post, collapse = ' ')) %>% ungroup() %>% 
  mutate(id = row_number(), .before = Semana)

corp_comentarios <- corpus(x = comentarios_lc_2020_2021_agrupados_nota$post,
                           docnames = comentarios_lc_2020_2021_agrupados_nota$id,
                           docvars = data.frame(id_join = comentarios_lc_2020_2021_agrupados_nota$id_join,
                                                fecha = comentarios_lc_2020_2021_agrupados_nota$Semana))

corp_sent <- corpus_reshape(corp_comentarios, to =  "paragraphs")
toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(c(tm::stopwords('es'),'si','asi','año','mar','plata','ser',
                  'años','ahora','hace','menos','mas','hoy','luego','solo',
                  'entonces','aun','casi','sino','cada','ahí','ahi','días',
                  'dias','dia','día','tan','tambien','va','aquí','aqui','parece',
                  'dos','meses','vez','puede','tampoco','tambien','mil','10',
                  'mismo','misma','etc','','aca','acá','ciudad','aunque',
                  'cualquier','así','mdp','mdq','tres','sólo','solo','mardel',
                  'q', 'mar', 'plata', 'años', 'año', 'van', 'vas', 'va',
                  'ciudad','etc','hs'))


dfmat_mt <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = c('x','d','mismo','sino','ahora','dos','si','día','dia',
                         'foto','tal','sr','misma','vez','ahí','dias','días','aca',
                         'asi','así','acá')) %>%
  dfm_replace(pattern = c('tenes','pais','mas'), replacement = c('tenés','país','más')) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")


tmod_lda <- textmodel_lda(dfmat_mt, k = 32)

(topicos_32 <- terms(tmod_lda, 100))
saveRDS(topicos_32,'./data/topicos_32.rds')

head(topics(tmod_lda), 20)

dfmat_mt$topic <- topics(tmod_lda)
saveRDS(dfmat_mt,'./data/dfmat_mt_topicos_32.rds')

#tmod_lda$theta
#tmod_lda$data@docvars

(tabla_frec_topicos <- table(dfmat_mt$topic))

df_topicos <- dfmat_mt@docvars %>% filter(!is.na(topic))

df_topicos <- df_topicos %>% 
  mutate(topico = case_when(topic ==  'topic1' ~ 'vacunación',# vip
                           topic ==  'topic2' ~ 'aforo',#bancos, etc.
                           topic ==  'topic3' ~ 'aspo_dispo', # distanciamiento
                           topic ==  'topic4' ~ 'reapertura_comercios',
                           topic ==  'topic5' ~ 'fiestas_clandestinas_juventud',
                           topic ==  'topic6' ~ 'maradona_médicos_gobiernos_etc',
                           topic ==  'topic7' ~ 'buenos_deseos_reconocimiento_aplausos_heroés',
                           topic ==  'topic8' ~ 'situación_sanitaria',# oms otros países
                           topic ==  'topic9' ~ 'grieta',# peronsimo vs antiperonismo
                           topic == 'topic10' ~ 'insultos',
                           topic == 'topic11' ~ 'situación_pesca',
                           topic == 'topic12' ~ 'conflicto_transporte',
                           topic == 'topic13' ~ 'aspo_dispo',
                           topic == 'topic14' ~ 'situación_sanitaria_trabajadorxs_salud',
                           topic == 'topic15' ~ 'grieta',# peronismo vs antiperonismo
                           topic == 'topic16' ~ 'grieta',# criticas a los gobirneos local, provincial y nacional
                           topic == 'topic17' ~ 'turismo_playas',# distanciamiento
                           topic == 'topic18' ~ 'turismo_controles',
                           topic == 'topic19' ~ 'grieta',# protestas antiperonistas
                           topic == 'topic20' ~ 'problemáticas_barriales',
                           topic == 'topic21' ~ 'grieta',# anses raverta
                           topic == 'topic22' ~ 'grieta',# suave
                           topic == 'topic23' ~ 'situación_sanitaria',# geriátricos
                           topic == 'topic24' ~ 'grieta',# planes sociales vagos trabajar anses
                           topic == 'topic25' ~ 'situación_económica',# inflación
                           topic == 'topic26' ~ 'situación_sanitaria',# fallecimientos
                           topic == 'topic27' ~ 'el_cordobés_otros_casos',# varados aeropuerto
                           topic == 'topic28' ~ 'protocolos',# barbijos, alcohol, jabón, etc.
                           topic == 'topic29' ~ 'situación_sanitaria',# testeos, casos
                           topic == 'topic30' ~ 'presencialidad_escuelas',
                           topic == 'topic31' ~ 'curiosisdades_pandemia_animales_sueltos', # insultos (solo se nombre una vez al kirchnerismo)
                           topic == 'topic32' ~ 'situación_sanitaria',# testeos, casos
                           TRUE ~ as.character(topic)))

df_topicos %>% count(topico)

df_topicos <- df_topicos %>% mutate(topico2 = case_when(topic ==  'topic1' ~ 'vacunación',# vip
                                                        topic ==  'topic2' ~ 'restricciones',#bancos, etc.(restricciones)
                                                        topic ==  'topic3' ~ 'restricciones', # distanciamiento
                                                        topic ==  'topic4' ~ 'restricciones',
                                                        topic ==  'topic5' ~ 'fiestas_clandestinas_juventud',
                                                        topic ==  'topic6' ~ 'restricciones',
                                                        topic ==  'topic7' ~ 'buenos_deseos_reconocimiento_aplausos_heroés', #()
                                                        topic ==  'topic8' ~ 'situación_sanitaria',# oms otros países
                                                        topic ==  'topic9' ~ 'grieta',# peronismo vs antiperonismo
                                                        topic == 'topic10' ~ 'grieta',# indignación 
                                                        topic == 'topic11' ~ 'otros',
                                                        topic == 'topic12' ~ 'otros', #(otros)
                                                        topic == 'topic13' ~ 'restricciones', # (restricciones)
                                                        topic == 'topic14' ~ 'situación_sanitaria', #(situación sanitaria)
                                                        topic == 'topic15' ~ 'grieta',# peronismo vs antiperonismo
                                                        topic == 'topic16' ~ 'grieta',# criticas a los gobiernos local, provincial y nacional
                                                        topic == 'topic17' ~ 'protocolos',# distanciamiento (protocolos)
                                                        topic == 'topic18' ~ 'restricciones', #(restricciones)
                                                        topic == 'topic19' ~ 'grieta',# protestas antiperonistas
                                                        topic == 'topic20' ~ 'otros', # otros
                                                        topic == 'topic21' ~ 'grieta',# anses raverta
                                                        topic == 'topic22' ~ 'grieta',# suave
                                                        topic == 'topic23' ~ 'situación_sanitaria',# geriátricos
                                                        topic == 'topic24' ~ 'grieta',# planes sociales vagos trabajar anses
                                                        topic == 'topic25' ~ 'otros',# inflación (otros)
                                                        topic == 'topic26' ~ 'situación_sanitaria',# fallecimientos
                                                        topic == 'topic27' ~ 'restricciones',# varados aeropuerto
                                                        topic == 'topic28' ~ 'protocolos',# barbijos, alcohol, jabón, etc.
                                                        topic == 'topic29' ~ 'situación_sanitaria',# testeos, casos
                                                        topic == 'topic30' ~ 'presencialidad_escuelas',
                                                        topic == 'topic31' ~ 'curiosisdades_pandemia_animales_sueltos', # insultos (solo se nombre una vez al kirchnerismo)
                                                        topic == 'topic32' ~ 'situación_sanitaria',# testeos, casos
                                                        TRUE ~ as.character(topic)))

df_topicos %>% count(topico2)

png('./viz/graf_tm.png', width = 1200, height = 800, res = 150)
df_topicos %>% 
  mutate(mes = floor_date(fecha, "1 month")) %>% 
  group_by(mes) %>% 
  count(topico2) %>% 
  mutate(topico2 = recode(topico2, "buenos_deseos_reconocimiento_aplausos_heroés" = "Héroes", 
                          "curiosisdades_pandemia_animales_sueltos" = "Curiosidades", 
                          "fiestas_clandestinas_juventud" = "Fiestas candestinas",
                          'grieta'="Grieta", 'otros'="Otros temas", 'presencialidad_escuelas'="Presencialidad",
                          'protocolos'="Protocolos",'restricciones'="Restricciones",
                          'situación_sanitaria'="Situación sanitaria", 'vacunación'="Vacunación"
  )) %>% 
  ggplot(aes(x=mes,y=n,color=topico2)) +
  geom_line(size = 1, show.legend = F) +
  scale_x_date(date_breaks = "1 month", 
               labels = scales::label_date_short(format = c("%Y", "%b"), sep = "-"),
               expand=c(0,0)) +
  facet_wrap(.~topico2, scales = 'free_y', nrow = 3) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
       subtitle = 'Distribución mensual de menciones sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()


png('./viz/graf_tm_2.png', width = 1200, height = 800, res = 150)
df_topicos %>% 
  filter(fecha > '2020-01-31') %>%  
  mutate(mes = floor_date(fecha, "1 month")) %>% 
  group_by(mes) %>% 
  count(topico2) %>% 
  mutate(n = (n / sum(n))*100) %>% 
  ggplot(aes(x=mes,y=n,fill=topico2)) +
  geom_col(position = "stack", color = "black") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", 
               labels = scales::label_date_short(format = c("%Y", "%b"), sep = "-"),
               expand=c(0,0)) +
  scale_fill_discrete(labels = c("Héroes","Curiosidades","Fiestas \ncandestinas",
                                 "Grieta", "Otros temas", "Presencialidad","Protocolos",
                                 "Restricciones","Situación \nsanitaria", "Vacunación")) +
  labs(x=NULL,y=NULL,
       title = 'Temas emergentes en los comentarios de lectorxs - Mar del Plata, 2020-2021',
       subtitle = 'Procesamiento de los comentarios con técnicas de modelado de tópicos',
       caption = expression(paste('Fuente: ',italic("La Capital"))),
       fill = "     Temas") +
  theme_minimal() +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          strip.text = element_text(size = 10, color = "black", face = "bold"),
          strip.background = element_rect(fill = 'grey90'),
          axis.text = element_text(color = 'black', size = 7.5),
          axis.text.x = element_text(angle = -90, vjust = 0.1, size = 9.5),
          axis.text.y = element_blank(),
          #legend.position = c(0.9, 0.13),
          legend.key.size = unit(.1, 'cm'),
          legend.key.height = unit(1.2, 'cm'),
          legend.key.width = unit(.4, 'cm'), 
          legend.title = element_text(size=10),
          legend.text = element_text(size=8))
  dev.off()





