#####----Cargamos--los--paquetes--##################################
require(tidyverse)
require(tidytext)
require(lubridate)
require(RColorBrewer)
require(ggspatial)
require(scales)
require(sf)
require(ggpubr)
library(formattable)
require(kableExtra)
require(ggsflabel)
#####----Cargamos--las--base--de--datos--de--los--comentarios--###########
comentarios_lc_2020_2021 <- readRDS('./data/notas_lc_con_comentarios_2020_2021.rds') %>% 
  mutate(id = row_number(), .before = fecha_nota)
#####----Contamos--menciones-covid-y-afines--#######################
# creamos vectores de términos
COVID         <- c('covid','pandemi','coronavirus','sars-cov','sars cov')
Restricciones <- c('cuarentena','restricciones','aislamiento','distanciamiento',' aspo ',
                   ' dispo ','barbijo','tapaboca','fase 1','fase 2','fase 3','fase 4',
                   'fase 5','trabajadores esenciales','actividades esenciales')
Educación     <- c('padresorg','padres org','presencialidad','cierres de escuelas')
Fiestas       <- c('fiesta cland','fiestas cland')
Test          <- c('hisopado','pcr','testeo','antígeno','test rápido','operativo detectar')
Vacunación    <- c('vacunas','vacunación','vacunate','vacunatorio','dosis')
Vacunas       <- c('sputnik','pfizer','astrazeneca','sinopharm','covishield','covax',
                   'sinovac','johnson & johnson','janssen','soberana 02','soberana 2',
                   'vacuna de moderna','vacuna china','vacuna rusa','vacuna india',
                   'vacuna cubana')
Salud         <- c('contagios','terapia','respiradores','camas',' uti ','casos',
                   'fallecimientos','muertes')
CBE           <- c('cbe','comités barriales','emergencia alimentaria',
                   'barriales de emergencia','plan cuidarnos','programa cuidarnos')
# creamos la nuevas variables con los vectores
comentarios_lc_dicc <- comentarios_lc_2020_2021 %>% select(id,fecha_nota,post) %>% 
  mutate(Palabras_p      = sapply(strsplit(post, " "), length),
         COVID_p         = str_count(str_to_lower(post), paste(COVID, collapse = "|")),
         Restricciones_p = str_count(str_to_lower(post), paste(Restricciones, collapse = "|")),
         Educación_p     = str_count(str_to_lower(post), paste(Educación, collapse = "|")),
         Fiestas_p       = str_count(str_to_lower(post), paste(Fiestas, collapse = "|")),
         Test_p          = str_count(str_to_lower(post), paste(Test, collapse = "|")),
         Vacunación_p    = str_count(str_to_lower(post), paste(Vacunación, collapse = "|")),
         Vacunas_p       = str_count(str_to_lower(post), paste(Vacunas, collapse = "|")),
         Salud_p         = str_count(str_to_lower(post), paste(Salud, collapse = "|")),
         CBE_p           = str_count(str_to_lower(post), paste(CBE, collapse = "|")))
# reorganizamos las variables
comentarios_lc_dicc_largo <- comentarios_lc_dicc %>%
  gather('topic','frec',-c(1:4)) %>% mutate(topic = str_remove_all(topic, '_p'))
# armamos una tabla de frecuencias por semana
tabla_topicos_fb <- comentarios_lc_dicc_largo %>% 
  group_by(Semanas = ceiling_date(fecha_nota, "1 week")) %>%
  group_by(Semanas,topic) %>% summarise(frec=sum(frec)) %>% ungroup()
tabla_sum_topicos_fb <- comentarios_lc_dicc_largo %>% 
  group_by(Semanas = ceiling_date(fecha_nota, "1 week")) %>%
  group_by(Semanas) %>% summarise(frec=sum(frec)) %>% ungroup()
# hacemos las visualizaciones
# armamos una tabla de frecuencias por semana de comentarios
tabla_comentarios <- comentarios_lc_2020_2021 %>% 
  group_by(Semanas = ceiling_date(fecha_nota, "1 week")) %>%
  group_by(Semanas) %>% summarise(frec= n()) %>% ungroup()
# hacemos una visualización de las frecuencias generales de comentarios
png('./viz/graf_04.png', width = 1200, height = 600, res = 150)
tabla_comentarios %>% 
  ggplot(aes(x=Semanas,y=frec)) +
  geom_line(color='skyblue', size = 1, show.legend = F) +
  stat_smooth(geom = 'line', se = F, color = 'red', size = .8, alpha = .5) +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  labs(y=NULL,x=NULL,
       title = expression(paste('Los comentarios de lxs lectorxs de',
                                italic(" La Capital "), '(Mar del Plata, 2020-2021)')),
       subtitle = 'Frecuencia semanal de comentarios totales sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()
# hacemos una visualización de las frecuencias generales de menciones
png('./viz/graf_05.png', width = 1200, height = 600, res = 150)
tabla_topicos_fb %>% 
  ggplot(aes(x=Semanas,y=frec)) +
  geom_line(color='skyblue', size = 1, show.legend = F) +
  stat_smooth(geom = 'line', se = F, color = 'red', size = .8, alpha = .5) +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  labs(y=NULL,x=NULL,
       title = expression(paste('La COVID en los comentarios de lxs lectorxs de',
                                italic(" La Capital "), '(Mar del Plata, 2020-2021)')),
       subtitle = 'Frecuencia semanal de menciones totales sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()
# hacemos una unificación de las dos series temporales 
png('./viz/graf_04_05.png', width = 1200, height = 600, res = 150)
tabla_comentarios %>% 
  ggplot(aes(x=Semanas,y=rescale(frec,c(0,10)))) +
  geom_line(color='blue', size = 1, show.legend = F) +
  stat_smooth(geom = 'line', se = F, color = 'skyblue', size = .8, alpha = .8) +
  geom_line(aes(x=Semanas,
                y=rescale((tabla_sum_topicos_fb$frec/tabla_comentarios$frec),c(0,10))), 
            color='purple', size = 1, show.legend = F, data = tabla_sum_topicos) +
  stat_smooth(aes(x=Semanas,
                  y=rescale((tabla_sum_topicos_fb$frec/tabla_comentarios$frec),c(0,10))), geom = 'line', se = F, 
              color = 'violet', size = .8, alpha = .7) +
  #geom_line(aes(x=Semanas,
                #y=rescale((tabla_comentarios$frec/tabla_notas$frec),c(0,10))), 
            #color='green', size = 1, show.legend = F, data = tabla_sum_topicos) +
  annotate("text", x = as.Date('2020-01-13'), y = 5, 
           label = "valores reescalados", size = 3, 
           color = 'grey20', angle = -90) +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
       subtitle = 'Frecuencia semanal de comentarios y media de menciones sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()
# hacemos una unificación de las dos series temporales 
png('./viz/graf_00_04.png', width = 1200, height = 600, res = 150)
tabla_notas %>% 
  ggplot(aes(x=Semanas,y=rescale(frec,c(0,10)))) +
  geom_line(color='blue', size = 1, show.legend = F) +
  stat_smooth(geom = 'line', se = F, color = 'skyblue', size = .8, alpha = .8) +
  geom_line(aes(x=Semanas,
                y=rescale((tabla_comentarios$frec/tabla_notas$frec),c(0,10))), 
            color='purple', size = 1, show.legend = F, data = tabla_sum_topicos) +
  stat_smooth(aes(x=Semanas,
                  y=rescale((tabla_sum_topicos_fb$frec/tabla_comentarios$frec),c(0,10))), geom = 'line', se = F, 
              color = 'violet', size = .8, alpha = .7) +
  #geom_line(aes(x=Semanas,
  #y=rescale((tabla_comentarios$frec/tabla_notas$frec),c(0,10))), 
  #color='green', size = 1, show.legend = F, data = tabla_sum_topicos) +
  annotate("text", x = as.Date('2020-01-13'), y = 7.5, 
           label = "valores reescalados", size = 3, 
           color = 'grey20', angle = -90) +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
       subtitle = 'Frecuencia semanal de notas y media de comentarios por nota sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()
# hacemos una visualización para comparar tópicos
png('./viz/graf_06.png', width = 1400, height = 800, res = 150)
tabla_topicos %>% 
  group_by(Semanas, topic) %>% 
  summarise(frec = sum(frec)) %>% 
  ggplot(aes(x=Semanas, y=frec, fill = topic)) +
  geom_area(colour = 'grey30') +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  labs(y=NULL,x=NULL,
       title = expression(paste('La COVID en los comentarios de lxs lectorxs de',
                                italic(" La Capital "), '(Mar del Plata, 2020-2021)')),
       subtitle = 'Frecuencia semanal de menciones sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5)) +
  guides(fill = guide_legend(title="Agenda", override.aes = list(size = .5))) +
  theme(legend.position = c(0.08, 0.75),
        legend.key.size = unit(.1, 'cm'),
        legend.key.height = unit(.1, 'cm'),
        legend.key.width = unit(.4, 'cm'), 
        legend.title = element_text(size=11),
        legend.text = element_text(size=8))
dev.off()
# ahora vamos a hacer un facet wrap
png('./viz/graf_07.png', width = 1200, height = 800, res = 150)
tabla_topicos %>% 
  ggplot(aes(x=Semanas,y=frec,color=topic)) +
  geom_line(size = 1, show.legend = F) +
  scale_x_date(date_breaks = "1 months", 
               labels = scales::label_date_short(format = c("%Y", "%b"), sep = "-"),
               expand=c(0,0)) +
  facet_wrap(.~topic, scales = 'free_y', nrow = 3) +
  labs(y=NULL,x=NULL,
       title = expression(paste('La COVID en los comentarios de lxs lectorxs de',
                                italic(" La Capital "), '(Mar del Plata, 2020-2021)')),
       subtitle = 'Distribución semanal de menciones sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()
####---Top--20--posts---#####################################################
tabla_top_comentarios_notas <- comentarios_lc_2020_2021 %>% count(fecha_nota, titulo, link) %>% arrange(desc(n))

tabla_top_comentarios_notas %>% top_n(20) %>% select(-link) %>% 
  rename(Fecha = fecha_nota, 'Título' = titulo, Frec = n) %>% 
  kbl(caption = "<center><b>Top 20 de las notas sobre COVID más comentadas</b></center>") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  column_spec(1, width = "3cm") %>% 
  column_spec(3, width = "1cm") %>%
  footnote(general_title = '\nFuente:',
           general = 'La Capital',
           footnote_as_chunk = T, 
           title_format = "italic") %>% 
  save_kable('./viz/tabla_top_comentarios_notas.png', zoom = 6)
####---Top--20--lectorxs---#####################################################
# preparamos la base
tabla_top_comentarios_lectorxs <- comentarios_lc_2020_2021 %>% count(autor) %>% arrange(desc(n))
# hacemos la tabla
tabla_top_20_lectorxs <- tabla_top_comentarios_lectorxs %>% top_n(20)
tabla_top_lectorxs <- tabla_top_20_lectorxs %>% left_join(comentarios_lc_2020_2021, by='autor')
tabla_top_lectorxs <- tabla_top_lectorxs %>% group_by(autor,n) %>% 
  summarise(posts = paste0(post, collapse = ' ')) %>% unnest_tokens(palabras,posts) %>%
  group_by(autor) %>% count(palabras) %>% arrange(desc(n)) %>% 
  filter(n > 9) %>% filter(nchar(palabras) > 2) %>% 
  mutate(palabras = str_remove_all(palabras,'19')) %>% 
  filter(!str_detect(palabras, '[[:digit:]]')) %>% 
  anti_join(tibble(palabras = c(tm::stopwords('es'),'si','asi','año','mar','plata','ser',
                                'años','ahora','hace','menos','mas','hoy','luego','solo',
                                'entonces','aun','casi','sino','cada','ahí','ahi','días',
                                'dias','dia','día','tan','tambien','va','aquí','aqui','parece',
                                'dos','meses','vez','puede','tampoco','tambien','mil','10',
                                'mismo','misma','etc','','aca','acá','ciudad','aunque',
                                'cualquier','así','mdp','mdq','tres','sólo','solo','mardel'))) %>% 
  slice_max(order_by = n, n = 20) %>% 
  summarise(keywords = paste0('| ', palabras, collapse = ' ')) %>% select(autor,keywords) %>% 
  ungroup() %>% full_join(tabla_top_20_lectorxs, by = 'autor') %>% 
  select(autor, n, keywords) %>% arrange(desc(n))

tabla_top_lectorxs %>% 
  rename(Comentarios = n, Usuarix = autor, 'Palabras más usadas por lxs usuarixs' = keywords) %>% 
  kbl(caption = "<center><b>Top 20 de lxs lectorxs que más comentaron las notas sobre COVID</b></center>") %>%
  kable_classic(full_width = F, "striped") %>% 
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "1cm") %>%
  column_spec(3, width = "13cm") %>%
  row_spec(c(1:19), extra_css = "border-bottom: 1px solid") %>% 
  footnote(general_title = '\nFuente:',
           general = 'La Capital',
           footnote_as_chunk = T, 
           title_format = "italic") %>% 
  save_kable('./viz/tabla_top_comentaristas_notas.png', zoom = 8)
####---Top--20--megusteo---#####################################################
# preparamos la base
tabla_top_megusteo_comentarios <- comentarios_lc_2020_2021 %>% arrange(desc(megustas)) %>% .[1:20,] %>% 
  select(fecha_nota,titulo,post,megustas)
tabla_top_megusteo_comentarios$titulo[is.na(tabla_top_megusteo_comentarios$titulo)] <- 'El coronavirus no detiene su avance en Mar del Plata y ayer alcanzó un nuevo récord de contagios'
# hacemos la tabla
tabla_top_megusteo_comentarios <- tabla_top_megusteo_comentarios %>% top_n(20) %>%  
  rename(Fecha = fecha_nota, Título = titulo, Comentario = post, Megusteos = megustas)

string_fun <- function(x) {
  ifelse((sapply(strsplit(x, " "), length)) < 35,
         x, 
         paste(paste(unlist(strsplit(x, split = "\\s+"))[1:35],collapse=" "),'...continúa...'))
}

tabla_top_megusteo_comentarios$Comentario <- unlist(lapply(tabla_top_megusteo_comentarios$Comentario, string_fun))

tabla_top_megusteo_comentarios[1:10,] %>% 
  kable(align = c("cllc"),
        caption = "<center><b>Top 10 de los comentarios más megusteados en las notas sobre COVID</b></center>") %>% 
  kable_styling("striped", "hover", full_width = FALSE, font_size = 11) %>% 
  column_spec(1, width = "1cm") %>%
  column_spec(2, width = "4cm") %>%
  column_spec(3, width = "9cm") %>%
  column_spec(4, width = "1cm") %>%
  footnote(general_title = '\nFuente:',
           general = 'La Capital',
           footnote_as_chunk = T, 
           title_format = "italic") %>% 
  save_kable('./viz/tabla_top_megustas_comentarios_notas_1_10.png', zoom = 8)
####---


