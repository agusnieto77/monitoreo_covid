#####----Cargamos--los--paquetes--##################################
require(tidyverse)
require(lubridate)
require(RColorBrewer)
require(ggspatial)
require(scales)
require(sf)
require(ggpubr)
require(kableExtra)
require(ggsflabel)
require(treemap)
#####----Cargamos--la--base--de--datos--con--las--notas--###########
notas_lc <- readRDS('./data/notas_lc.rds')
#####----Contamos--menciones-covid-y-afines--#######################
# creamos vectores de términos
COVID         <- c('covid','pandemi','coronavirus','sars-cov','sars cov')
Restricciones <- c('cuarentena','restricciones','aislamiento','distanciamiento',' aspo ',
                   ' dispo ','barbijo','tapaboca','fase 1','fase 2','fase 3','fase 4',
                   'fase 5','trabajadores esenciales','actividades esenciales')
Educación     <- c('padresorg','padres org','presencialidad','cierres de escuelas')
Fiestas       <- c('fiesta cland','fiestas cland')
Test          <- c('hisopado','pcr','testeo','antígeno','test rápido','operativo detectar','jornada de detección')
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
notas_lc_dicc <- notas_lc %>% 
  mutate(Palabras      = sapply(strsplit(nota, " "), length),
         COVID         = str_count(str_to_lower(nota), paste(COVID, collapse = "|")),
         Restricciones = str_count(str_to_lower(nota), paste(Restricciones, collapse = "|")),
         Educación     = str_count(str_to_lower(nota), paste(Educación, collapse = "|")),
         Fiestas       = str_count(str_to_lower(nota), paste(Fiestas, collapse = "|")),
         Test          = str_count(str_to_lower(nota), paste(Test, collapse = "|")),
         Vacunación    = str_count(str_to_lower(nota), paste(Vacunación, collapse = "|")),
         Vacunas       = str_count(str_to_lower(nota), paste(Vacunas, collapse = "|")),
         Salud         = str_count(str_to_lower(nota), paste(Salud, collapse = "|")),
         CBE           = str_count(str_to_lower(nota), paste(CBE, collapse = "|")))
# reorganizamos las variables
notas_lc_dicc_largo <- notas_lc_dicc %>% gather('topic','frec',-c(1:8))
# armamos tre tablas de frecuencias por semana
(tabla_notas <- notas_lc_dicc %>% 
    mutate(sum_topic = rowSums(.[9:17])) %>% 
    filter(sum_topic > 0) %>% 
    group_by(Semanas = ceiling_date(fecha, "1 week")) %>% 
    group_by(Semanas) %>% 
    summarise(frec = n()))
tabla_topicos <- notas_lc_dicc_largo %>% 
  group_by(Semanas = ceiling_date(fecha, "1 week")) %>%
  group_by(Semanas,topic) %>% summarise(frec=sum(frec)) %>% ungroup()
tabla_sum_topicos <- notas_lc_dicc_largo %>% 
  group_by(Semanas = ceiling_date(fecha, "1 week")) %>%
  group_by(Semanas) %>% summarise(frec=sum(frec)) %>% ungroup()
# hacemos las visualizaciones
# hacemos una visualización de las frecuencias generales de notas covid
png('./viz/graf_00.png', width = 1200, height = 600, res = 150)
tabla_notas %>% 
  ggplot(aes(x=Semanas,y=frec)) +
  geom_segment(aes(x = as.Date('2020-07-12'), y = 110, 
                   xend = as.Date('2020-07-12'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-07-12'), y=110), 
             label='de 6 a 93 casos x sem', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2020-03-24'), y = 100, 
                   xend = as.Date('2020-03-24'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-03-24'), y=100), 
             label='24/03/20: 1er fallecimiento MDP', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2020-03-12'), y = 110, 
                   xend = as.Date('2020-03-12'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-03-12'), y=110), 
             label='12/03/20: 1er caso MDP', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2020-08-30'), y = 95, 
                   xend = as.Date('2020-08-30'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-08-30'), y=95), 
             label='de 543 a 1274 casos x sem', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2021-01-10'), y = 65, 
                   xend = as.Date('2021-01-10'), yend = 0)) +
  geom_label(aes(x=as.Date('2021-01-10'), y=65), 
             label='pico de casos', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2021-05-02'), y = 88, 
                   xend = as.Date('2021-05-02'), yend = 0)) +
  geom_label(aes(x=as.Date('2021-05-02'), y=88), 
             label='pico de casos', size = 2.5, hjust = 1) +
  geom_segment(aes(x = as.Date('2021-06-06'), y = 78, 
                   xend = as.Date('2021-06-06'), yend = 0)) +
  geom_label(aes(x=as.Date('2021-06-06'), y=78), 
             label='pico de casos', size = 2.5, hjust = 1) +
  geom_hline(yintercept = 0) +
  geom_line(color='skyblue', size = 1, show.legend = F) +
  stat_smooth(geom = 'line', se = F, color = 'red', size = .8, alpha = .5) +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  scale_y_continuous(limits =  c(0,115),
                     breaks = seq(0,115,25)) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
       subtitle = 'Frecuencia semanal de notas totales sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Notas: 4.427 - Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()
# hacemos una visualización de las frecuencias generales de menciones
png('./viz/graf_01.png', width = 1200, height = 600, res = 150)
tabla_sum_topicos %>% 
  ggplot(aes(x=Semanas,y=frec)) +
  geom_segment(aes(x = as.Date('2020-07-12'), y = 1000, 
                   xend = as.Date('2020-07-12'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-07-12'), y=1000), 
             label='de 6 a 93 casos x sem', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2020-03-24'), y = 900, 
                   xend = as.Date('2020-03-24'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-03-24'), y=900), 
             label='24/03/20: 1er fallecimiento MDP', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2020-03-12'), y = 750, 
                   xend = as.Date('2020-03-12'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-03-12'), y=750), 
             label='12/03/20: 1er caso MDP', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2020-08-30'), y = 850, 
                   xend = as.Date('2020-08-30'), yend = 0)) +
  geom_label(aes(x=as.Date('2020-08-30'), y=850), 
             label='de 543 a 1274 casos x sem', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2021-01-10'), y = 450, 
                   xend = as.Date('2021-01-10'), yend = 0)) +
  geom_label(aes(x=as.Date('2021-01-10'), y=450), 
             label='pico de casos', size = 2.5, hjust = 0) +
  geom_segment(aes(x = as.Date('2021-05-02'), y = 880, 
                   xend = as.Date('2021-05-02'), yend = 0)) +
  geom_label(aes(x=as.Date('2021-05-02'), y=880), 
             label='pico de casos', size = 2.5, hjust = 1) +
  geom_segment(aes(x = as.Date('2021-06-06'), y = 780, 
                   xend = as.Date('2021-06-06'), yend = 0)) +
  geom_label(aes(x=as.Date('2021-06-06'), y=780), 
             label='pico de casos', size = 2.5, hjust = 1) +
  geom_hline(yintercept = 0) +
  geom_line(color='skyblue', size = 1, show.legend = F) +
  stat_smooth(geom = 'line', se = F, color = 'red', size = .8, alpha = .5) +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  scale_y_continuous(limits =  c(0,1000),
                     breaks = seq(0,1000,250)) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
       subtitle = 'Frecuencia semanal de menciones totales sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Menciones: 32.178 - Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()
# hacemos una unificación de las dos series temporales 
png('./viz/graf_00_01.png', width = 1200, height = 600, res = 150)
tabla_notas %>% 
  ggplot(aes(x=Semanas,y=rescale(frec,c(0,10)))) +
  geom_line(color='blue', size = 1, show.legend = F) +
  stat_smooth(geom = 'line', se = F, color = 'skyblue', size = .8, alpha = .8) +
  geom_line(aes(x=Semanas,y=rescale((tabla_sum_topicos$frec/tabla_notas$frec),c(0,10))), 
            color='purple', size = 1, show.legend = F, data = tabla_sum_topicos) +
  stat_smooth(aes(x=Semanas,y=rescale((tabla_sum_topicos$frec/tabla_notas$frec),c(0,10))), geom = 'line', se = F, 
              color = 'violet', size = .8, alpha = .7) +
  annotate("text", x = as.Date('2020-01-13'), y = 5, 
           label = "valores reescalados", size = 3, 
           color = 'grey20', angle = -90) +
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
       subtitle = 'Frecuencia semanal de notas y media de menciones sobre tópicos relacionados con la pandemia',
       caption = expression(paste('Fuente: ',italic("La Capital")))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        strip.background = element_rect(fill = 'grey90'),
        axis.text = element_text(color = 'black', size = 7.5),
        axis.text.x = element_text(angle = -90, vjust = 0.1, size = 6.5))
dev.off()

# hacemos una visualización para comparar tópicos en mosaico
png('./viz/graf_treemap.png', width = 1400, height = 900, res = 150)
treemap(tabla_topicos,
        index="topic",
        vSize="frec",
        type="index",
        title = 'Frecuencia de tópicos relacionados con la pandemia en la agenda mediática local (Mar del Plata, 2020-2021)',
        fontsize.title = 12
)
dev.off()
# hacemos una visualización para comparar tópicos
png('./viz/graf_02.png', width = 1400, height = 800, res = 150)
tabla_topicos %>% 
  group_by(Semanas, topic) %>% 
  summarise(frec = sum(frec)) %>% 
  ggplot(aes(x=Semanas, y=frec, fill = topic)) +
  geom_area(colour = 'grey30') +
  scale_x_date(date_breaks = "7 days", 
               labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "-"),
               expand=c(0,0)) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
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
png('./viz/graf_03.png', width = 1200, height = 800, res = 150)
tabla_topicos %>% 
  ggplot(aes(x=Semanas,y=frec,color=topic)) +
  geom_line(size = 1, show.legend = F) +
  scale_x_date(date_breaks = "1 months", 
               labels = scales::label_date_short(format = c("%Y", "%b"), sep = "-"),
               expand=c(0,0)) +
  facet_wrap(.~topic, scales = 'free_y', nrow = 3) +
  labs(y=NULL,x=NULL,
       title = 'La COVID en la agenda mediática local (Mar del Plata, 2020-2021)',
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
#####----Georreferenciamos--menciones-covid-y-afines--#######################
# preparamos la base
mapa_topic <- notas_lc_dicc_largo %>% filter(frec > 0) %>% 
  spread(topic,frec, fill = 0) %>% mutate(id = row_number(), .before = fecha)
# creamos las variables para los barrios
mapa_topic_barrios <- mapa_topic %>% 
  mutate(FRAY_LUIS_BELTRAN = ifelse(str_detect(nota, "Las Dalias|Alto Camet|Fray Beltrán|Beltrán|Fray Luis Beltrán|Luis Beltrán|Fray Beltran|Fray Luis Beltran|Luis Beltran"),1,0 ),
         LAS_AMERICAS = ifelse(str_detect(nota, "Las Américas|Las Americas|las américas|las americas"),1,0 ),
         LA_FLORIDA = ifelse(str_detect(nota, "La Florida"),1,0 ),
         SAN_JACINTO = ifelse(str_detect(nota, "San Jacinto|Verde Mundo"),1,0 ),
         PARQUE_INDEPENDENCIA = ifelse(str_detect(nota, "Parque Independencia"),1,0 ),
         NUEVO_GOLF = ifelse(str_detect(nota, "Nuevo Golf"),1,0 ),
         zona_sur = ifelse(str_detect(nota, "zona sur|zona Sur|Zona Sur|sur de la ciudad|sur de Mar del Plata"),1,0 ),
         EL_JARDIN_DE_PERALTA_RAMOS = ifelse(str_detect(nota, "Jardín de Peralta Ramos"),1,0 ),
         CAMET_FELIX_U. = ifelse(str_detect(nota, "Felix U. Camet|Felix Camet|Félix U. Camet|Félix Camet"),1,0 ),
         BOSQUE_PERALTA_RAMOS = ifelse(str_detect(nota, "Bosque de Peralta Ramos|Bosque Peralta Ramos|bosque de Peralta Ramos"),1,0 ),
         RIVADAVIA_BERNARDINO = ifelse(str_detect(nota, "Centenario|Bernardino Rivadavia"),1,0 ),
         EL_JARDIN_DE_STELLA_MARIS = ifelse(str_detect(nota, "Jardín de Stella Maris"),1,0 ),
         LOMAS_DE_STELLA_MARIS = ifelse(str_detect(nota, "Lomas de Stella Maris"),1,0 ),
         'PARQUE_MONTEMAR-EL_GROSELLAR' = ifelse(str_detect(nota, "El Grosellar|Parque Montemar"),1,0 ),
         EL_MARTILLO = ifelse(str_detect(nota, "El Martillo"),1,0 ),
         LAS_AVENIDAS = ifelse(str_detect(nota, "Las Avenidas"),1,0 ),
         DE_LAS_HERASJUAN_GREGORIO = ifelse(str_detect(nota, "Las Heras"),1,0 ),
         SANTA_ROSA_DEL_MAR_DE_PERALTA_RAMOS = ifelse(str_detect(nota, "Santa Rosa|Monte Terrabusi|Antártida Argentina"),1,0 ),
         PLAYA_SERENA = ifelse(str_detect(nota, "Playa Serena|playa Serena"),1,0 ),
         SAN_PATRICIO = ifelse(str_detect(nota, "San Patricio"),1,0 ),
         LOS_ACANTILADOS = ifelse(str_detect(nota, "Acantilados|Costa Azul|Mar y Sol"),1,0 ),
         ROLDAN_BELISARIO = ifelse(str_detect(nota, "Belisario Roldán|Belisario Roldan"),1,0 ),
         SAN_CARLOS = ifelse(str_detect(nota, "San Carlos"),1,0 ),
         DIVINO_ROSTRO = ifelse(str_detect(nota, "Divino Rostro"),1,0 ),
         AEROPARQUE = ifelse(str_detect(nota, "Aeroparque"),1,0 ),
         NEWBERY_JORGE = ifelse(str_detect(nota, "Jorge Newbery"),1,0 ),
         BOSQUE_GRANDE = ifelse(str_detect(nota, "Bosque Grande"),1,0 ),
         SANTA_PAULA = ifelse(str_detect(nota, "Santa Paula"),1,0 ),
         GENERAL_BELGRANO = ifelse(str_detect(nota, "Barrio Belgrano|barrio Belgrano|General Belgrano|Gral. Belgrano"),1,0 ),
         ALFAR = ifelse(str_detect(nota, "Alfar"),1,0 ),
         PARQUE_INDEPENDENCIA = ifelse(str_detect(nota, "Parque Independencia"),1,0 ),
         ESTACION_CHAPADMALAL = ifelse(str_detect(nota, "Estación Chapadmalal"),1,0 ),
         CERRITO_SUR = ifelse(str_detect(nota, "Cerrito"),1,0 ),
         CERRITO_Y_SAN_SALVADOR = ifelse(str_detect(nota, "San Salvador"),1,0 ),
         SANCHEZ_FLORENCIO = ifelse(str_detect(nota, "Florencio Sánchez"),1,0 ),
         PARQUE_HERMOSO_Y_VALLE_HERMOSO = ifelse(str_detect(nota, "Parque Hermoso|Valle Hermoso"),1,0 ),
         PARQUE_CAMET = ifelse(str_detect(nota, "Parque Camet|Parque Peña|parque Camet|parque Peña"),1,0 ),
         SIERRA_DE_LOS_PADRES = ifelse(str_detect(nota, "Sierra de los Padres|sierra de los Padres|sierra de los padres|Sierra de Los Padres"),1,0 ),
         EL_MARQUESADO = ifelse(str_detect(nota, "Marquesado"),1,0 ),
         PLAYA_LOS_LOBOS = ifelse(str_detect(nota, "Barranca de los Lobos|barranca de los Lobos|barranca de los lobos|Playa Los Lobos|Playa los Lobos|Playa los lobos|playa los lobos|playa los Lobos|playa Los Lobos"),1,0 ),
         DE_LA_PLAZA_FORTUNATO = ifelse(str_detect(nota, "Villa Evita|villa Evita|Fortunato de la Plaza|Fortunato de La Plaza|Fortunato De La Plaza"),1,0 ),
         SAN_CARLOS = ifelse(str_detect(nota, "Villa de Paso"),1,0 ),
         PERALTA_RAMOS_OESTE = ifelse(str_detect(nota, "Estadio José María Minella|Peralta Ramos Oeste|Peralta Ramos oeste"),1,0 ),
         QUEBRADAS_DE_PERALTA_RAMOS = ifelse(str_detect(nota, "Quebradas de Peralta Ramos"),1,0 ),
         JOSE_HERNANDEZ = ifelse(str_detect(nota, "José Hernández|Jose Hernandez|Ostende"),1,0 ),
         AUTODROMO = ifelse(str_detect(nota, "Autódromo|Autodromo"),1,0 ),
         DON_EMILIO = ifelse(str_detect(nota, "Don Emilio"),1,0 ),
         VILLA_PRIMERA = ifelse(str_detect(nota, "Villa Primera|villa Primera"),1,0 ),
         PARQUE_PALERMO = ifelse(str_detect(nota, "Parque Palermo"),1,0 ),
         DON_BOSCO = ifelse(str_detect(nota, "Don Bosco"),1,0 ),
         SAN_JUAN = ifelse(str_detect(nota, "San Juan"),1,0 ),
         SANTA_CELINA = ifelse(str_detect(nota, "Santa Celina|Don Diego"),1,0 ),
         COLINAS_DE_PERALTA_RAMOS = ifelse(str_detect(nota, "Colinas de Peralta Ramos|barrio Peralta Ramos"),1,0 ),
         COLINA_ALEGRE = ifelse(str_detect(nota, "Colina Alegre"),1,0 ),
         GENERAL_PUEYRREDON = ifelse(str_detect(nota, "barrio Pueyrredon|barrio General Pueyrredon|barrio Gral. Pueyrredon|barrio Pueyrredón|barrio General Pueyrredón|barrio Gral. Pueyrredón"),1,0 ),
         GENERAL_SAN_MARTIN = ifelse(str_detect(nota, "barrio San Martín|barrio General San Martín|Barrio San Martín|Barrio General San Martín|Barrio Gral San Martín|Barrio Gral. San Martín"),1,0 ),
         DOS_DE_ABRIL = ifelse(str_detect(nota, "barrio 2 de Abril|barrio 2 de abril|Dos de Abril|Dos de abril|dos de abril|Retazo"),1,0 ),
         DEL_PUERTO = ifelse(str_detect(nota, "barrio puerto|barrio Puerto|barrio del puerto|barrio del Puerto|barrio Del Puerto|barrio del puerto"),1,0 ),
         VIRGEN_DE_LUJAN = ifelse(str_detect(nota, "Virgen de Luján"),1,0 ),
         ESTACION_CAMET = ifelse(str_detect(nota, "Estación Camet|Castagnino|La Laura|El Tejado|El Sosiego"),1,0 ),
         SAN_EDUARDO_DEL_MAR = ifelse(str_detect(nota, "San Eduardo|San Eduardo del Mar"),1,0 ),
         LA_GLORIA_DE_LA_PEREGRINA = ifelse(str_detect(nota, "Colinas Verdes"),1,0 ),
         LA_PEREGRINA = ifelse(str_detect(nota, "El Coyunco|La Peregrina"),1,0 ),
         ARROYO_CHAPADMALAL = ifelse(str_detect(nota, "Arroyo Chapadmalal"),1,0 ),
         HIPODROMO = ifelse(str_detect(nota, "Hipódromo"),1,0 ),
         LOS_TILOS = ifelse(str_detect(nota, "Los Tilos|Etchepare"),1,0 ),
         NUEVE_DE_JULIO = ifelse(str_detect(nota, "9 de Julio|Nueve de Julio|9 de julio|nueve de julio"),1,0 ),
         AMEGHINO_FLORENTINO = ifelse(str_detect(nota, "Ameghino|La Zulema"),1,0 ),
         LOMAS_DEL_GOLF = ifelse(str_detect(nota, "Lomas del Golf|Lomas Del Golf"),1,0 ),
         BOSQUE_ALEGRE = ifelse(str_detect(nota, "Bosque Alegre|bosque Alegre"),1,0 ),
         ESTRADA_JOSE_MANUEL = ifelse(str_detect(nota, "Estrada|Caisamar"),1,0 ),
         CONSTITUCION = ifelse(str_detect(nota, "Barrio Constitución|barrio Constitución"),1,0 ),
         CORONEL_DORREGO = ifelse(str_detect(nota, "Barrio Dorrego|barrio Coronel Dorrego|Barrio Coronel Dorrego|barrio Dorrego"),1,0 ),
         EL_PROGRESO = ifelse(str_detect(nota, "El Progreso"),1,0 ),
         FARO_NORTE = ifelse(str_detect(nota, "El Faro|Faro norte|Faro Norte"),1,0 ),
         LOPEZ_DE_GOMARA = ifelse(str_detect(nota, "López de Gomara|Lopez de Gomara|Lopez De Gomara|López De Gomara"),1,0 ),
         JURAMENTO = ifelse(str_detect(nota, "Juramento"),1,0 ),
         LAS_CANTERAS = ifelse(str_detect(nota, "Las Canteras"),1,0 ),
         LA_PERLA = ifelse(str_detect(nota, "La Perla"),1,0 ),
         LAS_RETAMAS = ifelse(str_detect(nota, "Las Retamas"),1,0 ),
         LIBERTAD = ifelse(str_detect(nota, "barrio Libertad|Barrio Libertad"),1,0 ),
         LOS_PINARES = ifelse(str_detect(nota, "Los Pinares|San Gerónimo|San Geronimo|San Jerónimo|San Jeronimo"),1,0 ),
         LOS_TRONCOS = ifelse(str_detect(nota, "Los Troncos"),1,0 ),
         VILLA_LOURDES = ifelse(str_detect(nota, "Villa Lourdes|villa Lourdes"),1,0 ),
         MALVINAS_ARGENTINAS = ifelse(str_detect(nota, "Malvinas Argentinas"),1,0 ),
         NUEVA_POMPEYA = ifelse(str_detect(nota, "Nueva Pompeya|plaza Pueyrredon|Plaza Pueyrredon"),1,0 ),
         PARQUE_LURO = ifelse(str_detect(nota, "Parque Luro|Barrio Luro|parque Luro|barrio Luro|Canchita de los Bomberos|canchita de los bomberos| de los Bomberos"),1,0 ),
         PINOS_DE_ANCHORENA = ifelse(str_detect(nota, "Pinos de Anchorena"),1,0 ),
         PLAYA_CHAPADMALAL = ifelse(str_detect(nota, "Playa Chapadmalal"),1,0 ),
         PLAYA_GRANDE = ifelse(str_detect(nota, "Playa Grande"),1,0 ),
         PRIMERA_JUNTA = ifelse(str_detect(nota, "Primera Junta"),1,0 ),
         PUNTA_MOGOTES = ifelse(str_detect(nota, "Mogotes"),1,0 ),
         SAN_ANTONIO = ifelse(str_detect(nota, "San Antonio"),1,0 ),
         SAN_JOSE = ifelse(str_detect(nota, "San José"),1,0 ),
         SANTA_MONICA = ifelse(str_detect(nota, "Santa Mónica|Santa Monica"),1,0 ),
         SANTA_RITA = ifelse(str_detect(nota, "Santa Rita"),1,0 ),
         SANTA_ROSA_DE_LIMA = ifelse(str_detect(nota, "Santa Rosa de Lima"),1,0 ),
         SARMIENTO_DOMINGO_FAUSTINO = ifelse(str_detect(nota, "barrio Sarmiento|Barrio Sarmiento|barrio Domingo Faustino Sarmiento|Barrio Domingo Faustino Sarmiento"),1,0 ),
         TERMAS_HUINCO = ifelse(str_detect(nota, "Termas Huinco"),1,0 ),
         VIRGEN_DE_LUJAN = ifelse(str_detect(nota, "Virgen de Lujan"),1,0 ),
         ZACAGNINI_JOSE_MANUEL = ifelse(str_detect(nota, "Zacagnini"),1,0 ),
         BATAN = ifelse(str_detect(nota, "vecinos de Batán|en Batán|En Batán|batanense"),1,0 ),
         VILLA_SERRANA = ifelse(str_detect(nota, "Villa Serrana|villa Serrana"),1,0 ),
         EL_COLMENAR = ifelse(str_detect(nota, "El Colmenar|Barrio Colmenar|barrio Colmenar"),1,0 ),
         LOMAS_DE_BATAN = ifelse(str_detect(nota, "Lomas de Batán"),1,0 ),
         PARQUE_INDUSTRIAL = ifelse(str_detect(nota, "Parque Industrial"),1,0 ),
         EL_BOQUERON = ifelse(str_detect(nota, "El Boquerón|El Boqueron"),1,0 ),
         LA_GERMANA = ifelse(str_detect(nota, "La Germana"),1,0 ),
         PARQUE_EL_CASAL = ifelse(str_detect(nota, "El Casal"),1,0 ),
         COLONIA_BARRAGAN = ifelse(str_detect(nota, "Colonia Barragán"),1,0 ),
         LAS_MARGARITAS = ifelse(str_detect(nota, "Las Margaritas"),1,0 ),
         AREA_CENTRO = ifelse(str_detect(nota, "microcentro|Plaza Colón|Plaza Mitre|Plaza San Martín|Palacio Municipal|Concejo Deliberante"),1,0 ),
         LA_HERRADURA = ifelse(str_detect(nota, "La Herradura"),1,0 ),
         SAN_JORGE = ifelse(str_detect(nota, "San Jorge"),1,0 ),
         SAN_CAYETANO = ifelse(str_detect(nota, "San Cayetano"),1,0 ),
         ESTACION_NORTE = ifelse(str_detect(nota, "Estación Norte"),1,0 ),
         CARIBE = ifelse(str_detect(nota, "Barrio Caribe"),1,0 ),
         LAS_LILAS = ifelse(str_detect(nota, "Las Lilas"),1,0 ),
         REGIONAL = ifelse(str_detect(nota, "Barrio Regional"),1,0 ),
         LOS_ANDES = ifelse(str_detect(nota, "Los Andes"),1,0 ),
         PLAZA_PERALTA_RAMOS = ifelse(str_detect(nota, "Plaza Peralta Ramos"),1,0 ),
         EL_GAUCHO = ifelse(str_detect(nota, "El Gaucho"),1,0 ),
         FUNES_Y_SAN_LORENZO = ifelse(str_detect(nota, "Funes y San Lorenzo"),1,0 ),
         GENERAL_ROCA = ifelse(str_detect(nota, "General Roca|Gral. Roca|Gral Roca"),1,0 ),
         CAMINO_A_NECOCHEA = ifelse(str_detect(nota, "Camino a Necochea"),1,0 ),
         ANTARTIDA_ARGENTINA = ifelse(str_detect(nota, "Antártida Argentina"),1,0 ),
         RUMENCO = ifelse(str_detect(nota, "Rumencó|Rumenco"),1,0 ),
         LOS_ZORZALES = ifelse(str_detect(nota, "Los Zorzales"),1,0 ),
         LA_ARBOLEDA = ifelse(str_detect(nota, "La Arboleda"),1,0 ),
         ARENAS_DEL_SUR  = ifelse(str_detect(nota, "Arenas del Sur"),1,0 ),
         SAN_EDUARDO_DE_CHAPADMALAL = ifelse(str_detect(nota, "San Eduardo de Chapadmalal"),1,0 ),
         ESTACION_TERMINAL = ifelse(str_detect(nota, "Estación Terminal"),1,0 ),
         LEANDRO_N._ALEM  = ifelse(str_detect(nota, "Barrio Alem|Leandro N. Alem"),1,0 )) %>% 
  mutate(sum_barrios = rowSums(.[19:148])) %>% filter(sum_barrios > 0) %>% select(-149) %>% 
  gather('Barrio',"Frec", -c(1:18)) %>% filter(Frec > 0)
# hacemos un sumario
sumario_mapa <- mapa_topic_barrios %>% group_by(Barrio) %>% 
  summarise(Palabras      = sum(Palabras),
            MencionesB    = sum(Frec),
            CBE           = sum(CBE),
            COVID         = sum(COVID),
            Educación     = sum(Educación),
            Fiestas       = sum(Fiestas),
            Restricciones = sum(Restricciones),
            Salud         = sum(Salud),
            Test          = sum(Test),
            Vacunación    = sum(Vacunación),
            Vacunas       = sum(Vacunas)) %>% 
  mutate(sum_topic = rowSums(.[3:11]),
         IBPC      = round((sum_topic/Palabras)*MencionesB,2)) %>% 
  mutate(Barrio = str_replace_all(Barrio, '_', ' '))
# hacemos mapas 
# datos gis
barrios <- readRDS('./data/barriosmdp.rds') %>% rename(Barrio = BARRIOS) %>% left_join(sumario_mapa)
barrios[is.na(barrios)] <- 0
#mapa1
(mapa1 <- ggplot(barrios) +
  geom_sf(fill = alpha("purple",0.9)) +
  geom_sf(aes(fill = IBPC), data = barrios %>% filter(Barrio !='AREA CENTRO' & Barrio != 'BATAN')) +
  geom_sf_text(aes(label=ID), fontface = "bold", color = 'black', size = 2.5, 
               data = barrios %>% filter(IBPC >= 0.29 & IBPC < 1.1)) +
  geom_sf_text(aes(label=ID), fontface = "bold", color = 'white', size = 2.5, 
               data = barrios %>% filter(IBPC > 1.1)) +
  scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                       name="",
                       na.value = "grey100", 
                       values =  scales::rescale(seq(0, 20, length.out = 6))) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), 
                         pad_y = unit(0.25, "in"),
                         height = unit(.8, "cm"), 
                         width = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size = 8, color = 'black'),
        axis.text.y = element_text(angle = -90, hjust = .5, size = 8, color = 'black'),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 7, color = 'black'),
        legend.position = c(0.9, 0.13),
        legend.key.size = unit(.1, 'cm'),
        legend.key.height = unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm'), 
        legend.title = element_text(size=11),
        legend.text = element_text(size=8)))
# tabla top 20
tabla_ibpc_20 <- st_set_geometry(barrios, NULL) %>% filter(Barrio != 'zona sur') %>% 
  mutate(Barrio = case_when(Barrio == 'AREA CENTRO' ~ 'CENTRO',
                            Barrio == 'DE LAS HERASJUAN GREGORIO' ~ 'LAS HERAS',
                            Barrio == 'SANTA ROSA DEL MAR DE PERALTA RAMOS' ~ 'SANTA ROSA',
                            Barrio == 'NUEVE DE JULIO' ~ '9 DE JULIO',
                            Barrio == 'DE LA PLAZA FORTUNATO' ~ 'FORTUNATO DE LA PLAZA',
                            Barrio == 'AMEGHINO FLORENTINO' ~ 'FLORENTINO AMEGHINO',
                            TRUE ~ as.character(Barrio))) %>% 
  select(ID, Barrio, IBPC) %>% arrange(desc(IBPC)) %>% top_n(20) %>% 
  mutate(order = 20:1)
# geom_bar top 20 
(viz_bar_top_20 <- ggplot(aes(x=order, y=IBPC, fill=IBPC), data = tabla_ibpc_20 %>% filter(IBPC < 2)) +
  geom_bar(stat = 'identity') +
  geom_bar(aes(x=order, y=IBPC), stat = 'identity', fill = alpha("purple",0.9), 
           data = tabla_ibpc_20 %>% filter(IBPC > 2)) +
  geom_text(aes(label=Barrio),color = 'black', hjust = 0, size = 2.23, nudge_y = .2, 
            data = tabla_ibpc_20 %>% filter(IBPC < 2)) +
  annotate('text', x = 20, y = 4, size = 2.23, label = 'CENTRO', color = 'white') +
  annotate('text', x = 19, y = 2.7, size = 2.23, label = 'BATAN') +
  scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu"))) +
  scale_x_continuous(breaks = seq(1, 20, by=1), # paste(tabla_ibpc_20 %>% arrange(order) %>% select(ID) %>% as_vector() %>% as.vector() %>% as.character(), collapse = "','")
                     label = c('46','83','51','40','33','92','19','66','15','42',
                               '113','79','107','29','63','59','23','58','100','44'),
                     expand=c(0.01,0.01)) +
scale_y_continuous(expand=c(0.01,0.01)) +
  coord_flip() +
  labs(x=NULL,y=NULL) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 7, color = 'black'),
        axis.text.x = element_text(size = 7, color = 'black')
        ))
# 2 en 1
(mapa_1 <- ggarrange(mapa1, viz_bar_top_20, widths = c(2,1.5), ncol = 2, nrow = 1))
png('./viz/mapa_1.png', width = 900, height = 900, res = 100)
annotate_figure(mapa_1,
                top = text_grob("IBPC: Índice Barrial de Problemáticas relativas a la COVID [Mar del Plata, 2020-2021]", 
                                color = "grey30", size = 12),
                bottom = text_grob(expression(paste("Fuente: ",italic("La Capital"))), 
                                   color = "grey40", size = 9, hjust = -3))
dev.off()
# mapa2
barrios <- barrios %>% mutate(IIBPC = round((sum_topic/MencionesB)/10,2),
                              IIBPC = case_when(is.na(IIBPC) ~ 0, TRUE ~ as.numeric(IIBPC)))
(mapa2 <- ggplot(barrios) +
    geom_sf(fill = alpha("purple",0.9)) +
    geom_sf(aes(fill = IIBPC), data = barrios %>% filter(Barrio !='GENERAL ROCA')) +
    geom_sf_text(aes(label=ID), fontface = "bold", color = 'black', size = 2.5, 
                 data = barrios %>% filter(IIBPC > 0.99 & IIBPC < 2)) +
    #geom_sf_text_repel(aes(label=ID), fontface = "bold", color = 'black', size = 2.5, 
                       #nudge_x = -8, nudge_y = 1,
                 #data = barrios %>% filter(IIBPC > 1.80)) +
    scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                         name="",
                         na.value = "grey100", 
                         values =  scales::rescale(seq(0, 20, length.out = 6))) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.1, "in"), 
                           pad_y = unit(0.25, "in"),
                           height = unit(.8, "cm"), 
                           width = unit(.8, "cm"),
                           style = north_arrow_fancy_orienteering) +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size = 8, color = 'black'),
          axis.text.y = element_text(angle = -90, hjust = .5, size = 8, color = 'black'),
          plot.title = element_text(hjust = .5),
          plot.caption = element_text(size = 7, color = 'black'),
          legend.position = c(0.9, 0.13),
          legend.key.size = unit(.1, 'cm'),
          legend.key.height = unit(.5, 'cm'),
          legend.key.width = unit(.4, 'cm'), 
          legend.title = element_text(size=11),
          legend.text = element_text(size=8)))
# tabla top 20
tabla_ibpc_20 <- st_set_geometry(barrios, NULL) %>% filter(Barrio != 'zona sur') %>% 
  mutate(Barrio = case_when(Barrio == 'AREA CENTRO' ~ 'CENTRO',
                            Barrio == 'DE LAS HERASJUAN GREGORIO' ~ 'LAS HERAS',
                            Barrio == 'SANTA ROSA DEL MAR DE PERALTA RAMOS' ~ 'SANTA ROSA',
                            Barrio == 'NUEVE DE JULIO' ~ '9 DE JULIO',
                            Barrio == 'DE LA PLAZA FORTUNATO' ~ 'FORTUNATO DE LA PLAZA',
                            Barrio == 'AMEGHINO FLORENTINO' ~ 'FLORENTINO AMEGHINO',
                            Barrio == 'SAN EDUARDO DE CHAPADMALAL' ~ 'SAN EDUARDO',
                            TRUE ~ as.character(Barrio))) %>% 
  select(ID, Barrio, IIBPC) %>% arrange(desc(IIBPC)) %>% top_n(20) %>% 
  mutate(order = 20:1)
# geom_bar top 20 
(viz_bar_top_20_2 <- ggplot(aes(x=order, y=IIBPC, fill=IIBPC), data = tabla_ibpc_20 %>% filter(IIBPC < 2.1)) +
    geom_bar(stat = 'identity') +
    geom_bar(aes(x=order, y=IIBPC), stat = 'identity', fill = alpha("purple",0.9), 
             data = tabla_ibpc_20 %>% filter(IIBPC > 2.2)) +
    geom_text(aes(label=Barrio),color = 'black', hjust = 0, size = 2.23, nudge_y = .2, 
              data = tabla_ibpc_20 %>% filter(IIBPC < 2.2)) +
    annotate('text', x = 20, y = 2.9, size = 2.23, label = 'GENERAL ROCA', color = 'white') +
    scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu"))) +
    scale_x_continuous(breaks = seq(1, 20, by=1), # paste(tabla_ibpc_20 %>% arrange(order) %>% select(ID) %>% as_vector() %>% as.vector() %>% as.character(), collapse = "','")
                       label = c('113','21','61','38','23','107','46','59','58','48',
                                 '75','18','100','60','15','51','29','64','53','55'),
                       expand=c(0.01,0.01)) +
    scale_y_continuous(expand=c(0.01,0.01)) +
    coord_flip() +
    labs(x=NULL,y=NULL) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text.y = element_text(size = 8, color = 'black'),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 7, color = 'black'),
          axis.text.x = element_text(size = 7, color = 'black')
    ))
# 2 en 1
(mapa_2 <- ggarrange(mapa2, viz_bar_top_20_2, widths = c(2,1.5), ncol = 2, nrow = 1))
png('./viz/mapa_2.png', width = 900, height = 900, res = 100)
annotate_figure(mapa_2,
                top = text_grob("IIBPC: Índice de Intensidad Barrial de Problemáticas relativas a la COVID [Mar del Plata, 2020-2021]", 
                                color = "grey30", size = 12),
                bottom = text_grob(expression(paste("Fuente: ",italic("La Capital"))), 
                                   color = "grey40", size = 9, hjust = -3))
dev.off()
#mapa3
barrios_gather <- barrios %>% gather('topico','valor', -c(1:5,15:17)) %>% 
  mutate(IIBPCD = round((valor/MencionesB)/10,2),
         IIBPCD = case_when(is.na(IIBPCD) ~ 0, TRUE ~ as.numeric(IIBPCD)))
(mapa3 <- ggplot(barrios_gather %>% filter(topico == 'CBE')) +
    geom_sf(aes(fill = IIBPCD)) +
    geom_sf_text(aes(label=ID), fontface = "bold", color = 'black', size = 2.5, 
                 data = barrios_gather %>% filter(Barrio != 'zona sur', topico == 'CBE', IIBPCD > 0.08)) +
    scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                         name="",
                         na.value = "grey100", 
                         values =  scales::rescale(seq(0, 20, length.out = 6))) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.1, "in"), 
                           pad_y = unit(0.25, "in"),
                           height = unit(.8, "cm"), 
                           width = unit(.8, "cm"),
                           style = north_arrow_fancy_orienteering) +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size = 8, color = 'black'),
          axis.text.y = element_text(angle = -90, hjust = .5, size = 8, color = 'black'),
          plot.title = element_text(hjust = .5),
          plot.caption = element_text(size = 7, color = 'black'),
          legend.position = c(0.9, 0.13),
          legend.key.size = unit(.1, 'cm'),
          legend.key.height = unit(.5, 'cm'),
          legend.key.width = unit(.4, 'cm'), 
          legend.title = element_text(size=11),
          legend.text = element_text(size=8)))
# tabla top 20
tabla_ibpcd_20 <- st_set_geometry(barrios_gather, NULL) %>% filter(Barrio != 'zona sur', topico == 'CBE') %>% 
  mutate(Barrio = case_when(Barrio == 'AREA CENTRO' ~ 'CENTRO',
                            Barrio == 'DE LAS HERASJUAN GREGORIO' ~ 'LAS HERAS',
                            Barrio == 'SANTA ROSA DEL MAR DE PERALTA RAMOS' ~ 'SANTA ROSA',
                            Barrio == 'NUEVE DE JULIO' ~ '9 DE JULIO',
                            Barrio == 'DE LA PLAZA FORTUNATO' ~ 'FORTUNATO DE LA PLAZA',
                            Barrio == 'AMEGHINO FLORENTINO' ~ 'FLORENTINO AMEGHINO',
                            Barrio == 'SAN EDUARDO DE CHAPADMALAL' ~ 'SAN EDUARDO',
                            TRUE ~ as.character(Barrio))) %>% 
  select(ID, Barrio, IIBPCD) %>% arrange(desc(IIBPCD)) %>% .[1:20,] %>% 
  mutate(order = 20:1)
# geom_bar top 20 
(viz_bar_top_20_3 <- ggplot(aes(x=order, y=IIBPCD, fill=IIBPCD), data = tabla_ibpcd_20) +
    geom_bar(aes(x=order, y=IIBPCD), stat = 'identity') +
    geom_text(aes(label=Barrio),color = 'black', hjust = 0, size = 2.23, nudge_y = -0.085) +
    scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu"))) +
    scale_x_continuous(breaks = seq(1, 20, by=1),
                       label = c('87','34','16','68','52','31','20','5','43','35',
                                 '77','45','21','8','57','22','65','38','48','26'), # paste(tabla_ibpcd_20 %>% arrange(order) %>% select(ID) %>% as_vector() %>% as.vector() %>% as.character(), collapse = "','")
                       expand=c(0.01,0.01)) +
    scale_y_continuous(expand=c(0.01,0.01)) +
    coord_flip() +
    labs(x=NULL,y=NULL) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text.y = element_text(size = 8, color = 'black'),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 7, color = 'black'),
          axis.text.x = element_text(size = 7, color = 'black')
    ))
# 2 en 1
(mapa_3 <- ggarrange(mapa3, viz_bar_top_20_3, widths = c(2,1.5), ncol = 2, nrow = 1))
png('./viz/mapa_3.png', width = 900, height = 900, res = 100)
annotate_figure(mapa_3,
                top = text_grob("IIBPC: Índice de Intensidad Barrial de Problemáticas relativas a los CBE [Mar del Plata, 2020-2021]", 
                                color = "grey30", size = 12),
                bottom = text_grob(expression(paste("Fuente: ",italic("La Capital"))), 
                                   color = "grey40", size = 9, hjust = -3))
dev.off()
#mapa4
(mapa4 <- ggplot(barrios_gather %>% filter(topico == 'Fiestas')) +
    geom_sf(aes(fill = IIBPCD)) +
    geom_sf_text(aes(label=ID), fontface = "bold", color = 'black', size = 2.5, 
                 data = barrios_gather %>% filter(Barrio != 'zona sur', topico == 'Fiestas', IIBPCD > 0.02)) +
    scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                         name="",
                         na.value = "grey100", 
                         values =  scales::rescale(seq(0, 20, length.out = 6))) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.1, "in"), 
                           pad_y = unit(0.25, "in"),
                           height = unit(.8, "cm"), 
                           width = unit(.8, "cm"),
                           style = north_arrow_fancy_orienteering) +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size = 8, color = 'black'),
          axis.text.y = element_text(angle = -90, hjust = .5, size = 8, color = 'black'),
          plot.title = element_text(hjust = .5),
          plot.caption = element_text(size = 7, color = 'black'),
          legend.position = c(0.9, 0.13),
          legend.key.size = unit(.1, 'cm'),
          legend.key.height = unit(.5, 'cm'),
          legend.key.width = unit(.4, 'cm'), 
          legend.title = element_text(size=11),
          legend.text = element_text(size=8)))
# tabla top 20
tabla_ibpcd_20_2 <- st_set_geometry(barrios_gather, NULL) %>% filter(Barrio != 'zona sur', topico == 'Fiestas') %>% 
  mutate(Barrio = case_when(Barrio == 'AREA CENTRO' ~ 'CENTRO',
                            Barrio == 'DE LAS HERASJUAN GREGORIO' ~ 'LAS HERAS',
                            Barrio == 'SANTA ROSA DEL MAR DE PERALTA RAMOS' ~ 'SANTA ROSA',
                            Barrio == 'NUEVE DE JULIO' ~ '9 DE JULIO',
                            Barrio == 'DE LA PLAZA FORTUNATO' ~ 'FORTUNATO DE LA PLAZA',
                            Barrio == 'AMEGHINO FLORENTINO' ~ 'FLORENTINO AMEGHINO',
                            Barrio == 'SAN EDUARDO DE CHAPADMALAL' ~ 'SAN EDUARDO', 
                            Barrio == 'PARQUE MONTEMAR-EL GROSELLAR' ~ 'EL GROSELLAR',
                            Barrio == 'ESTRADA JOSE MANUEL' ~ 'ESTRADA',
                            Barrio == 'ZACAGNINI JOSE MANUEL' ~ 'ZACAGNINI', 
                            TRUE ~ as.character(Barrio))) %>% 
  select(ID, Barrio, IIBPCD) %>% arrange(desc(IIBPCD)) %>% .[1:20,] %>% 
  mutate(order = 20:1)
# geom_bar top 20 
(viz_bar_top_20_4 <- ggplot(aes(x=order, y=IIBPCD, fill=IIBPCD), data = tabla_ibpcd_20_2) +
    geom_bar(aes(x=order, y=IIBPCD), stat = 'identity') +
    geom_text(aes(label=Barrio),color = 'black', hjust = 0, size = 2.23, nudge_y = 0.01,
              data = tabla_ibpcd_20_2 %>% filter(ID != 10)) +
    geom_text(aes(label=Barrio),color = 'white', hjust = 0, size = 2.23, nudge_y = -0.06,
              data = tabla_ibpcd_20_2 %>% filter(ID == 10)) +
    scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu"))) +
    scale_x_continuous(breaks = seq(1, 20, by=1),
                       label = c('1','94','93','63','89','2','45','118','81','14','125',
                                 '24','9','98','17', '13','67','128','129','10'), # tabla_ibpcd_20_2 %>% arrange(order) %>% select(ID) %>% as_vector() %>% as.vector()
                       expand=c(0.01,0.01)) +
    scale_y_continuous(expand=c(0.01,0.01)) +
    coord_flip() +
    labs(x=NULL,y=NULL) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text.y = element_text(size = 8, color = 'black'),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 7, color = 'black'),
          axis.text.x = element_text(size = 7, color = 'black')
    ))
# 2 en 1
(mapa_4 <- ggarrange(mapa4, viz_bar_top_20_4, widths = c(2,1.5), ncol = 2, nrow = 1))
png('./viz/mapa_4.png', width = 900, height = 900, res = 100)
annotate_figure(mapa_4,
                top = text_grob("IIBPC: Índice de Intensidad Barrial de Problemáticas relativas a las fiestas clandestinas [Mar del Plata, 2020-2021]", 
                                color = "grey30", size = 12),
                bottom = text_grob(expression(paste("Fuente: ",italic("La Capital"))), 
                                   color = "grey40", size = 9, hjust = -3))
dev.off()

