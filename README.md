# Proyecto Final Bedu

## Este proyecto se enfoca en analizar la distribución de la carencia en recursos básicos y servicios dentro de México a nivel localidad, enfocandose principalmente en la división entre diferentes grupos indígenas. 

## Igualmente, se pretende comparar el cambio al acceso de estos servicios del 2010 al 2020. 

## Finalmente, se creó un algoritmo que categoriza las localidades más vulnerables para la creación de una estrategia para invertir recursos a los que más lo necesiten. 

### Agregamos los datos del 2010 (en R):

```R

install.packages(ggplot2)

library("ggplot2")

# importamos los diferentes df: 

censo_general <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Censo general.csv"))
geo_datos <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Datos geográficos.csv"))
viviendas <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Descripción de viviendas.csv"))
lenguas_ind <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Hablantes de lenguas indígenas.csv"))
rezago <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Rezago social simplific 2010.csv"))
servicios <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Servicios basicos de vivendas.csv"))

```
### Creamos una función para sacar el porcentaje de casas sin ciertos servicios, ya que sòlo indica el número de casas sin servicio por localidad para los datos del 2010 (en R):

```R

porcentaje <- function(DF,col_sin,col_con,round,nom_col){
  DF <- mutate(DF, col1 = round(
    (col_sin/(col_sin+col_con))*100, as.numeric(round)
  ))
  names(DF)[(length(DF))] <- nom_col
  return(DF)
}

# porcentae de casas sin agua:

servicios <- porcentaje(servicios, vph_aguafv, vph_aguadv, 2, "casasSinAgua_x100")

# porcentae de casas sin electricidad: 

servicios <- porcentaje(servicios, vph_s_elec, vph_c_elec, 2, "casasSinLuz_x100")

# porcentae de casas sin drenaje: 

servicios <- porcentaje(servicios, vph_nodren, vph_drenaj, 2, "casasSinDrenaje_x100")

# porcentaje de casas con piso de tierra:

attach(viviendas)

viviendas <- porcentaje(viviendas,viv_ocup_habit_piso_tierra,viv_ocup_habit_piso_no_tierra,2,"casasPisoTierra_x100")

# Ahora vamos a crear el porcentaje de hablantes de legua indígena por localidad a travès de una función par que nos de el porcentaje de hablantes de leguans indigenas:

porcentaje2 <- function(DF,habl,pop_tot,round,nom_col){
  DF <- mutate(DF, col1 = round(
    (habl/pop_tot)*100, as.numeric(round)
  ))
  names(DF)[(length(DF))] <- nom_col
  return(DF)
}

# porcentaje de hablantes de lenguas indígenas:

lenguas_ind_censo_DF <- porcentaje2(lenguas_ind_censo_DF, Hablantes_leng_indig, pobtot, 2, "HablantesInd_x100")

# porcentaje de hablantes de lenguas indígenas y no español:

lenguas_ind_censo_DF <- porcentaje2(lenguas_ind_censo_DF, Hablantes_leng_indig_no_esp, pobtot, 2, "HablantesNoEsp_x100")

# porcentaje de hablantes de lenguas indígenas y español:

lenguas_ind_censo_DF <- porcentaje2(lenguas_ind_censo_DF, Hablantes_leng_indig_y_esp, pobtot, 2, "HablantesBiLing_x100")


````

### Después limpiamos las Dataframes con las columnas necesarias y las juntamos en una sola columna final (en R): 
#### *Los ID de estas dataframes son una concatenación de el id de entidad, que continene 2 dígitos, el id de municipio que contiene 3 dígitos y el id de localidad que contiene 4 dígitos. estas columnas ya estaban con este formato, más adelante se muestra la función para crear este ID compuesto para los datos del 2020*

````R
# Borramos las columnas que no vamos a usar de todas los DF:

censo_general_selec <- censo_general[,-(3:11),drop=FALSE] # para eliminar una serie de columnas y guardar en otro DF 
viviendas_selec <- viviendas[,-(c(3:5,7:9,11,12)),drop=FALSE]
lenguas_ind_censo_select <- lenguas_ind_censo_DF[,c(1,15,16,17)]
servicios_selec <- servicios[,c(1,11:13)]

# Ahora juntamos los datos seleccionados en un solo df usando el ID 

geo_censo <- merge.data.frame(geo_datos,censo_general_selec,by="ID_.EST.MUN.LOC.") 
geo_cen_viv <- merge.data.frame(geo_censo, viviendas_selec,by="ID_.EST.MUN.LOC.")
geo_cen_viv_ind <- merge.data.frame(geo_cen_viv,lenguas_ind_censo_select,by="ID_.EST.MUN.LOC.")
geo_cen_viv_ind_rez <- merge.data.frame(geo_cen_viv_ind,rezago,by="ID_.EST.MUN.LOC.")
DF_Final <- merge.data.frame(geo_cen_viv_ind_rez,servicios_selec,by="ID_.EST.MUN.LOC.")

# Guardamos los resultados:

write.csv(DF_Final, "/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /DF_Final.csv", row.names = TRUE)

`````

### Ahora, arreglamos los nombres de las columnas creadas en R (Python): 

```Python
import pandas as pd
import numpy as np

# importamos los datos creados en R:

df_2010 = pd.read_csv('DF_Final.csv')
df_2010.dtypes

# creamos nombres con estandar PEP 8:

nombres_nuevos = {
    'ID_.EST.MUN.LOC.':'id_est_mun_loc',
    'longitud': 'longitud_decimal',
    'latitud': 'latitud_decimal',
    'altitud': 'altitud_2010',
    'pobtot': 'poblacion_total_2010',
    'Viv_tot': 'vivendas_totales_2010',
    'Tot_viv_part_hab': 'total_de_vivendas_particulares_habitadas_2010',
    'Prom_ocup_por_cuarto': 'promedio_de_ocupantes_por_cuarto_2010',
    'casasPisoTierra_x100': 'porcentaje_de_casas_con_piso_de_tierra_2010',
    'HablantesInd_x100': 'porcentaje_de_hablantes_indigenas_2010',
    'HablantesNoEsp_x100': 'porcentaje_de_hablantes_indigenas_que_no_hablan_español_2010',
    'HablantesBiLing_x100': 'porcentaje_de_hablantes_bilingues_2010',
    'Indice.de.rezago.social': 'indice_de_rezago_social_2010',
    'Grado.de.rezago.social': 'grado_de_rezago_social_2010',
    'Lugar.nacional': 'lugar_nacional_de_rezago_social_2010',
    'casasSinAgua_x100': 'porcentaje_de_casas_sin_agua_2010',
    'casasSinLuz_x100': 'porcentaje_de_casas_sin_luz_2010',
    'casasSinDrenaje_x100': 'porcentaje_de_casas_sin_drenaje_2010',
}

df_2010 = df_2010.rename(columns=nombres_nuevos)

df_2010['id_est_mun_loc'] = df_2010['id_est_mun_loc'].astype(str)
df_2010['id_est_mun_loc'] = df_2010.id_est_mun_loc.apply('{:0>9}'.format)

df_2010 = df_2010.set_index('id_est_mun_loc', drop= True)
df_2010.dtypes

df_2010.to_csv('datos_2010_arreglados.csv')

````

### Importamos los datos del 2020 ya que fueron publicados a la mitad del curso (Python):

````Python

df_2020 = pd.read_csv('conjunto_de_datos_iter_00CSV20.csv')

# usando los ID de estado, municipio y localdidad, hacemos el ID de casa localidad tal y como en R: 

df_2020['ENTIDAD'] = df_2020.ENTIDAD.apply('{:0>2}'.format)
df_2020['MUN'] = df_2020['MUN'].apply('{:0>3}'.format)
df_2020['LOC'] = df_2020['LOC'].apply('{:0>4}'.format)

df_2020['id_est_mun_loc'] = df_2020['ENTIDAD'].map(str) + df_2020['MUN'].map(str) + df_2020['LOC'].map(str)

# el ID lo ponemos como index: 

df_2020 = df_2020.set_index('id_est_mun_loc', drop=True)

# Limpiamops los datos que contiene valores de resumen general y no datos de localidad: 

df_2020_sin_resumen = df_2020[~df_2020['ENTIDAD'].str.contains('00')] 
df_2020_sin_resumen = df_2020_sin_resumen[~df_2020['MUN'].str.contains('000')]
df_2020_sin_resumen = df_2020_sin_resumen[~df_2020['LOC'].str.contains('0000')]
df_2020_sin_resumen = df_2020_sin_resumen[~df_2020['LOC'].str.contains('9999')]
df_2020_sin_resumen = df_2020_sin_resumen[~df_2020['LOC'].str.contains('9998')]

# seleccionamos sólamente las columnas necesarias y quitamos los * que se usaron como null en los datos vírgenes

df_2020_sin_resumen_select = df_2020_sin_resumen[['LONGITUD','LATITUD','ALTITUD','POBTOT',
                                                  'VIVTOT','VIVPAR_HAB','PRO_OCUP_C','P3YM_HLI',
                                                  'P3HLINHE', 'P3HLI_HE','VPH_PISODT','VPH_PISOTI',
                                                  'VPH_C_ELEC','VPH_S_ELEC','VPH_AGUAFV','VPH_DRENAJ',
                                                  'VPH_NODREN']]

df_2020_sin_resumen_select = df_2020_sin_resumen_select.replace('*', np.nan)

df_2020_sin_resumen_select.isna().sum()

df_2020_sin_resumen_select_sin_null = df_2020_sin_resumen_select.dropna(axis=0, how= 'any')

df_2020_sin_resumen_select_sin_null['ALTITUD'] = df_2020_sin_resumen_select_sin_null['ALTITUD'].replace('00-1', '0')
(df_2020_sin_resumen_select_sin_null['ALTITUD'] == '00-1').value_counts()

`````
### pasamos los datos a numeric (Python): 

````Python
df_2020_sin_resumen_select_sin_null_float = df_2020_sin_resumen_select_sin_null
lista_numeric = ['ALTITUD',
                 'VIVPAR_HAB',
                 'PRO_OCUP_C',
                 'P3YM_HLI',
                 'P3HLINHE', 
                 'P3HLI_HE',
                 'VPH_PISODT',
                 'VPH_PISOTI',
                 'VPH_C_ELEC',
                 'VPH_S_ELEC',
                 'VPH_AGUAFV',
                 'VPH_DRENAJ',
                 'VPH_NODREN']

for i in range(0,(len(lista_numeric))): 
  df_2020_sin_resumen_select_sin_null_float[lista_numeric[i]] = pd.to_numeric(df_2020_sin_resumen_select_sin_null_float[lista_numeric[i]],errors='coerce')

df_2020_sin_resumen_select_sin_null_float.dtypes  

````


### Similarmente creamos una función para obtener el porcentaje de casas sin servicio para 2020 en Python (en Python):

````Python

df = df_2020_sin_resumen_select_sin_null_float

def hacer_porcentajes(sin,con,df,nombre):
  total = sin + con
  porcentaje = (sin/total)*100
  df[nombre] = porcentaje
  return pd.DataFrame(df[nombre])


# porcentaje de casas sin agua:

hacer_porcentajes(df['VPH_NODREN'], df['VPH_DRENAJ'], df, 'porcentaje_de_casas_sin_drenaje_2020')

# porcentaje de casas sin luz:

hacer_porcentajes(df['VPH_S_ELEC'], df['VPH_C_ELEC'], df, 'porcentaje_de_casas_sin_luz_2020')

# porcentaje de casas con piso de tierra:
 
hacer_porcentajes(df['VPH_PISOTI'], df['VPH_PISODT'], df, 'porcentaje_de_casas_con_piso_de_tierra_2020')

def hacer_porcentajes_simplificado(fraccion,total,df,nombre):
  porcentaje = (fraccion/total)*100
  df[nombre] = porcentaje
  return pd.DataFrame(df[nombre])

# porcentaje de casas sin agua:

hacer_porcentajes(df['VPH_AGUAFV'], df['VIVPAR_HAB'], df, 'porcentaje_de_casas_sin_agua_2020')

# porcentaje de casas s:

hacer_porcentajes_simplificado(df['P3YM_HLI'],df['POBTOT'],df,'porcentaje_de_hablantes_indigena_2020')

# porcentaje de casas sin agua:

hacer_porcentajes_simplificado(df['P3HLINHE'],df['POBTOT'],df,'porcentaje_de_hablantes_indigena_y_no_esp_2020')

# porcentaje de casas sin agua:

hacer_porcentajes_simplificado(df['P3HLI_HE'],df['POBTOT'],df,'porcentaje_de_hablantes_bilingues_2020')

````
### Ahora Arreglamos los nombres faltantes de las columnas para la tabla del 2020 (Python):

````Python

df_2020_final = df[['LONGITUD',
                    'LATITUD',
                    'ALTITUD',
                    'POBTOT',
                    'VIVTOT',
                    'VIVPAR_HAB',
                    'PRO_OCUP_C',
                    'porcentaje_de_casas_sin_drenaje_2020',
                    'porcentaje_de_casas_sin_luz_2020',
                    'porcentaje_de_casas_con_piso_de_tierra_2020',
                    'porcentaje_de_casas_sin_agua_2020',
                    'porcentaje_de_hablantes_indigena_2020',
                    'porcentaje_de_hablantes_indigena_y_no_esp_2020',
                    'porcentaje_de_hablantes_bilingues_2020'
  ]]


nombres_nuevos_2020 = {
    'LONGITUD': 'Longitud_grados',
    'LATITUD': 'latitud_grados',
    'ALTITUD': 'altitud_2020',
    'POBTOT': 'poblacion_total_2020',
    'VIVTOT': 'viviendas_totales_2020',
    'VIVPAR_HAB': 'total_de_viviendas_particulares_habitadas_2020',
    'PRO_OCUP_C': 'promedio_de_ocupantes_por_cuarto_2020',
}

df_2020_final = df_2020_final.rename(columns=nombres_nuevos_2020)

`````
### Agregamos los datos de rezago social del 2020

````Python

rezago_2020 = pd.read_csv('rezago_2020.csv')

rezago_2020.index = rezago_2020['Clave localidad'].astype(str)
rezago_2020.index = rezago_2020['Clave localidad'].apply('{:0>9}'.format)

rezago_2020 = rezago_2020.drop(columns=['Clave localidad'])

nombres_rezago_2020 = {
    'Índice de rezago social': 'indice_de_rezago_social_2020',
    'Grado de rezago social': 'grado_de_rezago_social_2020',
    'Lugar que ocupa en el contexto nacional': 'lugar_nacional_de_rezago_social_2020',
}
rezago_2020 = rezago_2020.rename(columns=nombres_rezago_2020)

````

### Ahora juntamos las tablas del 2010 y 2020 usando el id de localidad y agregamos las regiones indígenas

````Python

df_2010_2020 = pd.merge(df_2010, df_2020_final, left_index=True, right_index=True)

region_ind = pd.read_csv('regiones índígenas.csv')

# renombramos la columna del los id y regiín indígena:

region_ind.id_est_mun_loc.names = ['id_est_mun_loc']
region_ind = region_ind.rename({'REG_IND': 'region_indigena'}, axis=1)

# formateamos los ID y los ponemos como index

region_ind['id_est_mun_loc'] = region_ind['id_est_mun_loc'].apply('{:0>9}'.format)
region_ind = region_ind.set_index('id_est_mun_loc')

agredamos laa regiones indíegnas a nuesrta Df:

df_2010_2020_reg = pd.merge(df_2010_2020, region_ind, left_index=True, right_index=True, how='left')

# Los registros que se quedaron nulos, los llenamos con 'Localidades no indigenas' ya que no pertencecen a ninguna región indígena:

df_2010_2020_reg['region_indigena'] = df_2010_2020_reg['region_indigena'].fillna('Localidades no indigenas')

df_2010_2020_reg.to_csv(df_2010_2020_reg.csv)

````
### Listo! Ahora podemos empezar a hacer comparaciones estadísticas gráficas y buscar patrones en nuestra DF

### Primero veamos las distribnuciones del acceso ciertos servicios en 2010 por región indígena (R)

`````R
df_2010_2020_reg <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /df_2010_2020_reg.csv"))

attach(df_2010_2020_reg)

Hist_sin_Rezago <- ggplot(Data.reg, aes(x = reorder(region_indigena, indice_de_rezago_social_2010, fun = mean), 
                     y = indice_de_rezago_social_2010)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Rezago social por región indígena en 2010") +
  xlab("Región Indígena") +
  ylab("Dispersión de rezago social") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

# Acceso al agua por región indígena 

Hist_sin_Agua <- ggplot(Data.reg, aes(x = reorder(region_indigena, porcentaje_de_casas_sin_agua_2010, fun = mean), 
                     y = porcentaje_de_casas_sin_agua_2010)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Acceso al agua por región indígena en 2010") +
  xlab("Región Indígena") +
  ylab("Porcentaje de viviendas por localidad \nsin acceso al agua entubada") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

# Acceso a luz eléctrica por región indígena 

Hist_sin_Luz <- ggplot(Data.reg, aes(x = reorder(region_indigena, porcentaje_de_casas_sin_luz_2010, fun = mean), 
                     y = porcentaje_de_casas_sin_luz_2010)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Acceso a luz eléctrica por\nregión indígena en 2010") +
  xlab("Región Indígena") +
  ylab("Porcentaje de viviendas por localidad \nsin acceso a luz eléctrica") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

# viviendas con piso de tierra por región indígena: 

Hist_piso_tierra <- ggplot(Data.reg, aes(x = reorder(region_indigena, porcentaje_de_casas_con_piso_de_tierra_2010, fun = mean), 
                                     y = porcentaje_de_casas_con_piso_de_tierra_2010)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Viviendas con piso de tierra por\nregión indígena en 2010") +
  xlab("Región Indígena") +
  ylab("Porcentaje de viviendas por \n localidad con piso de tierra") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

Hist_sin_Agua
Hist_sin_Luz
Hist_sin_Rezago
Hist_piso_tierra
`````

*Podemos ver que las tegiones tarahuamras y garn nayar tienen desproporcionadamente menos acceso a agua y al drenaje, al igual que mayor rezago social y casas con piso de tierra*

### Ahora veamos la distribución del índice de rezago social del 2010 al 2020 (Python):

````Python
sns.set(style="darkgrid")
sns.distplot(df_2010_2020_reg['indice_de_rezago_social_2020'],  kde=True, label='2020')

sns.distplot(df_2010_2020_reg['indice_de_rezago_social_2010'], 
             kde=True,label='2010')

# Agregamos titulos y estilo a nuestra gráfica:

plt.legend(prop={'size': 12});
plt.title('Distribución del Índice de Rezago Social del 2010 al 2020',
          size=26, family='monospace', weight=900, y=1.1)
plt.xlabel('Índice de Rezago Social', size=16, family='monospace')
plt.ylabel('Densidad', size=16, family='monospace')

````
*Podemos ver que aumentó la cantidad de viviendas en un grado de rezago medio, al igual que aumntó la cola superior (mas localidades con menos srevicios, y decreció la cola inferior, o se redujeron el número de localidades con muy poco rezago. Esto indica que ¨empeoró¨ el rezago social en méxico*

### Veamos la diferencia del rezago social del 2010 al 2020 por región indígena:

````Python

# Primero aggarramos el promedio del índice de rezago social de ambos años y lo agrupamos por región indígena:

rez_ind= df_2010_2020_reg.groupby('region_indigena')[['indice_de_rezago_social_2010', 'indice_de_rezago_social_2020']].mean()

# hacemos una ecuación sencilla para obtener la diferencia entre ambos años:

rez_ind['diferencia_rezago'] = (rez_ind['indice_de_rezago_social_2010'] - rez_ind['indice_de_rezago_social_2020']) * -1

# ordenamos de menor a mayor:

rez_ind = rez_ind.sort_values(by=['diferencia_rezago'])

# finalmente graficamos en un histgrama la diferencia por región: 

sns.figsize=(10,14)
sns.axes_style("white")
dif = sns.barplot(x=rez_ind.index, y=rez_ind['diferencia_rezago'], palette="vlag")

dif.set_xticklabels(dif.get_xticklabels(),rotation = 90, family='monospace', weight=900);

plt.xlabel('Región Indígena', size=16, family='monospace')
plt.ylabel('Índice de Rezago', size=16, family='monospace')
plt.title('Cambio en el Índice de Rezago del 2010 al 2020\n por Región Indígena', 
          size=26, family='monospace', weight=900, y=1.1)
````

*aguí podemos ver que hubo más diferencia positiva (refiriendise a que aumentó) que negativo (decreció el rezago). Las mas afectadas son Tarahumara y Gran nayar, mientras que las localidades no indígenas tuvieron un aumento muy pequeño. Ottras regiones como LoscNáhuas de Veracrúz y Valles Centrales, decreció significativamente el rezago.*

### Ahora, esta métrica de la diferencia de rezago del 2010 al 2020 nos va a servir más adelante para icluir la diferncia para seleccionar las localidades más vulnerables, así que se la aplicamos a todos nuestros registros: 

````Python 

df_2010_2020_reg['diferencia_rezago'] = (df_2010_2020_reg['indice_de_rezago_social_2010'] - df_2010_2020_reg['indice_de_rezago_social_2020']) * -1

````

### Ahora vemos el número de personas nuevas en rezago muy alto y alto: 

````Python 
rezago_muy_alto_2010 = df_2010_2020_reg[df_2010_2020_reg['grado_de_rezago_social_2010'] == 'Muy alto']
rezago_muy_alto_2020 = df_2010_2020_reg[df_2010_2020_reg['grado_de_rezago_social_2020'] == 'Muy alto']

pob_rezago_muy_alto_2010 = rezago_muy_alto_2010['poblacion_total_2010'].sum()
pob_rezago_muy_alto_2020 = rezago_muy_alto_2020['poblacion_total_2020'].sum()

rezago_alto_2010 = df_2010_2020_reg[df_2010_2020_reg['grado_de_rezago_social_2010'] == 'Alto']
rezago_alto_2020 = df_2010_2020_reg[df_2010_2020_reg['grado_de_rezago_social_2020'] == 'Alto']

pob_rezago_alto_2010 = rezago_alto_2010['poblacion_total_2010'].sum()
pob_rezago_alto_2020 = rezago_alto_2020['poblacion_total_2020'].sum()

porcentaje_de_cambio_muy_alto = round(((pob_rezago_muy_alto_2020/pob_rezago_muy_alto_2010)*100)-100,1)
porcentaje_de_cambio_alto = round(((pob_rezago_alto_2020/pob_rezago_alto_2010)*100)-100,1)

print(f'''La población en grado de rezago social muy alto en 2010 era {pob_rezago_muy_alto_2010} y 
aumentó en un {porcentaje_de_cambio_muy_alto}% en 2020, con {pob_rezago_muy_alto_2020} en esta categoría con \n''')

print(f'''Similarmente, la población en grado de rezago social alto en 2010 era {pob_rezago_alto_2010} y 
aumentó en un {porcentaje_de_cambio_alto}% en 2020, con {pob_rezago_alto_2020} en esta categoría''')
````
> La población en grado de rezago social muy alto en 2010 era 140476 y 
aumentó en un 158.4% en 2020, con 362962 en esta categoría

> Similarmente, la población en grado de rezago social alto en 2010 era 1721015 y 
aumentó en un 115.3% en 2020, con 3705725 en esta categoría

### Ahora veamos la distribución de hablan tes de lenguas indígenas por nivel de rezago social: 

````Python

ax = sns.set_style('darkgrid')
ax = sns.scatterplot(df_2010_2020_reg['porcentaje_de_hablantes_indigena_2020'], 
                     df_2010_2020_reg['indice_de_rezago_social_2020'], 
                     hue=df_2010_2020_reg['porcentaje_de_hablantes_indigena_y_no_esp_2020']);
ax.set(xlabel='porcentaje de hablantes indígenas por localidad')
ax.set(ylabel='índice de rezago social')
plt.legend(title='Porcentaje de personas\n que no hablan español',
           bbox_to_anchor=(1.05, 1), loc='upper left', borderaxespad=0)
````
*Esta gráfica indica que al incrementar el número de hablantes de alguna lengua indígena, aumenta el porcentaje de gente que no habla españo, al igual que incrementa el número de personas que no habla español al incrementar el nivel de rezago social. Similarmente, ay más localidades con 80% o más de personas que habla lengua indígena en niveles muy altos de rezago social*



### Ahora veamos que tan bien se correlacionan las vrriables del 2020 entre ellas para usarlas en la clasificación:

````Python

correlacion_2020 = df_2010_2020_reg[['altitud_2020','longitud_decimal','latitud_decimal',
                                     'promedio_de_ocupantes_por_cuarto_2020', 
                                     'porcentaje_de_casas_sin_drenaje_2020', 
                                     'porcentaje_de_casas_sin_luz_2020', 
                                     'porcentaje_de_casas_con_piso_de_tierra_2020', 
                                     'porcentaje_de_casas_sin_agua_2020', 
                                     'porcentaje_de_hablantes_indigena_2020', 
                                     'porcentaje_de_hablantes_indigena_y_no_esp_2020',
                                     'porcentaje_de_hablantes_bilingues_2020', 
                                     'region_indigena', 'diferencia_rezago']]


plt.figure(figsize=(12, 12))
corr_matriz_todos = sns.heatmap(correlacion_2020.corr(), annot=True, cmap="coolwarm",  vmin= -1, vmax=1);
plt.xlabel('Región Indígena', size=16, family='monospace')
plt.ylabel('Índice de Rezago', size=16, family='monospace')
plt.title('Correlación entre variables numéricas del 2020', 
          size=26, family='monospace', weight=900, y=1.1);
          
`````
*podemos ver que todas las variables excepto las geográficas (altitud, longitud y latitud) tienen cierta correlacion linel entre ellas*

## Parte 2: Seleccionar las comunidades más vulnerables

- Ahora que ya tenemos un entendimiento de los grupos más vulnerables, podemos hacer las preparaciones para seleccionar las comunidades más vulnerables

### Seleccionamos a los grupos vulnerables basado en las dos comunidades con mayor falta de servicios, Los Tarahumara y la región del Gran Nayar:

````Python 

df_tarahumara = df_2010_2020_reg[df_2010_2020_reg.region_indigena == 'Tarahumara']
df_gran_nayar = df_2010_2020_reg[df_2010_2020_reg.region_indigena == 'Huicot o Gran Nayar']
df_no_indigena = df_2010_2020_reg[df_2010_2020_reg.region_indigena == 'Localidades no indigenas']

# Le asignamos un 1 a las co,unidades vulnerables y un 0 al resto:

df_tarahumara['comunidad_vulnerable'] = 1
df_gran_nayar['comunidad_vulnerable'] = 1
df_no_indigena['comunidad_vulnerable'] = 0

# las juntamos en una sola DF 

comunidad_vul = pd.concat([df_tarahumara, df_no_indigena, df_gran_nayar])

# checamos la cantidad de localidades dentro de cada grupo: 

comunidad_vul.groupby('comunidad_vulnerable')['comunidad_vulnerable'].value_counts()

`````
output: 

comunidad_vulnerable | comunidad_vulnerable
------------ | -------------
0 | 64894
1 | 2741

*aquí podemos ver que las comunidades no vulnerables estan sobre representadas, casí 30 registros más que las vulnerables. En general esto haría que nuestro algoritmo hsesge los resultados, para nuestro caso, que sólo queremos encontrar las comunidades más vulnerables, nos conviene, así sólo selecciona lo más extremo.*

### AHora, hagamos una serie de boxplots con varias variables comparando los dos grupos seleccionados, previendo lo que 'vería' el algoritmo para tomar una decisión de si es vulnerable o no. Igual no ayudará a ver si las variables tienen una diferencia estadística suficiente

````Python

import matplotlib.pyplot as plt

sns.set(rc={'figure.figsize':(15,15)})
sns.set_style("darkgrid")
#sns.
fig, ax =plt.subplots(3,2, constrained_layout=True)



sns.boxenplot(x='region_indigena', y='indice_de_rezago_social_2020', data=comunidad_vul, ax = ax[0,0])
ax[0, 0].set(xlabel='', ylabel='índice de Rezago\nSocial 2020', 
             title='Distribución del Índice de\nRezago Social')

sns.boxenplot(x='region_indigena', y='porcentaje_de_casas_sin_drenaje_2020', data=comunidad_vul, ax = ax[0,1])
ax[0, 1].set(xlabel='', ylabel='Porcentaje de\ncasas sin drenaje', 
             title='Distribución de Viviendas\nSin Acceso al Drenaje')

sns.boxenplot(x='region_indigena', y='porcentaje_de_casas_con_piso_de_tierra_2020', data=comunidad_vul, ax=ax[1,0])
ax[1, 0].set(xlabel='', ylabel='Porcentaje de casas con\npiso de tierra', 
             title='Distribución de Viviendas\nSin Piso de Tierra')

sns.boxenplot(x='region_indigena', y='porcentaje_de_casas_sin_agua_2020', data=comunidad_vul, ax=ax[1,1])
ax[1, 1].set(xlabel='', ylabel='Porcentaje de\ncasas sin agua', 
             title='Distribución de Viviendas\nSin Acceso al Agua Potable')

sns.boxenplot(x='region_indigena', y='porcentaje_de_casas_sin_luz_2020', data=comunidad_vul, ax=ax[2,0])
ax[2, 0].set(xlabel='', ylabel='Porcentaje de\ncasas sin luz', 
             title='Distribución de Viviendas\nSin Acceso a la Luz')

sns.boxenplot(x='region_indigena', y='promedio_de_ocupantes_por_cuarto_2020', data=comunidad_vul, ax=ax[2,1])
ax[2, 1].set(xlabel='', ylabel='Promedio de ocupantes\n por cuarto', 
             title='Distribución de Ocupantes\npor Cuarto por Vivienda')

fig.show()

````
*Se puede apreciar que en todas las variables que vamos a usar tienen una distribución distinta, aunque menos marcada en el promedio de ocupantes por cuarto y la diferencia de rezago.*

### ahora seleccionemos nuestras variables para la predicción y hagamos una matríz de correlación con estos valores:

````Python
comunidad_vul_train = comunidad_vul[['promedio_de_ocupantes_por_cuarto_2020', 
                               'porcentaje_de_casas_sin_drenaje_2020', 
                               'porcentaje_de_casas_sin_luz_2020', 
                               'porcentaje_de_casas_con_piso_de_tierra_2020', 
                               'porcentaje_de_casas_sin_agua_2020', 
                               'porcentaje_de_hablantes_indigena_2020', 
                               'diferencia_rezago','comunidad_vulnerable']]
                               
# ahora checamos que todos sena valores numéricos

comunidad_vul_train.dtypes

`````
output:
`````
promedio_de_ocupantes_por_cuarto_2020          float64
porcentaje_de_casas_sin_drenaje_2020           float64
porcentaje_de_casas_sin_luz_2020               float64
porcentaje_de_casas_con_piso_de_tierra_2020    float64
porcentaje_de_casas_sin_agua_2020              float64
porcentaje_de_hablantes_indigena_2020          float64
diferencia_rezago                              float64
comunidad_vulnerable                             int64
dtype: object
`````
````Python
# hagamos la matríz de correlación una vez más

plt.figure(figsize=(12, 12))
sns.heatmap(comunidad_vul_train.corr(), annot=True, cmap= "vlag",  vmin= -0.7, vmax=0.7);

plt.title('Correlación entre variables numéricas del 2020\npara la predicción de localidades vulnerables', 
          size=26, family='monospace', weight=900, y=1.1);
````

### Bien, ahora separamos nuestra df en x y x, y la dividimos en entrenamiento (70%) y prueba (30%):

````Python
from sklearn.model_selection import train_test_split

x = comunidad_vul_train.drop(columns=['comunidad_vulnerable'])
y = comunidad_vul_train['comunidad_vulnerable']

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.30) 

`````

### Importamos la función de matríz de confusión de sklearn para evaluar nuestro modelo y creamos unas funciones para que nos de las métricas de precisión, sensibilidad y especificidad. 

````Python

from sklearn.metrics import confusion_matrix

def calcularAccuracy(TP, TN, FP, FN):
    accuracy = (TP + TN) / (TP + TN + FP + FN)
    accuracy = accuracy * 100
    return accuracy
def calcularSensibilidad(TP, TN, FP, FN):
    sensibilidad = TP / (TP + FN)
    sensibilidad = sensibilidad * 100
    return sensibilidad
def calcularEspecificidad(TP, TN, FP, FN):
    especificidad = TN / (TN + FP) 
    especificidad = especificidad * 100
    return especificidad

def evaluar(y_test, y_pred):
    resultado = confusion_matrix(y_test, y_pred)
    print(resultado)
    (TN, FP, FN, TP) = resultado.ravel()
    print("True positives: "+str(TP))
    print("True negatives: "+str(TN))
    print("False positives: "+str(FP))
    print("False negative: "+str(FN))

    acc = calcularAccuracy(TP, TN, FP, FN)
    sen = calcularSensibilidad(TP, TN, FP, FN)
    spec = calcularEspecificidad(TP, TN, FP, FN)
    print("Precision:"+str(acc)+"%")
    print("Sensibilidad:"+str(sen)+"%")
    print("Especificidad:"+str(spec)+"%")
````

