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

````
### Listo! Ahora podemos empezar a hacer comparaciones estadísticas gráficas y buscar patrones en nuestra DF

### Primero veamos las distribnuciones del acceso ciertos servicios en 2010 por región indígena (R)

*Podemos ver que las tegiones tarahuamras y garn nayar tienen desproporcionadamente menos acceso a agua y al drenaje, al igual que mayor rezago social y casas con piso de tierra*

### Ahora veamos



