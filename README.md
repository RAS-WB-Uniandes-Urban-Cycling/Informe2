# proBikePolicies

En este repositorio se encuentran los scripts en `R` para el proyecto RAS entre WB y Uniandes para SDM.

Se propone la siguiente estructura según los ejes de trabajo:

  * Propensión
  * Seguridad
  * Modelo de Red
  * LTS
  * Indicadores
  * General
    
Se propone tener los datos en un servidor (por el momento la carpeta de OneDrive). La estructura de dicha información es:

  * UNIANDES - RAS - SDM: Root
    * RESULTADOS: La idea sería guardar acá todos los joins, cruces y resultados que hagamos como parte de nuestro trabajo
      * PROPENSION: Proyecto Pablo Uriza
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
      * SEGURIDAD: Proyecto Germán Carvajal
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
      * VOLUMENES: Proyecto Sebastian Cardona
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
      * LTS: Proyecto Alejandro Palacio y Marcelo Botero
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
      * MODELO DE RED: Proyecto de Jorge Huertas
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
      * GENERAL: Guardar acá todos aquellos cruces o construcciones de bases de datos que sepamos que nos van a servir para todos, ie: las ZATs por localidades de Bogotá, el total de viajes, etc.
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
  * BASES DE DATOS: La idea es almacenar aca todas las bases de datos que recibamos de parte de SDM, BM y cualquier otra que nosotros consigamos tal cual y la recibimos (original). La propuesta es nunca modificar las cosas sobre las mismas bases, sino construir el código de tal forma que lea los originales, los transforme como sea necesario y almacenar las cosas en la ubicación respectiva dentro de RESULTADOS. Con esto garantizamos que todos podamos acceder a la información de la forma como la recibimos y posteriormente que cuando le entreguemos algo a WB-SDM no vayamos a tener problemas de compatibilidad con el almacenamiento/estructura.
      * Documentation RAS-SDM: Esta carpeta es una joya!, tiene absolutamente de todo incluyendo los manuales para entender las bases de datos, por favor revísenla. Adicionalmente sería muy bueno terminar de desarrollar la tabla de Diana y Jorge sobre la descripción de las bases y luego almacenarla dentro de la carpeta de MANUALES (al parecer Targa ya había hecho un intento de esto, ver: 20171755_Insumos_Info_Ciclistas_RAS.xlsx.
          * 0. MANUALES BASES DE DATOS: Documentos y aclaraciones sobre los datos y las bases
          * 1. BIG DATA MOTIVACION: Información sobre la conferencia que dio pie al contrato RAS
          * 2. RAS: Muchos documentos legales y de correspondencia que explican qué busca la secretaria, qué es lo que nosotros debemos entregar y muchas otras aclaraciones que explican la situación y el objetivo del proyecto
          * 3. PLAN DE SEGURIDAD: Explicaciones de lo que ya tiene la SDM y lo que buscan explorar e implementar en materia de seguridad personal
          * 4. BICITAXIS: Explicaciones de lo que se quiere hacer con bicitaxis y documentos de estudios previos
          * 5. CENTRO DE LA BICI: El objetivo a largo plazo del proyecto, edificio de administración
          * 6. CICLOVIA DOMINICAL: Existen algunas bases de datos particulares del día domingo, acá se contiene toda la explicación de la logística de las ciclovías de los días festivos
          * 7. CONTRATO SEMAFOROS: Documentos sobre el contrato de semaforización
          * 8. BIBLIOGRAFIA: Incluir aca todos los documentos de referencia externos que estemos utilizando (papers, libros, etc.)
      * Total de 20 bases de datos con ls que contamos actualmente.
    
