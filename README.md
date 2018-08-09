# proBikePolicies

En este repositorio se encuentran los scripts en `R` para el proyecto Industrial/SALURBAL sobre movilidad en biciletas para la ciudad de Bogotá.

Se propone la siguiente estructura según los ejes de trabajo:

  * Propensión: Pablo Uriza
  * Seguridad: German Carvajal
  * Modelo de Red: Jorge Huertas
  * LTS: Alejandro Palacio, Marcelo Botero
  * General
    
Se propone tener los datos de bases de datos de entrada y resultados en la carpeta de OneDrive. La estructura de dicha información es:

  * UNIANDES - RAS - SDM: Root
    * RESULTADOS: La idea sería guardar en esta ubicación todos los resultados que hagamos como parte de nuestro trabajo
      * PROPENSION: Proyecto Pablo Uriza
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
      * SEGURIDAD: Proyecto Germán Carvajal
        * Bases de datos: Resultados de análisis preliminares o de procesos intermedios en formato Rdata
        * Resultados: Almacenamiento de documentos finales o de preparación para presentación
          * TABLAS: Tablas de resultados finales
        * Resultados-Español: Gráficas y visualizaciones en español
        * Resultados-Inglés: Gráficas y visualizaciones en inglés
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
      * GENERAL: Guardar acá todos aquellas construcciones de bases de datos que puedan servir a todos, ie: las ZATs por localidades de Bogotá, el total de viajes, etc.
        * GRAFICOS: Para guardar imágenes y otros resultados de visualización
        * TABLAS: Para almacenar tablas no georeferenciadas
        * GEO-DATA: Almacenamiento de datos georeferenciados particulares al proyecto
        
  * BASES DE DATOS: La idea es almacenar en [esta ubicación](https://uniandes-my.sharepoint.com/:f:/g/personal/ga_carvajal10_uniandes_edu_co/Ev7vBy2Hv_xClnTY59R2wf8B1R0-xI4Rw1_3Kk22WJ_jPQ?e=WoSRkz) las bases de datos de cualquier origen en el formato en que se recibe (original). La propuesta es nunca modificar las cosas sobre las mismas bases, sino construir el código de tal forma que lea los originales, los transforme como sea necesario y almacenar las cosas en la ubicación respectiva dentro de RESULTADOS. Con esto garantizamos que todos podamos acceder a la información de la forma como la recibimos y posteriormente no existan problemas de compatibilidad, y adicionalmente que cualquier persona con acceso a la carpeta pueda ejecutar el código.
  
      * Documentation RAS-SDM: Esta carpeta es una joya!, tiene absolutamente de todo incluyendo los manuales para entender las bases de datos, por favor revísenla.
          * 0. MANUALES BASES DE DATOS: Documentos y aclaraciones sobre los datos y las bases
          * 1. BIG DATA MOTIVACION: Información sobre la conferencia que dio pie al contrato RAS
          * 2. RAS: Muchos documentos legales y de correspondencia que explican qué busca la secretaria, qué es lo que nosotros debemos entregar y muchas otras aclaraciones que explican la situación y el objetivo del proyecto
          * 3. PLAN DE SEGURIDAD: Explicaciones de lo que ya tiene la SDM y lo que buscan explorar e implementar en materia de seguridad personal
          * 4. BICITAXIS: Explicaciones de lo que se quiere hacer con bicitaxis y documentos de estudios previos
          * 5. CENTRO DE LA BICI: El objetivo a largo plazo del proyecto, edificio de administración
          * 6. CICLOVIA DOMINICAL: Existen algunas bases de datos particulares del día domingo, acá se contiene toda la explicación de la logística de las ciclovías de los días festivos
          * 7. CONTRATO SEMAFOROS: Documentos sobre el contrato de semaforización
          * 8. BIBLIOGRAFIA: Incluir aca todos los documentos de referencia externos que estemos utilizando (papers, libros, etc.)
      * Total de 25 fuentes de datos (al 4 de agosto de 2018), dentro de las diferentes carpetas pueden existir más de una base de datos, tablas, figuras geográficas y otros archivos usados como input para los procesos de análisis.
    
