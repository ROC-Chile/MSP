# Tutorial para el análisis de abundancia de aves playeras utilizando datos del [Migratory Shorebird Project](https://migratoryshorebirdproject.org) (MSP)

El **Migratory Shorebird** Project busca potenciar las capacidades locales para soluciones en la conservación de humedales a lo largo de la costa Pacifica de las Américas. Una arista del MSP se enfoca en el uso de los datos recolectados por censos estandarizados en sitios de importancia para aves playeras. En ese contexto, este repositorio busca entregar una herramienta analítica que describe el procedimiento para obtener tendencias en abundancia a nivel de especies, sitios y nacional de aves playeras, utilizando datos colectados en el marco del esquema de monitoreo MSP en Chile. No obstante, este código busca ser facilmente adaptable para datos recolectado bajo este esquema en cualquiera de los paises asociados.

El tutorial está dividido en tres componentes de código que detallan los pasos a seguir utilizando el paquete **SpAbundance** de [Doser et al.](https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/2041-210X.14332). Este paquete trabaja en un marco de estadística Bayesiana, pero drásticamente simplifica su implementación al adoptar el syntax ampliamente utilizado por paquetes como **lmer**. El uso de esta herramienta requiere un conocimiento moderado de R y el código está extensamente anotado.

## Preparación de Datos
Empezar por el archivo [PrepDataMSP+.R](https://github.com/ROC-Chile/MSP/blob/main/PrepDataMSP%2B.R) que detalla cómo realizar las preparaciones pre-análisis de los datos después de ser descargados desde CADC. En este caso proveemos un ejemplo de datos de Chile en el archivo [DatosMSPnov2023.csv](https://github.com/ROC-Chile/MSP/blob/main/DatosMSPnov2023.csv).

## Análisis de Abundancia
En el archivo [NMixMSPverAMOY.R](https://github.com/ROC-Chile/MSP/blob/main/NMixMSPverAMOY.R) se de detalla la construcción del modelo en spAbundance.

## Gráficar los Resultados
Por último, en el archivo [SummaryNacMSPverAMOY](https://github.com/ROC-Chile/MSP/blob/main/SummaryNacMSPverAMOY.R) se encuentra el código necesario para producir gráficos por especies, a nivel de sitios y a nivel nacional de todas las especies.

## 
Si tienes preguntas puedes contactarte con nosotros enviando un correo a [Marion Díaz](mariondiaz@redobservadores.cl) o [Erik Sandvig](eriksandvig@redobservadores.cl) del Programa de Monitoreo de la ROC.

Este tutorial fue desarrollado por la Red de Observadores de Aves y Vida Silvestre de Chile (ROC) en el marco de un proyecto MSP+ financiado por Point Blue Conservation.

![ROC](roc_logo_horizontal_Small.jpeg)      ![Point_Blue](Point_Blue_logo.jpeg)
