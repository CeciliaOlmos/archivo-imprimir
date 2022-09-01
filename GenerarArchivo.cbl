      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
        ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\imp\archSociosGen.dat".
       DATA DIVISION.
       FILE SECTION.

       FD  SOCIOS.
       01  soc-reg.
           03 soc-socio pic 9(4).
           03 soc-nombre pic x(20).
           03 soc-importe pic s9(8)v99.

       WORKING-STORAGE SECTION.
       01  w-cant-registros pic 9(5).
       01  w-i pic 9(5).
       01  w-cant-saldo-total pic s9(8)v99.
       01  w-cont-mil pic s9(8)v99.

      ******************LINEAS DE IMPRESION***************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(20) value "LISTADO DE REGISTROS".
           03 filler pic x(30) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-soc.
           03 filler pic x(17) value spaces.
           03 filler pic x(5) value "SOCIO".
           03 filler pic x(8) value spaces.
           03 filler pic x(20) value "NOMBRE".
      *     03 filler pic x(5) value spaces.
           03 filler pic x(7) value "IMPORTE".
           03 filler pic x(20) value spaces.
       01  lin-val.
           03 filler pic x(17) value spaces.
           03 l-soc pic x(5) value spaces.
           03 filler pic x(8) value spaces.
           03 l-nombre pic x(20).
           03 filler pic x(5) value spaces.
           03 l-saldo pic zz.zzz.zz9,99.
           03 filler pic x(20) value spaces.

       PROCEDURE DIVISION.
      ****************** PROGRAMA PRINCIPAL********************************
       MAIN-PROCEDURE.

           PERFORM 100-INICIO-GENERAL.
            PERFORM VARYING w-i FROM 1 BY 1 UNTIL w-i > w-cant-registros
               PERFORM 300-PROCESO
            END-PERFORM.
           PERFORM 500-FIN-GENERAL.

           STOP RUN.

      ****************** INICIO RUTINAS ********************************
       100-INICIO-GENERAL.
           PERFORM 110-INICIALIZAR-VARIABLES.
           PERFORM 120-ABRIR-ARCHIVO.
           PERFORM 150-PEDIR-CANT-REG-A-GENERAR.
           PERFORM 130-MOSTRAR-ENCABEZADO.

       110-INICIALIZAR-VARIABLES.
           MOVE ZERO TO w-cant-registros.
           MOVE ZERO TO w-cant-saldo-total.
           MOVE ZERO TO w-cont-mil.

       120-ABRIR-ARCHIVO.
           OPEN OUTPUT SOCIOS.

       130-MOSTRAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-titulo.
           DISPLAY lin-guarda.
           DISPLAY lin-soc.
           DISPLAY lin-guarda.

       150-PEDIR-CANT-REG-A-GENERAR.
           DISPLAY "***INGRESE CUANTOS REGISTROS DESEA GENERAR***".
           ACCEPT w-cant-registros.

       300-PROCESO.
           PERFORM 320-GENERAR-REGISTRO.
           PERFORM 350-ESCRIBIR-REGISTRO.

       320-GENERAR-REGISTRO.
           PERFORM 323-INCREMENTAR-NRO-SOCIO.
           PERFORM 324-GENERAR-IMPORTE.
           PERFORM 325-CONCATENAR-CADENA.
           PERFORM 330-MOSTRAR-DATOS.

       323-INCREMENTAR-NRO-SOCIO.
           MOVE w-i TO soc-socio.

       324-GENERAR-IMPORTE.
           ADD 1000 TO w-cont-mil.
           MOVE w-cont-mil to soc-importe.
           ADD soc-importe to w-cant-saldo-total.

       325-CONCATENAR-CADENA.
           STRING
            "socio" DELIMITED by SIZE
            soc-socio DELIMITED BY SIZE
           INTO soc-nombre
           end-string.

       330-MOSTRAR-DATOS.
           PERFORM 340-GENERAR-LINEA.
           DISPLAY lin-val.

       340-GENERAR-LINEA.
           MOVE soc-socio TO l-soc.
           MOVE soc-nombre TO l-nombre.
           MOVE soc-importe TO l-saldo.

       350-ESCRIBIR-REGISTRO.
           WRITE soc-reg.

       500-FIN-GENERAL.
           PERFORM 510-CERRAR-ARCHIVO.
           DISPLAY lin-guarda.

       510-CERRAR-ARCHIVO.
           CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
