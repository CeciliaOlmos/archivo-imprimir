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
           SELECT LISTADO ASSIGN TO
           PRINTER, "D:\linux cecilia\COBOL\archivo\imp\impre.dat".
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
       FD  listado.
       01  lis-reg pic x(80).

       WORKING-STORAGE SECTION.
       01  w-flag-archivo pic 9.
       01  w-cont-lineas pic 99.
       01  w-cont-paginas pic 99.
       01  w-contador pic 999.
       01  cabecera0.
           03 filler pic x(5) value spaces.
           03 filler pic x(7) value "PAG.NRO".
           03 filler pic x value "-".
           03 l-nro-pag pic 9(2).
           03 filler pic x(66) value SPACES.

       01  cabecera1.
           03 filler pic x(10).
           03 filler pic x(20) value "LISTADO DE REGISTROS".
           03 filler pic x(30) value spaces.
       01  cabecera2.
           03 filler pic x(80) value all "-".
       01  cabecera3.
           03 filler pic x(17) value spaces.
           03 filler pic x(5) value "SOCIO".
           03 filler pic x(8) value spaces.
           03 filler pic x(20) value "NOMBRE".
      *     03 filler pic x(5) value spaces.
           03 filler pic x(7) value "IMPORTE".
           03 filler pic x(20) value spaces.
       01  detalle.
           03 filler pic x(17) value spaces.
           03 l-soc pic x(5) value spaces.
           03 filler pic x(8) value spaces.
           03 l-nombre pic x(20).
           03 filler pic x(5) value spaces.
           03 l-saldo pic zz.zzz.zz9,99.
           03 filler pic x(20) value spaces.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-GENERAL.
           PERFORM 150-LEER-ARCHIVO.
            PERFORM UNTIL w-flag-archivo is equals 1
               PERFORM 300-PROCESO
               PERFORM 150-LEER-ARCHIVO
            END-PERFORM.
           PERFORM 500-FIN-GENERAL.
            STOP RUN.

        100-INICIO-GENERAL.
            PERFORM 105-ABRIR-ARCHIVOS.
            PERFORM 110-INICIALIZAR-VARIABLES.

        110-INICIALIZAR-VARIABLES.
            MOVE ZERO to w-flag-archivo.
            MOVE ZERO to w-contador.

        105-ABRIR-ARCHIVOS.
            OPEN INPUT SOCIOS.
            OPEN OUTPUT LISTADO.
        150-LEER-ARCHIVO.
            READ SOCIOS AT END MOVE 1 TO w-flag-archivo.

        200-INICIO-PAGINA.
            PERFORM 220-GENERAR-ENCABEZADO.
            PERFORM 240-LISTAR-ENCABEZADO.

        220-GENERAR-ENCABEZADO.
            ADD 1 TO w-cont-paginas.
            MOVE 50 to w-contador.
            MOVE w-cont-paginas to l-nro-pag.

        240-LISTAR-ENCABEZADO.
            WRITE lis-reg FROM cabecera0 AFTER 1.
            WRITE lis-reg FROM cabecera1 AFTER PAGE.
            WRITE lis-reg FROM cabecera2 AFTER 1.
            WRITE lis-reg FROM cabecera3 AFTER 1.
            MOVE 1 TO w-cont-lineas.

        300-PROCESO.
            IF w-cont-lineas >= w-contador THEN
               PERFORM 200-INICIO-PAGINA
               MOVE ZERO TO w-cont-lineas
            END-IF.
            IF w-cont-lineas < 50 THEN
               PERFORM  340-GENERAR-LINEA
               PERFORM 350-IMPRIMIR-LINEA
               ADD 1 TO w-cont-lineas
            END-IF.
        340-GENERAR-LINEA.
           MOVE soc-socio TO l-soc.
           MOVE soc-nombre TO l-nombre.
           MOVE soc-importe TO l-saldo.

        350-IMPRIMIR-LINEA.
           write lis-reg FROM detalle AFTER 1.
        500-FIN-GENERAL.
            PERFORM 510-CERRAR-ARCHIVOS.

        510-CERRAR-ARCHIVOS.
            CLOSE SOCIOS.
            CLOSE LISTADO.
       END PROGRAM YOUR-PROGRAM-NAME.
