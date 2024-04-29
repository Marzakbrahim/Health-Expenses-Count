       IDENTIFICATION DIVISION.

       PROGRAM-ID. 'CONVDATE'.

      *****************************************************************
      *                                                               *
      *                  C O N V D A T E                              *
      *                 ------------------                            *
      *                                                               *
      *                                                               *
      *****************************************************************
      *  AUTEUR : MARATRPA                                            *
      *                                                               *
      *  DATE   : 15/11/23                                            *
      *---------------------------------------------------------------*
      *                  MODIFICATIONS                                *
      *---------------------------------------------------------------*


      *=================================================================

      *=================================================================
       ENVIRONMENT DIVISION.
      *----------------------
       CONFIGURATION SECTION.
      *----------------------

      *SOURCE-COMPUTER. IBM-3090 WITH DEBUGGING MODE.
       SOURCE-COMPUTER. IBM-3090.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.


      *=================================================================
       DATA DIVISION.
      *=================================================================


      *----------------------
       WORKING-STORAGE SECTION.
      *----------------------

      *-----------------------------------------------------------------
      *    Definition des constantes
      *-----------------------------------------------------------------
      *  identifiant pgm
       01 C-NOMPGM                     PIC X(08)  VALUE 'CONVDATE'.

      *----------------------
       LOCAL-STORAGE SECTION.
      *----------------------

      *
       01  W-SSAAQQQ-LE                PIC 9(7).
       01  w-ssaaqqq-le-bis     redefines w-ssaaqqq-le.
           02 W-ANNEE-LE               PIC 9(4).
           02 W-QQQ-LE                 PIC 9(3).

       01  w-ssaammjj-le               pic 9(8).
       01  w-ssaammjj-bis redefines w-ssaammjj-le.
           02 W-ssaa-le                pic 9(4).
           02 W-mm-le                  pic 9(2).
           02 W-jj-le                  pic 9(2).

      *    Indicateur d'annee bissextile
       01  BISSEXTILE-ON               PIC X(01).
           88  EST-BISSEXTILE          VALUE 'O'.
           88  NON-BISSEXTILE          VALUE 'N'.

       01  L-DIVISION                  PIC S9(5) COMP.
       01  RESTE                       PIC S9(5) COMP.
       01  RESTE2                      PIC S9(5) COMP.


      *-----------------------------------------------------------------
      *    Declarations pour gestion des erreurs
      *-----------------------------------------------------------------
      *--  Liste des erreurs gerees par le programme
       01 W-LstErr                     PIC X(80).
          88 W-Trt-OK                  VALUE
          '00                                     '.
          88 W-ErrCasNonGer            VALUE
          '01Comparaison de reference non gere    '.
          88 W-Err02                   VALUE
          '02LA DATE N''EST PAS RENSEIGNEE OU EST ERRONEE'.

       01  FILLER REDEFINES W-LstErr.
           05 W-CodErr                 PIC X(02).
           05 W-LibErr                 PIC X(78).

      * Variables pour CONVDATE
       01 W-DATE-SSAAQQQ-U.
          05 W-DATE-SSAAQQQ            PIC S9(07) COMP-3.

       01 W-DATE-SSAA-MM-JJ            PIC X(10).


      *----------------------
       LINKAGE SECTION.
      *----------------------

       01 YDATCNV.           COPY 'C:\Users\HP\Downloads\YDATCNV'.


      *=================================================================
       PROCEDURE DIVISION USING YDATCNV.
      *=================================================================


      *-----------------------------------------------------------------
       DEBUT.
      *------

      *-----------------------------------------------------------------
      * DEBUT DE TRAITEMENT
      *-----------------------------------------------------------------

      D    DISPLAY 'je suis dans convdate'
           PERFORM INITIALISATION

           IF W-Trt-OK
              PERFORM TRAITEMENT
           END-IF

           PERFORM TRAITEMENT-FIN
           .

      *-----------------------------------------------------------------
       INITIALISATION.
      *---------------

           SET W-Trt-OK                       TO TRUE
           SET DATCNV-DatSor-CodErr-OK        TO TRUE
           .

      *-----------------------------------------------------------------
       TRAITEMENT.
      *---------------

      *--- Manipulation de SSAAQQQ pour devenir SSAAMMJJ
           MOVE  DATCNV-DatEnt-SSAAQQQ    TO  W-SSAAQQQ-LE

      *
      *--- Recherche si l'annee est bissextile
           PERFORM BISSEXTILE

      D    Display 'BISSEXTILE-ON =' BISSEXTILE-ON '>'
      D    display 'W-QQQ-LE =' W-QQQ-LE '>'
           IF (EST-BISSEXTILE  AND W-QQQ-LE > 366)   OR
              (NON-BISSEXTILE  and W-QQQ-LE > 365)
              Set W-Err02                            TO TRUE
      *>         PERFORM TRAITEMENT-FIN
           END-IF
           IF W-QQQ-LE <= 000
              Set W-Err02                            TO TRUE
      *>         PERFORM TRAITEMENT-FIN
           END-IF

           IF W-Trt-OK
              COMPUTE W-SSAAMMJJ-LE = FUNCTION DATE-OF-INTEGER(
                         FUNCTION INTEGER-OF-DAY (W-SSAAQQQ-LE))
      D       display "w-ssaaqqq-le =" w-ssaaqqq-le
              MOVE W-SSAAMMJJ-LE    TO DATCNV-DatSor-SSAAMMJJ
           END-IF
           .

      *-----------------------------------------------------------------
       BISSEXTILE.
      *    Verification si l'annee est bissextile ou non :
      *    - parametre en entree : AN-BISSEXT
      *                            DATS20-I-BISSEXTILE-365
      *    - parametre en sortie : DATS20-O-BISSEXTILE


           SET NON-BISSEXTILE TO TRUE
           DIVIDE  W-ANNEE-LE BY 4 GIVING L-DIVISION REMAINDER RESTE

           IF  RESTE = ZERO
      * C'est une annee bissextile mais pas fini
              SET EST-BISSEXTILE TO TRUE
      * Verification sur la division par 100
              DIVIDE  W-ANNEE-LE BY 100 GIVING L-DIVISION
              REMAINDER RESTE
              IF  RESTE = ZERO
      *          L'annee n'est pas bissextile finalement mais pas fini
                 SET NON-BISSEXTILE TO TRUE
      * Verification sur la division par 400 (la derniere)
                 DIVIDE  W-ANNEE-LE  BY 400 GIVING L-DIVISION
                 REMAINDER RESTE2
                 IF  RESTE2 = ZERO
      *             L'anne est finalement bissextile
                    SET EST-BISSEXTILE TO TRUE
                 END-IF
              END-IF
           END-IF
           .

      *-----------------------------------------------------------------
       TRAITEMENT-FIN .
      *---------------

           IF Not W-Trt-OK
              MOVE W-CodErr             TO DATCNV-DatSor-CodErr
              MOVE W-LibErr             TO DATCNV-DatSor-LibErr
           END-IF

           GOBACK
           .

       END PROGRAM CONVDATE.
