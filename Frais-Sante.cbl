       IDENTIFICATION DIVISION.
       PROGRAM-ID. Frais-Sante.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
      *    Fichier  d'entrie DCPT-S :
           SELECT DCPT-S ASSIGN TO
               "C:/Users/HP/Downloads/DCPT-S.txt"
                ORGANIZATION       IS LINE SEQUENTIAL
                FILE STATUS        IS L-Fst-In
                .
      *    Fichier de sortie MODIF-DCPT-S :
           SELECT MODIF-DCPT-S ASSIGN TO
               "C:/Users/HP/Downloads/MODIF-DCPT-S.txt"
                ORGANIZATION       IS LINE SEQUENTIAL
                FILE STATUS        IS L-Fst-Out
           .

      *    Fichier d'erreurs :
           SELECT DCPT-S-Erreur ASSIGN TO
               "C:/Users/HP/Downloads/DCPT-S-Erreur.txt"
                ORGANIZATION       IS LINE SEQUENTIAL
                FILE STATUS        IS L-Fst-Err
           .

       DATA DIVISION.
       FILE SECTION.

       FD DCPT-S.
       01 ENR-DCPT-S.
           05 ENR-DCPT-S-COD-PROD                       PIC X(2).
           05 FILLER                                    PIC X(3).
           05 ENR-DCPT-S-NUM-PLC                        PIC X(12).
           05 FILLER                                    PIC X(1).
           05 ENR-DCPT-S-DAT-SOINS                      PIC X(7).
           05 FILLER                                    PIC X(1).
           05 ENR-DCPT-S-MONT-REMB                      PIC 9(7)V99.


       FD MODIF-DCPT-S.
       01 ENR-MODIF-DCPT-S.
           05 ENR-MODIF-DCPT-S-Mois                  PIC X(4).
           05 ENR-MODIF-DCPT-S-ETOILE                PIC X(7).
           05 ENR-MODIF-DCPT-S-ANNEE                 PIC X(4).
           05 ENR-MODIF-DCPT-S-ETOILE2               PIC X(7).
           05 ENR-MODIF-DCPT-S-MONT-TOT              PIC X(23).


       FD DCPT-S-Erreur.
       01 ENR-DCPT-S-Erreur                              PIC X(36).

       WORKING-STORAGE SECTION.

      * Variables File status

       01 L-Fst-In                                        PIC 9.
       01 L-Fst-Out                                        PIC 9.
       01 L-Fst-Err                                        PIC 9.

      * Structures pour stocker le fichier en entrée
       01 WS-ENR-DCPT-S.
           05 WS-COD-PROD                                  PIC X(2).
           05 FILLER                                    PIC X(3).
           05 WS-NUM-PLC                                   PIC X(12).
           05 FILLER                                    PIC X(1).
           05 WS-DAT-SOINS                                 PIC X(7).
           05 FILLER                                    PIC X(1).
           05 WS-MONT-REMB                                 PIC 9(7)V99.


      * Booléens pour tester la fin de lecture :

       01 Fin-DCPT-S                            PIC 9.
           88 Fin-DCPT-S-Oui                       VALUE 1.
           88 Fin-DCPT-S-Non                       VALUE 0.

      * Compteurs
       01 COMPTEURS.
           05 CPT-DCPT-S                                PIC 9(10).
           05 CPT-MODIF-DCPT-S                          PIC 9(10).
           05 CPT-ERR                                   PIC 9(10).

      * Variables pour le traitement :
       01 WS-Variable-Trait.
           COPY 'C:\Users\HP\Downloads\YDATCNV'.


       01 WS-ENR-MODIF-DCPT-S.
           05 WS-ENR-MODIF-DCPT-S-Mois                  PIC X(4).
           05 WS-ENR-MODIF-DCPT-S-ETOILE                PIC X(7).
           05 WS-ENR-MODIF-DCPT-S-ANNEE                 PIC X(4).
           05 WS-ENR-MODIF-DCPT-S-ETOILE2               PIC X(7).
           05 WS-ENR-MODIF-DCPT-S-MONT-TOT              PIC 9(21)V99.
      * indice :
      * 01 i PIC 9999999.

      * le type d'erreur :
       01 EXISTE-RREUR                                  PIC 9.
           88 ERREUR-OUI                                VALUE 0.
           88 ERREUR-NON                                VALUE 1.

       PROCEDURE DIVISION.

      ****************
       MAIN-PROCEDURE.
      ****************

           PERFORM INITIALISATION             THRU FIN-INITIALISATION

           PERFORM TRAITEMENT                 THRU FIN-TRAITEMENT

           PERFORM FIN                        THRU FIN-FIN

           GOBACK
           .

      *-----------------------------------------------------------------
      *****************
       INITIALISATION.
      *****************
           DISPLAY '***************************************************'
           DISPLAY '***          PANIERS ET  PRODUITS               ***'
           DISPLAY '***************************************************'

      * Initialisation des dfférentes variables et des différents
      * booléens

           INITIALISE COMPTEURS
                      L-Fst-In
                      L-Fst-Out
                      L-Fst-Err
                      WS-Variable-Trait
                      WS-ENR-DCPT-S
                      WS-ENR-MODIF-DCPT-S


           SET Fin-DCPT-S-Non    TO TRUE
           SET ERREUR-NON        TO TRUE
      *     MOVE 1 TO i
           MOVE "   *   " TO WS-ENR-MODIF-DCPT-S-ETOILE
           MOVE "   *   " TO WS-ENR-MODIF-DCPT-S-ETOILE2

      * Ouverture des fichiers

           OPEN INPUT   DCPT-S
           OPEN OUTPUT   MODIF-DCPT-S
           OPEN OUTPUT  DCPT-S-Erreur

      *    L'ecriture de l'entête :
           PERFORM Premeire-Ecr THRU FIN-Premeire-Ecr

      * Première Traitement avant d'entrer au boucle.

           PERFORM LECTURE-FICHIER    THRU FIN-LECTURE-FICHIER
           PERFORM Controle THRU FIN-Controle
           IF ERREUR-NON
             MOVE ENR-DCPT-S TO WS-ENR-DCPT-S
             MOVE WS-DAT-SOINS TO DATCNV-DatEnt-SSAAQQQ
             CALL 'CONVDATE' USING WS-Variable-Trait
             MOVE DATCNV-DatSor-SSAA TO WS-ENR-MODIF-DCPT-S-ANNEE
             MOVE DATCNV-DatSor-MM TO WS-ENR-MODIF-DCPT-S-Mois
             ADD WS-MONT-REMB TO WS-ENR-MODIF-DCPT-S-MONT-TOT
           END-IF
           .

      ********************
       FIN-INITIALISATION. EXIT.
      ********************
      *-----------------------------------------------------------------
      *********************
       TRAITEMENT.
      *********************
           PERFORM UNTIL Fin-DCPT-S-Oui
             PERFORM LECTURE-FICHIER THRU FIN-LECTURE-FICHIER
             PERFORM Controle THRU FIN-Controle
             IF ERREUR-NON
               MOVE ENR-DCPT-S TO WS-ENR-DCPT-S
               MOVE WS-DAT-SOINS TO DATCNV-DatEnt-SSAAQQQ
               CALL 'CONVDATE' USING WS-Variable-Trait
               IF DATCNV-DatSor-MM NOT = WS-ENR-MODIF-DCPT-S-Mois
                 MOVE DATCNV-DatSor-SSAA TO WS-ENR-MODIF-DCPT-S-ANNEE
                 MOVE WS-ENR-MODIF-DCPT-S TO ENR-MODIF-DCPT-S
                 PERFORM ECR-FICHIER THRU FIN-ECR-FICHIER
                 MOVE 0 TO WS-ENR-MODIF-DCPT-S-MONT-TOT
                 MOVE DATCNV-DatSor-MM TO WS-ENR-MODIF-DCPT-S-Mois
               ELSE
                 ADD WS-MONT-REMB TO WS-ENR-MODIF-DCPT-S-MONT-TOT
               END-IF
             END-IF
           END-PERFORM
           .
      *************************
       FIN-TRAITEMENT. EXIT.
      *************************

      *----------------------------------------------------------------*
      **************
       Premeire-Ecr.
      **************
      * La première ecriture :
           MOVE "MOIS" TO ENR-MODIF-DCPT-S-Mois
           MOVE "   *   " TO ENR-MODIF-DCPT-S-ETOILE
           MOVE "ANNE" TO ENR-MODIF-DCPT-S-ANNEE
           MOVE "   *   " TO ENR-MODIF-DCPT-S-ETOILE2
           MOVE "MONTANT TOTAL REMBOURSE" TO ENR-MODIF-DCPT-S-MONT-TOT
           PERFORM ECR-FICHIER THRU FIN-ECR-FICHIER
           .
      ******************
       FIN-Premeire-Ecr. EXIT.
      ******************

      *******************
       LECTURE-FICHIER.
      *******************

           READ DCPT-S
           AT END
               SET Fin-DCPT-S-Oui  TO TRUE

           NOT AT END
               IF L-Fst-In NOT = ZERO
                   DISPLAY 'Erreur lecture fichier 1 =' L-Fst-In
               END-IF
               ADD 1 TO CPT-DCPT-S
      *>          DISPLAY "Fichier DCPT-S : Enregistrement numéro "
      *>                            CPT-DCPT-S " est " ENR-DCPT-S
           END-READ
           .
      ***********************
       FIN-LECTURE-FICHIER. EXIT.
      ***********************
      *-----------------------------------------------------------------

      *****************
       ECR-FICHIER.
      *****************
           WRITE ENR-MODIF-DCPT-S
           IF L-Fst-Out NOT ZERO
               DISPLAY "Erreur ecriture fichier erreur = " L-Fst-Out
           END-IF
           ADD 1              TO CPT-MODIF-DCPT-S
           .
      *********************
       FIN-ECR-FICHIER. EXIT.
      *********************
      *-----------------------------------------------------------------
      **********
       Controle.
      **********
           IF ENR-DCPT-S-COD-PROD IS NUMERIC
             SET ERREUR-OUI TO TRUE
             PERFORM ECR-ERREUR THRU FIN-ECR-ERREUR
           END-IF
           .
      **************
       FIN-Controle. EXIT.
      **************

      ************
       ECR-ERREUR.
      ************
           MOVE ENR-DCPT-S TO ENR-DCPT-S-Erreur
           WRITE ENR-DCPT-S-Erreur
           IF L-Fst-Err NOT ZERO
             DISPLAY "Erreur de type : " L-Fst-Err
           END-IF
           ADD 1 TO CPT-ERR

           .
      ****************
       FIN-ECR-ERREUR. EXIT.
      ****************

      ******
       FIN.
      ******

      * Fermeture de tous les fichiers

           CLOSE DCPT-S
           CLOSE MODIF-DCPT-S
           CLOSE DCPT-S-Erreur

      * Display des compteurs et du nombre d'erreurs s'il y en a

           DISPLAY "Nombre d'enregistrements lus : " CPT-DCPT-S
           DISPLAY "Nombre d'enregistrements écrits : " CPT-MODIF-DCPT-S
           IF CPT-ERR NOT = ZERO
               DISPLAY '***********************************************'
               DISPLAY '***********************************************'
               DISPLAY '**************IL Y A DES ERREURS***************'
               DISPLAY '***********************************************'
               DISPLAY '***********************************************'
               DISPLAY "Nombre d'erreurs : "           CPT-ERR
           END-IF

           DISPLAY 'Fin de traitement'
           .

      **********
       FIN-FIN.  EXIT.
      **********
       END PROGRAM Frais-Sante.
      *-----------------------------------------------------------------
