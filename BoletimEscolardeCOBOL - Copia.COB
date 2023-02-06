      ******************************************************************
      * Author:Pedro Barros
      * Date:02/02/2023
      * Purpose:Calcular um Boletim ESCOLAR
      * Tectonics: cobc
      * Sistema: CFP
      * Linguagem: COBOL
      * Analista do Sistema: Andre Costa
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BoletimEscolardeCOBOL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77  WS-NOME             PIC X(40)       VALUE SPACES.
       77  WS-MATERIA          PIC X(25)       VALUE SPACES.
       77  WS-STATUS           PIC X(10)       VALUE SPACES.
       77  WS-REPETIR          PIC X(01)       VALUE SPACES.

       77  WS-NOTA1            PIC 9(2)V99    VALUE ZEROS.
       77  WS-NOTA2            PIC 9(2)V99    VALUE ZEROS.
       77  WS-NOTA3            PIC 9(2)V99    VALUE ZEROS.
       77  WS-NOTA4            PIC 9(2)V99    VALUE ZEROS.
       77  WS-MEDIA            PIC 99(04)V99  VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       P001-START.
              PERFORM P100-INICIO.
              PERFORM P999-FINAL
            .

       P100-INICIO.

           DISPLAY ' Insira Nome do Aluno : '
                 ACCEPT WS-NOME.

           DISPLAY 'Materia :'
                 ACCEPT WS-MATERIA.

               DISPLAY 'Insira a primeira nota: '
                 ACCEPT WS-NOTA1.


               DISPLAY 'Insira a segunda nota: '
                 ACCEPT WS-NOTA2.


               DISPLAY 'Insira a terceira nota: '
                ACCEPT WS-NOTA3.


               DISPLAY 'Insira a quarta nota: '
                 ACCEPT WS-NOTA4


             PERFORM P400-VALINUM
             PERFORM P200-CALC
           .
       P100-FIM.


       P200-CALC.

           COMPUTE WS-MEDIA =
               (WS-NOTA1 + WS-NOTA2 + WS-NOTA3 + WS-NOTA4) / 4
                ON SIZE ERROR PERFORM P800-ERRO
           END-COMPUTE

           IF WS-MEDIA <0 OR WS-MEDIA >10
               PERFORM P800-ERRO


            ELSE
             IF WS-MEDIA >=7
               MOVE 'APROVADO' TO WS-STATUS
             ELSE
               MOVE 'REPROVADO' TO WS-STATUS

             END-IF

             PERFORM P450-RESULTADO


           .

       P200-FIM.

       P300-REPETIR.


         DISPLAY 'Digite S(sim) para repetir ou N(Nao) para sair.'
             ACCEPT WS-REPETIR

             EVALUATE WS-REPETIR

               WHEN 'S'
               WHEN 's'
               PERFORM P100-INICIO

               WHEN 'N'
               WHEN 'n'
               PERFORM P999-FINAL

               WHEN OTHER
               PERFORM P300-REPETIR

            END-EVALUATE.

       P300-FIM.


       P400-VALINUM.


            IF (WS-NOTA1 IS NOT NUMERIC) OR (WS-NOTA1 IS = 0000)
                OR (WS-NOTA2 IS NOT NUMERIC) OR (WS-NOTA2 IS = 0000)
                OR (WS-NOTA3 IS NOT NUMERIC) OR (WS-NOTA3 IS = 0000)
                OR (WS-NOTA4 IS NOT NUMERIC) OR (WS-NOTA4 IS = 0000)
                    PERFORM P800-ERRO
            ELSE
                PERFORM P200-CALC


             .


       P400-FIM.

       P450-RESULTADO.

               DISPLAY '*** RESULTADO DO PROCESSAMENTO ***'
               DISPLAY 'Nome do Aluno : ' WS-NOME
               DISPLAY 'Materia : '       WS-MATERIA
               DISPLAY 'Media : '         WS-MEDIA
               DISPLAY 'Status : '        WS-STATUS
               DISPLAY '*******************************************'


             PERFORM P300-REPETIR
             .
       P450-FIM.

       P800-ERRO.

            DISPLAY '***********************************************'
            DISPLAY '* NOTAS INVALIDAS - TENTE NOVAMENTE *'
            DISPLAY '***********************************************'

           PERFORM P300-REPETIR
           .

       P800-FIM.

       P999-FINAL.
             DISPLAY 'Muito obrigado.'
            STOP RUN.

       END PROGRAM BoletimEscolardeCOBOL.
