       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-WRITER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEMO_DATA ASSIGN TO "../src/resources/DEMO_DATA.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD DEMO_DATA.
           01 DEMO_DATA_RECORD     PIC X(50).
           
       
       WORKING-STORAGE SECTION.

       *> ===========================================================
       *> This program updates a database based off of information 
       *> read from an input data file.

       *> example argument usage:

       *> run live program
       *>      ./demo_writer

       *> ===========================================================

       *> end of file definitions
       01 WS_EOF                           PIC X(1) VALUE 'N'. 
           88 WS_EOF_FALSE                     VALUE 'N'.
           88 WS_EOF_TRUE                      VALUE 'Y'.
       
       *> other switches
       01 WS_GOOD_DATE                           PIC X(1) VALUE 'F'. 
           88 WS_GOOD_DATE_FALSE                     VALUE 'F'.
           88 WS_GOOD_DATE_TRUE                      VALUE 'T'.
       
       01 WS_GOOD_STRING                           PIC X(1) VALUE 'F'. 
           88 WS_GOOD_STRING_FALSE                     VALUE 'F'.
           88 WS_GOOD_STRING_TRUE                      VALUE 'T'.

       *> ocesql declarations
       01  DB_TABLENAME                PIC X(15).
       
       01 DB_FINISHED_FETCHING_FLAG         PIC X(1) VALUE 'F'. 
           88 DB_FINISHED_FETCHING                   VALUE 'T'.
       
       01  DEMO_REC.
           05 DEMO_DATE.
                   15 DEMO_DATE_YYYY PIC X(04).
                   15 DEMO_DATE_MM   PIC X(02).
                   15 DEMO_DATE_DD   PIC X(02).
           05 DEMO_STRING.
               10 DEMO_STRING_DAY              PIC X(02).
               10 DEMO_STRING_SWITCH         PIC X(01).
               10 DEMO_STRING_COUNTER        PIC 9(03).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       01  DB_RECORD_COUNT         PIC 9(04).
       01  DB_RECORDS.
           05  DB_DATE             PIC X(10).
           05  DB_STRING           PIC X(12).
               
       EXEC SQL END DECLARE SECTION END-EXEC.
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       *> end ocesql declaration
       
       PROCEDURE DIVISION.
       
           MOVE "postgres@db:5432" TO DBNAME
           MOVE "postgres"         TO USERNAME
           MOVE "postgres"         TO PASSWD


           PERFORM B1000_GENERAL_LOGIC

           STOP RUN.
       
       B1000_GENERAL_LOGIC.
           *> SETUP 
           PERFORM B3100_CONNECT
           
           *> GENERAL LOGIC            
           PERFORM B4000_ITERATE_FILE_ENTRIES

           PERFORM B3500_FETCH_ROWS_INIT

           PERFORM UNTIL DB_FINISHED_FETCHING
                PERFORM B3501_FETCH_ROWS_READ_NEXT 

           END-PERFORM
           
           *> FINISH UP
           PERFORM B3900_DISCONNECT
           
           EXIT.
               
       *> DATABASE PROCEDURES
       B3100_CONNECT.

           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           IF  SQLCODE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
   
           EXIT.

       B3400_INSERT_ROW.

            STRING DEMO_DATE_YYYY DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                DEMO_DATE_MM DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                DEMO_DATE_DD DELIMITED BY SIZE 
            INTO DB_DATE

            MOVE DEMO_STRING TO DB_STRING

            *> INSERT STATEMENT

            EXEC SQL
                INSERT INTO demo_table
                    (report_date, report_text)
                VALUES (:DB_DATE, :DB_STRING)
            END-EXEC

            IF SQLCODE NOT = ZERO 
                PERFORM B8000_SQL_ERROR STOP RUN.

           EXIT.
       
       B3500_FETCH_ROWS_INIT.

            *> DECLARE CURSOR
            EXEC SQL
                DECLARE C1 CURSOR FOR
                    SELECT report_date, report_text
                    FROM demo_table
            END-EXEC

            
            EXEC SQL
               OPEN C1
            END-EXEC.
            IF  SQLSTATE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
   
           MOVE 'F' TO DB_FINISHED_FETCHING_FLAG
   
           EXIT.
       
       B3501_FETCH_ROWS_READ_NEXT.
               
           EXEC SQL
                   FETCH C1 INTO :DB_DATE, :DB_STRING
           END-EXEC

           MOVE  DB_DATE(1:4)   TO    DEMO_DATE_YYYY
           MOVE  DB_DATE(6:2)   TO    DEMO_DATE_MM
           MOVE  DB_DATE(9:2)   TO    DEMO_DATE_DD
           MOVE  DB_STRING      TO    DEMO_STRING
           
           IF SQLCODE NOT = ZERO    
               IF SQLSTATE = "02000" 
                   SET DB_FINISHED_FETCHING TO TRUE
                   
               ELSE 
                   PERFORM B8000_SQL_ERROR STOP RUN
               END-IF
           END-IF
       
           EXIT.

       B3900_DISCONNECT.
           EXEC SQL COMMIT WORK END-EXEC.

           EXEC SQL 
               CLOSE C1
           END-EXEC

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

           EXIT.
       
       *> DATA LAYER
       B4000_ITERATE_FILE_ENTRIES.
           OPEN INPUT DEMO_DATA
               PERFORM UNTIL WS_EOF_TRUE
                   READ DEMO_DATA INTO DEMO_REC
                       AT END MOVE 'Y' TO WS_EOF
                       NOT AT END
                           
                           PERFORM B4100_PROCESS_FILE_RECORD
                           PERFORM B3400_INSERT_ROW
       
                   END-READ
               END-PERFORM.
           CLOSE DEMO_DATA.

           EXIT.
       
       *> BUSINESS LOGIC
       B4100_PROCESS_FILE_RECORD.
           PERFORM B4200_DATE_CHECK
           PERFORM B4300_STRING_CHECK.
           IF WS_GOOD_STRING_TRUE AND WS_GOOD_DATE_TRUE
                   PERFORM B4301_UPDATE_STRING_SWITCH    
           END-IF.
           
           EXIT.
       
       B4200_DATE_CHECK.
           MOVE 'T' TO WS_GOOD_DATE
           IF DEMO_DATE NOT NUMERIC 
               MOVE 'F' TO WS_GOOD_DATE
           EXIT.
       
       B4300_STRING_CHECK.
           MOVE 'T' TO WS_GOOD_STRING
           IF DEMO_STRING = SPACES OR DEMO_STRING = LOW-VALUE
               MOVE 'F' TO WS_GOOD_STRING
           END-IF.
       
           EXIT.
       
       B4301_UPDATE_STRING_SWITCH.
           MOVE 'Z' TO DEMO_STRING_SWITCH.
           EXIT.
       
       *> ERRORS
       B8000_SQL_ERROR.
          DISPLAY "*** SQL ERROR ***".
          DISPLAY "SQLCODE: " SQLCODE " " NO ADVANCING.
          EVALUATE SQLCODE
             WHEN  +10
                DISPLAY "Record not found"
             WHEN  -01
                DISPLAY "Connection falied"
             WHEN  -20
                DISPLAY "Internal error"
             WHEN  -30
                DISPLAY "PostgreSQL error"
                DISPLAY "ERRCODE: "  SQLSTATE
                DISPLAY SQLERRMC
             *> TO RESTART TRANSACTION, DO ROLLBACK.
                EXEC SQL
                    ROLLBACK
                END-EXEC
             WHEN  OTHER
                DISPLAY "Undefined error"
                DISPLAY "ERRCODE: "  SQLSTATE
                DISPLAY SQLERRMC
          END-EVALUATE.

          MOVE 1 TO RETURN-CODE.

          EXIT.
