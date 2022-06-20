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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       01  DB_RECORD_COUNT         PIC 9(04).
       01  DB_RECORDS.
           05  DB_DATE             PIC X(10).
           05  DB_STRING           PIC X(12).
               
OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.
       
OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".
       *> end ocesql declaration
       
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(067) VALUE "INSERT INTO demo_table (report"
OCESQL  &  "_date, report_text) VALUES ( $1, $2 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(047) VALUE "SELECT report_date, report_tex"
OCESQL  &  "t FROM demo_table".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
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

OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.
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

OCESQL*     EXEC SQL
OCESQL*         INSERT INTO demo_table
OCESQL*             (report_date, report_text)
OCESQL*         VALUES (:DB_DATE, :DB_STRING)
OCESQL*     END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_DATE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 12
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_STRING
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL          BY VALUE 2
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

            IF SQLCODE NOT = ZERO 
                PERFORM B8000_SQL_ERROR STOP RUN.

           EXIT.
       
       B3500_FETCH_ROWS_INIT.

            *> DECLARE CURSOR
OCESQL*     EXEC SQL
OCESQL*         DECLARE C1 CURSOR FOR
OCESQL*             SELECT report_date, report_text
OCESQL*             FROM demo_table
OCESQL*     END-EXEC
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_writer_C1" & x"00"
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL

            
OCESQL*     EXEC SQL
OCESQL*        OPEN C1
OCESQL*     END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_writer_C1" & x"00"
OCESQL     END-CALL.
            IF  SQLSTATE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
   
           MOVE 'F' TO DB_FINISHED_FETCHING_FLAG
   
           EXIT.
       
       B3501_FETCH_ROWS_READ_NEXT.
               
OCESQL*    EXEC SQL
OCESQL*            FETCH C1 INTO :DB_DATE, :DB_STRING
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_DATE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 12
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_STRING
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_writer_C1" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

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
OCESQL*    EXEC SQL COMMIT WORK END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

OCESQL*    EXEC SQL 
OCESQL*        CLOSE C1
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_writer_C1" & x"00"
OCESQL     END-CALL
OCESQL    

OCESQL*    EXEC SQL
OCESQL*        DISCONNECT ALL
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.

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
OCESQL*         EXEC SQL
OCESQL*             ROLLBACK
OCESQL*         END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ROLLBACK" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
             WHEN  OTHER
                DISPLAY "Undefined error"
                DISPLAY "ERRCODE: "  SQLSTATE
                DISPLAY SQLERRMC
          END-EVALUATE.

          MOVE 1 TO RETURN-CODE.

          EXIT.
          EXIT.
          EXIT.
