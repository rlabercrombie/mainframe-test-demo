       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-REPORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEMO_REPORT ASSIGN TO 
                "../src/resources/out/DEMO_REPORT.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD DEMO_REPORT.
           01 DEMO_REPORT_RECORD     PIC X(72).

       WORKING-STORAGE SECTION.
       *> ===========================================================
       *> This program reads from a database to generate a report

       *> example argument usage:

       *> run live program
       *>      ./demo_report
       
       *> ===========================================================

       01 WS_CURRENT_DATE_DATA.
         05  WS_CURRENT_DATE.
             10  WS_CURRENT_YEAR         PIC 9(04).
             10  WS_CURRENT_MONTH        PIC 9(02).
             10  WS_CURRENT_DAY          PIC 9(02).
         05  WS_CURRENT_TIME.
             10  WS_CURRENT_HOURS        PIC 9(02).
             10  WS_CURRENT_MINUTE       PIC 9(02).
             10  WS_CURRENT_SECOND       PIC 9(02).
             10  WS_CURRENT_MILLISECONDS PIC 9(02).

       01 WS_COUNTER                     PIC 9(5) VALUE ZERO.

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

       *> report working storage
       01  WS_RPT_TITLE.
           05  FILLER                     PIC X(31) VALUE SPACES.
           05  FILLER                     PIC X(11) VALUE "DEMO REPORT".
           05  FILLER                     PIC X(30) VALUE SPACES.
       01  WS_RPT_SUBTITLE.
           05  FILLER                       PIC X(31) VALUE SPACES.
           05  WS_RPT_SUBTITLE_DATE         PIC X(10).
           05  FILLER                       PIC X(31) VALUE SPACES.
       01  WS_RPT_LINE_BREAK.
           05  WS_RPT_LINE_BREAK_SPACES     PIC X(72) VALUE ALL SPACES.
           05  WS_RPT_LINE_STARS            PIC X(72) VALUE ALL "*".
       01  WS_RPT_DATA.
           05  WS_RPT_DATA_DATE.
               10  WS_RPT_DATA_DATE_MM      PIC X(02).
               10  FILLER                   PIC X(01) VALUE "/".
               10  WS_RPT_DATA_DATE_DD      PIC X(02).
               10  FILLER                   PIC X(01) VALUE "/".
               10  WS_RPT_DATA_DATE_YYYY    PIC X(04).
           05  FILLER                       PIC X(05) VALUE "  -  ".
           05  WS_RPT_DATA_SWITCH           PIC X(01).
           05  FILLER                       PIC X(05) VALUE "  -  ".
           05  WS_RPT_DATA_COUNTER          PIC X(03).
       01  WS_RPT_SUMMARY.
           05  FILLER                 PIC X(30) VALUE SPACES.
           05  FILLER                 PIC X(15) VALUE "TOTAL RECORDS: ".
           05  WS_RPT_SUMMARY_COUNT   PIC X(05).

       PROCEDURE DIVISION.

           MOVE "postgres@db:5432" TO DBNAME
           MOVE "postgres"         TO USERNAME
           MOVE "postgres"         TO PASSWD


           MOVE FUNCTION CURRENT-DATE TO WS_CURRENT_DATE_DATA.

           PERFORM B1000_GENERAL_LOGIC

           STOP RUN.

       B1000_GENERAL_LOGIC.
           *> SETUP 
           PERFORM B3100_CONNECT.
           OPEN OUTPUT DEMO_REPORT.

           *> GENERAL LOGIC 
           PERFORM B5000_INITIALIZE_REPORT.
           PERFORM B4000_READ_DB_ENTRIES_INTO_REPORT.

           PERFORM B5100_WRITE_REPORT_SUMMARY.

           *> FINISH UP
           PERFORM B3900_DISCONNECT.
           CLOSE DEMO_REPORT.

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

            STRING WS_CURRENT_YEAR DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                WS_CURRENT_MONTH DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                WS_CURRENT_DAY DELIMITED BY SIZE 
            INTO DB_DATE
            
            *> DECLARE CURSOR
            EXEC SQL
                DECLARE C1 CURSOR FOR
                    SELECT report_date, report_text
                    FROM demo_table
                    WHERE report_date = (:DB_DATE)
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
           ELSE
               ADD 1 TO WS_COUNTER

           END-IF
       
           EXIT.
       
       B3900_DISCONNECT.
           EXEC SQL 
               CLOSE C1
           END-EXEC

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

           EXIT.

       *> DATA LAYER
       B4000_READ_DB_ENTRIES_INTO_REPORT.

           PERFORM B3500_FETCH_ROWS_INIT
           PERFORM UNTIL DB_FINISHED_FETCHING 

               PERFORM B3501_FETCH_ROWS_READ_NEXT 
               
               IF NOT DB_FINISHED_FETCHING
                  PERFORM B4100_PROCESS_DB_RECORD
               END-IF

           END-PERFORM

           EXIT.

       B4100_PROCESS_DB_RECORD.

           PERFORM B5100_WRITE_REPORT_RECORD.
           MOVE SPACES TO DEMO_REC.

           EXIT.

       *> REPORT WRITING LOGIC
       B5000_INITIALIZE_REPORT.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_TITLE.

           STRING WS_CURRENT_YEAR DELIMITED BY SIZE,
               '-' DELIMITED BY SIZE,
               WS_CURRENT_MONTH DELIMITED BY SIZE,
               '-' DELIMITED BY SIZE,
               WS_CURRENT_DAY DELIMITED BY SIZE 
           INTO WS_RPT_SUBTITLE_DATE.

           WRITE DEMO_REPORT_RECORD FROM WS_RPT_SUBTITLE.

           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_STARS.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.

           EXIT.

       B5100_WRITE_REPORT_RECORD.
           
           PERFORM B5101_CREATE_RPT_REC_FROM_DEMO_DATA.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_DATA.

           EXIT.

       B5101_CREATE_RPT_REC_FROM_DEMO_DATA.

           MOVE DEMO_DATE_MM TO WS_RPT_DATA_DATE_MM.
           MOVE DEMO_DATE_DD TO WS_RPT_DATA_DATE_DD.
           MOVE DEMO_DATE_YYYY TO WS_RPT_DATA_DATE_YYYY.
           MOVE DEMO_STRING_SWITCH TO WS_RPT_DATA_SWITCH.
           MOVE DEMO_STRING_COUNTER TO WS_RPT_DATA_COUNTER.

           EXIT.

       B5100_WRITE_REPORT_SUMMARY.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_STARS.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.

           PERFORM B5101_CREATE_SUMMARY_LINE.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_SUMMARY.
           EXIT.

       B5101_CREATE_SUMMARY_LINE.
           MOVE WS_COUNTER TO WS_RPT_SUMMARY_COUNT
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
