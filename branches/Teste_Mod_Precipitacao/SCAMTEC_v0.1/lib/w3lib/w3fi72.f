      SUBROUTINE W3FI72(ITYPE,FLD,IFLD,IBITL,
     &                  IPFLAG,ID,PDS,
     &                  IGFLAG,IGRID,IGDS,ICOMP,
     &                  IBFLAG,IBMAP,IBLEN,IBDSFL,
     &                  NPTS,KBUF,ITOT,JERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FI72        MAKE A COMPLETE GRIB MESSAGE
C   PRGMMR: FARLEY           ORG: NMC421      DATE:94-11-22
C
C ABSTRACT: MAKES A COMPLETE GRIB MESSAGE FROM A USER SUPPLIED
C   ARRAY OF FLOATING POINT OR INTEGER DATA.  THE USER HAS THE
C   OPTION OF SUPPLYING THE PDS OR AN INTEGER ARRAY THAT WILL BE
C   USED TO CREATE A PDS (WITH W3FI68).  THE USER MUST ALSO
C   SUPPLY OTHER NECESSARY INFO; SEE USAGE SECTION BELOW.
C
C PROGRAM HISTORY LOG:
C   91-05-08  R.E.JONES
C   92-07-01  M. FARLEY    ADDED GDS AND BMS LOGIC.  PLACED EXISTING
C                          LOGIC FOR BDS IN A ROUTINE.
C   92-10-02  R.E.JONES    ADD ERROR EXIT FOR W3FI73
C   93-04-30  R.E.JONES    REPLACE DO LOOPS TO MOVE CHARACTER DATA
C                          WITH XMOVEX, USE XSTORE TO ZERO CHARACTER
C                          ARRAY. MAKE CHANGE SO FLAT FIELD WILL PACK.
C   93-08-06  CAVANAUGH    MODIFIED CALL TO W3FI75
C   93-10-26  CAVANAUGH    ADDED CODE TO RESTORE INPUT FIELD TO ORIGINAL
C                          VALUES IF D-SCALE NOT 0
C   94-01-27  CAVANAUGH    ADDED IGDS ARRAY IN CALL TO W3FI75 TO PROVIDE
C                          INFORMATION FOR BOUSTROPHEDONIC PROCESSING
C   94-03-03  CAVANAUGH    INCREASED SIZE OF GDS ARRAY FOR THIN GRIDS
C   94-05-16  FARLEY       CLEANED UP DOCUMENTATION
C   94-11-10  FARLEY       INCREASED SIZE OF PFLD/IFLD ARRARYS FROM
C                          100K TO 260K FOR .5 DEGREE SST ANAL FIELDS
C   94-12-04  R.E.JONES    CHANGE DOCUMENT FOR IPFLAG.
C   95-10-31  IREDELL      REMOVED SAVES AND PRINTS
C   98-05-19  Gilbert      Increased array dimensions to handle grids
C                          of up to 500,000 grid points.
C   95-10-31  IREDELL      GENERALIZED WORD SIZE
C   98-12-21  Gilbert      Replaced Function ICHAR with mova2i.
C   99-02-01  Gilbert      Changed the method of zeroing out array KBUF.
C                          the old method, using W3FI01 and XSTORE was
C                          incorrect with 4-byte integers and 8-byte reals.
C 2001-06-07  Gilbert      Removed calls to xmovex.
C                          changed IPFLD from integer to character.
C
C USAGE:  CALL W3FI72(ITYPE,FLD,IFLD,IBITL,
C        &            IPFLAG,ID,PDS,
C        &            IGFLAG,IGRID,IGDS,ICOMP,
C        &            IBFLAG,IBMAP,IBLEN,IBDSFL,
C        &            IBDSFL,
C        &            NPTS,KBUF,ITOT,JERR)
C
C   INPUT ARGUMENT LIST:
C     ITYPE    - 0 = FLOATING POINT DATA SUPPLIED IN ARRAY 'FLD'
C                1 = INTEGER DATA SUPPLIED IN ARRAY 'IFLD'
C     FLD      - REAL ARRAY OF DATA (AT PROPER GRIDPOINTS) TO BE
C                CONVERTED TO GRIB FORMAT IF ITYPE=0.
C                SEE REMARKS #1 & 2.
C     IFLD     - INTEGER ARRAY OF DATA (AT PROPER GRIDPOINTS) TO BE
C                CONVERTED TO GRIB FORMAT IF ITYPE=1.
C                SEE REMARKS #1 & 2.
C     IBITL    - 0 = COMPUTER COMPUTES LENGTH FOR PACKING DATA FROM
C                    POWER OF 2 (NUMBER OF BITS) BEST FIT OF DATA
C                    USING 'VARIABLE' BIT PACKER W3FI58.
C                8, 12, ETC. COMPUTER RESCALES DATA TO FIT INTO THAT
C                    'FIXED' NUMBER OF BITS USING W3FI59.
C                SEE REMARKS #3.
C
C     IPFLAG   - 0 = MAKE PDS FROM USER SUPPLIED ARRAY (ID)
C                1 = USER SUPPLYING PDS
C                NOTE: IF PDS IS GREATER THAN 30, USE IPLFAG=1.
C                THE USER COULD CALL W3FI68 BEFORE HE CALLS
C                W3FI72. THIS WOULD MAKE THE FIRST 30 BYTES OF
C                THE PDS, USER THEN WOULD MAKE BYTES AFTER 30.
C     ID       - INTEGER ARRAY OF  VALUES THAT W3FI68 WILL USE
C                TO MAKE AN EDITION 1 PDS IF IPFLAG=0.  (SEE THE
C                DOCBLOCK FOR W3FI68 FOR LAYOUT OF ARRAY)
C     PDS      - CHARACTER ARRAY OF VALUES (VALID PDS SUPPLIED
C                BY USER) IF IPFLAG=1. LENGTH MAY EXCEED 28 BYTES
C                (CONTENTS OF BYTES BEYOND 28 ARE PASSED
C                THROUGH UNCHANGED).
C
C     IGFLAG   - 0 = MAKE GDS BASED ON 'IGRID' VALUE.
C                1 = MAKE GDS FROM USER SUPPLIED INFO IN 'IGDS'
C                    AND 'IGRID' VALUE.
C                SEE REMARKS #4.
C     IGRID    - #   = GRID IDENTIFICATION (TABLE B)
C                255 = IF USER DEFINED GRID; IGDS MUST BE SUPPLIED
C                      AND IGFLAG MUST =1.
C     IGDS     - INTEGER ARRAY CONTAINING USER GDS INFO (SAME
C                FORMAT AS SUPPLIED BY W3FI71 - SEE DOCKBLOCK FOR
C                LAYOUT) IF IGFLAG=1.
C     ICOMP    - RESOLUTION AND COMPONENT FLAG FOR BIT 5 OF GDS(17)
C                0 = EARTH ORIENTED WINDS
C                1 = GRID ORIENTED WINDS
C
C     IBFLAG   - 0 = MAKE BIT MAP FROM USER SUPPLIED DATA
C                # = BIT MAP PREDEFINED BY CENTER
C                SEE REMARKS #5.
C     IBMAP    - INTEGER ARRAY CONTAINING BIT MAP
C     IBLEN    - LENGTH OF BIT MAP WILL BE USED TO VERIFY LENGTH
C                OF FIELD (ERROR IF IT DOESN'T MATCH).
C
C     IBDSFL   - INTEGER ARRAY CONTAINING TABLE 11 FLAG INFO
C                BDS OCTET 4:
C                (1) 0 = GRID POINT DATA
C                    1 = SPHERICAL HARMONIC COEFFICIENTS
C                (2) 0 = SIMPLE PACKING
C                    1 = SECOND ORDER PACKING
C                (3) ... SAME VALUE AS 'ITYPE'
C                    0 = ORIGINAL DATA WERE FLOATING POINT VALUES
C                    1 = ORIGINAL DATA WERE INTEGER VALUES
C                (4) 0 = NO ADDITIONAL FLAGS AT OCTET 14
C                    1 = OCTET 14 CONTAINS FLAG BITS 5-12
C                (5) 0 = RESERVED - ALWAYS SET TO 0
C         BYTE 6 OPTION 1 NOT AVAILABLE (AS OF 5-16-93)
C                (6) 0 = SINGLE DATUM AT EACH GRID POINT
C                    1 = MATRIX OF VALUES AT EACH GRID POINT
C         BYTE 7 OPTION 0 WITH SECOND ORDER PACKING N/A (AS OF 5-16-93)
C                (7) 0 = NO SECONDARY BIT MAPS
C                    1 = SECONDARY BIT MAPS PRESENT
C                (8) 0 = SECOND ORDER VALUES HAVE CONSTANT WIDTH
C                    1 = SECOND ORDER VALUES HAVE DIFFERENT WIDTHS
C
C   OUTPUT ARGUMENT LIST:
C     NPTS     - NUMBER OF GRIDPOINTS IN ARRAY FLD OR IFLD
C     KBUF     - ENTIRE GRIB MESSAGE ('GRIB' TO '7777')
C                EQUIVALENCE TO INTEGER ARRAY TO MAKE SURE IT
C                IS ON WORD BOUNARY.
C     ITOT     - TOTAL LENGTH OF GRIB MESSAGE IN BYTES
C     JERR     - = 0, COMPLETED MAKING GRIB FIELD WITHOUT ERROR
C                  1, IPFLAG NOT 0 OR 1
C                  2, IGFLAG NOT 0 OR 1
C                  3, ERROR CONVERTING IEEE F.P. NUMBER TO IBM370 F.P.
C                  4, W3FI71 ERROR/IGRID NOT DEFINED
C                  5, W3FK74 ERROR/GRID REPRESENTATION TYPE NOT VALID
C                  6, GRID TOO LARGE FOR PACKER DIMENSION ARRAYS
C                     SEE AUTOMATION DIVISION FOR REVISION!
C                  7, LENGTH OF BIT MAP NOT EQUAL TO SIZE OF FLD/IFLD
C                  8, W3FI73 ERROR, ALL VALUES IN IBMAP ARE ZERO
C
C   OUTPUT FILES:
C     FT06F001 - STANDARD FORTRAN OUTPUT PRINT FILE
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3LIB    - W3FI58, W3FI59, W3FI68, W3FI71, W3FI73, W3FI74
C                  W3FI75, W3FI76
C       FORTRAN 90 INTRINSIC - BIT_SIZE
C
C REMARKS:
C   1)  IF BIT MAP TO BE INCLUDED IN MESSAGE, NULL DATA SHOULD
C       BE INCLUDED IN FLD OR IFLD.  THIS ROUTINE WILL TAKE CARE
C       OF 'DISCARDING' ANY NULL DATA BASED ON THE BIT MAP.
C   2)  UNITS MUST BE THOSE IN GRIB DOCUMENTATION:  NMC O.N. 388
C       OR WMO PUBLICATION 306.
C   3)  IN EITHER CASE, INPUT NUMBERS WILL BE MULTIPLIED BY
C       '10 TO THE NTH' POWER FOUND IN ID(25) OR PDS(27-28),
C       THE D-SCALING FACTOR, PRIOR TO BINARY PACKING.
C   4)  ALL NMC PRODUCED GRIB FIELDS WILL HAVE A GRID DEFINITION
C       SECTION INCLUDED IN THE GRIB MESSAGE.  ID(6) WILL BE
C       SET TO '1'.
C       - GDS WILL BE BUILT BASED ON GRID NUMBER (IGRID), UNLESS
C         IGFLAG=1 (USER SUPPLYING IGDS).  USER MUST STILL SUPPLY
C         IGRID EVEN IF IGDS PROVIDED.
C   5)  IF BIT MAP USED THEN ID(7) OR PDS(8) MUST INDICATE THE
C       PRESENCE OF A BIT MAP.
C   6)  ARRAY KBUF SHOULD BE EQUIVALENCED TO AN INTEGER VALUE OR
C       ARRAY TO MAKE SURE IT IS ON A WORD BOUNDARY.
C   7)  SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
C
      REAL            FLD(*)
C
      INTEGER         IBDSFL(*)
      INTEGER         IBMAP(*)
      INTEGER         ID(*)
      INTEGER         IFLD(*)
      INTEGER         IGDS(*)
      INTEGER         IB(4)
C
      CHARACTER * 1   BDS11(11)
      CHARACTER * 1   KBUF(*)
      CHARACTER * 1   PDS(*)
      CHARACTER * 1   GDS(200)
      CHARACTER(1),ALLOCATABLE:: BMS(:)
      CHARACTER(1),ALLOCATABLE:: PFLD(:)
      CHARACTER(1),ALLOCATABLE:: IPFLD(:)
      CHARACTER * 1   SEVEN
      CHARACTER * 1   ZERO
C
C
C   ASCII REP OF  /'G', 'R', 'I', 'B'/
C
      DATA  IB    / 71,  82,  73,  66/
C
      IER    = 0
      IBERR  = 0
      JERR   = 0
      IGRIBL = 8
      IPDSL  = 0
      LENGDS = 0
      LENBMS = 0
      LENBDS = 0
      ITOSS  = 0
C
C$           1.0   PRODUCT DEFINITION SECTION(PDS).
C
C   SET ID(6) TO 1 ...OR... MODIFY PDS(8) ...
C      REGARDLESS OF USER SPECIFICATION...
C   NMC GRIB FIELDS WILL ALWAYS HAVE A GDS
C
      IF (IPFLAG .EQ.0) THEN
        ID(6) = 1
        CALL W3FI68(ID,PDS)
      ELSE IF (IPFLAG .EQ. 1) THEN
        IF (IAND(mova2i(PDS(8)),64) .EQ. 64) THEN
C         BOTH GDS AND BMS
          PDS(8) = CHAR(192)
        ELSE IF (mova2i(PDS(8)) .EQ. 0) THEN
C         GDS ONLY
          PDS(8) = CHAR(128)
        END IF
        CONTINUE
      ELSE
C       PRINT *,' W3FI72 ERROR, IPFLAG IS NOT 0 OR 1 IPFLAG = ',IPFLAG
        JERR = 1
        GO TO 900
      END IF
C
C     GET LENGTH OF PDS
C
      IPDSL = mova2i(PDS(1)) * 65536 + mova2i(PDS(2)) * 256 +
     &        mova2i(PDS(3))
C
C$           2.0   GRID DEFINITION SECTION (GDS).
C
C     IF IGFLAG=1 THEN USER IS SUPPLYING THE IGDS INFORMATION
C
      IF (IGFLAG .EQ. 0) THEN
        CALL W3FI71(IGRID,IGDS,IGERR)
        IF (IGERR .EQ. 1) THEN
C         PRINT *,' W3FI71 ERROR, GRID TYPE NOT DEFINED...',IGRID
          JERR = 4
          GO TO 900
        END IF
      END IF
      IF (IGFLAG .EQ. 0  .OR.  IGFLAG .EQ.1) THEN
        CALL W3FI74(IGDS,ICOMP,GDS,LENGDS,NPTS,IGERR)
        IF (IGERR .EQ. 1) THEN
C         PRINT *,' W3FI74 ERROR, GRID REP TYPE NOT VALID...',IGDS(3)
          JERR = 5
          GO TO 900
        ELSE
        END IF
      ELSE
C       PRINT *,' W3FI72 ERROR, IGFLAG IS NOT 0 OR 1 IGFLAG = ',IGFLAG
        JERR = 2
        GO TO 900
      END IF
C
C$           3.0   BIT MAP SECTION (BMS).
C
C     SET ITOSS=1 IF BITMAP BEING USED.  W3FI75 WILL TOSS DATA
C     PRIOR TO PACKING.  LATER CODING WILL BE NEEDED WHEN THE
C     'PREDEFINED' GRIDS ARE FINALLY 'DEFINED'.
C
      IF (mova2i(PDS(8)) .EQ. 64 .OR.
     &    mova2i(PDS(8)) .EQ. 192)   THEN
        ITOSS = 1
        IF (IBFLAG .EQ. 0) THEN
          IF (IBLEN .NE. NPTS) THEN
C           PRINT *,' W3FI72 ERROR, IBLEN .NE. NPTS = ',IBLEN,NPTS
            JERR = 7
            GO TO 900
          END IF
          ALLOCATE(BMS(NPTS/8+6))
          CALL W3FI73(IBFLAG,IBMAP,IBLEN,BMS,LENBMS,IER)
          IF (IER .NE. 0) THEN
C           PRINT *,' W3FI73 ERROR, IBMAP VALUES ARE ALL ZERO'
            JERR = 8
            GO TO 900
          END IF
        ELSE
C         PRINT *,'   BIT MAP PREDEFINED BY CENTER, IBFLAG = ',IBFLAG
        END IF
      END IF
C
C$           4.0   BINARY DATA SECTION (BDS).
C
C$           4.1   SCALE THE DATA WITH D-SCALE FROM PDS(27-28)
C
      JSCALE = mova2i(PDS(27)) * 256 + mova2i(PDS(28))
      IF (IAND(JSCALE,32768).NE.0) THEN
        JSCALE = - IAND(JSCALE,32767)
      END IF
      SCALE  = 10.0 ** JSCALE
      IF (ITYPE .EQ. 0) THEN
        DO 410 I = 1,NPTS
          FLD(I) = FLD(I) * SCALE
  410   CONTINUE
      ELSE
        DO 411 I = 1,NPTS
          IFLD(I) = NINT(FLOAT(IFLD(I)) * SCALE)
  411   CONTINUE
      END IF
C
C$           4.2   CALL W3FI75 TO PACK DATA AND MAKE BDS.
C
      ALLOCATE(PFLD(NPTS*4))
C
      IF(IBDSFL(2).NE.0) THEN
        ALLOCATE(IPFLD(NPTS*4))
        IPFLD=char(0)
      ENDIF
C
      CALL W3FI75(IBITL,ITYPE,ITOSS,FLD,IFLD,IBMAP,IBDSFL,
     &         NPTS,BDS11,IPFLD,PFLD,LEN,LENBDS,IBERR,PDS,IGDS)
C
      IF(IBDSFL(2).NE.0) THEN
C        CALL XMOVEX(PFLD,IPFLD,NPTS*4)
         do ii = 1, NPTS*4
            PFLD(ii) = IPFLD(ii)
         enddo
        DEALLOCATE(IPFLD)
      ENDIF
C
        IF (IBERR .EQ. 1) THEN
          JERR = 3
          GO TO 900
        END IF
C            4.3   IF D-SCALE NOT 0, RESCALE INPUT FIELD TO
C                   ORIGINAL VALUE
C
      IF (JSCALE.NE.0) THEN
          DSCALE = 1.0 / SCALE
          IF (ITYPE.EQ.0) THEN
              DO 412 I = 1, NPTS
                  FLD(I)  = FLD(I) * DSCALE
  412         CONTINUE
          ELSE
              DO 413 I = 1, NPTS
                  FLD(I)  = NINT(FLOAT(IFLD(I)) * DSCALE)
  413         CONTINUE
          END IF
      END IF
C
C$           5.0   OUTPUT SECTION.
C
C$           5.1   ZERO OUT THE OUTPUT ARRAY KBUF.
C
      ZERO    = CHAR(00)
      ITOT    = IGRIBL + IPDSL + LENGDS + LENBMS + LENBDS + 4
C     PRINT *,'IGRIBL  =',IGRIBL
C     PRINT *,'IPDSL   =',IPDSL
C     PRINT *,'LENGDS  =',LENGDS
C     PRINT *,'LENBMS  =',LENBMS
C     PRINT *,'LENBDS  =',LENBDS
C     PRINT *,'ITOT    =',ITOT
      KBUF(1:ITOT)=ZERO
C
C$           5.2   MOVE SECTION 0 - 'IS' INTO KBUF (8 BYTES).
C
      ISTART  = 0
      DO 520 I = 1,4
        KBUF(I) = CHAR(IB(I))
  520 CONTINUE
C
      KBUF(5) = CHAR(MOD(ITOT / 65536,256))
      KBUF(6) = CHAR(MOD(ITOT /   256,256))
      KBUF(7) = CHAR(MOD(ITOT        ,256))
      KBUF(8) = CHAR(1)
C
C$           5.3   MOVE SECTION 1 - 'PDS' INTO KBUF (28 BYTES).
C
      ISTART  = ISTART + IGRIBL
      IF (IPDSL.GT.0) THEN
C        CALL XMOVEX(KBUF(ISTART+1),PDS,IPDSL)
         do ii = 1, IPDSL
            KBUF(ISTART+ii) = PDS(ii)
         enddo
      ELSE
C       PRINT *,'LENGTH OF PDS LESS OR EQUAL 0, IPDSL = ',IPDSL
      END IF
C
C$           5.4   MOVE SECTION 2 - 'GDS' INTO KBUF.
C
      ISTART  = ISTART + IPDSL
      IF (LENGDS .GT. 0) THEN
C        CALL XMOVEX(KBUF(ISTART+1),GDS,LENGDS)
         do ii = 1, LENGDS
            KBUF(ISTART+ii) = GDS(ii)
         enddo
      END IF
C
C$           5.5   MOVE SECTION 3 - 'BMS' INTO KBUF.
C
      ISTART  = ISTART + LENGDS
      IF (LENBMS .GT. 0) THEN
C        CALL XMOVEX(KBUF(ISTART+1),BMS,LENBMS)
         do ii = 1, LENBMS
            KBUF(ISTART+ii) = BMS(ii)
         enddo
      END IF
C
C$           5.6   MOVE SECTION 4 - 'BDS' INTO KBUF.
C
C$                 MOVE THE FIRST 11 OCTETS OF THE BDS INTO KBUF.
C
      ISTART  = ISTART + LENBMS
C      CALL XMOVEX(KBUF(ISTART+1),BDS11,11)
      do ii = 1, 11
         KBUF(ISTART+ii) = BDS11(ii)
      enddo
C
C$                 MOVE THE PACKED DATA INTO THE KBUF
C
      ISTART  = ISTART + 11
      IF (LEN.GT.0) THEN
C        CALL XMOVEX(KBUF(ISTART+1),PFLD,LEN)
         do ii = 1, LEN
            KBUF(ISTART+ii) = PFLD(ii)
         enddo
      END IF
C
C$                 ADD '7777' TO END OFF KBUF
C   NOTE THAT THESE 4 OCTETS NOT INCLUDED IN ACTUAL SIZE OF BDS.
C
      SEVEN  = CHAR(55)
      ISTART = ITOT - 4
      DO 562 I = 1,4
        KBUF(ISTART+I) = SEVEN
 562  CONTINUE
C
 900  CONTINUE
      IF(ALLOCATED(BMS)) DEALLOCATE(BMS)
      IF(ALLOCATED(PFLD)) DEALLOCATE(PFLD)
      RETURN
      END