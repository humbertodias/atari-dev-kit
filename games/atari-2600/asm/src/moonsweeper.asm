;
;MAY 16, 1983
;
; PAL 5/26
; REV A 5/26/83
;
;       ZONE    AKA     MOONSWEEPER
;
; COPYRIGHT 1982/83 BY IMAGIC
;
; GAME DESIGN BY ROBERT GRAVES SMITH III
;
; GRAPHICS DESIGN BY MICHAEL BECKER AND WILLY AGUILAR
;
;OCTOBER 22, 1982

    processor 6502

FREE_BYTES SET 0   
   MAC BOUNDRY
      REPEAT 256
         IF <. % {1} = 0
            MEXIT
         ELSE
FREE_BYTES SET FREE_BYTES + 1
            DC $00
         ENDIF
      REPEND
   ENDM
         
;
; EQUATES
;
PAL         EQU     0                    ;IF NON-ZERO ASSEMBLE PALL VERSION
PROM        EQU     1                    ;NON-ZERO FOR ROM OR EPROM
RELEASE     EQU     1                    ;NON-ZERO FOR RELEASE VERSIONS
REVA        EQU     1                    ;NON-ZERO FOR REVA
;
;
MAXCXS      EQU     3                    ;NUMBER OF NO EFFECT CXS/FRAME
HITTOWER    EQU     1                    ;HIT TOWER
NOLIVES     EQU     3                    ;LIVES START
GRX         EQU     1                    ;PRINT FLAG
HORIZON     EQU     $9F
SHUTHGT     EQU     59
LOWHGT      EQU     94+4                 ;HEIGHT OF LOWER KERNELS
HLIMIT      EQU     2 << 3
MIDDLE      EQU     $63
MAXSCR      EQU     4 << 3               ;MAX SCROLL RATE
CXWINDY     EQU     $10
CXWINDX     EQU     12
LOAD        EQU     6                    ;MAX LOAD OF PASSENGERS
;
; SOUND EQUATES
;
FIRES       EQU     0
BONSND      EQU     1
MANSND      EQU     2
EXSND       EQU     3
DEATHS      EQU     4
EOG         EQU     5
LANDSND     EQU     6
;
; STELLA EQUATES
;
    
VSYNC       EQU      $00         ; VERT SYNC
VBLNK       EQU      $01         ;
HSYNC       EQU      $02         ; STROBE
MCRES       EQU      $03         ; MASTER COUNTER RESET
P0SIZE      EQU      $04         ; SIZE, NUMBER OF COPIES
P1SIZE      EQU      $05         ;
P0CLR       EQU      $06         ; COLOR
P1CLR       EQU      $07         ;
PFCLR       EQU      $08         ;
BKCLR       EQU      $09
PFCTRL      EQU      $0A         ; PLAYFIELD CONTROL

P0REF       EQU      $0B         ; REFLECT
P1REF       EQU      $0C         ;

PF0         EQU      $0D         ; PLAYFIELD REGISTERS
PF1         EQU      $0E         ;
PF2         EQU      $0F         ;

P0RES       EQU      $10         ; RESETS
P1RES       EQU      $11         ;
M0RES       EQU      $12         ;
M1RES       EQU      $13         ;
BRES        EQU      $14         ;

SNDC0       EQU      $15         ; SOUND CONTROL
SNDC1       EQU      $16         ;
SNDF0       EQU      $17         ; FREQUENCY
SNDF1       EQU      $18         ;
SNDV0       EQU      $19         ; VOLUME
SNDV1       EQU      $1A         ;

P0GR        EQU      $1B         ; GRAPHICS REGISTERS
P1GR        EQU      $1C         ;
M0GR        EQU      $1D         ;
M1GR        EQU      $1E         ;
BGR         EQU      $1F         ;

P0HM        EQU      $20         ; HMOVES
P1HM        EQU      $21
M0HM        EQU      $22
M1HM        EQU      $23
BHM         EQU      $24         ;

P0VDEL      EQU      $25         ; VERTICAL DELAYS
P1VDEL      EQU      $26         ;
BVDEL       EQU      $27         ;

MP0RES      EQU      $28         ; MISSILE TRACKING
MP1RES      EQU      $29         ;
HMOV        EQU      $2A         ; HORIZ MOVEMENT STROBE
CLRHM       EQU      $2B         ; CLEAR HMOVE STROBE
CCLR        EQU      $2C         ; CX REGISTER RESET STROBE

;READ ADDRESSES

M0PPCX      EQU      $00         ; M0 PLAYER CX
M1PPCX      EQU      $01         ; M1 PLAYER
P0BPCX      EQU      $02         ; P0 BALL/PLAYFIELD CX
P1BPCX      EQU      $03         ;
M0BPCX      EQU      $04         ; M0 BALL/PLAYFIELD CX
M1BPCX      EQU      $05         ;
BPCX        EQU      $06         ; BALL PLAYFIELD CX
PPMMCX      EQU      $07         ; P0/P1 MISSILE CX
POT0A       EQU      $08         ; POT IN, LEFT
POT0B       EQU      $09         ; POT IN, LEFT
POT1A       EQU      $0A         ; POT IN, RIGHT
POT1B       EQU      $0B         ; POT IN, RIGHT
TRIG0       EQU      $0C         ; SCHMIDT TRIGGER LEFT
TRIG1       EQU      $0D         ; RIGHT

; PIA ADDRESSES

PLRCTL      EQU      $280        ; PORT A (CONTROLLERS)
DDRA        EQU      $281        ; DATA DIRECTION A
FPCTL       EQU      $282        ; FRONT PANEL SWITCHES
DDRB        EQU      $283        ; DATA DRIECTION B
TIMER       EQU      $284        ; PIA TIMER
TIMOUT      EQU      $285        ; PIA TIMER FLAG
TIMD1       EQU      $294        ; CLOCK / 1
TIMD8       EQU      $295        ; CLOCK / 8
TIMD64      EQU      $296        ; CLOCK / 64
TIMD1K      EQU      $297        ; CLOCK / 1024
;
;       GAME EQUATES
;
RESLEN   EQU  3
C        EQU   0
L        EQU   $F0
R        EQU   $10
U        EQU   $0F
D        EQU   $01
UR       EQU   $1F
UL       EQU   $FF
DL       EQU   $F1
DR       EQU   $11
;
LBANK    EQU   $1FF8
HBANK    EQU   $1FF9
;
;
;           RAM ALLOCATION
;
   SEG.U RAM
   ORG   $80
;
;
;
HSCROLL  DS       1
RAND     DS       1
RANDH    DS       1
ATTRACT  DS       1
SELTIME  DS       1
GAMESTAT DS       1                       ;D7 RESET=CASTLE SCREEN, D6 RESET=ATTR
FLAGS    DS       1                       ;D7=0=LOGO, D6=1,SHOW VERSION NO.
                                          ;D0=RESET SWITCH
GAMNUM   DS       1                       ; GAME NUMBER
LIVES    DS       2                       ;LIFE COUNTERS
MODE     DS       1                       ;0= BIRTH TOWER OR CRYSTAL
                                          ;1= RINGS
                                          ;2= TOWERS
                                          ;3= SAUCERS ONLY
                                          ;D7 RESET= BIRTH ONLY OBJB
ZSTART
PUCNT    DS       1                       ;PICK UP COUNTER
WAVENO   DS       1                       ;WAVE NUMBER
SCOREL0  DS       2                       ;LO SCORE
SCOREM0  DS       2                       ;MID SCORE
SCOREH0  DS       2                       ;HI SCORE
PLAYNO   DS       1                       ;PLAYER NUMBER
PX       DS       1
SHUTX    DS       1
OBJAX    DS       1                       ;OBJECT ON LOWER SCREEN
OBJBX    DS       1
LASERX   DS       1
JETAX    DS       1                       ;JET XPOS
JETBX    DS       1
;
SCRATE   DS       1                       ;SCROLL RATE FOR LOWER SCREEN
XX       DS       1                       ;THE MYSTERIOUS XX
PLANETCX DS       1
WAVEINC  DS       1                       ;3 = INC GAME NUMBER
;
SND0     DS       1
SND1     DS       1
SND0TIME DS       1
SND1TIME DS       1
NOTE     DS       1                       ;POINT TO NOTE FOR EOG
EXIT0    DS       1                       ;EXIT FROM SHUTTLE SCREEN
SHUTDELX DS       1
OBJADELX DS       1                       ;OBJECT ON LOWER SCREEN
OBJBDELX DS       1
LASDELX  DS       1
JETADELX DS       1                       ;JET XPOS
;
EXIT1    DS       1                       ;EXIT FROM MAIN KERNEL
SHUTDELY DS       1
OBJADELY DS       1                       ;OBJST ON LOWER SCREEN
OBJBDELY DS       1
LASDELY  DS       1
JETADELY DS       1                       ;JET XPOS
;
ZEND
;
;
PY       DS       1
SHUTY    DS       1
OBJAY    DS       1                       ;OBJECT ON LOWER SCREEN
OBJBY    DS       1
LASERY   DS       1
JETAY    DS       1                       ;JET XPOS
;
PID      DS       1
SHUTID   DS       1
OBJAID   DS       1                       ;OBJECT ON LOWER SCREEN
OBJBID   DS       1
TX       DS       1                       ;LASERID
JETAID   DS       1                       ;JET XPOS
JETBID   DS       1
;
PSY      DS       1                       ;PLAYER SCREEN Y
SHUTSY   DS       1                       ;SHUTTLE SCREEN Y
DIFF     DS       1
P1SHM    DS       1                       ;HMOV FOR LOWER SCREEN P1
BCOLOR   DS       1                       ;COLOR FOR BACKGROUND
;
;
REFLS    DS       1                       ;D0     OBJECT B
                                          ;D1     SHUTTLE
                                          ;D2     OBJECT A
                                          ;D3     PLAYER
                                          ;D4     JET A
                                          ;D5     JET B
T0       DS       1
T1       DS       1
T2       DS       1
NECOUNT                                   ;NO EFFECT CX COUNTER
T3       DS       1
T5       DS       1
T6       DS       1
;
OFFSET   DS       1
FRAME    DS       2                       ; 16 BIT FRAME COUNTER
SIXIND0
JTIND    DS       2                       ;JET B INDIRECTION
SIXIND1
JTINDCLR DS       2
SIXIND2
JETIND   DS       2                       ;GRX INDIRECTION FOR JET A
T8
SIXIND3
JETINDCL DS       2                       ;COLOR INDIRECTION FOR JET A
SIXIND4
P0IND    DS       2
SIXIND5
P0INDCLR DS       2
P1IND    DS       2
P1INDCLR DS       2
P0Y      DS       1
P1Y      DS       1
P0TOP    DS       1
P1TOP    DS       1
BKCNT    DS       1
CLRPNT   DS       2
ENTER    DS       1                       ;SHUTTLE START
PNTS     DS       1
;
LIST     DS       13
LHGT     DS       1
RADARX   DS       1
SCREEN   DS       1                       ;D7 SET = SPACE SCREEN
DSTAT    DS       1
T7       DS       1
DEBOUNCE DS       1                       ;FIRE DEBOUNCE
GAMSHAD  DS       1                       ;GAME NUMBER SHADOW
T9       DS       1                       ;YET ANOTHER LAST MINUTE TEMP
T4       DS       1
XTRALIFE DS       1                       ;XTRA LIVES AVAILABLE

;
;  BANK 0
;
   seg code
         ORG      $1000

         STA      HBANK
         JMP      START
         IF       PROM
JMPHI    STA      HBANK
         ELSE
JMPHI    JMP      BANK1
         ENDIF
BANK0
;
;  SET UP LINE COUNTERS
;
         LDA      #0
         SEC
         SBC      DIFF
         PHA
         CLC
         ADC      XX
         STA      EXIT1                   ;EXIT FROM MAIN KERNEL
         PLA
       
         CLC
         ADC      #2+LOWHGT
         BIT      PNTS
         BMI      SKIP01
        CLC
        ADC     #3
SKIP01  STA     EXIT0                       ;EXIT FROM SHUTTLE KERNEL
        LDA     PSY
        SEC
        SBC     DIFF
        STA     PSY
        LDA     SHUTSY
        SEC
        SBC     DIFF
        STA     SHUTSY
;
; SET UP SCORE OR VERSION NUMBER
;
        BIT     FLAGS
        BMI     NOLOGO
        LDA     #$EF
        STA     SCOREL0
        LDA     #$CD
        STA     SCOREM0
        LDA     #$AB
        STA     SCOREH0
;SET UP VERSION NUMBER
NOLOGO  BVC     OUT09
        LDA     #0
        STA     PLAYNO
        LDA     GAMNUM
        AND     #1
        LSR
        STA     SCOREM0                     ;ZERO MID SCORE
        ADC     #$01
        STA     SCOREL0
        LDA     GAMNUM
        LSR
        TAX
        INX
        STX     SCOREH0
OUT09
;SCORE
        LDA     #4                          ;WHICH SCORE ?
        CLC
        ADC     PLAYNO
        TAY
        LDA     #<SPACE
        STA     T2                          ;STORE SPACE FOR ZPRESS
        
        LDA     #>NUMBERS
        STA     SIXIND0+1
        STA     SIXIND0+3
        STA     SIXIND0+5
        STA     SIXIND0+7
        STA     SIXIND0+9
        STA     SIXIND0+11
        
        STA     CLRHM
;
; WRITE SCORE INDIRECTION
;
        LDX     #11
SIXLOOP DEX
        LDA     SCOREL0,Y
        STY     T0                          ;HOLD INDEX
        PHA                                 ;SAVE SCORE BYTE
        AND     #$F0
        LSR
        ADC     #<N0
        CMP     #<N0
        BNE     SKIP89                      ;NON ZERO
        
        LDA     T2
        BNE     SKIP88
SKIP89  LDY     #<N0
        STY     T2                          ;MAKE T2 "0"
SKIP88  STA     SIXIND0,X
        DEX
        PLA
        DEX
        AND     #$F
        ASL
        ASL
        ASL
        ADC     #<N0                      ;CARRY HAS ALWAYS CLEAR
        CMP     #<N0
        BNE     SKIP86
        LDA     T2
        BNE     SKIP85
SKIP86  BIT     FLAGS
        BVS     SKIP85
        LDY     #<N0
        STY     T2
SKIP85
         STA      SIXIND0,X
         DEX
         CPX      #4                      ;ALWAYS STOP ZPRESS
         BCS      SKIP84
         BIT      FLAGS                   ;DONT STOP ZPRESS IF VERSION SHOWN
         BVS      SKIP84
         LDY      #<N0
         STY      T2
SKIP84
         LDY      T0
         DEY
         DEY
         BPL      SIXLOOP
         STA      CCLR
;
; SET UP SHUTTLE IN JET KERNELS
;           DELAY=18, NO FUDGE FACTOR
;
         LDX      T4
         LDA      T5                      ;SHUTID
         CMP      #(VM0 & $FF)/6
         BEQ      SKIP43
         LDX      #0
SKIP43   STA      HSYNC
         LDA      T0
         LDA      XTABLE,X
         STA      M1HM
         AND      #$F
         TAY
         NOP
         NOP
RESEP2   DEY
         BPL      RESEP2
         STA      M1RES
SKRESET
;
; SET UP LASER
;       DELAY=18, NO FUDGE FACTOR
         STA     HSYNC
         LDX     LASERX
         LDA     XTABLE,X
         STA     M0HM
         AND     #$F
         TAY
         NOP
         NOP
RESPL7   DEY
         BPL     RESPL7
         STA     M0RES
;
         STA     HSYNC
         LDY     #1
         LDX     PLAYNO                  ;POINT TO PLAYER UP    
         LDA     SCORECLR,X
         AND     ATTRACT
         STA     P0CLR
         STA     P1CLR
         LDA     #0
         STA     P1REF
         STA     P0REF
         LDA     #3                      ; DO SCORE RESET 
         STA     P1SIZE                  ; TRIPLE COPIES CLOSE
         STA     P0SIZE
         LDA     #$F0
         STA     P1RES                   ;39 MC
         STA     P0RES
         STA     P1HM
         STA     HSYNC                   ; FOR HMOV
         STA     HMOV
         STY     P1VDEL
         STY     P0VDEL
         LDA     #0
         STA     P0GR
         STA     P1GR
         STA     P0GR
SOUND    LDX     #0
         LDA     SND0
         CMP     #EOG
         BEQ     SKIP110
         INX
SOUNDLP  LDA     GAMESTAT
         BNE     SKIP110
         JMP     TURNOFF                 ;DISALLOW SOUND DURING ATTRACT
SKIP110
         LDA     SND0TIME,X
         BNE     SKIP100
; ROCKET SOUND
         STA     SND0,X                  ;RELEASE CHANNEL
         CPX     #0
         BEQ     DOROCK
;SAUCER SOUND
         BIT     SCREEN
         BPL     DOSAUC
         LDA     PID                     ;CHECK FOR SHEILD SOUND
         AND     #$FE
         CMP     #(SPC & $FF)/6
         BNE     OFF00
;SHIELDS ON
         LDA     #$0C
         STA     SNDC0,X
         LDA     FRAME
         LSR
         ORA     #$1C
         TAY
         LDA     #7
         JMP     TURNOFF
DOSAUC   LDA     JETAX
         BEQ     OFF00
         LDA     JETAY
         CMP     #4
         BCC     OFF00
         LDA     #$C
         STA     SNDC0,X
         LDA     FRAME
         ASL
         AND     #$7
         TAY
         BIT     JETAID
         BMI     LD7
         CMP     #$6
         BEQ     LD7
         LDA     #$0
         BEQ     SKIP116
LD7      LDA     #$7
SKIP116  JMP     TURNOFF
OFF00    LDA     #0
         TAY
         BEQ     SKIP116
DOROCK   LDA     #$8
         STA     SNDC0,X
         LDA     SCRATE
         LSR
         BIT     SCREEN
         BMI     SKIP149
         LSR
         LSR
SKIP149  ORA     #$1
         LDY     #$D
         BNE     SKIP116
SKIP100  DEC     SND0TIME,X
;
         LDY     SND0,X
         STY     T7
         LDA     CTLS,Y
         STA     SNDC0,X
         LDA     FREQS,Y
         TAY
         LDA     SND0TIME,X
         DEC     T7
         BPL     NOT0
         LSR
         LSR
         TAY
         ORA     #2
         BNE     TURNOFF
NOT0     DEC     T7
;BONUS SOUND
         BMI     TURNOFF
NOT1     DEC     T7
;GET MAN SOUND
         BPL     NOT2
         LDA     RAND
         AND     #$7
         TAY
         ORA     #4
         BNE     TURNOFF
NOT2     DEC     T7
         BPL     NOT3
         LSR
         LSR
         TAY
         JMP     TURNOFF
NOT3     DEC     T7
         BPL     NOT4
;DEATH   SOUND
         CMP     #$00
         BCS     DODEATH
         LDA     #0
         TAY
         BEQ     TURNOFF
DODEATH
         ASL
         ASL
         EOR     #$1F
         TAY
         LDA     SND0TIME,X
         LSR
         LSR
         LSR
         LSR
         JMP     TURNOFF
;
; END OF GAME SOUND
;
NOT4     DEC     T7
         BPL     NOT5
         LDA     SND0TIME,X
         CMP     #1
         BEQ     NEXTNOTE
         CMP     #$10
         BCS     NEXTSND
         LSR
         BPL     TURNOFF1                ;ALWAYS
;DO NEXT NOTE
NEXTNOTE DEC     NOTE
         LDA     NOTE
         BEQ     TURNOFF
         TAY
         LDA     #$C
         STA     SNDC0,X
         LDA     NDUR,Y
         STA     SND0TIME,X
         LDA     NFREQ,Y
         TAY
         LDA     #$8
         BNE     TURNOFF
NOT5
         ASL
         AND     #$9
         TAY
         LDA     #$F
TURNOFF  STY     SNDF0,X
TURNOFF1 STA     SNDV0,X
NEXTSND  DEX
         BMI     OUT24
         JMP     SOUNDLP
OUT24
;
; SUBTRACT FROM SCORE IF SHIELDS
;
         LDA     FRAME
         AND     #7
         BNE     OUT14
         LDA     PID
         AND     #$FE
         CMP     #(SPC & $FF)/6
         BNE     OUT14                   ;NOT SHIELDED 
         SED
         LDX     PLAYNO
         LDA     SCOREM0,X
         ORA     SCOREH0,X
         STA     T0
         LDA     SCOREL0,X
         SEC
         SBC     #$10
         BCC     BORROW
         STA     SCOREL0,X
         BCS     OUT14                   ;NO BORROW 
BORROW   LDY     T0
         BNE     SKIP161
         STY     SCOREL0,X
         BEQ     OUT14
SKIP161  STA     SCOREL0,X
         LDA     SCOREM0,X
         SBC     #0
         STA     SCOREM0,X
         LDA     SCOREH0,X
         SBC     #0
         STA     SCOREH0,X
OUT14    CLD
;
; SET MODE

         LDA     FRAME
         BNE     OUT16
         LDA     PUCNT
         CMP     #LOAD
         BCC     SKIP65
         LDA     #$81
         CMP     MODE
         BEQ     SKIP65
         BNE     STMODE
SKIP65
         LDA     WAVENO
         ASL
         ASL
         EOR     RAND
         AND     #$FC
         EOR     RAND
         TAX
         LDA     MODES,X
STMODE   STA     MODE
OUT16
;
;
; DECAY SCROLL RATE
;
         LDY     XX
         BNE     SKIP151
         LDA     FRAME
         AND     #$3F
         BNE     SKIP151
         BIT     SCREEN
         BMI     SKIP147
         LDY     WAVENO
SKIP147
         DEC     SCRATE
         LDA     SCRATE
         BPL     CHKMIN
WRITEMIN LDA     MINSCR1,Y
CHKMIN   CMP     MINSCR1,Y
         BCC     WRITEMIN
         STA     SCRATE
SKIP151
         JSR     RANDOM
;
; END OF VERTICAL BLANK
;
VBOUT    BIT     TIMOUT                  ; WAIT FOR VB OVER
         BPL     VBOUT
         STA     HSYNC
         STX     VBLNK                   ; ENABLE BEAM
         STA     CLRHM
;
;        DO SIX CHARACTER KERNEL TWICE
;
         LDY     #07

         JSR     SIXCHAR
;
         STY     P0VDEL
         STY     P1VDEL
         LDX     PLAYNO
         LDA     LIVES,X
         BPL     SKIP108
         LDA     #0
SKIP108
         AND     #7
         LDX     #0
         ASL
         TAY
         LDA     NOS,Y
         STA     P1SIZE
         LDA     NOS+1,Y
         STA     P0SIZE
         BPL     SCASE0
         STX     P1CLR
SCASE0   ASL
         BPL     SCASE1
         STX     P0CLR
SCASE1
         LDY     #3
LIFELOOP STA     HSYNC
         LDA     SHIP,Y
         STA     P0GR
         STA     P1GR
         DEY
         BPL     LIFELOOP
         STA     HSYNC
         LDA     #$3A
         AND     ATTRACT
         STA     P0CLR
         STA     P1CLR
         STX     P0GR
         STX     P1GR
         LDA     PUCNT
         CMP     #6
         BCC     SKIP143
         LDA     FRAME
         LSR
         LSR
         LSR
         LSR
         LDA     #6
         BCC     SKIP143
         TXA
SKIP143  ASL
         TAY
         LDA     NOS,Y
         STA     P1SIZE
         LDA     NOS+1,Y
         STA     P0SIZE
         BPL     SCASE2
         STX     P1CLR
SCASE2   ASL
         BPL     SCASE3
         STX     P0CLR
SCASE3
         LDY     #3
SHIPLOOP STA     HSYNC
         LDA     MAN,Y
         STA     P0GR
         STA     P1GR
         DEY
         BPL     SHIPLOOP
         STA     HSYNC
         STX     P0GR
         STX     P1GR
         LDA     #$7C
         STA     P1CLR
;
DORADAR  TXA
         STX     P0SIZE                  ;X=0 !
         LDX     RADARX
         BIT     SCREEN
         BMI     SKIP79
         LDA     #$DA
SKIP79
         STA     HSYNC
         NOP
         STA     P0CLR
         LDA     XTABLE,X
         STA     P0HM
         AND     #$F
         TAY
RESLP6   DEY
         BPL     RESLP6
         STA     P0RES
         STA     HSYNC
         STA     HMOV
         LDA     FRAME
         LSR
         AND     #7
         TAX
         LDA     RADARGRX,X
         STA     P0GR
;
; SET UP JETS
;
; DO REFLECTS
;
         LDA     REFLS
         LSR
         STA     P0REF                   ;D3 USED
         LSR
         STA     P1REF
         LDX     #>GRXPT0              ;GRX POINTERS PAGE 1
         LDA     JETAID
         BPL     SKIP10
         LDX     #>GRXPT1
SKIP10   STX     T2
         LDY     #0
         STY     T1                      ;SET UP INDIRECTION
         AND     #$7F
         STA     T0
         ASL
         ADC     T0
         ASL                       ;*6
         TAY
         STA     CLRHM
         LDA     (T1),Y
         STA     JETIND
         INY
         LDA     (T1),Y
         STA     JETIND+1
         INY
         LDA     (T1),Y
         STA     JETINDCL
         INY
         LDA     (T1),Y
         STA     JETINDCL+1
         INY
         LDA     (T1),Y
         STA     P0SIZE
         INY
         LDA     (T1),Y                  ;DO HI BYTES AND SIZE
         STA     T6
         LDX     #>GRXPT0              ;GRX POINTERS PAGE 1
         LDA     JETBID
         BPL     SKIP02
         LDX     #>GRXPT1
SKIP02   STX     T2
         LDY     #0
         STY     T1
         AND     #$7F
         STA     T0
         ASL
         ADC     T0
         ASL                       ;*6
         TAY
;
         LDA     (T1),Y
         STA     JTIND
         INY
         LDA     (T1),Y
         STA     JTIND+1
         INY
         LDA     (T1),Y
         STA     JTINDCLR
         INY
         LDA     (T1),Y
         STA     JTINDCLR+1
         INY
         LDA     (T1),Y                  ;DO HI BYTES AND SIZE
         STA     P1SIZE
;
;        SET UP SHUTTLE
;
         LDX     #>GRXPT0              ;GRX POINTERS PAGE 1
         LDA     T5                      ;SHUTID
         BPL     SKIP03
         LDX     #>GRXPT1
SKIP03   STX     T2
         LDY     #0
         STY     T1                      ;SET UP INDIRECTION
         AND     #$7F
         STA     T0
         ASL
         ADC     T0
         ASL                       ;*6
         TAY
;
         LDA     (T1),Y
         STA     P0IND
         INY
         LDA     (T1),Y
         STA     P0IND+1
         INY
         LDA     (T1),Y
         STA     P0INDCLR
         INY
         LDA     (T1),Y
         STA     P0INDCLR+1
         INY
         LDA     (T1),Y
         STA     T3                      ;SIZE
         INY
         LDA     (T1),Y                  ;DO HI BYTES AND SIZE
         STA     P0TOP
;
         LDX     T4                      ;SHUTX
         LDA     XTABLE,X
         STA     T4
;
         LDA     SHUTSY
         STA     P0Y
;
;        SET UP LOWER SCREEN
;
         LDA     PNTS
         AND     #$F
         TAX
         LDY     #>GRXPT0              ;GRX POINTERS PAGE 1
         LDA     PID,X
         BPL     SKIP07
         LDY     #>GRXPT1
SKIP07   STY     T2
         LDY     #0
         STY     T1                      ;SET UP INDIRECTION
         AND     #$7F
         STA     T0
         ASL
         ADC     T0
         ASL                       ;*6
         TAY
;
         LDA     (T1),Y
         STA     P1IND
         INY
         LDA     (T1),Y
         STA     P1IND+1
         INY
         LDA     (T1),Y
         STA     P1INDCLR
         INY
         LDA     (T1),Y
         STA     P1INDCLR+1
         INY
         LDA     (T1),Y                  ;DO HI BYTES AND SIZE
         STA     T5
         INY
         LDA     (T1),Y
         STA     P1TOP
         LDA     REFMASK,X
         LDY     #8
         AND     REFLS
         BNE     SKIP08
         LDY     #0
SKIP08   STY     T1                      ;SAVE REFLECT
         LDY     PX,X                    ;X HAS POINTERS
         LDA     XTABLE,Y
         STA     P1SHM
;
; SET UP XPOSITIONS
;
         LDA     JETAX
         TAX
         CLC
         ADC     #8
         STA     T0
         STA     HSYNC
         LDA     #0
         STA     P0GR
         LDA     XTABLE,X
         STA     P0HM
         AND     #$F
         TAY
RESLP0   DEY
         BPL     RESLP0
         STA     P0RES
;
         STA     HSYNC
         LDA     T0
         TAX
         LDA     XTABLE,X
         STA     P1HM
         AND     #$F
         TAY
RESLP1   DEY
         BPL     RESLP1
         STA     P1RES
         STA     HSYNC
         STA     HMOV
         LDA     #12
         STA     BKCNT
         LDX     #<M1GR
         TXS
;
         LDA     #LOWHGT+SHUTHGT
         SEC
         SBC     DIFF                    ;PLAYER 0 Y POSITION
         CLC
         ADC     XX
         TAX
         SEC
         SBC     JETAY                   ;PAD HEIGHT
         STA     ENTER                   ;LINE COUNT FOR ENTRANCE TO MAIN
         LDA     BCOLOR
         BIT     SCREEN
         BMI     SKIP54
         LDA     #2
SKIP54   EOR     BCOLOR
         STA     T0                      ;SAVE BACKGROUND VALUE
;
;        JET KERNEL
;
         LDA     JETAY                   ;SET UP PAD COUNT
         SEC
         SBC     T6
         TAY                             ;Y-HEIGHT
PADLOOP  STA     HSYNC
         TXA
         CLC
         SBC     SHUTSY
         AND     #$FC
         PHP
         PLA
         DEX
         STA     CLRHM
         DEY
         BPL     PADLOOP
;
;
         LDY     T6                      ;HEIGHT
         NOP
         CPY     JETAY                   ;YPOS
         BCC     OK02
         LDY     JETAY
OK02     NOP
LOOP4    LDA     (JTINDCLR),Y
         STA     P1CLR
         STA     HSYNC
         LDA     (JTIND),Y
         STA     P1GR
         LDA     (JETIND),Y
         STA     P0GR
         LDA     (JETINDCL),Y
         STA     P0CLR
         TXA
         CLC
         DC      $ED                     ;SBC ABS
         DC.W      SHUTSY
;        SBC     SHUTSY
         AND     #$FC
         PHP
         PLA
         DEX
         NOP
         NOP
         NOP
         NOP
         DEY
         BPL     LOOP4
         PHP
         LDA     T3
         LDX     ENTER
         STA     P0SIZE
         INY
         STY     P0GR
         STY     P1GR
         LDA     T4
         STA     P0HM
         AND     #$0F
         TAY
         TXA
         JMP     CONT00
         BOUNDRY 0
CONT00
         CLC
RESLP3   DEY
         BPL     RESLP3
         STA     P0RES
         STA     HSYNC
         STA     HMOV
;
;        SHUTTLE KERNEL
;
         SBC     SHUTSY
         TAY
SHUTLOOP CMP     P0TOP
         BCS     ET11
         LDA     (P0IND),Y
         STA     P0GR
         LDA     (P0INDCLR),Y
         STA     P0CLR
SKIP09
         DEX
         CPX     EXIT0
         BEQ     DOSETUP
; DO REFLECTS
         LDA     REFLS
         ASL
         ASL
         STA     P0REF
         LDA     T5                      ;LOAD SIZE
         STA     P1SIZE
         LDA     T1                      ;GET REFLECTION
         STA     P1REF
         CLC
         TXA
         STA     CLRHM
         SBC     SHUTSY
         TAY
         STA     HSYNC
         JMP     SHUTLOOP
ET11     NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         JMP     SKIP09
ET10     NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         JMP     SKIP04
;
DOSETUP
;
;        SET UP NEW P1
;
         TXA
         DEX
         CLC
         SBC     SHUTSY
         TAY
         CMP     P0TOP
         NOP
         NOP
         NOP
         BCS     ET10
         LDA     (P0INDCLR),Y
         STA     P0CLR
         LDA     (P0IND),Y
         STA     P0GR
SKIP04
         LDA     P1SHM
         STA     P1HM
         AND     #$F
         TAY
         CPX     P0TOP
         LDA     T0
         NOP
         NOP
         TXA
RESLP4   DEY
         BPL     RESLP4
         STA     P1RES
;
         STA     HSYNC
         STA     HMOV
         BIT     PNTS
         BMI     NOSETUP
;
; SET UP P0
;
         LDA     #0
         STA     P0GR
         DEX
         DEX
         LDY     #>GRXPT0              ;GRX POINTERS PAGE 1
         LDA     PID                     ;LOAD PLAYER
         BPL     SKIP11
         LDY     #>GRXPT1
SKIP11   STY     T2
         LDY     #0
         STY     T1                      ;SET UP INDIRECTION
         AND     #$7F
         STA     T3
         ASL
         ADC     T3
         ASL                       ;*6
         TAY
;
         STA     CLRHM
         STA     HSYNC
;
         LDA     (T1),Y
         STA     P0IND
         INY
         LDA     (T1),Y
         STA     P0IND+1
         INY
         LDA     (T1),Y
         STA     P0INDCLR
         INY
         LDA     (T1),Y
         STA     P0INDCLR+1
         INY
         LDA     (T1),Y
         STA     P0SIZE                  ;SIZE
         INY
         LDA     (T1),Y                  ;DO HI BYTES AND SIZE
         STA     P0TOP
;
         LDA     REFLS
         STA     P0REF                   ;PLAYER USES D3
         STA     HSYNC
         LDY     PX
         LDA     XTABLE,Y
         STA     P0HM
         AND     #$F
         TAY
         DEX                             ;LOWER LINE COUNT
RESLP5   DEY
         BPL     RESLP5
         STA     P0RES
         STA     HSYNC
         DC      $8D
         DC.W      HMOV
;        STA     HMOV
         CPX     P0TOP
         TXA
;
;
; SET UP MAIN KERNELS
;
NOSETUP
         BCS     ET8
         TAY
         LDA     (P0IND),Y
         STA     P0GR
         LDA     (P0INDCLR),Y
         STA     P0CLR
RET8
         DEX
         LDA     T0
         NOP
         NOP
         NOP
         LDA     T0
         NOP
         STA     CLRHM
         JMP     KLOOP                   ;MC 51
ET8      CLC
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         JMP      RET8
         BOUNDRY 0
         NOP
;
; MAIN KERNELS
;
ET0      CLC
         NOP
         JMP      RET0
ET1      CLC
         NOP
         NOP
         NOP
         JMP      RET1
ET2      CLC
         NOP
         NOP
         NOP
         NOP
         NOP
         JMP      RET2
KLOOP    CLC                              ;MC 53
         TXA
         SBC      P1Y
         TAY
         CMP      P1TOP
         BCS      ET0
         LDA      (P1IND),Y
         STA      P1GR
RET0
;
; LINE 1
;
;
;
         LDA      T0
         STA      BKCLR
         CPX      P0TOP
         BCS      ET1
         TXA
         TAY
         LDA      (P0IND),Y
         STA      P0GR
RET1
         CPX      LHGT
         PHP
         PLA
         CLC
;
         LDY      BKCNT
         LDA      (CLRPNT),Y
         EOR      BCOLOR
         STA      T0
;
         DEX
         TXA
         SBC      P1Y
         TAY
         CMP      P1TOP
         BCS      ET2
         LDA      (P1INDCLR),Y
         STA      P1CLR
         LDA      (P1IND),Y
         STA      P1GR
RET2
;
; LINE 2
;
;
;
         CPX      P0TOP
         BCS      ET7
         TXA
         TAY
         LDA      (P0IND),Y
         STA      P0GR
         LDA      (P0INDCLR),Y
         STA      P0CLR
RET7
         TXA
         LDY      BKCNT
         CMP      LIST,Y
         BNE      ETB
         DEC      BKCNT
         NOP
RETB
;
;
         DEX
         CPX      EXIT1
         BNE      KLOOP
         BEQ      ENDK
ETB      LDA      T0
         JMP      RETB
ET7      CLC
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         NOP
         JMP      RET7
ENDK
         JMP      JMPHI
;
;        PROGRAM START
;
;
START    SEI
         CLD                              ; NO DECIMAL
         LDX      #$00
         TXA                              ; CLEAR STELLA AND RAM
CLP      STA      $0,X
         TXS                              ; THANK YOU DAVE C
         INX
         BNE      CLP
;
; INITS
;
         DEC      ATTRACT
         LDA      #<COLORS
         STA      CLRPNT
         LDA      #>COLORS
         STA      CLRPNT+1
         STA      RAND
         LDA      #$A7
         STA      RANDH
         LDA      #HORIZON+$20
         STA      SHUTY
         LDA      #$06
         STA      BCOLOR
         LDA      #5
         STA      PFCTRL
         LDA      #$30
         STA      PF0
         LDA      #$80
         STA      SCREEN
         LDA      #MIDDLE
         STA      JETAX
         JMP      JMPHI
;
;
; SIX CHARACTER ROUTINE
;
SIXCHAR
SCKRNL   STY      T1
         LDA      (SIXIND5),Y
         STA      P1GR
         STA      HSYNC
         LDA      (SIXIND4),Y
         STA      P0GR
         LDA      (SIXIND3),Y
         STA      P1GR
         LDA      (SIXIND2),Y
         STA      T2
         LDA      (SIXIND1),Y
         TAX
         LDA      (SIXIND0),Y
         TAY
         LDA      T2
         STA      P0GR
         STX      P1GR
         STY      P0GR
         STY      P1GR
         LDY      T1
         DEY
         BPL      SCKRNL
         INY
         STY      P1GR
         STY      P0GR
         STY      P1GR
         STY      P0GR
         RTS
;
;
; GRX POINTERS
;
         BOUNDRY 0
GRXPT0
S0       DC.W       SAUC0
         DC.W       SAUC0CLR
         DC       0,12
S1       DC.W       SAUC1
         DC.W       SAUC1CLR
         DC       0,12
S2       DC.W       SAUC2
         DC.W       SAUC2CLR
         DC       0,16
S3       DC.W       SAUC3
         DC.W       SAUC3CLR
         DC       0,16
S01      DC.W       SAUC01
         DC.W       SAUC0CLR
         DC       0,12
S11      DC.W       SAUC11
         DC.W       SAUC1CLR
         DC       0,12
S21      DC.W       SAUC21
         DC.W       SAUC2CLR
         DC       0,16
S31      DC.W       SAUC31
         DC.W       SAUC3CLR
         DC       0,16
SAUCEND                                   ;END OF SAUCERS
CRY0     DC.W       CRYS0
         DC.W       CRYS0CLR
         DC       0,6
CRY1     DC.W       CRYS1
         DC.W       CRYS1CLR
         DC       0,8
CRY2     DC.W       CRYS2
         DC.W       CRYS2CLR
         DC       0,10
CRY3     DC.W       CRYS3
         DC.W       CRYS3CLR
         DC       0,10
CRY01    DC.W       CRYS01
         DC.W       CRYS0CLR
         DC       0,6
CRY11    DC.W       CRYS11
         DC.W       CRYS1CLR
         DC       0,8
CRY21    DC.W       CRYS21
         DC.W       CRYS2CLR
         DC       0,10
CRY31    DC.W       CRY31
         DC.W       CRYS3CLR
         DC       0,12
ANEND1                                    ;END OF TWO STEP ANIMATIONS
R0       DC.W       RING0
         DC.W       RING0CLR
         DC       0,05
R1       DC.W       RING1
         DC.W       RING0CLR
         DC       0,07
R2       DC.W       RING2
         DC.W       RING0CLR
         DC       0,10
R3       DC.W       RING3
         DC.W       RING0CLR
         DC       0,14
TWR0     DC.W       TOWER0
         DC.W       TOWER0CL
         DC       0,8
TWR1     DC.W       TOWER1
         DC.W       TOWER1CL
         DC       0,13
TWR2     DC.W       TOWER2
         DC.W       TOWER2CL
         DC       0,17
TWR3     DC.W       TOWER3
         DC.W       TOWER3CL
         DC       0,21
ANEND                                     ;END OF DISTANCED ANIMATION
SAT0     DC.W       SATT0
         DC.W       SATT0CLR
         DC       0,14
SAT1     DC.W       SATT1
         DC.W       SATT0CLR
         DC       0,14
SPC      DC.W       SCENT
         DC.W       SCENTCLR
         DC       0,16
SPR      DC.W       SRGHT
         DC.W       SCENTCLR
         DC       0,16
VM0      DC.W       VMIS0
         DC.W       VMIS0CLR
         DC       0,16
LND0     DC.W       LAND0
         DC.W       LAND0CLR
         DC       0,16
FB0      DC.W       FBALL0
         DC.W       FBALL0CL
         DC       0,16
FB1      DC.W       FBALL1
         DC.W       FBALL0CL
         DC       0,16
EX0      DC.W       EXPL0
         DC.W       EXPL0CLR
         DC       0,14
EX1      DC.W       EXPL1
         DC.W       EXPL1CLR
         DC       0,16
EX2      DC.W       EXPL2
         DC.W       EXPL2CLR
         DC       0,18
EX3      DC.W       EXPL3
         DC.W       EXPL3CLR
         DC       0,18
EXE                                       ;END OF EXPLOSIONS
P160     DC.W       PL160
         DC.W       PL160CLR
         DC       0,17
P161     DC.W       PL161
         DC.W       PL160CLR
         DC       0,17
PC       DC.W       PCENT
         DC.W       PCENTCLR
         DC       00
         DC       16
PR       DC.W       PRGHT
         DC.W       PRGHTCLR
         DC       0
         DC       16
CT       DC.W       CTORP
         DC.W       CTORPCLR
         DC       0
         DC       10
RT       DC.W       RTORP
         DC.W       RTORPCLR
         DC       0
         DC       8
         BOUNDRY 0
GRXPT1
;
; PLANETS
;
P0       DC.W       PL0
         DC.W       PL0CLR
         DC       0,4
P1       DC.W       PL1
         DC.W       PL0CLR
         DC       0,6
P2       DC.W       PL2
         DC.W       PL0CLR
         DC       0,10
P3       DC.W       PL3
         DC.W       PL0CLR
         DC       0,12
P0RED    DC.W       PL0
         DC.W       RED
         DC       0,4
P1RED    DC.W       PL1
         DC.W       RED
         DC       0,6
P2RED    DC.W       PL2
         DC.W       RED
         DC       0,10
P3RED    DC.W       PL3
         DC.W       RED
         DC       0,12
P0GRN    DC.W       PL0
         DC.W       GRN
         DC       0,4
P1GRN    DC.W       PL1
         DC.W       GRN
         DC       0,6
P2GRN    DC.W       PL2
         DC.W       GRN
         DC       0,10
P3GRN    DC.W       PL3
         DC.W       GRN
         DC       0,12
P0YEL    DC.W       PL0
         DC.W       YEL
         DC       0,4
P1YEL    DC.W       PL1
         DC.W       YEL
         DC       0,5
P2YEL    DC.W       PL2
         DC.W       YEL
         DC       0,10
P3YEL    DC.W       PL3
         DC.W       YEL
         DC       0,12
PXA      DC.W       PLL0
         DC.W       PL0CLR
         DC       0,12
PXB      DC.W       PLL1
         DC.W       PL0CLR
         DC       0,18
PXC      DC.W       PLLL0
         DC.W       PL0CLR
         DC       5,8
PXD      DC.W       PLLLL0
         DC.W       PL0CLR
         DC       7,8
PXARED   DC.W       PLL0
         DC.W       RED
         DC       0,12
PXBRED   DC.W       PLL1
         DC.W       RED
         DC       0,18
PXCRED   DC.W       PLLL0
         DC.W       RED
         DC       5,8
PXDRED   DC.W       PLLLL0
         DC.W       RED
         DC       7,8
PXAGRN   DC.W       PLL0
         DC.W       GRN
         DC       0,12
PXBGRN   DC.W       PLL1
         DC.W       GRN
         DC       0,18
PXCGRN   DC.W       PLLL0
         DC.W       GRN
         DC       5,8
PXDGRN   DC.W       PLLLL0
         DC.W       GRN
         DC       7,8
PXAYEL   DC.W       PLL0
         DC.W       YEL
         DC       0,12
PXBYEL   DC.W       PLL1
         DC.W       YEL
         DC       0,18
PXCYEL   DC.W       PLLL0
         DC.W       YEL
         DC       5,8
PXDYEL   DC.W       PLLLL0
         DC.W       YEL
         DC       7,8
JX00     DC.W       JET00
         DC.W       JETCLR
         DC       0
         DC       15
JX01     DC.W       JET01
         DC.W       JETCLR
         DC       0
         DC       15
JX10     DC.W       JET10
         DC.W       JETCLR
         DC       0
         DC       15
JX11     DC.W       JET11
         DC.W       JETCLR
         DC       0,15
JX20     DC.W       JET20
         DC.W       JETCLR
         DC       0,15
JX21     DC.W       JET21
         DC.W       JETCLR
         DC       0,15
P162     DC.W       PL162
         DC.W       PL160CLR
         DC       0,17
AST0     DC.W       ASTER2
         DC.W       ASTERCLR
         DC       0,16
AST1     DC.W       ASTER1
         DC.W       ASTERCLR
         DC       0,12
AST2     DC.W       ASTER0
         DC.W       ASTERCLR
         DC       0,14
         IF       GRX=0
         PRINT    OFF
         ENDIF
         
         BOUNDRY 0
;
; GRAPHICS DATA FROM D:SACER2
;

SAUC31   DC       $00,$7E,$FF,$7E,$00,$00,$00,$00,$24
         DC       $7E,$AB,$FF,$7E,$3C,$18,$00
SAUC3    DC       $00,$7E,$FF,$7E,$00,$00,$00,$00,$18
         DC       $7E,$D5,$FF,$7E,$3C,$18,$00
SAUC21   DC       $00,$7E,$FF,$7E,$00,$00,$00,$00
         DC       $24,$7E,$D7,$7E,$3C,$18,$00,$00
SAUC2    DC       $00,$7E,$FF,$7E,$00,$00,$00,$00
         DC       $18,$7E,$EB,$7E,$3C,$18,$00,$00
SAUC11   DC       $00,$18,$7E,$18,$00,$00
         DC       $00,$00,$18,$7E,$2C,$18
SAUC1    DC       $00,$18,$7E,$18,$00,$00
         DC       $00,$00,$24,$7E,$34,$18
SAUC01   DC       $00,$18,$18,$00,$00
         DC       $00,$00,$00,$18,$3C,$18,$00
SAUC0    DC       $00,$18,$18,$00,$00
         DC       $00,$00,$00,$18,$3C,$18,$00
         IF       PAL
CRYS0CLR DC       $BC,$00,$08,$2E,$46,$46
CRYS1CLR DC       $BC,$00,$08,$2E,$2E,$2E,$46,$46
CRYS2CLR DC       $BC,$00,$00,$08,$08,$2E,$2E,$46,$46
CRYS3CLR DC       $BC,$00,$00,$00,$08,$2E,$2E,$46
         DC       $46,$46,$46
TOWER0CL DC       $BC,$D6,$D6,$46,$46
TOWER1CL DC       $BC,$86,$86,$A8,$A8,$C6,$C6,$B6,$B6,$D8,$D8
         DC       $48,$48
TOWER2CL DC       $BC,$00,$00,$88,$88,$88,$88,$AA,$AA
         DC       $A8,$A8,$B8,$B8,$DA,$DA,$4A,$4A
TOWER3CL DC       $BC,$00,$00,$00,$00
         DC       $08,$08,$88,$88,$88,$88,$AA,$AA
         DC       $A8,$A8,$B8,$B8,$DA,$DA,$4A,$4A
EXPL1CLR DC       $BC,$48,$48,$48,$48,$48,$48,$48
         DC       $48,$48,$48,448,$48,$48,$48,$48,$48
EXPL2CLR DC       $BC,$28,$28,$28,$28,$28,$28,$28
         DC       $28,$28,$28,$28,$28,$28,$28,$28,$28
EXPL0CLR DC       $BC,$2F,$2F,$2F,$2F,$2F,$2F,$2F
         DC       $2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F,$2F
;         BOUNDRY 0
   align 256,0
         
EXPL3CLR DC       $BC,$44,$44,$44,$44,$44,$44,$44
         DC       $44,$44,$44,$44,$44,$44,$44,$44,$44
         ELSE
CRYS0CLR DC       $7C,$00,$08,$1E,$36,$36
CRYS1CLR DC       $7C,$00,$08,$1E,$1E,$1E,$36,$36
CRYS2CLR DC       $7C,$00,$00,$08,$08,$1E,$1E,$36,$36
CRYS3CLR DC       $7C,$00,$00,$00,$08,$1E,$1E,$36
         DC       $36,$36,$36
TOWER0CL DC       $7C,$96,$96,$36,$36
TOWER1CL DC       $7C,$56,$56,$68,$68,$66,$66,$86,$86,$98,$98
         DC       $38,$38
TOWER2CL DC       $7C,$00,$00,$58,$58,$58,$58,$6A,$6A
         DC       $68,$68,$88,$88,$9A,$9A,$3A,$3A
TOWER3CL DC       $7C,$00,$00,$00,$00
         DC       $08,$08,$58,$58,$58,$58,$6A,$6A
         DC       $68,$68,$88,$88,$9A,$9A,$3A,$3A
EXPL1CLR DC       $7C,$38,$38,$38,$38,$38,$38,$38
         DC       $38,$38,$38,$38,$38,$38,$38,$38,$38
EXPL2CLR DC       $7C,$18,$18,$18,$18,$18,$18,$18
         DC       $18,$18,$18,$18,$18,$18,$18,$18,$18
EXPL0CLR DC       $7C,$FF,$FF,$FF,$FF,$FF,$FF,$FF
         DC       $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
;         BOUNDRY 0
   align 256, 0
         
EXPL3CLR DC       $7C,$44,$44,$44,$44,$44,$44,$44
         DC       $44,$44,$44,$44,$44,$44,$44,$44,$44
         ENDIF
         
;
; GRAPHICS DATA FROM D:SAT7
;
SATT0    DC       $00,$00,$13,$07,$0F,$0F,$2F
         DC        $7F,$2F,$0F,$07,$13,$00,$00,0,0
SATT1     DC      $40,$20,$13,$07,$0F,$0F,$1F
          DC       $3F,$1F,$0F,$07,$13,$20,$40,0,0
SCENT    DC       $00,$00,$00,$00,$00,$00,$24,$AB
         DC       $D7,$81,$42,$81,$99,$C3,$66,$BC
SRGHT    DC       $00,$00,$00,$00,$00,$00,$52,$2C
         DC       $56,$81,$C3,$42,$99,$42,$AE,$5A
         IF       PAL
SCENTCLR DC       $BC,$00,$00,$00,$00,$00,$2F,$2F
         DC       $44,$44,$A8,$A8,$B8,$B8,$2F,$2F
         ELSE
SCENTCLR DC       $7C,$00,$00,$00,$00,$00,$FF,$FF
         DC       $44,$44,$68,$68,$88,$88,$FF,$FF
         ENDIF
         
;
; GRAPHICS DATA FROM D:SSHOT
;
FBALL0   DC       0,$38,$14,$28,$1C,$18,$28,$18,$10
         DC       $38,$08,$00,$00,$08,$00,$08,$10
FBALL1   DC       0,$18,$38,$1C,$10,$08,$10,$24,$08
         DC       $00,$20,$08,$10,$00,$10,$00,$08
         IF       PAL
FBALL0CL DC       $BC,$44,$44,$44,$44,$46,$46,$48
         DC       $48,$48,$48,$28,$28,$28,$28,$28
         ELSE
FBALL0CL DC       $7C,$24,$24,$24,$24,$26,$26,$28
         DC       $28,$28,$28,$18,$18,$18,$18,$18
         ENDIF
         
         
PL0      DC       $00,$18,$18,$00
PL1      DC       $00,$18
         DC       $3C,$3C,$18,$00
PL2      DC       $00,$18,$3C,$3C
         DC       $7E,$7E,$3C,$3C,$18,$00
PL3      DC       $00,$38,$7C,$7C,$FE,$FE
         DC       $FE,$FE,$7C,$7C,$38,$00
         IF       PAL
PL0CLR   DC       $BC,$B6,$B6,$B8,$B8,$BA,$BA,$BC,$BC
         DC       $BC,$BC,$BA,$BA,$B8,$B8,$B6,$B6,$B6
         ELSE
PL0CLR   DC       $7C,$86,$86,$88,$88,$8A,$8A,$8C,$8C
         DC       $8C,$8C,$8A,$8A,$88,$88,$86,$86,$86
         ENDIF
         
         
         
;
; GRAPHICS DATA FROM D:SUN5
;

PL160    DC       0,$01,$0B,$27,$1F,$2F,$1F,$2F,$1F
         DC       $1F,$2F,$1F,$4F,$3F,$6F,$53,$02,0
PL161    DC       0,$0B,$62,$3F,$57,$3F,$1F,$1F,$5F
         DC       $BF,$1F,$5F,$0F,$1F,$0B,$23,$09,0
PL162    DC       0,$00,$03,$07,$0F,$1F,$5F,$3F,$BF
         DC       $6F,$3F,$1F,$1F,$4F,$07,$03,$00,0
;
; GRAPHICS DATA FROM D:ENL3
;
         BOUNDRY 0
         IF       PAL
SATT0CLR
         DC       $2F,$2F,$4A,$4C,$4F,$BC,$BA,$BA
         DC       $0F,$4E,$4C,$4A,$48,$BA,$2F,$2F
         ELSE
SATT0CLR
         DC       $FF,$FF,$3A,$3C,$3F,$7C,$8A,$8A
         DC       $0F,$3E,$3C,$3A,$38,$8A,$FF,$FF
         ENDIF
LAND0    DC       $00,$08,$00,$00
         DC       $18,$00,$18,$00,$10,$18,$3C,$18
         DC       $00,$00,$00,$00
;
; GRAPHICS DATA FROM D:MEN0
;
CRYS0    DC       $00,$1C,$14,$1C,$3E,$2A
CRYS01   DC       $00,$1C,$14,$1C,$7F,$08
CRYS1    DC       $00,$1C,$14,$14,$1C,$3E,$2A,$00
CRYS11   DC       $00,$1C,$14,$14,$1C,$7F,$08,$00
CRYS2    DC       $00,$08,$1C,$14,$14,$1C,$1C,$3E,$2A,$00
CRYS21   DC       $00,$08,$1C,$14,$14,$1C,$1C,$7F,$08,$00
CRYS3    DC       $00,$08,$1C,$36,$14,$1C,$1C,$7F,$55
         DC       $08
CRYS31   DC       $00,$08,$1C,$36,$14,$1C,$1C,$1C,$36
         DC       $2A,$22,$00
         IF       PAL
SAUC0CLR DC       $BC,$00,$00,$00,$46,$46
         DC       $48,$48,$28,$28,$28,$28
SAUC1CLR DC       $BC,$00,$00,$00,$00,$46
         DC       $46,$48,$48,$28,$28,$28
SAUC2CLR DC       $BC,$00,$00,$00,$00,$00,$46,$46
         DC       $48,$48,$28,$28,$2A,$2A,$2A,$2A
SAUC3CLR DC       $BC,$00,$00,$00,$00,$00,$46,$46
         DC       $48,$48,$28,$28,$2A,$2A,$2C,$2C
         ELSE
SAUC0CLR DC       $7C,$00,$00,$00,$26,$26
         DC       $28,$28,$18,$18,$18,$18
SAUC1CLR DC       $7C,$00,$00,$00,$00,$26
         DC       $26,$28,$28,$18,$18,$18
SAUC2CLR DC       $7C,$00,$00,$00,$00,$00,$26,$26
         DC       $28,$28,$18,$18,$1A,$1A,$1A,$1A
SAUC3CLR DC       $7C,$00,$00,$00,$00,$00,$26,$26
         DC       $28,$28,$18,$18,$1A,$1A,$1C,$1C
         ENDIF

;
; GRAPHICS DATA FROM D:EXPL0
;

EXPL0    DC       $00,$08,$42,$18,$78,$3E,$7C
         DC       $3E,$7C,$3E,$18,$42,$10,$00
EXPL1    DC       $00,$00,$24,$52,$28,$3C,$7D,$BE
         DC       $7D,$BE,$7C,$3C,$18,$44,$00,$00
EXPL2    DC       $00,$00,$24,$5B,$BE,$7D,$BE,$7D
         DC       $BE,$7D,$BE,$7D,$3C,$5A,$00,$00,0,0
EXPL3    DC       $00,$28,$42,$25,$7C,$3E,$75,$AE
         DC       $3D,$7E,$34,$7E,$99,$24,$18,$00,0,0
NDUR     DC       $01,$50,$20,$10,$10,$10,$10,$10,$10,$30,$10,$10,$30
         DC       $20,$40,$20
         BOUNDRY  0

         IF       PAL
         
LAND0CLR DC       $BC,$42,$42,$42,$42,$42,$42,$42
         DC       $46,$46,$46,$46,$46,$46,$46,$46
;
; GRAPHICS DATA FROM D:VMIS
;
VMIS0CLR DC       $BC,$44,$44,$44,$44,$AA,$AA,$AA
         DC       $AA,$AA,$AA,$AA,$AA,$AA,$2F,$2F
         ELSE
LAND0CLR DC       $7C,$42,$42,$42,$42,$42,$42,$42
         DC       $26,$26,$26,$26,$26,$26,$26,$26
;
; GRAPHICS DATA FROM D:VMIS
;
VMIS0CLR DC       $7C,$44,$44,$44,$44,$6A,$6A,$6A
         DC       $6A,$6A,$6A,$6A,$6A,$6A,$FF,$FF
         ENDIF
VMIS0    DC       $00,$28,$10,$10,$00,$28,$38,$38
         DC       $10,$10,$10,$10,$10,$10,$10,$00
;
; **** NOTE: DD
;  The following was moved up in the ROM to match the listing supplied by Bob
;  Smith. In the ROM disassembly VMIS0CLR resides at location $1C20 and the
; listing has it at location $1C10
;
;       DC $7C ; | XXXXX  | $1C20
;       DC $44 ; | X   X  | $1C21
;       DC $44 ; | X   X  | $1C22
;       DC $44 ; | X   X  | $1C23
;       DC $44 ; | X   X  | $1C24
;       DC $6A ; | XX X X | $1C25
;       DC $6A ; | XX X X | $1C26
;       DC $6A ; | XX X X | $1C27
;       DC $6A ; | XX X X | $1C28
;       DC $6A ; | XX X X | $1C29
;       DC $6A ; | XX X X | $1C2A
;       DC $6A ; | XX X X | $1C2B
;       DC $6A ; | XX X X | $1C2C
;       DC $6A ; | XX X X | $1C2D
;       DC $FF ; |XXXXXXXX| $1C2E
;       DC $FF ; |XXXXXXXX| $1C2F

;
; GRAPHICS DATA FROM D:RING0
;
RING0    DC       $00,$3E,$08,$14,$08
RING1
         DC       $00,$7F,$14,$22,$22,$22,$14
RING2    DC       $00,$7F
         DC       $14,$14,$22,$41,$41,$41,$22,$14
RING3    DC       $00,$7F,$22,$14,$22,$14
         DC       $22,$41,$41,$41,$41,$41,$22,$00
         IF       PAL
RING0CLR DC       $BC,$8A,$8A,$BA,$BA,$BA
         DC       $BA,$BA,$BA,$BA,$BA,$BA,$BA,$BA
         ELSE
RING0CLR DC       $7C,$5A,$5A,$8A,$8A,$8A
         DC       $8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A
         ENDIF
;
; GRAPHICS DATA FROM D:STZOOM
;

TOWER0   DC       $00,$38,$28,$28,$28,$28,$7C,$38
TOWER1   DC       $00,$7E,$24,$24,$24
         DC       $24,$24,$24,$24,$3C,$66,$3C,$18
TOWER2   DC       $00,$7E,$3C,$7E,$24,$24,$24,$24,$24
         DC       $24,$24,$24,$3C,$7E,$A5,$7E,$18
TOWER3   DC       $00,$7E,$3C,$24,$7E
         DC       $7E,$24,$24,$24,$24,$24,$24,$24
         DC       $24,$24,$3C,$7E,$A5,$A5,$7E,$3C
         IF       PAL
RED      DC       $BC,$44,$44,$46,$46,$68,$68,$6A
         DC       $6A,$6A,$6A,$68,$68,$46,$46,$44,$44
         ELSE
RED      DC       $7C,$44,$44,$46,$46,$48,$48,$4A
         DC       $4A,$4A,$4A,$48,$48,$46,$46,$44,$44
         ENDIF
MAN      DC       $28,$38,$7C,$54
SHIP     DC       $FE,$82,$82,$FE
;
         IF       PAL
SCORECLR DC       $44,$A8
         ELSE
SCORECLR DC       $F4,$68
         ENDIF
REFMASK  DC       $08,$02,$04,$01,0,$10,$20
;
; GRAPHICS DATA FROM D:AST7
;
ASTER0   DC       $00,$80,$20,$40,$A8,$04,$12,$0B,$07,$03,$00,$00
         DC       $00,$00
ASTER1   DC       $00,$00,$00,$00,$00,$A8,$02
         DC       $B7,$0F,$52,$00,$00
ASTER2   DC       $00,$00,$00
         DC       $00,$00,$00,$03,$07,$0B,$12,$04,$48,$00,$20,$80,$40       
NFREQ    DC       $01,$13,$11,$0F,$0F,$13,$13,$13,$13,$1A,$17,$13,$13
         DC       $0F,$13,$0F
;
; MINIMUM SCROLL RATES
;
MINSCR1  DC       3,$11,7,$0A

         BOUNDRY 0
;
; GRAPHICS DATA FROM D: YOU4
;

PCENT    DC       $00,$FF,$7E,$3C,$18,$00,$00,$00
         DC       $00,$7E,$BD,$BD,$66,$24,$18,$18
PRGHT    DC       $00,$0C,$3E,$FE,$7F,$00,$00,$06
         DC       $1D,$3D,$FF,$BE,$B6,$7B,$0F,$03
         IF       PAL
PCENTCLR DC       $BC,$00,$00,$00,$00,$00,$00,$00
         DC       $44,$44,$A8,$A8,$B8,$B8,$4A,$4A
PRGHTCLR DC       $BC,$00,$00,$00,$00,$00,$00,$00
         DC       $44,$44,$A8,$A8,$B8,$B8,$4A,$4A
         ELSE
PCENTCLR DC       $7C,$00,$00,$00,$00,$00,$00,$00
         DC       $44,$44,$68,$68,$88,$88,$3A,$3A
PRGHTCLR DC       $7C,$00,$00,$00,$00,$00,$00,$00
         DC       $44,$44,$68,$68,$88,$88,$3A,$3A
         ENDIF
;
; GRAPHICS DATA FROM D: TORP4
;

CTORP    DC       0,$22,$3E,$08,$1C,$1C,$08,$08,$08,$00
RTORP    DC       $00,$30,$30,$F8,$1C,$0E,$03,$00

         IF       PAL
CTORPCLR DC       $BC,$0C,$0C,$0A,$0A,$9C,$9C,$44,$44
RTORPCLR DC       $BC,$0C,$0C,$0A,$0A,$9C,$9C,$44,$44
         ELSE
CTORPCLR DC       $7C,$0C,$0C,$0A,$0A,$AC,$AC,$44,$44
RTORPCLR DC       $7C,$0C,$0C,$0A,$0A,$AC,$AC,$44,$44
         ENDIF
;
; GRAPHICS DATA FROM D:SAC4
;
JET00    DC       $00,$10,$08,$07,$07,$0D
         DC        $7F,$DE,$7F,$08,$04,$06,$01,$00
JET01    DC       $00,$10,$08,$07,$07,$0D
         DC        $7F,$ED,$7F,$08,$04,$06,$01,$00
JET10    DC       $00,$01,$21,$17,$0F,$0B
         DC        $1F,$7D,$EF,$7C,$06,$03,$00,$00
JET11    DC       $00,$01,$21,$17,$0F,$0C
         DC        $1F,$7E,$F7,$7C,$06,$03,$00,$00
JET20    DC       $00,$10,$20,$C0,$E6,$7F
         DC        $DE,$F8,$90,$10,$20,$C0,$00,$00
JET21    DC       $00,$10,$20,$C0,$E6,$7F
         DC        $EE,$F8,$90,$10,$20,$C0,$00,$00
         IF       PAL
JETCLR   DC       $BC,$48,$48,$48,$3A,$3A,$3C,$3C,$BA
         DC       $BC,$BC,$BC,$BC,$00,$00,$00
         ELSE
JETCLR   DC       $7C,$38,$38,$38,$2A,$2A,$2C,$2C,$8A
         DC       $8C,$8C,$8C,$8C,$00,$00,$00
         ENDIF
;2 PLAYERS
PLL0     DC       $00,$03,$07,$0F,$0F,$1F,$1F,$1F
         DC       $1F,$1F,$0F,$0F,$07,$03
PLL1     DC       $00,$03,$07,$0F,$0F,$1F,$1F,$3F,$3F
         DC       $3F,$3F,$1F,$1F,$0F,$0F,$07,$03,$00
;
; GIANT PLANET
; PLAYERS, 2 CLOCK
;
PLLL0    DC       $00,$FF,$7F,$3F,$1F,$07,0,0
;
; SUPER GIANT PLANET
; 2 PLAYERS, 4 CLOCK
;
PLLLL0   DC       $00,$FF,$7F,$3F,$1F,$07,0,0
;
; RADAR GRAPHICS
;
RADARGRX DC       $C3,$E7,$FF,$7E,$3C,$18,$5A,$5A
;

;
; NUMBER SET
;
NUMBERS  EQU      *

;
; GRAPHICS DATA FROM D:NO.8
;
N0       DC       $FE,$CE,$CE,$CE,$C6,$C6,$C6,$FE
N1       DC       $3C,$3C,$3C,$3C,$1C,$1C,$1C,$1C
N2       DC       $FE,$E0,$E0,$FE,$06,$06,$C6,$FE
N3       DC       $FE,$CE,$CE,$0E,$3E,$0C,$CC,$FC
N4       DC       $1C,$1C,$1C,$FE,$CC,$CC,$CC,$CC
N5       DC       $FE,$CE,$0E,$0E,$FE,$C0,$C0,$FE
N6       DC       $FE,$CE,$CE,$CE,$FE,$C0,$C6,$FE
N7       DC       $0E,$0E,$0E,$0E,$06,$06,$C6,$FE
N8       DC       $FE,$CE,$CE,$CE,$FE,$6C,$6C,$7C

;
; GRAPHICS DATA FROM D:NO.9
N9       DC       $0E,$0E,$0E,$FE,$C6,$C6,$C6,$FE
;
;
;        IMAGIC COPYRIGHT 1982 DATA
;                 DATA IS 08 LINES HIGH
;                 AND IS STORED BACKWARDS
;                 IM1 REPRESENTS THE LEFT-MOST DATA
;
LOGO
IM1      DC       $38,$44,$92,$A2,$A2,$92,$44,$38
IM2      DC       $00,$45,$45,$45,$5D,$55,$5D,$00
IM3      DC       $00,$DC,$44,$44,$DC,$44,$DC,$00
IM4      DC       $AA,$AA,$AA,$AA,$AA,$AA,$AA,$94
IM5      DC       $93,$94,$94,$95,$F5,$94,$94,$63
IM6      DC       $26,$A9,$A8,$A8,$28,$28,$A9,$26
SPACE    DC       0,0,0,0,0,0,0,0
;
         IF       PAL
PL160CLR DC       $3A,$3A,$3A,$3A,$3A,$3C,$3C,$3E,$3E
         DC       $3E,$3E,$3C,$3C,$3A,$3A,$3A,$3A,$3A,$3A
         ELSE
PL160CLR DC       $2A,$2A,$2A,$2A,$2A,$2C,$2C,$2E,$2E
         DC       $2E,$2E,$2C,$2C,$2A,$2A,$2A,$2A,$2A,$2A
         ENDIF
;
;
;
; DATA
;
;
COLORS   DC       $00,$02,$00
         DC       $02,$00,$02
         DC       $00,$02,$00
         DC       $02,$00,$02,$06
COLORS1  DC       $02,$00,$02
         DC       $00,$02,$00
         DC       $02,$00,$02
         DC       $00,$02,$00,$06
         IF       PAL
ASTERCLR DC       $BC,$44,$44,$44,$44,$44,$44,$44
         DC       $44,$44,$44,$44,$44,$44,$44,$44
GRN      DC       $BC,$54,$54,$56,$56,$58,$58,$5A
         DC       $5A,$5A,$5A,$58,$58,$56,$56,$54,$54
YEL      DC       $BC,$26,$26,$28,$28,$2A,$2A,$2C
         DC       $2C,$2C,$2C,$2A,$2A,$28,$28,$26,$26
         ELSE
ASTERCLR DC       $7C,$44,$44,$44,$44,$44,$44,$44
         DC       $44,$F4,$F4,$F4,$F4,$F4,$F4,$F4
GRN      DC       $7C,$C4,$C4,$C6,$C6,$C8,$C8,$CA
         DC       $CA,$CA,$CA,$C8,$C8,$C6,$C6,$C4,$C4
YEL      DC       $7C,$16,$16,$18,$18,$1A,$1A,$1C
         DC       $1C,$1C,$1C,$1A,$1A,$18,$18,$16,$16
         ENDIF
;
; RANDOM NUMBER GENERATOR
;
RANDOM   LDX      #0                      ;FOR A WHILE
         LDA      RAND
         ROR
         ROR
         ROR
         EOR      RANDH
         ASL
         ASL
         ROL      RAND
         ROL      RANDH
         LDY      PLAYNO                  ;HANDLE TRIGGER DEBOUNCE
         LDA      TRIG0,Y
         BPL      TRIGON
         STX      DEBOUNCE
TRIGON:
         RTS
         BOUNDRY 0

XTABLE   DC       $60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60
         DC       $4A,$3A,$2A,$1A,$0A,$FA,$EA,$DA,$60,$50,$40,$30,$20,$10,$00,$F0
         DC       $E0,$D0,$C0,$B0,$A0,$90,$71,$61,$51,$41,$31,$21,$11,$01,$F1,$E1
         DC       $D1,$C1,$B1,$A1,$91,$72,$62,$52,$42,$32,$22,$12,$02,$F2,$E2,$D2
         DC       $C2,$B2,$A2,$92,$73,$63,$53,$43,$33,$23,$13,$03,$F3,$E3,$D3,$C3
         DC       $B3,$A3,$93,$74,$64,$54,$44,$34,$24,$14,$04,$F4,$E4,$D4,$C4,$B4
         DC       $A4,$94,$75,$65,$55,$45,$35,$25,$15,$05,$F5,$E5,$D5,$C5,$B5,$A5
         DC       $95,$76,$66,$56,$46,$36,$26,$16,$06,$F6,$E6,$D6,$C6,$B6,$A6,$96
         DC       $77,$67,$57,$47,$37,$27,$17,$07,$F7,$E7,$D7,$C7,$B7,$A7,$97,$78
         DC       $68,$58,$48,$38,$28,$18,$08,$F8,$E8,$D8,$C8,$B8,$A8,$98,$79,$69
         DC       $59,$49,$39,$29,$19,$09,$F9,$E9,$D9,$C9,$B9,$A9,$99,$7A,$6A,$5A
         DC       $4A,$3A,$2A,$1A,$0A,$FA,$EA,$DA,$60,$60,$60,$60,$60,$60,$60,$60
         DC       $60,$60,$60,$60,$60,$60,$60,$60
;
NOS      DC       $00,$C0                 ;0
         DC       $00,$40                 ;1
         DC       $00,$00                 ;2
         DC       $01,$00                 ;3
         DC       $01,$01                 ;4
         DC       $03,$01                 ;5
         DC       $03,$03                 ;6
         DC       $03,$03                 ;7
;
; MODES
;
;  1=RINGS 2=TOWERS 3=SAUCERS ONLY  0=TOWERS OR CRYSTALS
;  D7=0 DO SHUTTLE
;
; BLUE
MODES    DC       $80,$02,$80,$00
;RED
         DC       $00,$82,$02,$03
;GREEN
         DC       $00,$80,$02,$82
;YELLOW
         DC       $80,$80,$03,$02
CTLS     DC       $08,$04,$0C,$02,$0C,$0C,$0C
FREQS    DC       $00,$0F,$00,$0F,$00,$0F,$0F
         BOUNDRY 8

         DC.W       0
         DC.W       START
         DC.W       START
         DC.W       START

;
;        BANK     1
;
   org $2000
   rorg $3000
;         ORG      $3000

         STA      LBANK   
         JMP      START
         IF       PROM
JMPLO    STA      LBANK
         ELSE
JMPLO    JMP      BANK0
         ENDIF
         
BANK1
         STA      HSYNC
         LDX      #0
         STX      BKCLR
         STX      P0GR
         STX      P1GR
         STX      M0GR
         DEX
         TXS
;
;        OVERSCAN
;
         IF       PAL
OVSCAN   LDA      #$2C                    ;PAL TIMER
         ELSE
OVSCAN   LDA      #$23                    ;NTSC TIMER
         ENDIF
         STA      TIMD64
;
; JET COLLISIONS
;
         LDA      M1PPCX
         AND      #$C0    
         BEQ      NOJETCX   
         LDA      OBJAID
         CMP      #(VM0 & $FF)/6
         BNE      NOJETCX                 ;NO VMISSLE ALIVE
; BLOW UP JET
         LDX      #4                      ;POINT TO SATTELITE SCORE
         LDA      JETAID
         AND      #$FC    
         CMP      #(SAT0 & $FF)/6
         BEQ      SKIP118   
         LDX      #2                      ;POINT TO JET SCORE
SKIP118  LDA      #(EX0 & $FF)/6
         STA      JETAID
         STA      JETBID
         LDA      #0
         STA      OBJAX
         STA      OBJAID     
         STA      JETADELX     
         STA      JETADELY     
         TXA
         SEC
         JSR      SCORE
         LDX      #EXSND
         JSR      STARTSND
NOJETCX
;
;
;COLLISIONS
;
         LDA      #MAXCXS
         STA      NECOUNT
         BIT      SCREEN
         BPL      SKIP101
         LDA      PY
         CLC
         ADC      #$E-8
         STA      PY
SKIP101
         LDA      XX
         BNE      TOOUT05                 ;NO CXS IF RISING OR FALLING
         LDX      #4    
L1       TXA            
         TAY            
         DEY            
         BPL      SKIP55
TOOUT05  JMP      OUT05
SKIP55
;
         LDA      PID,X                   ; EXPLODING?
         AND      #$FC    
         CMP      #(EX0 & $FF)/6
         BEQ      NEXTCX1
L0
         LDA      PID,Y 
         AND      #$FC    
         CMP      #(EX0 & $FF)/6
         BEQ      NEXTCX
;
         LDA      PX,Y
         BEQ      NEXTCX   
         LDA      PX,X
         BEQ      NEXTCX1   
         CLC            
         ADC      #6    
         SEC            
         SBC      PX,Y
         CMP      #CXWINDX
         BCS      NEXTCX
; CHECK IN Y
;
         LDA      PY,X   
         CLC
         ADC      #$08    
         SEC
         SBC      PY,Y 
         CMP      #CXWINDY
         BCC      COLLIDE
;
NEXTCX   DEY            
         BPL      L0   
NEXTCX1  DEX
         BPL      L1
         JMP      OUT05
;
; COLLISION !!!!!!!
COLLIDE
         BIT      SCREEN
         BMI      SKIP104
         JMP      LANDCXS                 ;DOING LAND SCREEN
SKIP104
         LDA      PLANETCX
         BNE      TOOUT05   
         LDA      PID,X   
         AND      #$F0    
         CMP      #((PXA & $FF)/6) | $80
         BEQ      NEXTCX
         LDA      PID,Y 
         AND      #$F0    
         CMP      #((PXA & $FF)/6) | $80
         BEQ      NEXTCX   
         CPY      #0                      ;PLAYER INVOLVED
         BNE      OUT19
         LDA      PID                     ;PLAYER SHIELDED ?
         AND      #$FE    
         CMP      #(SPC & $FF)/6
         BEQ      TOOUT051                ;GO IF YES
;SPACE SCREEN HIT !!!!!
         CPX      #4                      ;LASER INVOLVED ?
         BNE      SKIP83
         BIT      M0PPCX
         BVC      NEXTCX   
KILLPL   LDA      #(EX0 & $FF)/6
         STA      PID
         LDY      #0
         STY      HSCROLL
         STY      PX,X   
         LDX      #EXSND
         JSR      STARTSND   
         LDA      #-$07
         STA      JETADELY     
TOOUT051 JMP      OUT05                   ;LEAVE CXS
SKIP83   LDA      PID,X   
         CMP      #((AST0 & $FF)/6) | $80
         BCS      KILLPL                  ;HIT BY ASTEROID
         AND      #$FE    
         CMP      #(CT & $FF)/6
         BEQ      NEXTCX   
         CMP      #(FB0 & $FF)/6
         BEQ      KILLPL   
         AND      #$F0    
         CMP      #((PXA & $FF)/6) | $80
         BEQ      NEXTCX1   
         STX      PLANETCX
         STY      OBJAY
         STY      SHUTDELY-1,X   
         STY      SHUTDELX-1,X            ;STOP OBJECT
         LDA      #-$07
         STA      JETADELY     
         LDA      #2    
         STA      SCRATE
         LDA      PY,X   
         BMI      STORE8                  ;OFF SCREEN
         CMP      #8    
         BCS      SKIP77
STORE8   LDA      #8    
         STA      PY,X   
SKIP77
         LDX      #LANDSND
         JSR      STARTSND   
         JMP      OUT05   
OUT19
; MISSSLE COLLISIONS
         CPX      #2    
         BNE      SKIP75
         CPY      #4    
         BEQ      SKIP120
         LDA      #0    
         STA      LASERX
         JMP      EXXX
SKIP120
;
         LDA      PID,Y 
         CMP      #((AST0 & $FF)/6) | $80
         BCC      DODIS
         JMP      EXX                     ;EXPLODE TORP
DODIS    LDA      #00                     ;DISAPPEAR TORP
         STA      PX,X   
         JMP      OUT05   
SKIP75   CPY      #2    
         BEQ      SKIP106                 ;DO NEXT COLLISION
         JMP      NEXTCX   
SKIP106
         CPX      #4    
         BNE      SKIP121
         LDA      #0    
         STA      LASERX
         JMP      EXX
SKIP121  LDA      PID,X
         CMP      #((AST0 & $FF)/6) | $80
         BCC      D0DISX
         JMP      EXXX
D0DISX   LDA      #00    
         STA      PX,Y 
         JMP      OUT05
;        COLLISIONS FOR LAND SCREEN
LANDCXS  STY      T2
         TXA            
         TAY            
         CPX      #3    
         BEQ      SKIP51
         CPX      #1    
         BNE      SKIP34
SKIP51   LDA      PID,X   
         LDY      #1    
         CMP      #(LND0 & $FF)/6
         BEQ      SKIP34   
         AND      #$FC    
         LDY      #5    
         CMP      #(TWR0 & $FF)/6
         BEQ      SKIP34   
         AND      #$F8    
         LDY      #1    
         CMP      #(S0 & $FF)/6
         BEQ      SKIP34   
         LDY      #3    
         CMP      #(CRY0 & $FF)/6
         BEQ      SKIP34   
         LDY      #6    
         CMP      #(R0 & $FF)/6
         BEQ      SKIP34   
SKIP34
         STY      T0
         LDY      T2
         TYA            
         STX      T2     
         TAX            
         CPY      #3                      ;LASER IS ALWAYS LASER
         BEQ      SKIP52
         CPY      #1                      ;MISSILE IS ALWAYS MISSILE
         BNE      SKIP35
SKIP52   LDA      PID,Y 
         LDX      #1    
         CMP      #(LND0 & $FF)/6
         BEQ      SKIP35   
         AND      #$FC    
         LDX      #5    
         CMP      #(TWR0 & $FF)/6
         BEQ      SKIP35   
         AND      #$F8    
         LDX      #1    
         CMP      #(S0 & $FF)/6
         BEQ      SKIP35   
         LDX      #3    
         CMP      #(CRY0 & $FF)/6
         BEQ      SKIP35   
         LDX      #6    
         CMP      #(R0 & $FF)/6
         BEQ      SKIP35   
SKIP35
;
         TXA            
         LDX      T2     
         ASL       
         ASL       
         ASL       
         EOR      T0     
         AND      #$38    
         EOR      T0     
         STY      T2     
         TAY            
         LDA      #>CXSERV
         STA      T1
         LDA      CXJUMPS,Y 
         STA      T0     
         LDY      T2     
         JMP      (T0)                    ;X AND Y NOT RETURNED UNSCATHED
OUT05
         BIT      SCREEN     
         BPL      SKIP102
         LDA      PY     
         SEC            
         SBC      #$E-8
         STA      PY     
SKIP102
;
; DO PLAYFIELD SCROLLING OFFSET
;
         LDA      FRAME
         AND      #$7    
         TAX            
         LDA      PHI,X 
         STA      T4                      ;PHI
         PHA
         CLC            
         ADC      HSCROLL
         LSR
         LSR
         LSR
         EOR      #$10
         SEC
         SBC      #$10
         STA      T5
         PLA
         CLC
         ADC      SCRATE
         LSR
         LSR
         LSR
         STA      T3
         CLC
         ADC      OFFSET
         TAY
         CPY      #$10
         BCC      SKIP00
         SBC      #$10
         TAY
         LDA      CLRPNT
         EOR      #(COLORS & $FF) ^ (COLORS1 & $FF)
         STA      CLRPNT
SKIP00   STY      OFFSET
;
; READ JOYSTICK
;         T0 GETS DELTA X
;
         LDX      PLRCTL
         INX
         BEQ      SKIP23
         LSR      FRAME+1
SKIP23
         LDX      #0
         STX      T0
         STX      T6
         LDA      FRAME
         BIT      SCREEN
         BPL      LANDACC
         AND      #0                      ;ACC FASTER IN SPACE
LANDACC  AND      #1                      ;ACCELERATION SPEED    
         STA      T1
         LDA      PLRCTL
         LDY      PLAYNO
         BEQ      SKIP05
         ASL
         ASL
         ASL
         ASL
SKIP05
;TURN OFF DEATH
         BIT      DSTAT
         BPL      SKIP109
         LDY      SND0
         CPY      #DEATHS
         BEQ      SKIP109
         LDY      SND1
         CPY      #DEATHS
         BEQ      SKIP109
         LDX      PLAYNO
         LDY      TRIG0,X                 ;STOP DEATH ON BUTTON
         BMI      CHKJOY                  ;NO TRIGGER
         LDY      #$FF                    ;DO BUTTON DEBOUNCE
         STY      DEBOUNCE
         BNE      DOSTAT
CHKJOY   CMP      #$F0                    ;JOYSTICK ACTIVE ?
         BCS      SKIP109
DOSTAT   LDY      #0    
         STY      DSTAT
SKIP109  LDX      #0    
         ASL       
         BCS      SKIP12
         DEX
SKIP12
         ASL       
         BCS      SKIP13
         INX
SKIP13
         LDY      XX
         BNE      ABORT1
         LDY      PLANETCX
         BNE      ABORT1
         STX      T0                      ;SAVE DELTA X FF,0,1
         STX      T9
         ASL
         LDY      T1                      ;DO EVERY EIGHT FRAMES
         BNE      ABORT1
         BIT      SCREEN
         BMI      SKIP153
         LDY      WAVENO
SKIP153
         LDX      MINSCR,Y
         STX      T2
         LDX      SCRATE
         BCS      SKIP14
         DEX
         BPL      SKIP95
         LDX      MINSCR,Y                ;KEEP POSITIVE SCRATE
SKIP95
         CPX      T2                      ;BELOW MINIMUM
         BCS      SKIP14
         INX
SKIP14   ASL
         BCS      SKIP15
         INX
         BIT      SCREEN     
         BPL      SKIP87
         CPX      #7    
         BCS      DEXIT1
SKIP87   CPX      #3 << 3
         BCC      SKIP15
DEXIT1   DEX
SKIP15   STX      SCRATE
ABORT1
;
; DECAY HSCROLL
;
         LDA      FRAME
         AND      #$1    
         BNE      OUT13
         LDX      HSCROLL
         BEQ      OUT13
         BPL      DEXIT
         INX
         JMP      SKIP96
DEXIT    DEX
SKIP96   STX      HSCROLL
OUT13
;
; MODIFY HORIZONTAL SCROLLING RATE
;
         LDA      T0                      ;GET DELTA X
         CLC
         ADC      HSCROLL
         TAX    
         BPL      SKIP45
         EOR      #$FF
         CLC
         ADC      #1
SKIP45   CMP      #HLIMIT
         BCS      SKIP62
         STX      HSCROLL
SKIP62
;
;        DELTA Y
;
         LDY      #4    
DELTLOOP LDA      SHUTX,Y
         BEQ      NEXTD
         LDA      SHUTDELY,Y
         CLC
         ADC      T4                      ;PHI+FRAME
         LSR
         LSR
         LSR
         EOR      #$10
         SEC
         SBC      #$10
         CLC
         ADC      SHUTY,Y
         CMP      #$F0
         BCS      SKIP56
         CMP      #HORIZON
         BCS      OFFLAND
SKIP56   CPY      #1    
         BEQ      OFFLAND
         CPY      #4
         BEQ      OFFLAND
         LDX      SHUTID,Y
         CPX      #(SAUCEND & $FF)/6
         BCC      OFFLAND                 ;SAUCER NOT SCRATED
         CPX      #((PC & $FF)/6)
         BEQ      OFFLAND
         CPX      #((PR & $FF)/6)
         BEQ      OFFLAND
         SEC
         SBC      T3
OFFLAND  BIT      SCREEN                  ;LET PLANETS GO NEGATIVE
         BPL      CMPF0
         CMP      #$E0
         BCC      OK08
         CMP      #$F0
         BCS      OK08
         BCC      KILL
CMPF0    CMP      #$F0    
         BCS      KILL   
OK08     STA      SHUTY,Y
;
;        DELTA    X
;
         LDA      SHUTDELX,Y
         CLC
         ADC      T4
         LSR
         LSR
         LSR
         EOR      #$10
         SEC
         SBC      #$10
         CLC
         ADC      SHUTX,Y
         BEQ      KILL
         CMP      #160+32                 ;SKIP16
         BCC      OK01
KILL     LDX      #(R0 & $FF)/6
         STX      SHUTID,Y
         LDA      #0
         CPY      #0                      ;DOING SHUTTLE ?
         BNE      OK00                    ;GO IF NO
         LDA      #HORIZON+$20
OK00     STA      SHUTY,Y
         LDA      #0
OK01     STA      SHUTX,Y
NEXTD    DEY
         BPL      DELTLOOP
NODELTA
         LDA      PX
         SEC
         SBC      T5
CHKAGIN  CMP      #MIDDLE+70
         BCS      SKIP39
         CMP      #MIDDLE-66
         BCC      SKIP39
         STA      PX
SKIP39
;
; SET UP LIST OF BANDS
;
         LDY      #LOWHGT-1-2
         BIT      SCREEN
         BPL      SKIP67
         LDY      #$FF
         LDA      #6    
         STA      BCOLOR
SKIP67   STY      LIST+12
         LDY      #11                     ;NUMBER OF BANDS
         LDA      #HORIZON
         SEC
         SBC      OFFSET
LISTLOOP
         STA      T0
         TAX
         LDA      PLOTY,X
         ASL
         CLC
         ADC      #5
         STA      LIST,Y
         LDA      T0
         SEC
         SBC      #$10
         CMP      #$E0
         BCC      ABORT0
         LDA      #0    
ABORT0   DEY            
         BPL      LISTLOOP
OVOUT    BIT      TIMOUT                  ; OVERSCAN TIMOUT
         BPL      OVOUT
         STA      HSYNC
;
;        SYNCH FOR NEW FRAME
;
SCRNTOP
         LDX      #2                      ; VBLANK ON (BIT 1)
         STX      VBLNK
         STX      HSYNC                   ; AND SYNCH FOR 3 LINES
         STX      VSYNC
;
;
;        ANIMATE PLAYER SHIP
;
         LDA      PID
         AND      #$FC                    ;EXPLODING ?
         CMP      #(EX0 & $FF)/6
         BNE      SKIP999
         STA      HSYNC
         BEQ      NOAMIM
SKIP999
         LDA      T9                      ;JOYSTICK DELAT
         EOR      #$FF
         CLC
         ADC      #1
         CLC
         ROL
         PHP
         PLA                              ;D0=SIGN, D1=ZERO
         ASL
         EOR      REFLS
         AND      #$6    
         EOR      REFLS
         AND      #$E    
         TAX
;
         LDA      PATBL,X                 ;ANIMATION TABLE
         STA      PID
         LDA      PATBL+1,X
         EOR      REFLS
         STA      REFLS
;
         STA      HSYNC
;
;
; TURN ON SHIELDS ?
;
         LDA      PLRCTL
         LDX      PLAYNO
         BNE      SKIP160
         LSR
         LSR
         LSR
         LSR
SKIP160  AND      #$0F    
         CMP      #$0D
         BNE      NOSHIELD
         LDA      TRIG0,X
         BMI      NOSHIELD
         BIT      SCREEN
         BPL      NOSHIELD
;SHIELDS ON
         LDA      #$FF
         STA      DEBOUNCE
         LDA      FRAME
         LSR
         LSR
         EOR      #(SPC & $FF)/6
         AND      #$1    
         EOR      #(SPC & $FF)/6
         STA      PID
NOSHIELD
NOAMIM
         STX      HSYNC
         STX      HSYNC
         LDX      #0                      ; OFF BIT 1 (THIS SAVES 1 BYTE)
         STX      VSYNC                   ; OFF GOES SYNC
         IF       PAL
         LDA      #$35                    ; TIMER
         ELSE
         LDA      #$2C                    ; TIMER
         ENDIF
         STA      TIMD64
;
;
;        VBLANK LOGIC
;
;
;
;        READ TRIGGER AND LAUNCH MISSLES
;
         LDA      PID                     ;DONT DO IF P EXPLODING
         AND      #$FC
         CMP      #(EX0 & $FF)/6
         BEQ      TONOTRIG
         LDA      PLRCTL
         LDX      PLAYNO
         BNE      SKIP40
         LSR
         LSR
         LSR
         LSR
SKIP40
         STA      T0
         LDY      TRIG0,X
         BMI      NOTRIG
;TRIG DOWN
         LDA      DEBOUNCE                ;MUST BE ZERO
         ORA      OBJAX
         ORA      DSTAT
         BNE      NOTRIG
         LDA      #$FF
         STA      DEBOUNCE
         LDX      #FIRES
         JSR      STARTSND
         LDA      T0                      ;GET STICK BACK
         BIT      SCREEN
         BMI      OUT10
         AND      #$F
         CMP      #$D                     ;DOWN
         BNE      SKIP41
;LAUNCH VERTICAL MISSLE         
         LDA      #(VM0 & $FF)/6
         STA      OBJAID
         LDA      PX
         STA      OBJAX
         LDA      PY
         CLC
         ADC      #HORIZON-LOWHGT+8
         STA      OBJAY
         LDA      #$20
         STA      OBJADELY
         LDA      #0
         STA      OBJADELX
TONOTRIG BEQ      NOTRIG
OUT10
;LAUNCH
SKIP41   LDA      #$18
         BIT      SCREEN
         BPL      SKIP140
         LDA      #$10
SKIP140
         STA      OBJADELY
         LDX      #((CT & $FF)/6)
         LDY      #00    
         LDA      PID
         CMP      #((PC & $FF)/6)
         BEQ      WRITEID                 ;CENTER SHIP, NO REFLECT
         LDX      #((RT & $FF)/6)
         LDY      #$07
         BIT      SCREEN
         BPL      SKIP141
         LDY      #$10
SKIP141
         LDA      REFLS
         AND      #8                      ;D3
         BEQ      SKIP17
         LDY      #-7
         BIT      SCREEN     
         BPL      SKIP142
         LDY      #-$10
SKIP142
SKIP17   LSR
         EOR      REFLS
         AND      #4    
         EOR      REFLS
         STA      REFLS
;
WRITEID  STY      OBJADELX
         LDA      PX
         STA      OBJAX
         LDA      PY
         STA      OBJAY
         STX      OBJAID
NOTRIG
;
; KILL TORPEDO ?
;
         LDA      OBJAID
         IF       PAL | REVA
         ELSE
         AND      #$FE
         ENDIF
         CMP      #(VM0 & $FF)/6
         BEQ      SKIP18
         LDA      OBJAY
;REVA CHANGES
         IF       PAL | REVA
         BIT      SCREEN
         BPL      NOTSP
         CMP      #$5F
         BCS      KILLTORP
         ENDIF
NOTSP    CMP      #HORIZON-16
         BCC      SKIP18
KILLTORP LDA      #$0    
         STA      OBJAX
SKIP18   BIT      SCREEN     
         BPL      DOLAND1
         JMP      DOSPACE1                ;DO SPACE SCREEN LOGIC
DOLAND1
;
; BIRTH LOGIC
;
         LDA      #$20
         LDY      WAVENO
         DEY
         BMI      SKIP112
         LDA      RAND
         AND      #$3F
SKIP112  STA      T1                      ;SAVE HORIZONTAL OFFSET
         LDX      #2
         LDA      XX
         BNE      TONEXTB
BIRTHLP  LDA      SHUTX,X
         BNE      TONEXTB
         CPX      #0                      ;DOING SHUTTLE
         BNE      SKIP152                 ;NO
;DOING SHUTTLE
         LDA      OBJBY
         CMP      #HORIZON/2
         BCC      SKIP152
TONEXTB  JMP      NEXTB
SKIP152
         LDA      #HORIZON-1
         STA      SHUTY,X
         LDA      RAND
         AND      #$7F
         CLC
         ADC      #32
         STA      SHUTX,X
         LDA      MODE
         AND      #$7F                    ;REMOVE D7
         BEQ      BANY
         CMP      #1
         BEQ      BRING
         CMP      #2
         BEQ      BTOWER
         BNE      DONT
;BIRTH TOWER
BTOWER   LDY      #(TWR0 & $FF)/6
         BNE      WRID
;BIRTH RING
BRING    LDA      XX
         BNE      DONT
         STX      T0
         TXA
         EOR      #2
         TAX  
         LDA      SHUTID,X
         AND      #$F8
         CMP      #(R0 & $FF)/6
         BNE      DORING
         LDA      SHUTY,X
         CMP      #HORIZON/3
         BCC      DORING
;DONT
         LDX      T0
DONT     LDY      #(EX0 & $FF)/6
         LDA      #0
         BEQ      WRID1
;BIRTH A RING
DORING
         LDX      T0
         LDA      #MIDDLE-$20
         CLC   
         ADC      T1
         LDY      #(R0 & $FF)/6
WRID1    STA      SHUTX,X
WRID     STY      SHUTID,X
         JMP      SKIP63
;LAUNCH ANYTHING
BANY     LDA      RAND
         AND      #7
         TAY  
         LDA      TERRAIN,Y
         STA      SHUTID,X
         CMP      #(CRY0 & $FF)/6
         BNE      SKIP63
;CRYSTAL BORN
         LDA      RADARX
         STA      SHUTX,X
;
; BIRTH NEW TARGET
;
         LDA      RAND
         AND      #$7F
         CLC
         ADC      #44
         STA      RADARX
SKIP63   LDA      #0    
         STA      SHUTDELY,X
         STA      SHUTDELX,X
         BEQ      OUT15
NEXTB    BIT      MODE
         BPL      OUT15                   ;ONLY BIRTH OBJB
         DEX
         DEX
         BMI      OUT15
         JMP      BIRTHLP
OUT15
OUT11
;
         JMP      NOSPACE1                ;SKIP SPACE SCREEN LOGIC
;
; SPACE SCREEN LOGIC
;
DOSPACE1
;
; MOVE LARGE PLANET DOWN
;
         LDA      FRAME
         AND      #$03
         BNE      SKIP81
         LDA      SND0
         CMP      #DEATHS
         BEQ      SKIP81
         LDA      SND1
         CMP      #DEATHS
         BEQ      SKIP81
         LDX      JETAY
         INX
         CPX      #$20
         BCS      SKIP81
         STX      JETAY
SKIP81
;
; BIRTH PLANET
;
         BIT      DSTAT
         BMI      TOOUT17
         LDA      PLANETCX
         BEQ      SKIP114
TOOUT17  JMP      OUT17
SKIP114
         LDX      #3
LOOP1
         LDA      SHUTX,X
         BNE      TONEXT01
         LDA      RAND
         AND      #$0F
         STA      T0
         AND      #$F
         TAY
         CPX      #00
         BNE      SKIP82                  ;DOING SHUTTLE ?
;BIRTH FIREBALL
         LDA      ATTRACT                 ;DONT LAUNCH IN ATTRACT
         CMP      #$F3
         BEQ      SKIP82
         LDA      #LOWHGT+SHUTHGT+$0E
         SEC
         SBC      JETAY
         SEC
         SBC      T0
         STA      SHUTY
         LDA      JETAX
         CLC
         ADC      #4    
         STA      SHUTX
         LDA      #(FB0 & $FF)/6
         STA      SHUTID
         LDA      #$F8
         STA      SHUTDELY
         LDA      RAND                    ;AIM FIREBALL
         AND      #$7
         TAY
         LDA      BALLDXS,Y
         STA      SHUTDELX
TONEXT01 JMP      NEXT01
TOLOOP1  JMP      LOOP1
SKIP82   LDA      #LOWHGT-$10             ;DOING LOWER PLANETS
         SEC
         SBC      T0
WRITEY   STA      SHUTY,X
         LDA      PLDXS,Y
         PHP
         STA      SHUTDELX,X
         LDA      PLYDYS,Y
         STA      SHUTDELY,X
         LDA      #24
         LDY      #$00
         PLP
         BPL      SKIP80
         LDA      #$B0
         DEY
SKIP80
         STA      SHUTX,X
         STY      T1                      ;DO REFLECTS
         LDA      REFLS
         EOR      T1
         AND      REFMASK1+1,X
         EOR      T1
         STA      REFLS
;DERIVE PLANET IDENTITY
         LDA      GAMNUM
         LSR
         TAY
         LDA      RANDH
         AND      PLMSKS,Y                ;VARIABLE MASK
         BEQ      NOAST
         LDA      #((AST0 & $FF)/6) | $80
         BNE      DOID                    ;ALWAYS
NOAST    LDA      RAND
         AND      #6
         CLC
         ADC      GAMNUM
         LSR
         TAY
         LDA      PLIDS,Y
DOID     STA      SHUTID,X
         JMP      OUT17
NEXT01   CPX      #2    
         BNE      SKIP60
         DEX
SKIP60   DEX
         BPL      TOLOOP1
OUT17
;
; ALTER DY
;
         LDA      FRAME
         AND      #7    
         BNE      OUT18
         LDX      #4    
LOOP2    CPX      PLANETCX
         BEQ      NEXT02
         INC      SHUTDELY-1,X
NEXT02   DEX
         CPX      #3
         BCS      LOOP2
OUT18
;
; EXECUTE FOR BOTH SCREENS
;
NOSPACE1
         INC      FRAME                   ; LOW FRAME INC
         BNE      NOINC                   ; BRANCH IF NO HIGH INC
         INC      FRAME+1                 ; HIGH FRAME INC
         BNE      SKIP162
         LDA      #$F3
         STA      ATTRACT
         LDA      #$80
         STA      SCREEN
         ASL
         STA      GAMESTAT
         STA      JETAX
SKIP162  LDA      GAMESTAT                ;GAME OVER ?
         BNE      NOATTR
         BIT      FLAGS
         BVS      NOATTR                  ;DOING VRSION NUMBER
         LDA      GAMNUM
         LSR
         BCC      NOATTR
         LDA      PLAYNO
         EOR      #1
         STA      PLAYNO
NOATTR
NOINC
         DEC      SELTIME
         BPL      SKIP44
         INC      SELTIME
SKIP44
;
;        GAME RESET AND GAME SELECT
;
         BIT      GAMESTAT                ;RESET GAME ON TRIGGER IF OVER
         BVS      SKIP46
         BIT      TRIG0
         BMI      SKIP46
         LDA      #$FF
         STA      DEBOUNCE
         LDA      NOTE                    ;LET TUNE SOUND PARTIALLLY
         CMP      #$0D
         BCC      NWGAME
SKIP46
         LDA      FPCTL
         LSR
         BCC      NWGAME
         LSR
         BCS      CONTVB
;SELECT SWITCH PRESSED
         LDA      SELTIME
         BNE      GMCONT
         STA      PLAYNO
         STA      GAMESTAT
         STA      PX
         LDA      #$1F    
         STA      SELTIME
         LDA      #$C0                    ;SET D6 AND D7 OF FLAGS
         STA      FLAGS
         INC      GAMSHAD
         LDA      GAMSHAD
         AND      #$7                     ;VERSIONS 0-7 ONLY
         STA      GAMSHAD
         STA      GAMNUM
         BPL      GMCONT
NWGAME   LDA      #$81                    ;GAME RESET BRINGS YOU HERE
         STA      FLAGS                   ;TURN OFF LOGO (SET D7)
;
;
; START NEW GAME
;
;
; ZERO SCORE ETC
;
         LDX      #ZEND-ZSTART-1
         LDA      #0    
ZEROLOOP STA      ZSTART,X
         DEX
         BPL      ZEROLOOP
         LDA      #MIDDLE
         STA      RADARX
         STA      JETAX
         STA      PX
         LDA      #8
         STA      PY
         STA      XTRALIFE
         LDA      #NOLIVES
         STA      LIVES
         STA      LIVES+1
         LDA      #$40                    ;YOU HERE
         STA      GAMESTAT                ;TURN OFF SOFT ATTRACT
         ASL
         STA      SCREEN
         LDA      #6
         STA      BCOLOR
         LDA      GAMSHAD
         STA      GAMNUM
;
GMCONT
         LDA      #$FF                    ;GAME SELECT SWITCH BRINGS
         STA      ATTRACT
;
;
;
CONTVB                                    ;NO SWITCHES BRING YOU HERE
;
; ANIMATE EVERYTHING
;
         LDX      #3
ANLOOP   LDA      PID,X
         BPL      SKIP71   
TONEXTA  JMP      NEXTA
SKIP71
;ANIMATE IN TIME
         CMP      #(ANEND1 & $FF)/6     ;END OF TIMED ANIMATIONS
         BCS      SKIP72
DOTIMED  LDA      FRAME                   ;USE D2
         LSR
         LSR
         EOR      PID,X
         AND      #$4
         EOR      PID,X   
         STA      PID,X
SKIP72   LDA      PID,X
         CMP      #(ANEND & $FF)/6
         BCS      SKIP92
DISTANCE LDA      PY,X
         CMP      #HORIZON
         BCS      NEXTA
         LSR
         LSR
         LSR
         LSR
         TAY
         LDA      SIZES,Y
         EOR      PID,X
         AND      #$3
         EOR      PID,X
         STA      PID,X
         JMP      NEXTA
SKIP92   AND      #$FC
         EOR      #(EX0 & $FF)/6        ;EXPLOSION ?
         BNE      NEXTA
         STA      SHUTDELY-1,X
         LDY      PID,X
         LDA      FRAME
         AND      #$7                     ;EXPLOSION RATE
         BNE      NEXTA
         INY
         CPY      #(EXE & $FF)/6
         BCC      SKIP32
         LDY      #(R0 & $FF)/6         ;KILLING PLAYER ?
         CPX      #0
         BNE      SKIP91
         STX      PX
         LDY      #(PC & $FF)/6
         STY      PID
;KILL PLAYER
         LDA      #$80                    ;HALT GAME
         STA      DSTAT
         STX      T0
         LDA      #2
         STA      SCRATE
         LDX      PLAYNO
         DEC      LIVES,X
;
; INIT SPACE
;
         LDX      #DEATHS
         JSR      STARTSND
         JMP      SKIP74                  ;LEAVE
;
SKIP91
         CPX      #1
         BNE      SKIP57
         LDA      #HORIZON+$20
         STA      PY,X
SKIP57   LDA      #0
         STA      PX,X
SKIP32   STY      PID,X
NEXTA    DEX
         BMI      SKIP74
         JMP      ANLOOP
SKIP74
;
; TURN ON PLAYER AFTER DEATH SOUND
         LDX      #1
LOOP3    LDA      SND0,X
         CMP      #DEATHS
         BNE      NEXTCK
         LDA      SND0TIME,X
         CMP      #1
         BNE      NEXTCK
         LDX      PLAYNO
         LDA      GAMNUM
         LSR
         BCC      ONEPLR
         TXA
         EOR      #1
         TAX
         LDA      LIVES,X
         BMI      ONEPLR
         STX      PLAYNO
ONEPLR
;CHECK FOR GAME OVER
         BIT      LIVES
         BPL      OK04
         LDA      GAMNUM
         LSR
         BCC      GAMEOVER
         BIT      LIVES+1
         BPL      OK04
;GAME OVER
GAMEOVER LDX      #$3F
         STX      SND0TIME
         LDX      #EOG
         STX      SND0                    ;TURN ON END OF GAME
         LDA      #0
         STA      SND1
         LDA      #$20                    ;DURATION
         STA      SND0TIME
         LDA      #$0F
         STA      NOTE
         LDA      #$0D
         STA      SNDV0
         LDA      #$C
         STA      SNDC0
         LDA      #$F
         STA      SNDF0
         LDA      #0
         STA      GAMESTAT
         STA      LIVES
         STA      LIVES+1
         STA      DSTAT
         BEQ      OUT08                   ;ALWAYS
OK04
         STA      JETAY
         LDA      #MIDDLE
         STA      JETAX
         STA      PX
         BNE      OUT08
NEXTCK   DEX
         BPL      LOOP3
OUT08
;
; DECIDE WHICH SCREEN TO DO
;
         BIT      SCREEN
         BPL      DOLAND
TODOPL   JMP      DOPLANET                ;DO SPACE SCREEN
;
;        LOGIC FOR LAND SCREEN
;
DOLAND
         LDA      XX
         BEQ      DODOLAND
         JMP      JETANIM
DODOLAND
;
; SHUTTLE INTELLIGENCE
;
         LDY      #2                      ;DO SHUT AND OBJB
ILOOP    LDA      SHUTID,Y 
         CMP      #(LND0 & $FF)/6
         BEQ      SKIP98
         CMP      #(SAUCEND & $FF)/6
         BCC      SKIP98
TONEXTI  JMP      NEXTI
SKIP98
         LDA      FRAME
         AND      #$1
         BEQ      SKIP97
         JMP      OUT02
SKIP97
         LDA      SHUTX,Y
         LDX      PX
         CPX      #MIDDLE-$20
         BCC      SKIP99
         CPX      #MIDDLE+$20
         BCC      USEPX
SKIP99   LDX      #MIDDLE
USEPX    STX      T0
         LDX      #$01
         CLC
         ADC      #$10
         CMP      T0
         BCC      SKIP19
         LDX      #0
         SEC
         SBC      #$20
         CMP      T0
         BCC      SKIP19
         LDX      #$FF
SKIP19   TXA
         CLC
         ADC      SHUTDELX,Y
         BVS      SKIP20
         STA      SHUTDELX,Y
SKIP20
;INTELLIGENCE FOR LOWER SCREEN
         LDX      SHUTY,Y
         CPX      #HORIZON
         BCC      SKIP93
         LDA      SHUTDELY,Y
         BMI      TONEXTI
         LDX      #$FE
         JMP      SKIP21
SKIP93   LDA      SHUTID,Y
         CMP      #(LND0 & $FF)/6
         BNE      SKIP103
         LDA      #(S0 & $FF)/6
         STA      SHUTID,Y                ;STORE SAUCER IN SHUTTLE
SKIP103  TXA
;
         LDX      #$FF
         CMP      #$38
         BCS      SKIP21
         DEX
         LDA      RAND
         AND      #$A4
         BEQ      SKIP21
         INX
         INX
         INX
SKIP21   TXA
         CLC
         ADC      SHUTDELY,Y
         
         TAX
         BPL      SKIP94
         EOR      #$FF
         CLC
         ADC      #1
SKIP94   CMP      #2 << 3
         BCS      SKIP22
         STX      SHUTDELY,Y
SKIP22
;
; MOVE SHUTTLE TO OBJB
;
         LDX      PLANETCX
         BNE      OUT99
         LDX      OBJBX
         BNE      OUT99
         LDA      SHUTY
         BMI      NEXTI
         STA      OBJBY
         LDA      SHUTX
         STA      OBJBX
         LDA      SHUTID
         STA      OBJBID
         LDA      #5
         STA      SHUTID
         STX      SHUTX
         LDA      #HORIZON+$20
         STA      SHUTY
         LDA      SHUTDELX
         STA      OBJBDELX
         LDA      SHUTDELY
         STA      OBJBDELY
OUT99
;
; DROP A MINE
;
DROPMINE
         LDA      FRAME
         AND      #$3F
         BNE      OUT02
         LDA      LASERX
         BNE      OUT02
         LDA      SHUTX,Y
         STA      LASERX
         SEC
         SBC      PX
         CMP      #$80
         ROR
         EOR      #$FF
         CMP      #$80
         ROR
         STA      LASDELX
         LDA      #$F4
         STA      LASDELY
         LDA      SHUTY,Y
         STA      LASERY
OUT02
NEXTI    DEY
         DEY
         BMI      SKIP25
         JMP      ILOOP
SKIP25
;
; JET INTELLIGENCE
;
         LDX      JETAX
         BEQ      BSATT                   ;DEAD BIRTH NEW ONE
         LDA      JETAID
         AND      #$FE
         CMP      #(SAT0 & $FF)/6
         BEQ      TOOUT04
         AND      #$FC
         CMP      #(EX0 & $FF)/6
         BEQ      TOOUT04                 ;EXPLODING
         LDA      WAVENO
         ASL
         ASL
         ASL
         ADC      #$04
         STA      T0                      ;LO LIMIT FOR LAUNCH ETC.
         BNE      OUT03
; SATTELITE OR MOTHER SAUCER ?
BSATT    LDA      RANDH
         AND      #$AA
         BNE      NOSAT1
         LDA      FRAME
         AND      #$F
         BNE      NOSAT1
;BIRTH SATTELITE
         STA      JETADELY
         LDA      #(SAT0 & $FF)/6
         STA      JETAID
         STA      JETBID
         LDA      #$10
         STA      JETAY
         LDA      RAND
         LSR
         LDX      #$F8
         LDY      #159
         BCC      SKIP122
         LDX      #$08
         LDY      #01
SKIP122
         STX      JETADELX
KILLJET  STY      JETAX
         JMP      NOTRIG1
NOSAT1
         LDY      #0
         BIT      MODE
         BMI      KILLJET                 ;NOT CORRECT MODE TO LIVE
         STX      JETAID
         STX      JETADELY
         STX      JETADELX
         STX      JETAY
         LDA      RAND
         CMP      #32+160
         BCC      SKIP42
         AND      #$7F
SKIP42   STA      JETAX
OUT03
         LDA      FRAME
         AND      #$0F
         BEQ      SKIP90
TOOUT04  JMP      OUT04
SKIP90
;
; X INTELLIGENCE
;
         LDA      JETAX
         CMP      #MIDDLE
         LDX      #$01
         BCC      SKIP27
         LDX      #$FF
SKIP27
         TXA
         CLC
         ADC      JETADELX
         BVS      SKIP30
         STA      JETADELX
SKIP30
;
; JET Y INTELLIGENCE
;
         LDA      JETADELY
         LDX      #$FF
         LDY      SHUTX
         BNE      OK03                    ;CLIMB IF SHUTTLE ON
         BIT      DSTAT
         BMI      OK03                    ;CLIMB IF PLAYER DEAD
;
         BIT      MODE
         BMI      OK03                    ;CLIMB IF NO LAUNCH
         LDX      #1                      ;DIVE IF ABOVE LO LIMIT
         LDY      JETAY
         CPY      T0
         BCC      OK03
         LDX      #$FF                    ;LEVEL OFF
         LDY      JETADELY
         BNE      SKIP24
         LDX      #0
SKIP24   BPL      SKIP28
         LDX      #1
SKIP28
OK03     TXA
         CLC
         ADC      JETADELY
         BVS      SKIP29
;
         PHA
         BPL      SKIP31
         EOR      #$FF
         CLC
         ADC      #1
SKIP31   CMP      #5
         PLA
         BCS      SKIP29
         STA      JETADELY
SKIP29
;
; LAUNCH SHUTTLE
;
         LDA      JETAX
         BEQ      OUT04
         LDA      JETADELY
         ORA      SHUTX
         BNE      OUT04
         LDA      JETAY
         CMP      T0                      ;LOLIMIT
         BCC      OUT04
;LAUNCH ONLY ONE SHIP FOR LOW WAVES
         LDA      OBJBID
         AND      #$F8
         CMP      #(S0 & $FF)/6
         BNE      GO00
         LDA      WAVENO
         CMP      #2
         BCC      OUT04
GO00     LDA      #(LND0 & $FF)/6
         STA      SHUTID
         LDA      JETAX
         CLC
         ADC      #8
         STA      SHUTX
         LDA      ENTER
         CLC
         ADC      #LOWHGT-SHUTHGT+7+16
         CLC
         ADC      DIFF
         STA      SHUTY
         LDX      WAVENO
         CPX      #3
         BCC      SKIP26
         LDX      #3
SKIP26
         LDA      JETADELX
         STA      SHUTDELX
         LDA      LSPEEDS,X
         STA      SHUTDELY
OUT04
NOTRIG1
;
;        ANIMATE JET
; D2=FRAME BIT D1=XZERO D0=XSIGN
;
JETANIM  LDX      JETAID
         TXA
         AND      #$FC
         CMP      #(EX0 & $FF)/6
         BEQ      DOEXPL
         CMP      #(SAT0 & $FF)/6
         BNE      DOJET
;ANIMATE SATTELITE
         LDA      FRAME
         LSR
         LSR
         EOR      JETAID
         AND      #$1
         EOR      JETAID
         STA      JETAID
         STA      JETBID
         LDA      #$20
         BNE      WREFLS                  ;DO REFLECTS
DOEXPL   LDA      FRAME
         AND      #3
         BNE      OUT12
         INX
         CPX      #(EXE & $FF)/6
         BCC      OK05                    ;EXPLOSION NOT OVER YET
;KILL JET
         LDX      #0
         STX      OBJAX
         STX      OBJAID
         STX      JETAX
         BEQ      DOJET                   ;BUT ANIMATE
OK05     STX      JETAID
         STX      JETBID
         JMP      OUT12
;
DOJET    LDA      JETADELX
         ASL
         PHP
         LDA      FRAME
         AND      #4                      ;D2
         TSX
         EOR      0+1,X
         AND      #4
         EOR      0+1,X
         PLP                              ;RESTORE STACK
         AND      #7
         STA      T0
         ASL
         ADC      T0
         TAX
         LDA      JETIDS,X
         STA      JETAID
         LDA      JETIDS+1,X
         STA      JETBID
         LDA      JETIDS+2,X
WREFLS   EOR      REFLS
         AND      #$30
         EOR      REFLS
         STA      REFLS
OUT12
         JMP      NOPLANET
DOPLANET
;
; ANIMATE LARGE PLANET ONTO JET AREA
;
         LDA      FRAME
         AND      #$1
         BNE      OUT07
         LDA      RAND
         AND      #$7
         TAY
         LDA      PLNTS,Y
         STA      JETAID
         LDA      RANDH
         AND      #$7
         TAY
         LDA      PLNTS,Y
         STA      JETBID
         LDA      REFLS                   ;D4-D5=JET REFL
         ORA      #$20
         AND      #~10
         STA      REFLS
OUT07
;
; ANIMATE PLANET
; DO 3 AND 1
;
         LDX      #3
PLANETLP LDA      PID,X
         BMI      SKIP115
;ANIMATE FIREBALL
         AND      #$FE
         CMP      #(FB0 & $FF)/6
         BNE      TONPL                   ;NOT FIREBALL
         LSR      PID,X
         LDA      FRAME
         LSR
         LSR   
         ROR
         ROL      PID,X
         JMP      TONPL                   ;DONEXT
SKIP115  LDA      PX,X
         BNE      DOPL
TONPL    JMP      NEXTPL
;ANIMATE ONE PLAYER PLANETS
DOPL
         LDA      PY,X
         LDY      PLANETCX
         BEQ      ONE
         CPX      PLANETCX
         BNE      ONE
         LDA      PY,X
         CMP      #9
         BCC      NOONE                   ;2 PLAYER PLANETS
ONE
         LDY      PID,X
         CPY      #((AST0 & $FF)/6) | $80
         BCC      OK07                    ;PROCESS NEXT EXPLOSIONS
; ANIMATE ASTEROIDS
         LDA      SHUTDELY-1,X
         CLC
         ADC      #$2
         ROL
         AND      #$F8
         PHP
         PLA
         AND      #3
         TAY
         LDA      ASTIDS,Y
         STA      PID,X
         JMP      TONEXTPL
OK07     LSR
         LSR
         LSR
         LSR
         TAY
         LDA      SIZES1,Y
         EOR      PID,X
         AND      #3
         EOR      PID,X
         STA      PID,X
TONEXTPL JMP      NEXTPL
;2 PLAYER PLANETS
NOONE
         LDA      PID,X
         AND      #~$C                ;REMOVE COLOR
         LDY      PY,X
         BEQ      SWITCH
         CPY      #3
         BCS      SKIP130
         JMP      PLAND
SKIP130  CPY      #5
         BCC      PLANC
         CPY      #7
         BCC      PLANB
         JMP      PLANA                   ;A=1
;
; SWITCH SCREENS
;
SWITCH   LDA      #((PC & $FF)/6)
         STA      SHUTID
         LDA      PX
         STA      SHUTX
         LDA      #$A8
         STA      SHUTY
         LDA      #$-8
         STA      SHUTDELY
;INCREMENT WAVE NUMBER
         LDA      OBJAID
         LSR                       ;WITHDRAW COLOR
         LSR
         AND      #3
         TAX
         STX      WAVENO
         LDA      PCOLORS,X
         STA      BCOLOR
         LDA      #0
         STA      OBJAX
         STA      OBJBX
         STA      JETAX
         STA      OBJAID
         STA      OBJBID
         STA      SCREEN
         STA      PX
         STA      SHUTDELX
         LDA      #$30
         STA      SCRATE
         LDA      #$60
         STA      XX
         JMP      OUT06
;
PLANA
         CMP      #((PXA & $FF)/6) | $80
         BEQ      SKIP68
;
         LDA      PX,X
         SEC
         SBC      #4
         BEQ      LDA1
         BCS      OK06
LDA1     LDA      #1
OK06
         STA      PX,X
         CLC
         ADC      #8
         STA      OBJAX
         LDA      #0
         STA      SHUTDELY-1,X
         STA      SHUTDELX-1,X
         STA      OBJADELX
         STA      OBJADELY
SKIP68
         LDA      #((PXA & $FF)/6) | $80
         BNE      NEXTPL1
PLANB    LDA      #((PXB & $FF)/6) | $80
         BNE      NEXTPL1
PLANC
         CMP      #((PXC & $FF)/6) | $80
         BEQ      SKIP69
;
         LDA      PX,X
         SEC
         SBC      #8
         BEQ      LDA2
         BCS      OK09
LDA2     LDA      #1
OK09
         STA      PX,X
         CLC
         ADC      #16
         STA      OBJAX
SKIP69
         LDA      #((PXC & $FF)/6) | $80
         BNE      NEXTPL1
PLAND
         CMP      #((PXD & $FF)/6) | $80
         BEQ      SKIP70
;
         LDA      PX,X
         SEC
         SBC      #16
         BEQ      LDA3
         BCS      OK99
LDA3:    LDA      #1
OK99
         STA      PX,X
         CLC
         ADC      #32
         STA      OBJAX
SKIP70
         LDA      #((PXD & $FF)/6) | $80
NEXTPL1  EOR      PID,X
         AND      #~$C
         EOR      PID,X
         STA      PID,X
         STA      OBJAID
         CPX      #1                      ;SHUTTLE?
         BNE      SKIP76
         DEY
SKIP76   STY      OBJAY
         LDA      #4                      ;DO REFLECTS
         ORA      REFLS
         AND      REFMASK1,X
         STA      REFLS
NEXTPL   DEX
         DEX
         BMI      OUT06
         JMP      PLANETLP
OUT06
;
NOPLANET
;
; SCROLL OFF AND ON A PLANET
;
         LDA      FRAME
         LSR
         BCS      SKIP107
TOSKIP33 JMP      SKIP33
SKIP107
         BIT      SCREEN
         BMI      TOSKIP33
         BIT      DSTAT
         BMI      DENTRY
         LDA      SCRATE
         CMP      #4 << 3
         BCC      TOSKIP33
         LDX      XX
         LDA      PUCNT
         BNE      SKIP145
         JMP      GODOWN
SKIP145
;LAUNCH SHIP FROM PLANET
         CPX      #0                      ;FIRST PASS ?
         BNE      SKIP73
         LDA      #((PC & $FF)/6)
         STA      SHUTID
         LDA      PX
         STA      SHUTX
         LDA      #HORIZON-LOWHGT+14
         STA      SHUTY
         LDA      #8
         STA      SHUTDELY
         LDA      #0
         STA      SHUTDELX
         STA      PX
         LDA      #$-8
         STA      JETADELY
SKIP73
         LDA      #3
         CLC
         JSR      SCORE
DENTRY   LDX      XX                      ;DEATH SCROLL OFF PLANET ENTRY
         INX
         INX
         CPX      #LOWHGT-1
         BCC      STXXX
;  LIFT OFF COMPLETE
;
; INCREMENT GAME NUMBER ?
;
         BIT      DSTAT                   ;NOT IF DYING
         BMI      SKIP144
         LDX      PLAYNO
         LDA      WAVEMSK,X
         ORA      WAVEINC
         STA      WAVEINC                 ;BOTH PLAYERS HAVE LIFTED
         LDA      GAMNUM
         LSR
         BCC      DOWINC
         LDA      WAVEINC
         CMP      #3
         BNE      SKIP144
DOWINC   LDA      #0
         STA      WAVEINC
         LDA      GAMNUM
         CLC
         ADC      #2
         CMP      #8
         BCS      SKIP144                 ;DONT FLOW GAME NUMBERS
         STA      GAMNUM
SKIP144
         LDA      #$80
         STA      SCREEN
         LDA      #$FF    
         STA      LIST+12
         LDA      #6
         STA      BCOLOR
         LDX      #2                      ;ZERO SCRATE
         LDA      #0
         STA      MODE     
         STA      SCRATE     
         STA      PUCNT     
         STA      OBJAX     
         STA      OBJBX     
         STA      JETADELX     
         STA      JETADELY     
         STA      JETAY     
         LDA      #MIDDLE
         STA      JETAX     
         LDA      #$6
         STA      PY
;RESTORE PLAYERS
         LDA      #0
         BIT      DSTAT     
         BMI      RESTORE
         LDA      #MIDDLE
RESTORE  STA      PX     
         LDA      #0
         STA      SHUTX
;GOING DOWN
GODOWN   DEX            
         DEX            
         BPL      STXXX
;BOTTOMED OUT
         LDA      #$18    
         STA      SCRATE     
         LDA      #0    
;        STA      LASERX
;        STA      OBJAX
;        STA      OBJBX
;        STA      OBJAID
         STA      PLANETCX
         LDA      SHUTX     
         LDX      #2    
         BNE      RESTORE   
STXXX    STX      XX
SKIP33
;
; FIGURE WHO'S ON THIS FRAME
;
         LDA      JETAY     
         BPL      SKIP111
         LDA      #0
         STA      JETADELY     
         STA      JETAY     
SKIP111
         LDA      SHUTY     
         STA      T3
         LDX      SHUTX     
         LDY      SHUTID     
         LDA      OBJAX     
         BEQ      NOFLICK
         LDA      OBJAID     
         CMP      #(VM0 & $FF)/6
         BNE      NOFLICK   
         LDA      OBJAY
         CMP      #HORIZON
         BCC      NOFLICK
;FLICKER VERTICAL MISSLE AND SHUTTLE
         PHA
         LDA      FRAME     
         LSR       
         PLA            
         BCC      NOFLICK   
DOM      LDA      OBJAY
         LDX      OBJAX     
         LDY      OBJAID
SKIP49
         STA      T3
NOFLICK  STX      T4
         STY      T5
;
         TYA            
         AND      #$FE    
         CPY      #(VM0 & $FF)/6
         SEC
         BEQ      SKIP48
         CMP      #(PC & $FF)/6)
         SEC
         BEQ      SKIP48   
         CLC
SKIP48   LDA      T3
         JSR      SCREENY
         STA      SHUTSY
         LDA      PY                      ;PY
         STA      PSY
         LDX      #0                      ;POINT TO PLAYER'S SHIP
         LDA      PX     
         BEQ      DOSHUT
         LDA      T3
         BIT      SCREEN     
         BPL      SKIP113
         CMP      #LOWHGT+8
         BCS      OUT00
SKIP113
         CMP      #HORIZON+$10            ;DISTANCE OK ?
         BCS      OUT00
; ODD OR EVEN FRAME
         LDA      FRAME     
         LSR       
         BCS      OUT00   
DOSHUT   INX            
OUT00    LDA      PSY,X                   ;LOAD POSITION OF ON GRAPHIC
         STA    DIFF
;SET UP POINTERS
         TXA            
         LSR       
         ROR       
         STA      PNTS
;
; MODIFY MAIN SCREEN
;
         LDA      LASERY
         CLC            
         JSR      SCREENY   
         AND      #$FE    
         STA      LHGT
         LDX      #13
LOOP0    LDA      LIST,X   
         SEC
         SBC      DIFF     
         STA      LIST,X   
         DEX
         BPL      LOOP0
;
; SET UP MAIN SCREEN
;
; SET UP PLAYER 1
;
         LDX      #02    
         LDA      OBJAX     
         BEQ      DOB
         LDA      OBJAY
         CMP      #HORIZON+$10
         BCS      DOB   
         LDA      FRAME     
         LSR
         BCC      OUT01
; ODD FRAME
DOB      LDA      OBJBX     
         BEQ      OUT01   
         INX
OUT01
         TXA
         ORA      PNTS
         STA      PNTS
;
         LDY      PID,X
         LDA      PY,X
         CPY      #(VM0 & $FF)/6
         CLC
         BNE      SKIP50
         SEC
SKIP50   LDA      PY,X
DOSCRY1  JSR      SCREENY
         SEC
         SBC      DIFF                    ;ADJUST FOR
         STA      P1Y
         JMP      JMPLO                   ;GO TO BANK 0
PLOTY
         DC       -2,-1,0,1,2,3,4,5,6,07
         DC       8,9,10,11,12,13
         
         DC       14,14,15,15,16,16,17,17
         DC       18,18,19,19,20,20,21,21
         
         DC       22,22,23,23,24,24,25,25
         DC       26,26,27,27,28,28,29,29
         
         DC       30,30,30,30,31,31,31,31
         DC       32,32,32,32,33,33,33,33
         
         DC       34,34,34,34,35,35,35,35
         DC       36,36,36,36,37,37,37,37
         
         DC       38,38,38,38,38,38,38,38
         DC       39,39,39,39,39,39,39,39
         
         DC       40,40,40,40,40,40,40,40
         DC       41,41,41,41,41,41,41,41
         
         DC       42,42,42,42,42,42,42,42
         DC       42,42,42,42,42,42,42,42
         
         DC       43,43,43,43,43,43,43,43
         DC       43,43,43,43,43,43,43,43
         
         DC       44,44,44,44,44,44,44,44
         DC       44,44,44,44,44,44,44,44
;
;
;        SUBROUTINES
;
;
;
;
; Y TO SCREEN Y
;  IF CARRY SET, ASSUME THAT IT IS OFF LAND
;
SCREENY  BIT      SCREEN
         BMI      EVENLINE                ;DONT PLOT FOR SPACE
         BCS      JUSTSUB                 ;POSITIVE POSITION, ON BOTTOM
         CMP      #HORIZON+1
         BCC      SKIP06
         SEC
JUSTSUB  SBC      #HORIZON-LOWHGT+6
         RTS

SKIP06
         TAX            
         LDA      PLOTY,X 
         ASL
WASTE12  RTS
EVENLINE
         RTS
;
; SCORE
; A HAS BYTE, CARRY CLEAR= ADD TO LO, CARRY SET= ADD TO MID SCORE
;

SCORE
         STX      T7
         STY      T8
         PHP
         ASL
         ASL
         CLC
         ADC      WAVENO     
         TAX
         LDA      SCORETBL,X
         PLP
         SED
         LDX      PLAYNO
         LDY      PLAYNO                  ;PLAYER UP
         BCC      SKIP78                  ;ADD TO LO BYTE
         INY                              ;POINT TO MID BYTE
         INY
;
SKIP78   CLC
SCORLOOP ADC      SCOREL0,Y
         STA      SCOREL0,Y
         BCC      OK10
         INY
         INY
         LDA      #1
         CPY      #6    
         BCS      OK10   
         CPY      #4    
         BCC      SCORLOOP   
         DEC      XTRALIFE     
         BMI      NOXTRA
         INC      LIVES,X   
NOXTRA   BCS      SKIP78   
         BCC      SCORLOOP   
OK10     CLD
         LDX      T7
         LDY      T8
         RTS
;
; START SOUND, HIGHEST NUMBER SOUND GETS PRIORITY
;         X HAS SOUND NUMBER
;
STARTSND
         LDY      #0    
         LDA      SNDNOS,X
         LDY      SND0TIME     
         BEQ      STARTIT
         LDY      #1
STARTLP  LDA      SNDNOS,X 
         CMP      SND0,Y
         BCC      NEXTST
STARTIT  STA      SND0,Y
         LDA      SNDTIMS,X 
         STA      SND0TIME,Y
         JMP      OUT25
NEXTST   DEY
         BPL      STARTLP   
OUT25    RTS
REF      DC       $0,0,0,0,0,DR,UR,R
         DC       0,DL,UL,L,0,D,U,C
SIZES1   DC       3,3,2,1,0,0,0,0,0,0,0,0,0,0,3,3
;

;
;
;
; COLLISION SERVICES
;
         BOUNDRY  0
;
; EXPLODE BOTH
;
CXSERV
EXSCX
         STX      T0
         TYA            
         TAX            
         LDY      T0
EXSC
         STX      T2     
         LDA      #$01    
         CLC            
         JSR      SCORE   
         LDX      #EXSND
         JSR      STARTSND   
         LDX      T2     
         LDA      #(EX0 & $FF)/6
         STA      PID,X   
         STA      PID,Y
         LDA      #0
         STA      SHUTDELX-1,X
         STA      SHUTDELX-1,Y
         STA      SHUTDELY-1,X
         STA      SHUTDELY-1,Y
         JMP      JMPBK
;
; EXPLODE BOTH    CHECK X FOR BEING VERITCAL MISSLE
;DISAPPEAR X
EXBX     STX      T0     
         TYA            
         TAX            
         LDY      T0     
EXB      LDA      PID,X   
         CMP      #(VM0 & $FF)/6
         BNE      SKIP117
         LDA      PY,Y                    ;CHECK FOR ABOVE HORIZON
         CMP      #HORIZON+$10
         BCC      TOJMPBK   
SKIP117  LDA      #(EX0 & $FF)/6
         STA      PID,Y
         LDA      #0
         STA      PX,X   
         STA      SHUTDELX-1,Y
         STA      SHUTDELY-1,Y
         LDX      #EXSND
         JSR      STARTSND   
         JMP      JMPBK
;
; DISAPPEAR AND SCORE
;
DSCX
;SCORE HERE
         STX      T0     
         TYA            
         TAX            
         LDY      T0
DSC
         STX      T2     
         LDA      PUCNT     
         CMP      #6    
         BCS      TOJMPBK   
         LDA      #$00    
         CLC            
         JSR      SCORE   
         LDX      #MANSND
         JSR      STARTSND   
         LDX      PUCNT     
         CPX      #6    
         BCS      SKIP105
         INX
SKIP105  STX      PUCNT     
         LDX      T2
SKIP64
         LDA      #0    
         STA      PX,X
;
TOJMPBK  JMP      JMPBK
; DISAPPEAR X
;
DISX     STX      T0     
         TYA            
         TAX            
         LDY      T0
DIS
         LDA      #0    
         STA      PX,X   
         LDX      #MANSND
         JSR      STARTSND   
         JMP      JMPBK
;
; ACCELERATE/DECELERATE SCROLL RATE
;
ACC
         LDA      #$1    
         CLC
         ADC      SCRATE     
         STA      SCRATE
         JMP      JMPBK
;
; EXPLODE X; NO EFFECT
;
EX1X     STX      T0
         TYA
         TAX
         LDY      T0
EX11     LDA      PID,X   
         CMP      #(VM0 & $FF)/6
         BEQ      BACK0
         LDA      #(EX0 & $FF)/6
         STA      PID,X   
         LDA      #0    
         STA      SHUTDELX-1,X
         STA      SHUTDELY-1,X
         LDX      #EXSND
         JSR      STARTSND   
BACK0    JMP      JMPBK
;
; EXPLODE LASER
;
LEX      BIT      M0PPCX
         BMI      DOLEX
         BVC      NE   
DOLEX    LDA      #0    
         STA      LASERX     
         LDA      #(EX0 & $FF)/6
         STA      PID,X   
         STA      PID,Y 
         LDX      #EXSND
         JSR      STARTSND   
         JMP      JMPBK
;
; NO EFFECT
;
NE       DEC      NECOUNT     
         BMI      JMPBK   
         JMP      NEXTCX                  ;LOOK FOR MORE CXS
;
; TORPEDO/XX      EXPLODE OBJECT, DISAPPEAR TORP
;         TORPEDO IN X
;
EXXX     STX      T0
         TYA
         TAX
         LDY      T0
EXX
         LDA      #$01    
         CLC
         JSR      SCORE
;
         LDA      PID,X   
         CMP      #(VM0 & $FF)/6
         BNE      SKIP53   
         LDA      PY,Y
         CMP      #HORIZON+$10
         BCC      JMPBK
SKIP53
         LDA      #0    
         STA      PX,X   
         STA      SHUTDELY-1,Y
         STA      SHUTDELX-1,Y
         LDA      #(EX0 & $FF)/6
         STA      PID,Y
         LDX      #EXSND
         JSR      STARTSND   
JMPBK    JMP      OUT05
;
;
;        DATA FOR BANK1
;
;
;
; COLLISION JUMP TABLE
;
CXJUMPS
         DC       <NE                   ;PLAYER/PLAYER
         DC       <EXB                  ;SHUTTLE/PLAYER
         DC       <NE                   ;MISSILE/PLAYER
         DC       <DSC                  ;CRYSTAL/PLAYER
         DC       <LEX                  ;LASER/PLAYER
         DC       <EX1X                 ;TOWER/PLAYER
         DC       <ACC                  ;RING/PLAYER
         DC       <NE
         DC       <EXBX                 ;PLAYER/SHUTTLE
         DC       <NE                   ;SHUTTLE/SHUTTLE
         DC       <EXX                  ;MISSLE/SHUTTLE
         DC       <DIS                  ;CRYSTAL/SHUTTLE
         DC       <NE                   ;LASER/SHUTTLE
         DC       <EX1X                 ;TOWER/SHUTTLE
         DC       <EX1X                 ;RING/SHUTTLE
         DC       <NE
         DC       <NE                   ;PLAYER/MISSLE
         DC       <EXXX                 ;SHUTTLE/MISSLE
         DC       <NE                   ;MISSLE/MISSLE
         DC       <EXBX                 ;CRYSTAL/MISSLE
         DC       <LEX                  ;LASER/MISSLE
         IF       HITTOWER
         DC       <EXXX
         ELSE
         DC       <EX1X                 ;TOWER/MISSLE
         ENDIF
         DC       <EXXX                 ;RING/MISSLE
         DC       <NE
         DC       <DSCX                 ;PLAYER/CRYSTAL
         DC       <DISX                 ;SHUTTLE/CRYSTAL
         DC       <EXB                  ;MISSLE/CRYSTAL
         DC       <NE                   ;CRYSTAL/CRYSTAL
         DC       <LEX                  ;LASER/CRYSTAL
         DC       <NE                   ;TOWER/CRYSTAL
         DC       <NE                   ;RING/CRYSTAL
         DC       <NE
         DC       <LEX                  ;PLAYER/LASER
         DC       <NE                   ;SHUTTLE/LASER
         DC       <LEX                  ;MISSLE/LASER
         DC       <LEX                  ;CRYSTAL/LASER
         DC       <NE                   ;LASER/LASER
         DC       <NE                   ;TOWER/LASER
         DC       <NE                   ;RING/LASER
         DC       <NE
         DC       <EX11                 ;PLAYER/TOWER
         DC       <EX11                 ;SHUTTLE/TOWER
         IF       HITTOWER
         DC       <EXX
         ELSE
         DC       <EX11                 ;MISSLE/TOWER
         ENDIF
         DC       <NE                   ;CRYSTAL/TOWER
         DC       <NE                   ;LASER/TOWER
         DC       <NE                   ;TOWER/TOWER
         DC       <NE                   ;RING/TOWER
         DC       <NE
         DC       <ACC                  ;PLAYER/RING
         DC       <EX11                 ;SHUTTLE/RING
         DC       <EXX                  ;MISSLE/RING
         DC       <NE                   ;CRYSTAL/RING
         DC       <NE                   ;LASER/RING
         DC       <NE                   ;TOWER/RING
         DC       <NE                   ;RING/RING
         DC       <NE
REFMASK1 DC       $F7,$FD,$FB,$FE,$FF,$EF,$DF
;
PLMSKS   DC       1,1,1,01
PLDXS    DC       $07,$08,$09,$0A
         DC       $07,$08,$09,$0A
         DC       -7,-9,-8,-10
         DC       -7,-9,-8,-10
PLYDYS   DC       $F6,$F6,$F6,$F6
         DC       $F5,$F6,$F7,$F4
         DC       $F5,$F6,$F6,$F7
         DC       $F5,$F6,$F6,$F6
PLIDS
         DC       ((P0 & $FF)/6) | $80
         DC       ((P0 & $FF)/6) | $80
         DC       ((P0 & $FF)/6) | $80
         DC       ((P0GRN & $FF)/6) | $80
         DC       ((P0 & $FF)/6) | $80
         DC       ((P0YEL & $FF)/6) | $80
         DC       ((P0RED & $FF)/6) | $80
;
;        JET ID TABLE
;        LEFTID,RIGHTID,REFLS(A=D4,B=D5)
; D2=ANIMATION #  D1=XZERO D0=XSIGN
;
JETIDS
;RIGHT
         DC       ((JX10 & $FF)/6) | $80,((JX20 & $FF)/6) | $80,$00
;LEFT
         DC       ((JX20 & $FF)/6) | $80,((JX10 & $FF)/6) | $80,$30
;CENTER
         DC       ((JX00 & $FF)/6) | $80,((JX00 & $FF)/6) | $80,$20
;
         DC       0,0,0
;RIGHT UP
         DC       ((JX11 & $FF)/6) | $80,((JX21 & $FF)/6) | $80,$00
;LEFT UP
         DC       ((JX21 & $FF)/6) | $80,((JX11 & $FF)/6) | $80,$30
;CENTER UP
         DC       ((JX01 & $FF)/6) | $80,((JX01 & $FF)/6) | $80
         DC       $20
;
; BRUCE'S MYSTICAL PHI TABLE
;
PHI      DC       5,2,7,4,1,6,3,0
;
TERRAIN  DC       (CRY0 & $FF)/6
         DC       (TWR0 & $FF)/6
         DC       (TWR0 & $FF)/6
         DC       (CRY0 & $FF)/6
         DC       (TWR0 & $FF)/6
         DC       (CRY0 & $FF)/6
         DC       (TWR0 & $FF)/6
         DC       (TWR0 & $FF)/6
PLNTS    DC       ((P161 & $FF)/6)
         DC       ((P162 & $FF)/6) | $80
         DC       ((P160 & $FF)/6)
         DC       ((P162 & $FF)/6) | $80
         DC       ((P160 & $FF)/6)
         DC       ((P160 & $FF)/6)
         DC       ((P161 & $FF)/6)
         DC       ((P161 & $FF)/6)
;
; PLANET COLORS
;
; 0=BLUE 1 =RED 2=GREEN 3=YELLOW
         IF       PAL
PCOLORS  DC       $B0,$40,$50,$20         ;WAVE COLORS
         ELSE
PCOLORS  DC       $80,$40,$C0,$10         ;WAVE COLORS
         ENDIF
;
; SCORE TABLE
;
SCORETBL DC       $10,$40,$20,$30         ;GET MAN
         DC       $10,$80,$10,$40         ;HIT SAUCER
         DC       $02,$03,$02,$03         ;HIT JET
         DC       $20,$40,$25,$30         ;FINISH WAVE
         DC       $03,$10,$05,$07         ;HIT SATTELITE
ASTIDS   DC       ((AST2 & $FF)/6) | $80
         DC       ((AST0 & $FF)/6) | $80
         DC       ((AST1 & $FF)/6) | $80
         DC       ((AST1 & $FF)/6) | $80
BALLDXS  DC       -6,-4,-2,-1,01,02,04,06
WAVEMSK  DC       1,2                     ;FOR GAME INC
;
SNDNOS   DC       FIRES,BONSND,MANSND,EXSND,DEATHS,EOG,LANDSND
;
; DURATION OF SOUND
;
SNDTIMS  DC       $1F,$1F,$1F,$2D,$6F,$7F,$3F
;
; JET DELTA DELTA Y TABLE
LSPEEDS  DC       $-5,$-7,$-9,$-10
SIZES    DC       3,3,3,2,1,1,0,0,0,0
;        PLAYER ANIMATION TABLE
;        D3-D1= REFLECT,SIGN,ZERO
;
PATBL    DC       ((PR & $FF)/6),0      ;MOVE RIGHT FROM RIGHT
         DC       ((PC & $FF)/6),8      ;STOP FROM RIGHT SHIP
         DC       ((PC & $FF)/6),0      ;MOVE LEFT FROM RIGHT S
         DC       0 | $80,0              ;NOT POSSIBLE
         DC       ((PC & $FF)/6),8      ;MOVE RIGHT FROM LEFT S
         DC       ((PR & $FF)/6),0      ;STOP FROM LEFT SHIP
         DC       ((PC & $FF)/6),0      ;MOVE LEFT FROM LEFT SH
MINSCR   DC       3,$11,7,$A
;
; VECTORS
;
         BOUNDRY 8
         DC.W       0
VECTORS  DC.W       $3000
         DC.W       $3000
         DC.W       $3000