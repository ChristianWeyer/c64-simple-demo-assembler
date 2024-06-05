;////////////////////////////////////////////////
;//                                            //
;// Assembler Corner 64er von 10-1993          //
;//                                            //
;// Demo - Programmierung                      //
;//                                            //
;////////////////////////////////////////////////

*= $0801                ; Start of BASIC program
!word next              ; Pointer to next line (will be calculated)
!word 10                ; Line number
!byte $9e               ; SYS token
!text "16384", 0        ; SYS 16384 (start address $4000)
next = *                ; Address of the next line (end of program)
!word 0                 ; End of BASIC program

;-------Zeichensaetze in den Speicher laden
*= $2800
!bin "../../assets/charsets/char4.bin",,2         ;Font1 liegt bei $2800  
*= $3000
!bin "../../assets/charsets/char1.bin",,2         ;Font2 liegt bei $3000

;-------SID Tune in den Speicher laden
*= $8000 
!bin "../../assets/music/music.sid",,$7c+2        ;SID-Tune liegt bei $8000

;---------------------------------------
;--------VARIABLEN SETZEN---------------
;---------------------------------------

IRQLOW    = $0314       ;IRQ-VEKTOR LOWBYTE
IRQHIG    = $0315       ;           HIGHBYTE
OLDIRQ    = $EA31       ;ALTE IRQ-ROUTINE

INITMUSIC = $8000       ;MUSIK INITIALISIEREN
PLAYMUSIC = $8012       ;ABSPIELEN

FLASHCOUNT  = $FB       ;ZAEHLER 1 UND 2
FLASHCOUNT2 = $FC       ;FUER FLASH-EFFEKTE

SCRHELP   = $03FB       ;HILFSREGISTER F.
SCROLLREG = $D016       ;SCROLLREGISTER

CHARSET   = $D018       ;ZEICHENS. DEFINIEREN
RASTER    = $D012       ;RASTERSTRAHL-POS
YSCROLL   = $D011       ;Y-SCROLL-REGISTER
IMR       = $D01A       ;IRQ MASK REGISTER

*= $4000                ;Programm liegt bei $4000

;---------------------------------------
;--------IRQ VORBEREITEN----------------
;---------------------------------------

        SEI              ;IRQ SPERREN

        JSR INIT         ;INITIALISIEREN

        LDA #<START      ;LOWBYTE LADEN
        STA IRQLOW       ;UND SPEICHERN
        LDA #>START      ;HIGHBYTE LADEN
        STA IRQHIG       ;UND SPEICHERN

        LDA YSCROLL      ;Y-SCROLL-REGISTER
        AND #$7F         ;RICHTIG
        STA YSCROLL      ;SETZEN

        LDA #$7F         ;TIMER
        STA $DC0D        ;SETZEN

        LDA #$01         ;RASTER-IRQ
        STA IMR          ;FESTLEGEN

        LDA #$00         ;MUSIK
        JSR INITMUSIC    ;INITIALISIEREN
        CLI              ;IRQ FREIGEBEN
        JMP *            ;ENDLOS-SCHLEIFE

;---------------------------------------
;--------IRQ-HAUPTSCHLEIFE--------------
;---------------------------------------

START   LDA YSCROLL      ;WARTEN BIS
        BPL START        ;UNTERER RAND
                         ;ERREICHT
        LDA #$1A         ;ZEICHENSATZ
        STA CHARSET      ;UMSCHALTEN
        LDA #$C8         ;SCROLLREG
        STA SCROLLREG    ;SETZEN

        JSR PLAYMUSIC    ;MUSIK SPIELEN

        LDA #$38         ;AUF RASTERLINE
ST1     CMP RASTER       ;$38
        BNE ST1          ;WARTEN

        JSR RASTERSHOW   ;RASTER-FARBEN
        JSR SCROLL       ;FARBEN SCROLLEN

        LDA #$8C         ;AUF RASTERLINE
ST2     CMP RASTER       ;$8C
        BNE ST2          ;WARTEN

        LDA #$1C         ;KLEINEN ZEICHEN
        STA CHARSET      ;SATZ EIN

        JSR FLASH        ;3 LINIEN FLASHEN

        LDA #$B8         ;AUF RASTERLINE
ST3     CMP RASTER       ;$B8
        BNE ST3          ;WARTEN

        JSR CHARSCROLL   ;SCROLLROUTINE
        JSR CHARFLASH    ;FARBSCROLLING

        JMP OLDIRQ       ;ALTER IRQ

;---------------------------------------
;--------SUB-ROUTINEN-------------------
;---------------------------------------

FLASH   LDY #$77         ;3LINES=$77 CHARS
        LDX FLASHCOUNT   ;COUNTER HOLEN
        LDA FLASHTAB,X   ;UND BYTE
FLASH1  STA $DA08,Y      ;SCHREIBEN SOLANG
        DEY              ;BIS ALLE DREI
        BPL FLASH1       ;ZEILEN FERTIG
        INC FLASHCOUNT   ;COUNTER+1
        CPX #$18         ;SCHON 18 WERTE?
        BNE FLASHEND     ;NEIN DANN ENDE
        LDA #$00         ;JA DANN ZAEHLER
        STA FLASHCOUNT   ;ZURUECKSETZEN
FLASHEND RTS              ;ENDE

FLASHTAB                  ;FARBWERTE
        !byte $02,$02,$02,$0A,$0A,$0A
        !byte $07,$07,$07,$0F,$0F,$0F
        !byte $07,$07,$07,$0A,$0A,$0A
        !byte $02,$02,$02,$00,$00,$00

;---------------------------------------

CHARFLASH
        LDY $DB20        ;FARBRAM VON
        LDX #$00         ;$DB20 BIS
CHARFL1 LDA $DB21,X      ;$DB47
        STA $DB20,X      ;ENDLOS
        INX              ;ROTIEREN
        CPX #$27         ;DAS Y-REG DIENT
        BNE CHARFL1      ;ALS ZWISCHEN-
        STY $DB47        ;SPEICHER
        RTS              ;ENDE

FLASHTAB2                 ;FARBTABELLE
        !byte $06,$06,$06,$04,$04,$04
        !byte $0E,$0E,$0E,$03,$03,$03
        !byte $0F,$0F,$0F,$01,$01,$01
        !byte $01,$01,$01,$0F,$0F,$0F
        !byte $03,$03,$03,$0E,$0E,$0E
        !byte $04,$04,$04,$06,$06,$06
        !byte $00,$00,$00,$00
TABEND2
;---------------------------------------

CHARSCROLL
        LDX SCRHELP        ;REGISTER HOLEN
        DEX                ;UND ZWEIMAL
        DEX                ;DEKREMENTIEREN
                           ;(SCROLLSPEED)
        STX SCRHELP        ;IN HILFS & SCRREG
        STX SCROLLREG      ;SCHREIBEN
        CPX #$BF           ;SCHON UNTERLAUF?
        BEQ HARDSCR        ;DANN HARDSCROLL
        RTS                ;ENDE

HARDSCR LDX #$C7           ;REGISTER
        STX SCROLLREG      ;ZURUECK-
        STX SCRHELP        ;SETZEN

        LDX #$00           ;BILDSCHIRMZEILE
HARD1   LDA $0721,X        ;UM EINS
        STA $0720,X        ;NACH
        INX                ;LINKS
        CPX #$27           ;ROTIEREN
        BNE HARD1          ;

CHANGE  LDA TEXT           ;TEXTBYTE LADEN
        CMP #$00           ;SCHON ABBRUCH?
        BNE CONT           ;NEIN DANN CONT
        LDA #<TEXT         ;LOWBYTE
        STA CHANGE+1       ;SCHREIBEN
        LDA #$20           ;UND AKKU AUF
        STA $0747          ;LEERZEICHEN
        RTS                ;ENDE
CONT    STA $0747          ;ZEICHEN SCHREIBEN
        INC CHANGE+1       ;CHANGE IM CODE
                           ;UM EINS ADDIEREN
SCREND  RTS                ;ENDE

;--- Lauftext ---
TEXT    !scr "proudly presented by brotkasten and christian..."   ;MAX. 255 ZEICHEN
        !byte $00

;---------------------------------------

RASTERSHOW
        LDX #$00         ;ZAEHLER AUF $00
COLOR1  LDA COLTAB,X     ;FARBWERT HOLEN
        LDY WAITAB,X     ;WAIT-WERT HOLEN
WAIT1   DEY              ;HERUNTERZAEHLEN
        BNE WAIT1        ;UM DIE ZYKLEN
                         ;AUSZUGLEICHEN
        STA $D021        ;FARBE
        STA $D021        ;SCHREIBEN
        INX              ;ZAEHLER+1
        CPX #$40         ;SCHON 40 FARBEN?
        BNE COLOR1       ;NEIN DANN COLOR1
        LDA #$00         ;JA DANN SCREEN
        STA $D020        ;AUF SCHWARZ
        STA $D021        ;SETZEN
        RTS              ;ENDE

COLTAB
        !byte $06,$04,$0E,$03,$07,$0F
        !byte $01,$01,$0F,$07,$03,$0E
        !byte $06,$04,$00,$00,$09,$02
        !byte $0A,$07,$0F,$01,$01,$0F
        !byte $07,$0A,$02,$09,$00,$00
        !byte $09,$0B,$08,$0C,$0F,$07
        !byte $01,$01,$0F,$0C,$08,$0B
        !byte $09,$00,$00,$02,$0A,$07
        !byte $0F,$01,$0B,$0C,$0F,$01
        !byte $01,$0F,$0C,$0B,$01,$0F
        !byte $07,$0A,$02,$00,$00
COLEND
WAITAB
        !byte $09,$08,$08,$01,$08,$08
        !byte $08,$08,$08,$08,$08,$01
        !byte $08,$08,$08,$08,$08,$08
        !byte $08,$01,$08,$08,$08,$08
        !byte $08,$08,$08,$01,$08,$08
        !byte $08,$08,$08,$08,$08,$01
        !byte $08,$08,$08,$08,$08,$08
        !byte $08,$01,$08,$08,$08,$08
        !byte $08,$08,$08,$01,$08,$08
        !byte $08,$08,$08,$08,$08,$01
        !byte $08,$08,$08,$08,$08

;---------------------------------------

SCROLL  LDY COLTAB       ;FARBTABELLE DER
        LDX #$00         ;RASTERFARBEN
SCR1    LDA COLTAB+1,X   ;UM EIN BYTE
        STA COLTAB,X     ;NACH LINKS
        INX              ;ROTIEREN.DADURCH
        CPX #$40         ;ENTSTEHT EIN
        BNE SCR1         ;FEINES
        STY COLEND-1     ;RASTERSCROLLING
        RTS              ;ENDE

;---------------------------------------

INIT    LDA #$C7           ;HILFSREGISTER
        STA SCRHELP        ;SETZEN

        LDA #$00           ;COUNTER
        STA FLASHCOUNT     ;INITIALISIEREN
        STA FLASHCOUNT2    ;
        STA $D020          ;SCREEN UND
        STA $D021          ;FRAME SCHWARZ
        LDA #$00           ;SCHWARZES
        STA $0286          ;FARBRAM
        JSR $E544          ;SCREEN LOESCHEN

        LDX #$00           ;SCREEN1
INIT1   LDA SCREEN1,X      ;TEXT
        STA $0454,X        ;OBEN
        INX                ;AUF DEN
        CPX #$C4           ;BILDSCHIRM
        BNE INIT1          ;SCHREIBEN

        LDX #$77           ;SCREEN2
INIT3   LDA SCREEN2,X      ;TEXT
        STA $0608,X        ;AUF DEN
        DEX                ;BILDSCHIRM
        BPL INIT3          ;SCHREIBEN

        LDX #$27           ;FARBTABELLE
INIT4   LDA FLASHTAB2,X    ;IN COLOR-RAM
        STA $DB20,X        ;SCHREIBEN
        DEX                ;(FUER'S
        BPL INIT4          ;FARBSCROLLING)

        LDA #$C7           ;SCROLLREGISTER
        STA SCROLLREG      ;SETZEN
        LDA #$1C           ;ZEICHENSATZ
        STA CHARSET        ;ANSCHALTEN
        RTS                ;ENDE

;--- Oberer Text ---
SCREEN1 !scr "i t e r a t e   r u h r  2 0 2 4"
        !scr "                                         "
        !scr "                                         "
        !scr "                                         "
        !scr "             c64  ki  keynote                "

;--- Mittlerer Text ---
SCREEN2 !scr "chuck peddle ist der goat: mos 6502 und commodore pet - steve jobs hat user her-vor gebracht, jack tramiel programmierer"
