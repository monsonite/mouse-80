;
; MOUSE.Z80, Peter Grogono
;
; Update log:
;
;   2/21/93 - Lee Bradley
;
;   Assembled/linked with ZMAC/ZML. Changed label mod to modu.
;   Changed file not found message to: Usage: MOUSE filename.mse.
;
;   6/15/86 - Lee Bradley
;
;   Made M80 compatible. Many small changes made to original program,
;   mostly cosmetic in nature. Programs of about 16K and which may
;   use about 250 values on the calculation stack may be interpreted
;   with this version.
;
           .Z80                         ;Use Zilog mnemonics
;
CpEnt      EQU       5H                 ;Use CP/M function
Fcb        EQU       5CH                ;Default file control block
Fcbex      EQU       Fcb+12             ;File extent


CurRec     EQU       Fcb+32             ;Current record number
Buffer     EQU       80H                ;Default file buffer
Tpa        EQU       100H               ;Transient program area
;
; CP/M Entry points
;
ConIn      EQU       1                  ;Read from console
ConOut     EQU       2                  ;Write to console
Status     EQU       11                 ;Get console status
Open       EQU       15                 ;Open file
Read       EQU       20                 ;Read file
;
; Program constants
;
RecSize    EQU       128                ;CP/M file buffer size
LocSize    EQU       26                 ;Number of local variables
FrSize     EQU       5                  ;Size of environment stack frame
MacSize    EQU       2*LocSize          ;Macro address table size
ProgSiz    EQU       16*1024            ;Maximum size of Mouse program
MaxLev     EQU       50                 ;Maximum nesting level
EnvSize    EQU       FrSize*MaxLev      ;Size of environment stack
StkSize    EQU       500                ;Size of local and calculation stacks
;


; Other useful constants
;
Cr         EQU       13                 ;Carriage return
Lf         EQU       10                 ;Line feed
CtrlC      EQU       3                  ;^C to exit from trace mode
Graphic    EQU       32                 ;First ASCII graphic character
Width      EQU       80                 ;Width of screen
Eof        EQU       1AH                ;CP/M end of file code
Other      EQU       0                  ;Tag values for
Macrox     EQU       1                  ; environment
Param      EQU       2
;
; Macro definitions
;
cpm        MACRO    func          ;Invoke a CP/M function
            LD       C,func
            CALL     CpEnt
           ENDM
;
Display    MACRO    string        ;Display a string on the console
            LD       HL,string
            CALL     Disp
           ENDM


;
skip       MACRO     lch,rch      ;Skip bracketing characters
            LD       B,lch
            LD       C,rch
            CALL     SkipNst
           ENDM
;
; Start of program
;
           ASEG                         ;Absolute segment
           ORG       Tpa                ;Load at Tpa
           LD        HL,0               ;Save Ccp's stack pointer
           ADD       HL,SP
           LD        (Ccp),HL
           LD        SP,Stack           ;Initialize local stack
nop1:      CALL      Loader             ;Load the Mouse program
           LD        HL,nop1            ;In case user wants to
           LD        (HL),0             ;SAVE "compiled" program
           INC       HL
           LD        (HL),0
           INC       HL
           LD        (HL),0
;


; Initialize the interpreter
;
init:      LD        HL,Prog
           LD        (CharPos),HL
           LD        IX,CalStak         ;Calculation stack pointer
           LD        IY,EnvStak         ;Environment stack pointer
           LD        HL,0
           LD        (OffSet),HL        ;Variable address offset
           LD        HL,LocSize
           LD        (NxtFree),HL       ;Next free variable address
           LD        A,0
           LD        (Tracing),A        ;Turn off tracing
;
; Central interpreter loop
;
cycle:     cpm       Status             ;Check keyboard
           CP        0
           JR        Z,get1             ;Jump if nothing entered
           cpm       ConIn              ;Read the character entered
           CP        CtrlC
           JP        Z,Return           ;Return to CP/M if ^C
get1:      CALL      GetChar
           CP        ' '


           JR        Z,get2             ;Don't trace blanks
           LD        A,(Tracing)
           OR        A
           JR        Z,get2             ;Process character unless tracing
           CALL      Trace              ;Display current environment
           cpm       ConIn              ;Read a character from keyboard
           OR        A                  ;Clear flags
           CP        ' '
           JR        Z,get2
           CP        Cr
           JR        Z,get2
           XOR       A                  ;Turn off tracing if character
           LD        (Tracing),A        ;is not a blank or a CR
get2:      LD        HL,(CharPos)
           LD        A,(HL)             ;Fetch the character again
           CP        Graphic
           JP        M,illegal          ;Reject nongraphic characters
           RLCA
           LD        H,0
           LD        L,A                ;HL  := 2*next character
           RRCA                         ;Restore character
           LD        DE,-2*Graphic+CharTab
           ADD       HL,DE              ;Computer table address


           LD        E,(HL)
           INC       HL
           LD        D,(HL)
           EX        DE,HL              ;HL := processing address
           JP        (HL)               ;Jump to process character
;
; Jump Address table for each ASCII Graphic character
;
CharTab:   DW        cycle              ;Blank
           DW        exclam             ;!
           DW        quote              ;"
           DW        sharp              ;#
           DW        Return             ;$
           DW        percent            ;%
           DW        file               ;&     5/19/86
           DW        apost              ;'
           DW        lparen             ;(
           DW        rparen             ;)
           DW        mul                ;*
           DW        add                ;+
           DW        endpar             ;,
           DW        sub                ;-
           DW        dot                ;.


           DW        div                ;/
           DW        digit              ;0
           DW        digit              ;1
           DW        digit              ;2
           DW        digit              ;3
           DW        digit              ;4
           DW        digit              ;5
           DW        digit              ;6
           DW        digit              ;7
           DW        digit              ;8
           DW        digit              ;9
           DW        colon              ;:
           DW        endpar             ;;
           DW        less               ;<
           DW        equal              ;=
           DW        greater            ;>
           DW        query              ;?
           DW        at                 ;@
           DW        uc                 ;A
           DW        uc                 ;B
           DW        uc                 ;C
           DW        uc                 ;D
           DW        uc                 ;E


           DW        uc                 ;F
           DW        uc                 ;G
           DW        uc                 ;H
           DW        uc                 ;I
           DW        uc                 ;J
           DW        uc                 ;K
           DW        uc                 ;L
           DW        uc                 ;M
           DW        uc                 ;N
           DW        uc                 ;O
           DW        uc                 ;P
           DW        uc                 ;Q
           DW        uc                 ;R
           DW        uc                 ;S
           DW        uc                 ;T
           DW        uc                 ;U
           DW        uc                 ;V
           DW        uc                 ;W
           DW        uc                 ;X
           DW        uc                 ;Y
           DW        uc                 ;Z
           DW        lbrack             ;[
           DW        modu               ;\


           DW        cycle              ;]
           DW        hat                ;^
           DW        illegal
           DW        illegal
           DW        lc                 ;a
           DW        lc                 ;b
           DW        lc                 ;c
           DW        lc                 ;d
           DW        lc                 ;e
           DW        lc                 ;f
           DW        lc                 ;g
           DW        lc                 ;h
           DW        lc                 ;i
           DW        lc                 ;j
           DW        lc                 ;k
           DW        lc                 ;l
           DW        lc                 ;m
           DW        lc                 ;n
           DW        lc                 ;o
           DW        lc                 ;p
           DW        lc                 ;q
           DW        lc                 ;r
           DW        lc                 ;s


           DW        lc                 ;t
           DW        lc                 ;u
           DW        lc                 ;v
           DW        lc                 ;w
           DW        lc                 ;x
           DW        lc                 ;y
           DW        lc                 ;z
           DW        lbrace             ;{
           DW        illegal            ;|
           DW        rbrace             ;}
           DW        illegal            ;~
           DW        illegal            ;DEL
;
; Actions according to character class
; A = current character
;
illegal:   Display   IllChar            ;Illegal character
           JP        Return
digit:     LD        HL,0               ;Digit
dig1:      SBC       A,'0'              ;Convert to binary
           JP        M,dig2             ;<0: not a digit
           CP        9+1
           JP        P,dig2             ;>9: not a digit


           LD        D,0
           LD        E,A                ;DE := digit
           PUSH      DE
           LD        DE,10
           CALL      Multply            ;HL := 10 * temp
           POP       DE
           ADD       HL,DE              ;HL := 10 * temp + digit
           EX        DE,HL              ;Save value in DE
           CALL      GetChar
           EX        DE,HL              ;Restore value
           JR        dig1
dig2:      CALL      PushCal
           CALL      BkSpace            ;Reposition character pointer
           JP        cycle
add:       CALL      PopCal             ;+
           EX        DE,HL              ;Pop two operands
           CALL      PopCal             ;and push their sum
           ADD       HL,DE
           CALL      PushCal
           JP        cycle
sub:       CALL      Diff               ;-
           CALL      PushCal            ;Pop two operands
           JP        cycle              ;and push their difference


mul:       CALL      PopCal             ;*
           EX        DE,HL              ;Pop two operands
           CALL      PopCal             ;and push their product
           CALL      Multply
           CALL      PushCal
           JP        cycle
div:       CALL      PopCal             ;/
           EX        DE,HL              ;Pop two operands
           CALL      PopCal             ;and push their quotient
           CALL      Divide
           CALL      PushCal
           JP        cycle
modu:      CALL      PopCal             ;\
           EX        DE,HL              ;Pop two operands
           CALL      PopCal             ;and push their modulus
           CALL      Modulus
           CALL      PushCal
           JP        cycle
query:     CALL      GetChar            ;?
           CP        27H                ;Read from keyboard
           JR        NZ,qy2
           cpm       ConIn              ;Read ASCII character
           LD        H,0


           LD        L,A
           CALL      PushCal            ;Stack it
           JP        cycle
qy2:       CALL      ReadNum            ;Otherwise read a number
           CALL      PushCal
           CALL      BkSpace            ;Reposition character pointer
           JP        cycle
exclam:    CALL      GetChar            ;!
           CP        27H                ;Display value
           JR        NZ,ex2
           CALL      PopCal             ;!' displays ASCII character
           LD        E,L
           cpm       ConOut
           JP        cycle
ex2:       CALL      PopCal             ;! displays numerical value
           CALL      DisNum
           CALL      BkSpace            ;Reposition character pointer
           JP        cycle
quote:     CALL      GetChar            ;Display string
           CP        '"'
           JP        Z,cycle            ;Terminate at matching quote
           CP        '!'
           JR        Z,newline          ;! becomes CR/LF


           LD        E,A
           cpm       ConOut             ;Display other characters
           JP        quote
newline:   Display   CrLf
           JP        quote
uc:        SBC       A,'A'              ;Upper case letter (Global)
           LD        D,0
           LD        E,A                ;DE := letter (0..25)
           LD        HL,0
           ADD       HL,DE              ;HL := address of variable
           CALL      PushCal
           JP        cycle
lc:        SBC       A,'a'              ;Lower case letter (Local)
           LD        D,0
           LD        E,A                ;DE := letter (0..25)
           LD        HL,(OffSet)
           ADD       HL,DE              ;HL := address of variable
           CALL      PushCal
           JP        cycle
colon:     CALL      Addr               ;Assignment
           EX        DE,HL              ;DE := address
           CALL      PopCal
           EX        DE,HL              ;DE := data, HL := address


           LD        (HL),D
           INC       HL
           LD        (HL),E             ;Store value
           JP        cycle
dot:       CALL      Addr               ;.
           LD        D,(HL)             ;Dereference
           INC       HL
           LD        E,(HL)             ;DE := contents
           EX        DE,HL
           CALL      PushCal            ;Stack contents
           JP        cycle
less:      CALL      Diff              ;<
           JP        M,true
false:     LD        HL,0              ;False = 0
           CALL      PushCal
           JP        cycle
true:      LD        HL,1              ;True = 1
           CALL      PushCal
           JP        cycle
equal:     CALL      Diff              ;=
           JR        Z,true
           JR        false
greater:   CALL      Diff              ;>


           CALL      Negate
           LD        A,H
           OR        A                 ;Set flags
           JP        M,true
           JR        false
lbrack:    CALL      PopCal            ;[ - Skip if stack <=0
           LD        A,H
           OR        A
           JP        M,lbr1            ;Skip if < 0
           OR        L
           JP        NZ,cycle          ;Skip if = 0
lbr1:      skip      '[',']'
           JP        cycle
lparen:    LD        A,Other           ;(
           CALL      PushEnv           ;Stack current position
           JP        cycle
rparen:    LD        H,(IY+3)          ;)
           LD        L,(IY+2)          ;Restore position without
           LD        (CharPos),HL      ;popping stack
           JP        cycle
hat:       CALL      PopCal            ;^
           LD        A,H
           OR        A


           JP        M,hat1            ;Exit loop if < 0
           OR        L
           JP        NZ,cycle
hat1:      CALL      PopEnv
           skip      '(',')'
           JP        cycle
sharp:     CALL      GetChar            ;#
           CP        'a'                ;Macro call
           JP        M,sh1
           ADD       A,'A'-'a'          ;Convert to upper case
sh1:       OR        A
           SBC       A,'A'              ;A..Z -> 0..25
           RLCA                         ;*2 for word address
           LD        B,0
           LD        C,A                ;BC := offset of name
           LD        A,Macrox
           CALL      PushEnv            ;Save current state
           LD        HL,MacDefs
           ADD       HL,BC              ;HL := Address of definition
           LD        E,(HL)
           INC       HL
           LD        D,(HL)             ;DE := macro address
           LD        A,D


           OR        E
           JR        Z,sh2              ;Undefined macro
           LD        (CharPos),DE
           LD        HL,(NxtFree)
           LD        (OffSet),HL
           LD        DE,LocSize
           ADD       HL,DE
           LD        (NxtFree),HL       ;NxtFree := NxtFree + 26
           JP        cycle
at:        LD        HL,(NxtFree)       ;@ (Return from macro)
           LD        DE,LocSize
           SBC       HL,DE
           LD        (NxtFree),HL       ;NxtFree := NxtFree - 26
sh2:       CALL      PopEnv             ;Recover status
           skip      '#',';'            ;and skip over call
           JP        cycle
percent:   LD        A,Param            ;% - formal parameter
           CALL      PushEnv            ;Save current state
           LD        C,1                ;parbal := 1
           PUSH      IY                 ;Save environment stack pointer
pc1:       LD        DE,5
           ADD       IY,DE              ;Next stack frame
           LD        A,(IY+1)           ;Get tag value


           CP        Macrox
           JR        NZ,pc2             ;tag = macro
           DEC       C                  ;parbal := parbal - 1
           JR        pc3
pc2:       CP        Param
           JR        NZ,pc3             ;tag = param
           INC       C                  ;parbal := parbal + 1
pc3:       LD        A,C
           OR        A
           JR        NZ,pc1             ;Loop until match
pc4:       LD        H,(IY+5)
           LD        L,(IY+4)
           LD        (OffSet),HL        ;Recover offset
           LD        H,(IY+3)           ; for calling environment
           LD        L,(IY+2)
           LD        (CharPos),HL       ;Restore position of call
           POP       IY                 ;Restore frame stack pointer
           CALL      PopCal
           LD        E,L                ;E := parnum
pc5:       CALL      GetChar            ;Search for actual parameter
           CP        '"'
           JR        NZ,pc6
           CALL      SkipStr


           JR        pc5
pc6:       CP        '#'
           JR        NZ,pc7
           skip      '#',';'
           JR        pc5
pc7:       CP        ','
           JR        NZ,pc8
           DEC       E                  ;parnum := parnum -1
           JR        pc9
pc8:       CP        ';'
           JR        NZ,pc9
           CALL      PopEnv             ;Null parameter
           JP        cycle
pc9:       LD        A,E
           OR        A
           JR        NZ,pc5             ;Loop until parameter found
           JP        cycle
endpar:    CALL      PopEnv             ;, or ;
           JP        cycle
apost:     CALL      GetChar            ;'
           LD        H,0                ;Push ASCII character onto
           LD        L,A                ; stack
           CALL      PushCal


           JP        cycle
lbrace:    LD        A,1                ;{
           LD        (Tracing),A        ;Turn on tracing
           JP        cycle
rbrace:    XOR       A                  ;}
           LD        (Tracing),A        ;Turn off tracing
           JP        cycle
file:      LD        DE,Fcbex           ;&
           XOR       A                  ;Load .MSE file
           LD        (DE),A             ;Clear key bytes in Fcb
           INC       DE
           LD        (DE),A
           INC       DE
           LD        (DE),A
           INC       DE
           LD        (DE),A
           LD        DE,CurRec
           LD        (DE),A
           LD        C,9                ;Keep track of number of
           LD        DE,Fcb             ;characters in filename
           LD        (DE),A
f1:        CALL      GetChar            ;Put characters in Fcb
           CP        '&'                ;until delimiting & found


           JR        Z,f2
           INC       DE
           DEC       C
           LD        (DE),A
           JR        f1
f2:        LD        A,' '              ;Pad if necessary with blanks
f3:        DEC       C
           JR        Z,f4
           INC       DE
           LD        (DE),A
           JR        f3
f4:        INC       DE                 ;Tack on MSE extension
           LD        A,'M'
           LD        (DE),A
           INC       DE
           LD        A,'S'
           LD        (DE),A
           INC       DE
           LD        A,'E'
           LD        (DE),A
           CALL      Loader             ;Load it and then jump
           JP        init               ;to initialization code
;


; Subroutines
;
; The Mouse program loader
;
Loader:    Display   Signon
           LD        DE,Fcb             ;Open the input file
           cpm       Open
           CP        255
           JP        NZ,readfil
           Display   OpnFail            ;File could not be opened
           JP        Return
readfil:   Display   Reading
           LD        B,MacSize          ;Clear macro table
           LD        HL,MacDefs
clemac:    LD        (HL),0
           INC       HL
           DJNZ      clemac
           LD        BC,ProgSiz          ;Clear program area
           LD        HL,Prog
clepgm:    LD        (HL),0
           INC       HL
           DEC       BC
           LD        A,B


           OR        C
           JR        NZ,clepgm
           LD        IX,Buffer+RecSize
           LD        IY,Prog            ;Set input and output pointers
ld1:       CALL      Gch
ld2:       CP        ' '
           JP        P,ld3
           LD        A,' '
           CALL      Store              ;Convert nongraphic characters
           JR        ld1                ;to blanks
ld3:       JR        NZ,ld5
           CALL      Store              ;Store first blank
ld4:       CALL      Gch
           CP        ' '
           JR        Z,ld4              ;Ignore following blanks
           JR        ld2
ld5:       CP        '~'
           JR        NZ,ld7
ld6:       CALL      Gch                ;Remove comments by
           CP        Cr                 ;skipping to EOL
           JR        NZ,ld6
           JR        ld1
ld7:       CP        '"'


           JR        NZ,ld9
           CALL      Store              ;Store strings as is
ld8:       CALL      Gch
           CALL      Store
           LD        A,(IY)             ;Retrieve character
           CP        '"'
           JR        NZ,ld8
           JR        ld1
ld9:       CP        '$'
           JR        NZ,ld11
           LD        A,(IY)
           CP        ''''
           LD        A,'$'
           JR        NZ,ld91
           JP        ld11
ld91:      CALL      Store              ;Store $ as terminator
           CALL      Gch                ;Macro definition
           CP        '@'                ;Test for program without
           JP        P,ld9a             ;any Macros (05/03/86)
           JR        ld1                ;If not, loop.
ld9a:      CP        'a'
           JP        M,ld10
           ADD       A,'A'-'a'          ;Convert lower to upper case


ld10:      OR        A
           SBC       A,'A'              ;A..Z -> 0..25
           RLCA                         ;* 2 for word address
           LD        D,0
           LD        E,A
           LD        HL,MacDefs         ;Address of definitions
           ADD       HL,DE              ;HL -> definition pointer
           PUSH      IY
           POP       DE                 ;DE := IY
           LD        (HL),E
           INC       HL
           LD        (HL),D             ;Store pointer to definition
           JR        ld1
ld11:      CALL      Store              ;Store everything else
           JR        ld1
;
; Put next character from file Buffer into A register.
; A new Buffer is read when IX = Buffer + 128.
;
Gch:       PUSH      IX
           POP       HL                 ;HL := IX
           LD        DE,Buffer+RecSize
           OR        A


           SBC       HL,DE              ;HL := IX - buffer - 128
           JR        NZ,Gch1
           LD        DE,Fcb             ;Read next sector
           cpm       Read
           OR        A
           JR        NZ,Gch2            ;Jump if end of file
           LD        E,'.'
;          cpm       ConOut             ;Show progress
           LD        IX,Buffer          ;Reset pointer
Gch1:      LD        A,(IX)             ;Get character
           OR        A                  ;Clear carry
           CP        Eof
           JR        Z,Gch2
           OR        A
           INC       IX
           RET
Gch2:      Display   Loaded             ;End of file
           POP       HL                 ;Pop link to Gch
           RET                          ;Return from Loader
;
; Store character in A register in Program Buffer.
;
Store:     INC       IY


           LD        (IY),A
           PUSH      IY
           POP       HL                 ;HL := IY
           LD        DE,ProgTop
           OR        A
           SBC       HL,DE              ;Return if there is
           RET       M                  ;space for more program
           Display   TooLong
           JP        Return
;
; Get next character from Program Buffer.
; On exit: A contains character;
; HL points to character in Buffer.
;
GetChar:   LD       HL,(CharPos)
           INC      HL
           LD       (CharPos),HL
           LD       A,(HL)
           OR       A                   ;Clear carry
           RET
;
; Backspace the character pointer
;


BkSpace:   LD        HL,(CharPos)
           DEC       HL
           LD        (CharPos),HL
           RET
;
; Calculation Stack.
; IX is the calculation Stack Pointer.
; HL is pushed/popped from the Stack.
; Underflow/overflow is not checked.
; The Stack looks like this between calls:
;
;          IX  ->        |   |
;          IX+1          |LSB|
;          IX+2          |MSB|
;
PushCal:   LD        (IX),H
           DEC       IX
           LD        (IX),L
           DEC       IX
           RET
PopCal:    INC       IX
           LD        L,(IX)
           INC       IX


           LD        H,(IX)
           RET
;
; Frame Stack.
; Each entry has 5 bytes.  IY is the Stack pointer.
; Overflow/underflow is not checked.  Destroys DE.
; On entry, A is assumed to hold the tag value.
; Between calls the Stack looks like this:
;
;          IY ->     |             |
;          IY+1      | tag         |
;          IY+2      | LSB CharPos |
;          IY+3      | MSB CharPos |
;          IY+4      | LSB OffSet  |
;          IY+5      | MSB OffSet  |
;
PushEnv:   LD        DE,-5
           ADD       IY,DE
           LD        (IY+1),A
           LD        HL,(CharPos)
           LD        (IY+2),L
           LD        (IY+3),H
           LD        HL,(OffSet)


           LD        (IY+4),L
           LD        (IY+5),H
           RET
PopEnv:    LD        H,(IY+5)
           LD        L,(IY+4)
           LD        (OffSet),HL
           LD        H,(IY+3)
           LD        L,(IY+2)
           LD        (CharPos),HL
           LD        DE,5
           ADD       IY,DE
           RET
;
; Skip over a string. On entry the character " has
; been seen.  This subroutine looks for the next ".
; Destroys A, HL
;
SkipStr:   CALL      GetChar
           CP        '"'
           JP        NZ,SkipStr
           RET
;
; Skip bracketing characters.


; On entry B = left character(e.g. [)
; and C = right character (e.g. ])
; Destroys A, D, HL; must preserve E for % processing.
;
SkipNst:   LD        D,1                ;Level counter
sk1:       XOR       A                  ;A := 0
           CP        D
           RET       Z                  ;Return when level counter = 0
sk2:       CALL      GetChar
           CP        '"'
           JR        NZ,sk3
           CALL      SkipStr            ;Skip a string
           JR        sk2
sk3:       CP        B
           JR        NZ,sk4
           INC       D                  ;Left character
           JR        sk1
sk4:       CP        C
           JR        NZ,sk1
           DEC       D                  ;Right character
           JR        sk1
;
; Display HL as a signed decimal string followed by a blank.


; All registers destroyed.
;
DisNum:    LD        A,H
           OR        A
           JP        P,ds1
           CALL      Negate             ;Number is negative
           PUSH      HL                 ;Save its value
           LD        E,'-'
           cpm       ConOut
           POP       HL                 ;Restore number
ds1:       LD        D,1
           PUSH      DE                 ;Set last digit flag
ds2:       LD        DE,10
           CALL      divmod             ;BC := HL/10, HL := HL\10
           PUSH      HL
           LD        H,B                ;Restore quotient
           LD        L,C
           LD        A,H
           OR        L
           JR        NZ,ds2             ;Loop until quotient is zero
ds3:       POP       DE                 ;Restore digit
           LD        A,D
           OR        A


           RET       NZ                 ;Exit when flag is found
           LD        A,E
           ADD       A,'0'              ;Convert digit to ASCII
           LD        E,A
           cpm       ConOut             ;Display digit
           JR        ds3
;
; Read a signed number from the keyboard.
; HL := value of number; other registers destroyed
;
ReadNum:   cpm       ConIn              ;Read a character
           LD        HL,0               ;Initialize value register
           CP        '-'
           PUSH      AF                 ;Save sign flag
           JR        Z,rd3              ;Read first digit
rd2:       SBC       A,'0'
           JP        M,rd4              ;Exit if character < '0'
           CP        9+1
           JP        P,rd4              ;Exit if character > '9'
           LD        D,0                ;DE := digit
           LD        E,A
           PUSH      DE
           LD        DE,10


           CALL      Multply            ;Value := 10 * value
           POP       DE
           ADD       HL,DE              ;+ digit
rd3:       PUSH      HL
           cpm       ConIn              ;Read another digit
           POP       HL
           JP        rd2
rd4:       POP       AF                 ;Restore sign flag
           CALL      Z,Negate           ;Negate register if '-'
           RET
;
; Set HL := second operand - first operand.
; Destroys DE
;
Diff:      CALL      PopCal
           EX        DE,HL
           CALL      PopCal
           SBC       HL,DE
           RET
;
; Pop the calculation stack and convert result to an address
; in the data area.  The entry in the stack consists of a
; letter value (0..25) + the current offset.


;
Addr:      CALL      PopCal
           ADD       HL,HL              ;HL := 2 * (letter + offset)
           LD        DE,Data
           ADD       HL,DE
           RET
;
; Multiply subroutine.
; HL := HL * DE; other registers destroyed.
;
Multply:   LD        A,H
           XOR       D
           PUSH      AF
           CALL      MakePos            ;Make operands positive
           LD        B,H                ;BC := HL
           LD        C,L
           LD        HL,0               ;HL := 0 (result register)
my1:       SRA       B                  ;BC := BC/2
           RR        C                  ;LSB to carry
           JR        NC,my2
           ADD       HL,DE              ;Add multiplicand if bit set
my2:       LD        A,B
           OR        C


           JR        Z,my3              ;Finished if BC = 0
           SLA       E                  ;DE := 2 * DE
           RL        D
           JR        my1
my3:       POP       AF
           CALL      M,Negate           ;Negate result if necessary
           RET
;
; Divide subroutine
; HL := HL div DE; other registers destroyed.
;
Divide:    LD        A,H
           XOR       D
           PUSH      AF                 ;Save sign of result
           CALL      divmod
           LD        H,B                ;Get quotient
           LD        L,C
           POP       AF
           CALL      M,Negate           ;Negate result if necessary
           RET
;
; Modulus subroutine.
; HL := HL mod DE; other registers destroyed.


;
Modulus:   LD        A,H
           XOR       D
           PUSH      AF                 ;Save sign of result
           CALL      divmod
           POP       AF
           CALL      M,Negate           ;Negate result if necessary
           RET
;
; This does the work for 'divide' and 'modulus'.
; BC := HL div DE; HL := HL mod DE; other registers destroyed
;
divmod:    LD        A,D
           OR        E
           JR        Z,DivErr           ;Attempted divide by zero
           CALL      MakePos            ;Make operands positive
           XOR       A                  ;A := 0
dm1:       EX        DE,HL
dm2:       BIT       6,H                ;Normalize divisor
           JR        NZ,dm3
           INC       A
           ADD       HL,HL              ;Shift left
           JR        dm2


dm3:       EX        DE,HL
           LD        BC,0               ;BC := 0 (result register)
           INC       A
dm4:       OR        A                  ;Clear flags
           SBC       HL,DE              ;Subtract divisor
           CCF
           JR        C,dm5
           ADD       HL,DE              ;Result is negative
           OR        A
dm5:       RL        C                  ;Shift 0 or 1 into quotient
           RL        B
           SRA       D                  ;Shift divisor
           RR        E
           DEC       A                  ;Count bits
           JR        NZ,dm4
           RET

;
; Make HL and DE positive
;
MakePos:   BIT       7,H
           JR        Z,sg1
           CALL      Negate             ;Make HL positive


sg1:       BIT       7,D
           RET       Z
           EX        DE,HL
           CALL      Negate             ;Make DE positive
           EX        DE,HL
           RET
;
; Negate HL.
;
Negate:    LD        A,H                ;Complement H
           CPL
           LD        H,A
           LD        A,L                ;Complement L
           CPL
           LD        L,A
           INC       HL                 ;Increment for 1's complement
           RET

DivErr:    Display   DivZero
           CALL      Trace
           JP        Return
;
; Display current environment: output 40 characters


; before and after current character position
; and show current position.
; Destroys all registers.
;
Trace:     Display   CrLf
           LD        HL,(CharPos)
           LD        DE,Width/2         ;Half screen width
           SBC       HL,DE              ;HL := character position - 40
           LD        B,Width            ;# of characters to be displayed
tr1:       LD        A,(HL)             ;Get a character
           PUSH      HL
           LD        DE,Prog
           SBC       HL,DE
           JP        M,tr2              ;Convert character to blank
           LD        HL,ProgTop-1       ;if it is outside the program buffer
           POP       DE
           PUSH      DE
           SBC       HL,DE              ;or not an ASCII graphic character
           JP        M,tr2
           CP        Graphic
           JP        P,tr3
tr2:       LD        A,' '
tr3:       LD        E,A


           PUSH      BC
           cpm       ConOut             ;Display the character
           POP       BC
           POP       HL
           INC       HL
           DJNZ      tr1                ;Display 80 characters
           Display   CrLf
           LD        B,Width/2
tr4:       LD        E,' '
           PUSH      BC
           cpm       ConOut             ;Display 40 blanks
           POP       BC
           DJNZ      tr4
           LD        E,'^'
           cpm       ConOut             ;Point to offending character
           Display   CrLf
           RET
;
;
; Display a message.  HL is the address of a string of bytes
; terminated by a zero byte.  Destroys all registers.
;
Disp:      LD       A,(HL)              ;Get a character from message


           OR       A
           RET      Z                   ;Finished if zero
           INC      HL
           PUSH     HL                  ;Save pointer
           LD       E,A
           cpm      ConOut              ;Display the character
           POP      HL
           JP       Disp
;
; Return to operating system
;
Return:    Display   CrLf
           Display   Signoff
           LD        HL,(Ccp)           ;Quiet return (05/03/86)
           LD        SP,HL
           RET

;
; Messages
;
CrLf:      DB        Cr,Lf,0
IllChar:   DB        Cr,Lf,'Illegal character',0
TooLong:   DB        ' Program is too long for buffer.',0


OpnFail:   DB        'Usage: MOUSE filename.mse',Cr,Lf,0
Loaded:    DB        0
Reading:   DB        0
DivZero:   DB        Cr,Lf,'Division/modulus by zero',0
;
; Data
;
Ccp:       DW        0                  ;Ccp address; used for quiet return
CharPos:   DW        0                  ;Current character pointer
OffSet:    DW        0                  ;Current variable address offset
NxtFree:   DW        0                  ;Next free address for local data
Tracing:   DB        0                  ;0 = no tracing, 1 = tracing
Signon:    DB        'MOUSE.Z80, 2/21/93',Cr,Lf,0
Signoff:   DB        0
MacDefs:   DB        0                  ;Macro definition table
Prog       EQU       MacDefs+MacSize    ;Start of Mouse program
ProgTop    EQU       Prog+ProgSiz       ;End of Mouse program
Stack      EQU       ProgTop+StkSize    ;Top of local stack
CalcLim    EQU       Stack+1            ;Limit of calculation stack
CalStak    EQU       CalcLim+StkSize    ;Top of calculation stack
EnvLim     EQU       CalStak+1          ;Limit of environment stack
EnvStak    EQU       EnvLim+EnvSize     ;Top of environment stack
Data       EQU       EnvStak+1          ;Local data area


MaxAddr    EQU       2*LocSize*MaxLev+Data
           END
