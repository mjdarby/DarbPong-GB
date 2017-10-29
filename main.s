;;; DarbPong, a rubbish Pong clone in gb-z80
;;; by MJDarby <me@mjdarby.net>

;;; Based heavily off of feeb's 'hello world demo'
;;; http://pp.feeb.dog/gb_z80_helloworld.txt

;;; [Information]
;;; Please see README.md for information on compilation, linking, etc.

;;; [Memory layout] (comments from feeb)
.memorymap
slotsize $4000             ; size of memory blocks
defaultslot 0             ; slot we will be writing to
slot 0 $0000             ; mapped to rom in hardware
slot 1 $4000             ; mapped to ram
.endme

.rombanksize $4000        ; size of the rom bank
.rombanks 2             ; this configuration is additionally written to
                ; 0x148. (wla-dx complains if this is not
                ; defined via a directive)

;;; [ROM configuration] (comments from feeb)
.org $100       ; if validation is successful, execution starts
                ; at memory addrexx 0x100.
_start:
    nop
    jr boot     ; the nintendo logo is checked at 0x104, so we
                ; need to quickly jump.

nintendo_logo:
    .org $104
    .db $CE $ED $66 $66 $CC $0D $00 $0B $03 $73 $00 $83 $00 $0C $00 $0D
    .db $00 $08 $11 $1F $88 $89 $00 $0E $DC $CC $6E $E6 $DD $DD $D9 $99
    .db $BB $BB $67 $63 $6E $0E $EC $CC $DD $DC $99 $9F $BB $B9 $33 $3E

configuration:
    ;; title of game in uppercase ascii. this is located in 0x0134
    ;; but this wla-dx specific directive makes this more legible
    .name "DARB PONG"

    .org $143        ; cartridge type
    .db $0             ; 0x00 = not a color gb

    .org $144         ; licensing code
                ; used in calculating checksum and complement
    .db $0            ; set to 0 by default
    .db $0            ; "

    .org $146         ; super gb function availability
    .db $0            ; 0x00 = no super gb function

    .org $147        ; cartridge architecture
    .DB $0            ; 0x00 = rom only

    .org $148        ; rom size
    .db $0            ; 0x00 = 256kbit

    .org $149         ; ram size
    .db $0             ; 0x00 none

    .org $14A        ; destination code
    .db $1             ; 0x01 = non-japanese

    .org $14B         ; old licensing code
    .db $0             ; dummy value

    .org $14C         ; mask rom version number
    .db $0            ; typical value

    ;; gameboy will not boot the rom if the checksum bytes aren't set
    ;; correctly. these are wla-dx specific directives that will calculate
    ;; these bytes, which saves us some headache.
    .computechecksum
    .computecomplementcheck


;;; [Interrupt setup] (comments from Matt)
;;; Other than VBlank, we don't want any other interrupts and have them
;;; return immediately.
.org $40                ; We define our own vblank interrupt so we avoid
    jp vblank	        ; writing to VRAM + OAM while it's being accessed
    reti
.org $48                ; LCDC status interrupt
    reti
.org $50                ; Timer overflow interrupt
    reti
.org $58                ; Serial transfer interrupt
    reti
.org $60                ; Joypad interrupt
    reti

;;; [RAM setup] (comments from Matt)
;;; Now we gain control of the runtime. We need to configure some additional options
;;; to do with the LCD display, as well as put together our sprites.

;;; [LCD setup] (comments from feeb)
.org $150            ; the first free byte after checksums
boot:
    di            ; disable interrupts
    nop            ; wait for disabled interrupts to take effect

    ;; Enable interrupts
    ld hl, $FFFF
    ld (hl), $00000001
    ;; window/background display configuration (LCDC)
    ;; 7: lcd control (1 = lcd on)
    ;; 6: window tile map memory position select (0 = 0x9800 -> 0x9BFF)
    ;; 5: window display (0 = off)
    ;; 4: bg & window tile memory position select (1 = 0x8000 -> 0x8FFF)
    ;; 3: bg tile map display memory position select (1 = 0x9C00 -> 0x9FFF)
    ;; 2: object (aka sprite) size (0 = 8x8)
    ;; 1: object (aka sprite) display (1 = on)
    ;; 0: bg & window display (1 = on)
    ld hl, $FF40
    ld (hl), %10011011

    ;; palette configuration
    ld a, %11100100        ; default value (11 darkest, 00 lightest, etc)
    cpl
    ldh ($47), a        ; palette for background tiles (BGP)
    ldh ($48), a        ; palette 1 for sprites (OBP0)
    ldh ($49), a         ; palette 2 for sprites (OBP1)

;;; [Constants] (comments from Matt)
;;; These are for the benefit of the developer - we define a number of
;;; constants that refer to either specific memory addresses where we
;;; will store values, or simply values that we don't want to have to
;;; chage everywhere each time we want to modify htem.
	
;;; Memory addresses
.define VblnkFlag $E000
.define LeftPaddleYLocation $DFF8
.define UpHeld $DFF0
.define DownHeld $DFE8
.define BallXLocation $DFE0
.define BallYLocation $DFD8
.define RightPaddleYLocation $DFD0
.define BallDirection $DFC8
.define NewBallDirection $DFC0
	
;;; Movement and speed constants
.define Speed 1
.define Up 1
.define Down 2
.define Left 4
.define Right 8
.define UpLeft 5
.define UpRight 9
.define DownLeft 6
.define DownRight 10
	
;;; Paddle X co-ordinates and offsets
.define LeftPaddleXLocation $12
.define RightPaddleXLocation $96
.define PaddleFrontOffset 4
.define LeftPaddleXFront LeftPaddleXLocation + PaddleFrontOffset
.define RightPaddleXFront RightPaddleXLocation + (PaddleFrontOffset / 2)
	
;;; Ball offset
.define BallMiddleOffset 3

;;; [Global variables] (comments from Matt)
;;; DarbPong needs to keep track of multiple different variables.
;;; We configure their initial values here.
    ld a, 0
    ld (VblnkFlag), a 		; Used in main to decide if we have to process the next frame's logic

    ld a, 64
    ld (LeftPaddleYLocation), a	; Starting Y co-ordinate for left paddle

    ld a, 64
    ld (RightPaddleYLocation), a ; Starting Y co-ordinate for right paddle

    ld a, 64
    ld (BallYLocation), a 	; Starting co-ordinates for ball

    ld a, 64
    ld (BallXLocation), a

    ld a, DownLeft
    ld (BallDirection), a	; Initial ball direction

    ei 				; We're ready to go - turn on interrupts
    nop

    call main 			; Go to our main loop

;;; [Loading sprite data] (comments from Matt)
load_tile:
;;; There's faster ways to do this (DMA etc.) but this will do
;;; We first to go the beginning of video ram, and then write our
;;; sprite information directly, 16 bytes at a time.
    ld hl, $8000

    ld bc, $1
    ;; Background
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc

    ;;  Ball
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $3c
    add hl, bc
    ld (hl), $3c
    add hl, bc
    ld (hl), $66
    add hl, bc
    ld (hl), $66
    add hl, bc
    ld (hl), $c3
    add hl, bc
    ld (hl), $c3
    add hl, bc
    ld (hl), $66
    add hl, bc
    ld (hl), $66
    add hl, bc
    ld (hl), $3c
    add hl, bc
    ld (hl), $3c
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $00
    add hl, bc
    ld (hl), $00
    add hl, bc

    ;;  Paddle
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ld (hl), $18
    add hl, bc
    ret

;;; [Display code] (comments from Matt)
;;; The next functions deal with 'rendering', which is just a grandiose way of saying
;;; we write information to the object memory, stating the x/y co-ordinates of each
;;; sprite to display, as well as the index of the sprite in VRAM.
	
;;; Fancy bg_display code from feeb's example - populates 9FFF through 9C00 (bg memory) with zeroes,
;;; which works to display our flat black background in this case.
bg_display:
    ld h, $9F

bg_d_loop:
    dec l
    ld (hl), $0
    jr nz, bg_d_loop

    dec h
    ld a, $9B
    cp h
    jr nz, bg_d_loop

    ret

;;; Less complicated, we now have to populate ORAM with the positions of both paddles
;;; and the ball. Format is 
;;; <Y co-ord - 1 byte>
;;; <X co-ord - 1 byte>
;;; <Sprite index in VRAM - 1 byte>
;;; <Attributes - 1 byte, zero in our case>
;;; And then on to the next one.
ball_render:
    ld hl, $FE00 		; Start of ORAM
    ld bc, $1			; Helper value for moving one byte at a time
    ld a, (BallYLocation)	; Grab the ball's Y-location
    ld (hl), a			; Write to OAM
    add hl, bc			; On to the next byte
    ld a, (BallXLocation)	; Same but now for X-location
    ld (hl), a
    add hl, bc
    ld (hl), $1			; Sprite index is $1 (not $0, which has our background sprite)
    add hl, bc
    ld (hl), $0			; No attributes
    ret

left_bat_render:
;;; As above, but with sprite index 2 and the left paddle location
    add hl, bc
    ld a, (LeftPaddleYLocation)
    ld (hl), a
    add hl, bc
    ld (hl), LeftPaddleXLocation
    add hl, bc
    ld (hl), $2
    add hl, bc
    ld (hl), $0
    ret

right_bat_render:
;;; As above, but with sprite index 2 and the right paddle location
    add hl, bc
    ld bc, $1
    ld a, (RightPaddleYLocation)
    ld (hl), a
    add hl, bc
    ld (hl), RightPaddleXLocation
    add hl, bc
    ld (hl), $2
    add hl, bc
    ld (hl), $0
    ret

vblank:
;;; Our vblank routine, which is called just after a frame is rendered
;;; It is the only safe time to write to VRAM/OAM
    push af			; Push valuable register data

    call load_tile		; Load our data into VRAM (should really only do this once)
    call bg_display		; Background draw
    call ball_render		; Sprite darw
    call left_bat_render
    call right_bat_render

    ld a, 1
    ld (VblnkFlag), a		; Let main know we just ran a vblank interrupt

    pop af
    reti			; Return and re-enable interrupts

;;; [Joypad control logic] (comments from Matt)
;;; This isn't going to be pretty.
control:
    ld a, $20
    ld ($FF00), a 		; Set bit 5 of the hardware register.
    ld a, ($FF00)		; This lets us then read the register to
    ld a, ($FF00)		; get the state of the up/down buttons.
    ld a, ($FF00)		; Do this like 5 times to make sure the
    ld a, ($FF00)		; register is updated.
    cpl				; Complement the result
    and $0F			; Discard the bottom bits
    ld c, a			; Copy result
    and $8 ; Is down held?	; Check down bit
    jp nz,setDown		; If it's set, tell the application
    ld a, $0			; Otherwise reset the DownHeld variable
    ld (DownHeld), a
    jp testUp
setDown:
    ld a, $1
    ld (DownHeld), a
testUp:
    ld a, c			; Grab our copy from earlier
    and $4			; Test for up this time
    jp nz,setUp			; Same deal as before
    ld a, $0
    ld (UpHeld), a
    ret
setUp:
    ld a, $1
    ld (UpHeld), a
    ret

;;; [Logic] (comments from Matt)
logic:
    call movePlayerPaddle
    call moveAIPaddle
    call moveBall
    ret

moveBall:
;;; Move the ball in the direction it is currently travelling
;;; then run the collision check. This action is undone
;;; if a collision is found.
    ld a, (BallDirection)
upLeftCheck:
    cp UpLeft
    jp nz, upRightCheck
    call MoveUpLeft
    jp collisionCheck
upRightCheck:
    cp UpRight
    jp nz, downLeftCheck
    call MoveUpRight
    jp collisionCheck
downLeftCheck:
    cp DownLeft
    jp nz, downRight
    call MoveDownLeft
    jp collisionCheck
downRight:
    call MoveDownRight
    jp collisionCheck

collisionCheck:
;;; Logic: Determine collision and new bearing, undo move if collision has happened, apply new bearing
;;; First, store the new ball direction as the current direction
    ld a, (BallDirection)
    ld (NewBallDirection), a
;;; Next, see if we hit the left or right walls (To be changed in favour of scoring)
edgeChecks:
    ld a, (BallXLocation)
leftCheck:
    cp 7
    jp nz, rightCheck
    ld a, (NewBallDirection)
    xor Left
    xor Right
    ld (NewBallDirection), a
    jp upDownCheck
rightCheck:
    cp 160
    jp nz, paddleChecks
    ld a, (NewBallDirection)
    xor Left
    xor Right
    ld (NewBallDirection), a

;;; How about the paddles?
paddleChecks:
    ; Check if ball X location (+0 or +8) is the same as the front of a paddle
    ; Check if ball Y + ball middle offset is between paddle Y and paddle Y + sprite height
    ; paddleY < ballY + ballMiddleOffset < paddleY+8
    ; 0 < ballY + ballMiddleOffset - paddleY < 8
    ; Reverse ball direction if true
leftPaddleCheck:
    ld a, (BallXLocation)
    cp LeftPaddleXFront
    jp nz, rightPaddleCheck
    ld a, (LeftPaddleYLocation)
    ld c, a
    ld a, (BallYLocation)
    add BallMiddleOffset
    cp c
    jp c,rightPaddleCheck
    ld a, c
    add 8
    ld c, a
    ld a, (BallYLocation)
    cp c
    jp nc, rightPaddleCheck
    ld a, (NewBallDirection)
    xor Left
    xor Right
    ld (NewBallDirection), a


rightPaddleCheck:
    ld a, (BallXLocation)
    add 8
    cp RightPaddleXFront
    jp nz, upDownCheck
    ld a, (RightPaddleYLocation)
    ld c, a
    ld a, (BallYLocation)
    add BallMiddleOffset
    cp c
    jp c,upDownCheck
    ld a, c
    add 8
    ld c, a
    ld a, (BallYLocation)
    cp c
    jp nc, upDownCheck
    ld a, (NewBallDirection)
    xor Left
    xor Right
    ld (NewBallDirection), a

;;; Finally, check the ceiling/floor.
upDownCheck:
    ld a, (BallYLocation)
upCheck:
    cp 16
    jp nz, downCheck
    ld a, (NewBallDirection)
    xor Up
    xor Down
    ld (NewBallDirection), a
downCheck:
    cp 153
    jp nz, collisionDone
    ld a, (NewBallDirection)
    xor Up
    xor Down
    ld (NewBallDirection), a
	
;;; Checks done - undo the move if the new direction
;;; differs from the old direction.
collisionDone:
    ld a, (NewBallDirection)
    ld c, a
    ld a, (BallDirection)
    xor c
    jp z, skipUndo
    call undoMove
skipUndo:
    ld a, (NewBallDirection)
    ld (BallDirection), a
    ret

;;; Undo Move gets called in case of collision
undoMove:
    ld a, (BallDirection)
upLeftCheckUndo:
    cp UpLeft
    jp nz, upRightCheckUndo
    call MoveDownRight
    ret
upRightCheckUndo:
    cp UpRight
    jp nz, downLeftCheckUndo
    call MoveDownLeft
    ret
downLeftCheckUndo:
    cp DownLeft
    jp nz, downRightUndo
    call MoveUpRight
    ret
downRightUndo:
    call MoveUpLeft
    ret

;;; [Updating movement variables] (comments from Matt)
;;; Very simple functions for updating the x/y of the ball
;;; based off of the ball's current movement direction
MoveUpLeft:
    ld a, (BallXLocation)
    sub Speed
    ld (BallXLocation), a
    ld a, (BallYLocation)
    sub Speed
    ld (BallYLocation), a
    ret

MoveUpRight:
    ld a, (BallXLocation)
    add Speed
    ld (BallXLocation), a
    ld a, (BallYLocation)
    sub Speed
    ld (BallYLocation), a
    ret

MoveDownLeft:
    ld a, (BallXLocation)
    sub Speed
    ld (BallXLocation), a
    ld a, (BallYLocation)
    add Speed
    ld (BallYLocation), a
    ret

MoveDownRight:
    ld a, (BallXLocation)
    add Speed
    ld (BallXLocation), a
    ld a, (BallYLocation)
    add Speed
    ld (BallYLocation), a
    ret

;;; Worst AI ever - never loses because it always updates
;;; the paddle position to the ball position.
moveAIPaddle:
    ld a, (BallYLocation)
    ld (RightPaddleYLocation), a
    ret

;;; Update the player position according to the joypad state.
movePlayerPaddle:
    ld a, (DownHeld)
    and $1
    jp nz, upTest
    ld a, (LeftPaddleYLocation)
    dec a
    cp 15
    jp nz, updateDown
    inc a
updateDown:
    ld (LeftPaddleYLocation), a
upTest:
    ld a, (UpHeld)
    and $1
    jp nz, logicEnd
    ld a, (LeftPaddleYLocation)
    inc a
    cp 153
    jp nz, updateUp
    dec a
updateUp:
    ld (LeftPaddleYLocation), a
logicEnd:
    ret

;;; Our main loop!
main:
    halt 			; Do nothing until we have an interrupt
    nop

    ld a, (VblnkFlag); What's that interrupt?
    or a
    jr z, main ; Not a VBlank? Back to the start.

;;; We've just had a vblank, process logic for next fraem
;;; and reset vblank flag
    xor a
    ld (VblnkFlag), a

;;; Rendering has already been done via vblank, let's get the next
;;; frame ready.
    call control
    call logic

;;;  One more time!
    jr main
