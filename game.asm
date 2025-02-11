.feature force_range

.segment "HEADER"
.byte "NES",$1a,$02,$01,$00,$08

.segment "VECTORS"
.addr vblank,reset,irq

.segment "ZEROPAGE"
frame_counter: .res 1 ; increments each frame
processing_complete: .res 1 ; 0 if frame hasn't finished processing, 1 if it has

; PLAYER DATA
player_x_pos: .res 2
player_y_pos: .res 2
player_x_vel: .res 1
player_x_vel_target: .res 1
player_y_vel: .res 1
player_in_air: .res 1
player_direction: .res 1

bullet_count: .res 1

; OBJECT FORMAT
; 1. %wwhh => w/h + 1 = width/height of object in tiles

controller: .res 1

.code
vblank:
  lda processing_complete
  beq irq

  lda #$00
  sta $2003
  lda #$02
  sta $4014

  lda #$00
  sta $2005
  sta $2005
  
  dec processing_complete
irq: rti

; modifies a and x, returns controller bits in controller address
.proc ReadController
  ldx #$01
  stx controller

  stx $4016
  dex
  stx $4016

: lda $4016
  lsr a ; puts bit 0 into C
  rol controller ; rolls C into controller byte
  bcc :- ; branches until the 1 in controller gets rolled out

  rts
.endproc

; a = pos low byte, x = pos high byte, returns pos in pixels in a, also modifies y
.proc GetPosPixels
  and #$F0
  ora #$08 ; carry flag will be set after 4 shifts
: tay
  txa
  lsr a
  tax
  tya
  ror a
  bcc :-
  rts
.endproc

reset:
  sei ; ignore interrupts
  cld ; disable decimal mode
  ldx #$40
  stx $4017 ; diable sound IRQ
  ldx #$FF
  txs ; set stack pointer
  inx
  stx $2000 ; disable NMI (in case PPU not reset to power up state)
  stx $2001 ; disable rendering (in case PPU not reset to power up state)
  stx $4010 ; disable PCM
  stx frame_counter
  stx processing_complete
  stx player_direction

  bit $2002
: bit $2002 ; wait for vblank
  bpl :-

: lda #$00
  sta $0000,x ; clear RAM
  sta $0100,x
  sta $0300,x
  sta $0400,x
  sta $0500,x
  sta $0600,x
  sta $0700,x
  lda #$FF
  sta $0200,x ; store #$FF instead of #$00 (OAM space)
  inx
  bne :-

  ; load sprites into OAM
  ldx #$00
: lda sprite,x
  sta $0200,x
  inx
  cpx #sprite_end
  bne :-

: bit $2002 ; wait for vblank (PPU should be ready after)
  bpl :-

  ; load palette data (woah really)
  lda #$3F
  sta $2006
  lda #$00
  sta $2006
  tax
: lda pallete,x
  sta $2007
  inx
  cpx #$20
  bne :-

  ; fill first nametable with zero
  lda #$20
  sta $2006
  lda #$00
  sta $2006
  tax
  ldy #$04
: sta $2007
  inx
  bne :-
  dey
  bne :-

; loads ground into first nametable
  lda #$23
  sta $2006
  lda #$40
  sta $2006
  ldy #$02
: ldx #$00
: lda background,x
  sta $2007
  inx
  cpx #$40
  bne :-
  dey
  bne :--

  cli
  lda #$90
  sta $2000 ; could call NMI immediately but processing_complete will be 0
  lda #$1E
  sta $2001

  lda #$01
  sta player_x_pos+1
  lda #$0C
  sta player_y_pos+1
  lda #$00
  sta player_x_pos
  sta player_y_pos

;; CONSTANTS
PLAYER_X_VEL = $28 ; 2.5 px/f
PLAYER_JUMP_VEL = -$50 ; 4.0 px/f
PLAYER_Y_ACC = $04 ; 0.0625 px/f^2

; ORDER:
;   read controller
;   handle x velocity
;   handle jump (y velocity)
;   handle gravity (y acceleration)
;   add x/y velocity to respective positions and sign extend
;   check if y position collides with floor
;   load positions into OAM
;   set player direction
frame_start:
  jsr ReadController

  ; handle x velocity
  ldx #$00
  lda controller
  and #$01 ; right
  beq :+
  txa
  clc
  adc #PLAYER_X_VEL
  tax
  lda #$00
  sta player_direction
: lda controller
  and #$02 ; left
  beq :+
  txa
  sec
  sbc #PLAYER_X_VEL
  tax
  lda #$01
  sta player_direction
:
  stx player_x_vel

  ; handle jump
  lda player_in_air
  bne :+
  lda controller
  and #$08 ; up
  beq :+
  lda #PLAYER_JUMP_VEL
  sta player_y_vel
  inc player_in_air
:
  ; handle gravity
  lda player_y_vel
  clc
  adc #PLAYER_Y_ACC
  bvc :+ ; prohibit velocity from overflowing (probably should be seperated from gravity code)
  lda #$7F
: sta player_y_vel

  ; add x velocity to position
  lda player_x_vel
  clc
  adc player_x_pos
  sta player_x_pos
  lda player_x_vel ; sign extend velocity
  and #$80 ; if postive then accumulator will be zero
  bpl :+
  lda #$FF
: adc player_x_pos+1
  sta player_x_pos+1

  ; add y velocity to position
  lda player_y_vel
  clc
  adc player_y_pos
  sta player_y_pos
  lda player_y_vel ; sign extend velocity
  and #$80 ; if postive then accumulator will be zero
  bpl :+
  lda #$FF
: adc player_y_pos+1
  sta player_y_pos+1

  ; check y position for floor
  lda player_y_pos+1
  and #$0F
  cmp #$0F
  beq :+
  cmp #$0C
  bmi :+
  lda #$0C
  sta player_y_pos+1
  lda #$00
  sta player_y_pos
  sta player_in_air
:

  ; load player position into OAM
  lda player_x_pos
  ldx player_x_pos+1
  jsr GetPosPixels
  sta $0203
  sta $020B
  clc
  adc #$08
  sta $0207
  sta $020F
  lda player_y_pos
  ldx player_y_pos+1
  jsr GetPosPixels
  sta $0200
  sta $0204
  clc
  adc #$08
  sta $0208
  sta $020C
  
  ; handle player facing
  lda #$00
  ldx player_direction
  beq :+
  lda #$40
: sta $0202
  sta $0206
  sta $020A
  sta $020E
  ; hack to change order of tiles - not scalable
  lda #$00
  eor player_direction
  sta $0201
  lda #$01
  eor player_direction
  sta $0205
  lda #$10
  eor player_direction
  sta $0209
  lda #$11
  eor player_direction
  sta $020D

frame_end:
  inc frame_counter
  inc processing_complete
: lda processing_complete
  bne :-
  jmp frame_start

.rodata
pallete:
  .byte $31,$0F,$16,$36, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00 ; background
  .byte $31,$0F,$26,$0A, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00 ; foreground
  
sprite: ; Y, SPRITE #, %VHB000PP, X
  ; player OAM data
  .byte $00,$00,$00,$00, $00,$01,$00,$00, $00,$10,$00,$00, $00,$11,$00,$00
  ; beautiful pony
  ; .byte $90,$08,$00,$80, $90,$09,$00,$88, $90,$0A,$00,$90, $90,$0B,$00,$98, $90,$0C,$00,$A0, $90,$0D,$00,$A8, $90,$0E,$00,$B0, $90,$0F,$00,$B8
  ; .byte $98,$18,$00,$80, $98,$19,$00,$88, $98,$1A,$00,$90, $98,$1B,$00,$98, $98,$1C,$00,$A0, $98,$1D,$00,$A8, $98,$1E,$00,$B0, $98,$1F,$00,$B8
  ; .byte $A0,$28,$00,$80, $A0,$29,$00,$88, $A0,$2A,$00,$90, $A0,$2B,$00,$98, $A0,$2C,$00,$A0, $A0,$2D,$00,$A8, $A0,$2E,$00,$B0, $A0,$2F,$00,$B8
  ; .byte $A8,$38,$00,$80, $A8,$39,$00,$88, $A8,$3A,$00,$90, $A8,$3B,$00,$98, $A8,$3C,$00,$A0, $A8,$3D,$00,$A8, $A8,$3E,$00,$B0, $A8,$3F,$00,$B8
  ; .byte $B0,$48,$00,$80, $B0,$49,$00,$88, $B0,$4A,$00,$90, $B0,$4B,$00,$98, $B0,$4C,$00,$A0, $B0,$4D,$00,$A8, $B0,$4E,$00,$B0, $B0,$4F,$00,$B8
  ; .byte $B8,$58,$00,$80, $B8,$59,$00,$88, $B8,$5A,$00,$90, $B8,$5B,$00,$98, $B8,$5C,$00,$A0, $B8,$5D,$00,$A8, $B8,$5E,$00,$B0, $B8,$5F,$00,$B8
  ; .byte $C0,$68,$00,$80, $C0,$69,$00,$88, $C0,$6A,$00,$90, $C0,$6B,$00,$98, $C0,$6C,$00,$A0, $C0,$6D,$00,$A8, $C0,$6E,$00,$B0, $C0,$6F,$00,$B8
  ; .byte $C8,$78,$00,$80, $C8,$79,$00,$88, $C8,$7A,$00,$90, $C8,$7B,$00,$98, $C8,$7C,$00,$A0, $C8,$7D,$00,$A8, $C8,$7E,$00,$B0, $C8,$7F,$00,$B8
@end:
sprite_end = (@end - sprite) & $FF

background:
  ; a line of mario brothers ground blocks
  .repeat 16
    .byte $30, $31
  .endrep
  .repeat 16
    .byte $40, $41
  .endrep

.segment "CHR0"
.incbin "rom.chr"