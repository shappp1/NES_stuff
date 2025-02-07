.segment "HEADER"
.byte "NES",$1a,$02,$01,$00,$08

.segment "VECTORS"
.addr vblank,reset,irq

.segment "ZEROPAGE"
frame_timer: .res 1
frame_complete: .res 1

goomba_x_pos: .res 2
goomba_y_pos: .res 2
goomba_x_vel: .res 1
goomba_y_vel: .res 1

controller: .res 1

.code
vblank:
  lda frame_complete
  beq irq

  lda #$00
  sta $2003
  lda #$02
  sta $4014

  lda #$00
  sta $2005
  sta $2005
  
  inc frame_timer
  inc frame_complete
irq: rti

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

reset:
  sei ; ignore interrupts
  cld ; disable decimal mode
  ldx #$40
  stx $4017 ; diable sound IRQ
  ldx #$FF
  txs ; set stack pointer
  inx
  stx $2000 ; clear PPUCTRL register
  stx $2001 ; clear PPUMASK register
  stx $4010 ; disable PCM
  stx frame_timer
  stx frame_complete

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

: bit $2002 ; wait for vblank
  bpl :-

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

  ldx #$00
: lda sprite,x
  sta $0200,x
  inx
  cpx #sprite_end
  bne :-

  cli
  lda #$90
  sta $2000
  lda #$1E
  sta $2001

  lda #$10
  sta goomba_x_pos
  lda #$C0
  sta goomba_y_pos

; convention: always jmp to frame_start and frame_end
frame_start: ; the main game loop hell yeah
  lda frame_timer
  and #$0F
  bne :+
  ; swap OAM[9] and OAM[13]
  lda $0209
  ldx $020D
  stx $0209
  sta $020D
:
  jsr ReadController

  lda #$00
  sta goomba_x_vel

  lda controller
  and #$01 ; right
  beq :+
  inc goomba_x_vel
: lda controller
  and #$02 ; left
  beq :+
  dec goomba_x_vel
:

  lda goomba_x_pos
  clc
  adc goomba_x_vel
  sta goomba_x_pos

  lda goomba_x_pos
  sta $0203
  sta $020B
  clc
  adc #$08
  sta $0207
  sta $020F
  lda goomba_y_pos
  sta $0200
  sta $0204
  clc
  adc #$08
  sta $0208
  sta $020C
  
frame_end:
  dec frame_complete
: lda frame_complete
  bne :-
  jmp frame_start

.rodata
pallete:
  .byte $31,$0F,$16,$36, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00 ; background
  .byte $31,$0F,$16,$36, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00 ; foreground
  
sprite: ; Y, SPRITE #, %VHB000PP, X
  ; the mario brothers goomba from the game the mario game brothers goomba
  .byte $C0,$00,$00,$10, $C0,$00,$40,$18, $C8,$01,$00,$10, $C8,$02,$40,$18
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