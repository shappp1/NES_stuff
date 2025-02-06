.segment "HEADER"
.byte "NES",$1a,$02,$01,$00,$08

.segment "VECTORS"
.word vblank,reset,irq

.segment "ZEROPAGE"
timer: .res 1

.code
vblank:
  lda #$00
  sta $2003
  lda #$02
  sta $4014

  ; ldx timer
  ; cpx #$1E ; every 0.5 seconds (hopefully)
  ; bne :+

  ; lda #$3F
  ; sta $2006
  ; lda #$10
  ; sta $2006
  ; ldx $2007
  ; inx

  ; lda #$3F
  ; sta $2006
  ; lda #$10
  ; sta $2006
  ; stx $2007

  ; ldx #$00
  ; stx timer
  ; stx $2005
  ; stx $2005
  ; ldx #$90
  ; stx $2000
  
  rti

: inx
  stx timer
irq: rti

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
  stx timer

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

  lda #$00
  sta $2003
  lda #$02
  sta $4014

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

  ldx #$00
: lda sprites,x
  sta $0200,x
  inx
  cpx #$10
  bne :-

  lda #$23
  sta $2006
  lda #$C0
  sta $2006
  lda #%01010101
  ldx #$40
: sta $2007
  dex
  bne :-

  lda #$00
  sta $2005
  sta $2005

  cli
  lda #$90
  sta $2000
  lda #$1E
  sta $2001

: jmp :-

.rodata
pallete:
  .byte $31,$0F,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00
  .byte $31,$0F,$16,$36, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00
  
sprites: ; Y, SPRITE #, attrib, X
  .byte $40,$40,$20,$40, $40,$40,$60,$48, $48,$50,$20,$40, $48,$50,$60,$48

.segment "CHR0"
.incbin "../sprites/rom.chr"