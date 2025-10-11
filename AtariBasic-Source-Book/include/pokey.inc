
; *******************
; **               **
; **  POKEY: D2xx  **
; **               **
; *******************

AUDF1		=	$D200		; (W) Audio channel 1 frequency
AUDC1		=	$D201		; (W) Audio channel 1 control
AUDF2		=	$D202		; (W) Audio channel 2 frequency
AUDC2		=	$D203		; (W) Audio channel 2 control
AUDF3		=	$D204		; (W) Audio channel 3 frequency
AUDC3		=	$D205		; (W) Audio channel 3 control
AUDF4		=	$D206		; (W) Audio channel 4 frequency
AUDC4		=	$D207		; (W) Audio channel 4 control
AUDCTL		=	$D208		; (W) Audio control

POT0		=	$D200		; (R) Pot ('paddle') 0
POT1		=	$D201		; (R) Pot ('paddle') 1
POT2		=	$D202		; (R) Pot ('paddle') 2
POT3		=	$D203		; (R) Pot ('paddle') 3
POT4		=	$D204		; (R) Pot ('paddle') 4
POT5		=	$D205		; (R) Pot ('paddle') 5
POT6		=	$D206		; (R) Pot ('paddle') 6
POT7		=	$D207		; (R) Pot ('paddle') 7
ALLPOT		=	$D208		; (R) Read AD converter state

STIMER		=	$D209		; (W) Start POKEY timers
KBCODE		=	$D209		; (R) keyboard code
SKRES		=	$D20A		; (W) Reset serial port status
RANDOM		=	$D20A		; (R) random generator

POTGO		=	$D20B		; (W) go read pot-meters (AD converter)

SEROUT		=	$D20D		; (W) serial port data out
SERIN		=	$D20D		; (R) serial port data in

IRQEN		=	$D20E		; (W) IRQ enable
IRQST		=	$D20E		; (R) IRQ status

SKCTL		=	$D20F		; (W) serial port control
SKSTAT		=	$D20F		; (R) serial port status
