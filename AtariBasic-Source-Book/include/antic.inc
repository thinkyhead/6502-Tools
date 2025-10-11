
; *******************
; **               **
; **  ANTIC: D4xx  **
; **               **
; *******************

DMACTL		=	$D400		; (W) DMA Control
CHACTL		=	$D401		; (W) Character mode control
DLISTL		=	$D402		; (W) Display List pointer
DLISTH		=	$D403		; (W) Display List pointer
HSCROL		=	$D404		; (W) Horizontal scroll
VSCROL		=	$D405		; (W) Vertical scroll

PMBASE		=	$D407		; (W) Player/Missile base address

CHBASE		=	$D409		; (W) Character base address
WSYNC		=	$D40A		; (W) Wait for horizontal sync.
VCOUNT		=	$D40B		; (R) Vertical line counter
PENH		=	$D40C		; (R) Light pen horizontal position
PENV		=	$D40D		; (R) Light pen vertical position
NMIEN		=	$D40E		; (W) NMI enable
NMIRES		=	$D40F		; (W) NMI reset
NMIST		=	$D40F		; (R) NMI status
