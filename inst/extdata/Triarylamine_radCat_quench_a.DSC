#DESC	1.2 * DESCRIPTOR INFORMATION ***********************
*
*	Dataset Type and Format:
*
DSRC	EXP
BSEQ	BIG
IKKF	REAL
XTYP	IDX
YTYP	IGD
ZTYP	NODATA
*
*	Item Formats:
*
IRFMT	D
YFMT	D
*
*	Data Ranges and Resolutions:
*
XPTS	2400
XMIN	3390.000000
XWID	199.916667
YPTS	100
YMIN	0.000000
YWID	1471.310000
*
*	Documentational Text:
*
TITL	'2D_Field_Delay'
IRNAM	'Intensity'
XNAM	'Field'
YNAM	'Time'
IRUNI	''
XUNI	'G'
YUNI	's'
*
************************************************************
*
#SPL	1.2 * STANDARD PARAMETER LAYER
*
OPER    xuser
DATE    01/05/22
TIME    16:18:25
CMNT    
SAMP    
SFOR    
STAG    C
EXPT    CW
OXS1    IADC
AXS1    B0VL
AXS2    ETIM
AXS3    
A2RS    100
A1CT    0.349
A1SW    0.02
MWFQ    9.792281e+09
MWPW    0.00317
AVGS    1
SPTP    0.00509
RCAG    26
RCHM    1
B0MA    7.2e-05
B0MF    100000
RCPH    0.0
RCOF    0.0
A1RS    2400
RCTC    0.00128
*
************************************************************
*
#DSL	1.0 * DEVICE SPECIFIC LAYER
*

.DVC     acqStart, 1.0


.DVC     delay, 1.0

Delay              100 ms
NbPoints           100

.DVC     ezcw, 1.0

MetalIonsDef       False
OrgRadicalsDef     False

.DVC     fieldCtrl, 1.0

AllegroMode        True
CenterField        3490.00 G
Delay              0.0 s
FieldFlyback       On
FieldWait          Wait LED off
GFactor            2.000000
MeasuringHall      False
SetToSampleG       False
StaticFieldMon     3390.000 G
SweepDirection     Up
SweepWidth         200.0 G
WidthTM            200.0 G

.DVC     fieldSweep, 1.0


.DVC     freqCounter, 1.0

FrequencyMon       9.792281 GHz
QMonitBridge       On

.DVC     mwBridge, 1.0

AcqFineTuning      Never
AcqScanFTuning     Off
AcqSliceFTuning    Off
BridgeCalib        100.0
Power              3.170 mW
PowerAtten         18.0 dB
QValue             1700

.DVC     recorder, 1.0

BaselineCorr       On
NbScansAcc         1
NbScansDone        1
NbScansToDo        1
ReplaceMode        Off
SmoothMode         Manual
SmoothPoints       0

.DVC     scanEnd, 1.0


.DVC     signalChannel, 1.0

AFCTrap            True
AllegroMode        True
AllowShortCt       False
Calibrated         True
ConvFact           8.51e-09
ConvTime           5.09 ms
DModDetectSCT      First
DualDetect         OFF
EliDelay           1.0 us
EnableImag         Disable
ExtLockIn          False
ExtTrigger         False
Gain               26 dB
Harmonic           1
HighPass           True
Integrator         False
IsCalibExp         False
ModAmp             0.720 G
ModFreq            100.00 kHz
ModPhase           0.0
ModResol           8.000
Offset             0.0 %
PolyCof            {2;3,9;0} 0,1.00179,0,0,-0.00307086,0,0,-0.0265409,0,0,0.000297603,0,0,0.000223277,0,0,-4.53833e-06,0,0,-4.1451e-07,0,0,1.89417e-08,0,0,-1.48241e-09,0
QuadMode           False
ResCenter          61 mm
ResLength          23 mm
Resolution         2400
Resonator          1
SctNorm            True
SctRevision        Allegro
SpuExtension       False
SweepTime          12.22 s
TimeConst          1.28 ms
TimeExp            False
TuneCaps           34

*
************************************************************
