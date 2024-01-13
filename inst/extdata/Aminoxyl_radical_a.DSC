#DESC	1.2 * DESCRIPTOR INFORMATION ***********************
*
*	Dataset Type and Format:
*
DSRC	EXP
BSEQ	BIG
IKKF	REAL
XTYP	IDX
YTYP	NODATA
ZTYP	NODATA
*
*	Item Formats:
*
IRFMT	D
*
*	Data Ranges and Resolutions:
*
XPTS	1500
XMIN	3332.700000
XWID	300.500000
*
*	Documentational Text:
*
TITL	'1D_FieldSweep'
IRNAM	'Intensity'
XNAM	'Field'
IRUNI	''
XUNI	'G'
*
************************************************************
*
#SPL	1.2 * STANDARD PARAMETER LAYER
*
OPER    xuser
DATE    01/10/24
TIME    17:26:44
CMNT    ASH-KABNO-H + H2O
SAMP    KABNO
SFOR
STAG    L
EXPT    CW
OXS1    IADC
AXS1    B0VL
AXS2    NONE
AXS3
A1CT    0.348
A1SW    0.03
MWFQ    9.806665e+09
MWPW    0.00317
AVGS    31
SPTP    0.01606
RCAG    24
RCHM    1
B0MA    0.00012
B0MF    100000
RCPH    0.0
RCOF    0.0
A1RS    1500
RCTC    0.00512
STMP    248.39
*
************************************************************
*
#DSL	1.0 * DEVICE SPECIFIC LAYER
*

.DVC     acqStart, 1.0


.DVC     ezcw, 1.0

MetalIonsDef       False
OrgRadicalsDef     False

.DVC     fieldCtrl, 1.0

AllegroMode        True
CenterField        3480.00 G
Delay              0.0 s
FieldFlyback       On
FieldWait          Wait LED off
GFactor            2.000000
MeasuringHall      False
SetToSampleG       False
StaticFieldMon     3330.000 G
SweepDirection     Up
SweepWidth         300.0 G
WidthTM            200.0 G

.DVC     fieldSweep, 1.0


.DVC     freqCounter, 1.0

FrequencyMon       9.806665 GHz
QMonitBridge       On

.DVC     mwBridge, 1.0

AcqFineTuning      Never
AcqScanFTuning     Off
AcqSliceFTuning    Off
BridgeCalib        100.0
Power              3.170 mW
PowerAtten         18.0 dB
QValue             2100

.DVC     recorder, 1.0

BaselineCorr       On
NbScansAcc         31
NbScansDone        31
NbScansToDo        240
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
ConvTime           16.06 ms
DModDetectSCT      First
DualDetect         OFF
EliDelay           1.0 us
EnableImag         Disable
ExtLockIn          False
ExtTrigger         False
Gain               24 dB
Harmonic           1
HighPass           True
Integrator         False
IsCalibExp         False
ModAmp             1.200 G
ModFreq            100.00 kHz
ModPhase           0.0
ModResol           6.000
Offset             0.0 %
PolyCof            {2;3,9;0} 0,1.00179,0,0,-0.00307086,0,0,-0.0265409,0,0,0.000297603,0,0,0.000223277,0,0,-4.53833e-06,0,0,-4.1451e-07,0,0,1.89417e-08,0,0,-1.48241e-09,0
QuadMode           False
ResCenter          61 mm
ResLength          23 mm
Resolution         1500
Resonator          1
SctNorm            True
SctRevision        Allegro
SpuExtension       False
SweepTime          24.09 s
TimeConst          5.12 ms
TimeExp            False
TuneCaps           34

.DVC     tempMonit, 1.0

AcqWaitTime        5.0 s
Temperature        295.00 K
TemperatureMon     248.39 K
Tolerance          1.00 K

*
************************************************************
