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
XPTS	1000
XMIN	2.000000
XWID	40.000000
*
*	Documentational Text:
*
TITL	'ENDOR'
IRNAM	'Intensity'
XNAM	'RF'
IRUNI	''
XUNI	'MHz'
*
************************************************************
*
#SPL	1.2 * STANDARD PARAMETER LAYER
*
OPER    xuser
DATE    08/22/22
TIME    15:37:43
CMNT    
SAMP    
SFOR    
STAG    C
EXPT    CW
OXS1    IADC
AXS1    R1VL
AXS2    NONE
AXS3    
R1AT    16
R1FM    100000
A1LL    2000000
A1SW    4e+07
B0VL    0.3385339
MWFQ    9.500219e+09
MWPW    0.002
AVGS    8
SPTP    0.016
RCAG    60
RCHM    1
B0MA    1e-07
R1MF    20000
RCPH    0.0
RCOF    0.0
A1RS    1000
RCTC    0.00512
STMP    230.57
*
************************************************************
*
#DSL	1.0 * DEVICE SPECIFIC LAYER
*

.DVC     acqStart, 1.0


.DVC     endor, 1.0

ENDORType          SEIF
RF1Atten           16 dB
RF1FreqPos         42.000 MHz
RF1ModDepth        100.000 kHz
RF1Resolution      0.04000 MHz/pt
RF1StartFreq       2.000 MHz
RF1SweepWidth      40.000 MHz

.DVC     endorSweep, 1.0


.DVC     fieldCtrl, 1.0

AllegroMode        True
Delay              0.0 s
FieldFlyback       On
FieldWait          Wait LED off
GFactor            2.000000
MeasuringHall      False
SetToSampleG       False
StaticField        3385.339 G
StaticFieldMon     3385.339 G
SweepDirection     Up
WidthTM            200.0 G

.DVC     freqCounter, 1.0

FrequencyMon       9.500219 GHz
QMonitBridge       On

.DVC     mwBridge, 1.0

AcqFineTuning      Never
AcqScanFTuning     Off
AcqSliceFTuning    Off
BridgeCalib        100.0
Power              2.000 mW
PowerAtten         20.0 dB
QValue             900

.DVC     recorder, 1.0

BaselineCorr       On
NbScansAcc         8
NbScansDone        8
NbScansToDo        8
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
ConvTime           16.00 ms
DModDetectSCT      First
DualDetect         OFF
EliDelay           1.0 us
EnableImag         Disable
ExtLockIn          False
ExtTrigger         False
Gain               60 dB
Harmonic           1
HighPass           True
Integrator         True
IsCalibExp         False
ModAmp             0.001 G
ModFreq            20.00 kHz
ModPhase           0.0
ModResol           10.000
Offset             0.0 %
PolyCof            {2;3,9;0} 0,1.00179,0,0,-0.00307086,0,0,-0.0265409,0,0,0.000297603,0,0,0.000223277,0,0,-4.53833e-06,0,0,-4.1451e-07,0,0,1.89417e-08,0,0,-1.48241e-09,0
QuadMode           False
ResCenter          61 mm
ResLength          23 mm
Resolution         1000
Resonator          1
SctNorm            True
SctRevision        Allegro
SpuExtension       False
SweepTime          16.00 s
TimeConst          5.12 ms
TimeExp            False
TuneCaps           0

.DVC     tempMonit, 1.0

AcqWaitTime        5.0 s
Temperature        295.00 K
TemperatureMon     230.57 K
Tolerance          1.00 K

*
************************************************************
