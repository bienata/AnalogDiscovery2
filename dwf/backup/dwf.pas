unit Dwf;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}
{$MACRO ON}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl }
  const dwf_lib = 'libdwf.so';
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE extdecl := stdcall }
  const dwf_lib = 'dwf.dll';
{$ENDIF}


type TDwfString512 = array [0..511] of AnsiChar;
type TDwfString32 =  array [0..31] of AnsiChar;
type TDwfString16 =  array [0..15] of AnsiChar;
type TDwfDoubleArr32 =  array [0..31] of double;

type PInteger = ^integer;
type PDouble = ^double;
type PSmallInt = ^smallint;
type BOOL = integer;
type PBOOL = ^integer;

// hardware device handle
type HDWF = integer;
type PHDWF = ^HDWF;
const hdwfNone : HDWF = 0;

// device enumeration filters
type ENUMFILTER = integer;
const enumfilterAll : ENUMFILTER = 0;
const enumfilterEExplorer : ENUMFILTER = 1;
const enumfilterDiscovery : ENUMFILTER = 2;

// device ID
type DEVID = integer;
type PDEVID = ^DEVID;
const devidEExplorer : DEVID = 1;
const  devidDiscovery : DEVID = 2;
const  devidDiscovery2 : DEVID = 3;
const  devidDDiscovery : DEVID = 4;

// device version
type DEVVER = integer;
type PDEVVER = ^DEVVER;
const devverEExplorerC : DEVVER = 2;
const devverEExplorerE : DEVVER    = 4;
const devverEExplorerF : DEVVER    = 5;
const devverDiscoveryA : DEVVER    = 1;
const devverDiscoveryB : DEVVER    = 2;
const devverDiscoveryC : DEVVER    = 3;

// trigger source
type TRIGSRC = byte;
type PTRIGSRC = ^TRIGSRC;
const trigsrcNone               : TRIGSRC = 0;
const trigsrcPC : TRIGSRC = 1;
const trigsrcDetectorAnalogIn : TRIGSRC    = 2;
const trigsrcDetectorDigitalIn : TRIGSRC   = 3;
const trigsrcAnalogIn : TRIGSRC            = 4;
const  trigsrcDigitalIn : TRIGSRC           = 5;
const  trigsrcDigitalOut : TRIGSRC          = 6;
const  trigsrcAnalogOut1 : TRIGSRC          = 7;
const  trigsrcAnalogOut2 : TRIGSRC          = 8;
const  trigsrcAnalogOut3 : TRIGSRC          = 9;
const  trigsrcAnalogOut4 : TRIGSRC          = 10;
const  trigsrcExternal1 : TRIGSRC           = 11;
const  trigsrcExternal2 : TRIGSRC           = 12;
const  trigsrcExternal3 : TRIGSRC           = 13;
const  trigsrcExternal4 : TRIGSRC           = 14;
const  trigsrcHigh : TRIGSRC                = 15;
const  trigsrcLow : TRIGSRC                 = 16;


// instrument states:
type DwfState = byte;
type PDwfState = ^DwfState;
const  DwfStateReady :DwfState       = 0;
const  DwfStateConfig :DwfState       = 4;
const  DwfStatePrefill :DwfState      = 5;
const  DwfStateArmed :DwfState        = 1;
const  DwfStateWait :DwfState         = 7;
const  DwfStateTriggered :DwfState    = 3;
const  DwfStateRunning :DwfState      = 3;
const  DwfStateDone :DwfState         = 2;

//
type DwfEnumConfigInfo = integer;
const  DECIAnalogInChannelCount : DwfEnumConfigInfo = 1;
const  DECIAnalogOutChannelCount : DwfEnumConfigInfo = 2;
const  DECIAnalogIOChannelCount : DwfEnumConfigInfo = 3;
const  DECIDigitalInChannelCount : DwfEnumConfigInfo = 4;
const  DECIDigitalOutChannelCount : DwfEnumConfigInfo = 5;
const  DECIDigitalIOChannelCount : DwfEnumConfigInfo = 6;
const  DECIAnalogInBufferSize : DwfEnumConfigInfo = 7;
const  DECIAnalogOutBufferSize : DwfEnumConfigInfo = 8;
const  DECIDigitalInBufferSize : DwfEnumConfigInfo = 9;
const  DECIDigitalOutBufferSize : DwfEnumConfigInfo = 10;

// acquisition modes:
type ACQMODE = integer;
type PACQMODE = ^ACQMODE;
const  acqmodeSingle : ACQMODE = 0;
const  acqmodeScanShift : ACQMODE   = 1;
const  acqmodeScanScreen  : ACQMODE = 2;
const  acqmodeRecord : ACQMODE      = 3;
const  acqmodeOvers : ACQMODE       = 4;
const  acqmodeSingle1 : ACQMODE     = 5;

// analog acquisition filter:
type FILTER = integer;
type PFILTER = ^FILTER;
const filterDecimate : FILTER  = 0;
const  filterAverage : FILTER  = 1;
const  filterMinMax : FILTER   = 2;

// analog acquisition filter:
type DwfTriggerSlope = integer;
type PDwfTriggerSlope = ^DwfTriggerSlope;
const  DwfTriggerSlopeRise : DwfTriggerSlope  = 0;
const  DwfTriggerSlopeFall : DwfTriggerSlope  = 1;
const  DwfTriggerSlopeEither : DwfTriggerSlope= 2;



// analog in trigger mode:
type TRIGTYPE = integer;
type PTRIGTYPE = ^TRIGTYPE;
const trigtypeEdge : TRIGTYPE = 0;
const trigtypePulse : TRIGTYPE        = 1;
const trigtypeTransition : TRIGTYPE   = 2;


// analog in trigger length condition
type TRIGLEN = integer;
type PTRIGLEN = ^TRIGLEN;
const triglenLess : TRIGLEN       = 0;
const triglenTimeout : TRIGLEN    = 1;
const triglenMore : TRIGLEN       = 2;

// error codes for DWF Public API:
type DWFERC = integer; 
type PDWFERC = ^DWFERC;                          
const    dwfercNoErc : DWFERC                  = 0;        //  No error occurred
const    dwfercUnknownError  : DWFERC          = 1;        //  API waiting on pending API timed out
const    dwfercApiLockTimeout  : DWFERC        = 2;        //  API waiting on pending API timed out
const    dwfercAlreadyOpened : DWFERC          = 3;        //  Device already opened
const    dwfercNotSupported : DWFERC           = 4;        //  Device not supported
const    dwfercInvalidParameter0 : DWFERC      = $10;     //  Invalid parameter sent in API call
const    dwfercInvalidParameter1 : DWFERC      = $11;     //  Invalid parameter sent in API call
const    dwfercInvalidParameter2 : DWFERC      = $12;     //  Invalid parameter sent in API call
const    dwfercInvalidParameter3 : DWFERC      = $13;     //  Invalid parameter sent in API call
const    dwfercInvalidParameter4 : DWFERC      = $14;     //  Invalid parameter sent in API call

// analog out signal types
type FUNC = byte;
type PFUNC = ^FUNC;
const  funcDC : FUNC       = 0;
const  funcSine : FUNC     = 1;
const  funcSquare : FUNC   = 2;
const  funcTriangle : FUNC = 3;
const  funcRampUp : FUNC   = 4;
const  funcRampDown : FUNC = 5;
const  funcNoise : FUNC    = 6;
const  funcPulse : FUNC    = 7;
const  funcTrapezium : FUNC= 8;
const  funcSinePower : FUNC= 9;
const  funcCustom : FUNC   = 30;
const  funcPlay : FUNC     = 31;

// analog io channel node types
type  ANALOGIO = byte;
type  PANALOGIO = ^ANALOGIO;
const  analogioEnable :ANALOGIO       = 1;
const  analogioVoltage:ANALOGIO      = 2;
const  analogioCurrent:ANALOGIO      = 3;
const  analogioPower:ANALOGIO        = 4;
const  analogioTemperature:ANALOGIO  = 5;

type AnalogOutNode = integer;
const  AnalogOutNodeCarrier:AnalogOutNode  = 0;
const  AnalogOutNodeFM:AnalogOutNode       = 1;
const  AnalogOutNodeAM:AnalogOutNode       = 2;

type  DwfAnalogOutMode = integer;
type  PDwfAnalogOutMode = ^DwfAnalogOutMode;
const  DwfAnalogOutModeVoltage : DwfAnalogOutMode = 0;
const DwfAnalogOutModeCurrent:DwfAnalogOutMode  = 1;

type DwfAnalogOutIdle = integer;
type PDwfAnalogOutIdle = ^DwfAnalogOutIdle;
const  DwfAnalogOutIdleDisable :DwfAnalogOutIdle = 0;
const DwfAnalogOutIdleOffset:DwfAnalogOutIdle   = 1;
const DwfAnalogOutIdleInitial :DwfAnalogOutIdle = 2;

type DwfDigitalInClockSource = integer;
const  DwfDigitalInClockSourceInternal : DwfDigitalInClockSource = 0;
const  DwfDigitalInClockSourceExternal : DwfDigitalInClockSource = 1;

type DwfDigitalInSampleMode = integer;
const DwfDigitalInSampleModeSimple : DwfDigitalInSampleMode   = 0;
// alternate samples: noise|sample|noise|sample|...  
// where noise is more than 1 transition between 2 samples
const DwfDigitalInSampleModeNoise :DwfDigitalInSampleMode     = 1; 

type DwfDigitalOutOutput = integer;
const DwfDigitalOutOutputPushPull : DwfDigitalOutOutput   = 0;
const  DwfDigitalOutOutputOpenDrain :DwfDigitalOutOutput  = 1;
const  DwfDigitalOutOutputOpenSource :DwfDigitalOutOutput = 2;
const  DwfDigitalOutOutputThreeState :DwfDigitalOutOutput = 3; // for custom and random

type DwfDigitalOutType = integer;
const  DwfDigitalOutTypePulse : DwfDigitalOutType     = 0;
const  DwfDigitalOutTypeCustom :DwfDigitalOutType     = 1;
const  DwfDigitalOutTypeRandom:DwfDigitalOutType     = 2;
const  DwfDigitalOutTypeROM:DwfDigitalOutType        = 3;
const  DwfDigitalOutTypeFSM:DwfDigitalOutType        = 3;

type DwfDigitalOutIdle = integer;
const  DwfDigitalOutIdleInit  : DwfDigitalOutIdle   = 0;
const  DwfDigitalOutIdleLow :DwfDigitalOutIdle     = 1;
const  DwfDigitalOutIdleHigh :DwfDigitalOutIdle    = 2;
const  DwfDigitalOutIdleZet :DwfDigitalOutIdle     = 3;


// Macro used to verify if bit is 1 or 0 in given bit field
//#define IsBitSet(fs; bit) ((fs & (1<<bit)) != 0)

// Error and version APIs:
function FDwfGetLastError( pdwferc : PDWFERC )   : boolean ; extdecl; external dwf_lib;
function FDwfGetLastErrorMsg( var szError : TDwfString512 )   : boolean ; extdecl; external dwf_lib;
// Returns DLL version; for instance: "2.4.3"
function FDwfGetVersion( var szVersion : TDwfString32 )  : boolean ; extdecl; external dwf_lib;



// DEVICE MANAGMENT FUNCTIONS
// Enumeration:
function FDwfEnum( enumfilter : ENUMFILTER; pcDevice : PInteger )    : boolean ; extdecl; external dwf_lib;


function FDwfEnumDeviceType(idxDevice : integer; pDeviceId : PDEVID; pDeviceRevision : PDEVVER)    : boolean ; extdecl; external dwf_lib;
function FDwfEnumDeviceIsOpened(idxDevice : integer; pfIsUsed : PBOOL)   : boolean ; extdecl; external dwf_lib;
function FDwfEnumUserName( idxDevice : integer; var szUserName : TDwfString32 )    : boolean ; extdecl; external dwf_lib;
function FDwfEnumDeviceName(idxDevice : integer; var szDeviceName : TDwfString32)    : boolean ; extdecl; external dwf_lib;
function FDwfEnumSN(idxDevice : integer; var szSN : TDwfString32)   : boolean ; extdecl; external dwf_lib;
function FDwfEnumConfig(idxDevice : integer; pcConfig : PInteger )   : boolean ; extdecl; external dwf_lib;
function FDwfEnumConfigInfo(idxConfig : integer;  info : DwfEnumConfigInfo; pv : PInteger)   : boolean ; extdecl; external dwf_lib;


// Open/Close:
function FDwfDeviceOpen(idxDevice : integer; phdwf : PHDWF)    : boolean ; extdecl; external dwf_lib;
function FDwfDeviceConfigOpen(idxDev: integer; idxCfg: integer; phdwf : PHDWF )   : boolean ; extdecl; external dwf_lib;
function FDwfDeviceClose(hdwf : HDWF )   : boolean ; extdecl; external dwf_lib;
function FDwfDeviceCloseAll    : boolean ; extdecl; external dwf_lib;
function FDwfDeviceAutoConfigureSet(hdwf : HDWF; fAutoConfigure : BOOL)   : boolean ; extdecl; external dwf_lib;
function FDwfDeviceAutoConfigureGet(hdwf : HDWF; pfAutoConfigure : PBOOL)   : boolean ; extdecl; external dwf_lib;
function FDwfDeviceReset(hdwf : HDWF)   : boolean ; extdecl; external dwf_lib;
function FDwfDeviceEnableSet(hdwf : HDWF; fEnable : BOOL)  : boolean ; extdecl; external dwf_lib;
function FDwfDeviceTriggerInfo(hdwf : HDWF; pfstrigsrc : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function FDwfDeviceTriggerSet(hdwf : HDWF; idxPin: integer;  trigsrc : TRIGSRC)   : boolean ; extdecl; external dwf_lib;

function FDwfDeviceTriggerGet(hdwf : HDWF; idxPin: integer;  ptrigsrc : PTRIGSRC )   : boolean ; extdecl; external dwf_lib;
function FDwfDeviceTriggerPC(hdwf : HDWF)  : boolean ; extdecl; external dwf_lib;
function FDwfDeviceTriggerSlopeInfo(hdwf : HDWF; pfsslope : PInteger)  : boolean ; extdecl; external dwf_lib;// use IsBitSet


// ANALOG IN INSTRUMENT FUNCTIONS
// Control and status: 
function FDwfAnalogInReset(hdwf : HDWF)   : boolean ; extdecl; external dwf_lib;
//function FDwfAnalogInConfigure(hdwf : HDWF; fReconfigure:BOOL;  fStart : BOOL)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInConfigure(hdwf : HDWF; fReconfigure: boolean;  fStart : boolean)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInTriggerForce(hdwf : HDWF)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatus(hdwf : HDWF;  fReadData:BOOL; psts : PDwfState)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusSamplesLeft(hdwf : HDWF; pcSamplesLeft : PInteger)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusSamplesValid(hdwf : HDWF; pcSamplesValid: PInteger)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusIndexWrite(hdwf : HDWF; pidxWrite: PInteger)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusAutoTriggered(hdwf : HDWF; pfAuto : PBOOL )   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusData(hdwf : HDWF; idxChannel : integer; rgdVoltData :PDouble; cdData : integer)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusData2(hdwf : HDWF; idxChannel : integer; rgdVoltData :PDouble; idxData : integer; cdData : integer)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusData16(hdwf : HDWF; idxChannel : integer; rgu16Data : PSmallInt; idxData : integer; cdData : integer)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusNoise(hdwf : HDWF; idxChannel : integer; rgdMin : PDouble; rgdMax :PDouble; cdData : integer)  : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusNoise2(hdwf : HDWF; idxChannel : integer; rgdMin : PDouble; rgdMax :PDouble; idxData : integer;  cdData : integer)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInStatusSample(hdwf : HDWF; idxChannel : integer; pdVoltSample :PDouble)   : boolean ; extdecl; external dwf_lib;

function FDwfAnalogInStatusRecord(hdwf : HDWF; pcdDataAvailable : PInteger; pcdDataLost : PInteger; pcdDataCorrupt : PInteger)  : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInRecordLengthSet(hdwf : HDWF;  sLegth : double)   : boolean ; extdecl; external dwf_lib;
function FDwfAnalogInRecordLengthGet(hdwf : HDWF; psLegth : PDouble)   : boolean ; extdecl; external dwf_lib;


// Acquisition configuration:
function   FDwfAnalogInFrequencyInfo(hdwf : HDWF ; phzMin :PDouble ; phzMax :PDouble)    : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInFrequencySet(hdwf : HDWF ; hzFrequency : double)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInFrequencyGet(hdwf : HDWF ; phzFrequency:PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInBitsInfo(hdwf : HDWF ; pnBits : PInteger)   : boolean ; extdecl; external dwf_lib;// Returns the number of ADC bits

function   FDwfAnalogInBufferSizeInfo(hdwf : HDWF ; pnSizeMin : PInteger; pnSizeMax : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInBufferSizeSet(hdwf : HDWF ; nSize : integer)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInBufferSizeGet(hdwf : HDWF ; pnSize : PInteger)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInNoiseSizeInfo(hdwf : HDWF ; pnSizeMax : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInNoiseSizeSet(hdwf : HDWF ; nSize : integer)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInNoiseSizeGet(hdwf : HDWF ; pnSize : PInteger)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInAcquisitionModeInfo(hdwf : HDWF ; pfsacqmode : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfAnalogInAcquisitionModeSet(hdwf : HDWF ;  acqmode : ACQMODE)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInAcquisitionModeGet(hdwf : HDWF ; pacqmode : PACQMODE)   : boolean ; extdecl; external dwf_lib;



// Channel configuration:
function   FDwfAnalogInChannelCount(hdwf : HDWF ; pcChannel : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelEnableSet(hdwf : HDWF ; idxChannel : integer;  fEnable:BOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelEnableGet(hdwf : HDWF ; idxChannel : integer; pfEnable : PBOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelFilterInfo(hdwf : HDWF ; pfsfilter : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfAnalogInChannelFilterSet(hdwf : HDWF ; idxChannel : integer; filter : FILTER)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelFilterGet(hdwf : HDWF ; idxChannel : integer; pfilter : PFILTER)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelRangeInfo(hdwf : HDWF ; pvoltsMin : PDouble; pvoltsMax : PDouble; pnSteps : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelRangeSteps(hdwf : HDWF ; var rgVoltsStep : TDwfDoubleArr32; pnSteps : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelRangeSet(hdwf : HDWF ; idxChannel : integer; voltsRange : double )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelRangeGet(hdwf : HDWF ; idxChannel : integer; pvoltsRange : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelOffsetInfo(hdwf : HDWF ; pvoltsMin : PDouble; pvoltsMax : PDouble; pnSteps : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelOffsetSet(hdwf : HDWF ; idxChannel : integer; voltOffset : double )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelOffsetGet(hdwf : HDWF ; idxChannel : integer; pvoltOffset : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelAttenuationSet(hdwf : HDWF ; idxChannel : integer; xAttenuation :double )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInChannelAttenuationGet(hdwf : HDWF ; idxChannel : integer; pxAttenuation : PDouble)   : boolean ; extdecl; external dwf_lib;



// Trigger configuration:
function   FDwfAnalogInTriggerSourceSet(hdwf : HDWF ;  trigsrc : TRIGSRC)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerSourceGet(hdwf : HDWF ; ptrigsrc : PTRIGSRC)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerPositionInfo(hdwf : HDWF ; psecMin : PDouble; psecMax : PDouble; pnSteps : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerPositionSet(hdwf : HDWF ;  secPosition : double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerPositionGet(hdwf : HDWF ; psecPosition : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerPositionStatus(hdwf : HDWF ; psecPosition : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerAutoTimeoutInfo(hdwf : HDWF ; psecMin : PDouble; psecMax : PDouble; pnSteps : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerAutoTimeoutSet(hdwf : HDWF ; secTimeout :double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerAutoTimeoutGet(hdwf : HDWF ; psecTimeout : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerHoldOffInfo(hdwf : HDWF ; psecMin : PDouble; psecMax : PDouble; pnStep : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerHoldOffSet(hdwf : HDWF ;  secHoldOff :double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerHoldOffGet(hdwf : HDWF ; psecHoldOff : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerTypeInfo(hdwf : HDWF ; pfstrigtype : PInteger )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerTypeSet(hdwf : HDWF ;  trigtype : TRIGTYPE)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerTypeGet(hdwf : HDWF ; ptrigtype : PTRIGTYPE)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerChannelInfo(hdwf : HDWF ; pidxMin : PInteger; pidxMax : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerChannelSet(hdwf : HDWF ; idxChannel : integer)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerChannelGet(hdwf : HDWF ; pidxChannel : PInteger)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerFilterInfo(hdwf : HDWF ; pfsfilter : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfAnalogInTriggerFilterSet(hdwf : HDWF ;  filter : FILTER)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerFilterGet(hdwf : HDWF ; pfilter : PFILTER)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerLevelInfo(hdwf : HDWF ; pvoltsMin : PDouble; pvoltsMax : PDouble; pnSteps : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerLevelSet(hdwf : HDWF ;  voltsLevel : double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerLevelGet(hdwf : HDWF ; pvoltsLevel : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerHysteresisInfo(hdwf : HDWF ; pvoltsMin : PDouble; pvoltsMax : PDouble; pnSteps : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerHysteresisSet(hdwf : HDWF ;  voltsLevel :double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerHysteresisGet(hdwf : HDWF ; pvoltsHysteresis : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerConditionInfo(hdwf : HDWF ; pfstrigcond : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfAnalogInTriggerConditionSet(hdwf : HDWF ;  trigcond : DwfTriggerSlope)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerConditionGet(hdwf : HDWF ; ptrigcond : PDwfTriggerSlope)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerLengthInfo(hdwf : HDWF ; psecMin : PDouble; psecMax : PDouble; pnSteps : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerLengthSet(hdwf : HDWF ; secLength : double )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerLengthGet(hdwf : HDWF ; psecLength : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInTriggerLengthConditionInfo(hdwf : HDWF ; pfstriglen : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerLengthConditionSet(hdwf : HDWF ; triglen : TRIGLEN)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInTriggerLengthConditionGet(hdwf : HDWF ; ptriglen : PTRIGLEN)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInSamplingSourceSet(hdwf : HDWF ; trigsrc : TRIGSRC)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInSamplingSourceGet(hdwf : HDWF ; ptrigsrc : PTRIGSRC )   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInSamplingSlopeSet(hdwf : HDWF ;  slope : DwfTriggerSlope)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInSamplingSlopeGet(hdwf : HDWF ; pslope : PDwfTriggerSlope)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogInSamplingDelaySet(hdwf : HDWF ; sec : double )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogInSamplingDelayGet(hdwf : HDWF ; psec : PDouble)   : boolean ; extdecl; external dwf_lib;




// ANALOG OUT INSTRUMENT FUNCTIONS
// Configuration:
function   FDwfAnalogOutCount(hdwf : HDWF ; pcChannel : PInteger)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutMasterSet(hdwf : HDWF ; idxChannel : integer;  idxMaster : integer)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutMasterGet(hdwf : HDWF ;  idxChannel : integer; pidxMaster : PInteger)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutTriggerSourceSet(hdwf : HDWF ;  idxChannel : integer;  trigsrc : TRIGSRC)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutTriggerSourceGet(hdwf : HDWF ;  idxChannel : integer; ptrigsrc : PTRIGSRC)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutTriggerSlopeSet(hdwf : HDWF ;  idxChannel : integer;  slope : DwfTriggerSlope )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutTriggerSlopeGet(hdwf : HDWF ;  idxChannel : integer; pslope : PDwfTriggerSlope)  : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutRunInfo(hdwf : HDWF ;  idxChannel : integer; psecMin : PDouble; psecMax  : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutRunSet(hdwf : HDWF ;  idxChannel : integer;  secRun : double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutRunGet(hdwf : HDWF ;  idxChannel : integer; psecRun : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutRunStatus(hdwf : HDWF ;  idxChannel : integer; psecRun : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutWaitInfo(hdwf : HDWF ;  idxChannel : integer; psecMin : PDouble; psecMax : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutWaitSet(hdwf : HDWF ;  idxChannel : integer;  secWait : double )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutWaitGet(hdwf : HDWF ;  idxChannel : integer; psecWait : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutRepeatInfo(hdwf : HDWF ; idxChannel : integer; pnMin : PInteger ; pnMax : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutRepeatSet(hdwf : HDWF ; idxChannel : integer; cRepeat : integer)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutRepeatGet(hdwf : HDWF ;  idxChannel : integer; pcRepeat : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutRepeatStatus(hdwf : HDWF ;  idxChannel : integer; pcRepeat: PInteger)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutRepeatTriggerSet(hdwf : HDWF ; idxChannel : integer;  fRepeatTrigger : BOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutRepeatTriggerGet(hdwf : HDWF ; idxChannel : integer; pfRepeatTrigger : PBOOL )   : boolean ; extdecl; external dwf_lib;



// EExplorer channel 3&4 current/voltage limitation
function   FDwfAnalogOutLimitationInfo(hdwf : HDWF ; idxChannel : integer; pMin : PDouble; pMax : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutLimitationSet(hdwf : HDWF ; idxChannel : integer; limit : double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutLimitationGet(hdwf : HDWF ;  idxChannel : integer; plimit : PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutModeSet(hdwf : HDWF ;  idxChannel : integer;  mode : DwfAnalogOutMode)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutModeGet(hdwf : HDWF ;  idxChannel : integer; pmode : PDwfAnalogOutMode)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutIdleInfo(hdwf : HDWF ;  idxChannel : integer; pfsidle : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutIdleSet(hdwf : HDWF ;  idxChannel : integer;  idle : DwfAnalogOutIdle)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutIdleGet(hdwf : HDWF ;  idxChannel : integer; pidle : PDwfAnalogOutIdle)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutNodeInfo(hdwf : HDWF ; idxChannel : integer; pfsnode : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet

function   FDwfAnalogOutNodeEnableSet(hdwf : HDWF ;  idxChannel : integer;  node : AnalogOutNode;  fEnable : BOOL )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeEnableGet(hdwf : HDWF ;  idxChannel : integer; node : AnalogOutNode ; pfEnable : PBOOL )   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutNodeFunctionInfo(hdwf : HDWF ;  idxChannel : integer;  node : AnalogOutNode; pfsfunc : PInteger) : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfAnalogOutNodeFunctionSet(hdwf : HDWF ;  idxChannel : integer;  node : AnalogOutNode;  func : FUNC)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeFunctionGet(hdwf : HDWF ;  idxChannel : integer;  node : AnalogOutNode; pfunc : PFUNC )   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutNodeFrequencyInfo(hdwf : HDWF ;  idxChannel : integer;  node : AnalogOutNode; phzMin :PDouble ; phzMax : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeFrequencySet(hdwf : HDWF ;  idxChannel : integer;  node :AnalogOutNode; hzFrequency  : double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeFrequencyGet(hdwf : HDWF ;  idxChannel : integer;  node:AnalogOutNode; phzFrequency : PDouble)   : boolean ; extdecl; external dwf_lib;


// Carrier Amplitude or Modulation Index 
function   FDwfAnalogOutNodeAmplitudeInfo(hdwf : HDWF ; idxChannel :  integer;  node:AnalogOutNode; pMin:PDouble; pMax:PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeAmplitudeSet(hdwf : HDWF ; idxChannel :  integer;  node:AnalogOutNode;  vAmplitude : double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeAmplitudeGet(hdwf : HDWF ; idxChannel :  integer;  node:AnalogOutNode; pvAmplitude:PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutNodeOffsetInfo(hdwf : HDWF ; idxChannel :  integer;  node:AnalogOutNode; pMin:PDouble; pMax:PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeOffsetSet(hdwf : HDWF ; idxChannel :  integer; node:AnalogOutNode;  vOffset :double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeOffsetGet(hdwf : HDWF ; idxChannel :  integer; node:AnalogOutNode; pvOffset:PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutNodeSymmetryInfo(hdwf : HDWF ; idxChannel :  integer;  node:AnalogOutNode; ppercentageMin:PDouble; ppercentageMax:PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeSymmetrySet(hdwf : HDWF ; idxChannel :  integer; node:AnalogOutNode;  percentageSymmetry : double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeSymmetryGet(hdwf : HDWF ; idxChannel : integer; node:AnalogOutNode ; ppercentageSymmetry:PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutNodePhaseInfo(hdwf : HDWF ; idxChannel :  integer; node:AnalogOutNode ; pdegreeMin:PDouble; pdegreeMax:PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodePhaseSet(hdwf : HDWF ; idxChannel :  integer;  node:AnalogOutNode ;  degreePhase :double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodePhaseGet(hdwf : HDWF ; idxChannel :  integer; node:AnalogOutNode ; pdegreePhase:PDouble)   : boolean ; extdecl; external dwf_lib;

function   FDwfAnalogOutNodeDataInfo(hdwf : HDWF ; idxChannel :  integer;  node:AnalogOutNode; pnSamplesMin : PInteger; pnSamplesMax : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodeDataSet(hdwf : HDWF ; idxChannel :  integer; node:AnalogOutNode ; rgdData:PDouble; cdData : integer)   : boolean ; extdecl; external dwf_lib;



// needed for EExplorer; not used for ADiscovery
function   FDwfAnalogOutCustomAMFMEnableSet(hdwf : HDWF ; idxChannel : integer;  fEnable : BOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutCustomAMFMEnableGet(hdwf : HDWF ; idxChannel : integer; pfEnable : PBOOL)   : boolean ; extdecl; external dwf_lib;

// Control:
function   FDwfAnalogOutReset(hdwf : HDWF ; idxChannel : integer)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutConfigure(hdwf : HDWF ; idxChannel : integer; fStart : BOOL )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutStatus(hdwf : HDWF ; idxChannel : integer; psts : PDwfState )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodePlayStatus(hdwf : HDWF ; idxChannel : integer;  node : AnalogOutNode; cdDataFree:PInteger; cdDataLost:PInteger; cdDataCorrupted:PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutNodePlayData(hdwf : HDWF ; idxChannel : integer;  node :AnalogOutNode; rgdData: PDouble; cdData:integer)   : boolean ; extdecl; external dwf_lib;



// ANALOG IO INSTRUMENT FUNCTIONS
// Control:
function   FDwfAnalogIOReset(hdwf : HDWF )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOConfigure(hdwf : HDWF )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOStatus(hdwf : HDWF )   : boolean ; extdecl; external dwf_lib;

// Configure:
function   FDwfAnalogIOEnableInfo(hdwf : HDWF ; pfSet : PBOOL; pfStatus : PBOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOEnableSet(hdwf : HDWF ;  fMasterEnable : BOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOEnableGet(hdwf : HDWF ; pfMasterEnable : PBOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOEnableStatus(hdwf : HDWF ; pfMasterEnable : PBOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelCount(hdwf : HDWF ; pnChannel: PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelName(hdwf : HDWF ; idxChannel : integer; var szName : TDwfString32; var szLabel: TDwfString16 )   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelInfo(hdwf : HDWF ; idxChannel : integer; pnNodes : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelNodeName(hdwf : HDWF ; idxChannel : integer; idxNode:integer; var szNodeName:TDwfString32; var szNodeUnits:TDwfString32)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelNodeInfo(hdwf : HDWF ; idxChannel : integer; idxNode:integer; panalogio : PANALOGIO)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelNodeSetInfo(hdwf : HDWF ; idxChannel : integer; idxNode:integer; pmin:PDouble; pmax:PDouble; pnSteps:PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelNodeSet(hdwf : HDWF ; idxChannel : integer; idxNode:integer; value: double)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelNodeGet(hdwf : HDWF ; idxChannel : integer; idxNode:integer; pvalue : PDouble)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelNodeStatusInfo(hdwf : HDWF ; idxChannel : integer; idxNode:integer; pmin: PDouble; pmax: PDouble; pnSteps:PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogIOChannelNodeStatus(hdwf : HDWF ; idxChannel : integer; idxNode:integer; pvalue: PDouble)   : boolean ; extdecl; external dwf_lib;


// DIGITAL IO INSTRUMENT FUNCTIONS
// Control:
function   FDwfDigitalIOReset(hdwf : HDWF )   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOConfigure(hdwf : HDWF )   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOStatus(hdwf : HDWF )   : boolean ; extdecl; external dwf_lib;

type PDWORD = ^DWord;
type PQWORD = ^UInt64;
// Configure:
function   FDwfDigitalIOOutputEnableInfo(hdwf : HDWF ; pfsOutputEnableMask : PDWORD )   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputEnableSet(hdwf : HDWF ; fsOutputEnable : DWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputEnableGet(hdwf : HDWF ; pfsOutputEnable :PDWORD )   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputInfo(hdwf : HDWF ; pfsOutputMask : PDWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputSet(hdwf : HDWF ; fsOutput : DWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputGet(hdwf : HDWF ; pfsOutput : PDWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOInputInfo(hdwf : HDWF ; pfsInputMask: PDWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOInputStatus(hdwf : HDWF ; pfsInput : PDWORD)   : boolean ; extdecl; external dwf_lib;

function   FDwfDigitalIOOutputEnableInfo64(hdwf : HDWF ; pfsOutputEnableMask :PQWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputEnableSet64(hdwf : HDWF ; fsOutputEnable:QWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputEnableGet64(hdwf : HDWF ; pfsOutputEnable:PQWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputInfo64(hdwf : HDWF ; pfsOutputMask:PQWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputSet64(hdwf : HDWF ; fsOutput:QWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOOutputGet64(hdwf : HDWF ; pfsOutput:PQWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOInputInfo64(hdwf : HDWF ; pfsInputMask:PQWORD)   : boolean ; extdecl; external dwf_lib;
function   FDwfDigitalIOInputStatus64(hdwf : HDWF ; pfsInput:PQWORD)   : boolean ; extdecl; external dwf_lib;
  (*

// DIGITAL IN INSTRUMENT FUNCTIONS
// Control and status: 
function   FDwfDigitalInReset(hdwf : HDWF );
function   FDwfDigitalInConfigure(hdwf : HDWF ; BOOL fReconfigure; BOOL fStart);
function   FDwfDigitalInStatus(hdwf : HDWF ; BOOL fReadData; DwfState *psts);
function   FDwfDigitalInStatusSamplesLeft(hdwf : HDWF ; int *pcSamplesLeft);
function   FDwfDigitalInStatusSamplesValid(hdwf : HDWF ; int *pcSamplesValid);
function   FDwfDigitalInStatusIndexWrite(hdwf : HDWF ; int *pidxWrite);
function   FDwfDigitalInStatusAutoTriggered(hdwf : HDWF ; BOOL *pfAuto);
function   FDwfDigitalInStatusData(hdwf : HDWF ; void *rgData; int countOfDataBytes);
function   FDwfDigitalInStatusData2(hdwf : HDWF ; void *rgData; int idxSample; int countOfDataBytes);
function   FDwfDigitalInStatusNoise2(hdwf : HDWF ; void *rgData; int idxSample; int countOfDataBytes);
function   FDwfDigitalInStatusRecord(hdwf : HDWF ; int *pcdDataAvailable; int *pcdDataLost; int *pcdDataCorrupt);

// Acquisition configuration:
function   FDwfDigitalInInternalClockInfo(hdwf : HDWF ; double *phzFreq);

function   FDwfDigitalInClockSourceInfo(hdwf : HDWF ; int *pfsDwfDigitalInClockSource); // use IsBitSet
function   FDwfDigitalInClockSourceSet(hdwf : HDWF ; DwfDigitalInClockSource v);
function   FDwfDigitalInClockSourceGet(hdwf : HDWF ; DwfDigitalInClockSource *pv);

function   FDwfDigitalInDividerInfo(hdwf : HDWF ; unsigned int *pdivMax);
function   FDwfDigitalInDividerSet(hdwf : HDWF ; unsigned int div);
function   FDwfDigitalInDividerGet(hdwf : HDWF ; unsigned int *pdiv);

function   FDwfDigitalInBitsInfo(hdwf : HDWF ; int *pnBits); // Returns the number of Digital In bits
function   FDwfDigitalInSampleFormatSet(hdwf : HDWF ; int nBits);  // valid options 8/16/32
function   FDwfDigitalInSampleFormatGet(hdwf : HDWF ; int *pnBits);

function   FDwfDigitalInInputOrderSet(hdwf : HDWF ; bool fDioFirst); // for Digital Discovery

function   FDwfDigitalInBufferSizeInfo(hdwf : HDWF ; int *pnSizeMax);
function   FDwfDigitalInBufferSizeSet(hdwf : HDWF ; int nSize);
function   FDwfDigitalInBufferSizeGet(hdwf : HDWF ; int *pnSize);

function   FDwfDigitalInSampleModeInfo(hdwf : HDWF ; int *pfsDwfDigitalInSampleMode); // use IsBitSet
function   FDwfDigitalInSampleModeSet(hdwf : HDWF ; DwfDigitalInSampleMode v);  //
function   FDwfDigitalInSampleModeGet(hdwf : HDWF ; DwfDigitalInSampleMode *pv);

function   FDwfDigitalInSampleSensibleSet(hdwf : HDWF ; unsigned int fs);
function   FDwfDigitalInSampleSensibleGet(hdwf : HDWF ; unsigned int *pfs);

function   FDwfDigitalInAcquisitionModeInfo(hdwf : HDWF ; int *pfsacqmode); // use IsBitSet
function   FDwfDigitalInAcquisitionModeSet(hdwf : HDWF ; ACQMODE acqmode);
function   FDwfDigitalInAcquisitionModeGet(hdwf : HDWF ; ACQMODE *pacqmode);

// Trigger configuration:
function   FDwfDigitalInTriggerSourceSet(hdwf : HDWF ; TRIGSRC trigsrc);
function   FDwfDigitalInTriggerSourceGet(hdwf : HDWF ; TRIGSRC *ptrigsrc);

function   FDwfDigitalInTriggerSlopeSet(hdwf : HDWF ; DwfTriggerSlope slope);
function   FDwfDigitalInTriggerSlopeGet(hdwf : HDWF ; DwfTriggerSlope *pslope);

function   FDwfDigitalInTriggerPositionInfo(hdwf : HDWF ; unsigned int *pnSamplesAfterTriggerMax);
function   FDwfDigitalInTriggerPositionSet(hdwf : HDWF ; unsigned int cSamplesAfterTrigger);
function   FDwfDigitalInTriggerPositionGet(hdwf : HDWF ; unsigned int *pcSamplesAfterTrigger);

function   FDwfDigitalInTriggerPrefillSet(hdwf : HDWF ; unsigned int cSamplesBeforeTrigger);
function   FDwfDigitalInTriggerPrefillGet(hdwf : HDWF ; unsigned int *pcSamplesBeforeTrigger);

function   FDwfDigitalInTriggerAutoTimeoutInfo(hdwf : HDWF ; double *psecMin; double *psecMax; double *pnSteps);
function   FDwfDigitalInTriggerAutoTimeoutSet(hdwf : HDWF ; double secTimeout);
function   FDwfDigitalInTriggerAutoTimeoutGet(hdwf : HDWF ; double *psecTimeout);

function   FDwfDigitalInTriggerInfo(hdwf : HDWF ; unsigned int *pfsLevelLow; unsigned int *pfsLevelHigh; unsigned int *pfsEdgeRise; unsigned int *pfsEdgeFall);
function   FDwfDigitalInTriggerSet(hdwf : HDWF ; unsigned int fsLevelLow; unsigned int fsLevelHigh; unsigned int fsEdgeRise; unsigned int fsEdgeFall);
function   FDwfDigitalInTriggerGet(hdwf : HDWF ; unsigned int *pfsLevelLow; unsigned int *pfsLevelHigh; unsigned int *pfsEdgeRise; unsigned int *pfsEdgeFall);
// the logic for trigger bits: Low and High and (Rise or Fall)
// bits set in Rise and Fall means any edge

function   FDwfDigitalInTriggerResetSet(hdwf : HDWF ; unsigned int fsLevelLow; unsigned int fsLevelHigh; unsigned int fsEdgeRise; unsigned int fsEdgeFall);
function   FDwfDigitalInTriggerCountSet(hdwf : HDWF ; int cCount; int fRestart);
function   FDwfDigitalInTriggerLengthSet(hdwf : HDWF ; double secMin; double secMax; int idxSync);
function   FDwfDigitalInTriggerMatchSet(hdwf : HDWF ; int iPin; unsigned int fsMask; unsigned int fsValue; int cBitStuffing);


// DIGITAL OUT INSTRUMENT FUNCTIONS
// Control:
function   FDwfDigitalOutReset(hdwf : HDWF );
function   FDwfDigitalOutConfigure(hdwf : HDWF ; BOOL fStart);
function   FDwfDigitalOutStatus(hdwf : HDWF ; DwfState *psts);

// Configuration:
function   FDwfDigitalOutInternalClockInfo(hdwf : HDWF ; double *phzFreq);

function   FDwfDigitalOutTriggerSourceSet(hdwf : HDWF ; TRIGSRC trigsrc);
function   FDwfDigitalOutTriggerSourceGet(hdwf : HDWF ; TRIGSRC *ptrigsrc);

function   FDwfDigitalOutRunInfo(hdwf : HDWF ; double *psecMin; double *psecMax);
function   FDwfDigitalOutRunSet(hdwf : HDWF ; double secRun);
function   FDwfDigitalOutRunGet(hdwf : HDWF ; double *psecRun);
function   FDwfDigitalOutRunStatus(hdwf : HDWF ; double *psecRun);

function   FDwfDigitalOutWaitInfo(hdwf : HDWF ; double *psecMin; double *psecMax);
function   FDwfDigitalOutWaitSet(hdwf : HDWF ; double secWait);
function   FDwfDigitalOutWaitGet(hdwf : HDWF ; double *psecWait);

function   FDwfDigitalOutRepeatInfo(hdwf : HDWF ; unsigned int *pnMin; unsigned int *pnMax);
function   FDwfDigitalOutRepeatSet(hdwf : HDWF ; unsigned int cRepeat);
function   FDwfDigitalOutRepeatGet(hdwf : HDWF ; unsigned int *pcRepeat);
function   FDwfDigitalOutRepeatStatus(hdwf : HDWF ; unsigned int *pcRepeat);

function   FDwfDigitalOutTriggerSlopeSet(hdwf : HDWF ; DwfTriggerSlope slope);
function   FDwfDigitalOutTriggerSlopeGet(hdwf : HDWF ; DwfTriggerSlope *pslope);

function   FDwfDigitalOutRepeatTriggerSet(hdwf : HDWF ; BOOL fRepeatTrigger);
function   FDwfDigitalOutRepeatTriggerGet(hdwf : HDWF ; BOOL *pfRepeatTrigger);

function   FDwfDigitalOutCount(hdwf : HDWF ; int *pcChannel);
function   FDwfDigitalOutEnableSet(hdwf : HDWF ; idxChannel : integer; BOOL fEnable);
function   FDwfDigitalOutEnableGet(hdwf : HDWF ; idxChannel : integer; BOOL *pfEnable);

function   FDwfDigitalOutOutputInfo(hdwf : HDWF ; idxChannel : integer; int *pfsDwfDigitalOutOutput); // use IsBitSet
function   FDwfDigitalOutOutputSet(hdwf : HDWF ; idxChannel : integer; DwfDigitalOutOutput v);
function   FDwfDigitalOutOutputGet(hdwf : HDWF ; idxChannel : integer; DwfDigitalOutOutput *pv);

function   FDwfDigitalOutTypeInfo(hdwf : HDWF ; idxChannel : integer; int *pfsDwfDigitalOutType); // use IsBitSet
function   FDwfDigitalOutTypeSet(hdwf : HDWF ; idxChannel : integer; DwfDigitalOutType v);
function   FDwfDigitalOutTypeGet(hdwf : HDWF ; idxChannel : integer; DwfDigitalOutType *pv);

function   FDwfDigitalOutIdleInfo(hdwf : HDWF ; idxChannel : integer; int *pfsDwfDigitalOutIdle); // use IsBitSet
function   FDwfDigitalOutIdleSet(hdwf : HDWF ; idxChannel : integer; DwfDigitalOutIdle v);
function   FDwfDigitalOutIdleGet(hdwf : HDWF ; idxChannel : integer; DwfDigitalOutIdle *pv);

function   FDwfDigitalOutDividerInfo(hdwf : HDWF ; idxChannel : integer; unsigned int *vMin; unsigned int *vMax);
function   FDwfDigitalOutDividerInitSet(hdwf : HDWF ; idxChannel : integer; unsigned int v);
function   FDwfDigitalOutDividerInitGet(hdwf : HDWF ; idxChannel : integer; unsigned int *pv);
function   FDwfDigitalOutDividerSet(hdwf : HDWF ; idxChannel : integer; unsigned int v);
function   FDwfDigitalOutDividerGet(hdwf : HDWF ; idxChannel : integer; unsigned int *pv);

function   FDwfDigitalOutCounterInfo(hdwf : HDWF ; idxChannel : integer; unsigned int *vMin; unsigned int *vMax);
function   FDwfDigitalOutCounterInitSet(hdwf : HDWF ; idxChannel : integer; BOOL fHigh; unsigned int v);
function   FDwfDigitalOutCounterInitGet(hdwf : HDWF ; idxChannel : integer; int *pfHigh; unsigned int *pv);
function   FDwfDigitalOutCounterSet(hdwf : HDWF ; idxChannel : integer; unsigned int vLow; unsigned int vHigh);
function   FDwfDigitalOutCounterGet(hdwf : HDWF ; idxChannel : integer; unsigned int *pvLow; unsigned int *pvHigh);


function   FDwfDigitalOutDataInfo(hdwf : HDWF ; idxChannel : integer; unsigned int *pcountOfBitsMax);
function   FDwfDigitalOutDataSet(hdwf : HDWF ; idxChannel : integer; void *rgBits; unsigned int countOfBits);
// bits order is lsb first
// for TS output the count of bits its the total number of IO|OE bits; it should be an even number
// BYTE:   0                 |1     ...
// bit:    0 |1 |2 |3 |...|7 |0 |1 |...
// sample: IO|OE|IO|OE|...|OE|IO|OE|...


function   FDwfDigitalUartReset(hdwf : HDWF );
function   FDwfDigitalUartRateSet(hdwf : HDWF ; double hz);
function   FDwfDigitalUartBitsSet(hdwf : HDWF ; int cBits);
function   FDwfDigitalUartParitySet(hdwf : HDWF ; int parity); // 0 none; 1 odd; 2 even
function   FDwfDigitalUartStopSet(hdwf : HDWF ; double cBit);
function   FDwfDigitalUartTxSet(hdwf : HDWF ; idxChannel : integer);
function   FDwfDigitalUartRxSet(hdwf : HDWF ; idxChannel : integer);

function   FDwfDigitalUartTx(hdwf : HDWF ; char *szTx; int cTx);
function   FDwfDigitalUartRx(hdwf : HDWF ; char *szRx; int cRx; int *pcRx; int *pParity);

function   FDwfDigitalSpiReset(hdwf : HDWF );
function   FDwfDigitalSpiFrequencySet(hdwf : HDWF ; double hz);
function   FDwfDigitalSpiClockSet(hdwf : HDWF ; idxChannel : integer);
function   FDwfDigitalSpiDataSet(hdwf : HDWF ; int idxDQ; idxChannel : integer); // 0 DQ0_MOSI_SISO; 1 DQ1_MISO; 2 DQ2; 3 DQ3
function   FDwfDigitalSpiModeSet(hdwf : HDWF ; int iMode); // bit1 CPOL; bit0 CPHA
function   FDwfDigitalSpiOrderSet(hdwf : HDWF ; int fMSBLSB); // bit order: 0 MSB first; 1 LSB first

function   FDwfDigitalSpiSelect(hdwf : HDWF ; idxChannel : integer; int level); // 0 low; 1 high; -1 Z
// cDQ 0 SISO; 1 MOSI/MISO; 2 dual; 4 quad; // 1-32 bits / word
function   FDwfDigitalSpiWriteRead(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned char *rgTX; int cTX; unsigned char *rgRX; int cRX);
function   FDwfDigitalSpiWriteRead16(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned short *rgTX; int cTX; unsigned short *rgRX; int cRX);
function   FDwfDigitalSpiWriteRead32(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned int *rgTX; int cTX; unsigned int *rgRX; int cRX);
function   FDwfDigitalSpiRead(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned char *rgRX; int cRX);
function   FDwfDigitalSpiReadOne(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned int *pRX);
function   FDwfDigitalSpiRead16(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned short *rgRX; int cRX);
function   FDwfDigitalSpiRead32(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned int *rgRX; int cRX);
function   FDwfDigitalSpiWrite(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned char *rgTX; int cTX);
function   FDwfDigitalSpiWriteOne(hdwf : HDWF ; int cDQ; int cBits; unsigned int vTX);
function   FDwfDigitalSpiWrite16(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned short *rgTX; int cTX);
function   FDwfDigitalSpiWrite32(hdwf : HDWF ; int cDQ; int cBitPerWord; unsigned int *rgTX; int cTX);

function   FDwfDigitalI2cReset(hdwf : HDWF );
function   FDwfDigitalI2cClear(hdwf : HDWF ; int *pfFree);
function   FDwfDigitalI2cRateSet(hdwf : HDWF ; double hz);
function   FDwfDigitalI2cReadNakSet(hdwf : HDWF ; int fNakLastReadByte);
function   FDwfDigitalI2cSclSet(hdwf : HDWF ; idxChannel : integer);
function   FDwfDigitalI2cSdaSet(hdwf : HDWF ; idxChannel : integer);

function   FDwfDigitalI2cWriteRead(hdwf : HDWF ; unsigned char adr8bits; unsigned char *rgbTx; int cTx; unsigned char *rgRx; int cRx; int *pNak);
function   FDwfDigitalI2cRead(hdwf : HDWF ; unsigned char adr8bits; unsigned char *rgbRx; int cRx; int *pNak);
function   FDwfDigitalI2cWrite(hdwf : HDWF ; unsigned char adr8bits; unsigned char *rgbTx; int cTx; int *pNak);
function   FDwfDigitalI2cWriteOne(hdwf : HDWF ; unsigned char adr8bits; unsigned char bTx; int *pNak);

function   FDwfDigitalCanReset(hdwf : HDWF );
function   FDwfDigitalCanRateSet(hdwf : HDWF ; double hz);
function   FDwfDigitalCanPolaritySet(hdwf : HDWF ; int fInvert); // 0 normal; 1 invert
function   FDwfDigitalCanTxSet(hdwf : HDWF ; idxChannel : integer);
function   FDwfDigitalCanRxSet(hdwf : HDWF ; idxChannel : integer);

function   FDwfDigitalCanTx(hdwf : HDWF ; int vID; int fExtended; int fRemote; int cDLC; unsigned char *rgTX);
function   FDwfDigitalCanRx(hdwf : HDWF ; int *pvID; int *pfExtended; int *pfRemote; int *pcDLC; unsigned char *rgRX; int cRX; int *pvStatus);


// OBSOLETE but supported; avoid using the following in new projects:

// use FDwfDigitalInTriggerSourceSet trigsrcAnalogIn
// call FDwfDigitalInConfigure before FDwfAnalogInConfigure
function   FDwfDigitalInMixedSet(hdwf : HDWF ; BOOL fEnable);
   *)
   
// use DwfTriggerSlope
type TRIGCOND = integer;
type PTRIGCOND = TRIGCOND;
const trigcondRisingPositive :TRIGCOND   = 0;
const trigcondFallingNegative : TRIGCOND = 1;

// use FDwfDeviceTriggerInfo(hdwf; ptrigsrcInfo);
function   FDwfAnalogInTriggerSourceInfo(hdwf : HDWF ; pfstrigsrc : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfAnalogOutTriggerSourceInfo(hdwf : HDWF ; idxChannel : integer; pfstrigsrc  : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfDigitalInTriggerSourceInfo(hdwf : HDWF ; pfstrigsrc : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet
function   FDwfDigitalOutTriggerSourceInfo(hdwf : HDWF ; pfstrigsrc : PInteger)   : boolean ; extdecl; external dwf_lib; // use IsBitSet

// use DwfState
type STS = byte;
type PSTS = ^STS;
const  stsRdy : STS        = 0;
const stsArm : STS        = 1;
const stsDone : STS      = 2;
const stsTrig : STS      = 3;
const stsCfg : STS       = 4;
const stsPrefill : STS   = 5;
const stsNotDone : STS   = 6;
const stsTrigDly : STS   = 7;
const stsError : STS     = 8;
const stsBusy : STS      = 9;
const  stsStop : STS      = 10;


// use FDwfAnalogOutNode*
function   FDwfAnalogOutEnableSet(hdwf : HDWF ; idxChannel : integer;  fEnable : BOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutEnableGet(hdwf : HDWF ; idxChannel : integer; pfEnable : PBOOL)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutFunctionInfo(hdwf : HDWF ; idxChannel : integer; pfsfunc : PInteger)   : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutFunctionSet(hdwf : HDWF ; idxChannel : integer;  func : FUNC)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutFunctionGet(hdwf : HDWF ; idxChannel : integer; pfunc : PFUNC )  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutFrequencyInfo(hdwf : HDWF ; idxChannel : integer; phzMin : PDouble; phzMax : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutFrequencySet(hdwf : HDWF ; idxChannel : integer;  hzFrequency : double )  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutFrequencyGet(hdwf : HDWF ; idxChannel : integer; phzFrequency : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutAmplitudeInfo(hdwf : HDWF ; idxChannel : integer; pvoltsMin : PDouble; pvoltsMax : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutAmplitudeSet(hdwf : HDWF ; idxChannel : integer;  voltsAmplitude : double)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutAmplitudeGet(hdwf : HDWF ; idxChannel : integer; pvoltsAmplitude : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutOffsetInfo(hdwf : HDWF ; idxChannel : integer; pvoltsMin : PDouble; pvoltsMax : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutOffsetSet(hdwf : HDWF ; idxChannel : integer; voltsOffset : double)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutOffsetGet(hdwf : HDWF ; idxChannel : integer; pvoltsOffset : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutSymmetryInfo(hdwf : HDWF ; idxChannel : integer; ppercentageMin : PDouble; ppercentageMax : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutSymmetrySet(hdwf : HDWF ; idxChannel : integer; percentageSymmetry : double)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutSymmetryGet(hdwf : HDWF ; idxChannel : integer; ppercentageSymmetry : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutPhaseInfo(hdwf : HDWF ; idxChannel : integer; pdegreeMin : PDouble; pdegreeMax : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutPhaseSet(hdwf : HDWF ; idxChannel : integer; degreePhase : double)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutPhaseGet(hdwf : HDWF ; idxChannel : integer; pdegreePhase  : PDouble)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutDataInfo(hdwf : HDWF ; idxChannel : integer; pnSamplesMin : PInteger; pnSamplesMax : PInteger)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutDataSet(hdwf : HDWF ; idxChannel : integer; rgdData : PDouble; cdData : integer)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutPlayStatus(hdwf : HDWF ; idxChannel : integer; cdDataFree : PInteger; cdDataLost : PInteger; cdDataCorrupted : PInteger)  : boolean ; extdecl; external dwf_lib;
function   FDwfAnalogOutPlayData(hdwf : HDWF ; idxChannel : integer; rgdData : PDouble; cdData : integer)  : boolean ; extdecl; external dwf_lib;

function   FDwfEnumAnalogInChannels(idxDevice : integer; pnChannels : PInteger)  : boolean ; extdecl; external dwf_lib;
function   FDwfEnumAnalogInBufferSize(idxDevice : integer; pnBufferSize : PInteger)  : boolean ; extdecl; external dwf_lib;
function   FDwfEnumAnalogInBits(idxDevice : integer; pnBits : PInteger)  : boolean ; extdecl; external dwf_lib;
function   FDwfEnumAnalogInFrequency(idxDevice : integer; phzFrequency : PDouble)  : boolean ; extdecl; external dwf_lib;

implementation

end.

