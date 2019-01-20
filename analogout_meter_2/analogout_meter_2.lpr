program analogout_meter_2;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes,
    { you can add units after this }
    crt, sysutils, dwf;

const   MAX_SAMPLES = 8192;


var
   hAd2 : HDWF;
   msgErr : TDwfString512;
   dwAllPins : dword;
   b, i : integer;
   status : DwfState;
   samplesBuff : array [0..MAX_SAMPLES-1] of double;
   U : double;
   txt : string;
const

WR_HI : dword = $8000;
WR_LO : dword = $0000;

SEL_DATA : dword = $1000;
SEL_CTRL : dword = $0000;

A3_HI : dword = $0800;
A3_LO : dword = $0000;

// co za wiocha...bosssshhhhe
procedure Hdsp2111CharAt( c : char; n : integer );
var
   addr_char : dword;
begin
  addr_char := dword( n shl 8 ) + ord(c);
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_DATA + addr_char );
  FDwfDigitalIOOutputSet( hAd2, WR_LO + SEL_DATA + addr_char );
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_DATA + addr_char );
end;

procedure Hdsp2111Text( s : string );
var i : integer;
begin
  for i := 1 to length(s) do
  begin
    hdsp2111CharAt( s[ i ], i-1 );
  end;
end;

procedure Hdsp2111Brightness( b : byte );
var
   brightness : byte;
begin
  brightness := ($7-b) and $07; // na wszelki wypadek
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_CTRL + A3_LO + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_LO + SEL_CTRL + A3_LO + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_CTRL + A3_HI + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_LO + SEL_CTRL + A3_HI + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_CTRL + A3_HI + brightness);
end;


begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;
  // channel 1, DC, 2.5V
  FDwfAnalogOutNodeEnableSet( hAd2, 0, AnalogOutNodeCarrier, true );
  FDwfAnalogOutNodeFunctionSet( hAd2, 0, AnalogOutNodeCarrier, funcDC );
  FDwfAnalogOutNodeAmplitudeSet( hAd2, 0, AnalogOutNodeCarrier, 0 );

  FDwfDigitalIOOutputEnableSet( hAd2, $FFFF ); // all OUT
  FDwfDigitalIOOutputSet( hAd2, WR_HI ); // !WR=1
  Hdsp2111Text ( '                ' );

  //AnalogIn, kanal 1
  FDwfAnalogInChannelEnableSet ( hAd2, 0, true );
  FDwfAnalogInChannelOffsetSet( hAd2, -1, 0.0 ); // offset 0mV
  FDwfAnalogInChannelRangeSet( hAd2, -1, 50.0 );   // zakres 50Vpp
  FDwfAnalogInFrequencySet( hAd2, 4E+6 ); // fs=4MHz
  FDwfAnalogInBufferSizeSet( hAd2, MAX_SAMPLES ); // buforek 8k sampli
  FDwfAnalogInTriggerAutoTimeoutSet( hAd2, 0.1 );     // autotrigger na ON
  FDwfAnalogInTriggerSourceSet( hAd2, trigsrcNone );

  repeat
     FDwfAnalogInConfigure ( hAd2, false, true );
     repeat
        FDwfAnalogInStatus( hAd2, true, @status );
        Delay (1);
     until status = stsDone;
     FDwfAnalogInStatusData( hAd2, 0, @samplesBuff, MAX_SAMPLES );
     // RMS
     U := 0.0;
     for i := 0 to MAX_SAMPLES-1 do U := U + samplesBuff[ i ] * samplesBuff[ i ] ;
     U := sqrt( U/MAX_SAMPLES );

     writeln ( FormatFloat ('00.00', U) ); // konsola
     Hdsp2111Text ( LeftStr('U: ' + RightStr( ' ' + FormatFloat ('00.00', U), 6)  + 'V RMS             ', 16) );
     // poslij na miernik
     FDwfAnalogOutNodeOffsetSet( hAd2, 0, AnalogOutNodeCarrier, U/2 );
     delay (500);
  until false;

  FDwfAnalogOutNodeOffsetSet( hAd2, 0, AnalogOutNodeCarrier, 0.0 );
  FDwfDigitalOutReset( hAd2 );
  FDwfDeviceClose( hAd2 );
end.

