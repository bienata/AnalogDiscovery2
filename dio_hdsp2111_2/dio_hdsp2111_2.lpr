program dio_hdsp2111_2;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes,
    { you can add units after this }
    crt, sysutils, dwf;

const   MAX_SAMPLES = 100;


var
   hAd2 : HDWF;
   msgErr : TDwfString512;
   dwAllPins : dword;
   b, i : integer;
   status : DwfState;
   samplesBuff : array [0..MAX_SAMPLES-1] of double;
   T : double;
   txt : string;
   r : byte;
const
  rot : array [0..3] of string = ( '-', '\', '|', '/' );

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

  FDwfDigitalIOOutputEnableSet( hAd2, $FFFF ); // all OUT
  FDwfDigitalIOOutputSet( hAd2, WR_HI ); // !WR=1

  //AnalogIn, kanal 1
  FDwfAnalogInChannelEnableSet ( hAd2, 0, true );
  FDwfAnalogInChannelOffsetSet( hAd2, -1, 0.0 ); // offset 0mV
  FDwfAnalogInChannelRangeSet( hAd2, -1, 5.0 );   // zakres 5Vpp
  FDwfAnalogInFrequencySet( hAd2, 10E+3 ); // fs=10kHz
  FDwfAnalogInBufferSizeSet( hAd2, MAX_SAMPLES ); // buforek 255 sampli
  FDwfAnalogInTriggerAutoTimeoutSet( hAd2, 0.1 );     // autotrigger na ON
  FDwfAnalogInTriggerSourceSet( hAd2, trigsrcNone );

  repeat
     FDwfAnalogInConfigure ( hAd2, false, true );
     repeat
        FDwfAnalogInStatus( hAd2, true, @status );
        Delay (1);
     until status = stsDone;
     FDwfAnalogInStatusData( hAd2, 0, @samplesBuff, MAX_SAMPLES );
     T := 0.0;
     for i := 0 to 99 do T := T + samplesBuff[ i ];
     T := (T/MAX_SAMPLES - 0.500 (* dV MCP9700 *))/0.01 (* 10mV/C *);
     Hdsp2111Text ( LeftStr('za oknem ' + FormatFloat ('##.#', T) + '''C' + rot[r], 16) );
     delay (500);
     r := (r+1) and $3;
  until false;

  FDwfDigitalOutReset( hAd2 );
  FDwfDeviceClose( hAd2 );
end.

