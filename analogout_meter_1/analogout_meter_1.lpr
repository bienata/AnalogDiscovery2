program analogout_meter_1;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes,
    { you can add units after this }
    crt, sysutils, dwf;

var
   hAd2 : HDWF;
   msgErr : TDwfString512;
   i : integer;
   outV : double;

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
  writeln ( '-----' );
  for i := 0 to 20 do
  begin
    outV := 0.25 * i;
    FDwfAnalogOutNodeOffsetSet( hAd2, 0, AnalogOutNodeCarrier, outV );
    writeln ( FormatFloat('00.00', outV*2 ) );
    delay( 500 );
  end;
  for i := 20 downto 0 do
  begin
    outV := 0.25 * i;
    FDwfAnalogOutNodeOffsetSet( hAd2, 0, AnalogOutNodeCarrier, outV );
    writeln ( FormatFloat('00.00', outV*2 ) );
    delay( 500 );
  end;

  FDwfDigitalOutReset( hAd2 );
  FDwfDeviceClose( hAd2 );
end.

