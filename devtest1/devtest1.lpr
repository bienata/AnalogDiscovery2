program devtest1;

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
   msgVer, msgDevName, msgSN, msgUserName : TDwfString32;
   channelsNum, nodesNum, c, n : integer;
   channelName, nodeName : TDwfString32;
   channelLabel, nodeUnits : TDwfString16;
   boxTemperature : double;

begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  FDwfEnumDeviceName( 0, msgDevName);
  FDwfEnumSN( 0, msgSN );
  FDwfEnumUserName( 0, msgUserName );
  FDwfGetVersion ( msgVer );

  writeln ('Device Name:   ' + msgDevName );
  writeln ('Serial Number: ' + msgSN );
  writeln ('User Name:     ' + msgUserName );
  writeln ('API version:   ' + msgVer );

  // details
//  FDwfAnalogIOChannelNodeStatus
  FDwfAnalogIOChannelCount( hAd2, @channelsNum );
  for c := 0 to channelsNum-1 do
  begin
    FDwfAnalogIOChannelName ( hAd2, c, channelName, channelLabel );
    writeln ( 'channel: ' + IntTostr(c) + ' - ' + channelName + ' (' + channelLabel + ')' );
    FDwfAnalogIOChannelInfo( hAd2, c, @nodesNum );
    for n := 0 to nodesNum-1 do
    begin
       FDwfAnalogIOChannelNodeName (hAd2, c, n, nodeName, nodeUnits );
       writeln ( '   node: ' + IntTostr(n) + ' - ' + nodeName + ' [' + nodeUnits + ']' );
    end;
  end;

  // read box temperature:  channel=2, node=2
  FDwfAnalogIOChannelNodeStatus ( hAd2, 2, 2, @boxTemperature );
  writeln ( 'box temperature: ' + FormatFloat ( '##.##', boxTemperature ) + '`C' );

  //PSU for 2 second, at 5V
  // enable plus supply
  FDwfAnalogIOChannelNodeSet( hAd2, 0, 1, 5.0 );
  FDwfAnalogIOChannelNodeSet( hAd2, 0, 0, 1 );
  // enable minus supply
  FDwfAnalogIOChannelNodeSet( hAd2, 1, 1, -5.0 );
  FDwfAnalogIOChannelNodeSet( hAd2, 1, 0, 1 );
  // all enable
  writeln ( 'power on' );
  FDwfAnalogIOEnableSet( hAd2, true );
  Delay  ( 2000 );
  // all disable
  writeln ( 'power off' );
  FDwfAnalogIOEnableSet( hAd2, false );

  FDwfDeviceClose( hAd2 );

end.

