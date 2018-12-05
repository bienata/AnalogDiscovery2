program psudemo1;

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
   boxTemperature : double;
   n : integer;
   v : double;
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  // read box temperature:  channel=2, node=2
  FDwfAnalogIOChannelNodeStatus ( 
    hAd2, 
    adAnalogIoChannel_USBMonitor, 
    adAnalogIoNode_USBMonitor_Temperature, 
    @boxTemperature 
  );
  writeln ( 'box temperature: ' + FormatFloat ( '##.##', boxTemperature ) + '`C' );

  //PSU for several seconds, at 5V
  // enable plus supply
  FDwfAnalogIOChannelNodeSet( 
    hAd2, 
    adAnalogIoChannel_PositiveSupply, 
    adAnalogIoNode_Supply_Enable, 
    double(true)
  );
  // all enable
  writeln ( 'power on' );
  FDwfAnalogIOEnableSet( hAd2, true );
  
  for n := 0 to 5 do 
  begin
    v := n * 1.0;
    FDwfAnalogIOChannelNodeSet( 
        hAd2, 
        adAnalogIoChannel_PositiveSupply, 
        adAnalogIoNode_Supply_Voltage, 
        v 
    );
    writeln ( 'positive voltage: ' + FormatFloat ( '00.00', v ) + 'V' );    
    Delay  ( 2000 );
  end;
  
  for n := 5 downto 0 do 
  begin
    v := n * 1.0;
    FDwfAnalogIOChannelNodeSet( 
        hAd2, 
        adAnalogIoChannel_PositiveSupply, 
        adAnalogIoNode_Supply_Voltage, 
        v 
    );
    writeln ( 'positive voltage: ' + FormatFloat ( '00.00', v ) + 'V' );    
    Delay  ( 2000 );
  end;
  
  // all disable
  writeln ( 'power off' );
  FDwfAnalogIOEnableSet( hAd2, false );

  FDwfDeviceClose( hAd2 );

end.

