program i2c_24c512_1;

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
   i, nak, x : integer;
   data : byte;
   dataBuff : array [0..$ff] of byte;
   address : array [0..1] of byte;
   done : boolean;
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  FDwfDigitalI2cRateSet( hAd2, 1E4 );  // takt 100kHz, bo dużo czytania
  FDwfDigitalI2cSclSet( hAd2, 0 );     // SCL - DIO_0
  FDwfDigitalI2cSdaSet( hAd2, 1);      // SDA = DIO_1

  FDwfDigitalI2cClear( hAd2, @nak );
  if nak = 0 then
  begin
       writeln( 'I2C err, możliwy problem z zasilaniem lub rezystorami pull-up na SDA/SCL :(' );
       exit;
  end;

  writeln( 'sekwencyjnie, po jednym bajcie' );
  writeln( ' ' );

  for i := 0 to $FFFF-1 do
  begin
       address [0] := (i shr 8) and $ff;
       address [1] := i and $ff;
       FDwfDigitalI2cWriteRead (hAd2, $A0, @address, 2, @data, 1, @nak );
       if data = $a then continue;
       if data = $ff then break;
       write( char(data) );
  end;

  writeln( ' ' );
  writeln( 'paczkami po $ff bajtów' );
  writeln( ' ' );

  done := false;
  for i := 0 to $ff-1 do
  begin
       address [0] := i;
       address [1] := 0; // czytamy paczkami po 256 bajtow
       FDwfDigitalI2cWriteRead (hAd2, $A0, @address, 2, @dataBuff, $ff, @nak );
       for x := 0 to $ff-1 do
       begin
              if dataBuff[x] = $a then continue;
              if dataBuff[x] = $ff then
              begin
                   done := true;
                   break;
              end;
              write( char(dataBuff[x]) );
       end;
       if done then break;
  end;
  writeln( ' ' );
  writeln( 'done!' );
  writeln( ' ' );

  FDwfDeviceClose( hAd2 );

end.

