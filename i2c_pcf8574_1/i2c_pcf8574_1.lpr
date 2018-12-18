program i2c_pcf8574_1;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes,
    { you can add units after this }
    crt, sysutils, dwf;

const
  pattern : array [0..$f] of byte = (
          $ff - %00000001,
          $ff - %00000010,
          $ff - %00000100,
          $ff - %00001000,
          $ff - %00010000,
          $ff - %00100000,
          $ff - %01000000,
          $ff - %10000000,
          $ff - %01000000,
          $ff - %00100000,
          $ff - %00010000,
          $ff - %00001000,
          $ff - %00000100,
          $ff - %00000010,
          $ff - %01010101,
          $ff - %10101010
  );

var
   hAd2 : HDWF;
   msgErr : TDwfString512;
   i, nak : integer;
   ledData: byte;
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  FDwfDigitalI2cRateSet( hAd2, 500 ); // taktowaie na przykład 500Hz
  FDwfDigitalI2cSclSet( hAd2, 0 );     // SCL - DIO_0
  FDwfDigitalI2cSdaSet( hAd2, 1);      // SDA = DIO_1

  FDwfDigitalI2cClear( hAd2, @nak );
  if nak = 0 then
  begin
       writeln( 'I2C err, możliwy problem z zasilaniem lub rezystorami pull-up na SDA/SCL :(' );
       exit;
  end;

  repeat
    for i := 0 to length( pattern )-1 do
    begin
         //FDwfDigitalI2cWrite( hAd2, $40, @pattern[ i ], 1, @nak );
         //lub
         FDwfDigitalI2cWriteOne( hAd2, $40, pattern[ i ], @nak );
         delay ( 100 );
    end;
  until false;

  FDwfDeviceClose( hAd2 );

end.

