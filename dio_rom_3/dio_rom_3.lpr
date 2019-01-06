program dio_rom_3;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes,
    { you can add units after this }
    crt, sysutils, dwf;

type
  TROMData = array [0..1] of byte;

var
   hAd2 : HDWF;
   msgErr : TDwfString512;
   dwAllPins : dword;
   i : integer;
const
  romSegmentsData : array [0..6] of TROMData = (
     ( $12, $28 ),      // DIO-8
     ( $60, $d8 ),      // DIO-9
     ( $04, $d0 ),      // DIO-10
     ( $92, $84 ),      // DIO-11
     ( $ba, $02 ),      // DIO-12
     ( $8e, $20 ),      // DIO-13
     ( $83, $10 )       // DIO-14
  );
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  // wyjscia DIO_3...DIO_0
  FDwfDigitalOutEnableSet( hAd2, DIO_0, false );
  FDwfDigitalOutEnableSet( hAd2, DIO_1, false );
  FDwfDigitalOutEnableSet( hAd2, DIO_2, false );
  FDwfDigitalOutEnableSet( hAd2, DIO_3, false );

  // wyjscia DIO_14...DIO_8
  for i := 0 to 6 do
  begin
       FDwfDigitalOutEnableSet( hAd2, DIO_8 + i, true );
       FDwfDigitalOutTypeSet( hAd2, DIO_8 + i, DwfDigitalOutTypeROM );
       FDwfDigitalOutOutputSet(hAd2, DIO_8 + i, DwfDigitalOutOutputPushPull );
       FDwfDigitalOutDividerSet (hAd2, DIO_8 + i, Cardinal(1) );
       FDwfDigitalOutDataSet( hAd2, DIO_8 + i, @romSegmentsData[ i ], 16 );
  end;

  writeln ('bramki gotowe');
  FDwfDigitalOutConfigure( hAd2, true );

  repeat
      FDwfDigitalIOStatus( hAd2 );
      FDwfDigitalIOInputStatus(hAd2, @dwAllPins );
      write( binStr( dwAllPins, 16 ) + #$D  );
      delay (100);
  until false;

  writeln ('i po bramkach');
  FDwfDigitalOutReset( hAd2 );

  FDwfDeviceClose( hAd2 );
end.

