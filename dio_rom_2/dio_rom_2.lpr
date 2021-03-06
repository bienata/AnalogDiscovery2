program dio_rom_2;

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
   dwAllPins : dword;

const
  romNAND_1 : array [0..1] of byte = ( $77, $77 );
  romNAND_2 : array [0..1] of byte = ( $ff, $0f );
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  // NAND 1
  // we: DIO-0, DIO-1
  // wy: DIO-15 = not ( DIO-0 and DIO-1 )
  FDwfDigitalOutEnableSet( hAd2, DIO_0, false );
  FDwfDigitalOutEnableSet( hAd2, DIO_1, false );
  FDwfDigitalOutEnableSet( hAd2, DIO_15, true );
  FDwfDigitalOutTypeSet( hAd2, DIO_15, DwfDigitalOutTypeROM );
  FDwfDigitalOutOutputSet(hAd2, DIO_15, DwfDigitalOutOutputPushPull ); // push-pull
  FDwfDigitalOutDataSet( hAd2, DIO_15, @romNAND_1, 16 ); // matryca

  // NAND 2
  // we: DIO-2, DIO-3
  // wy: DIO-14 = not ( DIO-2 and DIO-3 )
  FDwfDigitalOutEnableSet( hAd2, DIO_2, false );
  FDwfDigitalOutEnableSet( hAd2, DIO_3, false );
  FDwfDigitalOutEnableSet( hAd2, DIO_14, true );
  FDwfDigitalOutTypeSet( hAd2, DIO_14, DwfDigitalOutTypeROM );
  FDwfDigitalOutOutputSet(hAd2, DIO_14, DwfDigitalOutOutputPushPull ); // push-pull
  FDwfDigitalOutDataSet( hAd2, DIO_14, @romNAND_2, 16 ); // matryca

  // CLK - 100MHz/100:
  FDwfDigitalOutDividerSet( hAd2, DIO_14, Cardinal(100) );
  FDwfDigitalOutDividerSet( hAd2, DIO_15, Cardinal(100) );

  writeln ('bramki gotowe');
  FDwfDigitalOutConfigure( hAd2, true );

  repeat
     FDwfDigitalIOStatus( hAd2 );
     FDwfDigitalIOInputStatus(hAd2, @dwAllPins );
     dwAllPins := dwAllPins and %1100000000001111; // maska na nieuzywane piny
     writeln( binStr( dwAllPins, 16 )  );
     delay (100);
  until false;

  writeln ('i po bramkach');
  FDwfDigitalOutReset( hAd2 );

  FDwfDeviceClose( hAd2 );
end.

