program spi_41hv51_1;

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
   txData : smallint;
   rxData : smallint;
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  FDwfDigitalSpiFrequencySet( hAd2, 1E3 );     // 1kHz
  FDwfDigitalSpiClockSet( hAd2, DIO_1 );       // CLK = DIO_1

  FDwfDigitalSpiDataSet ( hAd2,  FUNC_DQ0_MOSI_SISO (* idxDQ *), DIO_2 );   // DQ0 (MOSI) = DIO_2
  FDwfDigitalSpiDataSet ( hAd2,  FUNC_DQ1_MISO (* idxD1 *), DIO_3 );        // DQ1 (MISO) = DIO_3

  FDwfDigitalSpiModeSet ( hAd2, 0 ); // SPI w 0 - Mode 0,0 , 3-Mode 1,1
  FDwfDigitalSpiOrderSet ( hAd2, 1 );  // MSB first

  FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_HIGH );   // /SEL = DIO_0, wymuś H

  txData := $0085;
  repeat
    FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_LOW );
    FDwfDigitalSpiWrite16 ( hAd2, DQ_MOSI_MISO , 16, @txData, 1 );
    FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_HIGH );
    delay (50);
  until false;

  FDwfDeviceClose( hAd2 );

end.

