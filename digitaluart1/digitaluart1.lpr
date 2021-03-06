program digitaluart1;

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
   txStr : string;
   n : integer;
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  // 9.6kHz
  FDwfDigitalUartRateSet( hAd2, double( 9600 ) );
  //DIO_0 jako TxD
  FDwfDigitalUartTxSet( hAd2, 0 );
  // 8 bit danych
  FDwfDigitalUartBitsSet( hAd2, 8);
  // 0-None, 1-Odd, 2-Even
  FDwfDigitalUartParitySet( hAd2, 0);
  // 1 stop
  FDwfDigitalUartStopSet( hAd2, double( 1 ) );

  // zgodnie z http://www.qscomp.cz/Pdf/CU406SCPB-T20A-05.pdf
  // reset, cursor off
  FDwfDigitalUartTx( hAd2, PChar( #$1B#$49#$16 ), 3 );

  n := 0;
  repeat
//    txStr := FormatDateTime( 'YYYY-MM-DD hh:nn:ss', Now );
    txStr := 'aktualny czas to: ' + FormatDateTime( 'hh:nn:ss', Now );
    FDwfDigitalUartTx( hAd2, PChar(#$1B#$48#$00), 3); // cursor @ 0
    FDwfDigitalUartTx( hAd2, PChar(txStr), length( txStr ));
    Delay ( 500 );
    Inc (n);
  until n = 100;

  FDwfDeviceClose( hAd2 );

end.

