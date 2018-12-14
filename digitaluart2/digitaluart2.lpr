program digitaluart2;

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
   rxBuffer : array [0..1023] of  AnsiChar;
   rcv, par : integer;
begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  // 9.6kHz
  FDwfDigitalUartRateSet( hAd2, double( 115200 ) );
  //DIO_0 jako TxD
  FDwfDigitalUartTxSet( hAd2, 3 );
  FDwfDigitalUartRxSet( hAd2, 2 );
  // 8 bit danych
  FDwfDigitalUartBitsSet( hAd2, 8);
  // 0-None, 1-Odd, 2-Even
  FDwfDigitalUartParitySet( hAd2, 0);
  // 1 stop
  FDwfDigitalUartStopSet( hAd2, double( 1 ) );

  FDwfDigitalUartRx (hAd2, rxBuffer, 0 (* !! enable *) , @rcv, @par );

  repeat
    FDwfDigitalUartRx (hAd2, rxBuffer, 1024, @rcv, @par );
    rxBuffer [ rcv ] := char(0);
    if rcv > 0 then
    begin
        write ( rxBuffer );
    end;
  until false;

  FDwfDeviceClose( hAd2 );

end.

