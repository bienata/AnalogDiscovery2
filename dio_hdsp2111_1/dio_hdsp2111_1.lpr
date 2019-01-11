program dio_hdsp2111_1;

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
   b, i : integer;

const
  msg : array [0..4] of string = (
  //  0123456789ABCDEF
     'to jest test    ',
     '    wyswietlacza',
     '*** HDSP2111 ***',
     '  tasza was here',
     'hello  microgeek'
  );

WR_HI : dword = $8000;
WR_LO : dword = $0000;

SEL_DATA : dword = $1000;
SEL_CTRL : dword = $0000;

A3_HI : dword = $0800;
A3_LO : dword = $0000;

// co za wiocha...bosssshhhhe
procedure Hdsp2111CharAt( c : char; n : integer );
var
   addr_char : dword;
begin
  addr_char := dword( n shl 8 ) + ord(c);
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_DATA + addr_char );
  FDwfDigitalIOOutputSet( hAd2, WR_LO + SEL_DATA + addr_char );
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_DATA + addr_char );
end;

procedure Hdsp2111Text( s : string );
var i : integer;
begin
  for i := 1 to length(s) do
  begin
    hdsp2111CharAt( s[ i ], i-1 );
  end;
end;

procedure Hdsp2111Brightness( b : byte );
var
   brightness : byte;
begin
  brightness := ($7-b) and $07; // na wszelki wypadek
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_CTRL + A3_LO + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_LO + SEL_CTRL + A3_LO + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_CTRL + A3_HI + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_LO + SEL_CTRL + A3_HI + brightness);
  FDwfDigitalIOOutputSet( hAd2, WR_HI + SEL_CTRL + A3_HI + brightness);
end;


begin

  if not FDwfDeviceOpen( -1, @hAd2 ) then
  begin
       FDwfGetLastErrorMsg( msgErr );
       writeln ( msgErr );
       exit;
  end;

  FDwfDigitalIOOutputEnableSet( hAd2, $FFFF ); // all OUT
  FDwfDigitalIOOutputSet( hAd2, WR_HI ); // !WR=1


  repeat
  for i := 0 to length( msg )-1 do
  begin
      Hdsp2111Text ( msg [i] );
      for b := 0 to 7 do
      begin
            hdsp2111brightness ( b );
            delay( 100 );
      end;
      delay( 500 );
      for b := 7 downto 0 do
      begin
            hdsp2111brightness ( b );
            delay( 100 );
      end;
  end;
  until false;

  FDwfDigitalOutReset( hAd2 );
  FDwfDeviceClose( hAd2 );
end.

