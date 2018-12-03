program devtest1;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes,
    { you can add units after this }
    dwf;

var
   hAd2 : HDWF;
   msgErr : TDwfString512;
   msgVer, msgDevName, msgSN, msgUserName : TDwfString32;
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

  FDwfDeviceClose( hAd2 );

end.

