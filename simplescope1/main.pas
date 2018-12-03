unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, ueled, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Dwf;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart1: TChart;
    WaveLineSeries1: TLineSeries;
    RefreshTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    hAd2 : HDWF;
    msgErr : TDwfString512;
    procedure PrepareSimpleScope;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  msg : TDwfString32;
begin
  if not FDwfDeviceOpen( -1, @hAd2 ) then
    begin
      FDwfGetLastErrorMsg( msgErr );
      showMessage ( msgErr );
      Application.Terminate;
    end;
  FDwfEnumDeviceName( 0, msg );
  self.Caption := msg;
  FDwfEnumSN( 0, msg );
  self.Caption := self.Caption + ' ' + msg;

  self.PrepareSimpleScope;
  RefreshTimer.Enabled := true;
end;


procedure TMainForm.PrepareSimpleScope;
begin
  FDwfAnalogInChannelOffsetSet( hAd2, -1, 0 );
  FDwfAnalogInChannelRangeSet( hAd2, -1, 5.0 );
  FDwfAnalogInFrequencySet( hAd2, 100E+3 ); // 100kHz, 100pt/div ->  1ms/div
  FDwfAnalogInBufferSizeSet( hAd2, 8192 );
  FDwfAnalogInTriggerSourceSet( hAd2, trigsrcDetectorAnalogIn );
  FDwfAnalogInTriggerAutoTimeoutSet( hAd2, (*10.0*) 0 );
  FDwfAnalogInTriggerChannelSet( hAd2, 0 );
  FDwfAnalogInTriggerTypeSet( hAd2, trigtypeEdge );
  FDwfAnalogInTriggerLevelSet( hAd2, 1.0 );
  FDwfAnalogInTriggerConditionSet( hAd2, trigcondRisingPositive );
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDwfDeviceClose( hAd2 );
end;

procedure TMainForm.RefreshTimerTimer(Sender: TObject);
var
   waveform : array [0..8191] of double;
   status : DwfState;
   n : integer;
begin
  FDwfAnalogInConfigure ( hAd2, false, true );

  repeat
        FDwfAnalogInStatus( hAd2, true, @status );
  until status = stsDone;

  FDwfAnalogInStatusData( hAd2, 0, @waveform, 1024 );

  self.WaveLineSeries1.Clear;
  Chart1.LeftAxis.Range.Min := -5;
  Chart1.LeftAxis.Range.Max := 5;
  for n := 0 to 1023 do
  begin
    self.WaveLineSeries1.AddXY( n, waveform [ n ] );
  end;

end;

end.

