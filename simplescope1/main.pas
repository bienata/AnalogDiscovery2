unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, ueled, uEGauge, uEKnob, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, crt, Dwf;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart1: TChart;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    TriggerConstantLine1: TConstantLine;
    WaveLineSeries1: TLineSeries;
    RefreshTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    hAd2 : HDWF;
    msgErr : TDwfString512;
    waveform : array [0..8191] of double;
    procedure PrepareSimpleScope;
  end;

const
     MAX_SAMPLES : integer = 2*512;
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
  // kanal 0 on
  FDwfAnalogInChannelEnableSet ( hAd2, 0, true );
  // offset na 0
  FDwfAnalogInChannelOffsetSet( hAd2, -1, 0 );
  // zakres 5Vpp
  FDwfAnalogInChannelRangeSet( hAd2, -1, 5.0 );
  // 100kHz, 100pt/div ->  1ms/div
  FDwfAnalogInFrequencySet( hAd2, 100E+3 );
  // buforek np 512 sampli
  FDwfAnalogInBufferSizeSet( hAd2, MAX_SAMPLES );
  // autotrigger na off
  FDwfAnalogInTriggerAutoTimeoutSet( hAd2, 0.0 );
  // wyzwalaj sygnalem analogowym
  FDwfAnalogInTriggerSourceSet( hAd2, trigsrcDetectorAnalogIn );
  // z kanalu 0
  FDwfAnalogInTriggerChannelSet( hAd2, 0 );
  // wyzwalanie zboczem
  FDwfAnalogInTriggerTypeSet( hAd2, trigtypeEdge );
  // poziom 1V
  FDwfAnalogInTriggerLevelSet( hAd2, 1.0 );
  // histereza 100mV
  FDwfAnalogInTriggerHysteresisSet ( hAd2, 0.1 );
  // narastajaco
  FDwfAnalogInTriggerConditionSet( hAd2, trigcondRisingPositive );
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDwfDeviceClose( hAd2 );
end;

procedure TMainForm.RefreshTimerTimer(Sender: TObject);
var
   status : DwfState;
   n : integer;
begin
  writeln ('.');
  FDwfAnalogInConfigure ( hAd2, false, true );

  repeat
        FDwfAnalogInStatus( hAd2, true, @status );
        Delay (1);
  until status = stsDone;

  FDwfAnalogInStatusData( hAd2, 0, @waveform, MAX_SAMPLES );

  self.WaveLineSeries1.Clear;
  for n := 0 to MAX_SAMPLES - 1 do
  begin
    self.WaveLineSeries1.AddXY( n - round ( MAX_SAMPLES/2 ), waveform [ n ] );
  end;

end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
var
   trigPos : double;
begin
  trigPos := self.TrackBar1.Position/10.0;
  FDwfAnalogInTriggerLevelSet( hAd2, trigPos );
  self.TriggerConstantLine1.Position := trigPos ;
  Label1.Caption := 'tr=' + FormatFloat('0.00', trigPos ) + 'V';
end;

end.

